rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(ggwordcloud)
library(patchwork)
library(wordcloud)
library(RColorBrewer)
library(readxl)
library(readr)
library(tidyr)

years <- 2016:2024
job_titles_by_year <- list()

for (y in years) {
  df <- readRDS(paste0("dol_", y, "_tc.rds"))
  job_titles_by_year[[as.character(y)]] <- df$clean_title
}


## Wordcloud for each cluster
cluster_titles <- read_csv("cluster_top_titles.csv")
custom_stopwords <- c("and", "i", "ii", "iii", "=")

h1b_cluster_wordcloud <- function(titles_text, cluster_num) {
  text_blob <- titles_text %>%
    tolower() %>%
    str_replace_all("[^a-z ]", " ") %>%
    str_squish()
  
  words_df <- tibble(text = text_blob) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words, by = "word") %>%
    filter(!word %in% custom_stopwords) %>%
    count(word, sort = TRUE) %>%
    filter(nchar(word) > 2)
  
  wordcloud(
    words = words_df$word,
    freq = words_df$n,
    max.words = 150,
    min.freq = 2,
    scale = c(8, 2),
    random.order = FALSE,
    rot.per = 0.2,
    colors = brewer.pal(8, "Dark2")
  )
  
  title(main = paste("Cluster", cluster_num), cex.main = 3)
}

png("h1b_cluster_wordcloud_panel.png", width = 2400, height = 1800)
par(mfrow = c(3, 3), mar = c(0.2, 0.2, 1.2, 0.2), oma = c(0, 0, 0, 0))

for (clust in cluster_titles$cluster) {
  h1b_cluster_wordcloud(cluster_titles$top_titles[cluster_titles$cluster == clust], clust)
}

dev.off()

## Combined Bar Charts
h1b_all_years <- read.csv("dol_all_tc_with_cluster_status.csv")

cluster_labels <- c(
  "0" = "Banking & Credit Services",
  "1" = "Financial Consulting",
  "2" = "Risk Management",
  "3" = "Tax, Audit & Accounting",
  "4" = "Financial & Business Analytics",
  "5" = "Quantitative research and evaluation",
  "6" = "Portfolio & Asset Management",
  "7" = "Compliance & Regulation"
)

case_status_colors <- c(
  "certified" = "#1a9850",
  "certified-withdrawn" = "#BBA900",
  "denied" = "#d73027",
  "withdrawn" = "#4575b4"
)

plot_df <- h1b_all_years %>%
  mutate(
    year = as.integer(year),
    cluster = as.integer(cluster),
    cluster_label = factor(
      paste0("Cluster ", cluster, ": ", cluster_labels[as.character(cluster)]),
      levels = paste0("Cluster ", 0:7, ": ", cluster_labels[as.character(0:7)])
    ),
    case_status = tolower(case_status)
  ) %>%
  count(cluster_label, year, case_status, name = "count") %>%
  tidyr::complete(cluster_label, year, case_status, fill = list(count = 0))

p_all <- ggplot(plot_df, aes(x = factor(year), y = count, fill = case_status)) +
  geom_col() +
  facet_wrap(~ cluster_label, ncol = 2, scales = "free_y") +
  labs(
    title = "H-1B Cases by Cluster and Case Status (2016–2024)",
    x = "Year",
    y = "Number of Cases",
    fill = "Case Status"
  ) +
  theme_minimal() +
  scale_fill_manual(values = case_status_colors) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 11, face = "bold")
  )

ggsave("all_clusters_bar_chart.png", p_all, width = 11, height = 12, dpi = 300)


### Statistics
cluster_summaries <- list()
summary_table <- h1b_all_years %>%
  group_by(cluster, year) %>%
  summarise(total_jobs = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = year,
    values_from = total_jobs,
    values_fill = 0
  ) %>%
  arrange(cluster)

View(summary_table)

status_summaries <- h1b_all_years %>%
  group_by(year, case_status) %>%
  summarise(total_jobs = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = case_status,
    values_from = total_jobs,
    values_fill = 0
  ) %>%
  arrange(year)

View(status_summaries)

cluster_year_share <- h1b_all_years %>%
  group_by(year, cluster) %>%
  summarise(total_jobs = n(), .groups = "drop") %>%
  group_by(year) %>%
  mutate(percent = total_jobs / sum(total_jobs) * 100) %>%
  ungroup() %>%
  select(cluster, year, percent) %>%
  pivot_wider(
    names_from = year,
    values_from = percent
  ) %>%
  arrange(cluster)

cluster_year_share <- cluster_year_share %>%
  mutate(across(-cluster, ~ round(.x, 2)))

View(cluster_year_share)

status_cluster_share <- h1b_all_years %>%
  group_by(year, cluster, case_status) %>%
  summarise(total_jobs = n(), .groups = "drop") %>%
  group_by(year, cluster) %>%
  mutate(percent = round(100 * total_jobs / sum(total_jobs), 2)) %>%
  ungroup() %>%
  select(year, cluster, case_status, percent) %>%
  pivot_wider(
    names_from = case_status,
    values_from = percent,
    values_fill = 0
  ) %>%
  arrange(year, cluster)

View(status_cluster_share)

cluster_year_counts <- h1b_all_years %>%
  group_by(year, cluster) %>%
  summarise(n = n(), .groups = "drop")

cluster_year_shares <- cluster_year_counts %>%
  group_by(year) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()


### Logit Regression
h1b_model <- h1b_all_years %>%
  mutate(certified_bin = ifelse(case_status == "certified", 1, 0))
model_logit <- glm(
  certified_bin ~ factor(cluster) + factor(year),
  data = h1b_model,
  family = binomial(link = "logit")
)
summary(model_logit)
exp(coef(model_logit))

### USCIS Approval Number
uscis_approval <- read_csv("Employer Information.csv")
uscis_approval_yearly <- uscis_approval %>%
  rename(year = `Fiscal Year`) %>%
  group_by(year) %>%
  summarise(
    `Initial Approvals` = sum(`Initial Approval`, na.rm = TRUE),
    `Initial Denials` = sum(`Initial Denial`, na.rm = TRUE),
    `Continuing Approvals` = sum(`Continuing Approval`, na.rm = TRUE),
    `Continuing Denials` = sum(`Continuing Denial`, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = "case_type",
    values_to = "count"
  ) %>%
  mutate(
    case_type = factor(
      case_type,
      levels = c("Continuing Approvals", "Initial Approvals", "Continuing Denials", "Initial Denials")
    )
  )

ggplot(uscis_approval_yearly, aes(x = year, y = count, color = case_type)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "H1B Visa Approvals for Finance & Insurance Industry According to USCIS (2016–2024)",
    x = "Year",
    y = "Number of Cases",
    color = "Case Type"
  ) +
  scale_color_manual(
    values = c(
      "Continuing Approvals" = "#2ca02c",
      "Initial Approvals" = "#61d621",
      "Continuing Denials" = "#bf0606",
      "Initial Denials" = "#dc5656"
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_text(face = "bold")
  )

print(uscis_approval_yearly, n = Inf)


