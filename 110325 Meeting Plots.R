# ============================================
# Political Decoys
# ============================================
# Date: 11/03/25
# Author: Anirvin Narayan

rm(list = ls())
setwd("/Users/anirvin/Downloads/Political Decoys Data")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  readr,
  lubridate,
  here,
  readr, 
  geosphere,
  sf,
  scales,
  rnaturalearth, 
  stringr, 
  lwgeom,
  stringdist,
  fixest, 
  modelsummary, 
  writexl, 
  plm, 
  openxlsx, 
  lmtest, 
  etable, 
  clubSandwich, 
  gridExtra,
  stargazer, 
  psych, 
  kableExtra,
  MASS, 
  glmmTMB, 
  survival, 
  sandwich, 
  xtable, 
  fixest, 
  RefManageR, 
  corrplot, 
  Hmisc, 
  patchwork, 
  scales
)

### Loading
candidate_pairs_merged_2 <- read.csv("Cleaned Data/candidate_pairs_lv_jw_ngram.csv")
all_states_GE <- read.csv("Raw Data/All_States_GE.csv")

# basic summary stats to understand the results
percentiles <- quantile(candidate_pairs_merged_2$Levenshtein_Similarity, 
                        probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
                        na.rm = TRUE)

lev_mean <- mean(candidate_pairs_merged_2$Levenshtein_Similarity, na.rm = TRUE)
lev_median <- median(candidate_pairs_merged_2$Levenshtein_Similarity, na.rm = TRUE)

ggplot(candidate_pairs_merged_2, aes(x = Levenshtein_Similarity)) +
  geom_density(fill = "skyblue", color = "darkblue", alpha = 0.7) +
  geom_vline(xintercept = percentiles, color = "red", linetype = "dashed") +
  annotate("text", x = percentiles, y = rep(0, 7) + 0.05,
           label = c("10th", "25th", "50th", "75th", "90th", "95th", "99th"),
           color = "red", angle = 90, vjust = -0.5) +
  labs(title = "Kernel Density of Levenshtein Similarity with Percentiles",
       x = "Levenshtein Similarity",
       y = "Density") +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold") 
  )

ggsave(
  filename = "110325 meeting/Levenshtein_Kernel.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

plot1 <- ggplot(candidate_pairs_merged_2, aes(x = Levenshtein_Similarity)) +
  geom_density(fill = "skyblue", color = "darkblue", alpha = 0.7) +
  geom_vline(xintercept = percentiles, color = "red", linetype = "dashed") +
  annotate("text", x = percentiles, y = rep(0, 7) + 0.05,
           label = c("10th", "25th", "50th", "75th", "90th", "95th", "99th"),
           color = "red", angle = 90, vjust = -0.5) +
  labs(title = "Kernel Density of Levenshtein Similarity with Percentiles",
       x = "Levenshtein Similarity",
       y = "Density") +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold") 
  )

stats_table <- data.frame(
  Statistic = c("Mean", "Median", "10th Percentile", "25th Percentile", 
                "50th Percentile", "75th Percentile", "90th Percentile",
                "95th Percentile", "99th Percentile"),
  Value = round(c(lev_mean, lev_median, percentiles), 3)
)

library(scales)
table_grob <- tableGrob(
  stats_table, 
  rows = NULL,
  theme = ttheme_minimal(
    core = list(
      fg_params = list(fontsize = 8),
      bg_params = list(fill = rgb(1, 1, 1, 0.8), col = "black", lwd = 1)
    ),
    colhead = list(
      fg_params = list(fontsize = 9, fontface = "bold"),
      bg_params = list(fill = rgb(1, 1, 1, 0.8), col = "black", lwd = 1)
    )
  )
)

plot1 + 
  annotation_custom(
    table_grob, 
    xmin = quantile(candidate_pairs_merged_2$Levenshtein_Similarity, 0.95, na.rm = TRUE), 
    xmax = max(candidate_pairs_merged_2$Levenshtein_Similarity, na.rm = TRUE),
    ymin = max(density(candidate_pairs_merged_2$Levenshtein_Similarity, na.rm = TRUE)$y) * 0.5, 
    ymax = max(density(candidate_pairs_merged_2$Levenshtein_Similarity, na.rm = TRUE)$y) * 0.7
  )

ggsave(
  filename = "110325 meeting/Levenshtein_Kernel_with_stats.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

candidate_pairs_merged_1_main_minor <- candidate_pairs_merged_2 %>%
  filter(Pair_Type == "main-minor")

candidate_pairs_merged_1_main_main <- candidate_pairs_merged_2 %>%
  filter(Pair_Type == "main-main")

candidate_pairs_merged_1_minor_minor <- candidate_pairs_merged_2 %>%
  filter(Pair_Type == "minor-minor")

percentiles <- quantile(candidate_pairs_merged_1_main_minor$Levenshtein_Similarity, 
                        probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
                        na.rm = TRUE)

ggplot(candidate_pairs_merged_1_main_minor, aes(x = Levenshtein_Similarity)) +
  geom_density(fill = "skyblue", color = "darkblue", alpha = 0.7) +
  geom_vline(xintercept = percentiles, color = "red", linetype = "dashed") +
  annotate("text", x = percentiles, y = rep(0, 7) + 0.05,
           label = c("10th", "25th", "50th", "75th", "90th", "95th", "99th"),
           color = "red", angle = 90, vjust = -0.5) +
  labs(title = "Kernel Density of Levenshtein Similarity with Percentiles (Main-Minor)",
       x = "Levenshtein Similarity",
       y = "Density") +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = "110325 meeting/Levenshtein_Kernel_main_minor.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

percentiles <- quantile(candidate_pairs_merged_1_main_main$Levenshtein_Similarity, 
                        probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
                        na.rm = TRUE)

ggplot(candidate_pairs_merged_1_main_main, aes(x = Levenshtein_Similarity)) +
  geom_density(fill = "skyblue", color = "darkblue", alpha = 0.7) +
  geom_vline(xintercept = percentiles, color = "red", linetype = "dashed") +
  annotate("text", x = percentiles, y = rep(0, 7) + 0.05,
           label = c("10th", "25th", "50th", "75th", "90th", "95th", "99th"),
           color = "red", angle = 90, vjust = -0.5) +
  labs(title = "Kernel Density of Levenshtein Similarity with Percentiles (Main-Main)",
       x = "Levenshtein Similarity",
       y = "Density") +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = "110325 meeting/Levenshtein_Kernel_main_main.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

percentiles <- quantile(candidate_pairs_merged_1_minor_minor$Levenshtein_Similarity, 
                        probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
                        na.rm = TRUE)

ggplot(candidate_pairs_merged_1_minor_minor, aes(x = Levenshtein_Similarity)) +
  geom_density(fill = "skyblue", color = "darkblue", alpha = 0.7) +
  geom_vline(xintercept = percentiles, color = "red", linetype = "dashed") +
  annotate("text", x = percentiles, y = rep(0, 7) + 0.05,
           label = c("10th", "25th", "50th", "75th", "90th", "95th", "99th"),
           color = "red", angle = 90, vjust = -0.5) +
  labs(title = "Kernel Density of Levenshtein Similarity with Percentiles (Minor-Minor)",
       x = "Levenshtein Similarity",
       y = "Density") +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = "110325 meeting/Levenshtein_Kernel_minor_minor.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# for JW
percentiles <- quantile(candidate_pairs_merged_2$Jaro_Winkler_Similarity, 
                        probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
                        na.rm = TRUE)

jw_mean <- mean(candidate_pairs_merged_2$Jaro_Winkler_Similarity, na.rm = TRUE)
jw_median <- median(candidate_pairs_merged_2$Jaro_Winkler_Similarity, na.rm = TRUE)


plot1 <- ggplot(candidate_pairs_merged_2, aes(x = Jaro_Winkler_Similarity)) +
  geom_density(fill = "skyblue", color = "darkblue", alpha = 0.7) +
  geom_vline(xintercept = percentiles, color = "red", linetype = "dashed") +
  annotate("text", x = percentiles, y = rep(0, 7) + 0.05,
           label = c("10th", "25th", "50th", "75th", "90th", "95th", "99th"),
           color = "red", angle = 90, vjust = -0.5) +
  labs(title = "Kernel Density of Jaro-Winkler Similarity with Percentiles",
       x = "Jaro-Winkler Similarity",
       y = "Density") +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold") 
  )

stats_table <- data.frame(
  Statistic = c("Mean", "Median", "10th Percentile", "25th Percentile", 
                "50th Percentile", "75th Percentile", "90th Percentile",
                "95th Percentile", "99th Percentile"),
  Value = round(c(jw_mean, jw_median, percentiles), 3)
)

library(scales)
table_grob <- tableGrob(
  stats_table, 
  rows = NULL,
  theme = ttheme_minimal(
    core = list(
      fg_params = list(fontsize = 8),
      bg_params = list(fill = rgb(1, 1, 1, 0.8), col = "black", lwd = 1)
    ),
    colhead = list(
      fg_params = list(fontsize = 9, fontface = "bold"),
      bg_params = list(fill = rgb(1, 1, 1, 0.8), col = "black", lwd = 1)
    )
  )
)

plot1 + 
  annotation_custom(
    table_grob, 
    xmin = quantile(candidate_pairs_merged_2$Jaro_Winkler_Similarity, 0.95, na.rm = TRUE), 
    xmax = max(candidate_pairs_merged_2$Jaro_Winkler_Similarity, na.rm = TRUE),
    ymin = max(density(candidate_pairs_merged_2$Jaro_Winkler_Similarity, na.rm = TRUE)$y) * 0.5, 
    ymax = max(density(candidate_pairs_merged_2$Jaro_Winkler_Similarity, na.rm = TRUE)$y) * 0.7
  )

ggsave(
  filename = "110325 meeting/JW_Kernel_with_stats.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# for n-gram
# for JW
percentiles <- quantile(candidate_pairs_merged_2$NGram_Similarity, 
                        probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
                        na.rm = TRUE)

ng_mean <- mean(candidate_pairs_merged_2$NGram_Similarity, na.rm = TRUE)
ng_median <- median(candidate_pairs_merged_2$NGram_Similarity, na.rm = TRUE)


plot1 <- ggplot(candidate_pairs_merged_2, aes(x = NGram_Similarity)) +
  geom_density(fill = "skyblue", color = "darkblue", alpha = 0.7) +
  geom_vline(xintercept = percentiles, color = "red", linetype = "dashed") +
  annotate("text", x = percentiles, y = rep(0, 7) + 0.05,
           label = c("10th", "25th", "50th", "75th", "90th", "95th", "99th"),
           color = "red", angle = 90, vjust = -0.5) +
  labs(title = "Kernel Density of NGram Similarity with Percentiles",
       x = "NGram Similarity",
       y = "Density") +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold") 
  )

stats_table <- data.frame(
  Statistic = c("Mean", "Median", "10th Percentile", "25th Percentile", 
                "50th Percentile", "75th Percentile", "90th Percentile",
                "95th Percentile", "99th Percentile"),
  Value = round(c(ng_mean, ng_median, percentiles), 3)
)

library(scales)
table_grob <- tableGrob(
  stats_table, 
  rows = NULL,
  theme = ttheme_minimal(
    core = list(
      fg_params = list(fontsize = 8),
      bg_params = list(fill = rgb(1, 1, 1, 0.8), col = "black", lwd = 1)
    ),
    colhead = list(
      fg_params = list(fontsize = 9, fontface = "bold"),
      bg_params = list(fill = rgb(1, 1, 1, 0.8), col = "black", lwd = 1)
    )
  )
)

plot1 + 
  annotation_custom(
    table_grob, 
    xmin = quantile(candidate_pairs_merged_2$NGram_Similarity, 0.95, na.rm = TRUE), 
    xmax = max(candidate_pairs_merged_2$NGram_Similarity, na.rm = TRUE),
    ymin = max(density(candidate_pairs_merged_2$NGram_Similarity, na.rm = TRUE)$y) * 0.5, 
    ymax = max(density(candidate_pairs_merged_2$NGram_Similarity, na.rm = TRUE)$y) * 0.7
  )

ggsave(
  filename = "110325 meeting/NG_Kernel_with_stats.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# summaries at election level
election_decoys <- candidate_pairs_merged_2 %>%
  group_by(Year, State_Name, Constituency_Name, Election_Type, Assembly_No) %>%
  summarise(
    main_main_decoys = sum(Pair_Type == "main-main" & is_decoy_lv_refined, na.rm = TRUE),
    main_minor_decoys = sum(Pair_Type == "main-minor" & is_decoy_lv_refined, na.rm = TRUE),
    minor_minor_decoys = sum(Pair_Type == "minor-minor" & is_decoy_lv_refined, na.rm = TRUE),
    .groups = "drop"
  )

max_decoy_info <- candidate_pairs_merged_2 %>%
  filter(Pair_Type == "main-minor" & is_decoy_lv_refined) %>%
  # who is main and minor
  mutate(
    main_candidate_id = ifelse(Candidate1_Type == "main", Candidate1_PID, Candidate2_PID),
    main_candidate_name = ifelse(Candidate1_Type == "main", Candidate1_Name, Candidate2_Name)
  ) %>%
  # group by election and main candidate
  group_by(Year, State_Name, Constituency_Name, Election_Type, Assembly_No, main_candidate_id, main_candidate_name) %>%
  summarise(decoy_count = n(), .groups = "drop") %>%
  # for each election, find the candidate with max decoys
  group_by(Year, State_Name, Constituency_Name, Election_Type, Assembly_No) %>%
  slice_max(order_by = decoy_count, n = 1, with_ties = FALSE) %>%
  dplyr::select(Year, State_Name, Constituency_Name, Election_Type, Assembly_No, 
                max_decoys_main_id = main_candidate_id, 
                max_decoys_main_name = main_candidate_name,
                max_decoys_per_main = decoy_count)

final_election_decoys <- election_decoys %>%
  left_join(max_decoy_info, 
            by = c("Year", "State_Name", "Constituency_Name", "Election_Type", "Assembly_No")) %>%
  # handle cases with no decoys
  mutate(
    max_decoys_per_main = replace_na(max_decoys_per_main, 0),
    max_decoys_main_id = if_else(max_decoys_per_main == 0, NA_character_, max_decoys_main_id),
    max_decoys_main_name = if_else(max_decoys_per_main == 0, NA_character_, max_decoys_main_name)
  )

sum(final_election_decoys$main_minor_decoys > 3)/nrow(final_election_decoys)
sum(final_election_decoys$main_minor_decoys > 0)/nrow(final_election_decoys)
sum(final_election_decoys$main_main_decoys > 0)/nrow(final_election_decoys)
sum(final_election_decoys$minor_minor_decoys > 0)/nrow(final_election_decoys)

thresholds <- 1:10

threshold_counts <- data.frame(
  threshold = thresholds,
  elections_count = sapply(thresholds, function(t) {
    sum(final_election_decoys$main_minor_decoys >= t)
  })
)

ggplot(threshold_counts, aes(x = factor(threshold), y = elections_count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = elections_count), vjust = -0.5, size = 3.5) +
  labs(
    title = "Number of Elections with Main-Minor Decoys Above Thresholds",
    x = "Number of Main-Minor Decoys (>)",
    y = "Number of Elections"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold")
  )

threshold_counts$percentage <- threshold_counts$elections_count / nrow(election_decoys) * 100

ggplot(threshold_counts, aes(x = factor(threshold), y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, size = 3.5) +
  labs(
    title = "Percentage of Elections with Main-Minor Decoys Above Thresholds",
    x = "Number of Main-Minor Decoys (>=)",
    y = "Percentage of Elections"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = "110325 meeting/Decoy_histogram.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# adjust for last name
# summaries at election level
election_decoys_2 <- candidate_pairs_merged_2 %>%
  filter(is_decoy_lv_refined_2) %>%
  distinct(Year, State_Name, Constituency_Name, Election_Type, Assembly_No, Candidate2_PID, Pair_Type) %>%
  group_by(Year, State_Name, Constituency_Name, Election_Type, Assembly_No) %>%
  summarise(
    main_main_decoys = sum(Pair_Type == "main-main", na.rm = TRUE),
    main_minor_decoys = sum(Pair_Type == "main-minor", na.rm = TRUE),
    minor_minor_decoys = sum(Pair_Type == "minor-minor", na.rm = TRUE),
    .groups = "drop"
  )

max_decoy_info_2 <- candidate_pairs_merged_2 %>%
  filter(Pair_Type == "main-minor" & is_decoy_lv_refined_2) %>%
  # who is main and minor
  mutate(
    main_candidate_id = ifelse(Candidate1_Type == "main", Candidate1_PID, Candidate2_PID),
    main_candidate_name = ifelse(Candidate1_Type == "main", Candidate1_Name, Candidate2_Name)
  ) %>%
  # group by election and main candidate
  group_by(Year, State_Name, Constituency_Name, Election_Type, Assembly_No, main_candidate_id, main_candidate_name) %>%
  summarise(decoy_count = n(), .groups = "drop") %>%
  # for each election, find the candidate with max decoys
  group_by(Year, State_Name, Constituency_Name, Election_Type, Assembly_No) %>%
  slice_max(order_by = decoy_count, n = 1, with_ties = FALSE) %>%
  dplyr::select(Year, State_Name, Constituency_Name, Election_Type, Assembly_No, 
                max_decoys_main_id = main_candidate_id, 
                max_decoys_main_name = main_candidate_name,
                max_decoys_per_main = decoy_count)

final_election_decoys_2 <- election_decoys_2 %>%
  left_join(max_decoy_info_2, 
            by = c("Year", "State_Name", "Constituency_Name", "Election_Type", "Assembly_No")) %>%
  # handle cases with no decoys
  mutate(
    max_decoys_per_main = replace_na(max_decoys_per_main, 0),
    max_decoys_main_id = if_else(max_decoys_per_main == 0, NA_character_, max_decoys_main_id),
    max_decoys_main_name = if_else(max_decoys_per_main == 0, NA_character_, max_decoys_main_name)
  )

sum(final_election_decoys_2$main_minor_decoys > 3)/nrow(final_election_decoys_2)
sum(final_election_decoys_2$main_minor_decoys > 0)/nrow(final_election_decoys_2)

thresholds <- 0:10

threshold_counts_2 <- data.frame(
  threshold = thresholds,
  elections_count = sapply(thresholds, function(t) {
    sum(final_election_decoys_2$main_minor_decoys > t)
  })
)

ggplot(threshold_counts, aes(x = factor(threshold), y = elections_count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = elections_count), vjust = -0.5, size = 3.5) +
  labs(
    title = "Number of Elections with Main-Minor Decoys Above Thresholds",
    x = "Number of Main-Minor Decoys (>)",
    y = "Number of Elections"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold")
  )

threshold_counts_2$percentage <- threshold_counts_2$elections_count / nrow(election_decoys) * 100

ggplot(threshold_counts_2, aes(x = factor(threshold), y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, size = 3.5) +
  labs(
    title = "Percentage of Elections with Main-Minor Decoys Above Thresholds",
    subtitle = "Decoy pairs with name sub-string frequency > 182 (95th percentile) omitted",
    x = "Number of Main-Minor Decoys (>)",
    y = "Percentage of Elections"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = "110325 meeting/Decoy_histogram_lastname_adjustment.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# by state
str(candidate_pairs_merged_2)

all_states_elections <- all_states_elections %>%
  mutate(MyNeta_education_numeric = case_when(
    MyNeta_education == "Illiterate" ~ 0,
    MyNeta_education == "Literate" ~ 1,
    MyNeta_education == "5th Pass" ~ 5,
    MyNeta_education == "8th Pass" ~ 8,
    MyNeta_education == "10th Pass" ~ 10,
    MyNeta_education == "12th Pass" ~ 12,
    MyNeta_education == "Graduate" ~ 16,
    MyNeta_education == "Graduate Professional" ~ 16,
    MyNeta_education == "Post Graduate" ~ 20,
    MyNeta_education == "Doctorate" ~ 22,
    MyNeta_education %in% c("Others", "Not Given Page Missing", "Not Given Not Filled", "") ~ NA_real_,
    TRUE ~ NA_real_))

all_states_elections <- all_states_elections %>%
  mutate(Party_type_numeric = recode(Party_Type_TCPD,
                                     "Independents" = 0,
                                     "Local Party" = 1, 
                                     "State-based Party" = 2, 
                                     "State-based Party (Other State" = 3, 
                                     "National Party" = 4, 
                                     .default = NA_real_
  ))

constituency_metrics <- candidate_pairs_merged_2 %>%
  group_by(State_Name, Year, Constituency_Name, Election_Type, Assembly_No) %>%
  dplyr::summarize(
    # basic constituency metrics
    total_candidates = max(Candidate1_N_Cand, na.rm = TRUE),
    decoy_candidates = sum(is_decoy_lv_refined & Pair_Type == "main-minor", na.rm = TRUE),
    decoy_share = round(sum(is_decoy_lv_refined & Pair_Type == "main-minor", na.rm = TRUE) / 
                          max(Candidate1_N_Cand, na.rm = TRUE) * 100, 2),
    
    # vote metrics
    total_votes = max(Candidate1_Valid_Votes, na.rm = TRUE),
    
    # count votes to decoys - adjusted to use is_decoy_lv_refined
    total_votes_to_decoys = sum(
      ifelse(is_decoy_lv_refined & Pair_Type == "main-minor", Candidate2_Votes, 
             ifelse(is_decoy_lv_refined & Pair_Type == "minor-main", Candidate1_Votes, 0)), 
      na.rm = TRUE),
    decoy_vote_share = round(
      sum(ifelse(is_decoy_lv_refined & Pair_Type == "main-minor", Candidate2_Votes, 
                 ifelse(is_decoy_lv_refined & Pair_Type == "minor-main", Candidate1_Votes, 0)), 
          na.rm = TRUE) / max(Candidate1_Valid_Votes, na.rm = TRUE) * 100, 2),
    
    # winner and runner-up metrics
    winning_margin = min(Candidate1_Margin[Candidate1_Position == 1], na.rm = TRUE),
    winning_margin_percentage = min(Candidate1_Margin_Percentage[Candidate1_Position == 1], na.rm = TRUE),
    winner_party = first(Candidate1_Party[Candidate1_Position == 1]),
    runner_up_party = first(Candidate1_Party[Candidate1_Position == 2]),
    winner_vote_share = max(Candidate1_Vote_Share_Percentage[Candidate1_Position == 1], na.rm = TRUE),
    runner_up_vote_share = max(Candidate1_Vote_Share_Percentage[Candidate1_Position == 2], na.rm = TRUE),
    
    # check for decoys associated with winner/runner-up - using is_decoy_lv_refined
    winner_has_decoys = any((is_decoy_lv_refined & Pair_Type == "main-minor" & Candidate1_Position == 1) | 
                              (is_decoy_lv_refined & Pair_Type == "minor-main" & Candidate2_Position == 1), 
                            na.rm = TRUE),
    runner_up_has_decoys = any((is_decoy_lv_refined & Pair_Type == "main-minor" & Candidate1_Position == 2) | 
                                 (is_decoy_lv_refined & Pair_Type == "minor-main" & Candidate2_Position == 2), 
                               na.rm = TRUE),
    
    # education metrics
    education = mean(c(Candidate1_MyNeta_education_numeric, 
                       Candidate2_MyNeta_education_numeric), na.rm = TRUE),
    
    # educ & party for decoys - using is_decoy_lv_refined
    decoy_education = mean(
      c(Candidate2_MyNeta_education_numeric[is_decoy_lv_refined & Pair_Type == "main-minor"],
        Candidate1_MyNeta_education_numeric[is_decoy_lv_refined & Pair_Type == "minor-main"]), 
      na.rm = TRUE),
    decoy_party = mean(
      c(Candidate2_Party_type_numeric[is_decoy_lv_refined & Pair_Type == "main-minor"],
        Candidate1_Party_type_numeric[is_decoy_lv_refined & Pair_Type == "minor-main"]), 
      na.rm = TRUE),
    
    # educ & party for main with decoys - using is_decoy_lv_refined
    main_with_decoy_education = mean(
      c(Candidate1_MyNeta_education_numeric[is_decoy_lv_refined & Pair_Type == "main-minor"],
        Candidate2_MyNeta_education_numeric[is_decoy_lv_refined & Pair_Type == "minor-main"]), 
      na.rm = TRUE),
    main_with_decoy_party = mean(
      c(Candidate1_Party_type_numeric[is_decoy_lv_refined & Pair_Type == "main-minor"],
        Candidate2_Party_type_numeric[is_decoy_lv_refined & Pair_Type == "minor-main"]), 
      na.rm = TRUE),
    
    # educ & party for main without decoys
    main_without_decoy_education = mean(
      c(Candidate1_MyNeta_education_numeric[!is_decoy_lv_refined & (Pair_Type == "main-minor" | Pair_Type == "main-main")],
        Candidate2_MyNeta_education_numeric[!is_decoy_lv_refined & (Pair_Type == "minor-main" | Pair_Type == "main-main")]), 
      na.rm = TRUE),
    main_without_decoy_party = mean(
      c(Candidate1_Party_type_numeric[!is_decoy_lv_refined & (Pair_Type == "main-minor" | Pair_Type == "main-main")],
        Candidate2_Party_type_numeric[!is_decoy_lv_refined & (Pair_Type == "minor-main" | Pair_Type == "main-main")]), 
      na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    close_race = ifelse(winning_margin_percentage < 5, TRUE, FALSE),
    decoy_impact_potential = ifelse(decoy_vote_share > winning_margin_percentage, TRUE, FALSE)
  ) %>%
  # sort by state, year and constituency
  arrange(State_Name, Year, Constituency_Name, Election_Type, Assembly_No)

state_metrics <- constituency_metrics %>%
  group_by(State_Name) %>%
  dplyr::summarize(
    # how many constituencies
    constituency_count = n(),
    
    # number of decoys
    mean_num_decoys = mean(decoy_candidates, na.rm = TRUE),
    median_num_decoys = median(decoy_candidates, na.rm = TRUE),
    p90_num_decoys = quantile(decoy_candidates, 0.9, na.rm = TRUE),
    max_num_decoys = max(decoy_candidates, na.rm = TRUE),
    
    # share of decoys
    mean_decoy_share = mean(decoy_share, na.rm = TRUE),
    median_decoy_share = median(decoy_share, na.rm = TRUE),
    p90_decoy_share = quantile(decoy_share, 0.9, na.rm = TRUE),
    max_decoy_share = max(decoy_share, na.rm = TRUE),
    
    # voteshare stuff
    total_votes = sum(total_votes, na.rm = TRUE),
    total_votes_to_decoys = sum(total_votes_to_decoys, na.rm = TRUE),
    overall_decoy_vote_share = (total_votes_to_decoys / total_votes) * 100,
    
    # decoys affect outcomes?
    races_with_decoys = sum(decoy_candidates > 0, na.rm = TRUE),
    close_races = sum(close_race, na.rm = TRUE),
    decoy_impact_races = sum(decoy_impact_potential, na.rm = TRUE),
    
    # calculate races
    pct_races_with_decoys = (races_with_decoys / constituency_count) * 100,
    pct_close_races = (close_races / constituency_count) * 100,
    pct_decoy_impact = (decoy_impact_races / constituency_count) * 100,
    
    # education and party
    decoy_education = mean(decoy_education, na.rm = TRUE), 
    decoy_party = mean(decoy_party, na.rm = TRUE),
    main_with_decoy_education = mean(main_with_decoy_education, na.rm = TRUE),
    main_with_decoy_party = mean(main_with_decoy_party, na.rm = TRUE),
    main_without_decoy_education = mean(main_without_decoy_education, na.rm = TRUE),
    main_without_decoy_party = mean(main_without_decoy_party, na.rm = TRUE),
    
    decoy_effectiveness = (overall_decoy_vote_share * decoy_impact_races) / constituency_count,
    .groups = "drop"
  )

state_metrics %>%
  mutate(State_Name = fct_reorder(State_Name, -mean_decoy_share)) %>%
  ggplot(aes(x = State_Name, y = mean_decoy_share)) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.8) +
  geom_errorbar(aes(ymin = median_decoy_share, ymax = p90_decoy_share), width = 0.2) +
  geom_point(aes(y = max_decoy_share), color = "red", size = 2) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 25),
    labels = scales::percent_format(scale = 1)
  ) + 
  labs(
    title = "Average Share of Decoy Candidates per Election by State",
    subtitle = "With median (bottom of bar), 90th percentile (top of bar), and maximum (red dot)",
    x = "State",
    y = "Proportion of Decoy Candidates"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9)
  )

ggsave(
  filename = "110325 meeting/Decoy Share by State 1.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

state_metrics %>%
  mutate(State_Name = fct_reorder(State_Name, -mean_num_decoys)) %>%
  ggplot(aes(x = State_Name, y = mean_num_decoys)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  geom_errorbar(aes(ymin = median_num_decoys, ymax = p90_num_decoys), width = 0.2) +
  geom_point(aes(y = max_num_decoys), color = "red", size = 2) +
  labs(
    title = "Average Number of Decoy Candidates per Election by State",
    subtitle = "With median (bottom of bar), 90th percentile (top of bar), and maximum (red dot)",
    x = "State",
    y = "Number of Decoy Candidates"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9)
  )

ggsave(
  filename = "110325 meeting/Decoy Number by State 1.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

state_metrics_long <- state_metrics %>%
  dplyr::select(State_Name, 
                `Average Number` = mean_num_decoys, 
                `Maximum Number` = max_num_decoys,
                `Average Share` = mean_decoy_share, 
                `Maximum Share` = max_decoy_share,
                constituency_count) %>%
  pivot_longer(cols = c(`Average Number`, `Maximum Number`, `Average Share`, `Maximum Share`),
               names_to = "Metric", values_to = "Value")

# faceted
state_metrics_long %>%
  mutate(State_Name = fct_reorder(State_Name, 
                                  -Value * (Metric == "Average Share"), 
                                  .fun = mean, na.rm = TRUE)) %>%
  ggplot(aes(x = State_Name, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 2) +
  # geom_text(aes(label = round(Value, 3)), position = position_dodge(width = 0.9), 
  #           vjust = -0.5, size = 2.5) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Decoy Candidates Metrics by State",
    subtitle = "States ordered by average share of decoy candidates",
    x = "State",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold"),
    legend.position = "none",
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9)
  )

ggsave(
  filename = "110325 meeting/Decoy Metrics by State 1.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

state_year_metrics <- constituency_metrics %>%
  group_by(State_Name, Year) %>%
  summarise(
    decoy_proportion = mean(decoy_candidates/total_candidates, na.rm = TRUE),
    .groups = "drop"
  )

year_metrics <- state_year_metrics %>%
  group_by(Year) %>%
  dplyr::summarize(
    decoy_proportion = mean(decoy_proportion, na.rm = TRUE), 
    .groups = "drop"
  )

ggplot(year_metrics, aes(x = Year, y = decoy_proportion)) +
  geom_line(color = "red") +
  geom_point(color = "darkred") +
  theme_minimal() +
  labs(
    title = "Avg Decoy Proportion Over Time",
    x = "Election Year",
    y = "Average Decoy Proportion"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

ggsave(
  filename = "110325 meeting/Decoy Share over Time 1.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

state_metrics %>%
  # reorder
  mutate(State_Name = fct_reorder(State_Name, overall_decoy_vote_share)) %>%
  ggplot(aes(x = State_Name, y = overall_decoy_vote_share)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = sprintf("%.1f%%", overall_decoy_vote_share)), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Votes Captured by Decoy Candidates",
    subtitle = "Percentage of total votes going to decoy candidates by state",
    y = "Decoy Vote Share (%)",
    x = ""
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = "110325 meeting/Decoy Voteshare by State 1.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

state_metrics %>%
  # reorder
  mutate(State_Name = fct_reorder(State_Name, pct_decoy_impact)) %>%
  ggplot(aes(x = State_Name, y = pct_decoy_impact)) +
  geom_bar(stat = "identity", fill = "firebrick") +
  geom_text(aes(label = sprintf("%.1f%%", pct_decoy_impact)), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Races Where Decoys Could Have Changed Outcomes",
    subtitle = "Percentage of races where decoy votes exceeded winning margin",
    y = "Percentage of Races (%)",
    x = ""
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = "110325 meeting/Decoy Swing Potential by State 1.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

state_metrics_for_viz <- state_metrics %>%
  dplyr::select(State_Name, pct_close_races, pct_decoy_impact) %>%
  tidyr::pivot_longer(
    cols = c(pct_close_races, pct_decoy_impact),
    names_to = "metric",
    values_to = "percentage"
  ) %>%
  mutate(
    metric = case_when(
      metric == "pct_close_races" ~ "Close Races (<5% margin)",
      metric == "pct_decoy_impact" ~ "Races Where Decoys Matter",
      TRUE ~ metric
    ),
    State_Name = fct_reorder(State_Name, percentage, .fun = max)
  )

ggplot(state_metrics_for_viz, aes(x = State_Name, y = percentage, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("darkblue", "red")) +
  coord_flip() +
  labs(
    title = "Close Races vs. Decoy Impact Races",
    subtitle = "Comparing races with tight margins to those where decoys could matter",
    y = "Percentage of Total Races (%)",
    x = "",
    fill = ""
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = "110325 meeting/Decoy Swing in Close Races 1.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# adjusted last name
constituency_metrics_2 <- candidate_pairs_merged_2 %>%
  group_by(State_Name, Year, Constituency_Name, Election_Type, Assembly_No) %>%
  dplyr::summarize(
    # basic constituency metrics
    total_candidates = max(Candidate1_N_Cand, na.rm = TRUE),
    decoy_candidates = sum(is_decoy_lv_refined_2 & Pair_Type == "main-minor", na.rm = TRUE),
    decoy_share = round(sum(is_decoy_lv_refined_2 & Pair_Type == "main-minor", na.rm = TRUE) / 
                          max(Candidate1_N_Cand, na.rm = TRUE) * 100, 2),
    
    # vote metrics
    total_votes = max(Candidate1_Valid_Votes, na.rm = TRUE),
    
    # count votes to decoys - adjusted to use is_decoy_lv_refined_2
    total_votes_to_decoys = sum(
      ifelse(is_decoy_lv_refined_2 & Pair_Type == "main-minor", Candidate2_Votes, 
             ifelse(is_decoy_lv_refined_2 & Pair_Type == "minor-main", Candidate1_Votes, 0)), 
      na.rm = TRUE),
    decoy_vote_share = round(
      sum(ifelse(is_decoy_lv_refined_2 & Pair_Type == "main-minor", Candidate2_Votes, 
                 ifelse(is_decoy_lv_refined_2 & Pair_Type == "minor-main", Candidate1_Votes, 0)), 
          na.rm = TRUE) / max(Candidate1_Valid_Votes, na.rm = TRUE) * 100, 2),
    
    # winner and runner-up metrics
    winning_margin = min(Candidate1_Margin[Candidate1_Position == 1], na.rm = TRUE),
    winning_margin_percentage = min(Candidate1_Margin_Percentage[Candidate1_Position == 1], na.rm = TRUE),
    winner_party = first(Candidate1_Party[Candidate1_Position == 1]),
    runner_up_party = first(Candidate1_Party[Candidate1_Position == 2]),
    winner_vote_share = max(Candidate1_Vote_Share_Percentage[Candidate1_Position == 1], na.rm = TRUE),
    runner_up_vote_share = max(Candidate1_Vote_Share_Percentage[Candidate1_Position == 2], na.rm = TRUE),
    
    # check for decoys associated with winner/runner-up - using is_decoy_lv_refined_2
    winner_has_decoys = any((is_decoy_lv_refined_2 & Pair_Type == "main-minor" & Candidate1_Position == 1) | 
                              (is_decoy_lv_refined_2 & Pair_Type == "minor-main" & Candidate2_Position == 1), 
                            na.rm = TRUE),
    runner_up_has_decoys = any((is_decoy_lv_refined_2 & Pair_Type == "main-minor" & Candidate1_Position == 2) | 
                                 (is_decoy_lv_refined_2 & Pair_Type == "minor-main" & Candidate2_Position == 2), 
                               na.rm = TRUE),
    
    # education metrics
    education = mean(c(Candidate1_MyNeta_education_numeric, 
                       Candidate2_MyNeta_education_numeric), na.rm = TRUE),
    
    # educ & party for decoys - using is_decoy_lv_refined_2
    decoy_education = mean(
      c(Candidate2_MyNeta_education_numeric[is_decoy_lv_refined_2 & Pair_Type == "main-minor"],
        Candidate1_MyNeta_education_numeric[is_decoy_lv_refined_2 & Pair_Type == "minor-main"]), 
      na.rm = TRUE),
    decoy_party = mean(
      c(Candidate2_Party_type_numeric[is_decoy_lv_refined_2 & Pair_Type == "main-minor"],
        Candidate1_Party_type_numeric[is_decoy_lv_refined_2 & Pair_Type == "minor-main"]), 
      na.rm = TRUE),
    
    # educ & party for main with decoys - using is_decoy_lv_refined_2
    main_with_decoy_education = mean(
      c(Candidate1_MyNeta_education_numeric[is_decoy_lv_refined_2 & Pair_Type == "main-minor"],
        Candidate2_MyNeta_education_numeric[is_decoy_lv_refined_2 & Pair_Type == "minor-main"]), 
      na.rm = TRUE),
    main_with_decoy_party = mean(
      c(Candidate1_Party_type_numeric[is_decoy_lv_refined_2 & Pair_Type == "main-minor"],
        Candidate2_Party_type_numeric[is_decoy_lv_refined_2 & Pair_Type == "minor-main"]), 
      na.rm = TRUE),
    
    # educ & party for main without decoys
    main_without_decoy_education = mean(
      c(Candidate1_MyNeta_education_numeric[!is_decoy_lv_refined_2 & (Pair_Type == "main-minor" | Pair_Type == "main-main")],
        Candidate2_MyNeta_education_numeric[!is_decoy_lv_refined_2 & (Pair_Type == "minor-main" | Pair_Type == "main-main")]), 
      na.rm = TRUE),
    main_without_decoy_party = mean(
      c(Candidate1_Party_type_numeric[!is_decoy_lv_refined_2 & (Pair_Type == "main-minor" | Pair_Type == "main-main")],
        Candidate2_Party_type_numeric[!is_decoy_lv_refined_2 & (Pair_Type == "minor-main" | Pair_Type == "main-main")]), 
      na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    close_race = ifelse(winning_margin_percentage < 5, TRUE, FALSE),
    decoy_impact_potential = ifelse(decoy_vote_share > winning_margin_percentage, TRUE, FALSE)
  ) %>%
  # sort by state, year and constituency
  arrange(State_Name, Year, Constituency_Name, Election_Type, Assembly_No)

state_metrics_2 <- constituency_metrics_2 %>%
  group_by(State_Name) %>%
  dplyr::summarize(
    # how many constituencies
    constituency_count = n(),
    
    # number of decoys
    mean_num_decoys = mean(decoy_candidates, na.rm = TRUE),
    median_num_decoys = median(decoy_candidates, na.rm = TRUE),
    p90_num_decoys = quantile(decoy_candidates, 0.9, na.rm = TRUE),
    max_num_decoys = max(decoy_candidates, na.rm = TRUE),
    
    # share of decoys
    mean_decoy_share = mean(decoy_share, na.rm = TRUE),
    median_decoy_share = median(decoy_share, na.rm = TRUE),
    p90_decoy_share = quantile(decoy_share, 0.9, na.rm = TRUE),
    max_decoy_share = max(decoy_share, na.rm = TRUE),
    
    # voteshare stuff
    total_votes = sum(total_votes, na.rm = TRUE),
    total_votes_to_decoys = sum(total_votes_to_decoys, na.rm = TRUE),
    overall_decoy_vote_share = (total_votes_to_decoys / total_votes) * 100,
    
    # decoys affect outcomes?
    races_with_decoys = sum(decoy_candidates > 0, na.rm = TRUE),
    close_races = sum(close_race, na.rm = TRUE),
    decoy_impact_races = sum(decoy_impact_potential, na.rm = TRUE),
    
    # calculate races
    pct_races_with_decoys = (races_with_decoys / constituency_count) * 100,
    pct_close_races = (close_races / constituency_count) * 100,
    pct_decoy_impact = (decoy_impact_races / constituency_count) * 100,
    
    # education and party
    decoy_education = mean(decoy_education, na.rm = TRUE), 
    decoy_party = mean(decoy_party, na.rm = TRUE),
    main_with_decoy_education = mean(main_with_decoy_education, na.rm = TRUE),
    main_with_decoy_party = mean(main_with_decoy_party, na.rm = TRUE),
    main_without_decoy_education = mean(main_without_decoy_education, na.rm = TRUE),
    main_without_decoy_party = mean(main_without_decoy_party, na.rm = TRUE),
    
    decoy_effectiveness = (overall_decoy_vote_share * decoy_impact_races) / constituency_count,
    .groups = "drop"
  )

state_metrics_2 %>%
  mutate(State_Name = fct_reorder(State_Name, -mean_decoy_share)) %>%
  ggplot(aes(x = State_Name, y = mean_decoy_share)) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.8) +
  geom_errorbar(aes(ymin = median_decoy_share, ymax = p90_decoy_share), width = 0.2) +
  geom_point(aes(y = max_decoy_share), color = "red", size = 2) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 25),
    labels = scales::percent_format(scale = 1)
  ) + 
  labs(
    title = "Average Share of Decoy Candidates per Election by State",
    subtitle = "With median (bottom of bar), 90th percentile (top of bar), and maximum (red dot)",
    x = "State",
    y = "Proportion of Decoy Candidates"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9)
  )

ggsave(
  filename = "110325 meeting/Decoy Share by State 2.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

state_metrics_2 %>%
  mutate(State_Name = fct_reorder(State_Name, -mean_num_decoys)) %>%
  ggplot(aes(x = State_Name, y = mean_num_decoys)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  geom_errorbar(aes(ymin = median_num_decoys, ymax = p90_num_decoys), width = 0.2) +
  geom_point(aes(y = max_num_decoys), color = "red", size = 2) +
  labs(
    title = "Average Number of Decoy Candidates per Election by State",
    subtitle = "With median (bottom of bar), 90th percentile (top of bar), and maximum (red dot)",
    x = "State",
    y = "Number of Decoy Candidates"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9)
  )

ggsave(
  filename = "110325 meeting/Decoy Number by State 2.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)


state_metrics_2_long <- state_metrics_2 %>%
  dplyr::select(State_Name, 
                `Average Number` = mean_num_decoys, 
                `Maximum Number` = max_num_decoys,
                `Average Share` = mean_decoy_share, 
                `Maximum Share` = max_decoy_share,
                constituency_count) %>%
  pivot_longer(cols = c(`Average Number`, `Maximum Number`, `Average Share`, `Maximum Share`),
               names_to = "Metric", values_to = "Value")

# faceted
state_metrics_2_long %>%
  mutate(State_Name = fct_reorder(State_Name, 
                                  -Value * (Metric == "Average Share"), 
                                  .fun = mean, na.rm = TRUE)) %>%
  ggplot(aes(x = State_Name, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 2) +
  # geom_text(aes(label = round(Value, 3)), position = position_dodge(width = 0.9), 
  #           vjust = -0.5, size = 2.5) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Decoy Candidates Metrics by State",
    subtitle = "States ordered by average share of decoy candidates",
    x = "State",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold"),
    legend.position = "none",
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9)
  )

ggsave(
  filename = "110325 meeting/Decoy Metrics by State 2.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

state_year_metrics_2 <- constituency_metrics_2 %>%
  group_by(State_Name, Year) %>%
  summarise(
    decoy_proportion = mean(decoy_candidates/total_candidates, na.rm = TRUE),
    .groups = "drop"
  )

year_metrics_2 <- state_year_metrics_2 %>%
  group_by(Year) %>%
  dplyr::summarize(
    decoy_proportion = mean(decoy_proportion, na.rm = TRUE), 
    .groups = "drop"
  )

ggplot(year_metrics, aes(x = Year, y = decoy_proportion)) +
  geom_line(color = "red") +
  geom_point(color = "darkred") +
  theme_minimal() +
  labs(
    title = "Avg Decoy Proportion Over Time",
    x = "Election Year",
    y = "Average Decoy Proportion"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

ggsave(
  filename = "110325 meeting/Decoy Share over Time 2.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

state_metrics_2 %>%
  # reorder
  mutate(State_Name = fct_reorder(State_Name, overall_decoy_vote_share)) %>%
  ggplot(aes(x = State_Name, y = overall_decoy_vote_share)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = sprintf("%.1f%%", overall_decoy_vote_share)), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Votes Captured by Decoy Candidates",
    subtitle = "Percentage of total votes going to decoy candidates by state",
    y = "Decoy Vote Share (%)",
    x = ""
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = "110325 meeting/Decoy Voteshare by State 2.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

state_metrics_2 %>%
  # reorder
  mutate(State_Name = fct_reorder(State_Name, pct_decoy_impact)) %>%
  ggplot(aes(x = State_Name, y = pct_decoy_impact)) +
  geom_bar(stat = "identity", fill = "firebrick") +
  geom_text(aes(label = sprintf("%.1f%%", pct_decoy_impact)), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Races Where Decoys Could Have Changed Outcomes",
    subtitle = "Percentage of races where decoy votes exceeded winning margin",
    y = "Percentage of Races (%)",
    x = ""
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = "110325 meeting/Decoy Swing Potential by State 2.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

state_metrics_2_for_viz <- state_metrics_2 %>%
  dplyr::select(State_Name, pct_close_races, pct_decoy_impact) %>%
  tidyr::pivot_longer(
    cols = c(pct_close_races, pct_decoy_impact),
    names_to = "metric",
    values_to = "percentage"
  ) %>%
  mutate(
    metric = case_when(
      metric == "pct_close_races" ~ "Close Races (<5% margin)",
      metric == "pct_decoy_impact" ~ "Races Where Decoys Matter",
      TRUE ~ metric
    ),
    State_Name = fct_reorder(State_Name, percentage, .fun = max)
  )

ggplot(state_metrics_2_for_viz, aes(x = State_Name, y = percentage, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("darkblue", "red")) +
  coord_flip() +
  labs(
    title = "Close Races vs. Decoy Impact Races",
    subtitle = "Comparing races with tight margins to those where decoys could matter",
    y = "Percentage of Total Races (%)",
    x = "",
    fill = ""
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = "110325 meeting/Decoy Swing in Close Races 2.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# finally, education & party
education_data <- state_metrics %>%
  dplyr::select(State_Name, 
                decoy_education, 
                main_with_decoy_education, 
                main_without_decoy_education) %>%
  pivot_longer(
    cols = c(decoy_education, main_with_decoy_education, main_without_decoy_education),
    names_to = "candidate_type",
    values_to = "education_level"
  ) %>%
  mutate(candidate_type = factor(candidate_type, 
                                 levels = c("decoy_education", 
                                            "main_with_decoy_education", 
                                            "main_without_decoy_education"),
                                 labels = c("Decoy Candidates", 
                                            "Main Candidates (with decoys)", 
                                            "Main Candidates (no decoys)")))

party_data <- state_metrics %>%
  dplyr::select(State_Name, 
                decoy_party, 
                main_with_decoy_party, 
                main_without_decoy_party) %>%
  pivot_longer(
    cols = c(decoy_party, main_with_decoy_party, main_without_decoy_party),
    names_to = "candidate_type",
    values_to = "party_affiliation"
  ) %>%
  mutate(candidate_type = factor(candidate_type, 
                                 levels = c("decoy_party", 
                                            "main_with_decoy_party", 
                                            "main_without_decoy_party"),
                                 labels = c("Decoy Candidates", 
                                            "Main Candidates (with decoys)", 
                                            "Main Candidates (no decoys)")))

ggplot(education_data, aes(x = education_level, fill = candidate_type)) +
  geom_density(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Distribution of Years of Education by Candidate Type",
    x = "Education Years",
    y = "Density",
    fill = "Candidate Type"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom") + 
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = "110325 meeting/Candidate Education 1.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

ggplot(party_data, aes(x = party_affiliation, fill = candidate_type)) +
  geom_density(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Distribution of Party Type by Candidate Type",
    x = "Party Type Score",
    y = "Density",
    fill = "Candidate Type"
  ) +
  scale_fill_brewer(palette = "Set1") +
  # x-axis
  scale_x_continuous(
    breaks = c(0, 1, 2, 3, 4),
    labels = c("Independents", "Local", "State", "State (Out of State)", "National")
  ) +
  theme(legend.position = "bottom") + 
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = "110325 meeting/Candidate Party Type 1.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

education_data_2 <- state_metrics_2 %>%
  dplyr::select(State_Name, 
                decoy_education, 
                main_with_decoy_education, 
                main_without_decoy_education) %>%
  pivot_longer(
    cols = c(decoy_education, main_with_decoy_education, main_without_decoy_education),
    names_to = "candidate_type",
    values_to = "education_level"
  ) %>%
  mutate(candidate_type = factor(candidate_type, 
                                 levels = c("decoy_education", 
                                            "main_with_decoy_education", 
                                            "main_without_decoy_education"),
                                 labels = c("Decoy Candidates", 
                                            "Main Candidates (with decoys)", 
                                            "Main Candidates (no decoys)")))

party_data_2 <- state_metrics_2 %>%
  dplyr::select(State_Name, 
                decoy_party, 
                main_with_decoy_party, 
                main_without_decoy_party) %>%
  pivot_longer(
    cols = c(decoy_party, main_with_decoy_party, main_without_decoy_party),
    names_to = "candidate_type",
    values_to = "party_affiliation"
  ) %>%
  mutate(candidate_type = factor(candidate_type, 
                                 levels = c("decoy_party", 
                                            "main_with_decoy_party", 
                                            "main_without_decoy_party"),
                                 labels = c("Decoy Candidates", 
                                            "Main Candidates (with decoys)", 
                                            "Main Candidates (no decoys)")))

test <- candidate_pairs_merged_2 %>%
  filter(Levenshtein_Similarity > 0.53)

test <- all_states_elections %>%
  filter(Vote_Share_Percentage < 10)

ggplot(education_data_2, aes(x = education_level, fill = candidate_type)) +
  geom_density(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Distribution of Years of Education by Candidate Type",
    x = "Education Years",
    y = "Density",
    fill = "Candidate Type"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom") + 
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = "110325 meeting/Candidate Education 2.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

ggplot(party_data_2, aes(x = party_affiliation, fill = candidate_type)) +
  geom_density(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Distribution of Party Type by Candidate Type",
    x = "Party Type Score",
    y = "Density",
    fill = "Candidate Type"
  ) +
  scale_fill_brewer(palette = "Set1") +
  # x-axis
  scale_x_continuous(
    breaks = c(0, 1, 2, 3, 4),
    labels = c("Independents", "Local", "State", "State (Out of State)", "National")
  ) +
  theme(legend.position = "bottom") + 
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = "110325 meeting/Candidate Party Type 2.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

### Comparing the 3 methods (3 that we have right now)
divergent_examples <- candidate_pairs_merged_2 %>%
  mutate(
    max_diff = pmax(
      abs(Levenshtein_Similarity - Jaro_Winkler_Similarity),
      abs(Levenshtein_Similarity - NGram_Similarity),
      abs(Jaro_Winkler_Similarity - NGram_Similarity)
    )
  ) %>%
  arrange(desc(max_diff)) %>%
  dplyr::select(Candidate1_Name, Candidate2_Name, 
                Levenshtein_Similarity, Jaro_Winkler_Similarity, NGram_Similarity, 
                max_diff, Year, State_Name, Constituency_Name) %>%
  head(10)

measure_disagreements_by_percentile <- candidate_pairs_merged_2 %>%
  mutate(
    lev_rank = percent_rank(Levenshtein_Similarity),
    jw_rank = percent_rank(Jaro_Winkler_Similarity),
    ngram_rank = percent_rank(NGram_Similarity),
    
    # calculate maximum difference in percentile ranks
    max_percentile_diff = pmax(
      abs(lev_rank - jw_rank),
      abs(lev_rank - ngram_rank),
      abs(jw_rank - ngram_rank)
    ),
    
    # define high and low percentiles (e.g., top 25% and bottom 25%)
    lev_high_pct = lev_rank > 0.75,
    jw_high_pct = jw_rank > 0.75,
    ngram_high_pct = ngram_rank > 0.75,
    lev_low_pct = lev_rank < 0.25,
    jw_low_pct = jw_rank < 0.25,
    ngram_low_pct = ngram_rank < 0.25
  ) %>%
  filter(
    # one measure in top 25% while another in bottom 25%
    (lev_high_pct & (jw_low_pct | ngram_low_pct)) |
      (jw_high_pct & (lev_low_pct | ngram_low_pct)) |
      (ngram_high_pct & (lev_low_pct | jw_low_pct))
  ) %>%
  dplyr::select(
    Candidate1_Name, Candidate2_Name, 
    Levenshtein_Similarity, Jaro_Winkler_Similarity, NGram_Similarity,
    lev_rank, jw_rank, ngram_rank,
    max_percentile_diff,
    Year, State_Name, Constituency_Name
  ) %>%
  arrange(desc(max_percentile_diff))

top_percentile_disagreements <- head(measure_disagreements_by_percentile, 20)

# name patterns that highlight algo difference
# name order swap
transposition_examples <- candidate_pairs_merged_2 %>%
  filter(
    grepl("\\w+\\s+\\w+", Candidate1_Name) & grepl("\\w+\\s+\\w+", Candidate2_Name),
    # reasonable similarity by at least one measure
    pmax(Levenshtein_Similarity, Jaro_Winkler_Similarity, NGram_Similarity) > 0.5
  ) %>%
  mutate(
    # get first and last names
    c1_first = sub("^(\\S+).*", "\\1", Candidate1_Name),
    c1_last = sub(".*\\s+(\\S+)$", "\\1", Candidate1_Name),
    c2_first = sub("^(\\S+).*", "\\1", Candidate2_Name),
    c2_last = sub(".*\\s+(\\S+)$", "\\1", Candidate2_Name),
    # check if names are swapped
    possible_transposition = (c1_first == c2_last & c1_last == c2_first) | 
      (c1_first == c2_last | c1_last == c2_first)
  ) %>%
  filter(possible_transposition) %>%
  dplyr::select(Candidate1_Name, Candidate2_Name, 
                Levenshtein_Similarity, Jaro_Winkler_Similarity, NGram_Similarity,
                Year, State_Name, Constituency_Name) %>%
  arrange(desc(NGram_Similarity))

# prefix matching where JW shld excel
prefix_examples <- candidate_pairs_merged_2 %>%
  mutate(
    common_prefix_length = mapply(function(s1, s2) {
      min_len <- min(nchar(s1), nchar(s2))
      i <- 1
      while (i <= min_len && substr(s1, 1, i) == substr(s2, 1, i)) i <- i + 1
      return(i - 1)
    }, Candidate1_Name, Candidate2_Name),
    prefix_ratio = common_prefix_length / pmin(nchar(Candidate1_Name), nchar(Candidate2_Name))
  ) %>%
  filter(
    prefix_ratio > 0.5,
    abs(nchar(Candidate1_Name) - nchar(Candidate2_Name)) > 3,
    Jaro_Winkler_Similarity > Levenshtein_Similarity
  ) %>%
  dplyr::select(Candidate1_Name, Candidate2_Name, common_prefix_length, prefix_ratio,
                Levenshtein_Similarity, Jaro_Winkler_Similarity, NGram_Similarity,
                Year, State_Name, Constituency_Name) %>%
  arrange(desc(Jaro_Winkler_Similarity - Levenshtein_Similarity))

# False positive
pair_type_summary <- candidate_pairs_merged_2 %>%
  group_by(Pair_Type) %>%
  dplyr::summarize(
    Count = n(),
    Avg_LV = mean(Levenshtein_Similarity),
    Avg_JW = mean(Jaro_Winkler_Similarity), 
    Avg_NG = mean(NGram_Similarity),
    High_LV_Count = sum(is_decoy_lv_refined),
    fp_rate = High_LV_Count / Count
  )