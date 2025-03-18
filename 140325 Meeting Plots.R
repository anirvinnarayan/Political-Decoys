# ============================================
# Political Decoys - String Similarity Working?
# ============================================
# Date: 12/03/25
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
  reticulate,
  sf,
  gt,
  formattable,
  DT,
  scales,
  rnaturalearth, 
  stringr, 
  lwgeom,
  stringdist,
  fixest, 
  modelsummary, 
  writexl, 
  plm, 
  PGRdup,
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
  scales, 
  phonics
)

### 30% of all candidates are main. around 170,000

### Loading
all_states_elections <- read.csv("Raw Data/all_states_elections.csv")
candidate_pairs <- read.csv("Cleaned Data/candidate_pairs_lv_jw_ngram_masala_dblmet.csv")

### Plots
percentiles <- quantile(candidate_pairs$Levenshtein_Similarity, 
                        probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
                        na.rm = TRUE)

lev_mean <- mean(candidate_pairs$Levenshtein_Similarity, na.rm = TRUE)
lev_median <- median(candidate_pairs$Levenshtein_Similarity, na.rm = TRUE)

plot1 <- ggplot(candidate_pairs, aes(x = Levenshtein_Similarity)) +
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
    xmin = quantile(candidate_pairs$Levenshtein_Similarity, 0.95, na.rm = TRUE), 
    xmax = max(candidate_pairs$Levenshtein_Similarity, na.rm = TRUE),
    ymin = max(density(candidate_pairs$Levenshtein_Similarity, na.rm = TRUE)$y) * 0.5, 
    ymax = max(density(candidate_pairs$Levenshtein_Similarity, na.rm = TRUE)$y) * 0.7
  )

ggsave(
  filename = "140325 meeting plots/Levenshtein_Kernel_with_stats.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# for JW
percentiles <- quantile(candidate_pairs$Jaro_Winkler_Similarity, 
                        probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
                        na.rm = TRUE)

jw_mean <- mean(candidate_pairs$Jaro_Winkler_Similarity, na.rm = TRUE)
jw_median <- median(candidate_pairs$Jaro_Winkler_Similarity, na.rm = TRUE)


plot1 <- ggplot(candidate_pairs, aes(x = Jaro_Winkler_Similarity)) +
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
    xmin = quantile(candidate_pairs$Jaro_Winkler_Similarity, 0.95, na.rm = TRUE), 
    xmax = max(candidate_pairs$Jaro_Winkler_Similarity, na.rm = TRUE),
    ymin = max(density(candidate_pairs$Jaro_Winkler_Similarity, na.rm = TRUE)$y) * 0.5, 
    ymax = max(density(candidate_pairs$Jaro_Winkler_Similarity, na.rm = TRUE)$y) * 0.7
  )

ggsave(
  filename = "140325 meeting plots/Jaro_Winkler_Kernel_with_stats.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# for DblMet
percentiles <- quantile(candidate_pairs$Metaphone_Similarity, 
                        probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
                        na.rm = TRUE)

met_mean <- mean(candidate_pairs$Metaphone_Similarity, na.rm = TRUE)
met_median <- median(candidate_pairs$Metaphone_Similarity, na.rm = TRUE)


plot1 <- ggplot(candidate_pairs, aes(x = Metaphone_Similarity)) +
  geom_density(fill = "skyblue", color = "darkblue", alpha = 0.7) +
  geom_vline(xintercept = percentiles, color = "red", linetype = "dashed") +
  annotate("text", x = percentiles, y = rep(0, 7) + 0.05,
           label = c("10th", "25th", "50th", "75th", "90th", "95th", "99th"),
           color = "red", angle = 90, vjust = -0.5) +
  labs(title = "Kernel Density of Metaphone Similarity with Percentiles",
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
  Value = round(c(met_mean, met_median, percentiles), 3)
)

table <- gt(stats_table) %>%
  tab_header(
    title = "Double Metaphone Summary Stats"
  ) %>%
  fmt_number(
    columns = Value,
    decimals = 3
  ) %>%
  tab_options(
    heading.background.color = "#000",
    column_labels.background.color = "#DDDDDD"
  )

gtsave(table, "140325 meeting plots/stats_table.png")

# summaries at election level
candidate_pairs <- candidate_pairs %>%
  mutate(is_decoy = ifelse(
    Levenshtein_Similarity > lv_99th & 
      Jaro_Winkler_Similarity > jw_99th & 
      Metaphone_Similarity > mp_99th,
    TRUE, 
    FALSE
  ))

election_decoys <- candidate_pairs %>%
  group_by(Year, State_Name, Constituency_Name, Election_Type, Assembly_No) %>%
  summarise(
    main_main_decoys = sum(Pair_Type == "main-main" & is_decoy, na.rm = TRUE),
    main_minor_decoys = sum((Pair_Type == "main-minor" | Pair_Type == "minor-main") & is_decoy, na.rm = TRUE),
    minor_minor_decoys = sum(Pair_Type == "minor-minor" & is_decoy, na.rm = TRUE),
    .groups = "drop"
  )

sum(election_decoys$main_minor_decoys > 0)/nrow(election_decoys)
sum(election_decoys$main_main_decoys > 0)/nrow(election_decoys)
sum(election_decoys$minor_minor_decoys > 0)/nrow(election_decoys)

thresholds <- 1:10

threshold_counts <- data.frame(
  threshold = thresholds,
  elections_count = sapply(thresholds, function(t) {
    sum(election_decoys$main_minor_decoys >= t)
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
    x = "Number of Main-Minor Decoys (At Least (>=))",
    y = "Percentage of Elections"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = "140325 meeting plots/Main_Minor_Decoy_Histogram.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

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

constituency_metrics <- candidate_pairs %>%
  group_by(State_Name, Year, Constituency_Name, Election_Type, Assembly_No) %>%
  dplyr::summarize(
    # basic constituency metrics
    total_candidates = max(Candidate1_N_Cand, na.rm = TRUE),
    decoy_candidates = sum(is_decoy & Pair_Type == "main-minor", na.rm = TRUE),
    decoy_share = round(sum(is_decoy & Pair_Type == "main-minor", na.rm = TRUE) / 
                          max(Candidate1_N_Cand, na.rm = TRUE) * 100, 2),
    
    # vote metrics
    total_votes = max(Candidate1_Valid_Votes, na.rm = TRUE),
    
    # count votes to decoys - adjusted to use is_decoy
    total_votes_to_decoys = sum(
      ifelse(is_decoy & Pair_Type == "main-minor", Candidate2_Votes, 
             ifelse(is_decoy & Pair_Type == "minor-main", Candidate1_Votes, 0)), 
      na.rm = TRUE),
    decoy_vote_share = round(
      sum(ifelse(is_decoy & Pair_Type == "main-minor", Candidate2_Votes, 
                 ifelse(is_decoy & Pair_Type == "minor-main", Candidate1_Votes, 0)), 
          na.rm = TRUE) / max(Candidate1_Valid_Votes, na.rm = TRUE) * 100, 2),
    
    # winner and runner-up metrics
    winning_margin = min(Candidate1_Margin[Candidate1_Position == 1], na.rm = TRUE),
    winning_margin_percentage = min(Candidate1_Margin_Percentage[Candidate1_Position == 1], na.rm = TRUE),
    winner_party = first(Candidate1_Party[Candidate1_Position == 1]),
    runner_up_party = first(Candidate1_Party[Candidate1_Position == 2]),
    winner_vote_share = max(Candidate1_Vote_Share_Percentage[Candidate1_Position == 1], na.rm = TRUE),
    runner_up_vote_share = max(Candidate1_Vote_Share_Percentage[Candidate1_Position == 2], na.rm = TRUE),
    
    # check for decoys associated with winner/runner-up - using is_decoy
    winner_has_decoys = any((is_decoy & Pair_Type == "main-minor" & Candidate1_Position == 1) | 
                              (is_decoy & Pair_Type == "minor-main" & Candidate2_Position == 1), 
                            na.rm = TRUE),
    runner_up_has_decoys = any((is_decoy & Pair_Type == "main-minor" & Candidate1_Position == 2) | 
                                 (is_decoy & Pair_Type == "minor-main" & Candidate2_Position == 2), 
                               na.rm = TRUE),
    
    # education metrics
    education = mean(c(Candidate1_MyNeta_education_numeric,
                       Candidate2_MyNeta_education_numeric), na.rm = TRUE),

    # educ & party for decoys - using is_decoy
    decoy_education = mean(
      c(Candidate2_MyNeta_education_numeric[is_decoy & Pair_Type == "main-minor"],
        Candidate1_MyNeta_education_numeric[is_decoy & Pair_Type == "minor-main"]),
      na.rm = TRUE),
    decoy_party = mean(
      c(Candidate2_Party_type_numeric[is_decoy & Pair_Type == "main-minor"],
        Candidate1_Party_type_numeric[is_decoy & Pair_Type == "minor-main"]),
      na.rm = TRUE),

    # educ & party for main with decoys - using is_decoy
    main_with_decoy_education = mean(
      c(Candidate1_MyNeta_education_numeric[is_decoy & Pair_Type == "main-minor"],
        Candidate2_MyNeta_education_numeric[is_decoy & Pair_Type == "minor-main"]),
      na.rm = TRUE),
    main_with_decoy_party = mean(
      c(Candidate1_Party_type_numeric[is_decoy & Pair_Type == "main-minor"],
        Candidate2_Party_type_numeric[is_decoy & Pair_Type == "minor-main"]),
      na.rm = TRUE),

    # educ & party for main without decoys
    main_without_decoy_education = mean(
      c(Candidate1_MyNeta_education_numeric[!is_decoy & (Pair_Type == "main-minor" | Pair_Type == "main-main")],
        Candidate2_MyNeta_education_numeric[!is_decoy & (Pair_Type == "minor-main" | Pair_Type == "main-main")]),
      na.rm = TRUE),
    main_without_decoy_party = mean(
      c(Candidate1_Party_type_numeric[!is_decoy & (Pair_Type == "main-minor" | Pair_Type == "main-main")],
        Candidate2_Party_type_numeric[!is_decoy & (Pair_Type == "minor-main" | Pair_Type == "main-main")]),
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
  scale_y_continuous(
    breaks = seq(0, 100, by = 2),
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

state_metrics %>%
  mutate(State_Name = fct_reorder(State_Name, -mean_num_decoys)) %>%
  ggplot(aes(x = State_Name, y = mean_num_decoys)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  geom_errorbar(aes(ymin = median_num_decoys, ymax = p90_num_decoys), width = 0.2) +
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

state_year_metrics <- constituency_metrics %>%
  group_by(State_Name, Year) %>%
  summarise(
    decoy_proportion = mean(decoy_candidates/total_candidates, na.rm = TRUE),
    decoy_vote_share = mean(decoy_vote_share, na.rm = TRUE),
    .groups = "drop"
  )

year_metrics <- state_year_metrics %>%
  group_by(Year) %>%
  dplyr::summarize(
    decoy_proportion = mean(decoy_proportion, na.rm = TRUE), 
    decoy_vote_share = mean(decoy_vote_share, na.rm = TRUE),
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
  scale_x_continuous(
    breaks = seq(1950, 2025, by = 5)) + 
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

ggplot(year_metrics, aes(x = Year, y = decoy_vote_share)) +
  geom_line(color = "red") +
  geom_point(color = "darkred") +
  theme_minimal() +
  labs(
    title = "Avg Decoy Vote Share Over Time",
    x = "Election Year",
    y = "Average Decoy Proportion"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(
    breaks = seq(1950, 2025, by = 5)) + 
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
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

### Notes
# Charts to Add
# % of pairs that are decoys in main-main, main-minor, minor-minor, denom is total pairs in that cat
# maybe delete < 1990
# on initial and one name delete

# bin the year chart by 5 year period
# separate points by AE and GE or something ... 
# cluster the states ??? 
