# ============================================
# Political Decoys - 220425 meeting plots
# ============================================
# Date: 22/04/25
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
  phonics, 
  flextable, 
  purrr
)

#### LOADING ####
all_states_elections <- read.csv("Raw Data/all_states_elections.csv")
CLEA_cleaned <- read.csv("Cleaned Data/CLEA_cleaned.csv")
candidate_pairs <- read.csv("Cleaned Data/candidate_pairs_lv_jw_ngram_masala_dblmet.csv")
candidate_pairs_CLEA <- read.csv("Cleaned Data/candidate_pairs_lv_jw_ngram_masala_dblmet_CLEA.csv")
candidate_pairs_CLEA_the_rest <- read.csv("Cleaned Data/candidate_pairs_lv_jw_ngram_masala_dblmet_CLEA_the_rest.csv")
candidate_level_with_WDI <- read.csv("Cleaned Data/CLEA_candidate_level_w_WDI.csv")

constituency_metrics <- read.csv("Cleaned Data/constituency_metrics_w_exogvars.csv")
constituency_metrics_filtered <- read.csv("Cleaned Data/constituency_metrics_w_exogvars_filtered.csv")
constituency_metrics_74_94 <- read.csv("Cleaned Data/constituency_metrics_w_exogvars_74_94.csv")
constituency_metrics_95_04 <- read.csv("Cleaned Data/constituency_metrics_w_exogvars_95_04.csv")
constituency_metrics_05_23 <- read.csv("Cleaned Data/constituency_metrics_w_exogvars_05_23.csv")

# what we care about!
candidate_level_with_WDI_minor <- candidate_level_with_WDI %>%
  filter(Candidate_Type == "minor")

### Combining the candidate_pairs_CLEA dfs
candidate_pairs_CLEA_full <- rbind(candidate_pairs_CLEA, candidate_pairs_CLEA_the_rest)

#### Preparing Candidate Pairs #### 
# first filtering out electoral fusion from the US
unique_US_sub <- CLEA_cleaned %>%
  filter(ctr_n == "US") %>%
  distinct(ctr_n, sub)

CLEA_US <- CLEA_cleaned %>%
  filter(ctr_n == "US" & yr >= 1930) %>%
  filter(sub != "new york" & sub != "New York" & 
           sub != "connecticut" & sub != "Connecticut")

country_codes <- CLEA_cleaned %>%
  distinct(ctr_n, ctr) %>%
  rename(
    Country_Name = ctr_n, 
    Country_Code = ctr
  )

# elections are identified as 
# distinct(Country_Code, Year, Election_Month, Constituency_Name, Election_ID)
elections_with_sub <- CLEA_cleaned %>%
  distinct(ctr_n, ctr, yr, mn, cst_n, cst, id, sub) %>%
  rename(
    Country_Name = ctr_n, 
    Country_Code = ctr, 
    Year = yr, 
    Election_Month = mn, 
    Constituency_Name = cst_n, 
    Constituency_Code = cst, 
    Election_ID = id, 
    State = sub
  )

candidate_pairs_CLEA_full <- candidate_pairs_CLEA_full %>%
  left_join(country_codes, 
            by = c("Country_Code")) %>%
  left_join(
    elections_with_sub %>%
      dplyr::select(Country_Code, Year, Election_Month, Constituency_Name, Election_ID, State),
    by = c("Country_Code", "Year", "Election_Month", "Constituency_Name", "Election_ID")
  )

# remove all countries < 1920, and NY and CT
candidate_pairs_CLEA_cleaned <- candidate_pairs_CLEA_full %>%
  filter(!(Country_Name == "US" & Year < 1960)) %>%
  filter(!(Country_Name == "US" & Year < 2012 & (State == "delaware"|State == "Delaware"))) %>%
  filter(!(Country_Name == "US" & Year < 2023 & (State == "south carolina"|State == "South Carolina"))) %>%
  filter(!(Country_Name == "US" & Year < 2000 & (State == "south dakota"|State == "South Dakota"))) %>%
  filter(State != "new york" & State != "New York" & 
           State != "connecticut" & State != "Connecticut")

candidate_pairs_CLEA_cleaned <- candidate_pairs_CLEA_cleaned %>%
  mutate(
    Candidate1_Election_ID = paste(Year, Country_Code, Constituency_Name, Candidate1_Party_Name, Candidate1_Name, Candidate1_Votes, sep = "_"),
    Candidate2_Election_ID = paste(Year, Country_Code, Constituency_Name, Candidate2_Party_Name, Candidate2_Name, Candidate2_Votes, sep = "_")
  )

str(candidate_pairs_CLEA_cleaned)

# creating is_decoy
# thresholds
lv_99th <- quantile(candidate_pairs_CLEA_cleaned$Levenshtein_Similarity, 0.99, na.rm = TRUE)
jw_99th <- quantile(candidate_pairs_CLEA_cleaned$Jaro_Winkler_Similarity, 0.99, na.rm = TRUE)
mp_99th <- quantile(candidate_pairs_CLEA_cleaned$Metaphone_Similarity, 0.99, na.rm = TRUE)
mas_99th <- quantile(candidate_pairs_CLEA_cleaned$Masala_Similarity, 0.99, na.rm = TRUE)
ng_99th <- quantile(candidate_pairs_CLEA_cleaned$NGram_Similarity, 0.99, na.rm = TRUE)

candidate_pairs_CLEA_cleaned <- candidate_pairs_CLEA_cleaned %>%
  mutate(is_decoy = ifelse(
    Levenshtein_Similarity > lv_99th & 
      Jaro_Winkler_Similarity > jw_99th & 
      Metaphone_Similarity > mp_99th & 
      Masala_Similarity > mas_99th & 
      NGram_Similarity > ng_99th,
    TRUE, 
    FALSE
  ))

str(candidate_pairs_CLEA_cleaned)

#### CLEA Plots with edited subset ####
candidate_pairs_CLEA_cleaned <- candidate_pairs_CLEA_cleaned %>%
  filter(Election_ID != -999)

unique_elections_CLEA_no_ind <- candidate_pairs_CLEA_cleaned %>%
  dplyr::select(Country_Code, Year, Election_Month, Constituency_Name, Election_ID) %>%
  # summarize and count number of candidates per election
  group_by(Country_Code, Year, Election_Month, Constituency_Name, Election_ID) %>%
  summarise(
    num_candidates = n(),
    .groups = "drop"
  ) %>%
  distinct()

### plots of summary stats using yr, rg, ctr
# count elections by region and year and create heatmap of regional and temporal coverage
CLEA_cleaned_final <- CLEA_cleaned %>%
  filter(!(ctr_n == "US" & yr < 1960)) %>%
  filter(!(ctr_n == "US" & yr < 2012 & (sub == "delaware"|sub == "Delaware"))) %>%
  filter(!(ctr_n == "US" & yr < 2023 & (sub == "south carolina"|sub == "South Carolina"))) %>%
  filter(!(ctr_n == "US" & yr < 2000 & (sub == "south dakota"|sub == "South Dakota"))) %>%
  filter(sub != "new york" & sub != "New York" & 
           sub != "connecticut" & sub != "Connecticut")

coverage_data <- CLEA_cleaned_final %>%
  group_by(rg, yr) %>%
  dplyr::summarize(election_count = n_distinct(ctr, yr, mn, cst_n, id)) %>%
  ungroup()

ggplot(coverage_data, aes(x = yr, y = rg, fill = election_count)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Election Count") +
  theme_minimal() +
  labs(title = "Election Coverage by Region and Year",
       x = "Year", y = "Region") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "220425 meeting plots/CLEA_coverage.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# categorize candidates as main or minor
CLEA_cleaned_final <- CLEA_cleaned_final %>%
  mutate(candidate_type = ifelse(cvs1 > 0.1, "main", "minor"))

# boxplot of main/minor across regions
ggplot(CLEA_cleaned_final, aes(x = rg, y = cvs1, fill = candidate_type)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "Distribution of Vote Shares by Region",
       x = "Region", y = "Vote Share (%)",
       fill = "Candidate Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "220425 meeting plots/CLEA_main_minor_rates.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

### voteshare distribution across countries
# calculate total vote share by candidate type per year
voteshare <- CLEA_cleaned_final %>%
  group_by(yr, candidate_type) %>%
  dplyr::summarize(total_voteshare = sum(cvs1, na.rm = TRUE)) %>%
  ungroup()

# area chart
ggplot(voteshare, aes(x = yr, y = total_voteshare, fill = candidate_type)) +
  geom_area() +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(title = "Distribution of Vote Share Between Main and Minor Candidates",
       x = "Year", y = "Total Vote Share",
       fill = "Candidate Type")

ggsave(
  filename = "220425 meeting plots/CLEA_voteshare_main_minor.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

### False Positives
candidate_pairs_CLEA_cleaned <- candidate_pairs_CLEA_cleaned %>%
  mutate(
    Candidate1_Election_ID = paste(Year, Country_Code, Constituency_Name, Candidate1_Party_Name, Candidate1_Name, Candidate1_Votes, sep = "_"),
    Candidate2_Election_ID = paste(Year, Country_Code, Constituency_Name, Candidate2_Party_Name, Candidate2_Name, Candidate2_Votes, sep = "_")
  )

candidate_pairs_CLEA_cleaned_main_minor <- candidate_pairs_CLEA_cleaned %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main", 
         is_decoy == TRUE)

candidate_pairs_CLEA_cleaned_main_main <- candidate_pairs_CLEA_cleaned %>%
  filter(Pair_Type == "main-main", 
         is_decoy == TRUE)

candidate_pairs_CLEA_cleaned_minor_minor <- candidate_pairs_CLEA_cleaned %>%
  filter(Pair_Type == "minor-minor", 
         is_decoy == TRUE)

minor_candidates_that_are_decoys_of_minor <- unique(c(
  candidate_pairs_CLEA_cleaned_minor_minor$Candidate1_Election_ID,
  candidate_pairs_CLEA_cleaned_minor_minor$Candidate2_Election_ID
))

minor_candidates_that_are_decoys_of_main <- unique(c(
  candidate_pairs_CLEA_cleaned_main_minor %>% 
    filter(Pair_Type == "minor-main") %>% 
    pull(Candidate1_Election_ID),
  candidate_pairs_CLEA_cleaned_main_minor %>% 
    filter(Pair_Type == "main-minor") %>% 
    pull(Candidate2_Election_ID)
))

overlap_candidates <- intersect(minor_candidates_that_are_decoys_of_minor, minor_candidates_that_are_decoys_of_main)

length(overlap_candidates) / length(minor_candidates_that_are_decoys_of_minor) * 100
# 10% of minor-minor candidates were decoys of main-minor candidates

elections_with_main_minor_decoys <- candidate_pairs_CLEA_cleaned %>%
  filter((Pair_Type == "main-minor" | Pair_Type == "minor-main") & is_decoy == TRUE) %>%
  distinct(Country_Code, Year, Election_Month, Constituency_Name, Election_ID) %>%
  nrow()

elections_with_main_main_decoys <- candidate_pairs_CLEA_cleaned %>%
  filter(Pair_Type == "main-main" & is_decoy == TRUE) %>%
  distinct(Country_Code, Year, Election_Month, Constituency_Name, Election_ID) %>%
  nrow()

# remove overlap minor candidates! 
elections_with_minor_minor_decoys <- candidate_pairs_CLEA_cleaned %>%
  filter((Pair_Type == "minor-minor") & is_decoy == TRUE) %>%
  filter(!(Candidate1_Election_ID %in% minor_candidates_that_are_decoys_of_main | 
             Candidate2_Election_ID %in% minor_candidates_that_are_decoys_of_main)) %>%
  distinct(Country_Code, Year, Election_Month, Constituency_Name, Election_ID) %>%
  nrow()

total_elections <- candidate_pairs_CLEA_cleaned %>%
  distinct(Country_Code, Year, Election_Month, Constituency_Name, Election_ID) %>%
  nrow()

elections_with_decoys_df <- data.frame(
  Consolidated_Pair_Type = c("main-minor/minor-main", "main-main", "minor-minor"),
  pct_elections_with_decoys = c(
    elections_with_main_minor_decoys / total_elections * 100,
    elections_with_main_main_decoys / total_elections * 100,
    elections_with_minor_minor_decoys / total_elections * 100
  )
)

decoy_by_pair_type <- candidate_pairs_CLEA_cleaned %>%
  # creare a new consolidated pair type variable
  mutate(
    Consolidated_Pair_Type = case_when(
      Pair_Type == "main-minor" ~ "main-minor/minor-main",
      Pair_Type == "minor-main" ~ "main-minor/minor-main",
      TRUE ~ Pair_Type
    )
  ) %>%
  # in "minor-minor" collapse dont have minor candidates that are decoys of main
  filter(!(Consolidated_Pair_Type == "minor-minor" & 
             (Candidate1_Election_ID %in% minor_candidates_that_are_decoys_of_main | 
                Candidate2_Election_ID %in% minor_candidates_that_are_decoys_of_main))) %>%
  # group by the consolidated pair type
  group_by(Consolidated_Pair_Type) %>%
  # calculate summary statistics
  dplyr::summarize(
    total_pairs = n(),
    decoy_pairs = sum(is_decoy, na.rm = TRUE),
    pct_decoy = round(decoy_pairs / total_pairs * 100, 2),
    .groups = "drop"
  )

plot_data <- bind_rows(
  decoy_by_pair_type %>% 
    dplyr::select(Consolidated_Pair_Type, percentage = pct_decoy) %>%
    mutate(metric = "% of Pairs that are Decoys"),
  
  elections_with_decoys_df %>%
    dplyr::select(Consolidated_Pair_Type, percentage = pct_elections_with_decoys) %>%
    mutate(metric = "% of Elections with Decoys")
)

ggplot(plot_data, aes(x = Consolidated_Pair_Type, y = percentage, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.2f%%", percentage)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, max(plot_data$percentage) * 1.1)) +
  labs(title = "Decoy Pairs by Pair Type",
       x = "Pair Type",
       y = "Percentage",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "220425 meeting plots/Decoy_pairs_by_type_CLEA.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# are the overlaps really overlaps? 
overlap_candidates_df <- candidate_pairs_CLEA_cleaned %>%
  filter(Candidate1_Election_ID %in% overlap_candidates | 
           Candidate2_Election_ID %in% overlap_candidates) %>%
  dplyr::select(Year, Country_Code, Constituency_Name, 
                Candidate1_Name, Candidate1_Party_Name, Candidate1_Votes, Candidate1_Election_ID,
                Candidate2_Name, Candidate2_Party_Name, Candidate2_Votes, Candidate2_Election_ID,
                Pair_Type, is_decoy) %>%
  arrange(Year, Country_Code, Constituency_Name)

# view the first few rows
head(overlap_candidates_df)

# count how many times each overlap candidate appears
overlap_candidate_counts <- bind_rows(
  # count for Candidate1
  overlap_candidates_df %>%
    filter(Candidate1_Election_ID %in% overlap_candidates & is_decoy == TRUE) %>%
    count(Candidate1_Election_ID, Candidate1_Name, Candidate1_Party_Name) %>%
    rename(Election_ID = Candidate1_Election_ID, 
           Candidate_Name = Candidate1_Name, 
           Party_Name = Candidate1_Party_Name,
           appearance_count = n),
  
  # count for Candidate2
  overlap_candidates_df %>%
    filter(Candidate2_Election_ID %in% overlap_candidates & is_decoy == TRUE) %>%
    count(Candidate2_Election_ID, Candidate2_Name, Candidate2_Party_Name) %>%
    rename(Election_ID = Candidate2_Election_ID, 
           Candidate_Name = Candidate2_Name, 
           Party_Name = Candidate2_Party_Name,
           appearance_count = n)
) %>%
  distinct() %>%
  arrange(desc(appearance_count))

### summaries at election level
election_decoys <- candidate_pairs_CLEA_cleaned %>%
  group_by(Country_Code, Year, Election_Month, Constituency_Name, Election_ID) %>%
  summarise(
    main_main_decoys = sum(Pair_Type == "main-main" & is_decoy, na.rm = TRUE),
    main_minor_decoys = sum((Pair_Type == "main-minor" | Pair_Type == "minor-main") & is_decoy, na.rm = TRUE),
    minor_minor_decoys = sum(Pair_Type == "minor-minor" & is_decoy, na.rm = TRUE),
    .groups = "drop"
  )

sum(election_decoys$main_minor_decoys > 0)/nrow(election_decoys)
sum(election_decoys$main_main_decoys > 0)/nrow(election_decoys)
sum(election_decoys$minor_minor_decoys > 0)/nrow(election_decoys)

sum(candidate_pairs_CLEA_cleaned$is_decoy)/nrow(candidate_pairs_CLEA_cleaned)

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
    x = "Number of Main-Minor Decoys (>=)",
    y = "Number of Elections"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold")
  )

threshold_counts$percentage <- threshold_counts$elections_count / nrow(election_decoys) * 100

ggplot(threshold_counts, aes(x = factor(threshold), y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.2f%%", percentage)), vjust = -0.5, size = 3.5) +
  labs(
    title = "Percentage of Elections with Decoys above Thresholds",
    x = "Percentage of Main-Minor Decoys (At Least (>=))",
    y = "Percentage of Elections"
  ) +
  theme_minimal()

ggsave(
  filename = "220425 meeting plots/Main_Minor_Decoy_Histogram_CLEA.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# creating no. of candidates per election df from CLEA_cleaned_fr_no_ind
constituency_candidates <- CLEA_cleaned_final %>%
  group_by(ctr, yr, mn, cst_n, id) %>%
  dplyr::summarize(
    total_candidates = n(),
    .groups = "drop"
  ) %>%
  # rename columns to match candidate_pairs_CLEA
  rename(
    Country_Code = ctr,
    Year = yr,
    Election_Month = mn,
    Constituency_Name = cst_n,
    Election_ID = id
  )

constituency_metrics <- candidate_pairs_CLEA_cleaned %>%
  # create helper columns to identify winners and runner-ups
  group_by(Country_Code, Year, Election_Month, Constituency_Name, Election_ID) %>%
  mutate(
    # create candidate rankings based on voteshare
    Candidate1_Rank = rank(-Candidate1_VoteShare, ties.method = "min"),
    Candidate2_Rank = rank(-Candidate2_VoteShare, ties.method = "min")
  ) %>%
  dplyr::summarize(
    # for decoy metrics
    decoy_candidates = sum((is_decoy & (Pair_Type == "main-minor" | Pair_Type == "minor-main")), na.rm = TRUE),
    
    # vote shares for decoys
    decoy_vote_share = round(
      sum(ifelse(is_decoy & Pair_Type == "main-minor", Candidate2_VoteShare, 
                 ifelse(is_decoy & Pair_Type == "minor-main", Candidate1_VoteShare, 0)), 
          na.rm = TRUE), 2),
    
    # find winner and runner-up info
    winner_vote_share = max(c(Candidate1_VoteShare, Candidate2_VoteShare), na.rm = TRUE),
    winner_party = first(ifelse(Candidate1_VoteShare == max(c(Candidate1_VoteShare, Candidate2_VoteShare), na.rm = TRUE), 
                                Candidate1_Party_Name, Candidate2_Party_Name)),
    
    # for runner-up, we need to find the second highest vote share
    all_vote_shares = list(c(Candidate1_VoteShare, Candidate2_VoteShare)),
    
    # check if winner has decoys
    winner_has_decoys = any(
      (is_decoy & Pair_Type == "main-minor" & Candidate1_Rank == 1) | 
        (is_decoy & Pair_Type == "minor-main" & Candidate2_Rank == 1), 
      na.rm = TRUE),
    
    # check if runner-up has decoys
    runner_up_has_decoys = any(
      (is_decoy & Pair_Type == "main-minor" & Candidate1_Rank == 2) | 
        (is_decoy & Pair_Type == "minor-main" & Candidate2_Rank == 2), 
      na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  # join with constituency_candidates to get total_candidates
  left_join(constituency_candidates, 
            by = c("Country_Code", "Year", "Election_Month", "Constituency_Name", "Election_ID")) %>%
  # now we need to find runner-up vote share
  mutate(
    # add derived metrics
    decoy_share = round(decoy_candidates / total_candidates * 100, 2)
  ) %>%
  # process the all_vote_shares to get runner-up
  rowwise() %>%
  mutate(
    # sort vote shares and get the second highest
    sorted_shares = list(sort(unlist(all_vote_shares), decreasing = TRUE)),
    runner_up_vote_share = ifelse(length(sorted_shares) >= 2, sorted_shares[2], NA),
    # calculate winning margin
    winning_margin = winner_vote_share - runner_up_vote_share,
    winning_margin_percentage = winning_margin,
    # derived metrics
    close_race = ifelse(winning_margin_percentage < 5, TRUE, FALSE),
    decoy_impact_potential = ifelse(decoy_vote_share > winning_margin_percentage, TRUE, FALSE)
  ) %>%
  # clean up intermediate columns
  dplyr::select(-all_vote_shares, -sorted_shares) %>%
  # sort by the required order
  arrange(Country_Code, Constituency_Name, Year, Election_Month, Election_ID)

country_metrics <- constituency_metrics %>%
  group_by(Country_Code) %>%
  mutate(
    total_votes_to_decoys = sum(decoy_vote_share, na.rm = TRUE),
    total_votes = sum(winner_vote_share + runner_up_vote_share, na.rm = TRUE)
  ) %>%
  # Now calculate country-level metrics
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
    total_votes_to_decoys = sum(decoy_vote_share, na.rm = TRUE),
    total_votes = sum(winner_vote_share + runner_up_vote_share, na.rm = TRUE),
    overall_decoy_vote_share = (total_votes_to_decoys / total_votes) * 100,
    
    # decoys affect outcomes?
    races_with_decoys = sum(decoy_candidates > 0, na.rm = TRUE),
    races_with_3_decoys = sum(decoy_candidates >= 3, na.rm = TRUE),
    close_races = sum(close_race, na.rm = TRUE),
    decoy_impact_races = sum(decoy_impact_potential, na.rm = TRUE),
    
    # calculate races
    pct_races_with_decoys = (races_with_decoys / constituency_count) * 100,
    pct_races_with_3_decoys = (races_with_3_decoys / constituency_count) * 100,
    pct_close_races = (close_races / constituency_count) * 100,
    pct_decoy_impact = (decoy_impact_races / constituency_count) * 100,
    
    decoy_effectiveness = (overall_decoy_vote_share * decoy_impact_races) / constituency_count,
    .groups = "drop"
  )

country_codes <- CLEA_cleaned_final %>%
  dplyr::select(ctr, ctr_n) %>%
  distinct() %>%
  rename(Country_Code = ctr, Country = ctr_n)

country_metrics <- country_metrics %>%
  left_join(country_codes, by = "Country_Code")

country_elections <- unique_elections_CLEA_no_ind %>%
  group_by(Country_Code) %>%
  summarise(num_elections = n())

country_metrics <- country_metrics %>%
  left_join(country_elections, 
            by = "Country_Code")

# how many decoy elections, country
# limit to country with more than 10 constituencies i.e.
country_metrics <- country_metrics %>%
  filter(num_elections > 10)

country_metrics %>%
  # filter(pct_races_with_decoys > 0) %>%
  ggplot(aes(x = reorder(Country, -pct_races_with_decoys), y = pct_races_with_decoys)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.0f%%", pct_races_with_decoys)), 
            vjust = -0.5, size = 3) +
  labs(title = "Percentage of Elections with Decoys by Country",
       x = "Country",
       y = "Percentage of Elections with Decoys (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "220425 meeting plots/Decoy_elections_country_histogram_CLEA.png",
  plot = last_plot(),
  width = 16,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

country_metrics %>%
  ggplot(aes(x = reorder(Country, -pct_races_with_3_decoys), y = pct_races_with_3_decoys)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", pct_races_with_3_decoys)), 
            vjust = -0.5, size = 3) +
  labs(title = "Percentage of Elections with at least 3 Decoys by Country",
       x = "Country",
       y = "Percentage of Elections with at least 3 Decoys (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "220425 meeting plots/Decoy_elections_atleast3_country_histogram_CLEA.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

country_metrics %>%
  filter(mean_decoy_share > 0) %>%
  mutate(Country = fct_reorder(Country, -mean_decoy_share)) %>%
  ggplot(aes(x = Country, y = mean_decoy_share)) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", mean_decoy_share)), 
            vjust = -0.5, size = 3) +
  geom_errorbar(aes(ymin = median_decoy_share, ymax = p90_decoy_share), width = 0.2) +
  scale_y_continuous(
    breaks = seq(0, 5, by = 0.2),
    labels = scales::percent_format(scale = 1)
  ) + 
  labs(
    title = "Average Share of Decoy Candidates per Election by Country",
    subtitle = "With median (bottom of bar), 90th percentile (top of bar), and maximum (red dot)",
    x = "Country",
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
  filename = "220425 meeting plots/Decoy_candidate_country_CLEA.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

country_metrics %>%
  filter(mean_num_decoys > 0) %>%
  mutate(Country = fct_reorder(Country, -mean_num_decoys)) %>%
  ggplot(aes(x = Country, y = mean_num_decoys)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", mean_num_decoys)), 
            vjust = -0.5, size = 3) +
  geom_errorbar(aes(ymin = median_num_decoys, ymax = p90_num_decoys), width = 0.2) +
  labs(
    title = "Average Number of Decoy Candidates per Election by Country",
    subtitle = "With median (bottom of bar), 90th percentile (top of bar), and maximum (red dot)",
    x = "Country",
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
  filename = "220425 meeting plots/Decoy_candidates_number_country_CLEA.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

country_year_metrics <- constituency_metrics %>%
  group_by(Country_Code, Year) %>%
  summarise(
    decoy_proportion = mean(decoy_candidates/total_candidates, na.rm = TRUE),
    decoy_vote_share = mean(decoy_vote_share, na.rm = TRUE),
    .groups = "drop"
  )

country_year_metrics <- country_year_metrics %>%
  left_join(country_codes, by = "Country_Code") %>%
  mutate(
    Year = as.numeric(Year)
  )

country_year_metrics <- country_year_metrics %>%
  filter(Country %in% country_metrics$Country)

year_metrics <- country_year_metrics %>%
  group_by(Year) %>%
  dplyr::summarize(
    decoy_proportion = mean(decoy_proportion, na.rm = TRUE), 
    decoy_vote_share = mean(decoy_vote_share, na.rm = TRUE),
    .groups = "drop"
  )

year_metrics_binned <- country_year_metrics %>%
  mutate(
    Year_bin = 5 * floor(Year / 5)  # 5 year bins
  ) %>%
  group_by(Year_bin) %>%
  dplyr::summarize(
    decoy_proportion = mean(decoy_proportion, na.rm = TRUE), 
    decoy_vote_share = mean(decoy_vote_share, na.rm = TRUE),
    # store the range of years in each bin
    year_range = paste0(min(Year), "-", max(Year)),
    .groups = "drop"
  )

ggplot(year_metrics_binned, aes(x = Year_bin, y = decoy_proportion)) +
  geom_line(color = "red") +
  geom_point(color = "darkred") +
  theme_minimal() +
  labs(
    title = "Avg Decoy Proportion Over Time",
    x = "Year",
    y = "Average Decoy Proportion"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(
    breaks = seq(1800, 2025, by = 10)) + 
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

ggsave(
  filename = "220425 meeting plots/Decoy_share_over_time_CLEA.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

ggplot(year_metrics_binned, aes(x = Year_bin, y = decoy_vote_share)) +
  geom_line(color = "red") +
  geom_point(color = "darkred") +
  theme_minimal() +
  labs(
    title = "Avg Decoy Vote Share Over Time",
    x = "Year",
    y = "Average Decoy Vote Share"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(
    breaks = seq(1800, 2025, by = 10)) + 
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

ggsave(
  filename = "220425 meeting plots/Decoy_voteshare_over_time_CLEA.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

country_metrics %>%
  filter(overall_decoy_vote_share > 0) %>%
  # reorder
  mutate(Country = fct_reorder(Country, overall_decoy_vote_share)) %>%
  ggplot(aes(x = Country, y = overall_decoy_vote_share)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = sprintf("%.2f%%", overall_decoy_vote_share)), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Votes Captured by Decoy Candidates",
    subtitle = "Percentage of total votes going to decoy candidates by country",
    y = "Decoy Vote Share (%)",
    x = ""
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = "220425 meeting plots/Decoy_voteshare_by_country_CLEA.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

### Reallocating votes to victim
decoy_pairs <- candidate_pairs_CLEA_cleaned %>%
  filter(is_decoy == TRUE)

election_outcomes_changed <- data.frame()

# first a grouped version of decoy pairs to account for main with multiple decoys
grouped_decoys <- decoy_pairs %>%
  # get main and decoy candidates based on pair type, including name and party code
  mutate(
    main_name = case_when(
      Pair_Type == "main-minor" ~ Candidate1_Name,
      Pair_Type == "minor-main" ~ Candidate2_Name
    ),
    main_party_code = case_when(
      Pair_Type == "main-minor" ~ Candidate1_Party_Name,
      Pair_Type == "minor-main" ~ Candidate2_Party_Name
    ),
    decoy_name = case_when(
      Pair_Type == "main-minor" ~ Candidate2_Name,
      Pair_Type == "minor-main" ~ Candidate1_Name
    ),
    decoy_party_code = case_when(
      Pair_Type == "main-minor" ~ Candidate2_Party_Name,
      Pair_Type == "minor-main" ~ Candidate1_Party_Name
    ),
    # create composite identifiers
    main_id = paste(main_name, main_party_code, Election_ID),
    decoy_id = paste(decoy_name, decoy_party_code, Election_ID)
  ) %>%
  # keep only relevant pairs
  filter(!is.na(main_name), !is.na(decoy_name)) %>%
  # get unique election + main candidate combinations
  group_by(Country_Code, Year, Election_Month, Constituency_Name, Election_ID, main_id, main_name, main_party_code) %>%
  # collect all decoys for this main candidate
  dplyr::summarize(
    decoy_ids = list(decoy_id),
    decoy_names = list(decoy_name),
    decoy_party_codes = list(decoy_party_code),
    num_decoys = n(),
    .groups = "drop"
  )

# create a results dataframe
election_outcomes_changed <- data.frame()

# now process each main candidate with potentially multiple decoys
for (i in 1:nrow(grouped_decoys)) {
  # get the current record
  current_group <- grouped_decoys[i,]
  
  # extract identifiers
  main_candidate_id <- current_group$main_id
  main_candidate_name <- current_group$main_name
  main_candidate_party <- current_group$main_party_code
  decoy_candidate_ids <- unlist(current_group$decoy_ids)
  decoy_candidate_names <- unlist(current_group$decoy_names)
  decoy_candidate_parties <- unlist(current_group$decoy_party_codes)
  
  # debug info
  if (i %% 50 == 0) {
    cat("Processing main candidate", i, "of", nrow(grouped_decoys), 
        "with", current_group$num_decoys, "decoys\n")
  }
  
  # get election details using CLEA column names
  election_candidates <- CLEA_cleaned_final %>%
    filter(ctr == current_group$Country_Code,  # country code
           yr == current_group$Year,           # year
           mn == current_group$Election_Month, # election month
           cst_n == current_group$Constituency_Name, # constituency name
           # create a unique election ID if not directly available
           paste(ctr, yr, mn, cst_n, sep="_") == current_group$Election_ID)
  
  # create candidate identifiers for comparison
  election_candidates <- election_candidates %>%
    mutate(candidate_id = paste(can, pty_n, paste(ctr, yr, mn, cst_n, sep="_")))
  
  # check main candidate
  main_candidate_record <- election_candidates %>% 
    filter(candidate_id == main_candidate_id)
  
  # skip if main candidate not found uniquely
  if (nrow(main_candidate_record) != 1) {
    next
  }
  
  # get all decoy records
  decoy_candidate_records <- election_candidates %>%
    filter(candidate_id %in% decoy_candidate_ids)
  
  # skip if some decoys aren't found
  if (nrow(decoy_candidate_records) != length(decoy_candidate_ids)) {
    next
  }
  
  # calculate combined vote share of all decoys - using cvs1 (candidate vote share)
  total_decoy_vote_share <- sum(decoy_candidate_records$cvs1)
  
  # calculate new vote share for main candidate
  main_old_vote_share <- main_candidate_record$cvs1
  main_new_vote_share <- main_old_vote_share + total_decoy_vote_share
  
  # find the winner - determine position based on vote share if not available
  election_candidates_sorted <- election_candidates %>%
    arrange(desc(cvs1))
  
  position_lookup <- data.frame(
    candidate_id = election_candidates_sorted$candidate_id,
    Position = 1:nrow(election_candidates_sorted)
  )
  
  # Add position to the records
  main_candidate_record <- main_candidate_record %>%
    left_join(position_lookup, by = "candidate_id")
  
  # find the winner (position 1)
  winner_record <- election_candidates_sorted[1, ]
  
  # check if outcome would change
  if (main_candidate_record$Position == 1) {
    outcome_changed <- FALSE
  } else {
    outcome_changed <- main_new_vote_share > winner_record$cvs1
  }
  
  # record if outcome changed
  if (outcome_changed) {
    result_row <- data.frame(
      Country_Code = winner_record$ctr,
      Year = winner_record$yr,
      Election_Month = winner_record$mn,
      Constituency_Name = winner_record$cst_n,
      Election_ID = paste(winner_record$ctr, winner_record$yr, winner_record$mn, winner_record$cst_n, sep="_"),
      main_candidate = main_candidate_name,
      main_party = main_candidate_party,
      main_id = main_candidate_id,
      main_old_position = main_candidate_record$Position,
      main_old_vote_share = main_old_vote_share,
      num_decoys = current_group$num_decoys,
      total_decoy_vote_share = total_decoy_vote_share,
      main_new_vote_share = main_new_vote_share,
      winner = winner_record$can,
      winner_party = winner_record$pty_n,
      winner_vote_share = winner_record$cvs1,
      vote_difference = main_new_vote_share - winner_record$cvs1,
      stringsAsFactors = FALSE
    )
    
    election_outcomes_changed <- rbind(election_outcomes_changed, result_row)
  }
}

### NONE! so, none in LS India. 

#### CLEA SENSITIVITY ####
### Sensitivity of decoy numbers around 99th percentile
# function to analyze decoys at given percentile thresholds
# can do strict where all are above a certain threshold
# can do bit loose where 4/5 are above that threshold
# or even more loose where only 1/5 is above a threshold and 4/5 are above a lower threshold
analyze_decoys_at_threshold <- function(candidate_pairs_data, percentile, method = "strict", 
                                        lower_percentile = 0.974) {
  # calculate thresholds at the given percentile
  lv_threshold <- quantile(candidate_pairs_data$Levenshtein_Similarity, percentile, na.rm = TRUE)
  jw_threshold <- quantile(candidate_pairs_data$Jaro_Winkler_Similarity, percentile, na.rm = TRUE)
  mp_threshold <- quantile(candidate_pairs_data$Metaphone_Similarity, percentile, na.rm = TRUE)
  mas_threshold <- quantile(candidate_pairs_data$Masala_Similarity, percentile, na.rm = TRUE)
  ng_threshold <- quantile(candidate_pairs_data$NGram_Similarity, percentile, na.rm = TRUE)
  
  # calculate lower thresholds for all methods (even if not used)
  lv_lower_threshold <- quantile(candidate_pairs_data$Levenshtein_Similarity, lower_percentile, na.rm = TRUE)
  jw_lower_threshold <- quantile(candidate_pairs_data$Jaro_Winkler_Similarity, lower_percentile, na.rm = TRUE)
  mp_lower_threshold <- quantile(candidate_pairs_data$Metaphone_Similarity, lower_percentile, na.rm = TRUE)
  mas_lower_threshold <- quantile(candidate_pairs_data$Masala_Similarity, lower_percentile, na.rm = TRUE)
  ng_lower_threshold <- quantile(candidate_pairs_data$NGram_Similarity, lower_percentile, na.rm = TRUE)
  
  # define comparison operator
  compare_val <- function(value, threshold) {
    if (threshold == 1) value >= threshold else value > threshold
  }
  
  # apply the thresholds to determine decoys
  temp_candidate_pairs <- candidate_pairs_data %>%
    mutate(
      # count how many measures are above the higher threshold
      high_threshold_count = (
        compare_val(Levenshtein_Similarity, lv_threshold) +
          compare_val(Jaro_Winkler_Similarity, jw_threshold) +
          compare_val(Metaphone_Similarity, mp_threshold) +
          compare_val(Masala_Similarity, mas_threshold) +
          compare_val(NGram_Similarity, ng_threshold)
      ),
      
      # flag for each similarity measure above threshold
      lv_above_threshold = compare_val(Levenshtein_Similarity, lv_threshold),
      jw_above_threshold = compare_val(Jaro_Winkler_Similarity, jw_threshold),
      mp_above_threshold = compare_val(Metaphone_Similarity, mp_threshold),
      mas_above_threshold = compare_val(Masala_Similarity, mas_threshold),
      ng_above_threshold = compare_val(NGram_Similarity, ng_threshold),
      
      # determine if a pair is a decoy based on selected method
      is_decoy = case_when(
        method == "strict" ~ high_threshold_count == 5,
        method == "flexible_4of5" ~ (high_threshold_count >= 4) &
          (Levenshtein_Similarity >= lv_lower_threshold) &
          (Jaro_Winkler_Similarity >= jw_lower_threshold) &
          (Metaphone_Similarity >= mp_lower_threshold) &
          (Masala_Similarity >= mas_lower_threshold) &
          (NGram_Similarity >= ng_lower_threshold),
        method == "flexible_1of5" ~ (high_threshold_count >= 1) &
          (Levenshtein_Similarity >= lv_lower_threshold) &
          (Jaro_Winkler_Similarity >= jw_lower_threshold) &
          (Metaphone_Similarity >= mp_lower_threshold) &
          (Masala_Similarity >= mas_lower_threshold) &
          (NGram_Similarity >= ng_lower_threshold),
        method == "only_levenshtein" ~ lv_above_threshold,
        method == "only_jaro_winkler" ~ jw_above_threshold,
        method == "only_metaphone" ~ mp_above_threshold,
        method == "only_masala" ~ mas_above_threshold,
        method == "only_ngram" ~ ng_above_threshold,
        TRUE ~ FALSE
      )
    ) %>%
    dplyr::select(-high_threshold_count, 
                  -lv_above_threshold, -jw_above_threshold, -mp_above_threshold,
                  -mas_above_threshold, -ng_above_threshold)
  
  # metrics for main-minor types
  main_minor_pairs <- temp_candidate_pairs %>%
    filter(Pair_Type == "main-minor" | Pair_Type == "minor-main") %>%
    dplyr::summarize(
      total_pairs = n(),
      decoy_pairs = sum(is_decoy, na.rm = TRUE),
      pct_decoy = round(decoy_pairs / total_pairs * 100, 2),
      .groups = "drop"
    ) %>%
    mutate(Consolidated_Pair_Type = "main-minor/minor-main")
  
  # metrics for main-main
  main_main_pairs <- temp_candidate_pairs %>%
    filter(Pair_Type == "main-main") %>%
    dplyr::summarize(
      total_pairs = n(),
      decoy_pairs = sum(is_decoy, na.rm = TRUE),
      pct_decoy = round(decoy_pairs / total_pairs * 100, 2),
      .groups = "drop"
    ) %>%
    mutate(Consolidated_Pair_Type = "main-main")
  
  # metrics for minor-minor, handling overlaps
  minor_candidates_that_are_decoys_of_main <- unique(c(
    temp_candidate_pairs %>% 
      filter(Pair_Type == "minor-main", is_decoy == TRUE) %>% 
      pull(Candidate1_Election_ID),
    temp_candidate_pairs %>% 
      filter(Pair_Type == "main-minor", is_decoy == TRUE) %>% 
      pull(Candidate2_Election_ID)
  ))
  
  minor_minor_pairs <- temp_candidate_pairs %>%
    filter(Pair_Type == "minor-minor") %>%
    filter(!(Candidate1_Election_ID %in% minor_candidates_that_are_decoys_of_main | 
               Candidate2_Election_ID %in% minor_candidates_that_are_decoys_of_main)) %>%
    dplyr::summarize(
      total_pairs = n(),
      decoy_pairs = sum(is_decoy, na.rm = TRUE),
      pct_decoy = round(decoy_pairs / total_pairs * 100, 2),
      .groups = "drop"
    ) %>%
    mutate(Consolidated_Pair_Type = "minor-minor")
  
  # combine results
  results <- bind_rows(main_minor_pairs, main_main_pairs, minor_minor_pairs) %>%
    mutate(
      percentile = percentile,
      method = method,
      lower_percentile = case_when(
        method == "strict" ~ NA_real_,
        method %in% c("threshold_sensitivity_all", "any_one_measure",
                      "only_levenshtein", "only_jaro_winkler", 
                      "only_metaphone", "only_masala", "only_ngram") ~ NA_real_,
        TRUE ~ lower_percentile
      )
    )
  
  # also calculate percentage of elections with decoys
  total_elections <- temp_candidate_pairs %>%
    distinct(Year, Country_Code, Constituency_Name, Election_Month, Election_ID) %>%
    nrow()
  
  elections_with_main_minor <- temp_candidate_pairs %>%
    filter((Pair_Type == "main-minor" | Pair_Type == "minor-main") & is_decoy == TRUE) %>%
    distinct(Year, Country_Code, Constituency_Name, Election_Month, Election_ID) %>%
    nrow()
  
  elections_with_main_main <- temp_candidate_pairs %>%
    filter(Pair_Type == "main-main" & is_decoy == TRUE) %>%
    distinct(Year, Country_Code, Constituency_Name, Election_Month, Election_ID) %>%
    nrow()
  
  elections_with_minor_minor <- temp_candidate_pairs %>%
    filter(Pair_Type == "minor-minor" & is_decoy == TRUE) %>%
    filter(!(Candidate1_Election_ID %in% minor_candidates_that_are_decoys_of_main | 
               Candidate2_Election_ID %in% minor_candidates_that_are_decoys_of_main)) %>%
    distinct(Year, Country_Code, Constituency_Name, Election_Month, Election_ID) %>%
    nrow()
  
  elections_results <- data.frame(
    Consolidated_Pair_Type = c("main-minor/minor-main", "main-main", "minor-minor"),
    pct_elections = c(
      elections_with_main_minor / total_elections * 100,
      elections_with_main_main / total_elections * 100,
      elections_with_minor_minor / total_elections * 100
    ),
    percentile = percentile,
    method = method,
    lower_percentile = NA_real_
  )
  
  # set lower_percentile only for methods that use it
  if (!method %in% c("strict",
                     "only_levenshtein", "only_jaro_winkler", 
                     "only_metaphone", "only_masala", "only_ngram")) {
    elections_results$lower_percentile <- lower_percentile
  }
  
  # return both metrics
  return(list(
    pair_results = results,
    election_results = elections_results
  ))
}

percentiles <- seq(0.974, 1.000, by = 0.001)

# if need to load
sensitivity_results <- readRDS("sensitivity_analysis_results/clea_no_fusion_w_ind/sensitivity_results.rds")
lv_results <- readRDS("sensitivity_analysis_results/clea_no_fusion_w_ind/lv_results.rds")
jw_results <- readRDS("sensitivity_analysis_results/clea_no_fusion_w_ind/jw_results.rds")
ng_results <- readRDS("sensitivity_analysis_results/clea_no_fusion_w_ind/ng_results.rds")
mp_results <- readRDS("sensitivity_analysis_results/clea_no_fusion_w_ind/mp_results.rds")
mas_results <- readRDS("sensitivity_analysis_results/clea_no_fusion_w_ind/mas_results.rds")

# run the analysis for all percentiles (using purrr's map function)
sensitivity_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs_CLEA_cleaned, .x, method = "strict", lower_percentile = 0.974))

lv_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs_CLEA_cleaned, .x, method = "only_levenshtein"))

jw_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs_CLEA_cleaned, .x, method = "only_jaro_winkler"))

ng_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs_CLEA_cleaned, .x, method = "only_ngram"))

mp_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs_CLEA_cleaned, .x, method = "only_metaphone"))

mas_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs_CLEA_cleaned, .x, method = "only_masala"))

# get pair results and election results
pair_results <- map_dfr(sensitivity_results, ~.x$pair_results)
election_results <- map_dfr(sensitivity_results, ~.x$election_results)

# create a visualization for pair percentages
ggplot(pair_results, aes(x = percentile, y = pct_decoy, color = Consolidated_Pair_Type)) +
  geom_line() +
  geom_point() +
  # adjust y axis breaks
  scale_y_continuous(limits = c(0, max(pair_results$pct_decoy, na.rm = TRUE)),
                     breaks = seq(0, max(pair_results$pct_decoy, na.rm = TRUE), by = 0.05)) +
  # adjust x axis breaks
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold (CLEA)",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "220425 meeting plots/Sensitivity_of_decoy_pairs_clea.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

plot_data <- pair_results %>%
  # grp by percentile
  group_by(percentile) %>%
  # create the two categories we want
  dplyr::summarize(
    # one line for cross-type (main-minor and minor-main)
    cross_type = mean(pct_decoy[Consolidated_Pair_Type %in% c("main-minor/minor-main")]),
    # one line for the average of same-type (main-main and minor-minor)
    same_type = mean(pct_decoy[Consolidated_Pair_Type %in% c("main-main", "minor-minor")])
  ) %>%
  # convert to long format for plotting
  pivot_longer(
    cols = c(cross_type, same_type),
    names_to = "pair_category",
    values_to = "pct_decoy"
  ) %>%
  # make the labels more readable
  mutate(pair_category = factor(
    pair_category,
    levels = c("cross_type", "same_type"),
    labels = c("Main-Minor/Minor-Main", "Avg of Main-Main & Minor-Minor")
  ))

ggplot(plot_data, aes(x = percentile, y = pct_decoy, color = pair_category)) +
  geom_line(size = 1) +
  geom_point() +
  scale_y_continuous(limits = c(0, max(pair_results$pct_decoy, na.rm = TRUE)),
                     breaks = seq(0, max(pair_results$pct_decoy, na.rm = TRUE), by = 0.05)) +
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold (CLEA)",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "220425 meeting plots/Sensitivity_of_decoy_pairs_clea_2.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

ggplot(election_results, aes(x = percentile, y = pct_elections, color = Consolidated_Pair_Type)) +
  geom_line() +
  geom_point() +
  # adjust y axis breaks
  scale_y_continuous(breaks = seq(0, max(election_results$pct_elections), by = 0.5)) +
  # adjust x axis breaks
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold (CLEA)",
       subtitle = "Percentage of Elections with Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "220425 meeting plots/Sensitivity_of_decoy_elections_clea.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# plots for each var independently
# levenshtein
pair_results <- map_dfr(lv_results, ~.x$pair_results)
election_results <- map_dfr(lv_results, ~.x$election_results)

# create a visualization for pair percentages
ggplot(pair_results, aes(x = percentile, y = pct_decoy, color = Consolidated_Pair_Type)) +
  geom_line() +
  geom_point() +
  # adjust y axis breaks
  scale_y_continuous(limits = c(0, max(pair_results$pct_decoy, na.rm = TRUE)),
                     breaks = seq(0, max(pair_results$pct_decoy, na.rm = TRUE), by = 0.5)) +
  # adjust x axis breaks
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold (CLEA)",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above Threshold (CLEA)",
       x = "Similarity Score Percentile Threshold (CLEA)",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "220425 meeting plots/Sensitivity_of_decoy_pairs_lv_clea.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

plot_data <- pair_results %>%
  # grp by percentile
  group_by(percentile) %>%
  # create the two categories we want
  dplyr::summarize(
    # one line for cross-type (main-minor and minor-main)
    cross_type = mean(pct_decoy[Consolidated_Pair_Type %in% c("main-minor/minor-main")]),
    # one line for the average of same-type (main-main and minor-minor)
    same_type = mean(pct_decoy[Consolidated_Pair_Type %in% c("main-main", "minor-minor")])
  ) %>%
  # convert to long format for plotting
  pivot_longer(
    cols = c(cross_type, same_type),
    names_to = "pair_category",
    values_to = "pct_decoy"
  ) %>%
  # make the labels more readable
  mutate(pair_category = factor(
    pair_category,
    levels = c("cross_type", "same_type"),
    labels = c("Main-Minor/Minor-Main", "Avg of Main-Main & Minor-Minor")
  ))

ggplot(plot_data, aes(x = percentile, y = pct_decoy, color = pair_category)) +
  geom_line(size = 1) +
  geom_point() +
  scale_y_continuous(limits = c(0, max(pair_results$pct_decoy, na.rm = TRUE)),
                     breaks = seq(0, max(pair_results$pct_decoy, na.rm = TRUE), by = 0.5)) +
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold (CLEA)",
       x = "Similarity Score Percentile Threshold (CLEA)",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "220425 meeting plots/Sensitivity_of_decoy_pairs_lv_clea_2.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# create a visualization for election percentages
ggplot(election_results, aes(x = percentile, y = pct_elections, color = Consolidated_Pair_Type)) +
  geom_line() +
  geom_point() +
  # adjust y axis breaks
  scale_y_continuous(breaks = seq(0, max(election_results$pct_elections), by = 1)) +
  # adjust x axis breaks
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.002)) +
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold (CLEA)",
       subtitle = "Percentage of Elections with Decoys; Normalized Levenshtein above x-axis value",
       x = "Similarity Score Percentile Threshold (CLEA)",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "220425 meeting plots/Sensitivity_of_decoy_elections_lv_clea.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# jaro-winkler
pair_results <- map_dfr(jw_results, ~.x$pair_results)
election_results <- map_dfr(jw_results, ~.x$election_results)

# create a visualization for pair percentages
ggplot(pair_results, aes(x = percentile, y = pct_decoy, color = Consolidated_Pair_Type)) +
  geom_line() +
  geom_point() +
  # adjust y axis breaks
  scale_y_continuous(limits = c(0, max(pair_results$pct_decoy, na.rm = TRUE)),
                     breaks = seq(0, max(pair_results$pct_decoy, na.rm = TRUE), by = 0.5)) +
  # adjust x axis breaks
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold (CLEA)",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above Threshold (CLEA)",
       x = "Similarity Score Percentile Threshold (CLEA)",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "220425 meeting plots/Sensitivity_of_decoy_pairs_jw_clea.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

plot_data <- pair_results %>%
  # grp by percentile
  group_by(percentile) %>%
  # create the two categories we want
  dplyr::summarize(
    # one line for cross-type (main-minor and minor-main)
    cross_type = mean(pct_decoy[Consolidated_Pair_Type %in% c("main-minor/minor-main")]),
    # one line for the average of same-type (main-main and minor-minor)
    same_type = mean(pct_decoy[Consolidated_Pair_Type %in% c("main-main", "minor-minor")])
  ) %>%
  # convert to long format for plotting
  pivot_longer(
    cols = c(cross_type, same_type),
    names_to = "pair_category",
    values_to = "pct_decoy"
  ) %>%
  # make the labels more readable
  mutate(pair_category = factor(
    pair_category,
    levels = c("cross_type", "same_type"),
    labels = c("Main-Minor/Minor-Main", "Avg of Main-Main & Minor-Minor")
  ))

ggplot(plot_data, aes(x = percentile, y = pct_decoy, color = pair_category)) +
  geom_line(size = 1) +
  geom_point() +
  scale_y_continuous(lim = c(0, max(plot_data$pct_decoy, na.rm = TRUE)), 
                     breaks = seq(0, max(plot_data$pct_decoy, na.rm = TRUE), by = 0.5)) +
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold (CLEA)",
       x = "Similarity Score Percentile Threshold (CLEA)",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "220425 meeting plots/Sensitivity_of_decoy_pairs_jw_clea_2.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# create a visualization for election percentages
ggplot(election_results, aes(x = percentile, y = pct_elections, color = Consolidated_Pair_Type)) +
  geom_line() +
  geom_point() +
  # adjust y axis breaks
  scale_y_continuous(breaks = seq(0, max(election_results$pct_elections), by = 1)) +
  # adjust x axis breaks
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.002)) +
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold (CLEA)",
       subtitle = "Percentage of Elections with Decoys; Jaro-Winkler above x-axis value",
       x = "Similarity Score Percentile Threshold (CLEA)",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "220425 meeting plots/Sensitivity_of_decoy_elections_jw_clea.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# double metaphone
pair_results <- map_dfr(mp_results, ~.x$pair_results)
election_results <- map_dfr(mp_results, ~.x$election_results)

# create a visualization for pair percentages
ggplot(pair_results, aes(x = percentile, y = pct_decoy, color = Consolidated_Pair_Type)) +
  geom_line() +
  geom_point() +
  # adjust y axis breaks
  scale_y_continuous(limits = c(0, max(pair_results$pct_decoy, na.rm = TRUE)),
                     breaks = seq(0, max(pair_results$pct_decoy, na.rm = TRUE), by = 0.5)) +
  # adjust x axis breaks
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold (CLEA)",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above Threshold (CLEA)",
       x = "Similarity Score Percentile Threshold (CLEA)",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Type") +
  theme_minimal()

plot_data <- pair_results %>%
  # grp by percentile
  group_by(percentile) %>%
  # create the two categories we want
  dplyr::summarize(
    # one line for cross-type (main-minor and minor-main)
    cross_type = mean(pct_decoy[Consolidated_Pair_Type %in% c("main-minor/minor-main")]),
    # one line for the average of same-type (main-main and minor-minor)
    same_type = mean(pct_decoy[Consolidated_Pair_Type %in% c("main-main", "minor-minor")])
  ) %>%
  # convert to long format for plotting
  pivot_longer(
    cols = c(cross_type, same_type),
    names_to = "pair_category",
    values_to = "pct_decoy"
  ) %>%
  # make the labels more readable
  mutate(pair_category = factor(
    pair_category,
    levels = c("cross_type", "same_type"),
    labels = c("Main-Minor/Minor-Main", "Avg of Main-Main & Minor-Minor")
  ))

ggplot(plot_data, aes(x = percentile, y = pct_decoy, color = pair_category)) +
  geom_line(size = 1) +
  geom_point() +
  scale_y_continuous(limits = c(0, max(pair_results$pct_decoy, na.rm = TRUE)),
                     breaks = seq(0, max(pair_results$pct_decoy, na.rm = TRUE), by = 0.5)) +
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold (CLEA)",
       x = "Similarity Score Percentile Threshold (CLEA)",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "220425 meeting plots/Sensitivity_of_decoy_pairs_mp_clea_2.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# create a visualization for election percentages
ggplot(election_results, aes(x = percentile, y = pct_elections, color = Consolidated_Pair_Type)) +
  geom_line() +
  geom_point() +
  # adjust y axis breaks
  scale_y_continuous(limits = c(0, max(election_results$pct_elections, na.rm = TRUE)),
                     breaks = seq(0, max(election_results$pct_elections, na.rm = TRUE), by = 0.5)) +
  # adjust x axis breaks
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.002)) +
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold (CLEA)",
       subtitle = "Percentage of Elections with Decoys; Double Metaphone above x-axis value",
       x = "Similarity Score Percentile Threshold (CLEA)",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "220425 meeting plots/Sensitivity_of_decoy_elections_mp_clea.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# ngram
pair_results <- map_dfr(ng_results, ~.x$pair_results)
election_results <- map_dfr(ng_results, ~.x$election_results)

# create a visualization for pair percentages
ggplot(pair_results, aes(x = percentile, y = pct_decoy, color = Consolidated_Pair_Type)) +
  geom_line() +
  geom_point() +
  # adjust y axis breaks
  scale_y_continuous(limits = c(0, max(pair_results$pct_decoy, na.rm = TRUE)),
                     breaks = seq(0, max(pair_results$pct_decoy, na.rm = TRUE), by = 0.5)) +
  # adjust x axis breaks
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold (CLEA)",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above Threshold (CLEA)",
       x = "Similarity Score Percentile Threshold (CLEA)",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Type") +
  theme_minimal()

plot_data <- pair_results %>%
  # grp by percentile
  group_by(percentile) %>%
  # create the two categories we want
  dplyr::summarize(
    # one line for cross-type (main-minor and minor-main)
    cross_type = mean(pct_decoy[Consolidated_Pair_Type %in% c("main-minor/minor-main")]),
    # one line for the average of same-type (main-main and minor-minor)
    same_type = mean(pct_decoy[Consolidated_Pair_Type %in% c("main-main", "minor-minor")])
  ) %>%
  # convert to long format for plotting
  pivot_longer(
    cols = c(cross_type, same_type),
    names_to = "pair_category",
    values_to = "pct_decoy"
  ) %>%
  # make the labels more readable
  mutate(pair_category = factor(
    pair_category,
    levels = c("cross_type", "same_type"),
    labels = c("Main-Minor/Minor-Main", "Avg of Main-Main & Minor-Minor")
  ))

ggplot(plot_data, aes(x = percentile, y = pct_decoy, color = pair_category)) +
  geom_line(size = 1) +
  geom_point() +
  scale_y_continuous(limits = c(0, max(pair_results$pct_decoy, na.rm = TRUE)),
                     breaks = seq(0, max(pair_results$pct_decoy, na.rm = TRUE), by = 0.5)) +
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold (CLEA)",
       x = "Similarity Score Percentile Threshold (CLEA)",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "220425 meeting plots/Sensitivity_of_decoy_pairs_ng_clea_2.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# create a visualization for election percentages
ggplot(election_results, aes(x = percentile, y = pct_elections, color = Consolidated_Pair_Type)) +
  geom_line() +
  geom_point() +
  # adjust y axis breaks
  scale_y_continuous(limits = c(0, max(election_results$pct_elections, na.rm = TRUE)),
                     breaks = seq(0, max(election_results$pct_elections, na.rm = TRUE), by = 0.5)) +
  # adjust x axis breaks
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.002)) +
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold (CLEA)",
       subtitle = "Percentage of Elections with Decoys; NGram above x-axis value",
       x = "Similarity Score Percentile Threshold (CLEA)",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "220425 meeting plots/Sensitivity_of_decoy_elections_ng_clea.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# masala
pair_results <- map_dfr(mas_results, ~.x$pair_results)
election_results <- map_dfr(mas_results, ~.x$election_results)

# create a visualization for pair percentages
ggplot(pair_results, aes(x = percentile, y = pct_decoy, color = Consolidated_Pair_Type)) +
  geom_line() +
  geom_point() +
  # adjust y axis breaks
  scale_y_continuous(limits = c(0, max(pair_results$pct_decoy, na.rm = TRUE)),
                     breaks = seq(0, max(pair_results$pct_decoy, na.rm = TRUE), by = 0.5)) +
  # adjust x axis breaks
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold (CLEA)",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above Threshold (CLEA)",
       x = "Similarity Score Percentile Threshold (CLEA)",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Type") +
  theme_minimal()

plot_data <- pair_results %>%
  # grp by percentile
  group_by(percentile) %>%
  # create the two categories we want
  dplyr::summarize(
    # one line for cross-type (main-minor and minor-main)
    cross_type = mean(pct_decoy[Consolidated_Pair_Type %in% c("main-minor/minor-main")]),
    # one line for the average of same-type (main-main and minor-minor)
    same_type = mean(pct_decoy[Consolidated_Pair_Type %in% c("main-main", "minor-minor")])
  ) %>%
  # convert to long format for plotting
  pivot_longer(
    cols = c(cross_type, same_type),
    names_to = "pair_category",
    values_to = "pct_decoy"
  ) %>%
  # make the labels more readable
  mutate(pair_category = factor(
    pair_category,
    levels = c("cross_type", "same_type"),
    labels = c("Main-Minor/Minor-Main", "Avg of Main-Main & Minor-Minor")
  ))

ggplot(plot_data, aes(x = percentile, y = pct_decoy, color = pair_category)) +
  geom_line(size = 1) +
  geom_point() +
  scale_y_continuous(limits = c(0, max(pair_results$pct_decoy, na.rm = TRUE)),
                     breaks = seq(0, max(pair_results$pct_decoy, na.rm = TRUE), by = 0.5)) +
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold (CLEA)",
       x = "Similarity Score Percentile Threshold (CLEA)",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "220425 meeting plots/Sensitivity_of_decoy_pairs_mas_clea_2.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# create a visualization for election percentages
ggplot(election_results, aes(x = percentile, y = pct_elections, color = Consolidated_Pair_Type)) +
  geom_line() +
  geom_point() +
  # adjust y axis breaks
  scale_y_continuous(breaks = seq(0, max(election_results$pct_elections), by = 1)) +
  # adjust x axis breaks
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.002)) +
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold (CLEA)",
       subtitle = "Percentage of Elections with Decoys; Masala Merge above x-axis value",
       x = "Similarity Score Percentile Threshold (CLEA)",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "220425 meeting plots/Sensitivity_of_decoy_elections_mas_clea.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# Save each result set as an RDS file
saveRDS(sensitivity_results, "sensitivity_analysis_results/clea_no_fusion_w_ind/sensitivity_results.rds")
saveRDS(lv_results, "sensitivity_analysis_results/clea_no_fusion_w_ind/lv_results.rds")
saveRDS(jw_results, "sensitivity_analysis_results/clea_no_fusion_w_ind/jw_results.rds")
saveRDS(ng_results, "sensitivity_analysis_results/clea_no_fusion_w_ind/ng_results.rds")
saveRDS(mp_results, "sensitivity_analysis_results/clea_no_fusion_w_ind/mp_results.rds")
saveRDS(mas_results, "sensitivity_analysis_results/clea_no_fusion_w_ind/mas_results.rds")

#### CLEA FP RATES - COUNTRY SPLITS ####
minor_candidates_that_are_decoys_of_main <- unique(c(
  candidate_pairs_CLEA_cleaned_main_minor %>% 
    filter(Pair_Type == "minor-main") %>% 
    pull(Candidate1_Election_ID),
  candidate_pairs_CLEA_cleaned_main_minor %>% 
    filter(Pair_Type == "main-minor") %>% 
    pull(Candidate2_Election_ID)
))

calculate_country_false_positive_rates <- function(data, pair_type) {
  # unique election identifier from country, year, month and constituency
  data <- data %>%
    mutate(Combined_Election_ID = paste(Country_Code, Year, Election_Month, Constituency_Name, sep = "_"))
  # filter data for the specific pair type
  filtered_data <- data %>%
    # create a consolidated pair type
    mutate(
      Consolidated_Pair_Type = case_when(
        Pair_Type == "main-minor" ~ "main-minor/minor-main",
        Pair_Type == "minor-main" ~ "main-minor/minor-main",
        TRUE ~ Pair_Type
      )
    ) %>%
    # additional filtering for minor-minor to exclude minor candidates that are decoys of main
    filter(Consolidated_Pair_Type == pair_type) %>%
    filter(!(Consolidated_Pair_Type == "minor-minor" & 
               (Candidate1_Election_ID %in% minor_candidates_that_are_decoys_of_main | 
                  Candidate2_Election_ID %in% minor_candidates_that_are_decoys_of_main)))
  
  # calculate false positive rates and elections with decoys by country
  country_false_positive_rates <- filtered_data %>%
    group_by(Country_Code) %>%
    dplyr::summarize(
      total_pairs = n(),
      decoy_pairs = sum(is_decoy, na.rm = TRUE),
      country_false_positive_rate = round(decoy_pairs / total_pairs * 100, 2),
      elections_with_decoys = n_distinct(
        Combined_Election_ID[is_decoy == TRUE],
        na.rm = TRUE
      )
    )
  
  # calculate the overall false positive rate for this pair type
  overall_false_positive_rate <- filtered_data %>%
    dplyr::summarize(
      overall_false_positive_rate = round(sum(is_decoy, na.rm = TRUE) / n() * 100, 2)
    ) %>%
    pull(overall_false_positive_rate)
  
  # add a column for deviation from overall rate
  country_false_positive_rates <- country_false_positive_rates %>%
    mutate(
      deviation_from_baseline = round(country_false_positive_rate - overall_false_positive_rate, 2),
      baseline_false_positive_rate = overall_false_positive_rate
    )
  
  # add pair type column
  country_false_positive_rates$pair_type <- pair_type
  
  return(country_false_positive_rates)
}

# calculate false positive rates for each pair type
main_minor_false_positive_rates <- calculate_country_false_positive_rates(candidate_pairs_CLEA_cleaned, "main-minor/minor-main")
main_main_false_positive_rates <- calculate_country_false_positive_rates(candidate_pairs_CLEA_cleaned, "main-main")
minor_minor_false_positive_rates <- calculate_country_false_positive_rates(candidate_pairs_CLEA_cleaned, "minor-minor")

# combine results
combined_false_positive_rates <- bind_rows(
  main_minor_false_positive_rates,
  main_main_false_positive_rates,
  minor_minor_false_positive_rates
)

str(combined_false_positive_rates)

combined_false_positive_rates <- combined_false_positive_rates %>%
  left_join(country_codes, by = "Country_Code")

# plots
n_countries <- 20

# omit countries with less than 10 elections
combined_false_positive_rates <- combined_false_positive_rates %>%
  left_join(country_elections, by = "Country_Code")

combined_false_positive_rates_filtered <- combined_false_positive_rates %>%
  filter(num_elections >= 10)

# create a dataset for the top contributing countries (largest positive deviation)
top_contributors <- combined_false_positive_rates_filtered %>%
  group_by(pair_type) %>%
  top_n(n_countries, deviation_from_baseline) %>%  # select the top n by deviation
  ungroup()

# plot for top contributors using a horizontal bar chart
top_contributors <- top_contributors %>%
  mutate(Country_faceted = fct_reorder(
    interaction(Country, pair_type),  # treat each country-pair_type combo as unique
    deviation_from_baseline
  ))

ggplot(top_contributors, aes(x = Country_faceted, y = deviation_from_baseline)) +
  geom_bar(stat = "identity", fill = "tomato") +
  facet_wrap(~ pair_type, scales = "free_y", 
             labeller = labeller(pair_type = function(x) {
               paste0(x, "\nBaseline FP Rate: ", 
                      top_contributors$baseline_false_positive_rate[match(x, top_contributors$pair_type)])
             })) +
  # add decimal points for negative values to align them better
  geom_text(aes(label = ifelse(deviation_from_baseline < 0, 
                               paste0("-", sprintf("%.2f", abs(deviation_from_baseline))),
                               sprintf("%.2f", deviation_from_baseline))),
            hjust = ifelse(top_contributors$deviation_from_baseline >= 0, -0.1, 1.1),
            size = 3, nudge_y = ifelse(top_contributors$deviation_from_baseline >= 0, 0.05, -0.05)) +
  coord_flip() +
  labs(title = "Top Contributing Countries by Pair Type",
       x = "Country Code",
       y = "Deviation from Baseline False Positive Rate") +
  scale_x_discrete(labels = function(x) sapply(strsplit(as.character(x), "\\."), `[`, 1)) +
  theme_minimal() +
  # add borders to each panel
  theme(
    strip.background = element_rect(fill = "lightgray", color = "black", linewidth = 1),
    strip.text = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.spacing = unit(0.5, "cm"),
    # make axis text smaller to avoid crowding
    axis.text.y = element_text(size = 8)
  ) +
  # expand the plot boundaries significantly to accommodate labels
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.3)))

ggsave(
  filename = "220425 meeting plots/Contributors_to_FP_CLEA.png",
  plot = ,
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# pivot the data to compare different pair types side by side
pivoted_false_positive_rates <- combined_false_positive_rates_filtered %>%
  dplyr::select(Country_Code, Country, pair_type, country_false_positive_rate, baseline_false_positive_rate, num_elections) %>%
  pivot_wider(
    id_cols = c(Country_Code, Country, num_elections),
    names_from = pair_type,
    values_from = c(country_false_positive_rate, baseline_false_positive_rate)
  )

# calculate differentials between pair types
differential_analysis <- pivoted_false_positive_rates %>%
  mutate(
    # calculate differentials between different pair types
    main_minor_vs_minor_minor = `country_false_positive_rate_main-minor/minor-main` - `country_false_positive_rate_minor-minor`,
    main_minor_vs_main_main = `country_false_positive_rate_main-minor/minor-main` - `country_false_positive_rate_main-main`,
    main_main_vs_minor_minor = `country_false_positive_rate_main-main` - `country_false_positive_rate_minor-minor`,
    baseline_main_minor_vs_minor_minor = `baseline_false_positive_rate_main-minor/minor-main` - `baseline_false_positive_rate_minor-minor`,
    baseline_main_minor_vs_main_main = `baseline_false_positive_rate_main-minor/minor-main` - `baseline_false_positive_rate_main-main`,
    baseline_main_main_vs_minor_minor = `baseline_false_positive_rate_main-main` - `baseline_false_positive_rate_minor-minor`
  ) %>%
  mutate(
    main_minor_vs_minor_minor_vs_main_main = (main_minor_vs_main_main + main_minor_vs_minor_minor)/2,
    baseline_main_minor_vs_minor_minor_vs_main_main = (baseline_main_minor_vs_main_main + baseline_main_minor_vs_minor_minor)/2
  )

# plot distribution of differentials
# select top countries by absolute differential
top_differential <- differential_analysis %>%
  top_n(n_countries, abs(main_minor_vs_minor_minor_vs_main_main))

top_differential_minor_minor <- differential_analysis %>%
  top_n(n_countries, abs(main_minor_vs_minor_minor))

top_differential_main_main <- differential_analysis %>%
  top_n(n_countries, abs(main_minor_vs_main_main))

# plot the differential

str(top_differential)
ggplot(top_differential, aes(x = reorder(Country, main_minor_vs_minor_minor_vs_main_main), y = main_minor_vs_minor_minor_vs_main_main)) +
  geom_hline(yintercept = top_differential$baseline_main_minor_vs_minor_minor_vs_main_main[1], 
             linetype = "dashed", color = "blue", size = 0.5) +
  geom_bar(stat = "identity", 
           aes(fill = main_minor_vs_minor_minor_vs_main_main > 0)) +
  geom_text(aes(label = round(main_minor_vs_minor_minor_vs_main_main, 2)),
            hjust = ifelse(top_differential$main_minor_vs_minor_minor_vs_main_main > 0, -0.1, 1.1),
            size = 3) +
  # add a text annotation for the baseline
  annotate("text", x = 1, y = top_differential$baseline_main_minor_vs_minor_minor_vs_main_main[1], 
           label = paste("Baseline Difference:", round(top_differential$baseline_main_minor_vs_minor_minor_vs_main_main[1], 2)), 
           hjust = 0, vjust = -0.5, color = "blue") +
  coord_flip() +
  labs(title = "Differential: Main-Minor vs Minor-Minor/Main-Main False Positive Rates",
       subtitle = "",
       x = "Country",
       y = "Differential (Main-Minor minus Avg. for Main-Main & Minor-Minor)",
       fill = "Main-Minor Higher") +
  scale_fill_manual(values = c("tomato", "chartreuse")) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "220425 meeting plots/Main_minor_vs_other_FP_country_CLEA.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

ggplot(top_differential_minor_minor, aes(x = reorder(Country, main_minor_vs_minor_minor), y = main_minor_vs_minor_minor)) +
  geom_hline(yintercept = top_differential$baseline_main_minor_vs_minor_minor[1], 
             linetype = "dashed", color = "blue", size = 0.5) +
  geom_bar(stat = "identity", 
           aes(fill = main_minor_vs_minor_minor > 0)) +
  geom_text(aes(label = round(main_minor_vs_minor_minor, 2)),
            hjust = ifelse(top_differential_minor_minor$main_minor_vs_minor_minor > 0, -0.1, 1.1),
            size = 3) +
  # add a text annotation for the baseline
  annotate("text", x = 1, y = top_differential$baseline_main_minor_vs_minor_minor[1], 
           label = paste("Baseline Difference:", round(top_differential$baseline_main_minor_vs_minor_minor[1], 2)), 
           hjust = 0, vjust = -0.5, color = "blue") +
  coord_flip() +
  labs(title = "Differential: Main-Minor vs Minor-Minor False Positive Rates",
       subtitle = "Countries with at least 10 elections",
       x = "Country",
       y = "Differential (Main-Minor minus Minor-Minor)",
       fill = "Main-Minor Higher") +
  scale_fill_manual(values = c("tomato", "chartreuse")) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "220425 meeting plots/Main_minor_vs_minor_minor_FP_country_CLEA.png",
  plot = last_plot(), 
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

ggplot(top_differential_main_main, aes(x = reorder(Country, main_minor_vs_main_main), y = main_minor_vs_main_main)) +
  geom_hline(yintercept = top_differential$baseline_main_minor_vs_main_main[1], 
             linetype = "dashed", color = "blue", size = 0.5) +
  geom_bar(stat = "identity", 
           aes(fill = main_minor_vs_main_main > 0)) +
  geom_text(aes(label = round(main_minor_vs_main_main, 2)),
            hjust = ifelse(top_differential$main_minor_vs_main_main > 0, -0.1, 1.1),
            size = 3) +
  annotate("text", x = 1, y = top_differential_main_main$baseline_main_minor_vs_main_main[1], 
           label = paste("Baseline Difference:", round(top_differential$baseline_main_minor_vs_main_main[1], 2)), 
           hjust = 0, vjust = -0.5, color = "blue") +
  coord_flip() +
  labs(title = "Differential: Main-Minor vs Main-Main False Positive Rates",
       subtitle = "Countries with at least 10 elections",
       x = "Country",
       y = "Differential (Main-Minor minus Main-Main)",
       fill = "Main-Minor Higher") +
  scale_fill_manual(values = c("tomato", "chartreuse")) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "220425 meeting plots/Main_minor_vs_main_main_FP_country_CLEA.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# difference of difference from baseline
difference_of_differential_analysis <- differential_analysis %>%
  mutate(
    main_minor_vs_main_main_diff = main_minor_vs_main_main - baseline_main_minor_vs_main_main, 
    main_minor_vs_minor_minor_diff = main_minor_vs_minor_minor - baseline_main_minor_vs_minor_minor,
    main_minor_vs_minor_minor_vs_main_main_diff = main_minor_vs_minor_minor_vs_main_main - baseline_main_minor_vs_minor_minor_vs_main_main
  ) %>%
  dplyr::select(Country_Code, Country, main_minor_vs_main_main_diff, main_minor_vs_minor_minor_diff, main_minor_vs_minor_minor_vs_main_main_diff, 
                main_minor_vs_main_main, main_minor_vs_minor_minor, main_minor_vs_minor_minor_vs_main_main, 
                baseline_main_minor_vs_main_main, baseline_main_minor_vs_minor_minor, baseline_main_minor_vs_minor_minor_vs_main_main)

# reshape
long_differentials <- difference_of_differential_analysis %>%
  dplyr::select(Country_Code, Country, 
                main_minor_vs_main_main_diff, 
                main_minor_vs_minor_minor_diff, 
                main_minor_vs_minor_minor_vs_main_main_diff,
                baseline_main_minor_vs_main_main,
                baseline_main_minor_vs_minor_minor,
                baseline_main_minor_vs_minor_minor_vs_main_main) %>%
  pivot_longer(
    cols = c(main_minor_vs_main_main_diff, 
             main_minor_vs_minor_minor_diff, 
             main_minor_vs_minor_minor_vs_main_main_diff),
    names_to = "differential_type",
    values_to = "differential_value"
  ) %>%
  # create a mapping between differential types and their baseline values
  mutate(
    baseline_value = case_when(
      differential_type == "main_minor_vs_main_main_diff" ~ baseline_main_minor_vs_main_main,
      differential_type == "main_minor_vs_minor_minor_diff" ~ baseline_main_minor_vs_minor_minor,
      differential_type == "main_minor_vs_minor_minor_vs_main_main_diff" ~ baseline_main_minor_vs_minor_minor_vs_main_main,
      TRUE ~ NA_real_
    ),
    differential_type = case_when(
      differential_type == "main_minor_vs_main_main_diff" ~ "main-minor vs main-main",
      differential_type == "main_minor_vs_minor_minor_diff" ~ "main-minor vs minor-minor",
      differential_type == "main_minor_vs_minor_minor_vs_main_main_diff" ~ "main_minor vs avg. of main_main & minor_minor",
      TRUE ~ differential_type
    )
  )

# get top contributors for each differential type
n_countries <- 20
top_contributors_diff <- long_differentials %>%
  group_by(differential_type) %>%
  top_n(n_countries, abs(differential_value)) %>%  # Select top n by absolute deviation
  arrange(differential_type, desc(differential_value)) %>%
  ungroup()

# create a unique identifier for each country-differential_type combination
top_contributors_diff <- top_contributors_diff %>%
  mutate(Country_faceted = fct_reorder(
    interaction(Country, differential_type),
    differential_value
  ))

# plot
ggplot(top_contributors_diff, aes(x = Country_faceted, y = differential_value)) +
  geom_bar(stat = "identity", fill = "tomato") +
  facet_wrap(~ differential_type, scales = "free_y", 
             labeller = labeller(differential_type = function(x) {
               first_match <- top_contributors_diff %>% 
                 filter(differential_type == x) %>% 
                 slice(1)
               paste0(x, "\nBaseline Value: ", sprintf("%.2f", first_match$baseline_value))
             })) +
  geom_text(aes(label = sprintf("%.2f", differential_value)),
            hjust = ifelse(top_contributors_diff$differential_value >= 0, -0.1, 1.1),
            size = 3, nudge_y = ifelse(top_contributors_diff$differential_value >= 0, 0.05, -0.05)) +
  coord_flip() +
  labs(title = "Top Contributing Countries by Differential Type",
       x = "Country",
       y = "Differential Value") +
  scale_x_discrete(labels = function(x) sapply(strsplit(as.character(x), "\\."), `[`, 1)) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lightgray", color = "black", linewidth = 1),
    strip.text = element_text(face = "bold", size = 8),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.spacing = unit(0.5, "cm"),
    axis.text.y = element_text(size = 8)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.3)))

ggsave(
  filename = "220425 meeting plots/Main_minor_other_FP_diff_country_CLEA.png",
  plot = last_plot(),
  width = 16,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

#### Candidate Level Regressions ####
## Summary Statistics
# make minor minor decoys not decoys
candidate_level_with_WDI_minor_2 <- candidate_level_with_WDI_minor %>%
  mutate(
    is_decoy = ifelse(decoy_relationships == "minor-minor", FALSE, )
  )

str(candidate_level_with_WDI_minor)

summary_stats <- candidate_level_with_WDI_minor %>%
  summarise(
    Years = n_distinct(Year, na.rm = TRUE),
    Countries = n_distinct(Country_Code, na.rm = TRUE),
    Decoys = mean(is_decoy, na.rm = TRUE),
    
    # candidate Variables
    mean_vote_share = mean(Candidate_VoteShare, na.rm = TRUE),
    sd_vote_share = sd(Candidate_VoteShare, na.rm = TRUE),
    var_vote_share = var(Candidate_VoteShare, na.rm = TRUE),
    
    # name Characteristics
    mean_name_length = mean(name_length, na.rm = TRUE),
    sd_name_length = sd(name_length, na.rm = TRUE),
    mean_name_length_z = mean(name_length_z_score, na.rm = TRUE),
    sd_name_length_z = sd(name_length_z_score, na.rm = TRUE),
    mean_common_component = mean(has_common_component, na.rm = TRUE),
    sd_common_component = sd(has_common_component, na.rm = TRUE),
    
    # gov
    mean_gov_effect = mean(GE.PER.RNK, na.rm = TRUE),
    sd_gov_effect = sd(GE.PER.RNK, na.rm = TRUE),
    mean_rule_law = mean(RL.PER.RNK, na.rm = TRUE),
    sd_rule_law = sd(RL.PER.RNK, na.rm = TRUE),
    mean_pol_stability = mean(PV.PER.RNK, na.rm = TRUE),
    sd_pol_stability = sd(PV.PER.RNK, na.rm = TRUE),
    
    # econ
    mean_gdp_pc = mean(NY.GDP.PCAP.KD, na.rm = TRUE),
    sd_gdp_pc = sd(NY.GDP.PCAP.KD, na.rm = TRUE),
    mean_rural_pop = mean(SP.RUR.TOTL.ZS, na.rm = TRUE),
    sd_rural_pop = sd(SP.RUR.TOTL.ZS, na.rm = TRUE),
    mean_youth_labor = mean(SL.TLF.ACTI.1524.ZS, na.rm = TRUE),
    sd_youth_labor = sd(SL.TLF.ACTI.1524.ZS, na.rm = TRUE),
    mean_inflation = mean(NY.GDP.DEFL.KD.ZG, na.rm = TRUE),
    sd_inflation = sd(NY.GDP.DEFL.KD.ZG, na.rm = TRUE),
    mean_military_exp = mean(MS.MIL.XPND.ZS, na.rm = TRUE),
    sd_military_exp = sd(MS.MIL.XPND.ZS, na.rm = TRUE),
    mean_tax_revenue = mean(GC.TAX.TOTL.GD.ZS, na.rm = TRUE),
    sd_tax_revenue = sd(GC.TAX.TOTL.GD.ZS, na.rm = TRUE),
    
    mean_edu_exp = mean(SE.XPD.TOTL.GD.ZS, na.rm = TRUE),
    sd_edu_exp = sd(SE.XPD.TOTL.GD.ZS, na.rm = TRUE),
    
    mean_electricity = mean(EG.ELC.ACCS.ZS, na.rm = TRUE),
    sd_electricity = sd(EG.ELC.ACCS.ZS, na.rm = TRUE),
    
    total_obs = n()
  )

summary_data <- candidate_level_with_WDI_minor %>%
  dplyr::select(
    Candidate_VoteShare, is_decoy,
    name_length, name_length_z_score, 
    
    has_common_component,
    
    GE.PER.RNK, RL.PER.RNK, PV.PER.RNK, NY.GDP.PCAP.KD, 
    SP.RUR.TOTL.ZS, SL.TLF.ACTI.1524.ZS, NY.GDP.DEFL.KD.ZG,
    MS.MIL.XPND.ZS, GC.TAX.TOTL.GD.ZS, SE.XPD.TOTL.GD.ZS,
    EG.ELC.ACCS.ZS
  )

stargazer(as.data.frame(summary_data),  # explicitly convert to data.frame
          type = "latex",
          title = "Summary Statistics for Minor Candidate Dataset",
          digits = 3,
          align = TRUE,
          mean.sd = TRUE,
          median = FALSE,
          min.max = TRUE,
          covariate.labels = c(
            # candidate Variables
            "Candidate Vote Share",
            "Is Decoy Candidate",
            
            # name Characteristics
            "Name Length",
            "Name Length Deviation (z-score)",
            "Name has common sub-string",
            
            "Government Effectiveness (Percentile Rank)",
            "Rule of Law (Percentile Rank)",
            "Political Stability (Percentile Rank)",
          
            "GDP Per Capita",
            "Rural Population (Perc)",
            "Youth Labor Force Participation (15-24 yrs, Perc)",
            "Inflation Rate (GDP Deflator, Perc)",
            "Military Expenditure (Perc of Gov. Exp.)",
            "Tax Revenue (Perc of GDP)",
            
            "Government Education Expenditure (Perc of GDP)",
            
            "Access to Electricity (Perc of Pop)"
          ),
          notes = "This table presents summary statistics for the candidate-level minor party dataset. Vote share is measured as percentage of total votes. Name characteristics are calculated based on candidate names across all countries.",
          notes.align = "l")

## OLS
candidate_level_with_WDI_minor$constituency_state_country
candidate_level_with_WDI_minor$is_decoy

candidate_level_with_WDI_minor <- candidate_level_with_WDI_minor %>%
  mutate(
    has_common_component_bin = ifelse(has_common_component == TRUE, 1, 0), 
    is_decoy_bin = ifelse(is_decoy == TRUE, 1, 0)
  )

candidate_level_with_WDI_minor_2 <- candidate_level_with_WDI_minor_2 %>%
  mutate(
    has_common_component_bin = ifelse(has_common_component == TRUE, 1, 0), 
    is_decoy_bin = ifelse(is_decoy == TRUE, 1, 0)
  )

reg_lpm <- feols(
  is_decoy ~ GE.PER.RNK + RL.PER.RNK + PV.PER.RNK + NY.GDP.PCAP.KD + 
    SP.RUR.TOTL.ZS + SL.TLF.ACTI.1524.ZS + NY.GDP.DEFL.KD.ZG +
    MS.MIL.XPND.ZS + GC.TAX.TOTL.GD.ZS + SE.XPD.TOTL.GD.ZS +
    EG.ELC.ACCS.ZS + name_length + has_common_component_bin + name_length_z_score | Country_Name + Year,
  data = candidate_level_with_WDI_minor, 
  cluster = "constituency_state_country"
)
reg_lpm

# Define the regression models with different Fixed Effects specifications
reg_specs <- list(
  "No FE" = "~ GE.PER.RNK + RL.PER.RNK + PV.PER.RNK + NY.GDP.PCAP.KD + 
    SP.RUR.TOTL.ZS + SL.TLF.ACTI.1524.ZS + NY.GDP.DEFL.KD.ZG +
    MS.MIL.XPND.ZS + GC.TAX.TOTL.GD.ZS + SE.XPD.TOTL.GD.ZS +
    EG.ELC.ACCS.ZS + name_length + has_common_component_bin + name_length_z_score",
  
  "Country FE" = "~ GE.PER.RNK + RL.PER.RNK + PV.PER.RNK + NY.GDP.PCAP.KD + 
    SP.RUR.TOTL.ZS + SL.TLF.ACTI.1524.ZS + NY.GDP.DEFL.KD.ZG +
    MS.MIL.XPND.ZS + GC.TAX.TOTL.GD.ZS + SE.XPD.TOTL.GD.ZS +
    EG.ELC.ACCS.ZS + name_length + has_common_component_bin + name_length_z_score | Country_Name",
  
  "Year FE" = "~ GE.PER.RNK + RL.PER.RNK + PV.PER.RNK + NY.GDP.PCAP.KD + 
    SP.RUR.TOTL.ZS + SL.TLF.ACTI.1524.ZS + NY.GDP.DEFL.KD.ZG +
    MS.MIL.XPND.ZS + GC.TAX.TOTL.GD.ZS + SE.XPD.TOTL.GD.ZS +
    EG.ELC.ACCS.ZS + name_length + has_common_component_bin + name_length_z_score | Year",
  
  "Candidate FE" = "~ GE.PER.RNK + RL.PER.RNK + PV.PER.RNK + NY.GDP.PCAP.KD + 
    SP.RUR.TOTL.ZS + SL.TLF.ACTI.1524.ZS + NY.GDP.DEFL.KD.ZG +
    MS.MIL.XPND.ZS + GC.TAX.TOTL.GD.ZS + SE.XPD.TOTL.GD.ZS +
    EG.ELC.ACCS.ZS | Candidate_Name",
  
  "Candidate*Country FE" = "~ GE.PER.RNK + RL.PER.RNK + PV.PER.RNK + NY.GDP.PCAP.KD + 
    SP.RUR.TOTL.ZS + SL.TLF.ACTI.1524.ZS + NY.GDP.DEFL.KD.ZG +
    MS.MIL.XPND.ZS + GC.TAX.TOTL.GD.ZS + SE.XPD.TOTL.GD.ZS +
    EG.ELC.ACCS.ZS| Candidate_Name^Country_Name",
  
  "Country+Year FE" = "~ GE.PER.RNK + RL.PER.RNK + PV.PER.RNK + NY.GDP.PCAP.KD + 
    SP.RUR.TOTL.ZS + SL.TLF.ACTI.1524.ZS + NY.GDP.DEFL.KD.ZG +
    MS.MIL.XPND.ZS + GC.TAX.TOTL.GD.ZS + SE.XPD.TOTL.GD.ZS +
    EG.ELC.ACCS.ZS + name_length + has_common_component_bin + name_length_z_score | Country_Name + Year",
  
  "Candidate*Country+Year FE" = "~ GE.PER.RNK + RL.PER.RNK + PV.PER.RNK + NY.GDP.PCAP.KD + 
    SP.RUR.TOTL.ZS + SL.TLF.ACTI.1524.ZS + NY.GDP.DEFL.KD.ZG +
    MS.MIL.XPND.ZS + GC.TAX.TOTL.GD.ZS + SE.XPD.TOTL.GD.ZS +
    EG.ELC.ACCS.ZS| Candidate_Name^Country_Name + Year"
)

# Initialize the list to hold all regression models
decoy_models <- list()

# Run all regressions and store in the list
for(name in names(reg_specs)) {
  decoy_models[[name]] <- feols(
    as.formula(paste("is_decoy_bin", reg_specs[[name]])),
    data = candidate_level_with_WDI_minor,
    cluster = "constituency_state_country"
  )
}

# Function to create a formatted results table
create_regression_results_table <- function(models_list) {
  results_df <- data.frame(
    Variable = character(0),
    `(1)` = character(0),
    `(2)` = character(0),
    `(3)` = character(0),
    `(4)` = character(0),
    `(5)` = character(0),
    `(6)` = character(0),
    `(7)` = character(0),
    stringsAsFactors = FALSE
  )
  
  # Helper function to add stars based on p-value
  add_stars <- function(p_value) {
    if (is.na(p_value)) return("")
    if (p_value < 0.01) return("***")
    if (p_value < 0.05) return("**")
    if (p_value < 0.1) return("*")
    return("")
  }
  
  # Variables to extract (all variables in the model)
  vars_to_extract <- c(
    "GE.PER.RNK", "RL.PER.RNK", "PV.PER.RNK", "NY.GDP.PCAP.KD",
    "SP.RUR.TOTL.ZS", "SL.TLF.ACTI.1524.ZS", "NY.GDP.DEFL.KD.ZG",
    "MS.MIL.XPND.ZS", "GC.TAX.TOTL.GD.ZS", "SE.XPD.TOTL.GD.ZS",
    "EG.ELC.ACCS.ZS", "name_length", "has_common_component_bin", "name_length_z_score"
  )
  
  # Map variable names to their display labels
  var_labels <- c(
    "GE.PER.RNK" = "Government Effectiveness (Percentile Rank)",
    "RL.PER.RNK" = "Rule of Law (Percentile Rank)",
    "PV.PER.RNK" = "Political Stability (Percentile Rank)",
    "NY.GDP.PCAP.KD" = "GDP Per Capita",
    "SP.RUR.TOTL.ZS" = "Rural Population (Perc)",
    "SL.TLF.ACTI.1524.ZS" = "Youth Labor Force Participation (15-24 yrs, Perc)",
    "NY.GDP.DEFL.KD.ZG" = "Inflation Rate (GDP Deflator, Perc)",
    "MS.MIL.XPND.ZS" = "Military Expenditure (Perc of Gov. Exp.)",
    "GC.TAX.TOTL.GD.ZS" = "Tax Revenue (Perc of GDP)",
    "SE.XPD.TOTL.GD.ZS" = "Government Education Expenditure (Perc of GDP)",
    "EG.ELC.ACCS.ZS" = "Access to Electricity (Perc of Population)",
    "name_length" = "Name Length",
    "has_common_component_bin" = "Name has common sub-string",
    "name_length_z_score" = "Name Length Deviation (z-score)"
  )
  
  # Model names
  model_names <- names(models_list)
  
  # Create rows for coefficients and SEs
  for (var in vars_to_extract) {
    # Use the variable label if available, otherwise use the variable name
    var_label <- ifelse(var %in% names(var_labels), var_labels[var], var)
    
    coef_row <- data.frame(
      Variable = var_label,
      matrix(NA, nrow = 1, ncol = 7),
      stringsAsFactors = FALSE
    )
    se_row <- data.frame(
      Variable = "",  # Leave blank for SE row
      matrix(NA, nrow = 1, ncol = 7),
      stringsAsFactors = FALSE
    )
    names(coef_row) <- names(results_df)
    names(se_row) <- names(results_df)
    
    # Fill in coefficients and SEs
    for (i in 1:7) {
      model_name <- model_names[i]
      if (model_name %in% names(models_list)) {
        tryCatch({
          model <- models_list[[model_name]]
          if (var %in% names(coef(model))) {
            coef_val <- coef(model)[var]
            se_val <- sqrt(vcov(model)[var, var])
            p_val <- model$coeftable[var, "Pr(>|t|)"]
            stars <- add_stars(p_val)
            
            coef_row[1, i + 1] <- sprintf("%.5f%s", coef_val, stars)
            se_row[1, i + 1] <- sprintf("(%.5f)", se_val)
          }
        }, error = function(e) {
          warning(sprintf("Could not get coefficient for %s in model %s", var, model_name))
        })
      }
    }
    
    results_df <- rbind(results_df, coef_row, se_row)
  }
  
  # Add an empty row
  empty_row <- data.frame(
    Variable = "",
    matrix("", nrow = 1, ncol = 7),
    stringsAsFactors = FALSE
  )
  names(empty_row) <- names(results_df)
  results_df <- rbind(results_df, empty_row)
  
  # Add F-statistics row
  fstat_row <- data.frame(
    Variable = "F-statistic",
    matrix(NA, nrow = 1, ncol = 7),
    stringsAsFactors = FALSE
  )
  names(fstat_row) <- names(results_df)
  
  # Add R-squared row
  rsq_row <- data.frame(
    Variable = "R-squared",
    matrix(NA, nrow = 1, ncol = 7),
    stringsAsFactors = FALSE
  )
  names(rsq_row) <- names(results_df)
  
  # Add Observations row
  obs_row <- data.frame(
    Variable = "Observations",
    matrix(NA, nrow = 1, ncol = 7),
    stringsAsFactors = FALSE
  )
  names(obs_row) <- names(results_df)
  
  # Fill in statistics
  for (i in 1:7) {
    model_name <- model_names[i]
    
    if (model_name %in% names(models_list)) {
      model <- models_list[[model_name]]
      tryCatch({
        # F-stat
        f_stat <- fitstat(model, "f.stat")
        fstat_row[1, i + 1] <- sprintf("%.5f", f_stat)
        
        # R2
        r_squared <- fitstat(model, "r2")
        rsq_row[1, i + 1] <- sprintf("%.5f", r_squared)
        
        # Observations
        obs <- nobs(model)
        obs_row[1, i + 1] <- sprintf("%d", obs)
        
      }, error = function(e) {
        warning(sprintf("Could not get statistics for model %s", model_name))
      })
    }
  }
  
  # Add all statistics rows to the results
  results_df <- rbind(results_df, fstat_row, rsq_row, obs_row)
  
  # Add additional rows for Fixed Effects information
  fe_country_row <- data.frame(
    Variable = "Country FE",
    `(1)` = "No",
    `(2)` = "Yes",
    `(3)` = "No",
    `(4)` = "No",
    `(5)` = "Yes",
    `(6)` = "Yes",
    `(7)` = "Yes",
    stringsAsFactors = FALSE
  )
  
  fe_year_row <- data.frame(
    Variable = "Year FE",
    `(1)` = "No",
    `(2)` = "No",
    `(3)` = "Yes",
    `(4)` = "No",
    `(5)` = "No",
    `(6)` = "Yes",
    `(7)` = "Yes",
    stringsAsFactors = FALSE
  )
  
  fe_candidate_row <- data.frame(
    Variable = "Candidate FE",
    `(1)` = "No",
    `(2)` = "No",
    `(3)` = "No",
    `(4)` = "Yes",
    `(5)` = "Yes",
    `(6)` = "No",
    `(7)` = "Yes",
    stringsAsFactors = FALSE
  )
  
  results_df <- rbind(results_df, fe_country_row, fe_year_row, fe_candidate_row)
  
  return(results_df)
}

# Create the formatted table
decoy_results_table <- create_regression_results_table(decoy_models)

# Format the table using xtable (optional - can be replaced with your preferred table format)
library(xtable)
create_formatted_table <- function(results_df, title, notes = NULL) {
  xtab <- xtable(results_df,
                 caption = title,
                 align = c("l", rep("c", ncol(results_df))))
  
  if (!is.null(notes)) {
    caption(xtab) <- paste0(title, "\\\\", notes)
  }
  
  print(xtab,
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x) x)
}

# Create the final formatted table
create_formatted_table(
  decoy_results_table, 
  "The Impact of Governance and Socioeconomic Factors on Political Decoy Candidates", 
  "Notes: *** p<0.01, ** p<0.05, * p<0.1. Standard errors in parentheses are clustered at the constituency-state-country level. The dependent variable is a binary indicator for whether a candidate is a decoy. Columns present different fixed effects specifications: (1) No fixed effects, (2) Country fixed effects, (3) Year fixed effects, (4) Candidate fixed effects, (5) Candidate×Country fixed effects, (6) Country + Year fixed effects, (7) Candidate×Country + Year fixed effects. Name characteristics are calculated based on candidate names across all countries."
)

## Logit
reg_log <- feglm(
  is_decoy_bin ~
    GE.PER.RNK + RL.PER.RNK + PV.PER.RNK + NY.GDP.PCAP.KD + 
    SP.RUR.TOTL.ZS + SL.TLF.ACTI.1524.ZS + NY.GDP.DEFL.KD.ZG +
    MS.MIL.XPND.ZS + GC.TAX.TOTL.GD.ZS + SE.XPD.TOTL.GD.ZS +
    EG.ELC.ACCS.ZS| Candidate_Name^Country_Name + Year,
  family = "logit",
  data = candidate_level_with_WDI_minor, 
  cluster = ~ constituency_state_country
)
reg_log

# Define the regression models with different Fixed Effects specifications
reg_specs <- list(
  "No FE" = "~ GE.PER.RNK + RL.PER.RNK + PV.PER.RNK + NY.GDP.PCAP.KD + 
    SP.RUR.TOTL.ZS + SL.TLF.ACTI.1524.ZS + NY.GDP.DEFL.KD.ZG +
    MS.MIL.XPND.ZS + GC.TAX.TOTL.GD.ZS + SE.XPD.TOTL.GD.ZS +
    EG.ELC.ACCS.ZS + name_length + has_common_component_bin + name_length_z_score",
  
  "Country FE" = "~ GE.PER.RNK + RL.PER.RNK + PV.PER.RNK + NY.GDP.PCAP.KD + 
    SP.RUR.TOTL.ZS + SL.TLF.ACTI.1524.ZS + NY.GDP.DEFL.KD.ZG +
    MS.MIL.XPND.ZS + GC.TAX.TOTL.GD.ZS + SE.XPD.TOTL.GD.ZS +
    EG.ELC.ACCS.ZS + name_length + has_common_component_bin + name_length_z_score | Country_Name",
  
  "Year FE" = "~ GE.PER.RNK + RL.PER.RNK + PV.PER.RNK + NY.GDP.PCAP.KD + 
    SP.RUR.TOTL.ZS + SL.TLF.ACTI.1524.ZS + NY.GDP.DEFL.KD.ZG +
    MS.MIL.XPND.ZS + GC.TAX.TOTL.GD.ZS + SE.XPD.TOTL.GD.ZS +
    EG.ELC.ACCS.ZS + name_length + has_common_component_bin + name_length_z_score | Year",
  
  "Candidate FE" = "~ GE.PER.RNK + RL.PER.RNK + PV.PER.RNK + NY.GDP.PCAP.KD + 
    SP.RUR.TOTL.ZS + SL.TLF.ACTI.1524.ZS + NY.GDP.DEFL.KD.ZG +
    MS.MIL.XPND.ZS + GC.TAX.TOTL.GD.ZS + SE.XPD.TOTL.GD.ZS +
    EG.ELC.ACCS.ZS | Candidate_Name",
  
  "Candidate*Country FE" = "~ GE.PER.RNK + RL.PER.RNK + PV.PER.RNK + NY.GDP.PCAP.KD + 
    SP.RUR.TOTL.ZS + SL.TLF.ACTI.1524.ZS + NY.GDP.DEFL.KD.ZG +
    MS.MIL.XPND.ZS + GC.TAX.TOTL.GD.ZS + SE.XPD.TOTL.GD.ZS +
    EG.ELC.ACCS.ZS| Candidate_Name^Country_Name",
  
  "Country+Year FE" = "~ GE.PER.RNK + RL.PER.RNK + PV.PER.RNK + NY.GDP.PCAP.KD + 
    SP.RUR.TOTL.ZS + SL.TLF.ACTI.1524.ZS + NY.GDP.DEFL.KD.ZG +
    MS.MIL.XPND.ZS + GC.TAX.TOTL.GD.ZS + SE.XPD.TOTL.GD.ZS +
    EG.ELC.ACCS.ZS + name_length + has_common_component_bin + name_length_z_score | Country_Name + Year"
)

# Initialize the list to hold all regression models
decoy_models <- list()

# Run all regressions and store in the list
for(name in names(reg_specs)) {
  decoy_models[[name]] <- feglm(
    as.formula(paste("is_decoy_bin", reg_specs[[name]])),
    family = "logit",
    data = candidate_level_with_WDI_minor,
    cluster = ~ constituency_state_country
  )
}

create_regression_results_table <- function(models_list) {
  results_df <- data.frame(
    Variable = character(0),
    `(1)` = character(0),
    `(2)` = character(0),
    `(3)` = character(0),
    `(4)` = character(0),
    `(5)` = character(0),
    `(6)` = character(0),
    stringsAsFactors = FALSE
  )
  
  # Helper function to add stars based on p-value
  add_stars <- function(coef_val, se_val) {
    z_value <- coef_val / se_val
    p_value <- 2 * (1 - pnorm(abs(z_value)))
    
    if (is.na(p_value)) return("")
    if (p_value < 0.01) return("***")
    if (p_value < 0.05) return("**") 
    if (p_value < 0.1) return("*")
    return("")
  }
  
  # Variables to extract (all variables in the model)
  vars_to_extract <- c(
    "GE.PER.RNK", "RL.PER.RNK", "PV.PER.RNK", "NY.GDP.PCAP.KD",
    "SP.RUR.TOTL.ZS", "SL.TLF.ACTI.1524.ZS", "NY.GDP.DEFL.KD.ZG",
    "MS.MIL.XPND.ZS", "GC.TAX.TOTL.GD.ZS", "SE.XPD.TOTL.GD.ZS",
    "EG.ELC.ACCS.ZS", "name_length", "has_common_component_bin", "name_length_z_score"
  )
  
  # Map variable names to their display labels
  var_labels <- c(
    "GE.PER.RNK" = "Government Effectiveness (Percentile Rank)",
    "RL.PER.RNK" = "Rule of Law (Percentile Rank)",
    "PV.PER.RNK" = "Political Stability (Percentile Rank)",
    "NY.GDP.PCAP.KD" = "GDP Per Capita",
    "SP.RUR.TOTL.ZS" = "Rural Population (Perc)",
    "SL.TLF.ACTI.1524.ZS" = "Youth Labor Force Participation (15-24 yrs, Perc)",
    "NY.GDP.DEFL.KD.ZG" = "Inflation Rate (GDP Deflator, Perc)",
    "MS.MIL.XPND.ZS" = "Military Expenditure (Perc of Gov. Exp.)",
    "GC.TAX.TOTL.GD.ZS" = "Tax Revenue (Perc of GDP)",
    "SE.XPD.TOTL.GD.ZS" = "Government Education Expenditure (Perc of GDP)",
    "EG.ELC.ACCS.ZS" = "Access to Electricity (Perc of Population)",
    "name_length" = "Name Length",
    "has_common_component_bin" = "Name has common sub-string",
    "name_length_z_score" = "Name Length Deviation (z-score)"
  )
  
  # Model names
  model_names <- names(models_list)
  
  # Create rows for coefficients and SEs
  for (var in vars_to_extract) {
    # Use the variable label if available, otherwise use the variable name
    var_label <- ifelse(var %in% names(var_labels), var_labels[var], var)
    
    # Initialize coef_row and se_row for each variable
    coef_row <- data.frame(
      Variable = var_label,
      matrix(NA, nrow = 1, ncol = 6),
      stringsAsFactors = FALSE
    )
    se_row <- data.frame(
      Variable = "",  # Leave blank for SE row
      matrix(NA, nrow = 1, ncol = 6),
      stringsAsFactors = FALSE
    )
    names(coef_row) <- names(results_df)
    names(se_row) <- names(results_df)
    
    # Fill in coefficients and SEs
    for (i in 1:6) {
      model_name <- model_names[i]
      if (model_name %in% names(models_list)) {
        tryCatch({
          model <- models_list[[model_name]]
          
          # Check if variable exists in model coefficients
          if (var %in% names(coef(model))) {
            coef_val <- coef(model)[var]
            se_val <- sqrt(diag(vcov(model)))[var]
            stars <- add_stars(coef_val, se_val)
            
            coef_row[1, i + 1] <- sprintf("%.5f%s", coef_val, stars)
            se_row[1, i + 1] <- sprintf("(%.5f)", se_val)
          }
        }, error = function(e) {
          warning(sprintf("Could not get coefficient for %s in model %s: %s", var, model_name, e$message))
        })
      }
    }
    
    results_df <- rbind(results_df, coef_row, se_row)
  }
  
  # Add an empty row
  empty_row <- data.frame(
    Variable = "",
    matrix("", nrow = 1, ncol = 6),
    stringsAsFactors = FALSE
  )
  names(empty_row) <- names(results_df)
  results_df <- rbind(results_df, empty_row)
  
  # Add Log Likelihood row
  loglik_row <- data.frame(
    Variable = "Log Likelihood",
    matrix(NA, nrow = 1, ncol = 6),
    stringsAsFactors = FALSE
  )
  names(loglik_row) <- names(results_df)
  
  # Add R-squared row
  rsq_row <- data.frame(
    Variable = "Pseudo R-squared",
    matrix(NA, nrow = 1, ncol = 6),
    stringsAsFactors = FALSE
  )
  names(rsq_row) <- names(results_df)
  
  # Add Observations row
  obs_row <- data.frame(
    Variable = "Observations",
    matrix(NA, nrow = 1, ncol = 6),
    stringsAsFactors = FALSE
  )
  names(obs_row) <- names(results_df)
  
  # For model statistics
  for (i in 1:6) {
    model_name <- model_names[i]
    
    if (model_name %in% names(models_list)) {
      model <- models_list[[model_name]]
      tryCatch({
        # Log likelihood
        log_lik <- logLik(model)
        loglik_row[1, i + 1] <- sprintf("%.5f", as.numeric(log_lik))
        
        # Pseudo R-squared
        tryCatch({
          null_model <- update(model, . ~ 1)
          null_log_lik <- logLik(null_model)
          pseudo_r2 <- 1 - (as.numeric(log_lik) / as.numeric(null_log_lik))
          rsq_row[1, i + 1] <- sprintf("%.5f", pseudo_r2)
        }, error = function(e) {
          warning(sprintf("Could not calculate pseudo R-squared for model %s: %s", model_name, e$message))
        })
        
        # Observations
        obs <- nobs(model)
        obs_row[1, i + 1] <- sprintf("%d", obs)
        
      }, error = function(e) {
        warning(sprintf("Could not get statistics for model %s: %s", model_name, e$message))
      })
    }
  }
  
  # Add all statistics rows to the results
  results_df <- rbind(results_df, loglik_row, rsq_row, obs_row)
  
  # Add additional rows for Fixed Effects information
  fe_country_row <- data.frame(
    Variable = "Country FE",
    `(1)` = "No",
    `(2)` = "Yes",
    `(3)` = "No",
    `(4)` = "No",
    `(5)` = "Yes",
    `(6)` = "Yes",
    stringsAsFactors = FALSE
  )
  
  fe_year_row <- data.frame(
    Variable = "Year FE",
    `(1)` = "No",
    `(2)` = "No",
    `(3)` = "Yes",
    `(4)` = "No",
    `(5)` = "No",
    `(6)` = "Yes",
    stringsAsFactors = FALSE
  )
  
  fe_candidate_row <- data.frame(
    Variable = "Candidate FE",
    `(1)` = "No",
    `(2)` = "No",
    `(3)` = "No",
    `(4)` = "Yes",
    `(5)` = "Yes",
    `(6)` = "No",
    stringsAsFactors = FALSE
  )
  
  results_df <- rbind(results_df, fe_country_row, fe_year_row, fe_candidate_row)
  
  return(results_df)
}

# Create the formatted table
decoy_results_table <- create_regression_results_table(decoy_models)

create_formatted_table <- function(results_df, title, notes = NULL) {
  xtab <- xtable(results_df,
                 caption = title,
                 align = c("l", rep("c", ncol(results_df))))
  
  if (!is.null(notes)) {
    caption(xtab) <- paste0(title, "\\\\", notes)
  }
  
  print(xtab,
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x) x)
}

create_formatted_table(
  decoy_results_table, 
  "The Impact of Governance and Socioeconomic Factors on Political Decoy Candidates", 
  "Notes: *** p<0.01, ** p<0.05, * p<0.1. Standard errors in parentheses are clustered at the constituency-state-country level. The dependent variable is a binary indicator for whether a candidate is a decoy. Columns present different fixed effects specifications: (1) No fixed effects, (2) Country fixed effects, (3) Year fixed effects, (4) Candidate fixed effects, (5) Candidate×Country fixed effects, (6) Country + Year fixed effects, (7) Candidate×Country + Year fixed effects. Name characteristics are calculated based on candidate names across all countries."
)
