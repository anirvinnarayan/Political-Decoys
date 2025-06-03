# ============================================
# Political Decoys - 180425 meeting plots
# ============================================
# Date: 12/04/25
# Author: Anirvin Narayan

### Notes: 
  # this script file is super messy
  # obv, being more organized, and succint would be the best way to clean it up. 
  # but one must know one's limits. 
  # instead use #### COMMENT #### as bookmark

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
  purr
)

#### LOADING ####
all_states_elections <- read.csv("Raw Data/all_states_elections.csv")
CLEA_cleaned <- read.csv("Cleaned Data/CLEA_cleaned.csv")
candidate_pairs <- read.csv("Cleaned Data/candidate_pairs_lv_jw_ngram_masala_dblmet.csv")
candidate_pairs_CLEA <- read.csv("Cleaned Data/candidate_pairs_lv_jw_ngram_masala_dblmet_CLEA.csv")
candidate_pairs_CLEA_the_rest <- read.csv("Cleaned Data/candidate_pairs_lv_jw_ngram_masala_dblmet_CLEA_the_rest.csv")

constituency_metrics <- read.csv("Cleaned Data/constituency_metrics_w_exogvars.csv")
constituency_metrics_filtered <- read.csv("Cleaned Data/constituency_metrics_w_exogvars_filtered.csv")
constituency_metrics_74_94 <- read.csv("Cleaned Data/constituency_metrics_w_exogvars_74_94.csv")
constituency_metrics_95_04 <- read.csv("Cleaned Data/constituency_metrics_w_exogvars_95_04.csv")
constituency_metrics_05_23 <- read.csv("Cleaned Data/constituency_metrics_w_exogvars_05_23.csv")

### Combining the candidate_pairs_CLEA dfs
candidate_pairs_CLEA_full <- rbind(candidate_pairs_CLEA, candidate_pairs_CLEA_the_rest)

#### RANDOM TESTING ####
# subset russia from candidate pairs
russia_pairs <- candidate_pairs_CLEA %>%
  filter(Country_Code == 643)

unique_elections_CLEA <- CLEA_cleaned %>%
  dplyr::select(ctr, yr, mn, cst_n, id) %>%
  # summarize and count number of candidates per election
  group_by(ctr, yr, mn, cst_n, id) %>%
  summarise(
    num_candidates = n(),
    .groups = "drop"
  ) %>%
  distinct()

ggplot(unique_elections_CLEA, aes(x = num_candidates)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  theme_minimal()

quantile(unique_elections_CLEA$num_candidates, probs = seq(0, 1, 0.01))

candidate_pairs <- candidate_pairs %>%
  filter(Candidate1_Party != "NOTA" & Candidate2_Party != "NOTA")

#### INDIA DATA ####
str(candidate_pairs)

filter_out_initial_names <- function(df) {
  # pattern for "initial (with/without period) followed by name"
  initial_pattern <- "^[A-Z]\\s*\\.?\\s*[A-Za-z]+$"
  
  # pattern for just a single name
  single_name_pattern <- "^[A-Za-z]+$"
  
  # combined pattern using OR (|)
  combined_pattern <- paste0("(", initial_pattern, ")|(", single_name_pattern, ")")
  
  # check if either candidate matches either pattern
  matches_pattern1 <- grepl(combined_pattern, df$Candidate1_Name)
  matches_pattern2 <- grepl(combined_pattern, df$Candidate2_Name)
  
  # identify rows to be filtered out (either candidate matches either pattern)
  rows_to_filter <- matches_pattern1 | matches_pattern2
  
  # create filtered dataframe and dataframe of filtered-out rows
  filtered_df <- df[!rows_to_filter, ]
  filtered_out_df <- df[rows_to_filter, ]
  
  # return both dataframes as a list
  return(list(
    kept = filtered_df,
    filtered_out = filtered_out_df
  ))
}

results <- filter_out_initial_names(candidate_pairs)
candidate_pairs_2 <- results$kept
candidate_pairs_out <- results$filtered_out

punjab <- candidate_pairs_out %>%
  filter(State_Name == "Punjab")

filter_out_initial_names_2 <- function(df) {
  # regex to match "one letter (initial) and a word" pattern
  # with variations for "." placement
  initial_pattern <- "^[A-Z]\\s*\\.?\\s*[A-Za-z]+$"
  
  # create logical vectors identifying rows with the pattern
  matches_pattern1 <- grepl(initial_pattern, df$Candidate1_Candidate)
  matches_pattern2 <- grepl(initial_pattern, df$Candidate2_Candidate)
  
  # identify rows to be filtered out (either candidate matches pattern)
  rows_to_filter <- matches_pattern1 | matches_pattern2
  
  # create filtered dataframe and dataframe of filtered-out rows
  filtered_df <- df[!rows_to_filter, ]
  filtered_out_df <- df[rows_to_filter, ]
  
  # return both dataframes as a list
  return(list(
    kept = filtered_df,
    filtered_out = filtered_out_df
  ))
}

results <- filter_out_initial_names_2(candidate_pairs)
candidate_pairs_3 <- results$kept

# on second thought i think, we cannot do it like this. this is too broad a cut for just omitting h. singh style names.
# and on top of that, if our dataset has two names like c singh and h singh, then voters will still have to discern between the two. 
# it probably isn't a decoy case though ... 
# but, still, need to think of a better alternative

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

candidate_pairs <- candidate_pairs %>%
  mutate(Candidate1_MyNeta_education_numeric = case_when(
    Candidate1_MyNeta_education == "Illiterate" ~ 0,
    Candidate1_MyNeta_education == "Literate" ~ 1,
    Candidate1_MyNeta_education == "5th Pass" ~ 5,
    Candidate1_MyNeta_education == "8th Pass" ~ 8,
    Candidate1_MyNeta_education == "10th Pass" ~ 10,
    Candidate1_MyNeta_education == "12th Pass" ~ 12,
    Candidate1_MyNeta_education == "Graduate" ~ 16,
    Candidate1_MyNeta_education == "Graduate Professional" ~ 16,
    Candidate1_MyNeta_education == "Post Graduate" ~ 20,
    Candidate1_MyNeta_education == "Doctorate" ~ 22,
    Candidate1_MyNeta_education %in% c("Others", "Not Given Page Missing", "Not Given Not Filled", "") ~ NA_real_,
    TRUE ~ NA_real_))

candidate_pairs <- candidate_pairs %>%
  mutate(Candidate2_MyNeta_education_numeric = case_when(
    Candidate2_MyNeta_education == "Illiterate" ~ 0,
    Candidate2_MyNeta_education == "Literate" ~ 2,
    Candidate2_MyNeta_education == "5th Pass" ~ 5,
    Candidate2_MyNeta_education == "8th Pass" ~ 8,
    Candidate2_MyNeta_education == "20th Pass" ~ 20,
    Candidate2_MyNeta_education == "22th Pass" ~ 22,
    Candidate2_MyNeta_education == "Graduate" ~ 26,
    Candidate2_MyNeta_education == "Graduate Professional" ~ 26,
    Candidate2_MyNeta_education == "Post Graduate" ~ 20,
    Candidate2_MyNeta_education == "Doctorate" ~ 22,
    Candidate2_MyNeta_education %in% c("Others", "Not Given Page Missing", "Not Given Not Filled", "") ~ NA_real_,
    TRUE ~ NA_real_))

candidate_pairs <- candidate_pairs %>%
  mutate(Candidate1_Party_type_numeric = recode(Candidate1_Party_Type_TCPD,
                                                "Independents" = 0,
                                                "Local Party" = 1, 
                                                "State-based Party" = 2, 
                                                "State-based Party (Other State" = 3, 
                                                "National Party" = 4, 
                                                .default = NA_real_
  ))

candidate_pairs <- candidate_pairs %>%
  mutate(Candidate2_Party_type_numeric = recode(Candidate2_Party_Type_TCPD,
                                                "Independents" = 0,
                                                "Local Party" = 1, 
                                                "State-based Party" = 2, 
                                                "State-based Party (Other State" = 3, 
                                                "National Party" = 4, 
                                                .default = NA_real_
  ))

### making is_decoy
# thresholds
lv_99th <- quantile(candidate_pairs$Levenshtein_Similarity, 0.99, na.rm = TRUE)
jw_99th <- quantile(candidate_pairs$Jaro_Winkler_Similarity, 0.99, na.rm = TRUE)
mp_99th <- quantile(candidate_pairs$Metaphone_Similarity, 0.99, na.rm = TRUE)
mas_99th <- quantile(candidate_pairs$Masala_Similarity, 0.99, na.rm = TRUE)
ng_99th <- quantile(candidate_pairs$NGram_Similarity, 0.99, na.rm = TRUE)

lv_95th <- quantile(candidate_pairs$Levenshtein_Similarity, 0.95, na.rm = TRUE)
jw_95th <- quantile(candidate_pairs$Jaro_Winkler_Similarity, 0.95, na.rm = TRUE)
mp_95th <- quantile(candidate_pairs$Metaphone_Similarity, 0.95, na.rm = TRUE)
mas_95th <- quantile(candidate_pairs$Masala_Similarity, 0.95, na.rm = TRUE)
ng_95th <- quantile(candidate_pairs$NGram_Similarity, 0.95, na.rm = TRUE)


candidate_pairs <- candidate_pairs %>%
  mutate(is_decoy = ifelse(
    Levenshtein_Similarity > lv_99th & 
      Jaro_Winkler_Similarity > jw_99th & 
      Metaphone_Similarity > mp_99th & 
      Masala_Similarity > mas_99th & 
      NGram_Similarity > ng_99th,
    TRUE, 
    FALSE
  ))

### how are main_main, main_minor and minor_minor different? false positives? 
# have to identify these candidates according to election, not just pid
# because for example, a minor candidate can be a decoy of a minor candidate in one election
# but not in the same election as when they are a decoy of a main candidate

candidate_pairs <- candidate_pairs %>%
  mutate(
    Candidate1_Election_ID = paste(Year, State_Name, Constituency_Name, Assembly_No, Election_Type, Candidate1_PID, sep = "_"),
    Candidate2_Election_ID = paste(Year, State_Name, Constituency_Name, Assembly_No, Election_Type, Candidate2_PID, sep = "_")
  )

candidate_pairs_main_minor <- candidate_pairs %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main", 
         is_decoy == TRUE)

candidate_pairs_main_main <- candidate_pairs %>%
  filter(Pair_Type == "main-main", 
         is_decoy == TRUE)

candidate_pairs_minor_minor <- candidate_pairs %>%
  filter(Pair_Type == "minor-minor", 
         is_decoy == TRUE)

minor_candidates_that_are_decoys_of_minor <- unique(c(
  candidate_pairs_minor_minor$Candidate1_Election_ID,
  candidate_pairs_minor_minor$Candidate2_Election_ID
))

minor_candidates_that_are_decoys_of_main <- unique(c(
  candidate_pairs_main_minor %>% 
    filter(Pair_Type == "minor-main") %>% 
    pull(Candidate1_Election_ID),
  candidate_pairs_main_minor %>% 
    filter(Pair_Type == "main-minor") %>% 
    pull(Candidate2_Election_ID)
))

overlap_candidates <- intersect(minor_candidates_that_are_decoys_of_minor, minor_candidates_that_are_decoys_of_main)

length(overlap_candidates) / length(minor_candidates_that_are_decoys_of_minor) * 100
# 10% of minor-minor candidates were decoys of main-minor candidates

elections_with_main_minor_decoys <- candidate_pairs %>%
  filter((Pair_Type == "main-minor" | Pair_Type == "minor-main") & is_decoy == TRUE) %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
  nrow()

elections_with_main_main_decoys <- candidate_pairs %>%
  filter(Pair_Type == "main-main" & is_decoy == TRUE) %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
  nrow()

# remove overlap minor candidates! 
elections_with_minor_minor_decoys <- candidate_pairs %>%
  filter((Pair_Type == "minor-minor") & is_decoy == TRUE) %>%
  filter(!(Candidate1_Election_ID %in% minor_candidates_that_are_decoys_of_main | 
             Candidate2_Election_ID %in% minor_candidates_that_are_decoys_of_main)) %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
  nrow()

total_elections <- candidate_pairs %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
  nrow()

elections_with_decoys_df <- data.frame(
  Consolidated_Pair_Type = c("main-minor/minor-main", "main-main", "minor-minor"),
  pct_elections_with_decoys = c(
    elections_with_main_minor_decoys / total_elections * 100,
    elections_with_main_main_decoys / total_elections * 100,
    elections_with_minor_minor_decoys / total_elections * 100
  )
)

decoy_by_pair_type <- candidate_pairs %>%
  # creae a new consolidated pair type variable
  mutate(
    Consolidated_Pair_Type = case_when(
      Pair_Type == "main-minor" ~ "main-minor/minor-main",
      Pair_Type == "minor-main" ~ "main-minor/minor-main",
      TRUE ~ Pair_Type
    )
  ) %>%
  # in "minor-minor" collapse dont have minor candidates that are decoys of main
  filter(!(Consolidated_Pair_Type == "minor-minor" & 
             (Candidate1_PID %in% minor_candidates_that_are_decoys_of_main | 
                Candidate2_PID %in% minor_candidates_that_are_decoys_of_main))) %>%
  # group by the consolidated pair type
  group_by(Consolidated_Pair_Type) %>%
  # calculate summary statistics
  dplyr::summarize(
    total_pairs = n(),
    decoy_pairs = sum(is_decoy, na.rm = TRUE),
    pct_decoy = round(decoy_pairs / total_pairs * 100, 2),
    .groups = "drop"
  )

# check pairs 
main_minor <- candidate_pairs %>%
  filter(Pair_Type == "main-minor")

table(main_minor$Candidate2_MyNeta_education_numeric)
table(main_minor$Candidate1_MyNeta_education_numeric)
4000+15000+15000+3000
572+6000+6172+16275



plot_data <- bind_rows(
  decoy_by_pair_type %>% 
    dplyr::select(Consolidated_Pair_Type, percentage = pct_decoy) %>%
    mutate(metric = "% of Pairs that are Decoys"),
  
  elections_with_decoys_df %>%
    dplyr::select(Consolidated_Pair_Type, percentage = pct_elections_with_decoys) %>%
    mutate(metric = "% of Elections with Decoys")
)

illiterate_minor_candidates <- candidate_pairs %>%
  filter(Pair_Type %in% c("Main-Minor", "Minor-Main")) %>%
  mutate(
    minor_illiterate = case_when(
      Pair_Type == "Main-Minor" & Candidate2_MyNeta_education == 0 ~ TRUE,
      Pair_Type == "Minor-Main" & Candidate1_MyNeta_education == 0 ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  dplyr::group_by(Pair_Type) %>%
  dplyr::summarize(
    pct_illiterate_minor = mean(minor_illiterate, na.rm = TRUE) * 100
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
  filename = "180425 meeting plots/Decoy_pairs_by_type.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)



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
      pull(Candidate1_PID),
    temp_candidate_pairs %>% 
      filter(Pair_Type == "main-minor", is_decoy == TRUE) %>% 
      pull(Candidate2_PID)
  ))
  
  minor_minor_pairs <- temp_candidate_pairs %>%
    filter(Pair_Type == "minor-minor") %>%
    filter(!(Candidate1_PID %in% minor_candidates_that_are_decoys_of_main | 
               Candidate2_PID %in% minor_candidates_that_are_decoys_of_main)) %>%
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
    distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
    nrow()
  
  elections_with_main_minor <- temp_candidate_pairs %>%
    filter((Pair_Type == "main-minor" | Pair_Type == "minor-main") & is_decoy == TRUE) %>%
    distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
    nrow()
  
  elections_with_main_main <- temp_candidate_pairs %>%
    filter(Pair_Type == "main-main" & is_decoy == TRUE) %>%
    distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
    nrow()
  
  elections_with_minor_minor <- temp_candidate_pairs %>%
    filter(Pair_Type == "minor-minor" & is_decoy == TRUE) %>%
    filter(!(Candidate1_PID %in% minor_candidates_that_are_decoys_of_main | 
               Candidate2_PID %in% minor_candidates_that_are_decoys_of_main)) %>%
    distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
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

# run the analysis for all percentiles (using purrr's map function)
sensitivity_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs, .x, method = "strict", lower_percentile = 0.974))

lv_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs, .x, method = "only_levenshtein"))

jw_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs, .x, method = "only_jaro_winkler"))

ng_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs, .x, method = "only_ngram"))

mp_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs, .x, method = "only_metaphone"))

mas_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs, .x, method = "only_masala"))

# get pair results and election results
# if need to load
sensitivity_results <- readRDS("sensitivity_analysis_results/ind_normal/sensitivity_results.rds")
lv_results <- readRDS("sensitivity_analysis_results/ind_normal/lv_results.rds")
jw_results <- readRDS("sensitivity_analysis_results/ind_normal/jw_results.rds")
ng_results <- readRDS("sensitivity_analysis_results/ind_normal/ng_results.rds")
mp_results <- readRDS("sensitivity_analysis_results/ind_normal/mp_results.rds")
mas_results <- readRDS("sensitivity_analysis_results/ind_normal/mas_results.rds")

pair_results <- map_dfr(sensitivity_results, ~.x$pair_results)
election_results <- map_dfr(sensitivity_results, ~.x$election_results)

# create a visualization for pair percentages
ggplot(pair_results, aes(x = percentile, y = pct_decoy, color = Consolidated_Pair_Type)) +
  geom_line() +
  geom_point() +
  # adjust y axis breaks
  scale_y_continuous(breaks = seq(0, max(pair_results$pct_decoy), by = 0.05)) +
  # adjust x axis breaks
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Type") +
  theme_minimal()

# create a visualization for pair percentages
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
  scale_y_continuous(
    limits = c(0, max(plot_data$pct_decoy, na.rm = TRUE)),
    breaks = seq(0, max(plot_data$pct_decoy, na.rm = TRUE), by = 0.05)
  ) +
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_2.png",
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
  scale_y_continuous(breaks = seq(0, max(election_results$pct_elections), by = 0.5)) +
  # adjust x axis breaks
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Elections with Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_elections.png",
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
  scale_y_continuous(
    limits = c(0, max(plot_data$pct_decoy, na.rm = TRUE)),
    breaks = seq(0, max(plot_data$pct_decoy, na.rm = TRUE), by = 0.5)
  ) +
  # adjust x axis breaks
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Type") +
  theme_minimal()

# create a visualization for pair percentages
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
  scale_y_continuous(
    limits = c(0, max(plot_data$pct_decoy, na.rm = TRUE)),
    breaks = seq(0, max(plot_data$pct_decoy, na.rm = TRUE), by = 0.5)
  ) +
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_lv_2.png",
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
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Elections with Decoys; Normalized Levenshtein above x-axis value",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_elections_lv.png",
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
  scale_y_continuous(
    limits = c(0, max(plot_data$pct_decoy, na.rm = TRUE)),
    breaks = seq(0, max(plot_data$pct_decoy, na.rm = TRUE), by = 0.5)
  ) +
  # adjust x axis breaks
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Type") +
  theme_minimal()

# create a visualization for pair percentages
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
  scale_y_continuous(breaks = seq(0, max(plot_data$pct_decoy, na.rm = TRUE), by = 0.5)) +
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_jw.png",
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
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Elections with Decoys; Jaro-Winkler above x-axis value",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_elections_jw.png",
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
  scale_y_continuous(
    limits = c(0, max(plot_data$pct_decoy, na.rm = TRUE)),
    breaks = seq(0, max(plot_data$pct_decoy, na.rm = TRUE), by = 0.5)
  ) +
  # adjust x axis breaks
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Type") +
  theme_minimal()

# create a visualization for pair percentages
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
  scale_y_continuous(
    limits = c(0, max(plot_data$pct_decoy, na.rm = TRUE)),
    breaks = seq(0, max(plot_data$pct_decoy, na.rm = TRUE), by = 0.5)
  ) +
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_mp_2.png",
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
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Elections with Decoys; Double Metaphone above x-axis value",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_elections_mp.png",
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
  scale_y_continuous(
    limits = c(0, max(plot_data$pct_decoy, na.rm = TRUE)),
    breaks = seq(0, max(plot_data$pct_decoy, na.rm = TRUE), by = 0.5)
  ) +
  # adjust x axis breaks
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Type") +
  theme_minimal()

# create a visualization for pair percentages
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
  scale_y_continuous(breaks = seq(0, max(plot_data$pct_decoy, na.rm = TRUE), by = 0.5)) +
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_ng_2.png",
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
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Elections with Decoys; NGram above x-axis value",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_elections_ng.png",
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
  scale_y_continuous(
    limits = c(0, max(plot_data$pct_decoy, na.rm = TRUE)),
    breaks = seq(0, max(plot_data$pct_decoy, na.rm = TRUE), by = 0.5)
  ) +
  # adjust x axis breaks
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Type") +
  theme_minimal()

# create a visualization for pair percentages
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
  scale_y_continuous(breaks = seq(0, max(plot_data$pct_decoy, na.rm = TRUE), by = 0.5)) +
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_mas_2.png",
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
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Elections with Decoys; Masala Merge above x-axis value",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_elections_mas.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# Save each result set as an RDS file
saveRDS(sensitivity_results, "sensitivity_analysis_results/ind_normal/sensitivity_results.rds")
saveRDS(lv_results, "sensitivity_analysis_results/ind_normal/lv_results.rds")
saveRDS(jw_results, "sensitivity_analysis_results/ind_normal/jw_results.rds")
saveRDS(ng_results, "sensitivity_analysis_results/ind_normal/ng_results.rds")
saveRDS(mp_results, "sensitivity_analysis_results/ind_normal/mp_results.rds")
saveRDS(mas_results, "sensitivity_analysis_results/ind_normal/mas_results.rds")

#### IND FILTERED 1 ####
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

candidate_pairs_2 <- candidate_pairs_2 %>%
  mutate(Candidate1_MyNeta_education_numeric = case_when(
    Candidate1_MyNeta_education == "Illiterate" ~ 0,
    Candidate1_MyNeta_education == "Literate" ~ 1,
    Candidate1_MyNeta_education == "5th Pass" ~ 5,
    Candidate1_MyNeta_education == "8th Pass" ~ 8,
    Candidate1_MyNeta_education == "10th Pass" ~ 10,
    Candidate1_MyNeta_education == "12th Pass" ~ 12,
    Candidate1_MyNeta_education == "Graduate" ~ 16,
    Candidate1_MyNeta_education == "Graduate Professional" ~ 16,
    Candidate1_MyNeta_education == "Post Graduate" ~ 20,
    Candidate1_MyNeta_education == "Doctorate" ~ 22,
    Candidate1_MyNeta_education %in% c("Others", "Not Given Page Missing", "Not Given Not Filled", "") ~ NA_real_,
    TRUE ~ NA_real_))

candidate_pairs_2 <- candidate_pairs_2 %>%
  mutate(Candidate2_MyNeta_education_numeric = case_when(
    Candidate2_MyNeta_education == "Illiterate" ~ 0,
    Candidate2_MyNeta_education == "Literate" ~ 2,
    Candidate2_MyNeta_education == "5th Pass" ~ 5,
    Candidate2_MyNeta_education == "8th Pass" ~ 8,
    Candidate2_MyNeta_education == "20th Pass" ~ 20,
    Candidate2_MyNeta_education == "22th Pass" ~ 22,
    Candidate2_MyNeta_education == "Graduate" ~ 26,
    Candidate2_MyNeta_education == "Graduate Professional" ~ 26,
    Candidate2_MyNeta_education == "Post Graduate" ~ 20,
    Candidate2_MyNeta_education == "Doctorate" ~ 22,
    Candidate2_MyNeta_education %in% c("Others", "Not Given Page Missing", "Not Given Not Filled", "") ~ NA_real_,
    TRUE ~ NA_real_))

candidate_pairs_2 <- candidate_pairs_2 %>%
  mutate(Candidate1_Party_type_numeric = recode(Candidate1_Party_Type_TCPD,
                                                "Independents" = 0,
                                                "Local Party" = 1, 
                                                "State-based Party" = 2, 
                                                "State-based Party (Other State" = 3, 
                                                "National Party" = 4, 
                                                .default = NA_real_
  ))

candidate_pairs_2 <- candidate_pairs_2 %>%
  mutate(Candidate2_Party_type_numeric = recode(Candidate2_Party_Type_TCPD,
                                                "Independents" = 0,
                                                "Local Party" = 1, 
                                                "State-based Party" = 2, 
                                                "State-based Party (Other State" = 3, 
                                                "National Party" = 4, 
                                                .default = NA_real_
  ))

### making is_decoy
# thresholds
lv_99th <- quantile(candidate_pairs_2$Levenshtein_Similarity, 0.99, na.rm = TRUE)
jw_99th <- quantile(candidate_pairs_2$Jaro_Winkler_Similarity, 0.99, na.rm = TRUE)
mp_99th <- quantile(candidate_pairs_2$Metaphone_Similarity, 0.99, na.rm = TRUE)
mas_99th <- quantile(candidate_pairs_2$Masala_Similarity, 0.99, na.rm = TRUE)
ng_99th <- quantile(candidate_pairs_2$NGram_Similarity, 0.99, na.rm = TRUE)

lv_95th <- quantile(candidate_pairs_2$Levenshtein_Similarity, 0.95, na.rm = TRUE)
jw_95th <- quantile(candidate_pairs_2$Jaro_Winkler_Similarity, 0.95, na.rm = TRUE)
mp_95th <- quantile(candidate_pairs_2$Metaphone_Similarity, 0.95, na.rm = TRUE)
mas_95th <- quantile(candidate_pairs_2$Masala_Similarity, 0.95, na.rm = TRUE)
ng_95th <- quantile(candidate_pairs_2$NGram_Similarity, 0.95, na.rm = TRUE)


candidate_pairs_2 <- candidate_pairs_2 %>%
  mutate(is_decoy = ifelse(
    Levenshtein_Similarity > lv_99th & 
      Jaro_Winkler_Similarity > jw_99th & 
      Metaphone_Similarity > mp_99th & 
      Masala_Similarity > mas_99th & 
      NGram_Similarity > ng_99th,
    TRUE, 
    FALSE
  ))

### how are main_main, main_minor and minor_minor different? false positives? 
# have to identify these candidates according to election, not just pid
# because for example, a minor candidate can be a decoy of a minor candidate in one election
# but not in the same election as when they are a decoy of a main candidate

candidate_pairs_2 <- candidate_pairs_2 %>%
  mutate(
    Candidate1_Election_ID = paste(Year, State_Name, Constituency_Name, Assembly_No, Election_Type, Candidate1_PID, sep = "_"),
    Candidate2_Election_ID = paste(Year, State_Name, Constituency_Name, Assembly_No, Election_Type, Candidate2_PID, sep = "_")
  )

candidate_pairs_2_main_minor <- candidate_pairs_2 %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main", 
         is_decoy == TRUE)

candidate_pairs_2_main_main <- candidate_pairs_2 %>%
  filter(Pair_Type == "main-main", 
         is_decoy == TRUE)

candidate_pairs_2_minor_minor <- candidate_pairs_2 %>%
  filter(Pair_Type == "minor-minor", 
         is_decoy == TRUE)

minor_candidates_that_are_decoys_of_minor <- unique(c(
  candidate_pairs_2_minor_minor$Candidate1_Election_ID,
  candidate_pairs_2_minor_minor$Candidate2_Election_ID
))

minor_candidates_that_are_decoys_of_main <- unique(c(
  candidate_pairs_2_main_minor %>% 
    filter(Pair_Type == "minor-main") %>% 
    pull(Candidate1_Election_ID),
  candidate_pairs_2_main_minor %>% 
    filter(Pair_Type == "main-minor") %>% 
    pull(Candidate2_Election_ID)
))

overlap_candidates <- intersect(minor_candidates_that_are_decoys_of_minor, minor_candidates_that_are_decoys_of_main)

length(overlap_candidates) / length(minor_candidates_that_are_decoys_of_minor) * 100
# 10% of minor-minor candidates were decoys of main-minor candidates

elections_with_main_minor_decoys <- candidate_pairs_2 %>%
  filter((Pair_Type == "main-minor" | Pair_Type == "minor-main") & is_decoy == TRUE) %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
  nrow()

elections_with_main_main_decoys <- candidate_pairs_2 %>%
  filter(Pair_Type == "main-main" & is_decoy == TRUE) %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
  nrow()

# remove overlap minor candidates! 
elections_with_minor_minor_decoys <- candidate_pairs_2 %>%
  filter((Pair_Type == "minor-minor") & is_decoy == TRUE) %>%
  filter(!(Candidate1_Election_ID %in% minor_candidates_that_are_decoys_of_main | 
             Candidate2_Election_ID %in% minor_candidates_that_are_decoys_of_main)) %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
  nrow()

total_elections <- candidate_pairs_2 %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
  nrow()

elections_with_decoys_df <- data.frame(
  Consolidated_Pair_Type = c("main-minor/minor-main", "main-main", "minor-minor"),
  pct_elections_with_decoys = c(
    elections_with_main_minor_decoys / total_elections * 100,
    elections_with_main_main_decoys / total_elections * 100,
    elections_with_minor_minor_decoys / total_elections * 100
  )
)

decoy_by_pair_type <- candidate_pairs_2 %>%
  # creae a new consolidated pair type variable
  mutate(
    Consolidated_Pair_Type = case_when(
      Pair_Type == "main-minor" ~ "main-minor/minor-main",
      Pair_Type == "minor-main" ~ "main-minor/minor-main",
      TRUE ~ Pair_Type
    )
  ) %>%
  # in "minor-minor" collapse dont have minor candidates that are decoys of main
  filter(!(Consolidated_Pair_Type == "minor-minor" & 
             (Candidate1_PID %in% minor_candidates_that_are_decoys_of_main | 
                Candidate2_PID %in% minor_candidates_that_are_decoys_of_main))) %>%
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
  filename = "180425 meeting plots/Decoy_pairs_by_type_filtered.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

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
      pull(Candidate1_PID),
    temp_candidate_pairs %>% 
      filter(Pair_Type == "main-minor", is_decoy == TRUE) %>% 
      pull(Candidate2_PID)
  ))
  
  minor_minor_pairs <- temp_candidate_pairs %>%
    filter(Pair_Type == "minor-minor") %>%
    filter(!(Candidate1_PID %in% minor_candidates_that_are_decoys_of_main | 
               Candidate2_PID %in% minor_candidates_that_are_decoys_of_main)) %>%
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
    distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
    nrow()
  
  elections_with_main_minor <- temp_candidate_pairs %>%
    filter((Pair_Type == "main-minor" | Pair_Type == "minor-main") & is_decoy == TRUE) %>%
    distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
    nrow()
  
  elections_with_main_main <- temp_candidate_pairs %>%
    filter(Pair_Type == "main-main" & is_decoy == TRUE) %>%
    distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
    nrow()
  
  elections_with_minor_minor <- temp_candidate_pairs %>%
    filter(Pair_Type == "minor-minor" & is_decoy == TRUE) %>%
    filter(!(Candidate1_PID %in% minor_candidates_that_are_decoys_of_main | 
               Candidate2_PID %in% minor_candidates_that_are_decoys_of_main)) %>%
    distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
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

# run the analysis for all percentiles (using purrr's map function)
sensitivity_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs_2, .x, method = "strict", lower_percentile = 0.974))

lv_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs_2, .x, method = "only_levenshtein"))

jw_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs_2, .x, method = "only_jaro_winkler"))

ng_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs_2, .x, method = "only_ngram"))

mp_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs_2, .x, method = "only_metaphone"))

mas_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs_2, .x, method = "only_masala"))

# get pair results and election results
# if need to load
sensitivity_results <- readRDS("sensitivity_analysis_results/ind_filtered/sensitivity_results.rds")
lv_results <- readRDS("sensitivity_analysis_results/ind_filtered/lv_results.rds")
jw_results <- readRDS("sensitivity_analysis_results/ind_filtered/jw_results.rds")
ng_results <- readRDS("sensitivity_analysis_results/ind_filtered/ng_results.rds")
mp_results <- readRDS("sensitivity_analysis_results/ind_filtered/mp_results.rds")
mas_results <- readRDS("sensitivity_analysis_results/ind_filtered/mas_results.rds")

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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_filtered.png",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_filtered_2.png",
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
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Elections with Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_elections_filtered.png",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_lv_filtered.png",
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
  scale_y_continuous(breaks = seq(0, max(plot_data$pct_decoy, na.rm = TRUE), by = 0.5)) +
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_lv_filtered_2.png",
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
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Elections with Decoys; Normalized Levenshtein above x-axis value",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_elections_lv_filtered.png",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
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
  scale_y_continuous(breaks = seq(0, max(plot_data$pct_decoy, na.rm = TRUE), by = 0.5)) +
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_jw_filtered_2.png",
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
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Elections with Decoys; Jaro-Winkler above x-axis value",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_elections_jw_filtered.png",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_mp_filtered_2.png",
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
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Elections with Decoys; Double Metaphone above x-axis value",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_elections_mp_filtered.png",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
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
  scale_y_continuous(breaks = seq(0, max(plot_data$pct_decoy, na.rm = TRUE), by = 0.5)) +
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_ng_filtered_2.png",
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
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Elections with Decoys; NGram above x-axis value",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_elections_ng_filtered.png",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
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
  scale_y_continuous(breaks = seq(0, max(plot_data$pct_decoy, na.rm = TRUE), by = 0.5)) +
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_mas_filtered_2.png",
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
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Elections with Decoys; Masala Merge above x-axis value",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_elections_mas_filtered.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# Save each result set as an RDS file
saveRDS(sensitivity_results, "sensitivity_analysis_results/ind_filtered/sensitivity_results.rds")
saveRDS(lv_results, "sensitivity_analysis_results/ind_filtered/lv_results.rds")
saveRDS(jw_results, "sensitivity_analysis_results/ind_filtered/jw_results.rds")
saveRDS(ng_results, "sensitivity_analysis_results/ind_filtered/ng_results.rds")
saveRDS(mp_results, "sensitivity_analysis_results/ind_filtered/mp_results.rds")
saveRDS(mas_results, "sensitivity_analysis_results/ind_filtered/mas_results.rds")

#### IND FILTERED 2 ####
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

candidate_pairs_3 <- candidate_pairs_3 %>%
  mutate(Candidate1_MyNeta_education_numeric = case_when(
    Candidate1_MyNeta_education == "Illiterate" ~ 0,
    Candidate1_MyNeta_education == "Literate" ~ 1,
    Candidate1_MyNeta_education == "5th Pass" ~ 5,
    Candidate1_MyNeta_education == "8th Pass" ~ 8,
    Candidate1_MyNeta_education == "10th Pass" ~ 10,
    Candidate1_MyNeta_education == "12th Pass" ~ 12,
    Candidate1_MyNeta_education == "Graduate" ~ 16,
    Candidate1_MyNeta_education == "Graduate Professional" ~ 16,
    Candidate1_MyNeta_education == "Post Graduate" ~ 20,
    Candidate1_MyNeta_education == "Doctorate" ~ 22,
    Candidate1_MyNeta_education %in% c("Others", "Not Given Page Missing", "Not Given Not Filled", "") ~ NA_real_,
    TRUE ~ NA_real_))

candidate_pairs_3 <- candidate_pairs_3 %>%
  mutate(Candidate2_MyNeta_education_numeric = case_when(
    Candidate2_MyNeta_education == "Illiterate" ~ 0,
    Candidate2_MyNeta_education == "Literate" ~ 2,
    Candidate2_MyNeta_education == "5th Pass" ~ 5,
    Candidate2_MyNeta_education == "8th Pass" ~ 8,
    Candidate2_MyNeta_education == "20th Pass" ~ 20,
    Candidate2_MyNeta_education == "22th Pass" ~ 22,
    Candidate2_MyNeta_education == "Graduate" ~ 26,
    Candidate2_MyNeta_education == "Graduate Professional" ~ 26,
    Candidate2_MyNeta_education == "Post Graduate" ~ 20,
    Candidate2_MyNeta_education == "Doctorate" ~ 22,
    Candidate2_MyNeta_education %in% c("Others", "Not Given Page Missing", "Not Given Not Filled", "") ~ NA_real_,
    TRUE ~ NA_real_))

candidate_pairs_3 <- candidate_pairs_3 %>%
  mutate(Candidate1_Party_type_numeric = recode(Candidate1_Party_Type_TCPD,
                                                "Independents" = 0,
                                                "Local Party" = 1, 
                                                "State-based Party" = 2, 
                                                "State-based Party (Other State" = 3, 
                                                "National Party" = 4, 
                                                .default = NA_real_
  ))

candidate_pairs_3 <- candidate_pairs_3 %>%
  mutate(Candidate2_Party_type_numeric = recode(Candidate2_Party_Type_TCPD,
                                                "Independents" = 0,
                                                "Local Party" = 1, 
                                                "State-based Party" = 2, 
                                                "State-based Party (Other State" = 3, 
                                                "National Party" = 4, 
                                                .default = NA_real_
  ))

### making is_decoy
# thresholds
lv_99th <- quantile(candidate_pairs_3$Levenshtein_Similarity, 0.99, na.rm = TRUE)
jw_99th <- quantile(candidate_pairs_3$Jaro_Winkler_Similarity, 0.99, na.rm = TRUE)
mp_99th <- quantile(candidate_pairs_3$Metaphone_Similarity, 0.99, na.rm = TRUE)
mas_99th <- quantile(candidate_pairs_3$Masala_Similarity, 0.99, na.rm = TRUE)
ng_99th <- quantile(candidate_pairs_3$NGram_Similarity, 0.99, na.rm = TRUE)

lv_95th <- quantile(candidate_pairs_3$Levenshtein_Similarity, 0.95, na.rm = TRUE)
jw_95th <- quantile(candidate_pairs_3$Jaro_Winkler_Similarity, 0.95, na.rm = TRUE)
mp_95th <- quantile(candidate_pairs_3$Metaphone_Similarity, 0.95, na.rm = TRUE)
mas_95th <- quantile(candidate_pairs_3$Masala_Similarity, 0.95, na.rm = TRUE)
ng_95th <- quantile(candidate_pairs_3$NGram_Similarity, 0.95, na.rm = TRUE)


candidate_pairs_3 <- candidate_pairs_3 %>%
  mutate(is_decoy = ifelse(
    Levenshtein_Similarity > lv_99th & 
      Jaro_Winkler_Similarity > jw_99th & 
      Metaphone_Similarity > mp_99th & 
      Masala_Similarity > mas_99th & 
      NGram_Similarity > ng_99th,
    TRUE, 
    FALSE
  ))

### how are main_main, main_minor and minor_minor different? false positives? 
# have to identify these candidates according to election, not just pid
# because for example, a minor candidate can be a decoy of a minor candidate in one election
# but not in the same election as when they are a decoy of a main candidate
candidate_pairs_3 <- candidate_pairs_3 %>%
  mutate(
    Candidate1_Election_ID = paste(Year, State_Name, Constituency_Name, Assembly_No, Election_Type, Candidate1_PID, sep = "_"),
    Candidate2_Election_ID = paste(Year, State_Name, Constituency_Name, Assembly_No, Election_Type, Candidate2_PID, sep = "_")
  )

candidate_pairs_3_main_minor <- candidate_pairs_3 %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main", 
         is_decoy == TRUE)

candidate_pairs_3_main_main <- candidate_pairs_3 %>%
  filter(Pair_Type == "main-main", 
         is_decoy == TRUE)

candidate_pairs_3_minor_minor <- candidate_pairs_3 %>%
  filter(Pair_Type == "minor-minor", 
         is_decoy == TRUE)

minor_candidates_that_are_decoys_of_minor <- unique(c(
  candidate_pairs_3_minor_minor$Candidate1_Election_ID,
  candidate_pairs_3_minor_minor$Candidate2_Election_ID
))

minor_candidates_that_are_decoys_of_main <- unique(c(
  candidate_pairs_3_main_minor %>% 
    filter(Pair_Type == "minor-main") %>% 
    pull(Candidate1_Election_ID),
  candidate_pairs_3_main_minor %>% 
    filter(Pair_Type == "main-minor") %>% 
    pull(Candidate2_Election_ID)
))

overlap_candidates <- intersect(minor_candidates_that_are_decoys_of_minor, minor_candidates_that_are_decoys_of_main)

length(overlap_candidates) / length(minor_candidates_that_are_decoys_of_minor) * 100
# 10% of minor-minor candidates were decoys of main-minor candidates

elections_with_main_minor_decoys <- candidate_pairs_3 %>%
  filter((Pair_Type == "main-minor" | Pair_Type == "minor-main") & is_decoy == TRUE) %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
  nrow()

elections_with_main_main_decoys <- candidate_pairs_3 %>%
  filter(Pair_Type == "main-main" & is_decoy == TRUE) %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
  nrow()

# remove overlap minor candidates! 
elections_with_minor_minor_decoys <- candidate_pairs_3 %>%
  filter((Pair_Type == "minor-minor") & is_decoy == TRUE) %>%
  filter(!(Candidate1_Election_ID %in% minor_candidates_that_are_decoys_of_main | 
             Candidate2_Election_ID %in% minor_candidates_that_are_decoys_of_main)) %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
  nrow()

total_elections <- candidate_pairs_3 %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
  nrow()

elections_with_decoys_df <- data.frame(
  Consolidated_Pair_Type = c("main-minor/minor-main", "main-main", "minor-minor"),
  pct_elections_with_decoys = c(
    elections_with_main_minor_decoys / total_elections * 100,
    elections_with_main_main_decoys / total_elections * 100,
    elections_with_minor_minor_decoys / total_elections * 100
  )
)

decoy_by_pair_type <- candidate_pairs_3 %>%
  # creae a new consolidated pair type variable
  mutate(
    Consolidated_Pair_Type = case_when(
      Pair_Type == "main-minor" ~ "main-minor/minor-main",
      Pair_Type == "minor-main" ~ "main-minor/minor-main",
      TRUE ~ Pair_Type
    )
  ) %>%
  # in "minor-minor" collapse dont have minor candidates that are decoys of main
  filter(!(Consolidated_Pair_Type == "minor-minor" & 
             (Candidate1_PID %in% minor_candidates_that_are_decoys_of_main | 
                Candidate2_PID %in% minor_candidates_that_are_decoys_of_main))) %>%
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
  filename = "180425 meeting plots/Decoy_pairs_by_type_filtered.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

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
      pull(Candidate1_PID),
    temp_candidate_pairs %>% 
      filter(Pair_Type == "main-minor", is_decoy == TRUE) %>% 
      pull(Candidate2_PID)
  ))
  
  minor_minor_pairs <- temp_candidate_pairs %>%
    filter(Pair_Type == "minor-minor") %>%
    filter(!(Candidate1_PID %in% minor_candidates_that_are_decoys_of_main | 
               Candidate2_PID %in% minor_candidates_that_are_decoys_of_main)) %>%
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
    distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
    nrow()
  
  elections_with_main_minor <- temp_candidate_pairs %>%
    filter((Pair_Type == "main-minor" | Pair_Type == "minor-main") & is_decoy == TRUE) %>%
    distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
    nrow()
  
  elections_with_main_main <- temp_candidate_pairs %>%
    filter(Pair_Type == "main-main" & is_decoy == TRUE) %>%
    distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
    nrow()
  
  elections_with_minor_minor <- temp_candidate_pairs %>%
    filter(Pair_Type == "minor-minor" & is_decoy == TRUE) %>%
    filter(!(Candidate1_PID %in% minor_candidates_that_are_decoys_of_main | 
               Candidate2_PID %in% minor_candidates_that_are_decoys_of_main)) %>%
    distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
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

# run the analysis for all percentiles (using purrr's map function)
sensitivity_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs_3, .x, method = "strict", lower_percentile = 0.974))

lv_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs_3, .x, method = "only_levenshtein"))

jw_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs_3, .x, method = "only_jaro_winkler"))

ng_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs_3, .x, method = "only_ngram"))

mp_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs_3, .x, method = "only_metaphone"))

mas_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs_3, .x, method = "only_masala"))

# get pair results and election results
# if need to load
sensitivity_results <- readRDS("sensitivity_analysis_results/ind_filtered_og/sensitivity_results.rds")
lv_results <- readRDS("sensitivity_analysis_results/ind_filtered_og/lv_results.rds")
jw_results <- readRDS("sensitivity_analysis_results/ind_filtered_og/jw_results.rds")
ng_results <- readRDS("sensitivity_analysis_results/ind_filtered_og/ng_results.rds")
mp_results <- readRDS("sensitivity_analysis_results/ind_filtered_og/mp_results.rds")
mas_results <- readRDS("sensitivity_analysis_results/ind_filtered_og/mas_results.rds")

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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_filtered_og.png",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_filtered_og_2.png",
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
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Elections with Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_elections_filtered_og.png",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_lv_filtered_og.png",
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
  scale_y_continuous(breaks = seq(0, max(plot_data$pct_decoy, na.rm = TRUE), by = 0.5)) +
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_lv_filtered_og_2.png",
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
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Elections with Decoys; Normalized Levenshtein above x-axis value",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_elections_lv_filtered_og.png",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
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
  scale_y_continuous(breaks = seq(0, max(plot_data$pct_decoy, na.rm = TRUE), by = 0.5)) +
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_jw_filtered_og_2.png",
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
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Elections with Decoys; Jaro-Winkler above x-axis value",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_elections_jw_filtered_og.png",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_mp_filtered_og_2.png",
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
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Elections with Decoys; Double Metaphone above x-axis value",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_elections_mp_filtered_og.png",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
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
  scale_y_continuous(breaks = seq(0, max(plot_data$pct_decoy, na.rm = TRUE), by = 0.5)) +
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_ng_filtered_og_2.png",
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
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Elections with Decoys; NGram above x-axis value",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_elections_ng_filtered_og.png",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
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
  scale_y_continuous(breaks = seq(0, max(plot_data$pct_decoy, na.rm = TRUE), by = 0.5)) +
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.005)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_mas_filtered_og_2.png",
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
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Elections with Decoys; Masala Merge above x-axis value",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_elections_mas_filtered_og.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# Save each result set as an RDS file
saveRDS(sensitivity_results, "sensitivity_analysis_results/ind_filtered_og/sensitivity_results.rds")
saveRDS(lv_results, "sensitivity_analysis_results/ind_filtered_og/lv_results.rds")
saveRDS(jw_results, "sensitivity_analysis_results/ind_filtered_og/jw_results.rds")
saveRDS(ng_results, "sensitivity_analysis_results/ind_filtered_og/ng_results.rds")
saveRDS(mp_results, "sensitivity_analysis_results/ind_filtered_og/mp_results.rds")
saveRDS(mas_results, "sensitivity_analysis_results/ind_filtered_og/mas_results.rds")

### Martin's suggestion
baseline_false_positive_rate <- mean(c(
  decoy_by_pair_type %>% 
    filter(Consolidated_Pair_Type == "main-main") %>% 
    pull(pct_decoy),
  decoy_by_pair_type %>% 
    filter(Consolidated_Pair_Type == "minor-minor") %>% 
    pull(pct_decoy)
))

# alternative baseline fp rate
baseline_false_positive_rate_2 <- 
  decoy_by_pair_type %>% 
  filter(Consolidated_Pair_Type == "main-minor/minor-main") %>% 
  pull(pct_decoy) * 
  (mean(
    (decoy_by_pair_type %>% 
       filter(Consolidated_Pair_Type == "main-main") %>% 
       pull(pct_decoy))/
      decoy_by_pair_type %>% 
      filter(Consolidated_Pair_Type == "main-minor/minor-main") %>% 
      pull(pct_decoy), 
    (decoy_by_pair_type %>% 
       filter(Consolidated_Pair_Type == "minor-minor") %>% 
       pull(pct_decoy))/
      decoy_by_pair_type %>% 
      filter(Consolidated_Pair_Type == "main-minor/minor-main") %>% 
      pull(pct_decoy))
  )

# how many main-minor/minor-main decoy pairs to keep
# first get the total number of main-minor/minor-main pairs
main_minor_total_pairs <- decoy_by_pair_type %>%
  filter(Consolidated_Pair_Type == "main-minor/minor-main") %>%
  pull(total_pairs)

# how many decoy pairs should remain to match the baseline rate
main_minor_decoys_to_keep <- round(main_minor_total_pairs * baseline_false_positive_rate / 100)

# get the current number of decoy pairs
main_minor_current_decoys <- decoy_by_pair_type %>%
  filter(Consolidated_Pair_Type == "main-minor/minor-main") %>%
  pull(decoy_pairs)

# calculate how many to remove
main_minor_decoys_to_remove <- main_minor_current_decoys - main_minor_decoys_to_keep

# create a modified version of candidate_pairs with randomly removed decoys
set.seed(123)

# get indices of main-minor/minor-main decoy pairs to keep
main_minor_decoy_pairs <- candidate_pairs %>%
  filter((Pair_Type == "main-minor" | Pair_Type == "minor-main") & is_decoy == TRUE)

decoys_to_remove <- main_minor_decoy_pairs %>%
  sample_n(main_minor_decoys_to_remove)

# create adjusted candidate_pairs dataset
adjusted_candidate_pairs <- candidate_pairs %>%
  anti_join(decoys_to_remove, by = c("Candidate1_PID", "Candidate2_PID", "Year", "State_Name", 
                                     "Constituency_Name", "Assembly_No", "Election_Type"))

# now, recalculate everything
# recreate elections with decoys calculations
adjusted_elections_with_main_minor_decoys <- adjusted_candidate_pairs %>%
  filter((Pair_Type == "main-minor" | Pair_Type == "minor-main") & is_decoy == TRUE) %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
  nrow()

adjusted_elections_with_main_main_decoys <- adjusted_candidate_pairs %>%
  filter(Pair_Type == "main-main" & is_decoy == TRUE) %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
  nrow()

# recalculate minor candidates that are decoys of main
adjusted_minor_candidates_that_are_decoys_of_main <- unique(c(
  adjusted_candidate_pairs %>% 
    filter(Pair_Type == "minor-main", is_decoy == TRUE) %>% 
    pull(Candidate1_Election_ID),
  adjusted_candidate_pairs %>% 
    filter(Pair_Type == "main-minor", is_decoy == TRUE) %>% 
    pull(Candidate2_Election_ID)
))

adjusted_elections_with_minor_minor_decoys <- adjusted_candidate_pairs %>%
  filter((Pair_Type == "minor-minor") & is_decoy == TRUE) %>%
  filter(!(Candidate1_Election_ID %in% adjusted_minor_candidates_that_are_decoys_of_main | 
             Candidate2_Election_ID %in% adjusted_minor_candidates_that_are_decoys_of_main)) %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
  nrow()

# get total elections
total_elections <- adjusted_candidate_pairs %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
  nrow()

# create new data frames for plotting
adjusted_elections_with_decoys_df <- data.frame(
  Consolidated_Pair_Type = c("main-minor/minor-main", "main-main", "minor-minor"),
  pct_elections_with_decoys = c(
    adjusted_elections_with_main_minor_decoys / total_elections * 100,
    adjusted_elections_with_main_main_decoys / total_elections * 100,
    adjusted_elections_with_minor_minor_decoys / total_elections * 100
  )
)

# recalculate decoy by pair type
adjusted_decoy_by_pair_type <- adjusted_candidate_pairs %>%
  mutate(
    Consolidated_Pair_Type = case_when(
      Pair_Type == "main-minor" ~ "main-minor/minor-main",
      Pair_Type == "minor-main" ~ "main-minor/minor-main",
      TRUE ~ Pair_Type
    )
  ) %>%
  filter(!(Consolidated_Pair_Type == "minor-minor" & 
             (Candidate1_Election_ID %in% adjusted_minor_candidates_that_are_decoys_of_main | 
                Candidate2_Election_ID %in% adjusted_minor_candidates_that_are_decoys_of_main))) %>%
  group_by(Consolidated_Pair_Type) %>%
  dplyr::summarize(
    total_pairs = n(),
    decoy_pairs = sum(is_decoy, na.rm = TRUE),
    pct_decoy = round(decoy_pairs / total_pairs * 100, 2),
    .groups = "drop"
  )

# combine for plotting
adjusted_plot_data <- bind_rows(
  adjusted_decoy_by_pair_type %>% 
    dplyr::select(Consolidated_Pair_Type, percentage = pct_decoy) %>%
    mutate(metric = "% of Pairs that are Decoys",
           adjusted = TRUE),
  
  adjusted_elections_with_decoys_df %>%
    dplyr::select(Consolidated_Pair_Type, percentage = pct_elections_with_decoys) %>%
    mutate(metric = "% of Elections with Decoys",
           adjusted = TRUE), 
  
  decoy_by_pair_type %>%
    dplyr::select(Consolidated_Pair_Type, percentage = pct_decoy) %>%
    mutate(metric = "% of Pairs that are Decoys",
           adjusted = FALSE), 
  
  elections_with_decoys_df %>%
    dplyr::select(Consolidated_Pair_Type, percentage = pct_elections_with_decoys) %>%
    mutate(metric = "% of Elections with Decoys",
           adjusted = FALSE)
)

# we just need the main-minor bars 
adjusted_adjusted_plot_data <- adjusted_plot_data %>%
  filter(Consolidated_Pair_Type == "main-minor/minor-main")

adjusted_adjusted_plot_data$adjusted <- factor(adjusted_adjusted_plot_data$adjusted, 
                                               levels = c(FALSE, TRUE), 
                                               labels = c("Not Adjusted", "Adjusted"))

# plot
ggplot(adjusted_adjusted_plot_data, aes(x = adjusted, y = percentage, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.2f%%", percentage)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), 
                     limits = c(0, max(adjusted_plot_data$percentage) * 1.1)) +
  labs(title = "Decoy Pairs by Pair Type (With Random Removal Adjustment)",
       subtitle = paste0("Baseline false positive rate: ", round(baseline_false_positive_rate, 2), "%"),
       x = "Pair Type",
       y = "Percentage",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "180425 meeting plots/Decoy_pairs_by_type_random_removal.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# using alternative baseline positive rate
# how many main-minor/minor-main decoy pairs to keep
# first get the total number of main-minor/minor-main pairs
main_minor_total_pairs <- decoy_by_pair_type %>%
  filter(Consolidated_Pair_Type == "main-minor/minor-main") %>%
  pull(total_pairs)

# how many decoy pairs should remain to match the baseline rate
main_minor_decoys_to_keep <- round(main_minor_total_pairs * baseline_false_positive_rate_2 / 100)

# get the current number of decoy pairs
main_minor_current_decoys <- decoy_by_pair_type %>%
  filter(Consolidated_Pair_Type == "main-minor/minor-main") %>%
  pull(decoy_pairs)

# calculate how many to remove
main_minor_decoys_to_remove <- main_minor_current_decoys - main_minor_decoys_to_keep

# create a modified version of candidate_pairs with randomly removed decoys
set.seed(123)

# get indices of main-minor/minor-main decoy pairs to keep
main_minor_decoy_pairs <- candidate_pairs %>%
  filter((Pair_Type == "main-minor" | Pair_Type == "minor-main") & is_decoy == TRUE)

decoys_to_remove <- main_minor_decoy_pairs %>%
  sample_n(main_minor_decoys_to_remove)

# create adjusted candidate_pairs dataset
adjusted_candidate_pairs <- candidate_pairs %>%
  anti_join(decoys_to_remove, by = c("Candidate1_Election_ID", "Candidate2_Election_ID", "Year", "State_Name", 
                                     "Constituency_Name", "Assembly_No", "Election_Type"))

# now, recalculate everything

# recreate elections with decoys calculations
adjusted_elections_with_main_minor_decoys <- adjusted_candidate_pairs %>%
  filter((Pair_Type == "main-minor" | Pair_Type == "minor-main") & is_decoy == TRUE) %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
  nrow()

adjusted_elections_with_main_main_decoys <- adjusted_candidate_pairs %>%
  filter(Pair_Type == "main-main" & is_decoy == TRUE) %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
  nrow()

# recalculate minor candidates that are decoys of main
adjusted_minor_candidates_that_are_decoys_of_main <- unique(c(
  adjusted_candidate_pairs %>% 
    filter(Pair_Type == "minor-main", is_decoy == TRUE) %>% 
    pull(Candidate1_Election_ID),
  adjusted_candidate_pairs %>% 
    filter(Pair_Type == "main-minor", is_decoy == TRUE) %>% 
    pull(Candidate2_Election_ID)
))

adjusted_elections_with_minor_minor_decoys <- adjusted_candidate_pairs %>%
  filter((Pair_Type == "minor-minor") & is_decoy == TRUE) %>%
  filter(!(Candidate1_Election_ID %in% adjusted_minor_candidates_that_are_decoys_of_main | 
             Candidate2_Election_ID %in% adjusted_minor_candidates_that_are_decoys_of_main)) %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
  nrow()

# get total elections
total_elections <- adjusted_candidate_pairs %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
  nrow()

# create new data frames for plotting
adjusted_elections_with_decoys_df <- data.frame(
  Consolidated_Pair_Type = c("main-minor/minor-main", "main-main", "minor-minor"),
  pct_elections_with_decoys = c(
    adjusted_elections_with_main_minor_decoys / total_elections * 100,
    adjusted_elections_with_main_main_decoys / total_elections * 100,
    adjusted_elections_with_minor_minor_decoys / total_elections * 100
  )
)

# recalculate decoy by pair type
adjusted_decoy_by_pair_type <- adjusted_candidate_pairs %>%
  mutate(
    Consolidated_Pair_Type = case_when(
      Pair_Type == "main-minor" ~ "main-minor/minor-main",
      Pair_Type == "minor-main" ~ "main-minor/minor-main",
      TRUE ~ Pair_Type
    )
  ) %>%
  filter(!(Consolidated_Pair_Type == "minor-minor" & 
             (Candidate1_Election_ID %in% adjusted_minor_candidates_that_are_decoys_of_main | 
                Candidate2_Election_ID %in% adjusted_minor_candidates_that_are_decoys_of_main))) %>%
  group_by(Consolidated_Pair_Type) %>%
  dplyr::summarize(
    total_pairs = n(),
    decoy_pairs = sum(is_decoy, na.rm = TRUE),
    pct_decoy = round(decoy_pairs / total_pairs * 100, 2),
    .groups = "drop"
  )

# combine for plotting
adjusted_plot_data <- bind_rows(
  adjusted_decoy_by_pair_type %>% 
    dplyr::select(Consolidated_Pair_Type, percentage = pct_decoy) %>%
    mutate(metric = "% of Pairs that are Decoys",
           adjusted = TRUE),
  
  adjusted_elections_with_decoys_df %>%
    dplyr::select(Consolidated_Pair_Type, percentage = pct_elections_with_decoys) %>%
    mutate(metric = "% of Elections with Decoys",
           adjusted = TRUE), 
  
  decoy_by_pair_type %>%
    dplyr::select(Consolidated_Pair_Type, percentage = pct_decoy) %>%
    mutate(metric = "% of Pairs that are Decoys",
           adjusted = FALSE), 
  
  elections_with_decoys_df %>%
    dplyr::select(Consolidated_Pair_Type, percentage = pct_elections_with_decoys) %>%
    mutate(metric = "% of Elections with Decoys",
           adjusted = FALSE)
)

# we just need the main-minor bars 
adjusted_adjusted_plot_data <- adjusted_plot_data %>%
  filter(Consolidated_Pair_Type == "main-minor/minor-main")

adjusted_adjusted_plot_data$adjusted <- factor(adjusted_adjusted_plot_data$adjusted, 
                                               levels = c(FALSE, TRUE), 
                                               labels = c("Not Adjusted", "Adjusted"))

# plot
ggplot(adjusted_adjusted_plot_data, aes(x = adjusted, y = percentage, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.2f%%", percentage)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), 
                     limits = c(0, max(adjusted_plot_data$percentage) * 1.1)) +
  labs(title = "Decoy Pairs by Pair Type (With Random Removal Adjustment)",
       subtitle = paste0("Baseline false positive rate: ", round(baseline_false_positive_rate_2, 2), "%"),
       x = "Pair Type",
       y = "Percentage",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "180425 meeting plots/Decoy_pairs_by_type_random_removal_2.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

#### CLEA #### 
# remove with invalid election id
CLEA_cleaned <- CLEA_cleaned %>%
  filter(id != -999)

candidate_pairs_CLEA_full <- candidate_pairs_CLEA_full %>%
  filter(Election_ID != -999)

### remove india
CLEA_cleaned_no_ind <- CLEA_cleaned %>%
  filter(ctr != 356)

candidate_pairs_CLEA_no_ind <- candidate_pairs_CLEA_full %>%
  filter(Country_Code != 356)

unique_elections_CLEA_no_ind <- candidate_pairs_CLEA_no_ind %>%
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
coverage_data <- CLEA_cleaned_no_ind %>%
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
  filename = "180425 meeting plots/CLEA_coverage.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# categorize candidates as main or minor
CLEA_cleaned_no_ind <- CLEA_cleaned_no_ind %>%
  mutate(candidate_type = ifelse(cvs1 > 0.1, "main", "minor"))

# boxplot of main/minor across regions
ggplot(CLEA_cleaned_no_ind, aes(x = rg, y = cvs1, fill = candidate_type)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "Distribution of Vote Shares by Region",
       x = "Region", y = "Vote Share (%)",
       fill = "Candidate Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "180425 meeting plots/CLEA_main_minor_rates.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

### voteshare distribution across countries
# calculate total vote share by candidate type per year
voteshare <- CLEA_cleaned_no_ind %>%
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
  filename = "180425 meeting plots/CLEA_voteshare_main_minor.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

### False positive stuff
### making is_decoy
# thresholds
lv_99th <- quantile(candidate_pairs_CLEA_no_ind$Levenshtein_Similarity, 0.99, na.rm = TRUE)
jw_99th <- quantile(candidate_pairs_CLEA_no_ind$Jaro_Winkler_Similarity, 0.99, na.rm = TRUE)
mp_99th <- quantile(candidate_pairs_CLEA_no_ind$Metaphone_Similarity, 0.99, na.rm = TRUE)
mas_99th <- quantile(candidate_pairs_CLEA_no_ind$Masala_Similarity, 0.99, na.rm = TRUE)
ng_99th <- quantile(candidate_pairs_CLEA_no_ind$NGram_Similarity, 0.99, na.rm = TRUE)

lv_95th <- quantile(candidate_pairs_CLEA_no_ind$Levenshtein_Similarity, 0.95, na.rm = TRUE)
jw_95th <- quantile(candidate_pairs_CLEA_no_ind$Jaro_Winkler_Similarity, 0.95, na.rm = TRUE)
mp_95th <- quantile(candidate_pairs_CLEA_no_ind$Metaphone_Similarity, 0.95, na.rm = TRUE)
mas_95th <- quantile(candidate_pairs_CLEA_no_ind$Masala_Similarity, 0.95, na.rm = TRUE)
ng_95th <- quantile(candidate_pairs_CLEA_no_ind$NGram_Similarity, 0.95, na.rm = TRUE)

candidate_pairs_CLEA_no_ind <- candidate_pairs_CLEA_no_ind %>%
  mutate(is_decoy = ifelse(
    Levenshtein_Similarity > lv_99th & 
      Jaro_Winkler_Similarity > jw_99th & 
      Metaphone_Similarity > mp_99th & 
      Masala_Similarity > mas_99th & 
      NGram_Similarity > ng_99th,
    TRUE, 
    FALSE
  ))

### how are main_main, main_minor and minor_minor different? false positives? 
candidate_pairs_CLEA_no_ind <- candidate_pairs_CLEA_no_ind %>%
  mutate(
    Candidate1_Election_ID = paste(Year, Country_Code, Constituency_Name, Candidate1_Party_Name, Candidate1_Name, Candidate1_Votes, sep = "_"),
    Candidate2_Election_ID = paste(Year, Country_Code, Constituency_Name, Candidate2_Party_Name, Candidate2_Name, Candidate2_Votes, sep = "_")
  )

candidate_pairs_CLEA_no_ind_main_minor <- candidate_pairs_CLEA_no_ind %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main", 
         is_decoy == TRUE)

candidate_pairs_CLEA_no_ind_main_main <- candidate_pairs_CLEA_no_ind %>%
  filter(Pair_Type == "main-main", 
         is_decoy == TRUE)

candidate_pairs_CLEA_no_ind_minor_minor <- candidate_pairs_CLEA_no_ind %>%
  filter(Pair_Type == "minor-minor", 
         is_decoy == TRUE)

minor_candidates_that_are_decoys_of_minor <- unique(c(
  candidate_pairs_CLEA_no_ind_minor_minor$Candidate1_Election_ID,
  candidate_pairs_CLEA_no_ind_minor_minor$Candidate2_Election_ID
))

minor_candidates_that_are_decoys_of_main <- unique(c(
  candidate_pairs_CLEA_no_ind_main_minor %>% 
    filter(Pair_Type == "minor-main") %>% 
    pull(Candidate1_Election_ID),
  candidate_pairs_CLEA_no_ind_main_minor %>% 
    filter(Pair_Type == "main-minor") %>% 
    pull(Candidate2_Election_ID)
))

overlap_candidates <- intersect(minor_candidates_that_are_decoys_of_minor, minor_candidates_that_are_decoys_of_main)

length(overlap_candidates) / length(minor_candidates_that_are_decoys_of_minor) * 100
# 10% of minor-minor candidates were decoys of main-minor candidates

elections_with_main_minor_decoys <- candidate_pairs_CLEA_no_ind %>%
  filter((Pair_Type == "main-minor" | Pair_Type == "minor-main") & is_decoy == TRUE) %>%
  distinct(Country_Code, Year, Election_Month, Constituency_Name, Election_ID) %>%
  nrow()

elections_with_main_main_decoys <- candidate_pairs_CLEA_no_ind %>%
  filter(Pair_Type == "main-main" & is_decoy == TRUE) %>%
  distinct(Country_Code, Year, Election_Month, Constituency_Name, Election_ID) %>%
  nrow()

# remove overlap minor candidates! 
elections_with_minor_minor_decoys <- candidate_pairs_CLEA_no_ind %>%
  filter((Pair_Type == "minor-minor") & is_decoy == TRUE) %>%
  filter(!(Candidate1_Election_ID %in% minor_candidates_that_are_decoys_of_main | 
             Candidate2_Election_ID %in% minor_candidates_that_are_decoys_of_main)) %>%
  distinct(Country_Code, Year, Election_Month, Constituency_Name, Election_ID) %>%
  nrow()

total_elections <- candidate_pairs_CLEA_no_ind %>%
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

decoy_by_pair_type <- candidate_pairs_CLEA_no_ind %>%
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
  filename = "180425 meeting plots/Decoy_pairs_by_type_CLEA.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# are the overlaps really overlaps? 
overlap_candidates_df <- candidate_pairs_CLEA_no_ind %>%
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

### Applying Martin's Suggestion to these as well
baseline_false_positive_rate <- mean(c(
  decoy_by_pair_type %>% 
    filter(Consolidated_Pair_Type == "main-main") %>% 
    pull(pct_decoy),
  decoy_by_pair_type %>% 
    filter(Consolidated_Pair_Type == "minor-minor") %>% 
    pull(pct_decoy)
))

# alternative baseline fp rate
baseline_false_positive_rate_2 <- 
  decoy_by_pair_type %>% 
  filter(Consolidated_Pair_Type == "main-minor/minor-main") %>% 
  pull(pct_decoy) * 
  (mean(
    (decoy_by_pair_type %>% 
       filter(Consolidated_Pair_Type == "main-main") %>% 
       pull(pct_decoy))/
      decoy_by_pair_type %>% 
      filter(Consolidated_Pair_Type == "main-minor/minor-main") %>% 
      pull(pct_decoy), 
    (decoy_by_pair_type %>% 
       filter(Consolidated_Pair_Type == "minor-minor") %>% 
       pull(pct_decoy))/
      decoy_by_pair_type %>% 
      filter(Consolidated_Pair_Type == "main-minor/minor-main") %>% 
      pull(pct_decoy))
  )

# how many main-minor/minor-main decoy pairs to keep
# first get the total number of main-minor/minor-main pairs
main_minor_total_pairs <- decoy_by_pair_type %>%
  filter(Consolidated_Pair_Type == "main-minor/minor-main") %>%
  pull(total_pairs)

# how many decoy pairs should remain to match the baseline rate
main_minor_decoys_to_keep <- round(main_minor_total_pairs * baseline_false_positive_rate / 100)

# get the current number of decoy pairs
main_minor_current_decoys <- decoy_by_pair_type %>%
  filter(Consolidated_Pair_Type == "main-minor/minor-main") %>%
  pull(decoy_pairs)

# calculate how many to remove
main_minor_decoys_to_remove <- main_minor_current_decoys - main_minor_decoys_to_keep

# create a modified version of candidate_pairs with randomly removed decoys
set.seed(123)

# get indices of main-minor/minor-main decoy pairs to keep
main_minor_decoy_pairs <- candidate_pairs_CLEA_no_ind %>%
  filter((Pair_Type == "main-minor" | Pair_Type == "minor-main") & is_decoy == TRUE)

decoys_to_remove <- main_minor_decoy_pairs %>%
  sample_n(main_minor_decoys_to_remove)

# create adjusted candidate_pairs dataset
adjusted_candidate_pairs <- candidate_pairs_CLEA_no_ind %>%
  anti_join(decoys_to_remove, by = c("Candidate1_Name", "Candidate2_Name", "Year", "Country_Code", 
                                     "Constituency_Name", "Election_ID", "Election_Month"))

# now, recalculate everything
# recreate elections with decoys calculations
# now, recalculate everything
# recreate elections with decoys calculations
adjusted_elections_with_main_minor_decoys <- adjusted_candidate_pairs %>%
  filter((Pair_Type == "main-minor" | Pair_Type == "minor-main") & is_decoy == TRUE) %>%
  distinct(Year, Country_Code, Constituency_Name, Election_Month, Election_ID) %>%
  nrow()

adjusted_elections_with_main_main_decoys <- adjusted_candidate_pairs %>%
  filter(Pair_Type == "main-main" & is_decoy == TRUE) %>%
  distinct(Year, Country_Code, Constituency_Name, Election_Month, Election_ID) %>%
  nrow()

# recalculate minor candidates that are decoys of main
adjusted_minor_candidates_that_are_decoys_of_main <- unique(c(
  adjusted_candidate_pairs %>% 
    filter(Pair_Type == "minor-main", is_decoy == TRUE) %>% 
    pull(Candidate1_Election_ID),
  adjusted_candidate_pairs %>% 
    filter(Pair_Type == "main-minor", is_decoy == TRUE) %>% 
    pull(Candidate2_Election_ID)
))

adjusted_elections_with_minor_minor_decoys <- adjusted_candidate_pairs %>%
  filter((Pair_Type == "minor-minor") & is_decoy == TRUE) %>%
  filter(!(Candidate1_Election_ID %in% adjusted_minor_candidates_that_are_decoys_of_main | 
             Candidate2_Election_ID %in% adjusted_minor_candidates_that_are_decoys_of_main)) %>%
  distinct(Year, Country_Code, Constituency_Name, Election_Month, Election_ID) %>%
  nrow()

# get total elections
total_elections <- adjusted_candidate_pairs %>%
  distinct(Year, Country_Code, Constituency_Name, Election_Month, Election_ID) %>%
  nrow()

# create new data frames for plotting
adjusted_elections_with_decoys_df <- data.frame(
  Consolidated_Pair_Type = c("main-minor/minor-main", "main-main", "minor-minor"),
  pct_elections_with_decoys = c(
    adjusted_elections_with_main_minor_decoys / total_elections * 100,
    adjusted_elections_with_main_main_decoys / total_elections * 100,
    adjusted_elections_with_minor_minor_decoys / total_elections * 100
  )
)

# recalculate decoy by pair type
adjusted_decoy_by_pair_type <- adjusted_candidate_pairs %>%
  mutate(
    Consolidated_Pair_Type = case_when(
      Pair_Type == "main-minor" ~ "main-minor/minor-main",
      Pair_Type == "minor-main" ~ "main-minor/minor-main",
      TRUE ~ Pair_Type
    )
  ) %>%
  filter(!(Consolidated_Pair_Type == "minor-minor" & 
             (Candidate1_Election_ID %in% adjusted_minor_candidates_that_are_decoys_of_main | 
                Candidate2_Election_ID %in% adjusted_minor_candidates_that_are_decoys_of_main))) %>%
  group_by(Consolidated_Pair_Type) %>%
  dplyr::summarize(
    total_pairs = n(),
    decoy_pairs = sum(is_decoy, na.rm = TRUE),
    pct_decoy = round(decoy_pairs / total_pairs * 100, 2),
    .groups = "drop"
  )

# combine for plotting
adjusted_plot_data <- bind_rows(
  adjusted_decoy_by_pair_type %>% 
    dplyr::select(Consolidated_Pair_Type, percentage = pct_decoy) %>%
    mutate(metric = "% of Pairs that are Decoys",
           adjusted = TRUE),
  
  adjusted_elections_with_decoys_df %>%
    dplyr::select(Consolidated_Pair_Type, percentage = pct_elections_with_decoys) %>%
    mutate(metric = "% of Elections with Decoys",
           adjusted = TRUE), 
  
  decoy_by_pair_type %>%
    dplyr::select(Consolidated_Pair_Type, percentage = pct_decoy) %>%
    mutate(metric = "% of Pairs that are Decoys",
           adjusted = FALSE), 
  
  elections_with_decoys_df %>%
    dplyr::select(Consolidated_Pair_Type, percentage = pct_elections_with_decoys) %>%
    mutate(metric = "% of Elections with Decoys",
           adjusted = FALSE)
)

# we just need the main-minor bars 
adjusted_adjusted_plot_data <- adjusted_plot_data %>%
  filter(Consolidated_Pair_Type == "main-minor/minor-main")

adjusted_adjusted_plot_data$adjusted <- factor(adjusted_adjusted_plot_data$adjusted, 
                                               levels = c(FALSE, TRUE), 
                                               labels = c("Not Adjusted", "Adjusted"))

# plot
ggplot(adjusted_adjusted_plot_data, aes(x = adjusted, y = percentage, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.2f%%", percentage)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), 
                     limits = c(0, max(adjusted_plot_data$percentage) * 1.1)) +
  labs(title = "Decoy Pairs by Pair Type (With Random Removal Adjustment)",
       subtitle = paste0("Baseline false positive rate: ", round(baseline_false_positive_rate, 2), "%"),
       x = "Pair Type",
       y = "Percentage",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "180425 meeting plots/Decoy_pairs_by_type_random_removal_CLEA.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

### summaries at election level
election_decoys <- candidate_pairs_CLEA_no_ind %>%
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

sum(candidate_pairs_CLEA_no_ind$is_decoy)/nrow(candidate_pairs_CLEA_no_ind)

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
  filename = "180425 meeting plots/Main_Minor_Decoy_Histogram_CLEA.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# creating no. of candidates per election df from CLEA_cleaned_fr_no_ind
constituency_candidates <- CLEA_cleaned_no_ind %>%
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

constituency_metrics <- candidate_pairs_CLEA_no_ind %>%
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

country_codes <- CLEA_cleaned_no_ind %>%
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
    geom_text(aes(label = sprintf("%.1f%%", pct_races_with_decoys)), 
              vjust = -0.5, size = 3) +
    labs(title = "Percentage of Elections with Decoys by Country",
         x = "Country",
         y = "Percentage of Elections with Decoys (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "180425 meeting plots/Decoy_elections_country_histogram_CLEA.png",
  plot = last_plot(),
  width = 12,
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
  filename = "180425 meeting plots/Decoy_elections_atleast3_country_histogram_CLEA.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

country_metrics_1 %>%
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
  filename = "180425 meeting plots/Decoy_candidate_country_CLEA.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

country_metrics_1 %>%
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
  filename = "180425 meeting plots/Decoy_candidates_number_country_CLEA.png",
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
  filename = "180425 meeting plots/Decoy_share_over_time_CLEA.png",
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
  filename = "180425 meeting plots/Decoy_voteshare_over_time_CLEA.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

country_metrics_1 %>%
  filter(overall_decoy_vote_share > 0) %>%
  # reorder
  mutate(Country = fct_reorder(Country, overall_decoy_vote_share)) %>%
  ggplot(aes(x = Country, y = overall_decoy_vote_share)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = sprintf("%.3f%%", overall_decoy_vote_share)), 
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
  filename = "180425 meeting plots/Decoy_voteshare_by_country_CLEA.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

### Reallocating votes to victim
decoy_pairs <- candidate_pairs_CLEA_no_ind %>%
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
  election_candidates <- CLEA_cleaned_no_ind %>%
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

### NONE!

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

quantile(candidate_pairs$Metaphone_Similarity, probs = c(seq(0.974, 1.000, by = 0.001)))

# mp_974th <- quantile(candidate_pairs_CLEA_no_ind$Metaphone_Similarity, 0.974, na.rm = TRUE)
# mp_986th <- quantile(candidate_pairs_CLEA_no_ind$Metaphone_Similarity, 0.986, na.rm = TRUE)
# mp_999th <- quantile(candidate_pairs_CLEA_no_ind$Metaphone_Similarity, 0.999, na.rm = TRUE)
# 
# candidate_pairs_CLEA_no_ind <- candidate_pairs_CLEA_no_ind %>%
#   mutate(is_decoy_metaphone = ifelse(Metaphone_Similarity > mp_974th, 1, 0))
# 
# sum(candidate_pairs_CLEA_no_ind$is_decoy_metaphone & (candidate_pairs_CLEA_no_ind$Pair_Type == "main-minor" | 
#                                                         candidate_pairs_CLEA_no_ind$Pair_Type == "minor-main"))
# 
# table(candidate_pairs_CLEA_no_ind$Metaphone_Similarity)
# table(candidate_pairs$Metaphone_Similarity)
# 
# (sum(candidate_pairs_CLEA_no_ind$is_decoy_metaphone & (candidate_pairs_CLEA_no_ind$Pair_Type == "main-minor" | 
#                                                  candidate_pairs_CLEA_no_ind$Pair_Type == "minor-main"))/sum(candidate_pairs_CLEA_no_ind$Pair_Type == "main-minor" | 
#                                                 candidate_pairs_CLEA_no_ind$Pair_Type == "minor-main"))
# 
# candidate_pairs_CLEA_no_ind <- candidate_pairs_CLEA_no_ind %>%
#   mutate(is_decoy_metaphone = ifelse(Metaphone_Similarity >= mp_986th, 1, 0))
# 
# (sum(candidate_pairs_CLEA_no_ind$is_decoy_metaphone & (candidate_pairs_CLEA_no_ind$Pair_Type == "main-minor" | 
#                                                          candidate_pairs_CLEA_no_ind$Pair_Type == "minor-main"))/sum(candidate_pairs_CLEA_no_ind$Pair_Type == "main-minor" | 
#                                                                                                                        candidate_pairs_CLEA_no_ind$Pair_Type == "minor-main"))
# 
# candidate_pairs_CLEA_no_ind <- candidate_pairs_CLEA_no_ind %>%
#   mutate(is_decoy_metaphone = ifelse(Metaphone_Similarity >= mp_999th, 1, 0))
# 
# (sum(candidate_pairs_CLEA_no_ind$is_decoy_metaphone & (candidate_pairs_CLEA_no_ind$Pair_Type == "main-minor" | 
#                                                          candidate_pairs_CLEA_no_ind$Pair_Type == "minor-main"))/sum(candidate_pairs_CLEA_no_ind$Pair_Type == "main-minor" | 
#                                                                                                                        candidate_pairs_CLEA_no_ind$Pair_Type == "minor-main"))

# run the analysis for all percentiles (using purrr's map function)
sensitivity_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs_CLEA_no_ind, .x, method = "strict", lower_percentile = 0.974))

lv_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs_CLEA_no_ind, .x, method = "only_levenshtein"))

jw_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs_CLEA_no_ind, .x, method = "only_jaro_winkler"))

ng_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs_CLEA_no_ind, .x, method = "only_ngram"))

mp_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs_CLEA_no_ind, .x, method = "only_metaphone"))

mas_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs_CLEA_no_ind, .x, method = "only_masala"))

# get pair results and election results
# if need to load
sensitivity_results <- readRDS("sensitivity_analysis_results/clea_normal/sensitivity_results.rds")
lv_results <- readRDS("sensitivity_analysis_results/clea_normal/lv_results.rds")
jw_results <- readRDS("sensitivity_analysis_results/clea_normal/jw_results.rds")
ng_results <- readRDS("sensitivity_analysis_results/clea_normal/ng_results.rds")
mp_results <- readRDS("sensitivity_analysis_results/clea_normal/mp_results.rds")
mas_results <- readRDS("sensitivity_analysis_results/clea_normal/mas_results.rds")

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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_clea.png",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_clea_2.png",
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
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Elections with Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_elections_clea.png",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_lv_clea.png",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_lv_clea_2.png",
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
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Elections with Decoys; Normalized Levenshtein above x-axis value",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_elections_lv_clea.png",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_jw_clea.png",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_jw_clea_2.png",
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
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Elections with Decoys; Jaro-Winkler above x-axis value",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_elections_jw_clea.png",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_mp_clea_2.png",
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
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Elections with Decoys; Double Metaphone above x-axis value",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_elections_mp_clea.png",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_ng_clea_2.png",
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
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Elections with Decoys; NGram above x-axis value",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_elections_ng_clea.png",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
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
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_pairs_mas_clea_2.png",
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
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Elections with Decoys; Masala Merge above x-axis value",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "180425 meeting plots/Sensitivity_of_decoy_elections_mas_clea.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# Save each result set as an RDS file
saveRDS(sensitivity_results, "sensitivity_analysis_results/clea_normal/sensitivity_results.rds")
saveRDS(lv_results, "sensitivity_analysis_results/clea_normal/lv_results.rds")
saveRDS(jw_results, "sensitivity_analysis_results/clea_normal/jw_results.rds")
saveRDS(ng_results, "sensitivity_analysis_results/clea_normal/ng_results.rds")
saveRDS(mp_results, "sensitivity_analysis_results/clea_normal/mp_results.rds")
saveRDS(mas_results, "sensitivity_analysis_results/clea_normal/mas_results.rds")

#### IND FP RATES - STATE SPLITS ####
constituency_election_metrics <- candidate_pairs_3 %>%
  group_by(State_Name, Year, Constituency_Name, Election_Type, Assembly_No) %>%
  dplyr::summarize(
    # basic constituency metrics
    total_candidates = max(Candidate1_N_Cand, na.rm = TRUE),
    decoy_candidates = sum((is_decoy & Pair_Type == "main-minor") | 
                             (is_decoy & Pair_Type == "minor-main"), na.rm = TRUE),
    decoy_share = round(sum((is_decoy & Pair_Type == "main-minor") | 
                              (is_decoy & Pair_Type == "minor-main"), na.rm = TRUE) /
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

state_metrics <- constituency_election_metrics %>%
  group_by(State_Name) %>%
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

unique_elections <- candidate_pairs_3 %>%
  dplyr::select(State_Name, Year, Constituency_Name, Election_Type, Assembly_No) %>%
  # summarize and count number of candidates per election
  group_by(State_Name, Year, Constituency_Name, Election_Type, Assembly_No) %>%
  summarise(
    num_candidates = n(),
    .groups = "drop"
  ) %>%
  distinct()

state_elections <- unique_elections %>%
  group_by(State_Name) %>%
  summarise(num_elections = n())

state_metrics <- state_metrics %>%
  left_join(state_elections, 
            by = "State_Name")

# limit to state with more than 10 constituencies i.e.
state_metrics <- state_metrics %>%
  filter(num_elections > 10)

minor_candidates_that_are_decoys_of_main <- unique(c(
  candidate_pairs_main_minor %>% 
    filter(Pair_Type == "minor-main") %>% 
    pull(Candidate1_Election_ID),
  candidate_pairs_main_minor %>% 
    filter(Pair_Type == "main-minor") %>% 
    pull(Candidate2_Election_ID)
))

calculate_state_false_positive_rates <- function(data, pair_type) {
  # unique election identifier from state, year, month and constituency
  data <- data %>%
    mutate(Combined_Election_ID = paste(State_Name, Year, Constituency_Name, Election_Type, Assembly_No, sep = "_"))
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
  
  # calculate false positive rates and elections with decoys by state
  state_false_positive_rates <- filtered_data %>%
    group_by(State_Name) %>%
    dplyr::summarize(
      total_pairs = n(),
      decoy_pairs = sum(is_decoy, na.rm = TRUE),
      state_false_positive_rate = round(decoy_pairs / total_pairs * 100, 2),
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
  state_false_positive_rates <- state_false_positive_rates %>%
    mutate(
      deviation_from_baseline = round(state_false_positive_rate - overall_false_positive_rate, 2),
      baseline_false_positive_rate = overall_false_positive_rate
    )
  
  # add pair type column
  state_false_positive_rates$pair_type <- pair_type
  
  return(state_false_positive_rates)
}

# calculate false positive rates for each pair type
main_minor_false_positive_rates <- calculate_state_false_positive_rates(candidate_pairs_3, "main-minor/minor-main")
main_main_false_positive_rates <- calculate_state_false_positive_rates(candidate_pairs_3, "main-main")
minor_minor_false_positive_rates <- calculate_state_false_positive_rates(candidate_pairs_3, "minor-minor")

# combine results
combined_false_positive_rates <- bind_rows(
  main_minor_false_positive_rates,
  main_main_false_positive_rates,
  minor_minor_false_positive_rates
)

# plots
n_states <- 30

# omit countries with less than 10 elections
combined_false_positive_rates <- combined_false_positive_rates %>%
  left_join(state_elections, by = "State_Name")

combined_false_positive_rates_filtered <- combined_false_positive_rates %>%
  filter(num_elections >= 10)

# create a dataset for the top contributing countries (largest positive deviation)
top_contributors <- combined_false_positive_rates_filtered %>%
  group_by(pair_type) %>%
  top_n(n_states, deviation_from_baseline) %>%  # select the top n by deviation
  ungroup()

# plot for top contributors using a horizontal bar chart
top_contributors <- top_contributors %>%
  mutate(State_faceted = fct_reorder(
    interaction(State_Name, pair_type),  # treat each country-pair_type combo as unique
    deviation_from_baseline
  ))

ggplot(top_contributors, aes(x = State_faceted, y = deviation_from_baseline)) +
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
  labs(title = "Top Contributing States by Pair Type",
       x = "State_Name",
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
  filename = "180425 meeting plots/Contributors_to_FP_IND_filtered_og.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# pivot the data to compare different pair types side by side
pivoted_false_positive_rates <- combined_false_positive_rates_filtered %>%
  dplyr::select(State_Name, pair_type, state_false_positive_rate, baseline_false_positive_rate, num_elections) %>%
  pivot_wider(
    id_cols = c(State_Name, num_elections),
    names_from = pair_type,
    values_from = c(state_false_positive_rate, baseline_false_positive_rate)
  )

# calculate differentials between pair types
differential_analysis <- pivoted_false_positive_rates %>%
  mutate(
    # calculate differentials between different pair types
    main_minor_vs_minor_minor = `state_false_positive_rate_main-minor/minor-main` - `state_false_positive_rate_minor-minor`,
    main_minor_vs_main_main = `state_false_positive_rate_main-minor/minor-main` - `state_false_positive_rate_main-main`,
    main_main_vs_minor_minor = `state_false_positive_rate_main-main` - `state_false_positive_rate_minor-minor`,
    baseline_main_minor_vs_minor_minor = `baseline_false_positive_rate_main-minor/minor-main` - `baseline_false_positive_rate_minor-minor`,
    baseline_main_minor_vs_main_main = `baseline_false_positive_rate_main-minor/minor-main` - `baseline_false_positive_rate_main-main`,
    baseline_main_main_vs_minor_minor = `baseline_false_positive_rate_main-main` - `baseline_false_positive_rate_minor-minor`
  ) %>%
  mutate(
    main_minor_vs_minor_minor_vs_main_main = (main_minor_vs_main_main + main_minor_vs_minor_minor)/2,
    baseline_main_minor_vs_minor_minor_vs_main_main = (baseline_main_minor_vs_main_main + baseline_main_minor_vs_minor_minor)/2
  )

# plot distribution of differentials
# select top states by absolute differential
top_differential <- differential_analysis %>%
  top_n(n_states, abs(main_minor_vs_minor_minor_vs_main_main))

top_differential_minor_minor <- differential_analysis %>%
  top_n(n_states, abs(main_minor_vs_minor_minor))

top_differential_main_main <- differential_analysis %>%
  top_n(n_states, abs(main_minor_vs_main_main))

# plot the differential
ggplot(top_differential, aes(x = reorder(State_Name, main_minor_vs_minor_minor_vs_main_main), y = main_minor_vs_minor_minor_vs_main_main)) +
  geom_hline(yintercept = top_differential$baseline_main_minor_vs_minor_minor_vs_main_main[1], 
             linetype = "dashed", color = "blue", size = 0.5) +
  geom_bar(stat = "identity", 
           aes(fill = main_minor_vs_minor_minor_vs_main_main > 0)) +
  geom_text(aes(label = round(main_minor_vs_minor_minor_vs_main_main, 2)),
            hjust = ifelse(top_differential$main_minor_vs_minor_minor_vs_main_main > 0, -0.1, 1.1),
            size = 3) +
  annotate("text", x = 1, y = top_differential$baseline_main_minor_vs_minor_minor_vs_main_main[1], 
           label = paste("Baseline Difference:", round(top_differential$baseline_main_minor_vs_minor_minor_vs_main_main[1], 2)), 
           hjust = 0, vjust = -0.5, color = "blue") +
  coord_flip() +
  labs(title = "Differential: Main-Minor vs Minor-Minor/Main-Main False Positive Rates",
       subtitle = "States with at least 10 elections",
       x = "State",
       y = "Differential (Main-Minor minus Avg. for Main-Main & Minor-Minor)",
       fill = "Main-Minor Higher") +
  scale_fill_manual(values = c("tomato", "chartreuse")) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Main_minor_vs_other_FP_states_IND_filtered_og.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

ggplot(top_differential_minor_minor, aes(x = reorder(State_Name, main_minor_vs_minor_minor), y = main_minor_vs_minor_minor)) +
  geom_hline(yintercept = top_differential$baseline_main_minor_vs_minor_minor[1], 
             linetype = "dashed", color = "blue", size = 0.5) +
  geom_bar(stat = "identity", 
           aes(fill = main_minor_vs_minor_minor > 0)) +
  geom_text(aes(label = round(main_minor_vs_minor_minor, 2)),
            hjust = ifelse(top_differential$main_minor_vs_minor_minor > 0, -0.1, 1.1),
            size = 3) +
  annotate("text", x = 1, y = top_differential$baseline_main_minor_vs_minor_minor[1], 
           label = paste("Baseline Difference:", round(top_differential$baseline_main_minor_vs_minor_minor[1], 2)), 
           hjust = 0, vjust = -0.5, color = "blue") +
  coord_flip() +
  labs(title = "Differential: Main-Minor vs Minor-Minor False Positive Rates",
       subtitle = "States with at least 10 elections",
       x = "State",
       y = "Differential (Main-Minor minus Minor-Minor)",
       fill = "Main-Minor Higher") +
  scale_fill_manual(values = c("tomato", "chartreuse")) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Main_minor_vs_minor_minor_FP_states_IND_filtered_og.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

ggplot(top_differential_main_main, aes(x = reorder(State_Name, main_minor_vs_main_main), y = main_minor_vs_main_main)) +
  geom_hline(yintercept = top_differential$baseline_main_minor_vs_main_main[1], 
             linetype = "dashed", color = "blue", size = 0.5) +
  geom_bar(stat = "identity", 
           aes(fill = main_minor_vs_main_main > 0)) +
  geom_text(aes(label = round(main_minor_vs_main_main, 2)),
            hjust = ifelse(top_differential$main_minor_vs_main_main > 0, -0.1, 1.1),
            size = 3) +
  coord_flip() +
  annotate("text", x = 1, y = top_differential$baseline_main_minor_vs_main_main[1], 
           label = paste("Baseline Difference:", round(top_differential$baseline_main_minor_vs_main_main[1], 2)), 
           hjust = 0, vjust = -0.5, color = "blue") +
  labs(title = "Differential: Main-Minor vs Main-Main False Positive Rates",
       subtitle = "States with at least 10 elections",
       x = "State",
       y = "Differential (Main-Minor minus Main-Main)",
       fill = "Main-Minor Higher") +
  scale_fill_manual(values = c("tomato", "chartreuse")) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = "180425 meeting plots/Main_minor_vs_main_main_FP_states_IND_filtered_og.png",
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
  dplyr::select(State_Name, main_minor_vs_main_main_diff, main_minor_vs_minor_minor_diff, main_minor_vs_minor_minor_vs_main_main_diff, 
                main_minor_vs_main_main, main_minor_vs_minor_minor, main_minor_vs_minor_minor_vs_main_main, 
                baseline_main_minor_vs_main_main, baseline_main_minor_vs_minor_minor, baseline_main_minor_vs_minor_minor_vs_main_main)

# reshape
long_differentials <- difference_of_differential_analysis %>%
  dplyr::select(State_Name,
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
n_states <- 30

top_contributors_diff <- long_differentials %>%
  group_by(differential_type) %>%
  top_n(n_states, abs(differential_value)) %>%  # Select top n by absolute deviation
  arrange(differential_type, desc(differential_value)) %>%
  ungroup()

# create a unique identifier for each country-differential_type combination
top_contributors_diff <- top_contributors_diff %>%
  mutate(States_faceted = fct_reorder(
    interaction(State_Name, differential_type),
    differential_value
  ))

# plot
ggplot(top_contributors_diff, aes(x = States_faceted, y = differential_value)) +
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
  labs(title = "Top Contributing States by Differential Type",
       x = "State",
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
  filename = "180425 meeting plots/Main_minor_other_FP_diff_states_IND_filtered_og.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

#### CLEA FP RATES - COUNTRY SPLITS ####
minor_candidates_that_are_decoys_of_main <- unique(c(
  candidate_pairs_CLEA_no_ind_main_minor %>% 
    filter(Pair_Type == "minor-main") %>% 
    pull(Candidate1_Election_ID),
  candidate_pairs_CLEA_no_ind_main_minor %>% 
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
main_minor_false_positive_rates <- calculate_country_false_positive_rates(candidate_pairs_CLEA_no_ind, "main-minor/minor-main")
main_main_false_positive_rates <- calculate_country_false_positive_rates(candidate_pairs_CLEA_no_ind, "main-main")
minor_minor_false_positive_rates <- calculate_country_false_positive_rates(candidate_pairs_CLEA_no_ind, "minor-minor")

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
  filename = "180425 meeting plots/Contributors_to_FP_CLEA_2.png",
  plot = p,
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
  filename = "180425 meeting plots/Main_minor_vs_other_FP_country_CLEA.png",
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
            hjust = ifelse(top_differential$main_minor_vs_minor_minor > 0, -0.1, 1.1),
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
  filename = "180425 meeting plots/Main_minor_vs_minor_minor_FP_country_CLEA.png",
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
  annotate("text", x = 1, y = top_differential$baseline_main_minor_vs_main_main[1], 
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
  filename = "180425 meeting plots/Main_minor_vs_main_main_FP_country_CLEA.png",
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

print(p)

ggsave(
  filename = "180425 meeting plots/Main_minor_other_FP_diff_country_CLEA.png",
  plot = p,
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

#### RANDOM SAMPLES ####
  # main minor tops: US, st lucia, maldives, jamaica
set.seed(999)

candidate_pairs_CLEA_main_minor_sample <- candidate_pairs_CLEA_no_ind_main_minor %>%
  filter(Country_Code == "462") %>%
  sample_n(min(15, nrow(.)), replace = FALSE) %>%
  dplyr::select(Year, Constituency_Name, Candidate1_Name, Candidate1_Party_Name, Candidate2_Name, Candidate2_Party_Name, Levenshtein_Similarity, Jaro_Winkler_Similarity, 
                NGram_Similarity, Masala_Similarity, Metaphone_Similarity)

candidate_pairs_CLEA_main_minor_sample %>%
  dplyr::select(Year, Constituency_Name, Candidate1_Name, Candidate1_Party_Name, Candidate2_Name, Candidate2_Party_Name, Levenshtein_Similarity, Jaro_Winkler_Similarity, 
                NGram_Similarity, Masala_Similarity, Metaphone_Similarity) %>%
  gt(groupname_col = "Pair_Type") %>%
  # 3dp
  fmt_number(
    columns = c(Levenshtein_Similarity, Jaro_Winkler_Similarity, 
                Metaphone_Similarity, Masala_Similarity, NGram_Similarity),
    decimals = 3
  ) %>%
  data_color(
    columns = Levenshtein_Similarity,
    fn = col_numeric(
      palette = c("pink", "yellow", "lightgreen"),
      domain = c(lv_99th, 1)
    )
  ) %>%
  data_color(
    columns = Jaro_Winkler_Similarity,
    fn = col_numeric(
      palette = c("pink", "yellow", "lightgreen"),
      domain = c(jw_99th, 1)
    )
  ) %>%
  data_color(
    columns = Metaphone_Similarity,
    fn = col_numeric(
      palette = c("pink", "yellow", "lightgreen"),
      domain = c(mp_99th, 1)
    )
  ) %>%
  data_color(
    columns = NGram_Similarity,
    fn = col_numeric(
      palette = c("pink", "yellow", "lightgreen"),
      domain = c(ng_99th, 1)
    )
  ) %>%
  data_color(
    columns = Masala_Similarity,
    fn = col_numeric(
      palette = c("pink", "yellow", "lightgreen"),
      domain = c(mas_99th, 1)
    )
  ) %>%
  tab_header(
    title = "High Similarity Candidate Pairs (Above 99th Percentile) Main-Minor",
  ) %>%
  cols_label(
    Year = "Year", 
    Candidate1_Name = "Name 1",
    Candidate2_Name = "Name 2",
    Levenshtein_Similarity = "Levenshtein",
    Jaro_Winkler_Similarity = "Jaro-Winkler",
    Metaphone_Similarity = "Metaphone", 
    Masala_Similarity = "Masala",
    NGram_Similarity = "NGram"
  )

#### COEFPLOTS WITH IND #### 
regressors <- c(
  "elevation_mean", "tri_mean", "dmsp_mean_light_cal",
  "vcf_mean", "land_area",
  "sc_m", "sc_f", "st_m", "st_f", "illit", 
  "ec13_emp_all", "ec13_count_all", "ec13_emp_manuf", "ec13_emp_services",
  "pc11_td_disp", "pc11_td_primary_gov", "pc11_td_el_dom", "Rural"
)

regressor_labels <- c(
  "elevation_mean" = "Mean Elevation",
  "tri_mean" = "Mean Ruggedness",
  "dmsp_mean_light_cal" = "Mean Night Lights (< 2011)",
  "land_area" = "Land Area",
  "vcf_mean" = "Vegetation Cover (2010)",
  "sc_m" = "SC Population (Male) % (2011) ",
  "sc_f" = "SC Population (Female) % (2011)",
  "st_m" = "ST Population (Male) % (2011)",
  "st_f" = "ST Population (Female) % (2011)",
  "illit" = "Illiterate Population % (2011)",
  "ec13_emp_all" = "Total Employment (2013)",
  "ec13_count_all" = "Total Firms (2013)",
  "ec13_emp_manuf" = "Manufacturing Employment (2013)",
  "ec13_emp_services" = "Services Employment (2013)",
  "pc11_td_disp" = "Dispensaries (2011)",
  "pc11_td_primary_gov" = "Primary Govt Schools (2011)",
  "pc11_td_el_dom" = "Hours of Power Supply during Summer (2011)", 
  "Rural" = "Rural Constituency Dummy Acc. shrid Coverage"
)

outcomes <- c("avg_decoy_candidates", "avg_decoy_share", "decoy_vote_share")  # Replace with your actual outcome variables
outcome_labels <- c(
  "avg_decoy_candidates" = "Average No. Decoys Per Election",
  "avg_decoy_share" = "Average % of Decoys Per Election ",
  "decoy_vote_share" = "Average Voteshare for Decoys"
)

# create standardized versions of outcome variables
for(outcome in outcomes) {
  constituency_metrics[[paste0("z_", outcome)]] <- scale(constituency_metrics[[outcome]])
}

# run separate regressions for each regressor and outcome
model_results <- list()

for(outcome in outcomes) {
  z_outcome <- paste0("z_", outcome)
  
  # run a separate regression for each regressor
  for(reg in regressors) {
    # create formula for individual regressor
    formula_str <- paste(z_outcome, "~", reg, "+ pre_delim")
    
    # Run the model
    model <- feols(
      as.formula(formula_str),
      data = constituency_metrics,
      cluster = "constituency_id"
    )
    
    # extract coefficient and standard error
    if(reg %in% names(coef(model))) {
      coef_val <- coef(model)[reg]
      se_val <- sqrt(vcov(model)[reg, reg])
      
      # store the results
      model_results[[paste0(outcome, "_", reg)]] <- data.frame(
        outcome = outcome,
        regressor = reg,
        estimate = coef_val,
        std.error = se_val,
        conf_low_90 = coef_val - 1.645 * se_val,
        conf_high_90 = coef_val + 1.645 * se_val,
        conf_low_95 = coef_val - 1.96 * se_val,
        conf_high_95 = coef_val + 1.96 * se_val,
        conf_low_99 = coef_val - 2.576 * se_val,
        conf_high_99 = coef_val + 2.576 * se_val
      )
    }
  }
}

# combine all results
plot_data <- do.call(rbind, model_results)

# add labels
plot_data$outcome_label <- outcome_labels[plot_data$outcome]
plot_data$regressor_label <- regressor_labels[plot_data$regressor]

# convert to factors for ordering
plot_data$outcome_label <- factor(plot_data$outcome_label,
                                  levels = outcome_labels,
                                  ordered = TRUE)
plot_data$regressor_label <- factor(plot_data$regressor_label,
                                    levels = rev(regressor_labels[regressors]),
                                    ordered = TRUE)

# show all outcomes on same chart
ggplot(plot_data, aes(y = regressor_label, x = estimate, color = outcome_label)) +
  # add reference line
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  
  # add CIs
  geom_linerange(aes(xmin = conf_low_95, xmax = conf_high_95),
                 linewidth = 1, alpha = 0.7,
                 position = position_dodge(width = 0.5)) +
  
  # add point estimates
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  
  # format
  theme_bw() +
  labs(x = "Standardized Coefficient (Standard Deviations)",
       y = NULL,
       color = "Outcome",
       shape = "Outcome",
       title = "Coeff Plot: Decoy Vars on Constituency Characteristics") +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 9),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Set1")

# group the regressors
regressor_groups <- list(
  "Geographic" = c("Mean Elevation", "Mean Ruggedness", "Vegetation Cover (2010)", "Land Area"),
  "Demographics" = c("SC Population (Male) % (2011) ", "SC Population (Female) % (2011)",
                     "ST Population (Male) % (2011)", "ST Population (Female) % (2011)",
                     "Illiterate Population % (2011)", "Rural Constituency Dummy Acc. shrid Coverage"),
  "Economy" = c("Total Employment (2013)", "Total Firms (2013)", 
                "Manufacturing Employment (2013)", "Services Employment (2013)"),
  "Infrastructure" = c("Dispensaries (2011)", "Primary Govt Schools (2011)", 
                       "Hours of Power Supply during Summer (2011)", "Mean Night Lights (< 2011)")
)

# add group column to plot_data
plot_data$group <- NA
for (group_name in names(regressor_groups)) {
  plot_data$group[plot_data$regressor_label %in% regressor_groups[[group_name]]] <- group_name
}

# convert group to factor with specific order
plot_data$group <- factor(plot_data$group, 
                          levels = names(regressor_groups))

# now plot with groups
ggplot(plot_data, aes(y = regressor_label, x = estimate, 
                      color = outcome_label)) +
  # add reference line
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  
  # add CIs for 95%
  geom_linerange(aes(xmin = conf_low_95, xmax = conf_high_95),
                 linewidth = 1, alpha = 0.7,
                 position = position_dodge(width = 0.7)) +
  
  # add point estimates
  geom_point(size = 2, position = position_dodge(width = 0.7)) +
  
  # grp by category
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  
  # format
  theme_bw() +
  labs(x = "Standardized Coefficient (Standard Deviations)",
       y = NULL,
       color = "Outcome",
       shape = "Outcome",
       title = "Coefficient Plot by Regressor Category") +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 10, r = 0, b = 0, l = 0),
    legend.box.margin = margin(t = 0, r = 0, b = 10, l = 0),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold")
  ) +
  guides(color = guide_legend(nrow = 3)) +  # force legend to use 2 rows
  scale_color_brewer(palette = "Set1")

ggsave(
  filename = "180425 meeting plots/Constituency_metrics_on_exogvars.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# for pre delim only
constituency_metrics_post <- constituency_metrics %>%
  filter(startsWith(constituency_id, "2008"))

constituency_metrics_pre <- constituency_metrics %>%
  filter(startsWith(constituency_id, "2007"))

# create standardized versions of outcome variables
for(outcome in outcomes) {
  constituency_metrics_pre[[paste0("z_", outcome)]] <- scale(constituency_metrics_pre[[outcome]])
}

# run separate regressions for each regressor and outcome
model_results <- list()

for(outcome in outcomes) {
  z_outcome <- paste0("z_", outcome)
  
  # run a separate regression for each regressor
  for(reg in regressors) {
    # create formula for individual regressor
    formula_str <- paste(z_outcome, "~", reg, "+ pre_delim")
    
    # Run the model
    model <- feols(
      as.formula(formula_str),
      data = constituency_metrics_pre,
      cluster = "constituency_id"
    )
    
    # extract coefficient and standard error
    if(reg %in% names(coef(model))) {
      coef_val <- coef(model)[reg]
      se_val <- sqrt(vcov(model)[reg, reg])
      
      # store the results
      model_results[[paste0(outcome, "_", reg)]] <- data.frame(
        outcome = outcome,
        regressor = reg,
        estimate = coef_val,
        std.error = se_val,
        conf_low_90 = coef_val - 1.645 * se_val,
        conf_high_90 = coef_val + 1.645 * se_val,
        conf_low_95 = coef_val - 1.96 * se_val,
        conf_high_95 = coef_val + 1.96 * se_val,
        conf_low_99 = coef_val - 2.576 * se_val,
        conf_high_99 = coef_val + 2.576 * se_val
      )
    }
  }
}

# combine all results
plot_data <- do.call(rbind, model_results)

# add labels
plot_data$outcome_label <- outcome_labels[plot_data$outcome]
plot_data$regressor_label <- regressor_labels[plot_data$regressor]

# convert to factors for ordering
plot_data$outcome_label <- factor(plot_data$outcome_label,
                                  levels = outcome_labels,
                                  ordered = TRUE)
plot_data$regressor_label <- factor(plot_data$regressor_label,
                                    levels = rev(regressor_labels[regressors]),
                                    ordered = TRUE)

# show all outcomes on same chart
ggplot(plot_data, aes(y = regressor_label, x = estimate, color = outcome_label)) +
  # add reference line
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  
  # add CIs
  geom_linerange(aes(xmin = conf_low_95, xmax = conf_high_95),
                 linewidth = 1, alpha = 0.7,
                 position = position_dodge(width = 0.5)) +
  
  # add point estimates
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  
  # format
  theme_bw() +
  labs(x = "Standardized Coefficient (Standard Deviations)",
       y = NULL,
       color = "Outcome",
       shape = "Outcome",
       title = "Coeff Plot: Decoy Vars on Constituency Characteristics") +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 9),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Set1")

# group the regressors
regressor_groups <- list(
  "Geographic" = c("Mean Elevation", "Mean Ruggedness", "Vegetation Cover (2010)", "Land Area"),
  "Demographics" = c("SC Population (Male) % (2011) ", "SC Population (Female) % (2011)",
                     "ST Population (Male) % (2011)", "ST Population (Female) % (2011)",
                     "Illiterate Population % (2011)", "Rural Constituency Dummy Acc. shrid Coverage"),
  "Economy" = c("Total Employment (2013)", "Total Firms (2013)", 
                "Manufacturing Employment (2013)", "Services Employment (2013)"),
  "Infrastructure" = c("Dispensaries (2011)", "Primary Govt Schools (2011)", 
                       "Hours of Power Supply during Summer (2011)", "Mean Night Lights (< 2011)")
)

# add group column to plot_data
plot_data$group <- NA
for (group_name in names(regressor_groups)) {
  plot_data$group[plot_data$regressor_label %in% regressor_groups[[group_name]]] <- group_name
}

# convert group to factor with specific order
plot_data$group <- factor(plot_data$group, 
                          levels = names(regressor_groups))

# now plot with groups
ggplot(plot_data, aes(y = regressor_label, x = estimate, 
                      color = outcome_label)) +
  # add reference line
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  
  # add CIs for 95%
  geom_linerange(aes(xmin = conf_low_95, xmax = conf_high_95),
                 linewidth = 1, alpha = 0.7,
                 position = position_dodge(width = 0.7)) +
  
  # add point estimates
  geom_point(size = 2, position = position_dodge(width = 0.7)) +
  
  # grp by category
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  
  # format
  theme_bw() +
  labs(x = "Standardized Coefficient (Standard Deviations)",
       y = NULL,
       color = "Outcome",
       shape = "Outcome",
       title = "Coefficient Plot by Regressor Category") +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 10, r = 0, b = 0, l = 0),
    legend.box.margin = margin(t = 0, r = 0, b = 10, l = 0),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold")
  ) +
  guides(color = guide_legend(nrow = 3)) +  # force legend to use 2 rows
  scale_color_brewer(palette = "Set1")

ggsave(
  filename = "180425 meeting plots/constituency_metrics_pre_delim_on_exogvars.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# for post delim only
# create standardized versions of outcome variables
for(outcome in outcomes) {
  constituency_metrics_post[[paste0("z_", outcome)]] <- scale(constituency_metrics_post[[outcome]])
}

# run separate regressions for each regressor and outcome
model_results <- list()

for(outcome in outcomes) {
  z_outcome <- paste0("z_", outcome)
  
  # run a separate regression for each regressor
  for(reg in regressors) {
    # create formula for individual regressor
    formula_str <- paste(z_outcome, "~", reg, "+ pre_delim")
    
    # Run the model
    model <- feols(
      as.formula(formula_str),
      data = constituency_metrics_post,
      cluster = "constituency_id"
    )
    
    # extract coefficient and standard error
    if(reg %in% names(coef(model))) {
      coef_val <- coef(model)[reg]
      se_val <- sqrt(vcov(model)[reg, reg])
      
      # store the results
      model_results[[paste0(outcome, "_", reg)]] <- data.frame(
        outcome = outcome,
        regressor = reg,
        estimate = coef_val,
        std.error = se_val,
        conf_low_90 = coef_val - 1.645 * se_val,
        conf_high_90 = coef_val + 1.645 * se_val,
        conf_low_95 = coef_val - 1.96 * se_val,
        conf_high_95 = coef_val + 1.96 * se_val,
        conf_low_99 = coef_val - 2.576 * se_val,
        conf_high_99 = coef_val + 2.576 * se_val
      )
    }
  }
}

# combine all results
plot_data <- do.call(rbind, model_results)

# add labels
plot_data$outcome_label <- outcome_labels[plot_data$outcome]
plot_data$regressor_label <- regressor_labels[plot_data$regressor]

# convert to factors for ordering
plot_data$outcome_label <- factor(plot_data$outcome_label,
                                  levels = outcome_labels,
                                  ordered = TRUE)
plot_data$regressor_label <- factor(plot_data$regressor_label,
                                    levels = rev(regressor_labels[regressors]),
                                    ordered = TRUE)

# show all outcomes on same chart
ggplot(plot_data, aes(y = regressor_label, x = estimate, color = outcome_label)) +
  # add reference line
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  
  # add CIs
  geom_linerange(aes(xmin = conf_low_95, xmax = conf_high_95),
                 linewidth = 1, alpha = 0.7,
                 position = position_dodge(width = 0.5)) +
  
  # add point estimates
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  
  # format
  theme_bw() +
  labs(x = "Standardized Coefficient (Standard Deviations)",
       y = NULL,
       color = "Outcome",
       shape = "Outcome",
       title = "Coeff Plot: Decoy Vars on Constituency Characteristics") +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 9),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Set1")

# group the regressors
regressor_groups <- list(
  "Geographic" = c("Mean Elevation", "Mean Ruggedness", "Vegetation Cover (2010)", "Land Area"),
  "Demographics" = c("SC Population (Male) % (2011) ", "SC Population (Female) % (2011)",
                     "ST Population (Male) % (2011)", "ST Population (Female) % (2011)",
                     "Illiterate Population % (2011)", "Rural Constituency Dummy Acc. shrid Coverage"),
  "Economy" = c("Total Employment (2013)", "Total Firms (2013)", 
                "Manufacturing Employment (2013)", "Services Employment (2013)"),
  "Infrastructure" = c("Dispensaries (2011)", "Primary Govt Schools (2011)", 
                       "Hours of Power Supply during Summer (2011)", "Mean Night Lights (< 2011)")
)

# add group column to plot_data
plot_data$group <- NA
for (group_name in names(regressor_groups)) {
  plot_data$group[plot_data$regressor_label %in% regressor_groups[[group_name]]] <- group_name
}

# convert group to factor with specific order
plot_data$group <- factor(plot_data$group, 
                          levels = names(regressor_groups))

# now plot with groups
ggplot(plot_data, aes(y = regressor_label, x = estimate, 
                      color = outcome_label)) +
  # add reference line
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  
  # add CIs for 95%
  geom_linerange(aes(xmin = conf_low_95, xmax = conf_high_95),
                 linewidth = 1, alpha = 0.7,
                 position = position_dodge(width = 0.7)) +
  
  # add point estimates
  geom_point(size = 2, position = position_dodge(width = 0.7)) +
  
  # grp by category
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  
  # format
  theme_bw() +
  labs(x = "Standardized Coefficient (Standard Deviations)",
       y = NULL,
       color = "Outcome",
       shape = "Outcome",
       title = "Coefficient Plot by Regressor Category") +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 10, r = 0, b = 0, l = 0),
    legend.box.margin = margin(t = 0, r = 0, b = 10, l = 0),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold")
  ) +
  guides(color = guide_legend(nrow = 3)) +  # force legend to use 2 rows
  scale_color_brewer(palette = "Set1")

ggsave(
  filename = "180425 meeting plots/constituency_metrics_post_delim_on_exogvars.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# for the filtered constituency_metrics
for(outcome in outcomes) {
  constituency_metrics_filtered[[paste0("z_", outcome)]] <- scale(constituency_metrics_filtered[[outcome]])
}

# run separate regressions for each regressor and outcome
model_results <- list()

for(outcome in outcomes) {
  z_outcome <- paste0("z_", outcome)
  
  # run a separate regression for each regressor
  for(reg in regressors) {
    # create formula for individual regressor
    formula_str <- paste(z_outcome, "~", reg, "+ pre_delim")
    
    # Run the model
    model <- feols(
      as.formula(formula_str),
      data = constituency_metrics_filtered,
      cluster = "constituency_id"
    )
    
    # extract coefficient and standard error
    if(reg %in% names(coef(model))) {
      coef_val <- coef(model)[reg]
      se_val <- sqrt(vcov(model)[reg, reg])
      
      # store the results
      model_results[[paste0(outcome, "_", reg)]] <- data.frame(
        outcome = outcome,
        regressor = reg,
        estimate = coef_val,
        std.error = se_val,
        conf_low_90 = coef_val - 1.645 * se_val,
        conf_high_90 = coef_val + 1.645 * se_val,
        conf_low_95 = coef_val - 1.96 * se_val,
        conf_high_95 = coef_val + 1.96 * se_val,
        conf_low_99 = coef_val - 2.576 * se_val,
        conf_high_99 = coef_val + 2.576 * se_val
      )
    }
  }
}

# combine all results
plot_data <- do.call(rbind, model_results)

# add labels
plot_data$outcome_label <- outcome_labels[plot_data$outcome]
plot_data$regressor_label <- regressor_labels[plot_data$regressor]

# convert to factors for ordering
plot_data$outcome_label <- factor(plot_data$outcome_label,
                                  levels = outcome_labels,
                                  ordered = TRUE)
plot_data$regressor_label <- factor(plot_data$regressor_label,
                                    levels = rev(regressor_labels[regressors]),
                                    ordered = TRUE)

# show all outcomes on same chart
ggplot(plot_data, aes(y = regressor_label, x = estimate, color = outcome_label)) +
  # add reference line
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  
  # add CIs
  geom_linerange(aes(xmin = conf_low_95, xmax = conf_high_95),
                 linewidth = 1, alpha = 0.7,
                 position = position_dodge(width = 0.5)) +
  
  # add point estimates
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  
  # format
  theme_bw() +
  labs(x = "Standardized Coefficient (Standard Deviations)",
       y = NULL,
       color = "Outcome",
       shape = "Outcome",
       title = "Coeff Plot: Decoy Vars on Constituency Characteristics") +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 9),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Set1")

# group the regressors
regressor_groups <- list(
  "Geographic" = c("Mean Elevation", "Mean Ruggedness", "Vegetation Cover (2010)", "Land Area"),
  "Demographics" = c("SC Population (Male) % (2011) ", "SC Population (Female) % (2011)",
                     "ST Population (Male) % (2011)", "ST Population (Female) % (2011)",
                     "Illiterate Population % (2011)", "Rural Constituency Dummy Acc. shrid Coverage"),
  "Economy" = c("Total Employment (2013)", "Total Firms (2013)", 
                "Manufacturing Employment (2013)", "Services Employment (2013)"),
  "Infrastructure" = c("Dispensaries (2011)", "Primary Govt Schools (2011)", 
                       "Hours of Power Supply during Summer (2011)", "Mean Night Lights (< 2011)")
)

# add group column to plot_data
plot_data$group <- NA
for (group_name in names(regressor_groups)) {
  plot_data$group[plot_data$regressor_label %in% regressor_groups[[group_name]]] <- group_name
}

# convert group to factor with specific order
plot_data$group <- factor(plot_data$group, 
                          levels = names(regressor_groups))

# now plot with groups
ggplot(plot_data, aes(y = regressor_label, x = estimate, 
                      color = outcome_label)) +
  # add reference line
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  
  # add CIs for 95%
  geom_linerange(aes(xmin = conf_low_95, xmax = conf_high_95),
                 linewidth = 1, alpha = 0.7,
                 position = position_dodge(width = 0.7)) +
  
  # add point estimates
  geom_point(size = 2, position = position_dodge(width = 0.7)) +
  
  # grp by category
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  
  # format
  theme_bw() +
  labs(x = "Standardized Coefficient (Standard Deviations)",
       y = NULL,
       color = "Outcome",
       shape = "Outcome",
       title = "Coefficient Plot by Regressor Category") +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 10, r = 0, b = 0, l = 0),
    legend.box.margin = margin(t = 0, r = 0, b = 10, l = 0),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold")
  ) +
  guides(color = guide_legend(nrow = 3)) +  # force legend to use 2 rows
  scale_color_brewer(palette = "Set1")

ggsave(
  filename = "180425 meeting plots/constituency_metrics_filtered_on_exogvars.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# for 74 to 94
str(constituency_metrics_74_94)

sum(is.na(constituency_metrics_74_94$lit))

regressors <- c(
  "elevation_mean", "tri_mean", "dmsp_mean_light_cal", "land_area",
  "sc_m", "sc_f", "st_m", "st_f", "lit", 
  "ec90_emp_all", "ec90_count_all", "ec90_emp_manuf", "ec90_emp_services",
  "pc91_td_health_ctr", "pc91_td_primary", "pc91_td_el_dom", "Rural"
)

regressor_labels <- c(
  "elevation_mean" = "Mean Elevation",
  "tri_mean" = "Mean Ruggedness",
  "dmsp_mean_light_cal" = "Mean Night Lights",
  "land_area" = "Land Area",
  "sc_m" = "SC Population (Male) % (1991) ",
  "sc_f" = "SC Population (Female) % (1991)",
  "st_m" = "ST Population (Male) % (1991)",
  "st_f" = "ST Population (Female) % (1991)",
  "lit" = "Literate Population % (1991)",
  "ec90_emp_all" = "Total Employment (1990)",
  "ec90_count_all" = "Total Firms (1990)",
  "ec90_emp_manuf" = "Manufacturing Employment (1990)",
  "ec90_emp_services" = "Services Employment (1990)",
  "pc91_td_health_ctr" = "Dispensaries (1991)",
  "pc91_td_primary" = "Primary Govt Schools (1991)",
  "pc91_td_el_dom" = "Hours of Power Supply during Summer (1991)", 
  "Rural" = "Rural Constituency Dummy Acc. shrid Coverage"
)

outcomes <- c("avg_decoy_candidates", "avg_decoy_share", "decoy_vote_share")  # Replace with your actual outcome variables
outcome_labels <- c(
  "avg_decoy_candidates" = "Average No. Decoys Per Election",
  "avg_decoy_share" = "Average % of Decoys Per Election ",
  "decoy_vote_share" = "Average Voteshare for Decoys"
)

# create standardized versions of outcome variables
for(outcome in outcomes) {
  constituency_metrics_74_94[[paste0("z_", outcome)]] <- scale(constituency_metrics_74_94[[outcome]])
}

# run separate regressions for each regressor and outcome
model_results <- list()

for(outcome in outcomes) {
  z_outcome <- paste0("z_", outcome)
  
  # run a separate regression for each regressor
  for(reg in regressors) {
    # create formula for individual regressor
    formula_str <- paste(z_outcome, "~", reg)
    
    # Run the model
    model <- feols(
      as.formula(formula_str),
      data = constituency_metrics_74_94,
      cluster = "constituency_id"
    )
    
    # extract coefficient and standard error
    if(reg %in% names(coef(model))) {
      coef_val <- coef(model)[reg]
      se_val <- sqrt(vcov(model)[reg, reg])
      
      # store the results
      model_results[[paste0(outcome, "_", reg)]] <- data.frame(
        outcome = outcome,
        regressor = reg,
        estimate = coef_val,
        std.error = se_val,
        conf_low_90 = coef_val - 1.645 * se_val,
        conf_high_90 = coef_val + 1.645 * se_val,
        conf_low_95 = coef_val - 1.96 * se_val,
        conf_high_95 = coef_val + 1.96 * se_val,
        conf_low_99 = coef_val - 2.576 * se_val,
        conf_high_99 = coef_val + 2.576 * se_val
      )
    }
  }
}

# combine all results
plot_data <- do.call(rbind, model_results)

# add labels
plot_data$outcome_label <- outcome_labels[plot_data$outcome]
plot_data$regressor_label <- regressor_labels[plot_data$regressor]

# convert to factors for ordering
plot_data$outcome_label <- factor(plot_data$outcome_label,
                                  levels = outcome_labels,
                                  ordered = TRUE)
plot_data$regressor_label <- factor(plot_data$regressor_label,
                                    levels = rev(regressor_labels[regressors]),
                                    ordered = TRUE)

# show all outcomes on same chart
ggplot(plot_data, aes(y = regressor_label, x = estimate, color = outcome_label)) +
  # add reference line
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  
  # add CIs
  geom_linerange(aes(xmin = conf_low_95, xmax = conf_high_95),
                 linewidth = 1, alpha = 0.7,
                 position = position_dodge(width = 0.5)) +
  
  # add point estimates
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  
  # format
  theme_bw() +
  labs(x = "Standardized Coefficient (Standard Deviations)",
       y = NULL,
       color = "Outcome",
       shape = "Outcome",
       title = "Coeff Plot: Decoy Vars on Constituency Characteristics") +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 9),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Set1")

# group the regressors
regressor_groups <- list(
  "Geographic" = c("Mean Elevation", "Mean Ruggedness", "Land Area"),
  "Demographics" = c("SC Population (Male) % (1991) ", "SC Population (Female) % (1991)",
                     "ST Population (Male) % (1991)", "ST Population (Female) % (1991)",
                     "Literate Population % (1991)", "Rural Constituency Dummy Acc. shrid Coverage"),
  "Economy" = c("Total Employment (1990)", "Total Firms (1990)", 
                "Manufacturing Employment (1990)", "Services Employment (1990)"),
  "Infrastructure" = c("Dispensaries (1991)", "Primary Govt Schools (1991)", 
                       "Hours of Power Supply during Summer (1991)", "Mean Night Lights")
)

# add group column to plot_data
plot_data$group <- NA
for (group_name in names(regressor_groups)) {
  plot_data$group[plot_data$regressor_label %in% regressor_groups[[group_name]]] <- group_name
}

# convert group to factor with specific order
plot_data$group <- factor(plot_data$group, 
                          levels = names(regressor_groups))

# now plot with groups
ggplot(plot_data, aes(y = regressor_label, x = estimate, 
                      color = outcome_label)) +
  # add reference line
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  
  # add CIs for 95%
  geom_linerange(aes(xmin = conf_low_95, xmax = conf_high_95),
                 linewidth = 1, alpha = 0.7,
                 position = position_dodge(width = 0.7)) +
  
  # add point estimates
  geom_point(size = 2, position = position_dodge(width = 0.7)) +
  
  # grp by category
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  
  # format
  theme_bw() +
  labs(x = "Standardized Coefficient (Standard Deviations)",
       y = NULL,
       color = "Outcome",
       shape = "Outcome",
       title = "Coefficient Plot by Regressor Category") +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 10, r = 0, b = 0, l = 0),
    legend.box.margin = margin(t = 0, r = 0, b = 10, l = 0),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold")
  ) +
  guides(color = guide_legend(nrow = 3)) +  # force legend to use 2 rows
  scale_color_brewer(palette = "Set1")

ggsave(
  filename = "180425 meeting plots/Constituency_metrics_74_94_on_exogvars.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# for 95 to 04
str(constituency_metrics_95_04)

sum(is.na(constituency_metrics_95_04$illit))

regressors <- c(
  "elevation_mean", "tri_mean", "dmsp_mean_light_cal", "land_area",
  "sc_m", "sc_f", "st_m", "st_f", "illit", 
  "ec98_emp_all", "ec98_count_all", "ec98_emp_manuf", "ec98_emp_services",
  "pc01_td_health_ctr", "pc01_td_primary", "pc01_td_el_dom", "Rural"
)

regressor_labels <- c(
  "elevation_mean" = "Mean Elevation",
  "tri_mean" = "Mean Ruggedness",
  "dmsp_mean_light_cal" = "Mean Night Lights",
  "land_area" = "Land Area",
  "sc_m" = "SC Population (Male) % (2001) ",
  "sc_f" = "SC Population (Female) % (2001)",
  "st_m" = "ST Population (Male) % (2001)",
  "st_f" = "ST Population (Female) % (2001)",
  "illit" = "Illiterate Population % (2001)",
  "ec98_emp_all" = "Total Employment (1998)",
  "ec98_count_all" = "Total Firms (1998)",
  "ec98_emp_manuf" = "Manufacturing Employment (1998)",
  "ec98_emp_services" = "Services Employment (1998)",
  "pc01_td_health_ctr" = "Dispensaries (2001)",
  "pc01_td_primary" = "Primary Govt Schools (2001)",
  "pc01_td_el_dom" = "Hours of Power Supply during Summer (2001)", 
  "Rural" = "Rural Constituency Dummy Acc. shrid Coverage"
)

outcomes <- c("avg_decoy_candidates", "avg_decoy_share", "decoy_vote_share")  # Replace with your actual outcome variables
outcome_labels <- c(
  "avg_decoy_candidates" = "Average No. Decoys Per Election",
  "avg_decoy_share" = "Average % of Decoys Per Election ",
  "decoy_vote_share" = "Average Voteshare for Decoys"
)

# create standardized versions of outcome variables
for(outcome in outcomes) {
  constituency_metrics_95_04[[paste0("z_", outcome)]] <- scale(constituency_metrics_95_04[[outcome]])
}

# run separate regressions for each regressor and outcome
model_results <- list()

for(outcome in outcomes) {
  z_outcome <- paste0("z_", outcome)
  
  # run a separate regression for each regressor
  for(reg in regressors) {
    # create formula for individual regressor
    formula_str <- paste(z_outcome, "~", reg)
    
    # Run the model
    model <- feols(
      as.formula(formula_str),
      data = constituency_metrics_95_04,
      cluster = "constituency_id"
    )
    
    # extract coefficient and standard error
    if(reg %in% names(coef(model))) {
      coef_val <- coef(model)[reg]
      se_val <- sqrt(vcov(model)[reg, reg])
      
      # store the results
      model_results[[paste0(outcome, "_", reg)]] <- data.frame(
        outcome = outcome,
        regressor = reg,
        estimate = coef_val,
        std.error = se_val,
        conf_low_90 = coef_val - 1.645 * se_val,
        conf_high_90 = coef_val + 1.645 * se_val,
        conf_low_95 = coef_val - 1.96 * se_val,
        conf_high_95 = coef_val + 1.96 * se_val,
        conf_low_99 = coef_val - 2.576 * se_val,
        conf_high_99 = coef_val + 2.576 * se_val
      )
    }
  }
}

# combine all results
plot_data <- do.call(rbind, model_results)

# add labels
plot_data$outcome_label <- outcome_labels[plot_data$outcome]
plot_data$regressor_label <- regressor_labels[plot_data$regressor]

# convert to factors for ordering
plot_data$outcome_label <- factor(plot_data$outcome_label,
                                  levels = outcome_labels,
                                  ordered = TRUE)
plot_data$regressor_label <- factor(plot_data$regressor_label,
                                    levels = rev(regressor_labels[regressors]),
                                    ordered = TRUE)

# show all outcomes on same chart
ggplot(plot_data, aes(y = regressor_label, x = estimate, color = outcome_label)) +
  # add reference line
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  
  # add CIs
  geom_linerange(aes(xmin = conf_low_95, xmax = conf_high_95),
                 linewidth = 1, alpha = 0.7,
                 position = position_dodge(width = 0.5)) +
  
  # add point estimates
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  
  # format
  theme_bw() +
  labs(x = "Standardized Coefficient (Standard Deviations)",
       y = NULL,
       color = "Outcome",
       shape = "Outcome",
       title = "Coeff Plot: Decoy Vars on Constituency Characteristics") +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 9),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Set1")

# group the regressors
regressor_groups <- list(
  "Geographic" = c("Mean Elevation", "Mean Ruggedness", "Land Area"),
  "Demographics" = c("SC Population (Male) % (2001) ", "SC Population (Female) % (2001)",
                     "ST Population (Male) % (2001)", "ST Population (Female) % (2001)",
                     "Illiterate Population % (2001)", "Rural Constituency Dummy Acc. shrid Coverage"),
  "Economy" = c("Total Employment (1998)", "Total Firms (1998)", 
                "Manufacturing Employment (1998)", "Services Employment (1998)"),
  "Infrastructure" = c("Dispensaries (2001)", "Primary Govt Schools (2001)", 
                       "Hours of Power Supply during Summer (2001)", "Mean Night Lights")
)

# add group column to plot_data
plot_data$group <- NA
for (group_name in names(regressor_groups)) {
  plot_data$group[plot_data$regressor_label %in% regressor_groups[[group_name]]] <- group_name
}

# convert group to factor with specific order
plot_data$group <- factor(plot_data$group, 
                          levels = names(regressor_groups))

# now plot with groups
ggplot(plot_data, aes(y = regressor_label, x = estimate, 
                      color = outcome_label)) +
  # add reference line
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  
  # add CIs for 95%
  geom_linerange(aes(xmin = conf_low_95, xmax = conf_high_95),
                 linewidth = 1, alpha = 0.7,
                 position = position_dodge(width = 0.7)) +
  
  # add point estimates
  geom_point(size = 2, position = position_dodge(width = 0.7)) +
  
  # grp by category
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  
  # format
  theme_bw() +
  labs(x = "Standardized Coefficient (Standard Deviations)",
       y = NULL,
       color = "Outcome",
       shape = "Outcome",
       title = "Coefficient Plot by Regressor Category") +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 10, r = 0, b = 0, l = 0),
    legend.box.margin = margin(t = 0, r = 0, b = 10, l = 0),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold")
  ) +
  guides(color = guide_legend(nrow = 3)) +  # force legend to use 2 rows
  scale_color_brewer(palette = "Set1")

ggsave(
  filename = "180425 meeting plots/Constituency_metrics_95_04_on_exogvars.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# for 05 to 23
str(constituency_metrics_05_23)

sum(is.na(constituency_metrics_05_23$illit))

regressors <- c(
  "elevation_mean", "tri_mean", "dmsp_mean_light_cal", "land_area",
  "sc_m", "sc_f", "st_m", "st_f", "illit", 
  "ec13_emp_all", "ec13_count_all", "ec13_emp_manuf", "ec13_emp_services",
  "pc11_td_disp", "pc11_td_primary_gov", "pc11_td_el_dom", "Rural"
)

regressor_labels <- c(
  "elevation_mean" = "Mean Elevation",
  "tri_mean" = "Mean Ruggedness",
  "dmsp_mean_light_cal" = "Mean Night Lights",
  "land_area" = "Land Area",
  "sc_m" = "SC Population (Male) % (2011) ",
  "sc_f" = "SC Population (Female) % (2011)",
  "st_m" = "ST Population (Male) % (2011)",
  "st_f" = "ST Population (Female) % (2011)",
  "illit" = "Illiterate Population % (2011)",
  "ec13_emp_all" = "Total Employment (2013)",
  "ec13_count_all" = "Total Firms (2013)",
  "ec13_emp_manuf" = "Manufacturing Employment (2013)",
  "ec13_emp_services" = "Services Employment (2013)",
  "pc11_td_disp" = "Dispensaries (2011)",
  "pc11_td_primary_gov" = "Primary Govt Schools (2011)",
  "pc11_td_el_dom" = "Hours of Power Supply during Summer (2011)", 
  "Rural" = "Rural Constituency Dummy Acc. shrid Coverage"
)

outcomes <- c("avg_decoy_candidates", "avg_decoy_share", "decoy_vote_share")  # Replace with your actual outcome variables
outcome_labels <- c(
  "avg_decoy_candidates" = "Average No. Decoys Per Election",
  "avg_decoy_share" = "Average % of Decoys Per Election ",
  "decoy_vote_share" = "Average Voteshare for Decoys"
)

# create standardized versions of outcome variables
for(outcome in outcomes) {
  constituency_metrics_05_23[[paste0("z_", outcome)]] <- scale(constituency_metrics_05_23[[outcome]])
}

# run separate regressions for each regressor and outcome
model_results <- list()

for(outcome in outcomes) {
  z_outcome <- paste0("z_", outcome)
  
  # run a separate regression for each regressor
  for(reg in regressors) {
    # create formula for individual regressor
    formula_str <- paste(z_outcome, "~", reg)
    
    # Run the model
    model <- feols(
      as.formula(formula_str),
      data = constituency_metrics_05_23,
      cluster = "constituency_id"
    )
    
    # extract coefficient and standard error
    if(reg %in% names(coef(model))) {
      coef_val <- coef(model)[reg]
      se_val <- sqrt(vcov(model)[reg, reg])
      
      # store the results
      model_results[[paste0(outcome, "_", reg)]] <- data.frame(
        outcome = outcome,
        regressor = reg,
        estimate = coef_val,
        std.error = se_val,
        conf_low_90 = coef_val - 1.645 * se_val,
        conf_high_90 = coef_val + 1.645 * se_val,
        conf_low_95 = coef_val - 1.96 * se_val,
        conf_high_95 = coef_val + 1.96 * se_val,
        conf_low_99 = coef_val - 2.576 * se_val,
        conf_high_99 = coef_val + 2.576 * se_val
      )
    }
  }
}

# combine all results
plot_data <- do.call(rbind, model_results)

# add labels
plot_data$outcome_label <- outcome_labels[plot_data$outcome]
plot_data$regressor_label <- regressor_labels[plot_data$regressor]

# convert to factors for ordering
plot_data$outcome_label <- factor(plot_data$outcome_label,
                                  levels = outcome_labels,
                                  ordered = TRUE)
plot_data$regressor_label <- factor(plot_data$regressor_label,
                                    levels = rev(regressor_labels[regressors]),
                                    ordered = TRUE)

# show all outcomes on same chart
ggplot(plot_data, aes(y = regressor_label, x = estimate, color = outcome_label)) +
  # add reference line
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  
  # add CIs
  geom_linerange(aes(xmin = conf_low_95, xmax = conf_high_95),
                 linewidth = 1, alpha = 0.7,
                 position = position_dodge(width = 0.5)) +
  
  # add point estimates
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  
  # format
  theme_bw() +
  labs(x = "Standardized Coefficient (Standard Deviations)",
       y = NULL,
       color = "Outcome",
       shape = "Outcome",
       title = "Coeff Plot: Decoy Vars on Constituency Characteristics") +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 9),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Set1")

# group the regressors
regressor_groups <- list(
  "Geographic" = c("Mean Elevation", "Mean Ruggedness", "Land Area"),
  "Demographics" = c("SC Population (Male) % (2011) ", "SC Population (Female) % (2011)",
                     "ST Population (Male) % (2011)", "ST Population (Female) % (2011)",
                     "Illiterate Population % (2011)", "Rural Constituency Dummy Acc. shrid Coverage"),
  "Economy" = c("Total Employment (2013)", "Total Firms (2013)", 
                "Manufacturing Employment (2013)", "Services Employment (2013)"),
  "Infrastructure" = c("Dispensaries (2011)", "Primary Govt Schools (2011)", 
                       "Hours of Power Supply during Summer (2011)", "Mean Night Lights")
)

# add group column to plot_data
plot_data$group <- NA
for (group_name in names(regressor_groups)) {
  plot_data$group[plot_data$regressor_label %in% regressor_groups[[group_name]]] <- group_name
}

# convert group to factor with specific order
plot_data$group <- factor(plot_data$group, 
                          levels = names(regressor_groups))

# now plot with groups
ggplot(plot_data, aes(y = regressor_label, x = estimate, 
                      color = outcome_label)) +
  # add reference line
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  
  # add CIs for 95%
  geom_linerange(aes(xmin = conf_low_95, xmax = conf_high_95),
                 linewidth = 1, alpha = 0.7,
                 position = position_dodge(width = 0.7)) +
  
  # add point estimates
  geom_point(size = 2, position = position_dodge(width = 0.7)) +
  
  # grp by category
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  
  # format
  theme_bw() +
  labs(x = "Standardized Coefficient (Standard Deviations)",
       y = NULL,
       color = "Outcome",
       shape = "Outcome",
       title = "Coefficient Plot by Regressor Category") +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 10, r = 0, b = 0, l = 0),
    legend.box.margin = margin(t = 0, r = 0, b = 10, l = 0),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold")
  ) +
  guides(color = guide_legend(nrow = 3)) +  # force legend to use 2 rows
  scale_color_brewer(palette = "Set1")

ggsave(
  filename = "180425 meeting plots/Constituency_metrics_05_23_on_exogvars.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)
