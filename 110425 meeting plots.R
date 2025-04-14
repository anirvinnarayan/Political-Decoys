# ============================================
# Political Decoys - 110425 meeting plots
# ============================================
# Date: 07/04/25
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
  purr
)

### Loading
all_states_elections <- read.csv("Raw Data/all_states_elections.csv")
CLEA_cleaned <- read.csv("Cleaned Data/CLEA_cleaned.csv")
candidate_pairs <- read.csv("Cleaned Data/candidate_pairs_lv_jw_ngram_masala_dblmet.csv")
candidate_pairs_CLEA <- read.csv("Cleaned Data/candidate_pairs_lv_jw_ngram_masala_dblmet_CLEA.csv")

### Random testing
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

### editing candidate_pairs 
str(candidate_pairs)

filter_out_initial_names <- function(df) {
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

results <- filter_out_initial_names(candidate_pairs)
candidate_pairs <- results$kept
candidate_pairs_out <- results$filtered_out

test <- candidate_pairs_out %>%
  filter(State_Name == "Punjab" & Year == 1967)

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

# sids edit
str(candidate_pairs)
sum(is.na(candidate_pairs$Candidate1_Party_Type_TCPD))
unique(candidate_pairs$Candidate1_Party_Type_TCPD)

candidate_pairs_new_minor <- candidate_pairs %>%
  mutate(
    # save original values to compare later
    Original_Pair_Type = Pair_Type,
    Original_Candidate1_Type = Candidate1_Type,
    Original_Candidate2_Type = Candidate2_Type
  ) %>%
  
  mutate(
    # Rule 1: main-minor pairs with Candidate2 from National Party
    Candidate2_Type = case_when(
      Pair_Type == "main-minor" & Candidate2_Party_Type_TCPD == "National Party" ~ "main",
      TRUE ~ Candidate2_Type
    ),
    
    # Rule 2: minor-main pairs with Candidate1 from National Party
    Candidate1_Type = case_when(
      Pair_Type == "minor-main" & Candidate1_Party_Type_TCPD == "National Party" ~ "main",
      TRUE ~ Candidate1_Type
    ),
    
    # Rule 3a: minor-minor pairs with Candidate1 from National Party
    Candidate1_Type = case_when(
      Pair_Type == "minor-minor" & Candidate1_Party_Type_TCPD == "National Party" ~ "main",
      TRUE ~ Candidate1_Type
    ),
    
    # Rule 3b: minor-minor pairs with Candidate2 from National Party
    Candidate2_Type = case_when(
      Pair_Type == "minor-minor" & Candidate2_Party_Type_TCPD == "National Party" ~ "main",
      TRUE ~ Candidate2_Type
    ),
    
    # update Pair_Type
    Pair_Type = case_when(
      # main-minor with Candidate2 as National Party
      Pair_Type == "main-minor" & Candidate2_Party_Type_TCPD == "National Party" ~ "main-main",
      
      # minor-main with Candidate1 as National Party
      Pair_Type == "minor-main" & Candidate1_Party_Type_TCPD == "National Party" ~ "main-main",
      
      # minor-minor with either candidate from National Party
      Pair_Type == "minor-minor" & Candidate1_Party_Type_TCPD == "National Party" ~ "main-minor",
      Pair_Type == "minor-minor" & Candidate2_Party_Type_TCPD == "National Party" ~ "minor-main",
      
      # Default: keep the original pair type
      TRUE ~ Pair_Type
    )
  ) %>%
  
  # add flag
  mutate(
    Row_Changed = 
      Original_Pair_Type != Pair_Type | 
      Original_Candidate1_Type != Candidate1_Type | 
      Original_Candidate2_Type != Candidate2_Type
  ) %>%
  
  dplyr::select(
    everything(),
  )

changed_rows <- candidate_pairs_new_minor %>% 
  filter(Row_Changed == TRUE)

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

candidate_pairs_new_minor <- candidate_pairs_new_minor %>%
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

# removed overlap minor candidates! 
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
  filename = "110425 meeting plots/Decoy_pairs_by_type.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

### Same thing for new_minor
candidate_pairs_new_minor <- candidate_pairs_new_minor %>%
  mutate(
    Candidate1_Election_ID = paste(Year, State_Name, Constituency_Name, Assembly_No, Election_Type, Candidate1_PID, sep = "_"),
    Candidate2_Election_ID = paste(Year, State_Name, Constituency_Name, Assembly_No, Election_Type, Candidate2_PID, sep = "_")
  )

candidate_pairs_main_minor <- candidate_pairs_new_minor %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main",
         is_decoy == TRUE)

candidate_pairs_main_main <- candidate_pairs_new_minor %>%
  filter(Pair_Type == "main-main",
         is_decoy == TRUE)

candidate_pairs_minor_minor <- candidate_pairs_new_minor %>%
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

length(overlap_candidates)
length(overlap_candidates) / length(minor_candidates_that_are_decoys_of_minor) * 100

elections_with_main_minor_decoys <- candidate_pairs_new_minor %>%
  filter((Pair_Type == "main-minor" | Pair_Type == "minor-main") & is_decoy == TRUE) %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
  nrow()

elections_with_main_main_decoys <- candidate_pairs_new_minor %>%
  filter(Pair_Type == "main-main" & is_decoy == TRUE) %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
  nrow()

# removed overlap minor candidates!
elections_with_minor_minor_decoys <- candidate_pairs_new_minor %>%
  filter((Pair_Type == "minor-minor") & is_decoy == TRUE) %>%
  filter(!(Candidate1_Election_ID %in% minor_candidates_that_are_decoys_of_main |
             Candidate2_Election_ID %in% minor_candidates_that_are_decoys_of_main)) %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type) %>%
  nrow()

total_elections <- candidate_pairs_new_minor %>%
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

decoy_by_pair_type <- candidate_pairs_new_minor %>%
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
  labs(title = "Decoy Pairs by Pair Type (Not Minor if National Party)",
       x = "Pair Type",
       y = "Percentage",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "110425 meeting plots/Decoy_pairs_by_type_new_minor.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png",
  bg = "white"
)

all_states_elections_minor <- all_states_elections %>%
  filter(Vote_Share_Percentage < 10, na.rm = TRUE)

sum(all_states_elections_minor$Party == "INC" | all_states_elections_minor$Party == "BJP")

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
  
  # apply the thresholds to determine decoys
  temp_candidate_pairs <- candidate_pairs_data %>%
    mutate(
      # count how many measures are above the higher threshold
      high_threshold_count = (
        (Levenshtein_Similarity > lv_threshold) +
          (Jaro_Winkler_Similarity > jw_threshold) +
          (Metaphone_Similarity > mp_threshold) +
          (Masala_Similarity > mas_threshold) +
          (NGram_Similarity > ng_threshold)
      ),
      # determine if a pair is a decoy based on selected method
      is_decoy = case_when(
        # original method: all 5 above higher threshold
        method == "strict" ~ high_threshold_count == 5,
        
        # flexible method: at least 4 above higher threshold, all above lower
        method == "flexible_4of5" ~ (high_threshold_count >= 4) &
          (Levenshtein_Similarity > lv_lower_threshold) &
          (Jaro_Winkler_Similarity > jw_lower_threshold) &
          (Metaphone_Similarity > mp_lower_threshold) &
          (Masala_Similarity > mas_lower_threshold) &
          (NGram_Similarity > ng_lower_threshold),
        
        # alternative method: at least 1 above higher threshold, all above lower
        method == "flexible_1of5" ~ (high_threshold_count >= 1) &
          (Levenshtein_Similarity > lv_lower_threshold) &
          (Jaro_Winkler_Similarity > jw_lower_threshold) &
          (Metaphone_Similarity > mp_lower_threshold) &
          (Masala_Similarity > mas_lower_threshold) &
          (NGram_Similarity > ng_lower_threshold),
        
        # dafault to FALSE if method is not recognized
        TRUE ~ FALSE
      )
    ) %>%
    dplyr::select(-high_threshold_count) # remove the temporary count column
  
  # The rest of the function remains the same
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
      lower_percentile = ifelse(method != "strict", lower_percentile, NA_real_)
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
    lower_percentile = if(method != "strict") lower_percentile else NA
  )
  
  # return both metrics
  return(list(
    pair_results = results,
    election_results = elections_results
  ))
}

percentiles <- seq(0.984, 0.998, by = 0.001)

# run the analysis for all percentiles (using purrr's map function)
sensitivity_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs, .x, method = "strict", lower_percentile = 0.974))

flexible_4of5_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs, .x, method = "flexible_4of5", lower_percentile = 0.99))

flexible_1of5_results <- map(percentiles, ~analyze_decoys_at_threshold(
  candidate_pairs, .x, method = "flexible_1of5", lower_percentile = 0.99))

# plots for strict
# get pair results and election results
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

ggsave(
  filename = "110425 meeting plots/Sensitivity_of_decoy_pairs_2.png",
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
       subtitle = "Percentage of Pairs that are Decoys; All 5 measures above threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "110425 meeting plots/Sensitivity_of_decoy_elections_2.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# calculate the strategic component at each threshold
strategic_component <- pair_results %>%
  group_by(percentile) %>%
  mutate(
    baseline_rate = mean(pct_decoy[Consolidated_Pair_Type %in% c("main-main", "minor-minor")]),
    strategic_component = pct_decoy - baseline_rate,
    is_strategic = Consolidated_Pair_Type == "main-minor/minor-main"
  ) %>%
  filter(is_strategic)

# plot the strategic component
ggplot(strategic_component, aes(x = percentile, y = strategic_component)) +
  geom_line() +
  geom_point() +
  labs(title = "Strategic Component of Main-Minor Decoys by Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Strategic Component (percentage points)") +
  theme_minimal()

# create a summary table
summary_table <- pair_results %>%
  pivot_wider(
    id_cols = percentile,
    names_from = Consolidated_Pair_Type,
    values_from = pct_decoy
  ) %>%
  mutate(
    baseline_rate = (`main-main` + `minor-minor`) / 2,
    strategic_component = `main-minor/minor-main` - baseline_rate
  )

# plots for flexible 4/5
# get pair results and election results
pair_results <- map_dfr(flexible_4of5_results, ~.x$pair_results)
election_results <- map_dfr(flexible_4of5_results, ~.x$election_results)

# create a visualization for pair percentages
ggplot(pair_results, aes(x = percentile, y = pct_decoy, color = Consolidated_Pair_Type)) +
  geom_line() +
  geom_point() +
  # adjust y axis breaks
  scale_y_continuous(breaks = seq(0, max(pair_results$pct_decoy), by = 0.02)) +
  # adjust x axis breaks
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.002)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; 1/5 above 0.984, 4/5 above x-axis value",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "110425 meeting plots/Sensitivity_of_decoy_pairs_4.png",
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
  scale_y_continuous(breaks = seq(0, max(election_results$pct_elections), by = 0.2)) +
  # adjust x axis breaks
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.002)) +
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; 1/5 above 0.984, 4/5 above x-axis value",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "110425 meeting plots/Sensitivity_of_decoy_elections_4.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# calculate the strategic component at each threshold
strategic_component <- pair_results %>%
  group_by(percentile) %>%
  mutate(
    baseline_rate = mean(pct_decoy[Consolidated_Pair_Type %in% c("main-main", "minor-minor")]),
    strategic_component = pct_decoy - baseline_rate,
    is_strategic = Consolidated_Pair_Type == "main-minor/minor-main"
  ) %>%
  filter(is_strategic)

# plot the strategic component
ggplot(strategic_component, aes(x = percentile, y = strategic_component)) +
  geom_line() +
  geom_point() +
  labs(title = "Strategic Component of Main-Minor Decoys by Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Strategic Component (percentage points)") +
  theme_minimal()

# create a summary table
summary_table <- pair_results %>%
  pivot_wider(
    id_cols = percentile,
    names_from = Consolidated_Pair_Type,
    values_from = pct_decoy
  ) %>%
  mutate(
    baseline_rate = (`main-main` + `minor-minor`) / 2,
    strategic_component = `main-minor/minor-main` - baseline_rate
  )

# plots for flexible 1/5
# get pair results and election results
pair_results <- map_dfr(flexible_1of5_results, ~.x$pair_results)
election_results <- map_dfr(flexible_1of5_results, ~.x$election_results)

# create a visualization for pair percentages
ggplot(pair_results, aes(x = percentile, y = pct_decoy, color = Consolidated_Pair_Type)) +
  geom_line() +
  geom_point() +
  # adjust y axis breaks
  scale_y_continuous(breaks = seq(0, max(pair_results$pct_decoy), by = 0.02)) +
  # adjust x axis breaks
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.002)) +
  labs(title = "Sensitivity of Decoy Percentages to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; 4/5 above 0.984, 1/5 above x-axis value",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Pairs that are Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "110425 meeting plots/Sensitivity_of_decoy_pairs_3.png",
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
  scale_y_continuous(breaks = seq(0, max(election_results$pct_elections), by = 0.2)) +
  # adjust x axis breaks
  scale_x_continuous(breaks = seq(0.975, 1, by = 0.002)) +
  labs(title = "Sensitivity of Elections with Decoys to Similarity Threshold",
       subtitle = "Percentage of Pairs that are Decoys; 4/5 above 0.984, 1/5 above x-axis value",
       x = "Similarity Score Percentile Threshold",
       y = "Percentage of Elections with Decoys",
       color = "Pair Type") +
  theme_minimal()

ggsave(
  filename = "110425 meeting plots/Sensitivity_of_decoy_elections_3.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# calculate the strategic component at each threshold
strategic_component <- pair_results %>%
  group_by(percentile) %>%
  mutate(
    baseline_rate = mean(pct_decoy[Consolidated_Pair_Type %in% c("main-main", "minor-minor")]),
    strategic_component = pct_decoy - baseline_rate,
    is_strategic = Consolidated_Pair_Type == "main-minor/minor-main"
  ) %>%
  filter(is_strategic)

# plot the strategic component
ggplot(strategic_component, aes(x = percentile, y = strategic_component)) +
  geom_line() +
  geom_point() +
  labs(title = "Strategic Component of Main-Minor Decoys by Threshold",
       x = "Similarity Score Percentile Threshold",
       y = "Strategic Component (percentage points)") +
  theme_minimal()

# create a summary table
summary_table <- pair_results %>%
  pivot_wider(
    id_cols = percentile,
    names_from = Consolidated_Pair_Type,
    values_from = pct_decoy
  ) %>%
  mutate(
    baseline_rate = (`main-main` + `minor-minor`) / 2,
    strategic_component = `main-minor/minor-main` - baseline_rate
  )


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
    pull(Candidate1_PID),
  adjusted_candidate_pairs %>% 
    filter(Pair_Type == "main-minor", is_decoy == TRUE) %>% 
    pull(Candidate2_PID)
))

adjusted_elections_with_minor_minor_decoys <- adjusted_candidate_pairs %>%
  filter((Pair_Type == "minor-minor") & is_decoy == TRUE) %>%
  filter(!(Candidate1_PID %in% adjusted_minor_candidates_that_are_decoys_of_main | 
             Candidate2_PID %in% adjusted_minor_candidates_that_are_decoys_of_main)) %>%
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
             (Candidate1_PID %in% adjusted_minor_candidates_that_are_decoys_of_main | 
                Candidate2_PID %in% adjusted_minor_candidates_that_are_decoys_of_main))) %>%
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
  filename = "110425 meeting plots/Decoy_pairs_by_type_random_removal_2.png",
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
    pull(Candidate1_PID),
  adjusted_candidate_pairs %>% 
    filter(Pair_Type == "main-minor", is_decoy == TRUE) %>% 
    pull(Candidate2_PID)
))

adjusted_elections_with_minor_minor_decoys <- adjusted_candidate_pairs %>%
  filter((Pair_Type == "minor-minor") & is_decoy == TRUE) %>%
  filter(!(Candidate1_PID %in% adjusted_minor_candidates_that_are_decoys_of_main | 
             Candidate2_PID %in% adjusted_minor_candidates_that_are_decoys_of_main)) %>%
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
             (Candidate1_PID %in% adjusted_minor_candidates_that_are_decoys_of_main | 
                Candidate2_PID %in% adjusted_minor_candidates_that_are_decoys_of_main))) %>%
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
  filename = "110425 meeting plots/Decoy_pairs_by_type_random_removal_3.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

### Working with CLEA data
  # Note, we only have about 80% of the results because of a mistake in the code
# Understanding the results
# first clean
# how many main/minor candidates? # 30% main, 70% minor
total_main <- sum(CLEA_cleaned$cvs1 > 0.1, na.rm = TRUE)
total_minor <- sum(CLEA_cleaned$cvs1 < 0.1, na.rm = TRUE)
total <- sum(CLEA_cleaned$cvs1 > 0, na.rm = TRUE)

# is this dataset representative? 
# first, which elections are covered
unique_elections_pairs <- candidate_pairs_CLEA %>%
  dplyr::select(Country_Code, Year, Election_Month, Constituency_Name, Election_ID) %>%
  distinct()

unique_elections_CLEA <- CLEA_cleaned %>%
  dplyr::select(ctr, yr, mn, cst_n, id) %>%
  distinct()

elections_in_pairs <- unique_elections_pairs %>%
  rename(
    ctr = Country_Code,
    yr = Year,
    mn = Election_Month,
    cst_n = Constituency_Name,
    id = Election_ID
  )

CLEA_cleaned_fr <- CLEA_cleaned %>%
  semi_join(elections_in_pairs, by = c("ctr", "yr", "mn", "cst_n", "id"))

# remove with invalid election id
CLEA_cleaned_fr <- CLEA_cleaned_fr %>%
  filter(id != -999)

candidate_pairs_CLEA <- candidate_pairs_CLEA %>%
  filter(Election_ID != -999)

### remove india
CLEA_cleaned_fr_no_ind <- CLEA_cleaned_fr %>%
  filter(ctr != 356)

candidate_pairs_CLEA_no_ind <- candidate_pairs_CLEA %>%
  filter(Country_Code != 356)

# plots of summary stats using yr, rg, ctr
# count elections by region and year and create heatmap of regional and temporal coverage
coverage_data <- CLEA_cleaned_fr_no_ind %>%
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
  filename = "110425 meeting plots/CLEA_coverage.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# categorize candidates as main or minor
CLEA_cleaned_fr_no_ind <- CLEA_cleaned_fr_no_ind %>%
  mutate(candidate_type = ifelse(cvs1 > 0.1, "main", "minor"))

# boxplot of main/minor across regions
ggplot(CLEA_cleaned_fr_no_ind, aes(x = rg, y = cvs1, fill = candidate_type)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "Distribution of Vote Shares by Region",
       x = "Region", y = "Vote Share (%)",
       fill = "Candidate Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "110425 meeting plots/CLEA_main_minor_rates.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# voteshare distribution across countries
# calculate total vote share by candidate type per year
voteshare <- CLEA_cleaned_fr_no_ind %>%
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
  filename = "110425 meeting plots/CLEA_voteshare_main_minor.png",
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
    Levenshtein_Similarity >= lv_99th & 
      Jaro_Winkler_Similarity >= jw_99th & 
      Metaphone_Similarity >= mp_99th & 
      Masala_Similarity >= mas_99th & 
      NGram_Similarity >= ng_99th,
    TRUE, 
    FALSE
  ))

### how are main_main, main_minor and minor_minor different? false positives? 
candidate_pairs_CLEA_main_minor <- candidate_pairs_CLEA_no_ind %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main", 
         is_decoy == TRUE)

candidate_pairs_CLEA_main_main <- candidate_pairs_CLEA_no_ind %>%
  filter(Pair_Type == "main-main", 
         is_decoy == TRUE)

candidate_pairs_CLEA_minor_minor <- candidate_pairs_CLEA_no_ind %>%
  filter(Pair_Type == "minor-minor", 
         is_decoy == TRUE)

minor_candidates_that_are_decoys_of_minor <- unique(
  candidate_pairs_CLEA_minor_minor %>%
    dplyr::select(Candidate1_Name, Candidate1_Party_Name, Candidate2_Name, Candidate2_Party_Name, Election_ID) %>%
    pivot_longer(cols = c(Candidate1_Name, Candidate2_Name),
                 names_to = "position",
                 values_to = "candidate_name") %>%
    # Extract party code based on position
    mutate(party_code = ifelse(position == "Candidate1_Name", Candidate1_Party_Name, Candidate2_Party_Name)) %>%
    dplyr::select(candidate_name, party_code, Election_ID)
)

# minor minor candidates who are decoys in main-minor pairs
minor_candidates_that_are_decoys_of_main <- bind_rows(
  candidate_pairs_CLEA_main_minor %>% 
    filter(Pair_Type == "minor-main") %>% 
    dplyr::select(Candidate1_Name, Candidate1_Party_Name, Election_ID) %>%
    rename(candidate_name = Candidate1_Name, party_code = Candidate1_Party_Name),
  
  candidate_pairs_CLEA_main_minor %>% 
    filter(Pair_Type == "main-minor") %>% 
    dplyr::select(Candidate2_Name, Candidate2_Party_Name, Election_ID) %>%
    rename(candidate_name = Candidate2_Name, party_code = Candidate2_Party_Name)
)

overlap_candidates <- inner_join(
  minor_candidates_that_are_decoys_of_minor,
  minor_candidates_that_are_decoys_of_main,
  by = c("candidate_name", "party_code", "Election_ID")
)

# count elections with main-minor decoys
elections_with_main_minor_decoys <- candidate_pairs_CLEA_no_ind %>%
  filter((Pair_Type == "main-minor" | Pair_Type == "minor-main") & is_decoy == TRUE) %>%
  distinct(Country_Code, Year, Election_Month, Constituency_Name, Election_ID) %>%
  nrow()

# count elections with main-main decoys
elections_with_main_main_decoys <- candidate_pairs_CLEA_no_ind %>%
  filter(Pair_Type == "main-main" & is_decoy == TRUE) %>%
  distinct(Country_Code, Year, Election_Month, Constituency_Name, Election_ID) %>%
  nrow()

# count elections with minor-minor decoys, excluding the overlaps
elections_with_minor_minor_decoys <- candidate_pairs_CLEA_no_ind %>%
  filter(Pair_Type == "minor-minor" & is_decoy == TRUE) %>%
  # Modified to create keys with party codes
  mutate(
    key1 = paste(Candidate1_Name, Candidate1_Party_Code, Election_ID),
    key2 = paste(Candidate2_Name, Candidate2_Party_Code, Election_ID)
  ) %>%
  # Modified to match with new keys
  filter(!(key1 %in% paste(overlap_candidates$candidate_name, overlap_candidates$party_code, overlap_candidates$Election_ID) | 
             key2 %in% paste(overlap_candidates$candidate_name, overlap_candidates$party_code, overlap_candidates$Election_ID))) %>%
  distinct(Country_Code, Year, Election_Month, Constituency_Name, Election_ID) %>%
  nrow()

# get total number of elections
total_elections <- candidate_pairs_CLEA_no_ind %>%
  distinct(Country_Code, Year, Election_Month, Constituency_Name, Election_ID) %>%
  nrow()

# create final df
elections_with_decoys_df <- data.frame(
  Consolidated_Pair_Type = c("main-minor/minor-main", "main-main", "minor-minor"),
  pct_elections_with_decoys = c(
    elections_with_main_minor_decoys / total_elections * 100,
    elections_with_main_main_decoys / total_elections * 100,
    elections_with_minor_minor_decoys / total_elections * 100
  )
)

# a key for easy matching
minor_candidates_that_are_decoys_of_main$key <- paste(
  minor_candidates_that_are_decoys_of_main$candidate_name,
  minor_candidates_that_are_decoys_of_main$party_code,
  minor_candidates_that_are_decoys_of_main$Election_ID
)


decoy_by_pair_type <- candidate_pairs_CLEA_no_ind %>%
  # No changes in this part
  mutate(
    Consolidated_Pair_Type = case_when(
      Pair_Type == "main-minor" ~ "main-minor/minor-main",
      Pair_Type == "minor-main" ~ "main-minor/minor-main",
      TRUE ~ Pair_Type
    ),
    # Modified to include party codes in keys
    key1 = paste(Candidate1_Name, Candidate1_Party_Code, Election_ID),
    key2 = paste(Candidate2_Name, Candidate2_Party_Code, Election_ID)
  ) %>%
  # Modified filter to use new key structure
  filter(!(Consolidated_Pair_Type == "minor-minor" & 
             (key1 %in% minor_candidates_that_are_decoys_of_main$key | 
                key2 %in% minor_candidates_that_are_decoys_of_main$key))) %>%
  # No changes needed in these parts
  group_by(Consolidated_Pair_Type) %>%
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
  filename = "110425 meeting plots/Decoy_pairs_by_type_CLEA.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

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
adjusted_elections_with_main_minor_decoys <- adjusted_candidate_pairs %>%
  filter((Pair_Type == "main-minor" | Pair_Type == "minor-main") & is_decoy == TRUE) %>%
  distinct(Country_Code, Year, Election_Month, Constituency_Name, Election_ID) %>%
  nrow()

adjusted_elections_with_main_main_decoys <- adjusted_candidate_pairs %>%
  filter(Pair_Type == "main-main" & is_decoy == TRUE) %>%
  distinct(Country_Code, Year, Election_Month, Constituency_Name, Election_ID) %>%
  nrow()

# recalculate minor candidates that are decoys of main
adjusted_minor_candidates_that_are_decoys_of_main <- unique(c(
  adjusted_candidate_pairs %>% 
    filter(Pair_Type == "minor-main") %>% 
    dplyr::select(Candidate1_Name, Candidate1_Party_Name, Election_ID) %>%
    rename(candidate_name = Candidate1_Name, party_code = Candidate1_Party_Name),
  adjusted_candidate_pairs %>% 
    filter(Pair_Type == "main-minor") %>% 
    dplyr::select(Candidate2_Name, Candidate2_Party_Name, Election_ID) %>%
    rename(candidate_name = Candidate2_Name, party_code = Candidate2_Party_Name)
))

adjusted_elections_with_minor_minor_decoys <- adjusted_candidate_pairs %>%
  filter(Pair_Type == "minor-minor" & is_decoy == TRUE) %>%
  # Modified to create keys with party codes
  mutate(
    key1 = paste(Candidate1_Name, Candidate1_Party_Code, Election_ID),
    key2 = paste(Candidate2_Name, Candidate2_Party_Code, Election_ID)
  ) %>%
  # Modified to match with new keys
  filter(!(key1 %in% paste(overlap_candidates$candidate_name, overlap_candidates$party_code, overlap_candidates$Election_ID) | 
             key2 %in% paste(overlap_candidates$candidate_name, overlap_candidates$party_code, overlap_candidates$Election_ID))) %>%
  distinct(Country_Code, Year, Election_Month, Constituency_Name, Election_ID) %>%
  nrow()

# get total elections
total_elections <- adjusted_candidate_pairs %>%
  distinct(Country_Code, Year, Election_Month, Constituency_Name, Election_ID) %>%
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
    ),
    # Modified to include party codes in keys
    key1 = paste(Candidate1_Name, Candidate1_Party_Code, Election_ID),
    key2 = paste(Candidate2_Name, Candidate2_Party_Code, Election_ID)
  ) %>%
  # Modified filter to use new key structure
  filter(!(Consolidated_Pair_Type == "minor-minor" & 
             (key1 %in% adjusted_minor_candidates_that_are_decoys_of_main$key | 
                key2 %in% adjusted_minor_candidates_that_are_decoys_of_main$key))) %>%
  # No changes needed in these parts
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
  filename = "110425 meeting plots/Decoy_pairs_by_type_random_removal_CLEA.png",
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
  filename = "110425 meeting plots/Main_Minor_Decoy_Histogram_CLEA.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# creating no. of candidates per election df from CLEA_cleaned_fr_no_ind
constituency_candidates <- CLEA_cleaned_fr_no_ind %>%
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

country_codes <- CLEA_cleaned_fr_no_ind %>%
  dplyr::select(ctr, ctr_n) %>%
  distinct() %>%
  rename(Country_Code = ctr, Country = ctr_n)

country_metrics <- country_metrics %>%
  left_join(country_codes, by = "Country_Code")

# how many decoy elections, country
# limit to country with more than 10 constituencies i.e.
country_metrics_1 <- country_metrics %>%
  filter(constituency_count > 10)

ggplot(country_metrics_1, aes(x = reorder(Country, -pct_races_with_decoys), y = pct_races_with_decoys)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", pct_races_with_decoys)), 
            vjust = -0.5, size = 3) +
  labs(title = "Percentage of Elections with Decoys by Country",
       x = "Country",
       y = "Percentage of Elections with Decoys (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "110425 meeting plots/Decoy_elections_state_histogram_CLEA.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

ggplot(country_metrics_1, aes(x = reorder(Country, -pct_races_with_3_decoys), y = pct_races_with_3_decoys)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", pct_races_with_3_decoys)), 
            vjust = -0.5, size = 3) +
  labs(title = "Percentage of Elections with at least 3 Decoys by Country",
       x = "Country",
       y = "Percentage of Elections with at least 3 Decoys (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "110425 meeting plots/Decoy_elections_atleast3_state_histogram_CLEA.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

country_metrics_1 %>%
  mutate(Country = fct_reorder(Country, -mean_decoy_share)) %>%
  ggplot(aes(x = Country, y = mean_decoy_share)) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.8) +
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
  filename = "110425 meeting plots/Decoy_candidate_country_CLEA.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

country_metrics_1 %>%
  mutate(Country = fct_reorder(Country, -mean_num_decoys)) %>%
  ggplot(aes(x = Country, y = mean_num_decoys)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
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
  filename = "110425 meeting plots/Decoy_candidates_number_country_CLEA.png",
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
    breaks = seq(1880, 2025, by = 5)) + 
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

ggsave(
  filename = "110425 meeting plots/Decoy_share_over_time_CLEA.png",
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
    breaks = seq(1880, 2025, by = 5)) + 
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

ggsave(
  filename = "110425 meeting plots/Decoy_voteshare_over_time_CLEA.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

country_metrics_1 %>%
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
  filename = "110425 meeting plots/Decoy_voteshare_by_country_CLEA.png",
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
  election_candidates <- CLEA_cleaned_fr_no_ind %>%
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

top_examples <- election_outcomes_changed %>%
  arrange(desc(num_decoys), desc(vote_difference)) %>%
  tail(10)

bottom_examples <- election_outcomes_changed %>%
  arrange(desc(num_decoys), desc(vote_difference)) %>%
  tail(10)

for (i in 1:nrow(top_examples)) {
  example <- top_examples[i,]
  
  # get the decoy details for this election
  decoys_info <- all_states_elections %>%
    filter(Year == example$Year,
           State_Name == example$State_Name,
           Constituency_Name == example$Constituency_Name,
           Assembly_No == example$Assembly_No,
           Election_Type == example$Election_Type,
           pid %in% unlist(grouped_decoys$decoy_pids[
             grouped_decoys$Year == example$Year &
               grouped_decoys$State_Name == example$State_Name &
               grouped_decoys$Constituency_Name == example$Constituency_Name &
               grouped_decoys$Assembly_No == example$Assembly_No &
               grouped_decoys$Election_Type == example$Election_Type &
               grouped_decoys$main_pid == example$main_candidate_pid
           ])) %>%
    dplyr::select(Candidate, Vote_Share_Percentage)
  
  # create a string with decoy names and vote shares
  decoy_details <- paste(
    sapply(1:nrow(decoys_info), function(j) {
      sprintf("%s (%.2f%%)", decoys_info$Candidate[j], decoys_info$Vote_Share_Percentage[j])
    }),
    collapse = "; "
  )
  
  # add to the top_examples dataframe
  top_examples$decoy_details[i] <- decoy_details
}

election_flip_examples <- top_examples %>%
  # select and rename columns for clarity
  dplyr::select(
    Year,
    State = State_Name,
    `Election Type` = Election_Type,
    Constituency = Constituency_Name,
    `Main Candidate` = main_candidate,
    `Original Position` = main_old_position,
    `Original Vote %` = main_old_vote_share,
    `Combined Vote %` = main_new_vote_share,
    `Number of Decoys` = num_decoys,
    `Decoy Details` = decoy_details,
    `Actual Winner` = winner,
    `Winner Vote %` = winner_vote_share,
    `Margin (% pts)` = vote_difference
  ) %>%
  # format stuff
  mutate(
    `Original Vote %` = sprintf("%.2f%%", `Original Vote %`),
    `Combined Vote %` = sprintf("%.2f%%", `Combined Vote %`),
    `Winner Vote %` = sprintf("%.2f%%", `Winner Vote %`),
    `Margin (% pts)` = sprintf("+%.2f", `Margin (% pts)`)
  )

election_flip_examples %>%
  gt() %>%
  tab_header(
    title = "Elections Where Decoy Candidates Changed the Outcome"
  ) %>%
  fmt_markdown(columns = "Decoy Details") %>%
  cols_width(
    `Decoy Details` ~ px(300),
    `Main Candidate` ~ px(150),
    `Actual Winner` ~ px(150),
    everything() ~ px(100)
  ) %>%
  tab_options(
    row.striping.include_table_body = TRUE,
    table.width = pct(100)
  ) %>%
  opt_table_font(font = "Arial") %>%
  opt_row_striping() %>%
  opt_all_caps() %>%
  opt_table_lines(extent = "none")

# combine plots
if (length(plot_list) > 0) {
  combined_plot <- do.call(grid.arrange, c(plot_list, ncol = 2))
  ggsave("decoy_impact_examples.png", combined_plot, width = 12, height = 8)
}

for (i in 1:nrow(bottom_examples)) {
  example <- bottom_examples[i,]
  
  # get the decoy details for this election
  decoys_info <- all_states_elections %>%
    filter(Year == example$Year,
           State_Name == example$State_Name,
           Constituency_Name == example$Constituency_Name,
           Assembly_No == example$Assembly_No,
           Election_Type == example$Election_Type,
           pid %in% unlist(grouped_decoys$decoy_pids[
             grouped_decoys$Year == example$Year &
               grouped_decoys$State_Name == example$State_Name &
               grouped_decoys$Constituency_Name == example$Constituency_Name &
               grouped_decoys$Assembly_No == example$Assembly_No &
               grouped_decoys$Election_Type == example$Election_Type &
               grouped_decoys$main_pid == example$main_candidate_pid
           ])) %>%
    dplyr::select(Candidate, Vote_Share_Percentage)
  
  # create a string with decoy names and vote shares
  decoy_details <- paste(
    sapply(1:nrow(decoys_info), function(j) {
      sprintf("%s (%.2f%%)", decoys_info$Candidate[j], decoys_info$Vote_Share_Percentage[j])
    }),
    collapse = "; "
  )
  
  # add to the bottom_examples dataframe
  bottom_examples$decoy_details[i] <- decoy_details
}

election_flip_examples <- bottom_examples %>%
  # select and rename columns for clarity
  dplyr::select(
    Year,
    State = State_Name,
    `Election Type` = Election_Type,
    Constituency = Constituency_Name,
    `Main Candidate` = main_candidate,
    `Original Position` = main_old_position,
    `Original Vote %` = main_old_vote_share,
    `Combined Vote %` = main_new_vote_share,
    `Number of Decoys` = num_decoys,
    `Decoy Details` = decoy_details,
    `Actual Winner` = winner,
    `Winner Vote %` = winner_vote_share,
    `Margin (% pts)` = vote_difference
  ) %>%
  # format stuff
  mutate(
    `Original Vote %` = sprintf("%.2f%%", `Original Vote %`),
    `Combined Vote %` = sprintf("%.2f%%", `Combined Vote %`),
    `Winner Vote %` = sprintf("%.2f%%", `Winner Vote %`),
    `Margin (% pts)` = sprintf("+%.2f", `Margin (% pts)`)
  )

election_flip_examples %>%
  gt() %>%
  tab_header(
    title = "Elections Where Decoy Candidates Changed the Outcome"
  ) %>%
  fmt_markdown(columns = "Decoy Details") %>%
  cols_width(
    `Decoy Details` ~ px(300),
    `Main Candidate` ~ px(150),
    `Actual Winner` ~ px(150),
    everything() ~ px(100)
  ) %>%
  tab_options(
    row.striping.include_table_body = TRUE,
    table.width = pct(100)
  ) %>%
  opt_table_font(font = "Arial") %>%
  opt_row_striping() %>%
  opt_all_caps() %>%
  opt_table_lines(extent = "none")

# Combine plots
if (length(plot_list) > 0) {
  combined_plot <- do.call(grid.arrange, c(plot_list, ncol = 2))
  ggsave("decoy_impact_examples.png", combined_plot, width = 12, height = 8)
}

unique_elections_w_decoys <- decoy_pairs %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main") %>%
  dplyr::select(Year, State_Name, Constituency_Name, Election_Type, Assembly_No) %>%
  distinct()

unique_elections <- all_states_elections %>%
  dplyr::select(Year, State_Name, Constituency_Name, Election_Type, Assembly_No) %>%
  distinct()

# 3 % of elections with decoys flipped because of decoys
nrow(election_outcomes_changed) / nrow(unique_elections_w_decoys) * 100
# next step is to figure out how meaningful these flipped (if true) elections were

nrow(election_outcomes_changed) / nrow(unique_elections) * 100

for (i in 1:nrow(election_outcomes_changed)) {
  example <- election_outcomes_changed[i,]
  
  # get the decoy details for this election
  decoys_info <- all_states_elections %>%
    filter(Year == example$Year,
           State_Name == example$State_Name,
           Constituency_Name == example$Constituency_Name,
           Assembly_No == example$Assembly_No,
           Election_Type == example$Election_Type,
           pid %in% unlist(grouped_decoys$decoy_pids[
             grouped_decoys$Year == example$Year &
               grouped_decoys$State_Name == example$State_Name &
               grouped_decoys$Constituency_Name == example$Constituency_Name &
               grouped_decoys$Assembly_No == example$Assembly_No &
               grouped_decoys$Election_Type == example$Election_Type &
               grouped_decoys$main_pid == example$main_candidate_pid
           ])) %>%
    dplyr::select(Candidate, Vote_Share_Percentage)
  
  # create a string with decoy names and vote shares
  decoy_details <- paste(
    sapply(1:nrow(decoys_info), function(j) {
      sprintf("%s (%.2f%%)", decoys_info$Candidate[j], decoys_info$Vote_Share_Percentage[j])
    }),
    collapse = "; "
  )
  
  # add to the election_outcomes_changed dataframe
  election_outcomes_changed$decoy_details[i] <- decoy_details
}

### What do minor-minor and main-main decoys look like? 
set.seed(2222)

candidate_pairs_CLEA_main_minor_sample <- candidate_pairs_CLEA_main_minor %>%
  sample_n(min(15, nrow(.)), replace = FALSE) %>%
  dplyr::select(Year, Candidate1_Name, Candidate2_Name, Levenshtein_Similarity, Jaro_Winkler_Similarity, 
                NGram_Similarity, Masala_Similarity, Metaphone_Similarity)

candidate_pairs_CLEA_main_minor_sample %>%
  dplyr::select(Year, Candidate1_Name, Candidate2_Name, Levenshtein_Similarity, Jaro_Winkler_Similarity, Metaphone_Similarity, 
                NGram_Similarity, Masala_Similarity) %>%
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

candidate_pairs_main_main_sample <- candidate_pairs_main_main %>%
  sample_n(min(15, nrow(.))) %>%
  dplyr::select(Candidate1_Name, Candidate2_Name, Levenshtein_Similarity, Jaro_Winkler_Similarity, 
                NGram_Similarity, Masala_Similarity, Metaphone_Similarity)

candidate_pairs_main_main_sample %>%
  dplyr::select(Candidate1_Name, Candidate2_Name, Levenshtein_Similarity, Jaro_Winkler_Similarity, Metaphone_Similarity, 
                NGram_Similarity, Masala_Similarity) %>%
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
    title = "High Similarity Candidate Pairs (Above 99th Percentile) Main-Main",
  ) %>%
  cols_label(
    Candidate1_Name = "Name 1",
    Candidate2_Name = "Name 2",
    Levenshtein_Similarity = "Levenshtein",
    Jaro_Winkler_Similarity = "Jaro-Winkler",
    Metaphone_Similarity = "Metaphone", 
    Masala_Similarity = "Masala",
    NGram_Similarity = "NGram"
  )