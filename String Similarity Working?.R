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

### Loading
all_states_elections <- read.csv("Raw Data/all_states_elections.csv")
candidate_pairs <- read.csv("Cleaned Data/candidate_pairs_lv_jw_ngram_masala_dblmet.csv")

### 30% of all candidates are main. around 170,000
sum(all_states_elections$Vote_Share_Percentage > 10, na.rm = TRUE)
sum(all_states_elections$Vote_Share_Percentage < 10, na.rm = TRUE)
sum(all_states_elections$Vote_Share_Percentage > 10, na.rm = TRUE) / sum(all_states_elections$Vote_Share_Percentage > 0, na.rm = TRUE)

# small cleaning smmore
candidate_pairs <- candidate_pairs %>%
  filter(Candidate1_Party != "NOTA" & Candidate2_Party != "NOTA")

unique_elections <- all_states_elections %>%
  dplyr::select(Year, State_Name, Constituency_Name, Election_Type, Assembly_No) %>%
  distinct()

### Creating some tables to see how this is working
mahasamund_CT_2014_GE <- candidate_pairs %>%
  filter(Year == "2014" & State_Name == "Chhattisgarh" & Constituency_Name == "MAHASAMUND" & Election_Type == "Lok Sabha Election (GE)")

kattumannarkoil_TN_2016_AE <- candidate_pairs %>%
  filter(Year == "2016" & State_Name == "Tamil_Nadu" & Constituency_Name == "KATTUMANNARKOIL" & Election_Type == "State Assembly Election (AE)")

westdelhi_DL_2014_GE <- candidate_pairs %>%
  filter(Year == "2014" & State_Name == "Delhi" & Constituency_Name == "WEST DELHI" & Election_Type == "Lok Sabha Election (GE)")

holenarasipur_KN_2023_AE <- candidate_pairs %>%
  filter(Year == "2023" & State_Name == "Karnataka" & Constituency_Name == "HOLENARASIPUR" & Election_Type == "State Assembly Election (AE)")

jayanagar_KN_2023_AE <- candidate_pairs %>%
  filter(Year == "2023" & State_Name == "Karnataka" & Constituency_Name == "JAYANAGAR" & Election_Type == "State Assembly Election (AE)")

malur_KN_2023_AE <- candidate_pairs %>%
  filter(Year == "2023" & State_Name == "Karnataka" & Constituency_Name == "MALUR" & Election_Type == "State Assembly Election (AE)")

quantile(candidate_pairs$Levenshtein_Similarity, probs = c(0.5, 0.95, 0.99))
quantile(candidate_pairs$Jaro_Winkler_Similarity, probs = c(0.5, 0.95, 0.99))
quantile(candidate_pairs$NGram_Similarity, probs = c(0.5, 0.95, 0.99))
quantile(candidate_pairs$Masala_Similarity, probs = c(0.5, 0.95, 0.99))
quantile(candidate_pairs$Metaphone_Similarity, probs = c(0.5, 0.95, 0.99))

random_sample <- candidate_pairs %>%
  filter(Pair_Type == "main-minor") %>%
  mutate(
    # percentile bins for each score (example with 5 bins)
    score1_bin = cut(Levenshtein_Similarity, 
                     breaks = quantile(Levenshtein_Similarity, 
                                       probs = c(0, 0.5, 0.99, 1), 
                                       na.rm = TRUE), 
                     labels = c("Low", "Medium", "Very High"), 
                     include.lowest = TRUE),
    score2_bin = cut(Jaro_Winkler_Similarity, 
                     breaks = quantile(Jaro_Winkler_Similarity, 
                                       probs = c(0, 0.5, 0.99, 1), 
                                       na.rm = TRUE), 
                     labels = c("Low", "Medium", "Very High"), 
                     include.lowest = TRUE),
    score3_bin = factor(Metaphone_Similarity)
  )

high_scores_sample <- random_sample %>%
  filter(score1_bin == "Very High" & 
           score2_bin == "Very High" & 
           Metaphone_Similarity == 1) %>%
  slice_sample(n = 10, replace = FALSE) %>%
  mutate(source = "All High Scores")

low_lv_high_jw_scores_sample <- random_sample %>%
  filter(score1_bin == "Medium" & 
           score2_bin == "Very High") %>%
  slice_sample(n = 10, replace = FALSE) %>%
  mutate(source = "Low Levenshtein, High Jaro-Winkler")

low_jw_high_lv_scores_sample <- random_sample %>%
  filter(score1_bin == "Very High" & 
           score2_bin == "Medium") %>%
  slice_sample(n = 10, replace = FALSE) %>%
  mutate(source = "High Levenshtein, Low Jaro-Winkler")

# get other samples
set.seed(123)
stratified_sample <- random_sample %>%
  anti_join(high_scores_sample, by = c("Year", "State_Name", "Constituency_Name", 
                                       "Candidate1_Name", "Candidate2_Name")) %>%
  anti_join(low_lv_high_jw_scores_sample, by = c("Year", "State_Name", "Constituency_Name", 
                                       "Candidate1_Name", "Candidate2_Name")) %>%
  anti_join(low_jw_high_lv_scores_sample, by = c("Year", "State_Name", "Constituency_Name", 
                                       "Candidate1_Name", "Candidate2_Name")) %>%
  group_by(score1_bin, score2_bin, score3_bin) %>%
  slice_sample(n = 1, replace = TRUE) %>%
  ungroup() %>%
  slice_sample(n = 20)

final_sample <- bind_rows(
  high_scores_sample,
  low_lv_high_jw_scores_sample,
  low_jw_high_lv_scores_sample,
  stratified_sample
) %>%
  dplyr::select(Year, State_Name, Constituency_Name, Election_Type, Assembly_No, 
                Candidate1_Name, Candidate2_Name, Levenshtein_Similarity, 
                Jaro_Winkler_Similarity, Metaphone_Similarity,
                score1_bin, score2_bin, score3_bin, source) %>%
  rename(
    LV_bin = score1_bin,
    JW_bin = score2_bin, 
    Metaphone_bin = score3_bin
  ) %>%
  arrange(desc(Metaphone_Similarity), desc(Levenshtein_Similarity))

final_sample %>%
  gt() %>%
  # color Levenshtein similarity cells
  data_color(
    columns = Levenshtein_Similarity,
    fn = scales::col_numeric(
      palette = c("pink", "yellow", "lightgreen"),
      domain = c(0, 1)
    )
  ) %>%
  # color Jaro-Winkler similarity cells
  data_color(
    columns = Jaro_Winkler_Similarity,
    fn = scales::col_numeric(
      palette = c("pink", "yellow", "lightgreen"),
      domain = c(0, 1)
    )
  ) %>%
  # color Metaphone similarity cells
  data_color(
    columns = Metaphone_Similarity,
    fn = scales::col_bin(
      palette = c("pink", "lightgreen"),
      domain = c(0, 1),
      bins = 2
    )
  )

### How do we identify decoys testing!
candidate_pairs_above95th <- candidate_pairs %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main") %>%
  mutate(
    exceeds_threshold1 = Levenshtein_Similarity > quantile(Levenshtein_Similarity, 0.95, na.rm = TRUE),
    exceeds_threshold2 = Jaro_Winkler_Similarity > quantile(Jaro_Winkler_Similarity, 0.95, na.rm = TRUE),
    exceeds_threshold3 = NGram_Similarity > quantile(NGram_Similarity, 0.95, na.rm = TRUE),
    exceeds_threshold4 = Masala_Similarity > quantile(Masala_Similarity, 0.95, na.rm = TRUE), 
    exceeds_threshold5 = Metaphone_Similarity > quantile(Metaphone_Similarity, 0.95, na.rm = TRUE)
  ) %>%
  filter(exceeds_threshold1 & exceeds_threshold2 & exceeds_threshold3 & exceeds_threshold4 & exceeds_threshold5) %>%
  dplyr::select(-starts_with("exceeds_threshold"))

unique_main_95 <- unique(candidate_pairs_above95th$Candidate1_Name)

candidate_pairs_above99th <- candidate_pairs %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main") %>%
  mutate(
    exceeds_threshold1 = Levenshtein_Similarity > quantile(Levenshtein_Similarity, 0.99, na.rm = TRUE),
    exceeds_threshold2 = Jaro_Winkler_Similarity > quantile(Jaro_Winkler_Similarity, 0.99, na.rm = TRUE),
    exceeds_threshold3 = NGram_Similarity > quantile(NGram_Similarity, 0.99, na.rm = TRUE),
    exceeds_threshold4 = Masala_Similarity > quantile(Masala_Similarity, 0.99, na.rm = TRUE), 
    exceeds_threshold5 = Metaphone_Similarity > quantile(Metaphone_Similarity, 0.99, na.rm = TRUE)
  ) %>%
  filter(exceeds_threshold1 & exceeds_threshold2 & exceeds_threshold3 & exceeds_threshold4 & exceeds_threshold5) %>%
  dplyr::select(-starts_with("exceeds_threshold"))

test <- candidate_pairs %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main")

candidate_pairs_above99th <- candidate_pairs %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main") %>%
  mutate(
    exceeds_threshold1 = Levenshtein_Similarity > quantile(Levenshtein_Similarity, 0.99, na.rm = TRUE),
    exceeds_threshold2 = Jaro_Winkler_Similarity > quantile(Jaro_Winkler_Similarity, 0.99, na.rm = TRUE),
    exceeds_threshold3 = NGram_Similarity > quantile(NGram_Similarity, 0.99, na.rm = TRUE),
    exceeds_threshold4 = Masala_Similarity > quantile(Masala_Similarity, 0.99, na.rm = TRUE), 
    exceeds_threshold5 = Metaphone_Similarity > quantile(Metaphone_Similarity, 0.99, na.rm = TRUE)
  ) %>%
  filter(exceeds_threshold1 & exceeds_threshold2 & exceeds_threshold3 & exceeds_threshold4 & exceeds_threshold5) %>%
  dplyr::select(-starts_with("exceeds_threshold"))

test <- candidate_pairs %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main")

unique_main_99 <- unique(candidate_pairs_above99th$Candidate1_Name)
multiple_decoy <- candidate_pairs_above99th$Candidate1_Name[duplicated(candidate_pairs_above99th$Candidate1_Name) | duplicated(candidate_pairs_above99th$Candidate1_Name, fromLast = TRUE)]
multiple_decoy_rows <- candidate_pairs_above99th[candidate_pairs_above99th$Candidate1_Name %in% multiple_decoy, ]

candidate_with_decoy_99 <- data.frame(CandidateName = unique_main_99)

### What is each measure good at? 
candidate_pairs_lv_good <- candidate_pairs %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main") %>%
  mutate(
    exceeds_threshold1 = Levenshtein_Similarity > quantile(Levenshtein_Similarity, 0.99, na.rm = TRUE),
    exceeds_threshold2 = Jaro_Winkler_Similarity < quantile(Jaro_Winkler_Similarity, 0.99, na.rm = TRUE),
    exceeds_threshold3 = NGram_Similarity < quantile(NGram_Similarity, 0.99, na.rm = TRUE),
    exceeds_threshold4 = Masala_Similarity < quantile(Masala_Similarity, 0.99, na.rm = TRUE), 
    exceeds_threshold5 = Metaphone_Similarity < quantile(Metaphone_Similarity, 0.99, na.rm = TRUE)
  ) %>%
  filter(exceeds_threshold1 & exceeds_threshold2 & exceeds_threshold3 & exceeds_threshold4 & exceeds_threshold5) %>%
  dplyr::select(-starts_with("exceeds_threshold"))

# LV catches common strings well. This is bad when we have same last name type situations. 

candidate_pairs_jw_good <- candidate_pairs %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main") %>%
  mutate(
    exceeds_threshold1 = Levenshtein_Similarity < quantile(Levenshtein_Similarity, 0.99, na.rm = TRUE),
    exceeds_threshold2 = Jaro_Winkler_Similarity > quantile(Jaro_Winkler_Similarity, 0.99, na.rm = TRUE),
    exceeds_threshold3 = NGram_Similarity < quantile(NGram_Similarity, 0.99, na.rm = TRUE),
    exceeds_threshold4 = Masala_Similarity < quantile(Masala_Similarity, 0.99, na.rm = TRUE), 
    exceeds_threshold5 = Metaphone_Similarity < quantile(Metaphone_Similarity, 0.99, na.rm = TRUE)
  ) %>%
  filter(exceeds_threshold1 & exceeds_threshold2 & exceeds_threshold3 & exceeds_threshold4 & exceeds_threshold5) %>%
  dplyr::select(-starts_with("exceeds_threshold"))

# JW catches common strings well also. But, the good thing that it weighs starting string more heavily. 
# This is good if we assume that candidates are trying to trick people by just changing their middle name. 

candidate_pairs_ng_good <- candidate_pairs %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main") %>%
  mutate(
    exceeds_threshold1 = Levenshtein_Similarity < quantile(Levenshtein_Similarity, 0.99, na.rm = TRUE),
    exceeds_threshold2 = Jaro_Winkler_Similarity < quantile(Jaro_Winkler_Similarity, 0.99, na.rm = TRUE),
    exceeds_threshold3 = NGram_Similarity > quantile(NGram_Similarity, 0.99, na.rm = TRUE),
    exceeds_threshold4 = Masala_Similarity < quantile(Masala_Similarity, 0.99, na.rm = TRUE), 
    exceeds_threshold5 = Metaphone_Similarity < quantile(Metaphone_Similarity, 0.99, na.rm = TRUE)
  ) %>%
  filter(exceeds_threshold1 & exceeds_threshold2 & exceeds_threshold3 & exceeds_threshold4 & exceeds_threshold5) %>%
  dplyr::select(-starts_with("exceeds_threshold"))

# Okay, N-Gram is pretty shit. No saving grace I think. This is especially because Indian names are long as fuck. 

candidate_pairs_masala_good <- candidate_pairs %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main") %>%
  mutate(
    exceeds_threshold1 = Levenshtein_Similarity < quantile(Levenshtein_Similarity, 0.99, na.rm = TRUE),
    exceeds_threshold2 = Jaro_Winkler_Similarity < quantile(Jaro_Winkler_Similarity, 0.99, na.rm = TRUE),
    exceeds_threshold3 = NGram_Similarity < quantile(NGram_Similarity, 0.99, na.rm = TRUE),
    exceeds_threshold4 = Masala_Similarity > quantile(Masala_Similarity, 0.99, na.rm = TRUE), 
    exceeds_threshold5 = Metaphone_Similarity < quantile(Metaphone_Similarity, 0.99, na.rm = TRUE)
  ) %>%
  filter(exceeds_threshold1 & exceeds_threshold2 & exceeds_threshold3 & exceeds_threshold4 & exceeds_threshold5) %>%
  dplyr::select(-starts_with("exceeds_threshold"))

# Terrible! 

candidate_pairs_dblmet_good <- candidate_pairs %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main") %>%
  mutate(
    exceeds_threshold1 = Levenshtein_Similarity < quantile(Levenshtein_Similarity, 0.99, na.rm = TRUE),
    exceeds_threshold2 = Jaro_Winkler_Similarity < quantile(Jaro_Winkler_Similarity, 0.99, na.rm = TRUE),
    exceeds_threshold3 = NGram_Similarity < quantile(NGram_Similarity, 0.99, na.rm = TRUE),
    exceeds_threshold4 = Masala_Similarity < quantile(Masala_Similarity, 0.99, na.rm = TRUE), 
    exceeds_threshold5 = Metaphone_Similarity > quantile(Metaphone_Similarity, 0.99, na.rm = TRUE)
  ) %>%
  filter(exceeds_threshold1 & exceeds_threshold2 & exceeds_threshold3 & exceeds_threshold4 & exceeds_threshold5) %>%
  dplyr::select(-starts_with("exceeds_threshold"))

# This is definitely a good sanity check. But, the result is weighed towards first name. 

### Saving
write.csv(final_sample, "Cleaned Data/candidate_pairs_random_sample_lv_jw_dblmet.csv")

# Testing LLM
test <- final_sample %>%
  dplyr::select(Candidate1_Name, Candidate2_Name)

write.csv(test, "Raw Data/test_LLM.csv")

### For Survey
lv_99th <- quantile(candidate_pairs$Levenshtein_Similarity, 0.99, na.rm = TRUE)
jw_99th <- quantile(candidate_pairs$Jaro_Winkler_Similarity, 0.99, na.rm = TRUE)
mp_99th <- quantile(candidate_pairs$Metaphone_Similarity, 0.99, na.rm = TRUE)

lv_95th <- quantile(candidate_pairs$Levenshtein_Similarity, 0.95, na.rm = TRUE)
jw_95th <- quantile(candidate_pairs$Jaro_Winkler_Similarity, 0.95, na.rm = TRUE)
mp_95th <- quantile(candidate_pairs$Metaphone_Similarity, 0.95, na.rm = TRUE)

candidate_pairs_above99th_lv_jw_dblmet <- candidate_pairs %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main") %>%
  filter(
    Levenshtein_Similarity > lv_99th,
    Jaro_Winkler_Similarity > jw_99th,
    Metaphone_Similarity > mp_99th
  )

candidate_pairs_above99th_lv_jw_not_dblmet <- candidate_pairs %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main") %>%
  filter(
    Levenshtein_Similarity > lv_99th,
    Jaro_Winkler_Similarity > jw_99th,
    Metaphone_Similarity < mp_99th, 
    Metaphone_Similarity > mp_95th, 
  )

candidate_pairs_above99th_lv_not_jw_dblmet <- candidate_pairs %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main") %>%
  filter(
    Levenshtein_Similarity > lv_99th,
    Jaro_Winkler_Similarity < jw_99th,
    Jaro_Winkler_Similarity > jw_95th,
    Metaphone_Similarity > mp_99th
  )

candidate_pairs_above99th_not_lv_jw_dblmet <- candidate_pairs %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main") %>%
  filter(
    Levenshtein_Similarity < lv_99th,
    Levenshtein_Similarity > lv_95th,
    Jaro_Winkler_Similarity > jw_99th,
    Metaphone_Similarity > mp_99th
  )

candidate_pairs_above99th_lv_not_jw_not_dblmet <- candidate_pairs %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main") %>%
  filter(
    Levenshtein_Similarity > lv_99th,
    Jaro_Winkler_Similarity < jw_99th,
    Jaro_Winkler_Similarity > jw_95th,
    Metaphone_Similarity < mp_99th, 
    Metaphone_Similarity > mp_95th
  )

candidate_pairs_above99th_not_lv_jw_not_dblmet <- candidate_pairs %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main") %>%
  filter(
    Levenshtein_Similarity < lv_99th,
    Levenshtein_Similarity > lv_95th,
    Jaro_Winkler_Similarity > jw_99th,
    Metaphone_Similarity < mp_99th, 
    Metaphone_Similarity > mp_95th
  )

candidate_pairs_above99th_not_lv_not_jw_dblmet <- candidate_pairs %>%
  filter(Pair_Type == "main-minor" | Pair_Type == "minor-main") %>%
  filter(
    Levenshtein_Similarity < lv_99th,
    Levenshtein_Similarity > lv_95th,
    Jaro_Winkler_Similarity < jw_99th,
    Jaro_Winkler_Similarity > jw_95th,
    Metaphone_Similarity > mp_99th
  )

candidate_pairs_above99th_lv_jw_dblmet <- candidate_pairs_above99th_lv_jw_dblmet %>%
  mutate(Similar_Acc_To = "Levenshtein, Jaro-Winkler, Double Metaphone")

candidate_pairs_above99th_lv_jw_not_dblmet <- candidate_pairs_above99th_lv_jw_not_dblmet %>%
  mutate(Similar_Acc_To = "Levenshtein, Jaro-Winkler")

candidate_pairs_above99th_lv_not_jw_dblmet <- candidate_pairs_above99th_lv_not_jw_dblmet %>%
  mutate(Similar_Acc_To = "Levenshtein, Double Metaphone")

candidate_pairs_above99th_not_lv_jw_dblmet <- candidate_pairs_above99th_not_lv_jw_dblmet %>%
  mutate(Similar_Acc_To = "Jaro-Winkler, Double Metaphone")

candidate_pairs_above99th_lv_not_jw_not_dblmet <- candidate_pairs_above99th_lv_not_jw_not_dblmet %>%
  mutate(Similar_Acc_To = "Levenshtein")

candidate_pairs_above99th_not_lv_jw_not_dblmet <- candidate_pairs_above99th_not_lv_jw_not_dblmet %>%
  mutate(Similar_Acc_To = "Jaro-Winkler")

candidate_pairs_above99th_not_lv_not_jw_dblmet <- candidate_pairs_above99th_not_lv_not_jw_dblmet %>%
  mutate(Similar_Acc_To = "Double Metaphone")

possible_decoys_different_levels <- bind_rows(
  candidate_pairs_above99th_lv_jw_dblmet,
  candidate_pairs_above99th_lv_jw_not_dblmet,
  candidate_pairs_above99th_lv_not_jw_dblmet,
  candidate_pairs_above99th_not_lv_jw_dblmet,
  candidate_pairs_above99th_lv_not_jw_not_dblmet, 
  candidate_pairs_above99th_not_lv_jw_not_dblmet, 
  candidate_pairs_above99th_not_lv_not_jw_dblmet
)

possible_decoys_different_levels <- possible_decoys_different_levels %>%
  dplyr::select(Candidate1_Candidate, Candidate2_Candidate, Similar_Acc_To) %>%
  rename(Name_1 = Candidate1_Candidate, 
         Name_2 = Candidate2_Candidate) %>%
  distinct(Name_1, Name_2, .keep_all = TRUE)

above99th_for_lv_jw_dblmet <- candidate_pairs_above99th_lv_jw_dblmet %>%
  dplyr::select(Candidate1_Candidate, Candidate2_Candidate) %>%
  rename(Name_1 = Candidate1_Candidate, 
         Name_2 = Candidate2_Candidate) %>%
  distinct(Name_1, Name_2, .keep_all = TRUE)

write.csv(above99th_for_lv_jw_dblmet, "Cleaned Data/potential_decoys_above99th_for_all.csv")
write.csv(possible_decoys_different_levels, "Cleaned Data/potential_decoys_different_levels.csv")

### Margin Notes
# other ways to color tables
datatable(final_sample) %>%
  # Color Levenshtein similarity
  formatStyle(
    'Levenshtein_Similarity',
    background = styleInterval(c(0.5, 0.8), c('#ffcccc', '#ffff99', '#ccffcc'))
  ) %>%
  # Color Jaro-Winkler similarity
  formatStyle(
    'Jaro_Winkler_Similarity',
    background = styleInterval(c(0.7, 0.9), c('#ffcccc', '#ffff99', '#ccffcc'))
  ) %>%
  formatStyle(
    'Metaphone_Similarity',
    background = styleInterval(c(0, 1), c('#ffcccc', '#ccffcc'))
  )

final_sample %>%
  kbl() %>%
  kable_styling() %>%
  # Color cells based on LV bins
  column_spec(
    column = which(colnames(final_sample) == "Levenshtein_Similarity"),
    background = case_when(
      final_sample$Levenshtein_Similarity > 0.8 ~ "#ccffcc",
      final_sample$Levenshtein_Similarity > 0.5 ~ "#ffff99",
      TRUE ~ "#ffcccc"
    )
  ) %>%
  # Color cells based on JW bins
  column_spec(
    column = which(colnames(final_sample) == "Jaro_Winkler_Similarity"),
    background = case_when(
      final_sample$Jaro_Winkler_Similarity > 0.9 ~ "#ccffcc",
      final_sample$Jaro_Winkler_Similarity > 0.7 ~ "#ffff99",
      TRUE ~ "#ffcccc"
    )
  )