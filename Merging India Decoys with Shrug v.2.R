# ============================================
# Political Decoys - Merging with Shrug v.2
# ============================================
# Date: 20/05/25
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
  purrr, 
  haven
)

py_run_string('
import sys
import os
sys.path.append(os.path.abspath("masala-merge-master/"))
from lev import levenshtein
')

standardize_state_names <- function(state_name) { 
  # Convert state name to lowercase and trim whitespace
  state_name <- trimws(tolower(state_name)) 
  
  # Dictionary mapping variations to standard names from pol_outcomes_no_pre_2011
  state_dict <- list(
    "andhra pradesh" = c("andhara pradesh", "ap", "andhra", "andhra pradesh state", "telangana", "andhra_pradesh"),
    "arunachal pradesh" = c("arunachal", "arunachal pradesh state", "arunanchal pradesh", "arunachal_pradesh"),
    "assam" = c("assam"),
    "bihar" = c("bihar"),
    "chhattisgarh" = c("chattisgarh", "chhattisgarh state", "chhattisgarh"),
    "delhi" = c("delhi", "delhi state"),
    "goa" = c("goa", "goa state"),
    "gujarat" = c("gujarat", "gujrath", "gujarat state", "guj"),
    "haryana" = c("haryana"),
    "himachal pradesh" = c("himachal pradesh", "hp", "himachal pradesh state", "himachal_pradesh"),
    "jammu kashmir" = c("jammu and kashmir", "j&k", "j & k", "jk", "jammu & kashmir", "jammu and kashmir", "jammu_kashmir", "jammu_&_kashmir"),
    "jharkhand" = c("jharkhand", "jharkhand state"),
    "karnataka" = c("karnatka", "karnataka state", "karnataka"),
    "kerala" = c("kerala", "kerala state"),
    "madhya pradesh" = c("mp", "m.p.", "madhya pradesh state", "madhya_pradesh"),
    "maharashtra" = c("maharashtra"),
    "manipur" = c("manipur"),
    "meghalaya" = c("meghalaya", "meghalaya state"),
    "mizoram" = c("mizoram", "mizoram state"),
    "nagaland" = c("nagaland", "nagaland state"),
    "orissa" = c("odisha", "orissa state", "orissa"),
    "pondicherry" = c("puducherry", "pondicherry state", "pondicherry"),
    "punjab" = c("punjab", "pnjb", "punjab state", "pnb"),
    "rajasthan" = c("rajasthan", "rajsthan", "rajsthan state", "raj"),
    "sikkim" = c("sikkim"),
    "tamil nadu" = c("tamilnadu", "tamil nadu state", "tn", "tamil_nadu"),
    "tripura" = c("tripura", "tripura state"),
    "uttar pradesh" = c("up", "u.p.", "uttar pradesh state", "uttar_pradesh"),
    "uttarakhand" = c("uttarkhand", "uttaranchal", "uttrakhand", "uttarakhand state", "uttarakhand"),
    "west bengal" = c("wb", "w.b.", "westbengal", "west_bengal")
  ) 
  
  # Search for a match in the dictionary
  for (standard_name in names(state_dict)) { 
    if (state_name %in% state_dict[[standard_name]] || state_name == standard_name) { 
      return(standard_name) # Return lowercase to match pol_outcomes_no_pre_2011 format
    } 
  } 
  
  # Return original if no match found
  return(state_name)
}

#### Loading ####
urban_rural_control <- read_csv("/Users/anirvin/Downloads/Corruption Protest Data/Cleaned Data/urban_rural_control_ac08.csv")
constituency_metrics <- read_csv("Cleaned Data/constituency_metrics_crosswalk.csv")
constituency_metrics_post_delim <- read_csv("Cleaned Data/constituency_metrics_post_delim.csv")
constituency_metrics_pre_delim <- read_csv("Cleaned Data/constituency_metrics_pre_delim.csv")

constituency_election_metrics <- read_csv("Cleaned Data/constituency_election_photo.csv")

constituency_metrics_post_delim <- constituency_metrics_post_delim %>%
  rename(ac08_id = ac_id)

constituency_metrics_pre_delim <- constituency_metrics_pre_delim %>%
  rename(ac07_id = ac_id)

constituency_metrics <- constituency_metrics %>%
  rename(ac08_id = ac_id)

shrid_07 <- read_csv("Raw Data/Shrug Data/shrug-con-keys-csv/shrid_frag_con07_key.csv")
urban_shrids <- read_csv("Raw Data/Shrug Data/secc/shrug-secc-parsed-urban-csv/secc_urban_shrid.csv")
rural_shrids <- read_csv("Raw Data/Shrug Data/secc/shrug-secc-mord-rural-csv/secc_rural_shrid.csv")

#### Merging with Shrug ####
# creating urban rural control for 07
shrid_07_with_flags <- shrid_07 %>%
  mutate(
    is_rural = shrid2 %in% rural_shrids$shrid2,
    is_urban = shrid2 %in% urban_shrids$shrid2
  ) %>%
  filter(is_rural != is_urban)

urban_rural_control_07 <- shrid_07_with_flags %>%
  group_by(ac07_id) %>%
  summarise(
    total_shrids = n_distinct(shrid2),
    n_rural = n_distinct(shrid2[is_rural]),
    n_urban = n_distinct(shrid2[is_urban])
  ) %>%
  mutate(
    proportion_rural = n_rural / total_shrids,
    proportion_urban = n_urban / total_shrids
  ) %>%
  dplyr::select(ac07_id, proportion_urban, proportion_rural)

#### Post Delim ####
# 1. Ruggedness
rug <- read.csv("Raw Data/Shrug Data/elevation ruggedness/shrug-rugged-csv/tri_con08.csv")

ac_rug <- rug %>%
  dplyr::group_by(ac08_id) %>%
  dplyr::summarize(
    ruggedness_mean = mean(tri_mean, na.rm = TRUE),
    ruggedness_median = mean(tri_median, na.rm = TRUE),
    ruggedness_min = min(tri_min, na.rm = TRUE),
    ruggedness_max = max(tri_max, na.rm = TRUE),
    ruggedness_std = mean(tri_std, na.rm = TRUE)
  )

constituency_metrics_post_delim <- constituency_metrics_post_delim %>%
  left_join(ac_rug %>% 
              dplyr::select(ac08_id, ruggedness_mean),
            by = "ac08_id")

sum(is.na(constituency_metrics_post_delim$ruggedness_mean))

# 2. Elevation
elev <- read.csv("Raw Data/Shrug Data/elevation ruggedness/shrug-elevation-csv/elevation_con08.csv")

ac_elev <- elev %>%
  dplyr::group_by(ac08_id) %>%
  dplyr::summarize(
    elevation_mean = if(all(is.na(elevation_mean))) NA_real_ else mean(elevation_mean, na.rm = TRUE),
    elevation_median = if(all(is.na(elevation_median))) NA_real_ else mean(elevation_median, na.rm = TRUE),
    elevation_max = if(all(is.na(elevation_max))) NA_real_ else mean(elevation_max, na.rm = TRUE),
    elevation_min = if(all(is.na(elevation_min))) NA_real_ else mean(elevation_min, na.rm = TRUE),
    elevation_std = if(all(is.na(elevation_std))) NA_real_ else mean(elevation_std, na.rm = TRUE),
    .groups = "drop"
  )

constituency_metrics_post_delim <- constituency_metrics_post_delim %>%
  left_join(ac_elev %>% 
              dplyr::select(ac08_id, elevation_mean),
            by = "ac08_id")

sum(is.na(constituency_metrics_post_delim$elevation_mean))

# 3. Land Area
popwt <- read.csv("Raw Data/Shrug Data/pop weight/shrug-con08-wt-csv/con08_pop_area_key.csv")

ac_popwt <- popwt %>%
  group_by(ac08_id) %>%
  dplyr::summarize(
    land_area = sum(con08_land_area, na.rm = TRUE)
  )

constituency_metrics_post_delim <- constituency_metrics_post_delim %>%
  left_join(ac_popwt %>% 
              dplyr::select(ac08_id, land_area),
            by = "ac08_id")

sum(is.na(constituency_metrics_post_delim$land_area))

# 4. SC/ST Population, Literacy Rate
pc11 <- read.csv("Raw Data/Shrug Data/pc/shrug-pca11-csv/pc11_pca_clean_con08.csv")

ac_pc11 <- pc11 %>%
  group_by(ac08_id) %>%
  dplyr::summarize(
    pc11_pca_p_lit = sum(pc11_pca_p_lit, na.rm = TRUE),
    pc11_pca_main_al_p = sum(pc11_pca_main_al_p, na.rm = TRUE), 
    pc11_pca_tot_p = sum(pc11_pca_tot_p, na.rm = TRUE),
    pc11_pca_m_sc = sum(pc11_pca_m_sc, na.rm = TRUE), 
    pc11_pca_f_sc = sum(pc11_pca_f_sc, na.rm = TRUE), 
    pc11_pca_m_st = sum(pc11_pca_m_st, na.rm = TRUE), 
    pc11_pca_f_st = sum(pc11_pca_f_st, na.rm = TRUE)
  )

constituency_metrics_post_delim <- constituency_metrics_post_delim %>%
  left_join(ac_pc11 %>% 
              dplyr::select(ac08_id, pc11_pca_p_lit, pc11_pca_main_al_p, pc11_pca_tot_p, 
                            pc11_pca_m_sc, pc11_pca_f_sc, pc11_pca_m_st, pc11_pca_f_st),
            by = "ac08_id")

sum(is.na(constituency_metrics_post_delim$pc11_pca_p_lit))

# 5. Rural Urban
constituency_metrics_post_delim <- constituency_metrics_post_delim %>%
  left_join(urban_rural_control, 
            by = "ac08_id")

# 6. Employment, Firms
ec13 <- read.csv("Raw Data/Shrug Data/ec/shrug-ec13-csv/ec13_con08.csv")

ec13 <- ec13 %>%
  dplyr::select(ac08_id, ec13_emp_all, ec13_count_all, ec13_emp_manuf, ec13_emp_services)

ac_ec13 <- ec13 %>%
  group_by(ac08_id) %>%
  dplyr::summarize(
    ec13_emp_all = sum(ec13_emp_all, na.rm = TRUE),
    ec13_count_all = sum(ec13_count_all, na.rm = TRUE),
    ec13_emp_manuf = sum(ec13_emp_manuf, na.rm = TRUE),
    ec13_emp_services = sum(ec13_emp_services, na.rm = TRUE)
  )

constituency_metrics_post_delim <- constituency_metrics_post_delim %>%
  left_join(ac_ec13 %>% 
              dplyr::select(ac08_id, ec13_emp_all, ec13_count_all, ec13_emp_manuf, ec13_emp_services),
            by = "ac08_id")

sum(is.na(constituency_metrics_post_delim$ec13_emp_all))

# 7. Nightlights
dmsp <- read.csv("Raw Data/Shrug Data/night lights/shrug-dmsp-csv/dmsp_con08.csv")

ac_dmsp <- dmsp %>%
  filter(year == 2011) %>%
  dplyr::group_by(ac08_id) %>%
  dplyr::summarize(
    dmsp_total_light = if(all(is.na(dmsp_total_light))) NA_real_ else mean(dmsp_total_light, na.rm = TRUE),
    dmsp_total_light_cal = if(all(is.na(dmsp_total_light_cal))) NA_real_ else sum(dmsp_total_light_cal, na.rm = TRUE),
    dmsp_mean_light = if(all(is.na(dmsp_mean_light))) NA_real_ else sum(dmsp_mean_light, na.rm = TRUE),
    dmsp_mean_light_cal = if(all(is.na(dmsp_mean_light_cal))) NA_real_ else sum(dmsp_mean_light_cal, na.rm = TRUE),
    dmsp_num_cells = if(all(is.na(dmsp_num_cells))) NA_real_ else sum(dmsp_num_cells, na.rm = TRUE),
    .groups = "drop"
  )

constituency_metrics_post_delim <- constituency_metrics_post_delim %>%
  left_join(ac_dmsp %>% 
              dplyr::select(ac08_id, dmsp_total_light_cal, dmsp_mean_light_cal),
            by = "ac08_id")

sum(is.na(constituency_metrics_post_delim$dmsp_total_light_cal))

# 8. Dispensaries, Primary Schools, Healthcare, Power Supply
  # can think about adding educ 
td11 <- read.csv("Raw Data/Shrug Data/pc/shrug-td11-csv/pc11_td_clean_con08.csv")

ac_td11 <- td11 %>%
  group_by(ac08_id) %>%
  dplyr::summarize(
    pc11_td_health_ctr = sum(pc11_td_disp, na.rm = TRUE),
    pc11_td_primary = sum(pc11_td_primary_gov, na.rm = TRUE),
    pc11_td_el_dom = sum(pc11_td_el_dom, na.rm = TRUE)
  )

constituency_metrics_post_delim <- constituency_metrics_post_delim %>%
  left_join(ac_td11 %>% 
              dplyr::select(ac08_id, pc11_td_health_ctr, pc11_td_primary, pc11_td_el_dom),
            by = "ac08_id")

sum(is.na(constituency_metrics_post_delim$pc11_td_health_ctr))

#### Pre Delim ####
# 1. Ruggedness
rug <- read.csv("Raw Data/Shrug Data/elevation ruggedness/shrug-rugged-csv/tri_con07.csv")

ac_rug <- rug %>%
  dplyr::group_by(ac07_id) %>%
  dplyr::summarize(
    ruggedness_mean = mean(tri_mean, na.rm = TRUE),
    ruggedness_median = mean(tri_median, na.rm = TRUE),
    ruggedness_min = min(tri_min, na.rm = TRUE),
    ruggedness_max = max(tri_max, na.rm = TRUE),
    ruggedness_std = mean(tri_std, na.rm = TRUE)
  )

constituency_metrics_pre_delim <- constituency_metrics_pre_delim %>%
  left_join(ac_rug %>% 
              dplyr::select(ac07_id, ruggedness_mean),
            by = "ac07_id")

sum(is.na(constituency_metrics_pre_delim$ruggedness_mean))

# 2. Elevation
elev <- read.csv("Raw Data/Shrug Data/elevation ruggedness/shrug-elevation-csv/elevation_con07.csv")

ac_elev <- elev %>%
  dplyr::group_by(ac07_id) %>%
  dplyr::summarize(
    elevation_mean = if(all(is.na(elevation_mean))) NA_real_ else mean(elevation_mean, na.rm = TRUE),
    elevation_median = if(all(is.na(elevation_median))) NA_real_ else mean(elevation_median, na.rm = TRUE),
    elevation_max = if(all(is.na(elevation_max))) NA_real_ else mean(elevation_max, na.rm = TRUE),
    elevation_min = if(all(is.na(elevation_min))) NA_real_ else mean(elevation_min, na.rm = TRUE),
    elevation_std = if(all(is.na(elevation_std))) NA_real_ else mean(elevation_std, na.rm = TRUE),
    .groups = "drop"
  )

constituency_metrics_pre_delim <- constituency_metrics_pre_delim %>%
  left_join(ac_elev %>% 
              dplyr::select(ac07_id, elevation_mean),
            by = "ac07_id")

sum(is.na(constituency_metrics_pre_delim$elevation_mean))

# 3. Land Area
popwt <- read.csv("Raw Data/Shrug Data/pop weight/shrug-con07-wt-csv/con07_pop_area_key.csv")

ac_popwt <- popwt %>%
  group_by(ac07_id) %>%
  dplyr::summarize(
    land_area = sum(con07_land_area, na.rm = TRUE)
  )

constituency_metrics_pre_delim <- constituency_metrics_pre_delim %>%
  left_join(ac_popwt %>% 
              dplyr::select(ac07_id, land_area),
            by = "ac07_id")

sum(is.na(constituency_metrics_pre_delim$land_area))

# 4. SC/ST Population, Literacy Rate
pc01 <- read.csv("Raw Data/Shrug Data/pc/shrug-pca01-csv/pc01_pca_clean_con07.csv")

ac_pc01 <- pc01 %>%
  group_by(ac07_id) %>%
  dplyr::summarize(
    pc01_pca_p_lit = sum(pc01_pca_p_lit, na.rm = TRUE),
    pc01_pca_main_al_p = sum(pc01_pca_main_al_p, na.rm = TRUE), 
    pc01_pca_tot_p = sum(pc01_pca_tot_p, na.rm = TRUE),
    pc01_pca_m_sc = sum(pc01_pca_m_sc, na.rm = TRUE), 
    pc01_pca_f_sc = sum(pc01_pca_f_sc, na.rm = TRUE), 
    pc01_pca_m_st = sum(pc01_pca_m_st, na.rm = TRUE), 
    pc01_pca_f_st = sum(pc01_pca_f_st, na.rm = TRUE)
  )

constituency_metrics_pre_delim <- constituency_metrics_pre_delim %>%
  left_join(ac_pc01 %>% 
              dplyr::select(ac07_id, pc01_pca_p_lit, pc01_pca_main_al_p, pc01_pca_tot_p, 
                            pc01_pca_m_sc, pc01_pca_f_sc, pc01_pca_m_st, pc01_pca_f_st),
            by = "ac07_id")

sum(is.na(constituency_metrics_pre_delim$pc01_pca_p_lit))

# 5. Rural Urban
constituency_metrics_pre_delim <- constituency_metrics_pre_delim %>%
  left_join(urban_rural_control_07, 
            by = "ac07_id")

# 6. Employment, Firms
ec05 <- read.csv("Raw Data/Shrug Data/ec/shrug-ec05-csv/ec05_con07.csv")

ec05 <- ec05 %>%
  dplyr::select(ac07_id, ec05_emp_all, ec05_count_all, ec05_emp_manuf, ec05_emp_services)

ac_ec05 <- ec05 %>%
  group_by(ac07_id) %>%
  dplyr::summarize(
    ec05_emp_all = sum(ec05_emp_all, na.rm = TRUE),
    ec05_count_all = sum(ec05_count_all, na.rm = TRUE),
    ec05_emp_manuf = sum(ec05_emp_manuf, na.rm = TRUE),
    ec05_emp_services = sum(ec05_emp_services, na.rm = TRUE)
  )

constituency_metrics_pre_delim <- constituency_metrics_pre_delim %>%
  left_join(ac_ec05 %>% 
              dplyr::select(ac07_id, ec05_emp_all, ec05_count_all, ec05_emp_manuf, ec05_emp_services),
            by = "ac07_id")

sum(is.na(constituency_metrics_pre_delim$ec05_emp_all))

# 7. Nightlights
dmsp <- read.csv("Raw Data/Shrug Data/night lights/shrug-dmsp-csv/dmsp_con07.csv")

ac_dmsp <- dmsp %>%
  filter(year == 2001) %>%
  dplyr::group_by(ac07_id) %>%
  dplyr::summarize(
    dmsp_total_light = if(all(is.na(dmsp_total_light))) NA_real_ else mean(dmsp_total_light, na.rm = TRUE),
    dmsp_total_light_cal = if(all(is.na(dmsp_total_light_cal))) NA_real_ else sum(dmsp_total_light_cal, na.rm = TRUE),
    dmsp_mean_light = if(all(is.na(dmsp_mean_light))) NA_real_ else sum(dmsp_mean_light, na.rm = TRUE),
    dmsp_mean_light_cal = if(all(is.na(dmsp_mean_light_cal))) NA_real_ else sum(dmsp_mean_light_cal, na.rm = TRUE),
    dmsp_num_cells = if(all(is.na(dmsp_num_cells))) NA_real_ else sum(dmsp_num_cells, na.rm = TRUE),
    .groups = "drop"
  )

constituency_metrics_pre_delim <- constituency_metrics_pre_delim %>%
  left_join(ac_dmsp %>% 
              dplyr::select(ac07_id, dmsp_total_light_cal, dmsp_mean_light_cal),
            by = "ac07_id")

sum(is.na(constituency_metrics_pre_delim$dmsp_total_light_cal))

# 8. Dispensaries, Primary Schools, Healthcare, Power Supply
td01 <- read.csv("Raw Data/Shrug Data/pc/shrug-td01-csv/pc01_td_clean_con07.csv")

ac_td01 <- td01 %>%
  group_by(ac07_id) %>%
  dplyr::summarize(
    pc01_td_health_ctr = sum(pc01_td_health_ctr, na.rm = TRUE),
    pc01_td_primary = sum(pc01_td_primary, na.rm = TRUE),
    pc01_td_el_dom = sum(pc01_td_el_dom, na.rm = TRUE)
  )

constituency_metrics_pre_delim <- constituency_metrics_pre_delim %>%
  left_join(ac_td01 %>% 
              dplyr::select(ac07_id, pc01_td_health_ctr, pc01_td_primary, pc01_td_el_dom),
            by = "ac07_id")

sum(is.na(constituency_metrics_pre_delim$pc01_td_health_ctr))

#### For Crosswalk ####
# 1. Ruggedness
rug <- read.csv("Raw Data/Shrug Data/elevation ruggedness/shrug-rugged-csv/tri_con08.csv")

ac_rug <- rug %>%
  dplyr::group_by(ac08_id) %>%
  dplyr::summarize(
    ruggedness_mean = mean(tri_mean, na.rm = TRUE),
    ruggedness_median = mean(tri_median, na.rm = TRUE),
    ruggedness_min = min(tri_min, na.rm = TRUE),
    ruggedness_max = max(tri_max, na.rm = TRUE),
    ruggedness_std = mean(tri_std, na.rm = TRUE)
  )

constituency_metrics <- constituency_metrics %>%
  left_join(ac_rug %>% 
              dplyr::select(ac08_id, ruggedness_mean),
            by = "ac08_id")

sum(is.na(constituency_metrics$ruggedness_mean))

# 2. Elevation
elev <- read.csv("Raw Data/Shrug Data/elevation ruggedness/shrug-elevation-csv/elevation_con08.csv")

ac_elev <- elev %>%
  dplyr::group_by(ac08_id) %>%
  dplyr::summarize(
    elevation_mean = if(all(is.na(elevation_mean))) NA_real_ else mean(elevation_mean, na.rm = TRUE),
    elevation_median = if(all(is.na(elevation_median))) NA_real_ else mean(elevation_median, na.rm = TRUE),
    elevation_max = if(all(is.na(elevation_max))) NA_real_ else mean(elevation_max, na.rm = TRUE),
    elevation_min = if(all(is.na(elevation_min))) NA_real_ else mean(elevation_min, na.rm = TRUE),
    elevation_std = if(all(is.na(elevation_std))) NA_real_ else mean(elevation_std, na.rm = TRUE),
    .groups = "drop"
  )

constituency_metrics <- constituency_metrics %>%
  left_join(ac_elev %>% 
              dplyr::select(ac08_id, elevation_mean),
            by = "ac08_id")

sum(is.na(constituency_metrics$elevation_mean))

# 3. Land Area
popwt <- read.csv("Raw Data/Shrug Data/pop weight/shrug-con08-wt-csv/con08_pop_area_key.csv")

ac_popwt <- popwt %>%
  group_by(ac08_id) %>%
  dplyr::summarize(
    land_area = sum(con08_land_area, na.rm = TRUE)
  )

constituency_metrics <- constituency_metrics %>%
  left_join(ac_popwt %>% 
              dplyr::select(ac08_id, land_area),
            by = "ac08_id")

sum(is.na(constituency_metrics$land_area))

# 4. SC/ST Population, Literacy Rate
pc11 <- read.csv("Raw Data/Shrug Data/pc/shrug-pca11-csv/pc11_pca_clean_con08.csv")

ac_pc11 <- pc11 %>%
  group_by(ac08_id) %>%
  dplyr::summarize(
    pc11_pca_p_lit = sum(pc11_pca_p_lit, na.rm = TRUE),
    pc11_pca_main_al_p = sum(pc11_pca_main_al_p, na.rm = TRUE), 
    pc11_pca_tot_p = sum(pc11_pca_tot_p, na.rm = TRUE),
    pc11_pca_m_sc = sum(pc11_pca_m_sc, na.rm = TRUE), 
    pc11_pca_f_sc = sum(pc11_pca_f_sc, na.rm = TRUE), 
    pc11_pca_m_st = sum(pc11_pca_m_st, na.rm = TRUE), 
    pc11_pca_f_st = sum(pc11_pca_f_st, na.rm = TRUE)
  )

constituency_metrics <- constituency_metrics %>%
  left_join(ac_pc11 %>% 
              dplyr::select(ac08_id, pc11_pca_p_lit, pc11_pca_main_al_p, pc11_pca_tot_p, 
                            pc11_pca_m_sc, pc11_pca_f_sc, pc11_pca_m_st, pc11_pca_f_st),
            by = "ac08_id")

sum(is.na(constituency_metrics$pc11_pca_p_lit))

# 5. Rural Urban
constituency_metrics <- constituency_metrics %>%
  left_join(urban_rural_control, 
            by = "ac08_id")

# 6. Employment, Firms
ec13 <- read.csv("Raw Data/Shrug Data/ec/shrug-ec13-csv/ec13_con08.csv")

ec13 <- ec13 %>%
  dplyr::select(ac08_id, ec13_emp_all, ec13_count_all, ec13_emp_manuf, ec13_emp_services)

ac_ec13 <- ec13 %>%
  group_by(ac08_id) %>%
  dplyr::summarize(
    ec13_emp_all = sum(ec13_emp_all, na.rm = TRUE),
    ec13_count_all = sum(ec13_count_all, na.rm = TRUE),
    ec13_emp_manuf = sum(ec13_emp_manuf, na.rm = TRUE),
    ec13_emp_services = sum(ec13_emp_services, na.rm = TRUE)
  )

constituency_metrics <- constituency_metrics %>%
  left_join(ac_ec13 %>% 
              dplyr::select(ac08_id, ec13_emp_all, ec13_count_all, ec13_emp_manuf, ec13_emp_services),
            by = "ac08_id")

sum(is.na(constituency_metrics$ec13_emp_all))

# 7. Nightlights
dmsp <- read.csv("Raw Data/Shrug Data/night lights/shrug-dmsp-csv/dmsp_con08.csv")

ac_dmsp <- dmsp %>%
  filter(year == 2011) %>%
  dplyr::group_by(ac08_id) %>%
  dplyr::summarize(
    dmsp_total_light = if(all(is.na(dmsp_total_light))) NA_real_ else mean(dmsp_total_light, na.rm = TRUE),
    dmsp_total_light_cal = if(all(is.na(dmsp_total_light_cal))) NA_real_ else sum(dmsp_total_light_cal, na.rm = TRUE),
    dmsp_mean_light = if(all(is.na(dmsp_mean_light))) NA_real_ else sum(dmsp_mean_light, na.rm = TRUE),
    dmsp_mean_light_cal = if(all(is.na(dmsp_mean_light_cal))) NA_real_ else sum(dmsp_mean_light_cal, na.rm = TRUE),
    dmsp_num_cells = if(all(is.na(dmsp_num_cells))) NA_real_ else sum(dmsp_num_cells, na.rm = TRUE),
    .groups = "drop"
  )

constituency_metrics <- constituency_metrics %>%
  left_join(ac_dmsp %>% 
              dplyr::select(ac08_id, dmsp_total_light_cal, dmsp_mean_light_cal),
            by = "ac08_id")

sum(is.na(constituency_metrics$dmsp_total_light_cal))

# 8. Dispensaries, Primary Schools, Healthcare, Power Supply
td11 <- read.csv("Raw Data/Shrug Data/pc/shrug-td11-csv/pc11_td_clean_con08.csv")

ac_td11 <- td11 %>%
  group_by(ac08_id) %>%
  dplyr::summarize(
    pc11_td_health_ctr = sum(pc11_td_disp, na.rm = TRUE),
    pc11_td_primary = sum(pc11_td_primary_gov, na.rm = TRUE),
    pc11_td_el_dom = sum(pc11_td_el_dom, na.rm = TRUE)
  )

constituency_metrics <- constituency_metrics %>%
  left_join(ac_td11 %>% 
              dplyr::select(ac08_id, pc11_td_health_ctr, pc11_td_primary, pc11_td_el_dom),
            by = "ac08_id")

sum(is.na(constituency_metrics$pc11_td_health_ctr))

#### Exporting to STATA ####
write_dta(constituency_metrics, 
          "STATA/Data/constituency_metrics_crosswalk.dta")
write_dta(constituency_metrics_post_delim, 
          "STATA/Data/constituency_metrics_post_delim.dta")
write_dta(constituency_metrics_pre_delim, 
          "STATA/Data/constituency_metrics_pre_delim.dta")
write_dta(constituency_election_metrics, 
          "STATA/Data/constituency_election_metrics.dta")

str(constituency_metrics)

# Count NA values in each column
na_counts <- sapply(constituency_metrics, function(x) sum(is.na(x)))

# For a more readable format with percentages
na_summary <- data.frame(
  column = names(na_counts),
  na_count = na_counts,
  na_percent = round(na_counts / nrow(constituency_metrics) * 100, 2)
)

#### Summary Stats ####
constituency_metrics <- constituency_metrics %>%
  mutate(sc_st_pct = (pc11_pca_m_sc + pc11_pca_f_sc + pc11_pca_m_st + pc11_pca_f_st)/pc11_pca_tot_p, 
         lit = pc11_pca_p_lit/pc11_pca_tot_p, 
         is_rural = ifelse((proportion_urban >= 0.5 | is.na(proportion_urban)), 1, 0))
         
str(constituency_metrics)

# Summary statistics for the specified variables
summary_stats <- constituency_metrics %>%
  summarise(
    # Dependent Variables
    mean_pct_elections_with_decoys = mean(pct_elections_with_decoys, na.rm = TRUE),
    sd_pct_elections_with_decoys = sd(pct_elections_with_decoys, na.rm = TRUE),
    var_pct_elections_with_decoys = var(pct_elections_with_decoys, na.rm = TRUE),
    mean_avg_decoy_share = mean(avg_decoy_share, na.rm = TRUE),
    sd_avg_decoy_share = sd(avg_decoy_share, na.rm = TRUE),
    var_avg_decoy_share = var(avg_decoy_share, na.rm = TRUE),
    mean_avg_decoy_vote_share = mean(avg_decoy_vote_share, na.rm = TRUE),
    sd_avg_decoy_vote_share = sd(avg_decoy_vote_share, na.rm = TRUE),
    var_avg_decoy_vote_share = var(avg_decoy_vote_share, na.rm = TRUE),
    
    # Regressors
    mean_sc_st_pct = mean(sc_st_pct, na.rm = TRUE),
    sd_sc_st_pct = sd(sc_st_pct, na.rm = TRUE),
    var_sc_st_pct = var(sc_st_pct, na.rm = TRUE),
    mean_is_rural = mean(is_rural, na.rm = TRUE),
    sd_is_rural = sd(is_rural, na.rm = TRUE),
    var_is_rural = var(is_rural, na.rm = TRUE),
    mean_lit = mean(lit, na.rm = TRUE),
    sd_lit = sd(lit, na.rm = TRUE),
    var_lit = var(lit, na.rm = TRUE),
    
    # Controls
    mean_ruggedness = mean(ruggedness_mean, na.rm = TRUE),
    sd_ruggedness = sd(ruggedness_mean, na.rm = TRUE),
    var_ruggedness = var(ruggedness_mean, na.rm = TRUE),
    mean_elevation = mean(elevation_mean, na.rm = TRUE),
    sd_elevation = sd(elevation_mean, na.rm = TRUE),
    var_elevation = var(elevation_mean, na.rm = TRUE),
    mean_land_area = mean(land_area, na.rm = TRUE),
    sd_land_area = sd(land_area, na.rm = TRUE),
    var_land_area = var(land_area, na.rm = TRUE),
    mean_emp_all = mean(ec13_emp_all, na.rm = TRUE),
    sd_emp_all = sd(ec13_emp_all, na.rm = TRUE),
    var_emp_all = var(ec13_emp_all, na.rm = TRUE),
    mean_count_all = mean(ec13_count_all, na.rm = TRUE),
    sd_count_all = sd(ec13_count_all, na.rm = TRUE),
    var_count_all = var(ec13_count_all, na.rm = TRUE),
    mean_light = mean(dmsp_mean_light_cal, na.rm = TRUE),
    sd_light = sd(dmsp_mean_light_cal, na.rm = TRUE),
    var_light = var(dmsp_mean_light_cal, na.rm = TRUE),
    mean_primary = mean(pc11_td_primary, na.rm = TRUE),
    sd_primary = sd(pc11_td_primary, na.rm = TRUE),
    var_primary = var(pc11_td_primary, na.rm = TRUE),
    mean_health_ctr = mean(pc11_td_health_ctr, na.rm = TRUE),
    sd_health_ctr = sd(pc11_td_health_ctr, na.rm = TRUE),
    var_health_ctr = var(pc11_td_health_ctr, na.rm = TRUE),
    mean_el_dom = mean(pc11_td_el_dom, na.rm = TRUE),
    sd_el_dom = sd(pc11_td_el_dom, na.rm = TRUE),
    var_el_dom = var(pc11_td_el_dom, na.rm = TRUE),
    
    # Other controls
    mean_decoy_ballot_order = mean(avg_decoy_ballot_order, na.rm = TRUE),
    sd_decoy_ballot_order = sd(avg_decoy_ballot_order, na.rm = TRUE),
    var_decoy_ballot_order = var(avg_decoy_ballot_order, na.rm = TRUE),
    
    total_obs = n()
  )

# Create a subset of the data for use with stargazer
summary_data <- constituency_metrics %>%
  dplyr::select(
    # Dependent Variables
    pct_elections_with_decoys, avg_decoy_share, avg_decoy_vote_share,
    
    # Regressors
    sc_st_pct, is_rural, lit,
    
    # Controls
    ruggedness_mean, elevation_mean, land_area, ec13_emp_all, ec13_count_all, 
    dmsp_mean_light_cal, pc11_td_primary, pc11_td_health_ctr, pc11_td_el_dom,
    
    # Other controls
    avg_decoy_ballot_order
  )

# Generate the stargazer table
stargazer(as.data.frame(summary_data),  # explicitly convert to data.frame
          type = "latex",
          title = "Summary Statistics for Decoy Analysis",
          digits = 3,
          align = TRUE,
          mean.sd = TRUE,
          median = FALSE,
          min.max = TRUE,
          covariate.labels = c(
            # Dependent Variables
            "Percentage of Elections with Decoys",
            "Average Decoy Share",
            "Average Decoy Vote Share",
            
            # Regressors
            "SC/ST Population Percentage",
            "Is Rural",
            "Literacy Rate",
            
            # Controls
            "Mean Ruggedness",
            "Mean Elevation",
            "Land Area",
            "Total Employment (2013)",
            "Total Firms (2013)",
            "Mean Night Lights (Calibrated)",
            "Primary Schools",
            "Health Centers",
            "Domestic Electricity Connections",
            
            # Other controls
            "Average Decoy Ballot Order"
          ),
          notes = "This table presents summary statistics for the main variables used in the decoy candidate analysis.",
          notes.align = "l")

constituency_metrics_post_delim <- constituency_metrics_post_delim %>%
  mutate(sc_st_pct = (pc11_pca_m_sc + pc11_pca_f_sc + pc11_pca_m_st + pc11_pca_f_st)/pc11_pca_tot_p, 
         lit = pc11_pca_p_lit/pc11_pca_tot_p, 
         is_rural = ifelse((proportion_urban >= 0.5 | is.na(proportion_urban)), 1, 0))

str(constituency_metrics_post_delim)

# Summary statistics for the specified variables
summary_stats <- constituency_metrics_post_delim %>%
  summarise(
    # Dependent Variables
    mean_pct_elections_with_decoys = mean(pct_elections_with_decoys, na.rm = TRUE),
    sd_pct_elections_with_decoys = sd(pct_elections_with_decoys, na.rm = TRUE),
    var_pct_elections_with_decoys = var(pct_elections_with_decoys, na.rm = TRUE),
    mean_avg_decoy_share = mean(avg_decoy_share, na.rm = TRUE),
    sd_avg_decoy_share = sd(avg_decoy_share, na.rm = TRUE),
    var_avg_decoy_share = var(avg_decoy_share, na.rm = TRUE),
    mean_avg_decoy_vote_share = mean(avg_decoy_vote_share, na.rm = TRUE),
    sd_avg_decoy_vote_share = sd(avg_decoy_vote_share, na.rm = TRUE),
    var_avg_decoy_vote_share = var(avg_decoy_vote_share, na.rm = TRUE),
    
    # Regressors
    mean_sc_st_pct = mean(sc_st_pct, na.rm = TRUE),
    sd_sc_st_pct = sd(sc_st_pct, na.rm = TRUE),
    var_sc_st_pct = var(sc_st_pct, na.rm = TRUE),
    mean_is_rural = mean(is_rural, na.rm = TRUE),
    sd_is_rural = sd(is_rural, na.rm = TRUE),
    var_is_rural = var(is_rural, na.rm = TRUE),
    mean_lit = mean(lit, na.rm = TRUE),
    sd_lit = sd(lit, na.rm = TRUE),
    var_lit = var(lit, na.rm = TRUE),
    
    # Controls
    mean_ruggedness = mean(ruggedness_mean, na.rm = TRUE),
    sd_ruggedness = sd(ruggedness_mean, na.rm = TRUE),
    var_ruggedness = var(ruggedness_mean, na.rm = TRUE),
    mean_elevation = mean(elevation_mean, na.rm = TRUE),
    sd_elevation = sd(elevation_mean, na.rm = TRUE),
    var_elevation = var(elevation_mean, na.rm = TRUE),
    mean_land_area = mean(land_area, na.rm = TRUE),
    sd_land_area = sd(land_area, na.rm = TRUE),
    var_land_area = var(land_area, na.rm = TRUE),
    mean_emp_all = mean(ec13_emp_all, na.rm = TRUE),
    sd_emp_all = sd(ec13_emp_all, na.rm = TRUE),
    var_emp_all = var(ec13_emp_all, na.rm = TRUE),
    mean_count_all = mean(ec13_count_all, na.rm = TRUE),
    sd_count_all = sd(ec13_count_all, na.rm = TRUE),
    var_count_all = var(ec13_count_all, na.rm = TRUE),
    mean_light = mean(dmsp_mean_light_cal, na.rm = TRUE),
    sd_light = sd(dmsp_mean_light_cal, na.rm = TRUE),
    var_light = var(dmsp_mean_light_cal, na.rm = TRUE),
    mean_primary = mean(pc11_td_primary, na.rm = TRUE),
    sd_primary = sd(pc11_td_primary, na.rm = TRUE),
    var_primary = var(pc11_td_primary, na.rm = TRUE),
    mean_health_ctr = mean(pc11_td_health_ctr, na.rm = TRUE),
    sd_health_ctr = sd(pc11_td_health_ctr, na.rm = TRUE),
    var_health_ctr = var(pc11_td_health_ctr, na.rm = TRUE),
    mean_el_dom = mean(pc11_td_el_dom, na.rm = TRUE),
    sd_el_dom = sd(pc11_td_el_dom, na.rm = TRUE),
    var_el_dom = var(pc11_td_el_dom, na.rm = TRUE),
    
    # Other controls
    mean_decoy_ballot_order = mean(avg_decoy_ballot_order, na.rm = TRUE),
    sd_decoy_ballot_order = sd(avg_decoy_ballot_order, na.rm = TRUE),
    var_decoy_ballot_order = var(avg_decoy_ballot_order, na.rm = TRUE),
    
    total_obs = n()
  )

# Create a subset of the data for use with stargazer
summary_data <- constituency_metrics_post_delim %>%
  dplyr::select(
    # Dependent Variables
    pct_elections_with_decoys, avg_decoy_share, avg_decoy_vote_share,
    
    # Regressors
    sc_st_pct, is_rural, lit,
    
    # Controls
    ruggedness_mean, elevation_mean, land_area, ec13_emp_all, ec13_count_all, 
    dmsp_mean_light_cal, pc11_td_primary, pc11_td_health_ctr, pc11_td_el_dom,
    
    # Other controls
    avg_decoy_ballot_order
  )

# Generate the stargazer table
stargazer(as.data.frame(summary_data),  # explicitly convert to data.frame
          type = "latex",
          title = "Summary Statistics for Decoy Analysis",
          digits = 3,
          align = TRUE,
          mean.sd = TRUE,
          median = FALSE,
          min.max = TRUE,
          covariate.labels = c(
            # Dependent Variables
            "Percentage of Elections with Decoys",
            "Average Decoy Share",
            "Average Decoy Vote Share",
            
            # Regressors
            "SC/ST Population Percentage",
            "Is Rural",
            "Literacy Rate",
            
            # Controls
            "Mean Ruggedness",
            "Mean Elevation",
            "Land Area",
            "Total Employment (2013)",
            "Total Firms (2013)",
            "Mean Night Lights (Calibrated)",
            "Primary Schools",
            "Health Centers",
            "Domestic Electricity Connections",
            
            # Other controls
            "Average Decoy Ballot Order"
          ),
          notes = "This table presents summary statistics for the main variables used in the decoy candidate analysis.",
          notes.align = "l")

constituency_metrics_pre_delim <- constituency_metrics_pre_delim %>%
  mutate(sc_st_pct = (pc01_pca_m_sc + pc01_pca_f_sc + pc01_pca_m_st + pc01_pca_f_st)/pc01_pca_tot_p, 
         lit = pc01_pca_p_lit/pc01_pca_tot_p, 
         is_rural = ifelse((proportion_urban >= 0.5 | is.na(proportion_urban)), 1, 0))

str(constituency_metrics_pre_delim)

# Summary statistics for the specified variables
summary_stats <- constituency_metrics_pre_delim %>%
  summarise(
    # Dependent Variables
    mean_pct_elections_with_decoys = mean(pct_elections_with_decoys, na.rm = TRUE),
    sd_pct_elections_with_decoys = sd(pct_elections_with_decoys, na.rm = TRUE),
    var_pct_elections_with_decoys = var(pct_elections_with_decoys, na.rm = TRUE),
    mean_avg_decoy_share = mean(avg_decoy_share, na.rm = TRUE),
    sd_avg_decoy_share = sd(avg_decoy_share, na.rm = TRUE),
    var_avg_decoy_share = var(avg_decoy_share, na.rm = TRUE),
    mean_avg_decoy_vote_share = mean(avg_decoy_vote_share, na.rm = TRUE),
    sd_avg_decoy_vote_share = sd(avg_decoy_vote_share, na.rm = TRUE),
    var_avg_decoy_vote_share = var(avg_decoy_vote_share, na.rm = TRUE),
    
    # Regressors
    mean_sc_st_pct = mean(sc_st_pct, na.rm = TRUE),
    sd_sc_st_pct = sd(sc_st_pct, na.rm = TRUE),
    var_sc_st_pct = var(sc_st_pct, na.rm = TRUE),
    mean_is_rural = mean(is_rural, na.rm = TRUE),
    sd_is_rural = sd(is_rural, na.rm = TRUE),
    var_is_rural = var(is_rural, na.rm = TRUE),
    mean_lit = mean(lit, na.rm = TRUE),
    sd_lit = sd(lit, na.rm = TRUE),
    var_lit = var(lit, na.rm = TRUE),
    
    # Controls
    mean_ruggedness = mean(ruggedness_mean, na.rm = TRUE),
    sd_ruggedness = sd(ruggedness_mean, na.rm = TRUE),
    var_ruggedness = var(ruggedness_mean, na.rm = TRUE),
    mean_elevation = mean(elevation_mean, na.rm = TRUE),
    sd_elevation = sd(elevation_mean, na.rm = TRUE),
    var_elevation = var(elevation_mean, na.rm = TRUE),
    mean_land_area = mean(land_area, na.rm = TRUE),
    sd_land_area = sd(land_area, na.rm = TRUE),
    var_land_area = var(land_area, na.rm = TRUE),
    mean_emp_all = mean(ec05_emp_all, na.rm = TRUE),
    sd_emp_all = sd(ec05_emp_all, na.rm = TRUE),
    var_emp_all = var(ec05_emp_all, na.rm = TRUE),
    mean_count_all = mean(ec05_count_all, na.rm = TRUE),
    sd_count_all = sd(ec05_count_all, na.rm = TRUE),
    var_count_all = var(ec05_count_all, na.rm = TRUE),
    mean_light = mean(dmsp_mean_light_cal, na.rm = TRUE),
    sd_light = sd(dmsp_mean_light_cal, na.rm = TRUE),
    var_light = var(dmsp_mean_light_cal, na.rm = TRUE),
    mean_primary = mean(pc01_td_primary, na.rm = TRUE),
    sd_primary = sd(pc01_td_primary, na.rm = TRUE),
    var_primary = var(pc01_td_primary, na.rm = TRUE),
    mean_health_ctr = mean(pc01_td_health_ctr, na.rm = TRUE),
    sd_health_ctr = sd(pc01_td_health_ctr, na.rm = TRUE),
    var_health_ctr = var(pc01_td_health_ctr, na.rm = TRUE),
    mean_el_dom = mean(pc01_td_el_dom, na.rm = TRUE),
    sd_el_dom = sd(pc01_td_el_dom, na.rm = TRUE),
    var_el_dom = var(pc01_td_el_dom, na.rm = TRUE),
    
    # Other controls
    mean_decoy_ballot_order = mean(avg_decoy_ballot_order, na.rm = TRUE),
    sd_decoy_ballot_order = sd(avg_decoy_ballot_order, na.rm = TRUE),
    var_decoy_ballot_order = var(avg_decoy_ballot_order, na.rm = TRUE),
    
    total_obs = n()
  )

# Create a subset of the data for use with stargazer
summary_data <- constituency_metrics_pre_delim %>%
  dplyr::select(
    # Dependent Variables
    pct_elections_with_decoys, avg_decoy_share, avg_decoy_vote_share,
    
    # Regressors
    sc_st_pct, is_rural, lit,
    
    # Controls
    ruggedness_mean, elevation_mean, land_area, ec05_emp_all, ec05_count_all, 
    dmsp_mean_light_cal, pc01_td_primary, pc01_td_health_ctr, pc01_td_el_dom,
    
    # Other controls
    avg_decoy_ballot_order
  )

# Generate the stargazer table
stargazer(as.data.frame(summary_data),  # explicitly convert to data.frame
          type = "latex",
          title = "Summary Statistics for Decoy Analysis",
          digits = 3,
          align = TRUE,
          mean.sd = TRUE,
          median = FALSE,
          min.max = TRUE,
          covariate.labels = c(
            # Dependent Variables
            "Percentage of Elections with Decoys",
            "Average Decoy Share",
            "Average Decoy Vote Share",
            
            # Regressors
            "SC/ST Population Percentage",
            "Is Rural",
            "Literacy Rate",
            
            # Controls
            "Mean Ruggedness",
            "Mean Elevation",
            "Land Area",
            "Total Employment (2005)",
            "Total Firms (2005)",
            "Mean Night Lights (Calibrated)",
            "Primary Schools",
            "Health Centers",
            "Domestic Electricity Connections",
            
            # Other controls
            "Average Decoy Ballot Order"
          ),
          notes = "This table presents summary statistics for the main variables used in the decoy candidate analysis.",
          notes.align = "l")
