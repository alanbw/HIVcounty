#################### LIBRARIES ############################
library(igraph)
library(tidyverse)
library(stringr)
library(zipcodeR)
library(tidycensus)
library(reshape2)
library(lubridate)
#################### END LIBRARIES ########################

#################### ZIP CODE DATA ########################
# Load in R's zip code database; keep only county/zipcode
data("zip_code_db")
# Just Clark County zip codes
zip_codes_clark <- zip_code_db %>%
  dplyr::filter(state == "NV" & county == "Clark County") %>%
  dplyr::select(c(zipcode, county))
# Just San Diego zip codes
zip_codes_sd <- zip_code_db %>%
  dplyr::filter(state == "CA" & county == "San Diego County") %>%
  dplyr::select(c(zipcode, county))
# Pre-select for just zip/county
zip_code_db <- zip_code_db %>% 
  dplyr::select(c(zipcode, county)) 
#################### END ZIP CODE DATA ####################

#################### CENSUS DATA ##########################
# Load in all possible ACS variables
# acs_vars <- load_variables(2021, "acs5", cache = TRUE)

# For renaming later; all non-insurance variables
# vars_dict <- c("Age" = "B01002_001",
#                "Gender_All" = "B01001_001",
#                "Gender_Male" = "B01001_002",
#                "Gender_Female" = "B01001_026",
#                "Race_All" = "B02001_001",
#                "Race_White" = "B02001_002",
#                "Race_Black" = "B02001_003",
#                "Race_NativeAmericanAlaskan" = "B02001_004",
#                "Race_Asian" = "B02001_005",
#                "Race_PacificIslander" = "B02001_006",
#                "Race_Other_Alone" = "B02001_007",
#                "Race_Other_2+" = "B02001_008",
#                "Speaks_only_English" = "B16005_003",
#                "Speaks_Spanish" = "B16005_004",
#                "Speaks_English_VeryWell" = "B16005_005",
#                "Speaks_English_Well" = "B16005_006",
#                "Speaks_English_NotWell" = "B16005_007",
#                "Speaks_English_NotAtAll" = "B16005_008",
#                "Poverty" = "B17001_002",
#                "Median_Income_Total" = "B20001_001",
#                "Median_Income_Male" = "B20001_002",
#                "Median_Income_Female" = "B20001_023",
#                "Housing_Total" = "B25024_001",
#                "Housing_1_Detached" = "B25024_002",
#                "Housing_1_Attached" = "B25024_003",
#                "Housing_2" = "B25024_004",
#                "Housing_3_to_4" = "B25024_005",
#                "Housing_5_to_9" = "B25024_006",
#                "Housing_10_to_19" = "B25024_007",
#                "Housing_20_to_49" = "B25024_008",
#                "Housing_50+" = "B25024_009",
#                "Housing_Mobile_Home" = "B25024_010",
#                "Housing_Other_Type" = "B25024_011",
#                "HousingOcc_Total" = "B25014_001",
#                "HousingOcc_Owner" = "B25014_002",
#                "HousingOcc_Owner_Up_To_1_Per_Room" = "B25014_003",
#                "HousingOcc_Owner_1+_Per_Room" = "B25014_004",
#                "HousingOcc_Renter" = "B25014_005",
#                "HousingOcc_Renter_Up_To_1_Per_Room" = "B25014_006",
#                "HousingOcc_Renter_1+_Per_Room" = "B25014_007",
#                "SubFamily_Total" = "B11013_001",
#                "SubFamily_Married_Couple" = "B11013_002",
#                "SubFamily_Single_Parent" = "B11013_003",
#                "Disability_Total" = "B21100_001",
#                "Disability_No_Service_Connected_Disability_Rating" = "B21100_002",
#                "PubAssist_Total" = "B19057_001",
#                "PubAssist_Have_PubAssist" = "B19057_002",
#                "Employment_Total" = "B23025_001",
#                "Employment_In_LF" = "B23025_002",
#                "Employment_Civilian_LF" = "B23025_003",
#                "Employment_Employed_Citizens" = "B23025_004",
#                "Employment_Unemployed_Citizens" = "B23025_005",
#                "Employment_Employed_Labor_Armed_Services" = "B23025_006",
#                "Employment_Not_In_LF" = "B23025_007")
# 
# # Separate them by variable type
# age_vec <- c("B01002_001")
# gender_vec <- c("B01001_001", "B01001_002", "B01001_026")
# race_vec <- c("B02001_001", "B02001_002", "B02001_003", "B02001_004", "B02001_005",
#               "B02001_006", "B02001_007", "B02001_008")
# lang_vec <- c("B16005_003", "B16005_004", "B16005_005", "B16005_006", "B16005_007", "B16005_008")
# income_vec <- c("B17001_002", "B20001_001", "B20001_002", "B20001_023")
# housing_vec <- c("B25024_001", "B25024_002", "B25024_003", "B25024_004", "B25024_005",
#                  "B25024_006", "B25024_007", "B25024_008", "B25024_009", "B25024_010", "B25024_011")
# housingocc_vec <- c("B25014_001", "B25014_002", "B25014_003", "B25014_004",
#                     "B25014_005", "B25014_006", "B25014_007")
# subfamily_vec <- c("B11013_001", "B11013_002", "B11013_003")
# diability_vec <- c("B21100_001", "B21100_002")
# pubassist_vec <- c("B19057_001", "B19057_002")
# employment_vec <- c("B23025_001", "B23025_002", "B23025_003", "B23025_004",
#                     "B23025_005", "B23025_006", "B23025_007")
# insurance_vec <- c("B27001_001", "B27001_002", "B27001_003", "B27001_004", "B27001_005",
#                    "B27001_006", "B27001_007", "B27001_008", "B27001_009", "B27001_010",
#                    "B27001_011", "B27001_012", "B27001_013", "B27001_014", "B27001_015",
#                    "B27001_016", "B27001_017", "B27001_018", "B27001_019", "B27001_020",
#                    "B27001_021", "B27001_022", "B27001_023", "B27001_024", "B27001_025",
#                    "B27001_026", "B27001_027", "B27001_028", "B27001_029", "B27001_030",
#                    "B27001_031", "B27001_032", "B27001_033", "B27001_034", "B27001_035",
#                    "B27001_036", "B27001_037", "B27001_038", "B27001_039", "B27001_040",
#                    "B27001_041", "B27001_042", "B27001_043", "B27001_044", "B27001_045",
#                    "B27001_046", "B27001_047", "B27001_048", "B27001_049", "B27001_050",
#                    "B27001_051", "B27001_052", "B27001_053", "B27001_054", "B27001_055",
#                    "B27001_056", "B27001_057")
# 
# # Download the census data for all variables of interest
# # Will raise a warning without API; not an issue unless we pull it 500+ times per day
# census_dat <- get_acs(geography = "zcta", variables = c(age_vec, gender_vec, race_vec, lang_vec, income_vec, housing_vec,
#                                                         housingocc_vec, subfamily_vec, diability_vec, pubassist_vec,
#                                                         employment_vec, insurance_vec),cache_table = T)
# # Split by county zips
# census_sd <- census_dat %>% filter(GEOID %in% zip_codes_sd$zipcode)
# census_clark <- census_dat %>% filter(GEOID %in% zip_codes_clark$zipcode)
# 
# # Function to pull the insurance stuff together
# sum_values <- function(df){
#   with_ins_vars <- c("B27001_004", "B27001_007", "B27001_010", "B27001_013", "B27001_016",
#                      "B27001_019", "B27001_022", "B27001_025", "B27001_028", "B27001_032",
#                      "B27001_035", "B27001_038", "B27001_041", "B27001_044", "B27001_047",
#                      "B27001_050", "B27001_053", "B27001_056")
#   without_ins_vars <- c("B27001_005", "B27001_008", "B27001_011", "B27001_014", "B27001_017",
#                         "B27001_020", "B27001_023", "B27001_026", "B27001_029", "B27001_033",
#                         "B27001_036", "B27001_039", "B27001_042", "B27001_045", "B27001_048",
#                         "B27001_051", "B27001_054", "B27001_057")
#   With_Insurance <- c()
#   No_Insurance <- c()
#   Insurance_All <- c()
#   # Loop through each zip code
#   for(i in c(1:nrow(df))){
#     # Set sums for with/without to 0
#     x_with <- 0
#     x_without <- 0
#     # Loop through the with/without columns & sum
#     for(j in c(1:length(with_ins_vars))){
#       x_with <- x_with + df[i,with_ins_vars[j]]
#       x_without <- x_without + df[i,without_ins_vars[j]]
#     }
#     # Put sums in vectors and get a "total" column
#     With_Insurance[i] = x_with
#     No_Insurance[i] = x_without
#     Insurance_All[i] = sum(x_with, x_without)
#   }
#   # Remove everything but the zip code
#   df <- df %>% select("GEOID")
#   # Add our 3 columns
#   df$Insurance_All <- Insurance_All
#   df$With_Insurance <- With_Insurance
#   df$No_Insurance <- No_Insurance
# 
#   return(df)
# }
# 
# # Summarize SD by-zip data
# census_sd_by_zip <- census_sd %>%
#   arrange(GEOID, variable) %>%
#   group_by(GEOID, variable) %>%
#   # After grouping, this gives us 3 columns: zip code, variable number, and count
#   summarise(estimate = estimate) %>%
#   # Sets variable as column names and counts as values
#   dcast(GEOID ~ variable)
# 
# # Call the function to get insurance counts
# sd_by_zip_with_ins <- sum_values(df = census_sd_by_zip %>%
#                                    select(all_of(c("GEOID", insurance_vec))))
# 
# census_sd_summary <- census_sd_by_zip %>%
#   # Remove separate insurance variables
#   select(all_of(c("GEOID", age_vec, gender_vec, race_vec, lang_vec, income_vec, housing_vec, housingocc_vec,
#                   subfamily_vec, diability_vec, pubassist_vec, employment_vec, insurance_vec))) %>%
#   # Set descriptive names
#   rename(all_of(vars_dict)) %>%
#   # Add the insurance data to the other items
#   left_join(sd_by_zip_with_ins, by = "GEOID") %>%
#   rename(zipcode = "GEOID")
# 
# # Summarize Clark by-zip data
# census_clark_by_zip <- census_clark %>%
#   arrange(GEOID, variable) %>%
#   group_by(GEOID, variable) %>%
#   # After grouping, this gives us 3 columns: zip code, variable number, and count
#   summarise(estimate = estimate) %>%
#   # Sets variable as column names and counts as values
#   dcast(GEOID ~ variable)
# 
# # Call the function to get insurance counts
# clark_by_zip_with_ins <- sum_values(df = census_clark_by_zip %>%
#                                       select(all_of(c("GEOID", insurance_vec))))
# 
# census_clark_summary <- census_clark_by_zip %>%
#   # Remove separate insurance variables
#   select(all_of(c("GEOID", age_vec, gender_vec, race_vec, lang_vec, income_vec, housing_vec, housingocc_vec,
#                   subfamily_vec, diability_vec, pubassist_vec, employment_vec, insurance_vec))) %>%
#   # Set descriptive names
#   rename(all_of(vars_dict)) %>%
#   # Add the insurance data to the other items
#   left_join(clark_by_zip_with_ins, by = "GEOID") %>%
#   rename(zipcode = "GEOID")
#################### END CENSUS DATA ######################

#################### LOAD & FIX PLWH DATA #################
# Load SD PLWH data
# load(paste("~/University of California, San Diego Health/",
#            "TrIUMPH - San Diego - San Diego/Data/Current_Data/UCSD_eHARS_alltables_052023.Rdata", sep = ""))
# Function to fix dates
# date.format.export <- '%Y%m%d'
# fx.date.fmt <- function(x){as.Date(str_replace_all(x, "[.][.]", "01"),format = date.format.export)}
# date_remove_na <- function(x){
#   if(x == as.Date("01010101", format = "%Y%m%d")){
#     return(NA)}
#   else{return(x)}}
date_cols <- c("vl_first_dt", "vl_recent_dt", "cd4_first_dt", "cd4_recent_dt", 
               "cd4_vl_first_hiv_dt", "cd4_low_cnt_dt", "hiv_aids_dx_dt", "dob", "dod")
# Attach County information
# demographics_sd <- demographics %>%
#   # These have a bunch of NA values. Remove here, replace in mutate()
#   select(-c("vl_first_dt_fmt", "vl_recent_dt_fmt", "cd4_first_dt_fmt", 
#             "cd4_recent_dt_fmt", "cd4_vl_first_hiv_dt_fmt", "cd4_low_cnt_dt_fmt", 
#             "hiv_aids_dx_dt_fmt", "dob_fmt", "dod_fmt")) %>%
#   mutate(
#     # Format dates
#     across(.cols = all_of(date_cols),.names = '{.col}_fmt',.fns = fx.date.fmt), 
#     vl_first_dt_fmt = as.Date(ifelse(vl_first_dt_fmt == as.Date("01010101", "%Y%m%d"), NA, as.character(vl_first_dt_fmt)), "%Y-%m-%d"),
#     vl_recent_dt_fmt = as.Date(ifelse(vl_recent_dt_fmt == as.Date("01010101", "%Y%m%d"), NA, as.character(vl_recent_dt_fmt)), "%Y-%m-%d"),
#     cd4_first_dt_fmt = as.Date(ifelse(cd4_first_dt_fmt == as.Date("01010101", "%Y%m%d"), NA, as.character(cd4_first_dt_fmt)), "%Y-%m-%d"),
#     cd4_recent_dt_fmt = as.Date(ifelse(cd4_recent_dt_fmt == as.Date("01010101", "%Y%m%d"), NA, as.character(cd4_recent_dt_fmt)), "%Y-%m-%d"),
#     cd4_vl_first_hiv_dt_fmt = as.Date(ifelse(cd4_vl_first_hiv_dt_fmt == as.Date("01010101", "%Y%m%d"), NA, as.character(cd4_vl_first_hiv_dt_fmt)), "%Y-%m-%d"),
#     cd4_low_cnt_dt_fmt = as.Date(ifelse(cd4_low_cnt_dt_fmt == as.Date("01010101", "%Y%m%d"), NA, as.character(cd4_low_cnt_dt_fmt)), "%Y-%m-%d"),
#     hiv_aids_dx_dt_fmt = as.Date(ifelse(hiv_aids_dx_dt_fmt == as.Date("01010101", "%Y%m%d"), NA, as.character(hiv_aids_dx_dt_fmt)), "%Y-%m-%d"),
#     dob_fmt = as.Date(ifelse(dob_fmt == as.Date("01010101", "%Y%m%d"), NA, as.character(dob_fmt)), "%Y-%m-%d"),
#     dod_fmt = as.Date(ifelse(dod_fmt == as.Date("01010101", "%Y%m%d"), NA, as.character(dod_fmt)), "%Y-%m-%d"), 
#     # Change zip codes to character values for merging
#     cur_zip_cd = as.character(cur_zip_cd),
#     rsd_zip_cd = as.character(rsd_zip_cd),
#     # 1 for yes, 2 for no
#     rsd_in_sd_county = ifelse(rsd_zip_cd %in% zip_codes_sd$zipcode, 1, 0),
#     cur_in_sd_county = ifelse(cur_zip_cd %in% zip_codes_sd$zipcode, 1, 0),
#     # Impute Gender to M/F/Other; MTF = Female, FTM = Male
#     gender_imputed = factor(`Current gender`, 
#                             levels = c("Male", NA, "Female", "Transgender-- Male to female", 
#                                        "Transgender-- Female to male", "Additional Gender Identity"),
#                             labels = c("Male", "Other", "Female", "Female", "Male", "Other"),
#                             exclude = NULL)) %>%
#   # Set current county
#   left_join(zip_code_db, by = c("cur_zip_cd" = "zipcode")) %>%
#   rename(cur_county = "county",
#          UCI = "UCSD_id") %>%
#   # Set rsd county
#   left_join(zip_code_db, by = c("rsd_zip_cd" = "zipcode")) %>%
#   rename(rsd_county = "county")
# # Save SD labs data
# labs_sd <- labs %>%
#   mutate(sample_year = year(sample_dt_fmt)) %>%
#   rename(UCI = "UCSD_id")
# labs_sd_distinct <- labs_sd %>% distinct(UCI, sample_year, .keep_all = TRUE)


# Load Clark County PLWH data; overwrites non-altered SD variables
load(paste("~/University of California, San Diego Health/",
           "TrIUMPH - Clark County - Clark County/Data/Current_Data/Nevada_eHARS_alltables_073123.Rdata", sep = ""))
# Attach County information
demographics_clark <- demographics %>%
  mutate(
    # Change zip codes to character values for merging
    cur_zip_cd = as.character(cur_zip_cd),
    rsd_zip_cd = as.character(rsd_zip_cd),
    # 1 for yes, 2 for no
    rsd_in_clark_county = ifelse(rsd_zip_cd %in% zip_codes_clark$zipcode, 1, 0),
    cur_in_clark_county = ifelse(cur_zip_cd %in% zip_codes_clark$zipcode, 1, 0),
    # Impute Gender to M/F/Other; MTF = Female, FTM = Male
    gender_imputed = factor(`Current gender`, 
                            levels = c("Male", NA, "Female", "Transgender-- Male to female", 
                                       "Transgender-- Female to male", "Additional Gender Identity"),
                            labels = c("Male", "Other", "Female", "Female", "Male", "Other"),
                            exclude = NULL)) %>%
  # Set current county
  left_join(zip_code_db, by = c("cur_zip_cd" = "zipcode")) %>%
  rename(cur_county = "county") %>%
  # Set rsd county
  left_join(zip_code_db, by = c("rsd_zip_cd" = "zipcode")) %>%
  rename(rsd_county = "county")
# Save labs data
# labs_clark <- labs %>%
#   mutate(sample_year = year(sample_dt_fmt))
# labs_clark_distinct <- labs_clark %>% distinct(UCI, sample_year, .keep_all = TRUE)

# Get rid of these so we don't accidentally use them
rm("demographics", "genotypes", "genotypes.baseline", "labs", "sti")

# Mark if alive on 1/1/2019
# demographics_sd <- demographics_sd %>%
#   mutate(alive_1_1_2019 = ifelse(dod == "........", 1, 
#                                  ifelse(dod_fmt >= as.Date("20190101", "%Y%m%d"), 1, 0)))
# demographics_clarktmp <- demographics_clark %>%
#   mutate(alive_1_1_2019 = ifelse(dod == ".", 1, 
#                                  ifelse(dod_fmt >= as.Date("20190101", "%Y%m%d"), 1, 0)))
#################### END LOAD & FIX PLWH DATA #############




#################### SUMMARISE AND SAVE DATA ##############
# demo_summary_sd <- demo_sd_out %>% 
#   arrange(rsd_zip_cd) %>%
#   group_by(rsd_zip_cd) %>%
#   summarise(
#     total_linked_2018 = sum(linked_2018, na.rm = TRUE), 
#     percent_linked_2018 = (sum(linked_2018, na.rm = TRUE)/sum(!is.na(linked_2018))), 
#     total_linked_2019 = sum(linked_2019, na.rm = TRUE), 
#     percent_linked_2019 = (sum(linked_2019, na.rm = TRUE)/sum(!is.na(linked_2019))), 
#     total_linked_2020 = sum(linked_2020, na.rm = TRUE), 
#     percent_linked_2020 = (sum(linked_2020, na.rm = TRUE)/sum(!is.na(linked_2020))), 
#     total_linked_2021 = sum(linked_2021, na.rm = TRUE), 
#     percent_linked_2021 = (sum(linked_2021, na.rm = TRUE)/sum(!is.na(linked_2021))), 
#     total_linked_2022 = sum(linked_2022, na.rm = TRUE), 
#     percent_linked_2022 = (sum(linked_2022, na.rm = TRUE)/sum(!is.na(linked_2022))),
#     total_clustered_2011 = sum(clustered_2011, na.rm = TRUE), 
#     percent_clustered_2011 = (sum(clustered_2011, na.rm = TRUE)/sum(!is.na(clustered_2011))), 
#     total_clustered_2012 = sum(clustered_2012, na.rm = TRUE), 
#     percent_clustered_2012 = (sum(clustered_2012, na.rm = TRUE)/sum(!is.na(clustered_2012))), 
#     total_clustered_2013 = sum(clustered_2013, na.rm = TRUE), 
#     percent_clustered_2013 = (sum(clustered_2013, na.rm = TRUE)/sum(!is.na(clustered_2013))), 
#     total_clustered_2014 = sum(clustered_2014, na.rm = TRUE), 
#     percent_clustered_2014 = (sum(clustered_2014, na.rm = TRUE)/sum(!is.na(clustered_2014))), 
#     total_clustered_2015 = sum(clustered_2015, na.rm = TRUE), 
#     percent_clustered_2015 = (sum(clustered_2015, na.rm = TRUE)/sum(!is.na(clustered_2015))), 
#     total_clustered_2016 = sum(clustered_2016, na.rm = TRUE), 
#     percent_clustered_2016 = (sum(clustered_2016, na.rm = TRUE)/sum(!is.na(clustered_2016))), 
#     total_clustered_2017 = sum(clustered_2017, na.rm = TRUE), 
#     percent_clustered_2017 = (sum(clustered_2017, na.rm = TRUE)/sum(!is.na(clustered_2017))), 
#     total_clustered_2018 = sum(clustered_2018, na.rm = TRUE), 
#     percent_clustered_2018 = (sum(clustered_2018, na.rm = TRUE)/sum(!is.na(clustered_2018))), 
#     total_clustered_2019 = sum(clustered_2019, na.rm = TRUE), 
#     percent_clustered_2019 = (sum(clustered_2019, na.rm = TRUE)/sum(!is.na(clustered_2019))), 
#     total_clustered_2020 = sum(clustered_2020, na.rm = TRUE), 
#     percent_clustered_2020 = (sum(clustered_2020, na.rm = TRUE)/sum(!is.na(clustered_2020))), 
#     total_clustered_2021 = sum(clustered_2021, na.rm = TRUE), 
#     percent_clustered_2021 = (sum(clustered_2021, na.rm = TRUE)/sum(!is.na(clustered_2021))), 
#     total_clustered_2022 = sum(clustered_2022, na.rm = TRUE), 
#     percent_clustered_2022 = (sum(clustered_2022, na.rm = TRUE)/sum(!is.na(clustered_2022))), 
#     total_stage_3 = sum(stage_3, na.rm = TRUE),
#     percent_stage_3 = (sum(stage_3, na.rm = TRUE)/sum(!is.na(stage_3))),
#     total_stage_4 = sum(stage_4, na.rm = TRUE),
#     percent_stage_4 = (sum(stage_4, na.rm = TRUE)/sum(!is.na(stage_4))),
#     total_stage_5 = sum(stage_5, na.rm = TRUE),
#     percent_stage_5 = (sum(stage_5, na.rm = TRUE)/sum(!is.na(stage_5))),
#     total_stage_6 = sum(stage_6, na.rm = TRUE),
#     percent_stage_6 = (sum(stage_6, na.rm = TRUE)/sum(!is.na(stage_6))),
#     total_stage_7 = sum(stage_7, na.rm = TRUE),
#     percent_stage_7 = (sum(stage_7, na.rm = TRUE)/sum(!is.na(stage_7)))
#   ) %>%
#   dplyr::filter(!is.na(rsd_zip_cd))
# sd_all_vars_by_zip <- full_join(census_sd_summary,
#                                    demo_summary_sd, by = c("zipcode" = "rsd_zip_cd"))



# demo_summary_clark <- demo_clark_out %>% 
#   arrange(rsd_zip_cd) %>%
#   group_by(rsd_zip_cd) %>%
#   summarise(
#     total_linked_2011 = sum(linked_2011, na.rm = TRUE), 
#     percent_linked_2011 = (sum(linked_2011, na.rm = TRUE)/sum(!is.na(linked_2011))), 
#     total_linked_2012 = sum(linked_2012, na.rm = TRUE), 
#     percent_linked_2012 = (sum(linked_2012, na.rm = TRUE)/sum(!is.na(linked_2012))), 
#     total_linked_2013 = sum(linked_2013, na.rm = TRUE), 
#     percent_linked_2013 = (sum(linked_2013, na.rm = TRUE)/sum(!is.na(linked_2013))), 
#     total_linked_2014 = sum(linked_2014, na.rm = TRUE), 
#     percent_linked_2014 = (sum(linked_2014, na.rm = TRUE)/sum(!is.na(linked_2014))), 
#     total_linked_2015 = sum(linked_2015, na.rm = TRUE), 
#     percent_linked_2015 = (sum(linked_2015, na.rm = TRUE)/sum(!is.na(linked_2015))), 
#     total_linked_2016 = sum(linked_2016, na.rm = TRUE), 
#     percent_linked_2016 = (sum(linked_2016, na.rm = TRUE)/sum(!is.na(linked_2016))), 
#     total_linked_2017 = sum(linked_2017, na.rm = TRUE), 
#     percent_linked_2017 = (sum(linked_2017, na.rm = TRUE)/sum(!is.na(linked_2017))), 
#     total_linked_2018 = sum(linked_2018, na.rm = TRUE), 
#     percent_linked_2018 = (sum(linked_2018, na.rm = TRUE)/sum(!is.na(linked_2018))), 
#     total_linked_2019 = sum(linked_2019, na.rm = TRUE), 
#     percent_linked_2019 = (sum(linked_2019, na.rm = TRUE)/sum(!is.na(linked_2019))), 
#     total_linked_2020 = sum(linked_2020, na.rm = TRUE), 
#     percent_linked_2020 = (sum(linked_2020, na.rm = TRUE)/sum(!is.na(linked_2020))), 
#     total_linked_2021 = sum(linked_2021, na.rm = TRUE), 
#     percent_linked_2021 = (sum(linked_2021, na.rm = TRUE)/sum(!is.na(linked_2021))), 
#     total_linked_2022 = sum(linked_2022, na.rm = TRUE), 
#     percent_linked_2022 = (sum(linked_2022, na.rm = TRUE)/sum(!is.na(linked_2022))),
#     total_clustered_2018 = sum(clustered_2018, na.rm = TRUE), 
#     percent_clustered_2018 = (sum(clustered_2018, na.rm = TRUE)/sum(!is.na(clustered_2018))), 
#     total_clustered_2019 = sum(clustered_2019, na.rm = TRUE), 
#     percent_clustered_2019 = (sum(clustered_2019, na.rm = TRUE)/sum(!is.na(clustered_2019))), 
#     total_clustered_2020 = sum(clustered_2020, na.rm = TRUE), 
#     percent_clustered_2020 = (sum(clustered_2020, na.rm = TRUE)/sum(!is.na(clustered_2020))), 
#     total_clustered_2021 = sum(clustered_2021, na.rm = TRUE), 
#     percent_clustered_2021 = (sum(clustered_2021, na.rm = TRUE)/sum(!is.na(clustered_2021))), 
#     total_clustered_2022 = sum(clustered_2022, na.rm = TRUE), 
#     percent_clustered_2022 = (sum(clustered_2022, na.rm = TRUE)/sum(!is.na(clustered_2022)))
#   ) %>%
#   dplyr::filter(!is.na(rsd_zip_cd))
# Replace NA percents with 0
# Should ask if this is wanted or not
# demo_summary_clark <- replace(demo_summary_clark, is.na(demo_summary_clark), 0)

#source quarto document which calculates individual level predictors 
quarto::quarto_render(input = "./Clark_County_2022_Individual_predictors.qmd",
              # output_file = paste0(".\\HTML\\my_file_to_render","_",today(), '.html'),
              # execute_params =list(pcn = a_param), 
              output_format='html')
#read in files generated by quarto report
# indv.pred <- read.csv(file = "~./clark_indiv_predictors.csv")
# zip.pred <- read.csv(file = "~./clark_county_by_zip.csv")
load(file = "./clark_indiv_predictors.Rdata")
load(file = "./clark_county_by_zip.Rdata")

# clark_all_vars_by_zip <- full_join(census_clark_summary,
#                                    predictors_df_zip_clark %>% mutate(rsd_zip_cd = as.character(rsd_zip_cd)), by = c("zipcode" = "rsd_zip_cd"))

demo_clark_out <- demographics_clark %>%
  left_join(predictors_df_clark,by = 'UCI')

# save(clark_all_vars_by_zip, sd_all_vars_by_zip, demo_clark_out, demo_sd_out,
#      file = "Census_and_PLWH_summaries.RData")
write.csv(predictors_df_zip_clark, file = "./clark_county_by_zip.csv", row.names = FALSE)
# write.csv(sd_all_vars_by_zip, file = "sd_county_by_zip.csv", row.names = FALSE)
write.csv(demo_clark_out, file = "./clark_county_demographics.csv", row.names = FALSE)
# write.csv(demo_sd_out, file = "sd_county_demographics.csv", row.names = FALSE)
#################### END SUMMARISE AND SAVE DATA ##########