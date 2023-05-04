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
vars_dict <- c("Age" = "B01002_001", 
               "Gender_All" = "B01001_001", 
               "Gender_Male" = "B01001_002", 
               "Gender_Female" = "B01001_026", 
               "Race_All" = "B02001_001", 
               "Race_White" = "B02001_002", 
               "Race_Black" = "B02001_003", 
               "Race_NativeAmericanAlaskan" = "B02001_004", 
               "Race_Asian" = "B02001_005", 
               "Race_PacificIslander" = "B02001_006", 
               "Race_Other_Alone" = "B02001_007", 
               "Race_Other_2+" = "B02001_008", 
               "Speaks_only_English" = "B16005_003", 
               "Speaks_Spanish" = "B16005_004", 
               "Speaks_English_VeryWell" = "B16005_005", 
               "Speaks_English_Well" = "B16005_006", 
               "Speaks_English_NotWell" = "B16005_007", 
               "Speaks_English_NotAtAll" = "B16005_008", 
               "Poverty" = "B17001_002", 
               "Median_Income_Total" = "B20001_001", 
               "Median_Income_Male" = "B20001_002", 
               "Median_Income_Female" = "B20001_023", 
               "Housing_Total" = "B25024_001", 
               "Housing_1_Detached" = "B25024_002", 
               "Housing_1_Attached" = "B25024_003", 
               "Housing_2" = "B25024_004", 
               "Housing_3_to_4" = "B25024_005", 
               "Housing_5_to_9" = "B25024_006", 
               "Housing_10_to_19" = "B25024_007", 
               "Housing_20_to_49" = "B25024_008", 
               "Housing_50+" = "B25024_009", 
               "Housing_Mobile_Home" = "B25024_010", 
               "Housing_Other_Type" = "B25024_011", 
               "HousingOcc_Total" = "B25014_001", 
               "HousingOcc_Owner" = "B25014_002", 
               "HousingOcc_Owner_Up_To_1_Per_Room" = "B25014_003", 
               "HousingOcc_Owner_1+_Per_Room" = "B25014_004", 
               "HousingOcc_Renter" = "B25014_005", 
               "HousingOcc_Renter_Up_To_1_Per_Room" = "B25014_006", 
               "HousingOcc_Renter_1+_Per_Room" = "B25014_007", 
               "SubFamily_Total" = "B11013_001", 
               "SubFamily_Married_Couple" = "B11013_002", 
               "SubFamily_Single_Parent" = "B11013_003", 
               "Disability_Total" = "B21100_001", 
               "Disability_No_Service_Connected_Disability_Rating" = "B21100_002", 
               "PubAssist_Total" = "B19057_001", 
               "PubAssist_Have_PubAssist" = "B19057_002", 
               "Employment_Total" = "B23025_001", 
               "Employment_In_LF" = "B23025_002", 
               "Employment_Civilian_LF" = "B23025_003", 
               "Employment_Employed_Citizens" = "B23025_004", 
               "Employment_Unemployed_Citizens" = "B23025_005", 
               "Employment_Employed_Labor_Armed_Services" = "B23025_006", 
               "Employment_Not_In_LF" = "B23025_007")

# Separate them by variable type
age_vec <- c("B01002_001")
gender_vec <- c("B01001_001", "B01001_002", "B01001_026")
race_vec <- c("B02001_001", "B02001_002", "B02001_003", "B02001_004", "B02001_005", 
              "B02001_006", "B02001_007", "B02001_008")
lang_vec <- c("B16005_003", "B16005_004", "B16005_005", "B16005_006", "B16005_007", "B16005_008")
income_vec <- c("B17001_002", "B20001_001", "B20001_002", "B20001_023")
housing_vec <- c("B25024_001", "B25024_002", "B25024_003", "B25024_004", "B25024_005", 
                 "B25024_006", "B25024_007", "B25024_008", "B25024_009", "B25024_010", "B25024_011")
housingocc_vec <- c("B25014_001", "B25014_002", "B25014_003", "B25014_004", 
                    "B25014_005", "B25014_006", "B25014_007")
subfamily_vec <- c("B11013_001", "B11013_002", "B11013_003")
diability_vec <- c("B21100_001", "B21100_002")
pubassist_vec <- c("B19057_001", "B19057_002")
employment_vec <- c("B23025_001", "B23025_002", "B23025_003", "B23025_004", 
                    "B23025_005", "B23025_006", "B23025_007")
insurance_vec <- c("B27001_001", "B27001_002", "B27001_003", "B27001_004", "B27001_005", 
                   "B27001_006", "B27001_007", "B27001_008", "B27001_009", "B27001_010", 
                   "B27001_011", "B27001_012", "B27001_013", "B27001_014", "B27001_015", 
                   "B27001_016", "B27001_017", "B27001_018", "B27001_019", "B27001_020", 
                   "B27001_021", "B27001_022", "B27001_023", "B27001_024", "B27001_025", 
                   "B27001_026", "B27001_027", "B27001_028", "B27001_029", "B27001_030", 
                   "B27001_031", "B27001_032", "B27001_033", "B27001_034", "B27001_035", 
                   "B27001_036", "B27001_037", "B27001_038", "B27001_039", "B27001_040", 
                   "B27001_041", "B27001_042", "B27001_043", "B27001_044", "B27001_045", 
                   "B27001_046", "B27001_047", "B27001_048", "B27001_049", "B27001_050", 
                   "B27001_051", "B27001_052", "B27001_053", "B27001_054", "B27001_055", 
                   "B27001_056", "B27001_057")

# Download the census data for all variables of interest
# Will raise a warning without API; not an issue unless we pull it 500+ times per day
census_dat <- get_acs(geography = "zcta", variables = c(age_vec, gender_vec, race_vec, lang_vec, income_vec, housing_vec, 
                                                        housingocc_vec, subfamily_vec, diability_vec, pubassist_vec, 
                                                        employment_vec, insurance_vec))
# Split by county zips
census_sd <- census_dat %>% filter(GEOID %in% zip_codes_sd$zipcode)
census_clark <- census_dat %>% filter(GEOID %in% zip_codes_clark$zipcode)

# Function to pull the insurance stuff together
sum_values <- function(df){
  with_ins_vars <- c("B27001_004", "B27001_007", "B27001_010", "B27001_013", "B27001_016", 
                     "B27001_019", "B27001_022", "B27001_025", "B27001_028", "B27001_032", 
                     "B27001_035", "B27001_038", "B27001_041", "B27001_044", "B27001_047", 
                     "B27001_050", "B27001_053", "B27001_056")
  without_ins_vars <- c("B27001_005", "B27001_008", "B27001_011", "B27001_014", "B27001_017", 
                        "B27001_020", "B27001_023", "B27001_026", "B27001_029", "B27001_033", 
                        "B27001_036", "B27001_039", "B27001_042", "B27001_045", "B27001_048", 
                        "B27001_051", "B27001_054", "B27001_057")
  With_Insurance <- c()
  No_Insurance <- c()
  Insurance_All <- c()
  # Loop through each zip code
  for(i in c(1:nrow(df))){
    # Set sums for with/without to 0
    x_with <- 0
    x_without <- 0
    # Loop through the with/without columns & sum
    for(j in c(1:length(with_ins_vars))){
      x_with <- x_with + df[i,with_ins_vars[j]]
      x_without <- x_without + df[i,without_ins_vars[j]]
    }
    # Put sums in vectors and get a "total" column
    With_Insurance[i] = x_with
    No_Insurance[i] = x_without
    Insurance_All[i] = sum(x_with, x_without)
  }
  # Remove everything but the zip code
  df <- df %>% select("GEOID")
  # Add our 3 columns
  df$Insurance_All <- Insurance_All
  df$With_Insurance <- With_Insurance
  df$No_Insurance <- No_Insurance
  
  return(df)
}

# Summarize SD by-zip data
census_sd_by_zip <- census_sd %>% 
  arrange(GEOID, variable) %>% 
  group_by(GEOID, variable) %>%
  # After grouping, this gives us 3 columns: zip code, variable number, and count
  summarise(estimate = estimate) %>%
  # Sets variable as column names and counts as values
  dcast(GEOID ~ variable)  

# Call the function to get insurance counts
sd_by_zip_with_ins <- sum_values(df = census_sd_by_zip %>% 
                                   select(all_of(c("GEOID", insurance_vec)))) 

census_sd_summary <- census_sd_by_zip %>%
  # Remove separate insurance variables
  select(all_of(c("GEOID", age_vec, gender_vec, race_vec, lang_vec, income_vec, housing_vec, housingocc_vec, 
                  subfamily_vec, diability_vec, pubassist_vec, employment_vec, insurance_vec))) %>%
  # Set descriptive names
  rename(all_of(vars_dict)) %>%
  # Add the insurance data to the other items
  left_join(sd_by_zip_with_ins, by = "GEOID") %>% 
  rename(zipcode = "GEOID")

# Summarize Clark by-zip data
census_clark_by_zip <- census_clark %>% 
  arrange(GEOID, variable) %>% 
  group_by(GEOID, variable) %>%
  # After grouping, this gives us 3 columns: zip code, variable number, and count
  summarise(estimate = estimate) %>%
  # Sets variable as column names and counts as values
  dcast(GEOID ~ variable)  

# Call the function to get insurance counts
clark_by_zip_with_ins <- sum_values(df = census_clark_by_zip %>% 
                                      select(all_of(c("GEOID", insurance_vec)))) 

census_clark_summary <- census_clark_by_zip %>%
  # Remove separate insurance variables
  select(all_of(c("GEOID", age_vec, gender_vec, race_vec, lang_vec, income_vec, housing_vec, housingocc_vec, 
                  subfamily_vec, diability_vec, pubassist_vec, employment_vec, insurance_vec))) %>%
  # Set descriptive names
  rename(all_of(vars_dict)) %>%
  # Add the insurance data to the other items
  left_join(clark_by_zip_with_ins, by = "GEOID") %>% 
  rename(zipcode = "GEOID")
#################### END CENSUS DATA ######################

#################### LOAD & FIX PLWH DATA #################
# Load SD PLWH data
load(paste("C:/Users/Geoffrey Lizar/OneDrive - University of California, San Diego Health",
           "/San Diego/Data/Current_Data/UCSD_eHARS_alltables_022023.Rdata", sep = ""))
# Function to fix dates
date.format.export <- '%Y%m%d'
fx.date.fmt <- function(x){as.Date(str_replace_all(x, "[.][.]", "01"),format = date.format.export)}
# date_remove_na <- function(x){
#   if(x == as.Date("01010101", format = "%Y%m%d")){
#     return(NA)}
#   else{return(x)}}
date_cols <- c("vl_first_dt", "vl_recent_dt", "cd4_first_dt", "cd4_recent_dt", 
               "cd4_vl_first_hiv_dt", "cd4_low_cnt_dt", "hiv_aids_dx_dt", "dob", "dod")
# Attach County information
demographics_sd <- demographics %>%
  # These have a bunch of NA values. Remove here, replace in mutate()
  select(-c("vl_first_dt_fmt", "vl_recent_dt_fmt", "cd4_first_dt_fmt", 
            "cd4_recent_dt_fmt", "cd4_vl_first_hiv_dt_fmt", "cd4_low_cnt_dt_fmt", 
            "hiv_aids_dx_dt_fmt", "dob_fmt", "dod_fmt")) %>%
  mutate(
    # Format dates
    across(.cols = all_of(date_cols),.names = '{.col}_fmt',.fns = fx.date.fmt), 
    vl_first_dt_fmt = as.Date(ifelse(vl_first_dt_fmt == as.Date("01010101", "%Y%m%d"), NA, as.character(vl_first_dt_fmt)), "%Y-%m-%d"),
    vl_recent_dt_fmt = as.Date(ifelse(vl_recent_dt_fmt == as.Date("01010101", "%Y%m%d"), NA, as.character(vl_recent_dt_fmt)), "%Y-%m-%d"),
    cd4_first_dt_fmt = as.Date(ifelse(cd4_first_dt_fmt == as.Date("01010101", "%Y%m%d"), NA, as.character(cd4_first_dt_fmt)), "%Y-%m-%d"),
    cd4_recent_dt_fmt = as.Date(ifelse(cd4_recent_dt_fmt == as.Date("01010101", "%Y%m%d"), NA, as.character(cd4_recent_dt_fmt)), "%Y-%m-%d"),
    cd4_vl_first_hiv_dt_fmt = as.Date(ifelse(cd4_vl_first_hiv_dt_fmt == as.Date("01010101", "%Y%m%d"), NA, as.character(cd4_vl_first_hiv_dt_fmt)), "%Y-%m-%d"),
    cd4_low_cnt_dt_fmt = as.Date(ifelse(cd4_low_cnt_dt_fmt == as.Date("01010101", "%Y%m%d"), NA, as.character(cd4_low_cnt_dt_fmt)), "%Y-%m-%d"),
    hiv_aids_dx_dt_fmt = as.Date(ifelse(hiv_aids_dx_dt_fmt == as.Date("01010101", "%Y%m%d"), NA, as.character(hiv_aids_dx_dt_fmt)), "%Y-%m-%d"),
    dob_fmt = as.Date(ifelse(dob_fmt == as.Date("01010101", "%Y%m%d"), NA, as.character(dob_fmt)), "%Y-%m-%d"),
    dod_fmt = as.Date(ifelse(dod_fmt == as.Date("01010101", "%Y%m%d"), NA, as.character(dod_fmt)), "%Y-%m-%d"), 
    # Change zip codes to character values for merging
    cur_zip_cd = as.character(cur_zip_cd),
    rsd_zip_cd = as.character(rsd_zip_cd),
    # 1 for yes, 2 for no
    rsd_in_sd_county = ifelse(rsd_zip_cd %in% zip_codes_sd$zipcode, 1, 0),
    cur_in_sd_county = ifelse(cur_zip_cd %in% zip_codes_sd$zipcode, 1, 0),
    # Impute Gender to M/F/Other; MTF = Female, FTM = Male
    gender_imputed = factor(`Current gender`, 
                            levels = c("Male", NA, "Female", "Transgender-- Male to female", 
                                       "Transgender-- Female to male", "Additional Gender Identity"),
                            labels = c("Male", "Other", "Female", "Female", "Male", "Other"),
                            exclude = NULL)) %>%
  # Set current county
  left_join(zip_code_db, by = c("cur_zip_cd" = "zipcode")) %>%
  rename(cur_county = "county",
         UCI = "UCSD_id") %>%
  # Set rsd county
  left_join(zip_code_db, by = c("rsd_zip_cd" = "zipcode")) %>%
  rename(rsd_county = "county")
# Save SD labs data
labs_sd <- labs %>%
  mutate(sample_year = year(sample_dt_fmt)) %>%
  rename(UCI = "UCSD_id")
labs_sd_distinct <- labs_sd %>% distinct(UCI, sample_year, .keep_all = TRUE)


# Load Clark County PLWH data; overwrites non-altered SD variables
load(paste("C:/Users/Geoffrey Lizar/OneDrive - University of California, San Diego Health",
           "/Clark County/Data/Current_Data/Nevada_eHARS_alltables_040323.Rdata", sep = ""))
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
labs_clark <- labs %>%
  mutate(sample_year = year(sample_dt_fmt))
labs_clark_distinct <- labs_clark %>% distinct(UCI, sample_year, .keep_all = TRUE)

# Get rid of these so we don't accidentally use them
rm("demographics", "genotypes", "genotypes.baseline", "labs", "sti")

# Mark if alive on 1/1/2019
demographics_sd <- demographics_sd %>%
  mutate(alive_1_1_2019 = ifelse(dod == "........", 1, 
                                 ifelse(dod_fmt >= as.Date("20190101", "%Y%m%d"), 1, 0)))
demographics_clarktmp <- demographics_clark %>%
  mutate(alive_1_1_2019 = ifelse(dod == ".", 1, 
                                 ifelse(dod_fmt >= as.Date("20190101", "%Y%m%d"), 1, 0)))
#################### END LOAD & FIX PLWH DATA #############

#################### CALC LINKAGE #########################
# Function to see if someone is linked to care for a given year
# Defined as having a test other than dx in the given year
# Returns JUST A VECTOR; Use to add a column to the demo dataframe
calc_linkage <- function(df, df_labs, link_year, use_labs = TRUE){
  # Possible tests from the demographic dataframe
  demo_tests <- c("vl_first_dt_fmt", "vl_recent_dt_fmt", "cd4_first_dt_fmt", 
                  "cd4_recent_dt_fmt", "cd4_vl_first_hiv_dt_fmt", "cd4_low_cnt_dt_fmt")
  linkage_vec <- c()
  # Filter labs only to the given year
  # df_labs <- df_labs %>% 
  #   filter(sample_year == link_year)
  
  # Loop through each individual
  for(i in c(1:nrow(df))){
    # If they weren't diagnosed, mark as NA
    if(year(df[i,"hiv_aids_dx_dt_fmt"]) > link_year){linkage_vec[i] = NA}
    # If they died before that year, mark as NA
    # Does not mark people who died that year as NA
    else if(!is.na(df[i,"death_year"]) & df[i,"death_year"] < link_year){linkage_vec[i] = NA}
    # If alive and diagnosed, check for linkage
    else{
      test_exists = FALSE
      # We only have labs data for UCSD starting in June 2017
      if(use_labs){
        # If there's a test in the labs data for that year/person, mark 1
        if(nrow(df_labs %>% filter(UCI == df[i,"UCI"])) > 0){
          linkage_vec[i] = 1
          test_exists = TRUE
        }
      }
      # If not, check cd4/viral load dates from demographics data
      if(!test_exists){
        # Loop through demo test columns
        for(test in demo_tests){
          # If there's a test in the given year, mark that a test exists
          if(!is.na(year(df[i, test])) & year(df[i, test]) == link_year){
            test_exists = TRUE
          }
        }
        # If we marked that a test exists, set as linked to care; otherwise not
        if(test_exists){
          linkage_vec[i] = 1
        }else{linkage_vec[i] = 0}
      }
    }
  }
  # Returns JUST THE VECTOR; Use to add a column to the demo dataframe
  return(linkage_vec)
}

# Make new linkage to care columns
# Each line here takes several seconds to run
demo_sd_reduced <- demographics_sd %>% 
  dplyr::select(c("UCI", "death_year", "hiv_aids_dx_dt_fmt", "vl_first_dt_fmt", "vl_recent_dt_fmt", 
                  "cd4_first_dt_fmt", "cd4_recent_dt_fmt", "cd4_vl_first_hiv_dt_fmt", "cd4_low_cnt_dt_fmt"))
labs_sd_reduced <- labs_sd %>% 
  dplyr::select(c("UCI", "sample_year"))
demo_sd_out <- demographics_sd
Sys.time()
# We only have labs data for the whole year starting in 2018; 2011–2017 would be misleading
# demo_sd_out$linked_2011 <- calc_linkage(df = demographics_sd, df_labs = labs_sd_reduced, link_year = 2011, use_labs = FALSE)
# Sys.time()
# demo_sd_out$linked_2012 <- calc_linkage(df = demographics_sd, df_labs = labs_sd_reduced, link_year = 2012, use_labs = FALSE)
# Sys.time()
# demo_sd_out$linked_2013 <- calc_linkage(df = demographics_sd, df_labs = labs_sd_reduced, link_year = 2013, use_labs = FALSE)
# Sys.time()
# demo_sd_out$linked_2014 <- calc_linkage(df = demographics_sd, df_labs = labs_sd_reduced, link_year = 2014, use_labs = FALSE)
# Sys.time()
# demo_sd_out$linked_2015 <- calc_linkage(df = demographics_sd, df_labs = labs_sd_reduced, link_year = 2015, use_labs = FALSE)
# Sys.time()
# demo_sd_out$linked_2016 <- calc_linkage(df = demographics_sd, df_labs = labs_sd_reduced, link_year = 2016, use_labs = FALSE)
# Sys.time()
# demo_sd_out$linked_2017 <- calc_linkage(df = demographics_sd, df_labs = labs_sd_reduced, link_year = 2017)
# Sys.time()
demo_sd_out$linked_2018 <- calc_linkage(df = demo_sd_reduced, 
                                        df_labs = labs_sd_reduced %>% dplyr::filter(sample_year == 2018), 
                                        link_year = 2018)
Sys.time()
demo_sd_out$linked_2019 <- calc_linkage(df = demo_sd_reduced, 
                                        df_labs = labs_sd_reduced %>% dplyr::filter(sample_year == 2019), 
                                        link_year = 2019)
Sys.time()
demo_sd_out$linked_2020 <- calc_linkage(df = demo_sd_reduced, 
                                        df_labs = labs_sd_reduced %>% dplyr::filter(sample_year == 2020), 
                                        link_year = 2020)
Sys.time()
demo_sd_out$linked_2021 <- calc_linkage(df = demo_sd_reduced, 
                                        df_labs = labs_sd_reduced %>% dplyr::filter(sample_year == 2021), 
                                        link_year = 2021)
Sys.time()
demo_sd_out$linked_2022 <- calc_linkage(df = demo_sd_reduced, 
                                        df_labs = labs_sd_reduced %>% dplyr::filter(sample_year == 2022), 
                                        link_year = 2022)
Sys.time()

demo_clark_reduced <- demographics_clark %>% 
  dplyr::select(c("UCI", "death_year", "hiv_aids_dx_dt_fmt", "vl_first_dt_fmt", "vl_recent_dt_fmt", 
                  "cd4_first_dt_fmt", "cd4_recent_dt_fmt", "cd4_vl_first_hiv_dt_fmt", "cd4_low_cnt_dt_fmt"))
demo_clark_out <- demographics_clark
Sys.time()
demo_clark_out$linked_2011 <- calc_linkage(df = demo_clark_reduced, df_labs = labs_clark, link_year = 2011)
Sys.time()
demo_clark_out$linked_2012 <- calc_linkage(df = demo_clark_reduced, df_labs = labs_clark, link_year = 2012)
Sys.time()
demo_clark_out$linked_2013 <- calc_linkage(df = demo_clark_reduced, df_labs = labs_clark, link_year = 2013)
Sys.time()
demo_clark_out$linked_2014 <- calc_linkage(df = demo_clark_reduced, df_labs = labs_clark, link_year = 2014)
Sys.time()
demo_clark_out$linked_2015 <- calc_linkage(df = demo_clark_reduced, df_labs = labs_clark, link_year = 2015)
Sys.time()
demo_clark_out$linked_2016 <- calc_linkage(df = demo_clark_reduced, df_labs = labs_clark, link_year = 2016)
Sys.time()
demo_clark_out$linked_2017 <- calc_linkage(df = demo_clark_reduced, df_labs = labs_clark, link_year = 2017)
Sys.time()
demo_clark_out$linked_2018 <- calc_linkage(df = demo_clark_reduced, df_labs = labs_clark, link_year = 2018)
Sys.time()
demo_clark_out$linked_2019 <- calc_linkage(df = demo_clark_reduced, df_labs = labs_clark, link_year = 2019)
Sys.time()
demo_clark_out$linked_2020 <- calc_linkage(df = demo_clark_reduced, df_labs = labs_clark, link_year = 2020)
Sys.time()
demo_clark_out$linked_2021 <- calc_linkage(df = demo_clark_reduced, df_labs = labs_clark, link_year = 2021)
Sys.time()
demo_clark_out$linked_2022 <- calc_linkage(df = demo_clark_reduced, df_labs = labs_clark, link_year = 2022)
Sys.time()
#################### END CALC LINKAGE #####################


#################### CALC CLUSTERING ######################
### Formatting  San Diego
distances_sd <- read.csv(paste("C:/Users/Geoffrey Lizar/OneDrive - University of California, San Diego Health/", 
                               "San Diego/Data/Current_Data/input_baseline.pwd.limit.0.015.csv", sep = ""))
# Duplicate columns for separation
distances_sd$fastaID1=distances_sd$ID1
distances_sd$fastaID2=distances_sd$ID2
# Separate into pid & date
distances_sd<- separate(distances_sd,col=fastaID1,into=c("pid1","date1"),sep="\\_")
distances_sd<- separate(distances_sd,col=fastaID2,into=c("pid2","date2"),sep="\\_")
# format dates
distances_sd <- distances_sd%>%
  mutate(
    sampling_date1=as.Date(distances_sd$date1,format="%Y%m%d") %>% format("20%y%m%d") %>% as.Date("%Y%m%d"),
    sampling_date2=as.Date(distances_sd$date2,format="%Y%m%d") %>% format("20%y%m%d") %>% as.Date("%Y%m%d"))
# filter out older sequences from same pairs
# Start by splitting into left & right
distances_sd_a <- distances_sd %>% 
  dplyr::select(ID1,pid1,sampling_date1) %>% 
  rename(ID = ID1, 
         pid = pid1, 
         sampling_date = sampling_date1)
distances_sd_b <- distances_sd %>% 
  dplyr::select(ID2,pid2,sampling_date2) %>% 
  rename(ID = ID2, 
         pid = pid2, 
         sampling_date = sampling_date2)
# Stack one on the other
distances_sd_unique <- bind_rows(distances_sd_a, distances_sd_b)
distances_sd_unique <- distances_sd_unique%>%
  # Filter duplicates
  distinct()%>%
  # Arrange by oldest on top for each pid
  arrange(pid,sampling_date)%>%
  # Grab the oldest date per pid
  group_by(pid)%>%
  slice(1)%>%
  ungroup()
# Filter distances_sd based on previous filtering
distances_sd <- distances_sd%>%
  dplyr::filter(ID1 %in% distances_sd_unique$ID & ID2 %in% distances_sd_unique$ID)%>%
  dplyr::filter(!(ID1 == ID2) & !(pid1 == pid2) &  Distance < 0.015)

# Now we do the clark distances
### Formatting  Clark County
distances_clark <- read.csv(paste("C:/Users/Geoffrey Lizar/OneDrive - University of California, San Diego Health/",
                                  "Clark County/Data/Current_Data/input_baseline.pwd.limit.0.015.csv", sep = ""))
distances_clark$fastaID1=distances_clark$ID1
distances_clark$fastaID2=distances_clark$ID2
distances_clark<- separate(distances_clark,col=fastaID1,into=c("pid1","date1"),sep="\\_")
distances_clark<- separate(distances_clark,col=fastaID2,into=c("pid2","date2"),sep="\\_")
distances_clark <- distances_clark%>%
  mutate(sampling_date1=as.Date(as.Date(paste0("01", date1, sep = ""), "%d%b%Y") %>% format("%Y%m%d"), "%Y%m%d"),
         sampling_date2=as.Date(as.Date(paste0("01", date2, sep = ""), "%d%b%Y") %>% format("%Y%m%d"), "%Y%m%d"),
         date1=as.Date(as.Date(paste0("01", date1, sep = ""), "%d%b%Y") %>% format("%Y%m%d"), "%Y%m%d"),
         date2=as.Date(as.Date(paste0("01", date2, sep = ""), "%d%b%Y") %>% format("%Y%m%d"), "%Y%m%d"),)


# filter out older sequences from same pairs
distances_clark_a <- distances_clark%>% dplyr::select(c(ID1,pid1,sampling_date1))%>%rename(ID = ID1, pid = pid1, sampling_date = sampling_date1)
distances_clark_b <- distances_clark%>% dplyr::select(c(ID2,pid2,sampling_date2))%>%rename(ID = ID2, pid = pid2, sampling_date = sampling_date2)

distances_clark_unique <- bind_rows(distances_clark_a, distances_clark_b)
distances_clark_unique <- distances_clark_unique%>%
  distinct()%>%
  arrange(pid,sampling_date)%>%
  group_by(pid)%>%
  slice(1)%>%
  ungroup()
distances_clark <- distances_clark%>%
  dplyr::filter(ID1 %in% distances_clark_unique$ID & ID2 %in% distances_clark_unique$ID)%>%
  dplyr::filter(!(ID1 == ID2) & !(pid1 == pid2) &  Distance < 0.015)


build_clusters <- function(distances, filter_year){
  # Eliminate edges that didn't exist yet
  distances <- distances %>%
    dplyr::filter(year(sampling_date1) <= filter_year & 
                   year(sampling_date2) <= filter_year)
  
  # Burner column
  distances$label=1
  
  # This is the clustering bit
  nodes=gather(distances[,c(1,2,10)],id,label,ID1:ID2)
  nodes=nodes[!duplicated(nodes[,2]), ]
  #id has to be the same like from and to columns in edges
  nodes$id <- nodes$label
  # edges
  edges=distances[,1:3]
  colnames(edges) <- c("from", "to", "width")
  edges$width=1
  #Create graph for Louvain
  graph <- graph_from_data_frame(edges, directed = FALSE)
  #Louvain Comunity Detection
  cluster <- cluster_louvain(graph)
  cluster_df <- data.frame(as.list(membership(cluster)))
  cluster_df <- as.data.frame(t(cluster_df))
  cluster_df$label <- gsub("X", "",x = rownames(cluster_df))
  cluster_df <- cluster_df %>% 
    rename(clusterID = V1) %>% 
    mutate(temp_col = label) %>%
    separate(col=temp_col,into=c("pid","date"),sep="\\_") %>% 
    select(c(clusterID, pid))
  
  # Generates a table of cluster IDs and sizes
  cluster_size_df=as.data.frame(table(cluster_df$clusterID))
  colnames(cluster_size_df)=c("clusterID","size")
  
  clust_3_plus <- cluster_size_df %>%
    dplyr::filter(size >= 3)
    
  cluster_df_out <- cluster_df %>%
    dplyr::filter(clusterID %in% clust_3_plus$clusterID)
  return(cluster_df_out$pid)
}

for(i in c(2011:2022)){
  # San Diego County
  # Returns a list of pids in clusters
  pid_in_cluster <- build_clusters(distances = distances_sd, filter_year = i)
  clustered <- c()
  # Loop through every pid in the demographics file
  for(j in c(1:nrow(demo_sd_out))){
    # If it's in a cluster, mark as 1. Otherwise 0
    if(demo_sd_out[j,"UCI"] %in% pid_in_cluster){
      clustered[j] = 1
    }else{clustered[j] = 0}
  }
  # Add our column for the given year
  demo_sd_out[paste("clustered_", as.character(i), sep = "")] <- clustered
  
  # And now we do the same for Clark County
  # Min date for Clark County clustering is 2/1/2018
  if(i > 2017){
    pid_in_cluster <- build_clusters(distances = distances_clark, filter_year = i)
    clustered <- c()
    for(j in c(1:nrow(demo_clark_out))){
      if(demo_clark_out[j,"UCI"] %in% pid_in_cluster){
        clustered[j] = 1
      }else{clustered[j] = 0}
    }
    demo_clark_out[paste("clustered_", as.character(i), sep = "")] <- clustered
  }
}
#################### END CALC CLUSTERING ##################

#################### STAGE OF CARE ########################
# Trim demographics_sd so it only includes people diagnosed pre-2019 and in SD county
demographics_sd.sub <- demographics_sd%>%
  filter(hiv_aids_dx_dt < 20190101)%>%
  # Keep only relevant columns
  select(c(UCI, dod_fmt, hiv_aids_dx_dt_fmt, vl_first_dt_fmt, vl_recent_dt_fmt, vl_first_det_value, 
           vl_recent_value, cd4_first_dt_fmt, cd4_low_cnt_dt_fmt, cd4_recent_dt_fmt, cd4_vl_first_hiv_dt_fmt))%>%
  rename(
    "ID" = UCI,
    "vl_first_dt_demo" = vl_first_dt_fmt, 
    "vl_recent_dt_demo" = vl_recent_dt_fmt, 
    "vl_first_det_value_demo" = vl_first_det_value, 
    "vl_recent_value_demo" = vl_recent_value,
    "cd4_first_dt_demo" = cd4_first_dt_fmt, 
    "cd4_low_dt_demo" = cd4_low_cnt_dt_fmt,
    "cd4_recent_dt_demo" = cd4_recent_dt_fmt, 
    "combined_first_dt_demo" = cd4_vl_first_hiv_dt_fmt)#%>%
# mutate(hiv_aids_dx_dt_dt = as.Date(hiv_aids_dx_dt, format = "%Y%m%d"))


labs_sd_2018 <- labs_sd %>%
  rename("test_type" = lab_test_cd, 
         "ID" = UCI,
         "test_value" = result) %>%
  filter(year(sample_dt_fmt) == 2018) %>%
  distinct(ID, test_type, sample_dt_fmt, .keep_all = TRUE) %>%
  select(-c(result_units, result_interpretation)) 



# Vector of every ID in demographics_sd.sub in order; will be looped through
all_ppts <- sort(unique(demographics_sd.sub$ID))

# By default people are in stage 0. 
# If they're in stage 0 by the end, the else function is broken
stage_vector <- rep(0, length(all_ppts))

# Just curious
Sys.time()

for(i in c(1:length(all_ppts))){
  # Am I making progress?
  if(i%%1000==0){
    print(i)
    # How long is it taking? (my computer takes ~30-60s to run 1000 ppts, limited by RAM availability)
    print(Sys.time())
  }
  
  # Grab info for one ppt
  current_ppt <- all_ppts[i]
  # subset of demographics_sd.sub for just 1 ppt
  demo_temp <- demographics_sd.sub%>%filter(ID == current_ppt)
  # subset of sample.dat.distinct for just 1 ppt; most recent test first
  sample_temp <- labs_sd_2018%>%filter(ID == current_ppt)%>%arrange(desc(sample_dt_fmt))
  
  # If dead, stage 7
  if(!is.na(demo_temp[1, "dod_fmt"]) & year(demo_temp[1, "dod_fmt"]) < 2019){
    stage_vector[i] = 7
    
    # If not dead, but no tests on file, "Stage" 8
  }else if(nrow(sample_temp) == 0){
    
    # Stage 8 is a catch-all for people not in care in 2018 who have not been sorted into stage 3 or 7. Will revisit
    stage_vector[i] = 8
    
    # If the interval between the most recent test and dx_dt <= 30 days, Stage 3
  }else if(nrow(sample_temp) != 0 & (interval(start = as.Date(demo_temp[1, "hiv_aids_dx_dt_fmt"], format = "%Y%m%d"), end = as.Date(sample_temp[1, "sample_dt_fmt"], format = "%Y%m%d")) %/% days(1)) <= 30){
    stage_vector[i] = 3
    
    # If in care in 2018:
  }else{
    
    # If the first test is a VL, we can just get the result and sort stage 4/5
    if(sample_temp[1, "test_type"] == "Viral Load"){
      
      # A VL of 200 or below (or missing) means suppressed. Stage 5
      if(is.na(sample_temp[1, "test_value"]) | sample_temp[1, "test_value"] <= 200){
        stage_vector[i] = 5
        
        # Above 200 means not suppressed. Stage 4
      }else if(!is.na(sample_temp[1, "test_value"]) & sample_temp[1, "test_value"] > 200){
        stage_vector[i] = 4
      }
      
      # If the first test is not a VL, check to see if a VL exists
    }else if("Viral Load" %in% sample_temp$test_type){
      
      # If so, loop through until you find it
      test_found <- FALSE
      for(j in c(2:nrow(sample_temp))){
        
        # Once we find the first VL test, mark test_found = TRUE to stop checking
        if(!test_found & sample_temp[j, "test_type"] == "Viral Load"){
          test_found <- TRUE
          
          # Now we sort
          
          # A VL of 200 or below (or missing) means suppressed. Stage 5
          if(is.na(sample_temp[j, "test_value"]) | sample_temp[j, "test_value"] <= 200){
            stage_vector[i] = 5
            
            # Above 200 means not suppressed. Stage 4
          }else if(!is.na(sample_temp[j, "test_value"]) & sample_temp[j, "test_value"] > 200){
            stage_vector[i] = 4
          }
        }
      }
      
      # If we don't find a VL test, mark as Stage 4. (Assume not suppressed)
    }else{
      stage_vector[i] = 4
    }
  }
}

# merge our vectors into a dataframe
dsl_df <- data.frame(all_ppts, stage_vector)
dsl_df <- dsl_df%>%
  rename("ID" = all_ppts,
         "stage" = stage_vector)
# Set the rownames for easy indexing
rownames(dsl_df) = dsl_df[,"ID"]

# Now we want to loop through the list of people in stage 8 to see if any of them are actually stage 6
# Set our test types to be cycled through
all_tests <- c("vl_first_dt_demo", "cd4_first_dt_demo", "combined_first_dt_demo", 
               "vl_recent_dt_demo", "cd4_low_dt_demo", "cd4_recent_dt_demo")
# Filter to just stage 8
stage.8.df <- dsl_df%>%filter(stage == 8)
# Join in the demo information; keep only the necessary columns
demo.stage.8 <- left_join(stage.8.df, demographics_sd.sub%>%select(ID, hiv_aids_dx_dt_fmt, all_tests, vl_first_det_value_demo, vl_recent_value_demo), by = "ID")#%>%
# mutate(combined_first_dt_demo = ifelse(substr(combined_first_dt_demo, 7, 8) == "..", 
#                                        ifelse(substr(combined_first_dt_demo, 5, 8) == "....", 
#                                               paste(substr(combined_first_dt_demo, 1, 4), "0101", sep = ""), 
#                                               paste(substr(combined_first_dt_demo, 1, 6), "01", sep = "")), 
#                                        combined_first_dt_demo),
#        combined_first_dt_demo = as.Date(as.character(combined_first_dt_demo), format = "%Y-%m-%d"),
#        cd4_low_dt_demo = ifelse(substr(cd4_low_dt_demo, 7, 8) == "..", 
#                                 ifelse(substr(cd4_low_dt_demo, 5, 8) == "....", 
#                                        paste(substr(cd4_low_dt_demo, 1, 4), "0101", sep = ""), 
#                                        paste(substr(cd4_low_dt_demo, 1, 6), "01", sep = "")), 
#                                 cd4_low_dt_demo),
#        cd4_low_dt_demo = as.Date(cd4_low_dt_demo, format = "%Y%m%d"))
rownames(demo.stage.8) = demo.stage.8[,"ID"]


# Cycle through stage 8 people and check demographics_sd tests
Sys.time()
for(i in rownames(demo.stage.8)){ 
  max_dt <- 0
  vl_first_dt <- 0
  vl_first_val <- 0
  vl_recent_dt <- 0
  vl_recent_val <- 0
  test_in_2018 <- F
  # Cycle through all possible tests
  for(test_name in all_tests){
    # Checking that test exists & is before 1/1/2019
    if(dsl_df[i, "stage"] == 8 & !is.na(demo.stage.8[i, test_name]) & demo.stage.8[i, test_name] < 20190101){
      # Update max_dt
      if(demo.stage.8[i, test_name] > max_dt){
        max_dt <- demo.stage.8[i, test_name]
      }
      
      # If the test is in 2018, ppt is in stage 4 or 5
      if(demo.stage.8[i, test_name] >=20180101){
        test_in_2018 <- T
        if(test_name == "vl_first_dt_demo"){
          vl_first_dt <- demo.stage.8[i, test_name]
          vl_first_val <- demo.stage.8[i, "vl_first_det_value_demo"]
        }
        else if(test_name == "vl_recent_dt_demo"){
          vl_recent_dt <- demo.stage.8[i, test_name]
          vl_recent_val <- demo.stage.8[i, "vl_recent_value_demo"]
        }
      }
    }
  }
  # If we haven't already changed the stage and max_dt has been set
  if(dsl_df[i, "stage"] == 8 & max_dt != 0){
    # If the interval is 30 days or less, stage is 3
    if((interval(start = demo.stage.8[i, "hiv_aids_dx_dt_fmt"], end = max_dt) %/% days(1)) <= 30){
      dsl_df[i, "stage"] = 3
    }
    # If not stage 3, if there's a test in 2018, stage is 4 or 5
    if(test_in_2018 == T){
      # If there's a 2018 VL test
      if(vl_recent_dt != 0 | vl_first_dt != 0){
        # Check which date is more recent; vl_first_dt should be higher
        # "Assuming vl_recent_dt is not na: if vl_first_dt is na OR vl_recent_dt > vl_first_dt, then..."
        if(!is.na(vl_recent_dt) & (is.na(vl_first_dt) | vl_recent_dt > vl_first_dt)){
          # If the test value is missing or <= 200, mark stage 5
          if(is.na(vl_recent_val) | vl_recent_val <= 200){
            dsl_df[i, "stage"] = 5
          }
          # If the test value is > 200, mark stage 4
          else if(vl_recent_val > 200){
            dsl_df[i, "stage"] = 4
          }
        }
        # If the most recent test info is vl_first_dt
        else if(is.na(vl_recent_dt) | vl_first_dt > vl_recent_dt){
          # If the test value is missing or <= 200, mark stage 5
          if(is.na(vl_first_val) | vl_first_val <= 200){
            dsl_df[i, "stage"] = 5
          }
          # If the test value is > 200, mark stage 4
          else if(vl_first_val > 200){
            dsl_df[i, "stage"] = 4
          }
        }
      }
      # If there was a 2018 non-VL test and no 2018 VL tests, mark stage 4
      else{
        dsl_df[i, "stage"] = 4
      }
    }
    # If not stages 3-5, if the interval between test & dx is >30 days, mark stage 6
    else if((interval(start = demo.stage.8[i, "hiv_aids_dx_dt_fmt"], end = max_dt) %/% days(1)) > 30){
      dsl_df[i, "stage"] = 6
    }
  }
}
Sys.time()

demo_sd_out <- left_join(demo_sd_out, dsl_df, by = c("UCI" = "ID")) %>%
  mutate(stage_3 = ifelse(stage == 3, 1, 0),
         stage_4 = ifelse(stage == 4, 1, 0),
         stage_5 = ifelse(stage == 5, 1, 0),
         stage_6 = ifelse(stage == 6, 1, 0),
         stage_7 = ifelse(stage == 7, 1, 0))
#################### END STAGE OF CARE ####################

#################### SUMMARISE AND SAVE DATA ##############
demo_summary_sd <- demo_sd_out %>% 
  arrange(rsd_zip_cd) %>%
  group_by(rsd_zip_cd) %>%
  summarise(
    total_linked_2018 = sum(linked_2018, na.rm = TRUE), 
    percent_linked_2018 = (sum(linked_2018, na.rm = TRUE)/sum(!is.na(linked_2018))), 
    total_linked_2019 = sum(linked_2019, na.rm = TRUE), 
    percent_linked_2019 = (sum(linked_2019, na.rm = TRUE)/sum(!is.na(linked_2019))), 
    total_linked_2020 = sum(linked_2020, na.rm = TRUE), 
    percent_linked_2020 = (sum(linked_2020, na.rm = TRUE)/sum(!is.na(linked_2020))), 
    total_linked_2021 = sum(linked_2021, na.rm = TRUE), 
    percent_linked_2021 = (sum(linked_2021, na.rm = TRUE)/sum(!is.na(linked_2021))), 
    total_linked_2022 = sum(linked_2022, na.rm = TRUE), 
    percent_linked_2022 = (sum(linked_2022, na.rm = TRUE)/sum(!is.na(linked_2022))),
    total_clustered_2011 = sum(clustered_2011, na.rm = TRUE), 
    percent_clustered_2011 = (sum(clustered_2011, na.rm = TRUE)/sum(!is.na(clustered_2011))), 
    total_clustered_2012 = sum(clustered_2012, na.rm = TRUE), 
    percent_clustered_2012 = (sum(clustered_2012, na.rm = TRUE)/sum(!is.na(clustered_2012))), 
    total_clustered_2013 = sum(clustered_2013, na.rm = TRUE), 
    percent_clustered_2013 = (sum(clustered_2013, na.rm = TRUE)/sum(!is.na(clustered_2013))), 
    total_clustered_2014 = sum(clustered_2014, na.rm = TRUE), 
    percent_clustered_2014 = (sum(clustered_2014, na.rm = TRUE)/sum(!is.na(clustered_2014))), 
    total_clustered_2015 = sum(clustered_2015, na.rm = TRUE), 
    percent_clustered_2015 = (sum(clustered_2015, na.rm = TRUE)/sum(!is.na(clustered_2015))), 
    total_clustered_2016 = sum(clustered_2016, na.rm = TRUE), 
    percent_clustered_2016 = (sum(clustered_2016, na.rm = TRUE)/sum(!is.na(clustered_2016))), 
    total_clustered_2017 = sum(clustered_2017, na.rm = TRUE), 
    percent_clustered_2017 = (sum(clustered_2017, na.rm = TRUE)/sum(!is.na(clustered_2017))), 
    total_clustered_2018 = sum(clustered_2018, na.rm = TRUE), 
    percent_clustered_2018 = (sum(clustered_2018, na.rm = TRUE)/sum(!is.na(clustered_2018))), 
    total_clustered_2019 = sum(clustered_2019, na.rm = TRUE), 
    percent_clustered_2019 = (sum(clustered_2019, na.rm = TRUE)/sum(!is.na(clustered_2019))), 
    total_clustered_2020 = sum(clustered_2020, na.rm = TRUE), 
    percent_clustered_2020 = (sum(clustered_2020, na.rm = TRUE)/sum(!is.na(clustered_2020))), 
    total_clustered_2021 = sum(clustered_2021, na.rm = TRUE), 
    percent_clustered_2021 = (sum(clustered_2021, na.rm = TRUE)/sum(!is.na(clustered_2021))), 
    total_clustered_2022 = sum(clustered_2022, na.rm = TRUE), 
    percent_clustered_2022 = (sum(clustered_2022, na.rm = TRUE)/sum(!is.na(clustered_2022))), 
    total_stage_3 = sum(stage_3, na.rm = TRUE),
    percent_stage_3 = (sum(stage_3, na.rm = TRUE)/sum(!is.na(stage_3))),
    total_stage_4 = sum(stage_4, na.rm = TRUE),
    percent_stage_4 = (sum(stage_4, na.rm = TRUE)/sum(!is.na(stage_4))),
    total_stage_5 = sum(stage_5, na.rm = TRUE),
    percent_stage_5 = (sum(stage_5, na.rm = TRUE)/sum(!is.na(stage_5))),
    total_stage_6 = sum(stage_6, na.rm = TRUE),
    percent_stage_6 = (sum(stage_6, na.rm = TRUE)/sum(!is.na(stage_6))),
    total_stage_7 = sum(stage_7, na.rm = TRUE),
    percent_stage_7 = (sum(stage_7, na.rm = TRUE)/sum(!is.na(stage_7)))
  ) %>%
  dplyr::filter(!is.na(rsd_zip_cd))
sd_all_vars_by_zip <- full_join(census_sd_summary,
                                   demo_summary_sd, by = c("zipcode" = "rsd_zip_cd"))



demo_summary_clark <- demo_clark_out %>% 
  arrange(rsd_zip_cd) %>%
  group_by(rsd_zip_cd) %>%
  summarise(
    total_linked_2011 = sum(linked_2011, na.rm = TRUE), 
    percent_linked_2011 = (sum(linked_2011, na.rm = TRUE)/sum(!is.na(linked_2011))), 
    total_linked_2012 = sum(linked_2012, na.rm = TRUE), 
    percent_linked_2012 = (sum(linked_2012, na.rm = TRUE)/sum(!is.na(linked_2012))), 
    total_linked_2013 = sum(linked_2013, na.rm = TRUE), 
    percent_linked_2013 = (sum(linked_2013, na.rm = TRUE)/sum(!is.na(linked_2013))), 
    total_linked_2014 = sum(linked_2014, na.rm = TRUE), 
    percent_linked_2014 = (sum(linked_2014, na.rm = TRUE)/sum(!is.na(linked_2014))), 
    total_linked_2015 = sum(linked_2015, na.rm = TRUE), 
    percent_linked_2015 = (sum(linked_2015, na.rm = TRUE)/sum(!is.na(linked_2015))), 
    total_linked_2016 = sum(linked_2016, na.rm = TRUE), 
    percent_linked_2016 = (sum(linked_2016, na.rm = TRUE)/sum(!is.na(linked_2016))), 
    total_linked_2017 = sum(linked_2017, na.rm = TRUE), 
    percent_linked_2017 = (sum(linked_2017, na.rm = TRUE)/sum(!is.na(linked_2017))), 
    total_linked_2018 = sum(linked_2018, na.rm = TRUE), 
    percent_linked_2018 = (sum(linked_2018, na.rm = TRUE)/sum(!is.na(linked_2018))), 
    total_linked_2019 = sum(linked_2019, na.rm = TRUE), 
    percent_linked_2019 = (sum(linked_2019, na.rm = TRUE)/sum(!is.na(linked_2019))), 
    total_linked_2020 = sum(linked_2020, na.rm = TRUE), 
    percent_linked_2020 = (sum(linked_2020, na.rm = TRUE)/sum(!is.na(linked_2020))), 
    total_linked_2021 = sum(linked_2021, na.rm = TRUE), 
    percent_linked_2021 = (sum(linked_2021, na.rm = TRUE)/sum(!is.na(linked_2021))), 
    total_linked_2022 = sum(linked_2022, na.rm = TRUE), 
    percent_linked_2022 = (sum(linked_2022, na.rm = TRUE)/sum(!is.na(linked_2022))),
    total_clustered_2018 = sum(clustered_2018, na.rm = TRUE), 
    percent_clustered_2018 = (sum(clustered_2018, na.rm = TRUE)/sum(!is.na(clustered_2018))), 
    total_clustered_2019 = sum(clustered_2019, na.rm = TRUE), 
    percent_clustered_2019 = (sum(clustered_2019, na.rm = TRUE)/sum(!is.na(clustered_2019))), 
    total_clustered_2020 = sum(clustered_2020, na.rm = TRUE), 
    percent_clustered_2020 = (sum(clustered_2020, na.rm = TRUE)/sum(!is.na(clustered_2020))), 
    total_clustered_2021 = sum(clustered_2021, na.rm = TRUE), 
    percent_clustered_2021 = (sum(clustered_2021, na.rm = TRUE)/sum(!is.na(clustered_2021))), 
    total_clustered_2022 = sum(clustered_2022, na.rm = TRUE), 
    percent_clustered_2022 = (sum(clustered_2022, na.rm = TRUE)/sum(!is.na(clustered_2022)))
  ) %>%
  dplyr::filter(!is.na(rsd_zip_cd))
# Replace NA percents with 0
# Should ask if this is wanted or not
# demo_summary_clark <- replace(demo_summary_clark, is.na(demo_summary_clark), 0)
clark_all_vars_by_zip <- full_join(census_clark_summary,
                                   demo_summary_clark, by = c("zipcode" = "rsd_zip_cd"))


save(clark_all_vars_by_zip, sd_all_vars_by_zip, demo_clark_out, demo_sd_out,
     file = "Census_and_PLWH_summaries.RData")
write.csv(clark_all_vars_by_zip, file = "clark_county_by_zip.csv", row.names = FALSE)
write.csv(sd_all_vars_by_zip, file = "sd_county_by_zip.csv", row.names = FALSE)
write.csv(demo_clark_out, file = "clark_county_demographics.csv", row.names = FALSE)
write.csv(demo_sd_out, file = "sd_county_demographics.csv", row.names = FALSE)
#################### END SUMMARISE AND SAVE DATA ##########