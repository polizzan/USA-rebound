####################
# LOAD 2019 DEATHS #
####################

## create data frame with column widths and variable names
## 1) column widths
columns.2019 <- data.frame(widths = c(19, 1, 
                                      40, 
                                      2, 1, 1, 2, 
                                      2, 1, 4, 1, 
                                      2, 
                                      2, 2, 
                                      2, 1, 1, 
                                      1, 16, 4, 1, 1, 1, 1, 
                                      34, 1, 1, 4, 3, 1, 3, 
                                      3, 2, 1, 2, 140, 
                                      36, 2, 1, 100, 
                                      1, 2,
                                      1, 1,
                                      1, 1, 33, 3, 1, 1, 2))

## 2) variable names
columns.2019$names <- c("Reserved_Positions_1", "Resident_Status", 
                        "Reserved_Positions_2", 
                        "Education_1989", "Education_2003", "Education_reporting_flag", "Month_of_Death", 
                        "Reserved_Positions_3", "Sex", "Detail_Age", "Age_Substitution_Flag", 
                        "Age_Recode_52",
                        "Age_Recode_27", "Age_Recode_12",
                        "Infant_Age_Recode_22", "Place_of_Death_and_Decedents_Status", "Marital_Status",
                        "Day_of_Week_of_Death", "Reserved_Positions_4", "Current_Data_Year", "Injury_at_Work", "Manner_of_Death", "Method_of_Disposition", "Autopsy", 
                        "Reserved_Positions_5", "Activity_Code", "Place_of_Injury", "ICD_Code_10", "Cause_Recode_358", "Reserved_Positions_6", "Cause_Recode_113", 
                        "Infant_Cause_Recode_130", "Cause_Recode_39", "Reserved_Positions_7", "Number_of_Entity_Axis_Conditions", "Entity_Axis_Conditions", 
                        "Reserved_Positions_8", "Number_of_Record_Axis_Conditions", "Reserved_Positions_9", "Record_Axis_Conditions", 
                        "Reserved_Positions_10", "Bridged_Race",
                        "Bridged_Race_Flag", "Race_Imputation_Flag", 
                        "Bridged_Race_Recode_3", "Bridged_Race_Recode_5", "Reserved_Positions_11", "Hispanic_Origin", "Reserved_Positions_12", "Hispanic_Origin_Bridged_Race_Recode", "Race_Recode_40")

## read in mortality file
deaths.2019.raw.long <- laf_open_fwf(here::here("in-data", "mort2019us", "VS19MORT.DUSMCPUB_r20210304"), 
                                     column_widths = columns.2019$widths, 
                                     column_types = rep("character", length(columns.2019$names)),
                                     column_names = columns.2019$names)

# select variables
deaths.2019.raw.short <- 
  deaths.2019.raw.long[, c("Resident_Status", "Sex", "Detail_Age", "ICD_Code_10")] %>% 
  ## restrict to residents
  filter(Resident_Status %in% c("1", "2", "3")) %>% 
  select(-Resident_Status,
         Sex,
         Age = Detail_Age,
         Cause = ICD_Code_10) %>% 
  mutate(Year = 2019)

####################
# LOAD 2020 DEATHS #
####################

## create data frame with column widths and variable names
## 1) column widths
columns.2020 <- data.frame(widths = c(19, 1, 
                                      40, 
                                      2, 1, 1, 2, 2, 1, 
                                      4, 1, 2, 
                                      2, 
                                      2, 2, 
                                      1, 1, 1, 16, 4, 1, 
                                      1, 1, 1, 34, 1, 1, 
                                      4, 3, 1, 3, 3, 2, 1, 2, 140, 
                                      36, 2, 1, 
                                      100, 
                                      1, 2,
                                      1, 1, 1, 1, 33, 3, 1, 
                                      1, 2,
                                      315, 4, 2,
                                      4, 2))

## 2) variable names
columns.2020$names <- c("Reserved_Positions_1", "Resident_Status", 
                        "Reserved_Positions_2", 
                        "Education_1989", "Education_2003", "Education_reporting_flag", "Month_of_Death", "Reserved_Positions_3", "Sex", 
                        "Detail_Age", "Age_Substitution_Flag", "Age_Recode_52",
                        "Age_Recode_27", 
                        "Age_Recode_12", "Infant_Age_Recode_22", 
                        "Place_of_Death_and_Decedents_Status", "Marital_Status", "Day_of_Week_of_Death", "Reserved_Positions_4", "Current_Data_Year", "Injury_at_Work", 
                        "Manner_of_Death", "Method_of_Disposition", "Autopsy", "Reserved_Positions_5", "Activity_Code", "Place_of_Injury", 
                        "ICD_Code_10", "Cause_Recode_358", "Reserved_Positions_6", "Cause_Recode_113", "Infant_Cause_Recode_130", "Cause_Recode_39", "Reserved_Positions_7", "Number_of_Entity_Axis_Conditions", "Entity_Axis_Conditions", 
                        "Reserved_Positions_8", "Number_of_Record_Axis_Conditions", "Reserved_Positions_9", 
                        "Record_Axis_Conditions", 
                        "Reserved_Positions_10", "Race",
                        "Bridged_Race_Flag", "Race_Imputation_Flag", "Race_Recode_3", "Race_Recode_5", "Reserved_Positions_11", "Hispanic_Origin", "Reserved_Positions_12", 
                        "Hispanic_Origin_Race_Recode", "Race_Recode_40",
                        "Reserved_Positions_13", "Occupation_4_digit_code", "Occupation_recode",
                        "Industry_4_digit_code", "Industry_recode")

## read in mortality file
deaths.2020.raw.long <- laf_open_fwf(here::here("in-data", "mort2020us", "VS20MORT.DUSMCPUB_r20220105"), 
                                     column_widths = columns.2020$widths, 
                                     column_types = rep("character", length(columns.2020$names)),
                                     column_names = columns.2020$names)

# select variables
deaths.2020.raw.short <- 
  deaths.2020.raw.long[, c("Resident_Status", "Sex", "Detail_Age", "ICD_Code_10")] %>% 
  ## restrict to residents
  filter(Resident_Status %in% c("1", "2", "3")) %>% 
  select(-Resident_Status,
         Sex,
         Age = Detail_Age,
         Cause = ICD_Code_10) %>% 
  mutate(Year = 2020)

####################
# LOAD 2021 DEATHS #
####################

## create data frame with column widths and variable names
## 1) column widths
columns.2021 <- data.frame(widths = c(18, 1, 1, 42, 1, 1,
                                      2, 2, 1, 4, 1, 
                                      2, 
                                      2, 2, 
                                      2, 1, 1, 
                                      1, 16, 4, 1, 1, 1, 1, 34, 
                                      1, 1, 
                                      4, 3, 1, 3, 3, 2, 1, 
                                      2, 140, 
                                      36, 2, 1, 100, 
                                      4, 1, 35, 3, 2, 2, 
                                      315, 
                                      4, 2,
                                      4, 2))

## 2) variable names
columns.2021$names <- c("Reserved_Positions_1", "Record_Type", "Resident_Status", "Reserved_Positions_2", "Education", "Education_reporting_flag",  
                        "Month_of_Death", "Reserved_Positions_3", "Sex", "Detail_Age", "Age_Substitution_Flag", 
                        "Age_Recode_52",
                        "Age_Recode_27", "Age_Recode_12",
                        "Infant_Age_Recode_22", "Place_of_Death_and_Decedents_Status", "Marital_Status",
                        "Day_of_Week_of_Death", "Reserved_Positions_4", "Current_Data_Year", "Injury_at_Work", "Manner_of_Death", "Method_of_Disposition", "Autopsy", "Reserved_Positions_5",
                        "Activity_Code", "Place_of_Injury", 
                        "ICD_Code_10", "Cause_Recode_358", "Reserved_Positions_6", "Cause_Recode_113", "Infant_Cause_Recode_130", "Cause_Recode_39", "Reserved_Positions_7", 
                        "Number_of_Entity_Axis_Conditions", "Entity_Axis_Conditions", 
                        "Reserved_Positions_8", "Number_of_Record_Axis_Conditions", "Reserved_Positions_9", "Record_Axis_Conditions",
                        "Reserved_Positions_10", "Race_Imputation_Flag", "Reserved_Positions_11", "Hispanic_Origin", "Reserved_Positions_12", "Race_Recode_40",
                        "Reserved_Positions_13", 
                        "Occupation_4_digit_code", "Occupation_recode",
                        "Industry_4_digit_code", "Industry_recode")

## read in mortality file
deaths.2021.raw.long <- laf_open_fwf(here::here("in-data", "mort2021us", "VS21MORT.DUSMCPUB_r20230320.txt"), 
                                     column_widths = columns.2021$widths, 
                                     column_types = rep("character", length(columns.2021$names)),
                                     column_names = columns.2021$names)

# select variables
deaths.2021.raw.short <- 
  deaths.2021.raw.long[, c("Resident_Status", "Sex", "Detail_Age", "ICD_Code_10")] %>% 
  ## restrict to residents
  filter(Resident_Status %in% c("1", "2", "3")) %>% 
  select(-Resident_Status,
         Sex,
         Age = Detail_Age,
         Cause = ICD_Code_10) %>% 
  mutate(Year = 2021)

####################
# LOAD 2022 DEATHS #
####################

## create data frame with column widths and variable names
## 1) column widths
columns.2022 <- data.frame(widths = c(18, 1, 1, 42, 1, 1,
                                      2, 2, 1, 4, 1, 
                                      2, 
                                      2, 2, 
                                      2, 1, 1, 
                                      1, 16, 4, 1, 1, 1, 1, 34, 
                                      1, 1, 
                                      4, 3, 1, 3, 3, 2, 1, 
                                      2, 140, 
                                      36, 2, 1, 100, 
                                      4, 1, 1, 1, 33, 3, 
                                      2, 2, 
                                      315, 
                                      4, 2,
                                      4, 2))

## 2) variable names
columns.2022$names <- c("Reserved_Positions_1", "Record_Type", "Resident_Status", "Reserved_Positions_2", "Education", "Education_reporting_flag",  
                        "Month_of_Death", "Reserved_Positions_3", "Sex", "Detail_Age", "Age_Substitution_Flag", 
                        "Age_Recode_52",
                        "Age_Recode_27", "Age_Recode_12",
                        "Infant_Age_Recode_22", "Place_of_Death_and_Decedents_Status", "Marital_Status",
                        "Day_of_Week_of_Death", "Reserved_Positions_4", "Current_Data_Year", "Injury_at_Work", "Manner_of_Death", "Method_of_Disposition", "Autopsy", "Reserved_Positions_5",
                        "Activity_Code", "Place_of_Injury", 
                        "ICD_Code_10", "Cause_Recode_358", "Reserved_Positions_6", "Cause_Recode_113", "Infant_Cause_Recode_130", "Cause_Recode_39", "Reserved_Positions_7", 
                        "Number_of_Entity_Axis_Conditions", "Entity_Axis_Conditions", 
                        "Reserved_Positions_8", "Number_of_Record_Axis_Conditions", "Reserved_Positions_9", "Record_Axis_Conditions", 
                        "Reserved_Positions_10", "Race_Imputation_Flag", "Reserved_Positions_11", "Race_Recode_6", "Reserved_Positions_12", "Hispanic_Origin", 
                        "Hispanic_Origin_Race_Recode", "Race_Recode_40",
                        "Reserved_Positions_13", 
                        "Occupation_4_digit_code", "Occupation_recode",
                        "Industry_4_digit_code", "Industry_recode")

## read in mortality file
deaths.2022.raw.long <- laf_open_fwf(here::here("in-data", "mort2022us", "VS22MORT.DUSMCPUB_r20240307"), 
                                     column_widths = columns.2022$widths, 
                                     column_types = rep("character", length(columns.2022$names)),
                                     column_names = columns.2022$names)

# select variables
deaths.2022.raw.short <- 
  deaths.2022.raw.long[, c("Resident_Status", "Sex", "Detail_Age", "ICD_Code_10")] %>% 
  ## restrict to residents
  filter(Resident_Status %in% c("1", "2", "3")) %>% 
  select(-Resident_Status,
         Sex,
         Age = Detail_Age,
         Cause = ICD_Code_10) %>% 
  mutate(Year = 2022)

####################
# LOAD 2023 DEATHS #
####################

## create data frame with column widths and variable names
## 1) column widths
columns.2023 <- data.frame(widths = c(18, 1, 1, 42, 1, 1, 
                                      2, 2, 1, 4, 1, 
                                      2, 
                                      2, 2, 
                                      2, 1, 1, 
                                      1, 16, 4, 1, 1, 1, 1, 34, 
                                      1, 1, 
                                      4, 3, 1, 3, 3, 2, 1, 
                                      2, 140, 
                                      36, 2, 1, 100, 
                                      4, 1, 1, 1, 33, 3, 
                                      2, 2, 
                                      315, 
                                      4, 2, 
                                      4, 2))

## 2) variable names
columns.2023$names <- c("Reserved_Positions_1", "Record_Type", "Resident_Status", "Reserved_Positions_2", "Education", "Education_reporting_flag",  
                        "Month_of_Death", "Reserved_Positions_3", "Sex", "Detail_Age", "Age_Substitution_Flag", 
                        "Age_Recode_52",
                        "Age_Recode_27", "Age_Recode_12",
                        "Infant_Age_Recode_22", "Place_of_Death_and_Decedents_Status", "Marital_Status",
                        "Day_of_Week_of_Death", "Reserved_Positions_4", "Current_Data_Year", "Injury_at_Work", "Manner_of_Death", "Method_of_Disposition", "Autopsy", "Reserved_Positions_5",
                        "Activity_Code", "Place_of_Injury", 
                        "ICD_Code_10", "Cause_Recode_358", "Reserved_Positions_6", "Cause_Recode_113", "Infant_Cause_Recode_130", "Cause_Recode_39", "Reserved_Positions_7", 
                        "Number_of_Entity_Axis_Conditions", "Entity_Axis_Conditions", 
                        "Reserved_Positions_8", "Number_of_Record_Axis_Conditions", "Reserved_Positions_9", "Record_Axis_Conditions", 
                        "Reserved_Positions_10", "Race_Imputation_Flag", "Reserved_Positions_11", "Race_Recode_6", "Reserved_Positions_12", "Hispanic_Origin", 
                        "Hispanic_Origin_Race_Recode", "Race_Recode_40",
                        "Reserved_Positions_13", 
                        "Occupation_4_digit_code", "Occupation_recode",
                        "Industry_4_digit_code", "Industry_recode")

## read in mortality file
deaths.2023.raw.long <- laf_open_fwf(here::here("in-data", "mort2023us", "VS23MORT.DUSMCPUB_r20241030"), 
                                     column_widths = columns.2023$widths, 
                                     column_types = rep("character", length(columns.2023$names)),
                                     column_names = columns.2023$names)

# select variables
deaths.2023.raw.short <- 
  deaths.2023.raw.long[, c("Resident_Status", "Sex", "Detail_Age", "ICD_Code_10")] %>% 
  ## restrict to residents
  filter(Resident_Status %in% c("1", "2", "3")) %>% 
  select(-Resident_Status,
         Sex,
         Age = Detail_Age,
         Cause = ICD_Code_10) %>% 
  mutate(Year = 2023)

####################
# LOAD 2024 DEATHS #
####################

## create data frame with column widths and variable names
## 1) column widths
columns.2024 <- data.frame(widths = c(18, 1, 1, 42, 1, 1, 
                                      2, 2, 1, 4, 1, 
                                      2, 
                                      2, 2, 
                                      2, 1, 1, 
                                      1, 16, 4, 1, 1, 1, 1, 34, 
                                      1, 1, 4, 3, 1, 3, 3, 
                                      2, 1, 2, 140, 
                                      36, 2, 1, 100, 
                                      4, 1, 1, 1, 
                                      33, 3, 2, 
                                      2, 
                                      315, 4, 2, 
                                      4, 2))

## 2) variable names
columns.2024$names <- c("Reserved_Positions_1", "Record_Type", "Resident_Status", "Reserved_Positions_2", "Education", "Education_reporting_flag",  
                        "Month_of_Death", "Reserved_Positions_3", "Sex", "Detail_Age", "Age_Substitution_Flag", 
                        "Age_Recode_52",
                        "Age_Recode_27", "Age_Recode_12",
                        "Infant_Age_Recode_22", "Place_of_Death_and_Decedents_Status", "Marital_Status",
                        "Day_of_Week_of_Death", "Reserved_Positions_4", "Current_Data_Year", "Injury_at_Work", "Manner_of_Death", "Method_of_Disposition", "Autopsy", "Reserved_Positions_5",
                        "Activity_Code", "Place_of_Injury", "ICD_Code_10", "Cause_Recode_358", "Reserved_Positions_6", "Cause_Recode_113", "Infant_Cause_Recode_130",
                        "Cause_Recode_39", "Reserved_Positions_7", "Number_of_Entity_Axis_Conditions", "Entity_Axis_Conditions", 
                        "Reserved_Positions_8", "Number_of_Record_Axis_Conditions", "Reserved_Positions_9", "Record_Axis_Conditions", 
                        "Reserved_Positions_10", "Race_Imputation_Flag", "Reserved_Positions_11", "Race_Recode_6",
                        "Reserved_Positions_12", "Hispanic_Origin", "Hispanic_Origin_Race_Recode",
                        "Race_Recode_40",
                        "Reserved_Positions_13", "Occupation_4_digit_code", "Occupation_recode",
                        "Industry_4_digit_code", "Industry_recode")

## read in mortality file
deaths.2024.raw.long <- laf_open_fwf(here::here("in-data", "mort2024us", "VS24MORT.DUSMCPUB_r20251208"), 
                                     column_widths = columns.2024$widths, 
                                     column_types = rep("character", length(columns.2024$names)),
                                     column_names = columns.2024$names)

# select variables
deaths.2024.raw.short <- 
  deaths.2024.raw.long[, c("Resident_Status", "Sex", "Detail_Age", "ICD_Code_10")] %>% 
  ## restrict to residents
  filter(Resident_Status %in% c("1", "2", "3")) %>% 
  select(-Resident_Status,
         Sex,
         Age = Detail_Age,
         Cause = ICD_Code_10) %>% 
  mutate(Year = 2024)

##################
# COMBINE DEATHS #
##################

deaths.raw <-
  bind_rows(
    
    deaths.2019.raw.short,
    deaths.2020.raw.short,
    deaths.2021.raw.short,
    deaths.2022.raw.short,
    deaths.2023.raw.short,
    deaths.2024.raw.short
    
  ) %>% 
  ## recode sex variable
  mutate(Sex = ifelse(Sex == "M", "Male", "Female"),
         ## generate numerical age variable
         Age_flag = str_sub(Age, 1, 1),
         Age = ifelse(Age_flag == 9 | Age == 1999, NA, ifelse(Age_flag == 1, as.numeric(str_sub(Age, 2, 4)), 0))) %>% 
  ## count deaths per year-sex-age-cause combination
  summarize(Deaths = n(), .by = c(Year, Sex, Age, Cause))

###############
# SAVE DEATHS #
###############

write.csv(deaths.raw, here::here("out-data", "deaths-2019-2024.csv"), row.names = FALSE)
