library(readxl)
library(dplyr)
library(janitor)
library(ggplot2)
library(tidyr)
library(comorbidity)
library(stringr)
library(lubridate)
library(pROC)
library(naniar)
library(BaylorEdPsych)
library(mvnmle)
library(ggplot2)
library(mice)
library(caret)
library(purrr)
library(knitr)
library(scales)  # for percent_format()
library(flextable)
library(officer)
# Read the admissions csv file
admissions_data <- read.csv("D:/mimic_data/admissions.csv")

# Clean admissions column names
admissions_data <- admissions_data %>%
  clean_names()
View(admissions_data)
#Convert all character data into factor
admissions_data <- admissions_data %>%
  mutate(across(where(is.character), as.factor))
summary(admissions_data)
duplicated(admissions_data$subject_id)
anyDuplicated(admissions_data$subject_id)
table(admissions_data$subject_id)

# See which values appear more than once
table(admissions_data$subject_id)[table(admissions_data$subject_id) > 1]


#Labs hourly
labs_hourly_data <- read.csv("D:/mimic_data/labs_hourly.csv")
#Clean Labs hourly column names
labs_hourly_data <- labs_hourly_data%>%
  clean_names()
View(labs_hourly_data)


#Lots of clearly wrong lab results like 3565 glucose. Creating a cleaning loop based on arbitrarily set range that allows for out of range lab values but not too insane.
# Define plausible ICU ranges for labs only (ignore hr)
# Updated plausible ranges, zero not allowed for any lab
plausible_ranges_labs <- list(
  sodium = c(110, 160),
  glucose = c(40, 600),
  alaninetransaminase = c(1, 10000),
  aspartatetransaminase = c(1, 10000),
  intnormalisedratio = c(0.1, 10),
  chloride = c(80, 120),
  creatinine = c(0.2, 20),
  neutrophil = c(0.1, 100),        # now zero excluded
  bilirubin = c(0.1, 100),
  troponin = c(0.01, 50)        # adjust units as needed
)
# Function to check if a value is extreme
is_extreme <- function(x, range) { ifelse(is.na(x), FALSE, x < range[1] | x > range[2]) }
# Extreme flag calculation
labs_hourly_data_flagged <- labs_hourly_data %>%
  mutate(
    extreme_flag = ifelse(
      is_extreme(sodium, plausible_ranges_labs$sodium) |
        is_extreme(glucose, plausible_ranges_labs$glucose) |
        is_extreme(alaninetransaminase, plausible_ranges_labs$alaninetransaminase) |
        is_extreme(aspartatetransaminase, plausible_ranges_labs$aspartatetransaminase) |
        is_extreme(intnormalisedratio, plausible_ranges_labs$intnormalisedratio) |
        is_extreme(chloride, plausible_ranges_labs$chloride) |
        is_extreme(creatinine, plausible_ranges_labs$creatinine) |
        is_extreme(neutrophil, plausible_ranges_labs$neutrophil) |
        is_extreme(bilirubin, plausible_ranges_labs$bilirubin) |
        is_extreme(troponin, plausible_ranges_labs$troponin),
      TRUE, FALSE
    )
  )

# Filter only rows with extreme lab values,shows only 13390
labs_hourly_data_flagged_only_flagged <- labs_hourly_data_flagged %>%
  filter(extreme_flag == TRUE)
View(labs_hourly_data_flagged_only_flagged)
#Eliminate these flagged 13390 results.
labs_hourly_data_clean <- labs_hourly_data_flagged %>%
  filter(extreme_flag == FALSE )
#Checking how many duplicates, no duplicates yay!
duplicate_counts_labs <- labs_hourly_data_clean %>%
  group_by(icustay_id, hr) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)
duplicate_counts_labs
# View duplicates
duplicate_counts_labs #0

#Viewing how many unique icu ids had lab hour data
# Unique ICU stays in original data
unique_icustays_original_labs <- n_distinct(labs_hourly_data$icustay_id)
unique_icustays_original_labs #60313

# Calculating Unique ICU stays in cleaned data
unique_icustays_clean_labs <- n_distinct(labs_hourly_data_clean$icustay_id)
unique_icustays_clean_labs #60271

#ICU stay information extraction
icu_stays_data <- read.csv("D:/mimic_data/icustays.csv")
#Names cleaned
icu_stays_data <- icu_stays_data%>%
  clean_names()
View(icu_stays_data)
#Checking how many subject ids and hadm ids appear more than once. 
table(icu_stays_data$subject_id)[table(icu_stays_data$subject_id) > 1]
table(icu_stays_data$hadm_id)[table(icu_stays_data$hadm_id) > 1]

summary(icu_stays_data)
#Converting date and time correctly
icu_stays_data <- icu_stays_data %>%
  mutate(
    intime  = ymd_hms(intime,  quiet = TRUE),
    outtime = ymd_hms(outtime, quiet = TRUE)
  )
#Convert care unit into factor
icu_stays_data <- icu_stays_data %>%
  mutate(
    dbsource = factor(dbsource),
    first_careunit = factor(first_careunit),
    last_careunit  = factor(last_careunit)
  )
#checking how many ICU care units changed, result in 3833 True flags, meaning 3833 patients changed their wards.
#Extracting ICU outcomes from file
icu_outcome_data <- read.csv("D:/mimic_data/pt_icu_outcome.csv",
                             stringsAsFactors = FALSE)

# Clean icu outcome column names
icu_outcome_data <- icu_outcome_data%>%
  clean_names()
icu_outcome_data <- icu_outcome_data %>%
  mutate(
    dob         = ymd_hms(dob, quiet = TRUE),
    admittime   = ymd_hms(admittime, quiet = TRUE),
    dischtime   = ymd_hms(dischtime, quiet = TRUE),
    intime      = ymd_hms(intime, quiet = TRUE),
    outtime     = ymd_hms(outtime, quiet = TRUE),
    hosp_deathtime = ymd_hms(hosp_deathtime, quiet = TRUE),
    dod         = ymd_hms(dod, quiet = TRUE)
  ) %>%
  mutate(
    icu_expire_flag = factor(icu_expire_flag),
    hospital_expire_flag = factor(hospital_expire_flag),
    expire_flag = factor(expire_flag)
  )
View(icu_outcome_data)
icu_outcome_data  <- icu_outcome_data  %>%
  mutate(across(where(is.character), as.factor))
table(icu_outcome_data$hadm_id)[table(icu_stays_data$hadm_id) > 1]
table(icu_outcome_data$icu_stay_id)[table(icu_stays_data$icu_stay_id) > 1]
summary(icu_outcome_data)
#icustay_id has identical duplicate in 229922, will delete it.
icu_outcome_data <- icu_outcome_data[!duplicated(icu_outcome_data$icustay_id), ]
#Patient data extraction
patient_data <- read.csv("D:/mimic_data/patients.csv")
# Clean Patient data column names
patient_data <- patient_data%>%
  clean_names()
patient_data <- patient_data %>%
  mutate(across(where(is.character), as.factor))
View(patient_data)
#Vitals hourly data extraction
vitals_hourly_data <- read.csv("D:/mimic_data/vitals_hourly.csv")
#Cleaning vitals hourly column names
vitals_hourly_data <- vitals_hourly_data%>%
  clean_names()
vitals_hourly_data <- vitals_hourly_data %>%
  mutate(across(where(is.character), as.factor))
vitals_hourly_data_24 <- vitals_hourly_data %>%
  filter(hr >= 0 & hr <= 24)
View(vitals_hourly_data_24)
#linking icu outcome data to icustays
merged_stays_icu_outcome <- merge(icu_stays_data, icu_outcome_data , by = "icustay_id", all.x = TRUE)
sum(duplicated(icu_outcome_data$icustay_id))
#Checking subject_ids etc are identical
merged_stays_icu_outcome %>%
  filter(subject_id.x != subject_id.y)
merged_stays_icu_outcome %>%
  filter(intime.x != intime.y)
merged_stays_icu_outcome %>%
  filter(outtime.x != outtime.y)
#Coalesce and subject id and then remove the old ones
merged_stays_icu_outcome <- merged_stays_icu_outcome %>%
  mutate(subject_id = coalesce(subject_id.x, subject_id.y))
merged_stays_icu_outcome <- merged_stays_icu_outcome %>%
  select(-subject_id.x, -subject_id.y)
#merge patient file and merged icu files based on subject id
merged_patient_icu <- merged_stays_icu_outcome %>%
  left_join( patient_data , by = "subject_id")
#discrepency checks
discrepancies_merged_intime <- merged_patient_icu$intime.x != merged_patient_icu$intime.y &
  !is.na(merged_patient_icu$intime.x) &
  !is.na(merged_patient_icu$intime.y)

# Any discrepancies?
any(discrepancies_merged_intime)

# View rows with mismatch for intime
merged_patient_icu[discrepancies_merged_intime, ]
# View rows with mismatch for out time
discrepancies_merged_outtime <- merged_patient_icu$outtime.x != merged_patient_icu$outtime.y &
  !is.na(merged_patient_icu$intime.x) &
  !is.na(merged_patient_icu$intime.y)

# Any discrepancies?
any(discrepancies_merged_outtime)

# View rows with mismatch
merged_patient_icu[discrepancies_merged_outtime, ]
#Coalescing the intime and outime
merged_patient_icu <- merged_patient_icu %>%
  mutate(intime = coalesce(intime.x, intime.y))
merged_patient_icu <- merged_patient_icu %>%
  select(-intime.x, -intime.y)
#Logic checks
#No hospital death time but has expired flag, result is 0.
filtered_death_time <- merged_patient_icu %>%
  filter(is.na(hosp_deathtime) & hospital_expire_flag == 1)
#ICU has death but not hospital expire, result is 0
filtered_death_icu_not_hosp <- merged_patient_icu %>%
  filter(icu_expire_flag == 1 & hospital_expire_flag == 0)
#ICU has no death but hospital has expire, result is 1988, 1988 people died in hospital but not while in ICU?
filtered_death_hosp_not_icu <- merged_patient_icu %>%
  filter(icu_expire_flag == 0 & hospital_expire_flag == 1)
#Dead but not in hospital, some 15510 entries
filtered_death_not_hosp <- merged_patient_icu %>%
  filter(expire_flag.x == 1 & hospital_expire_flag == 0)
#Not dead but dead in hospital? 0 such cases
filtered_not_hosp_dead <- merged_patient_icu %>%
  filter(expire_flag.x == 0 & hospital_expire_flag == 1)
#Checking patients Aged under 18
filtered_under_18 <- merged_patient_icu %>%
  filter(age_years < 18 )
nrow(filtered_under_18)
#Filtering only 18 or above.
filtered_18 <- merged_patient_icu %>%
  filter(age_years >= 18 )
#Removing things without admission time
filtered_intime_time <- filtered_18 %>%
  filter(!is.na(intime))
nrow(filtered_18)#53332
nrow(filtered_intime_time)#53332, so nothing is without admission time.
#Remove anything that does not have a Outtime time
filtered_outtime_time <- filtered_intime_time %>%
     filter(!is.na(outtime.x))
nrow(filtered_outtime_time) #53329 Number with an outtime
#Identifying whether length of stay are correct
correct_los <- filtered_outtime_time %>%
  mutate(
    computed_los = as.numeric(difftime(outtime.x, intime, units = "hours")) / 24,
    correct_los = abs(computed_los - los.x) < 0.001
  )
summary(correct_los$correct_los) #3 False, True = 53326
View(correct_los) #Manually viewed that the 3 false are only different within 1 decimal places and are all under 7 days anyway. Will ignore them.
#Feature engineering long_icu_stay based on stay 7 days or more
SEVEN_days_feature <- filtered_outtime_time %>%
  mutate( long_icu_stay = los.x >= 7 )
summary(SEVEN_days_feature$long_icu_stay) #48576 False, 7453 True, 3 NA. The 3 NAs have no admission time, dischtime, outtime, los, intime. Eliminating them
SEVEN_days_feature_clean <- SEVEN_days_feature %>%
  filter(!is.na(long_icu_stay))
summary(SEVEN_days_feature_clean$long_icu_stay)#48576 False, 7453 True, cleaned.
#Eliminating people whose length of stay are under a day
over_a_day <- SEVEN_days_feature_clean %>%
  filter(los.x >= 1 )
nrow(SEVEN_days_feature_clean) #53329
nrow(over_a_day) #45253
#sanity check about whether there are people with dod.x not empty and dod_hosp is empty and vice versa
sanity_zombies <- over_a_day %>%
  filter(!is.na(dod.x) & is.na(dod_hosp))
nrow(sanity_zombies)
sanity_zombies_2 <- over_a_day %>%
  filter(is.na(dod.x) & !is.na(dod_hosp))
nrow(sanity_zombies_2)
sanity_zombies_3 <- over_a_day %>%
  filter(is.na(dod.x) & !is.na(hosp_deathtime))
nrow(sanity_zombies_3)
sanity_zombies_4 <- over_a_day %>%
  filter(!is.na(dod.x) & is.na(hosp_deathtime))
nrow(sanity_zombies_4)
#Dead individual reports
#Convert to measurable time

#Finding difference in time between intime and dead time
dead_men_reports <- over_a_day %>%
  mutate(
    # Convert columns correctly
    intime         = as.POSIXct(intime, format = "%Y-%m-%d %H:%M:%S"),
    dod.x          = as.Date(dod.x, format = "%Y-%m-%d"),             # date-only
    dod_hosp       = as.POSIXct(dod_hosp, format = "%Y-%m-%d %H:%M:%S"),
    dod_ssn        = as.POSIXct(dod_ssn, format = "%Y-%m-%d %H:%M:%S"),
    hosp_deathtime = as.POSIXct(hosp_deathtime, format = "%Y-%m-%d %H:%M:%S")
  ) %>%
  mutate(
    # Compute differences in days
    dod_x_intime      = as.numeric(difftime(dod.x, intime, units = "days")),
    dod_hosp_intime   = as.numeric(difftime(dod_hosp, intime, units = "days")),
    dod_ssn_intime    = as.numeric(difftime(dod_ssn, intime, units = "days")),
    hosp_death_intime = as.numeric(difftime(hosp_deathtime, intime, units = "days"))
  )
dead_men_reports_less7 <- dead_men_reports %>%
  filter(
    (dod_x_intime    <= 7 & !is.na(dod_x_intime)) |
      (dod_hosp_intime <= 7 & !is.na(dod_hosp_intime)) |
      (dod_ssn_intime  <= 7 & !is.na(dod_ssn_intime)) |
      (hosp_death_intime <= 7 & !is.na(hosp_death_intime))
  )
nrow(dead_men_reports_less7) #3364
#now removing these dead less than 7 days in icu entirely
dead_men_reports_clean <- dead_men_reports %>%
  filter(
    !(
      (dod_x_intime    <= 7 & !is.na(dod_x_intime)) |
        (dod_hosp_intime <= 7 & !is.na(dod_hosp_intime)) |
        (dod_ssn_intime  <= 7 & !is.na(dod_ssn_intime)) |
        (hosp_death_intime <= 7 & !is.na(hosp_death_intime))
    )
  )
nrow(over_a_day) #45253
nrow(dead_men_reports_clean) #41889

#Counting number of identical hadm_id and their order. We want only the first icu entry
hadm_id_count <- dead_men_reports_clean %>%
  arrange(hadm_id.x, intime) %>%
  group_by(hadm_id.x) %>%
  mutate(
    order_by_intime = row_number(),
    n_within_hadm   = n()
  ) %>%
  ungroup()
nrow(hadm_id_count) #41889
#Filtering it so that only first icu entry remained
first_hadm_id <- hadm_id_count %>%
  filter(order_by_intime == 1 )
nrow(first_hadm_id) #39424

#Importing Labs hourly data 
labs_hourly_data <- read.csv("D:/mimic_data/labs_hourly.csv")
#Clean Labs hourly column names
labs_hourly_data <- labs_hourly_data%>%
  clean_names()
View(labs_hourly_data)
#Creating a plausible range of labs hourly data 
plausible_ranges_labs <- list(
  sodium = c(110, 160),
  glucose = c(40, 600),
  alaninetransaminase = c(1, 10000),
  aspartatetransaminase = c(1, 10000),
  intnormalisedratio = c(0.1, 10),
  chloride = c(80, 120),
  creatinine = c(0.2, 20),
  neutrophil = c(0.1, 100),        # now zero excluded
  bilirubin = c(0.1, 100),
  troponin = c(0.01, 30)        # adjust units as needed
)
# Function to check if a value is extreme
is_extreme <- function(x, range) { ifelse(is.na(x), FALSE, x < range[1] | x > range[2]) }
# Extreme flag calculation, flagging extreme data
labs_hourly_data_flagged <- labs_hourly_data %>%
  mutate(
    extreme_flag = ifelse(
      is_extreme(sodium, plausible_ranges_labs$sodium) |
        is_extreme(glucose, plausible_ranges_labs$glucose) |
        is_extreme(alaninetransaminase, plausible_ranges_labs$alaninetransaminase) |
        is_extreme(aspartatetransaminase, plausible_ranges_labs$aspartatetransaminase) |
        is_extreme(intnormalisedratio, plausible_ranges_labs$intnormalisedratio) |
        is_extreme(chloride, plausible_ranges_labs$chloride) |
        is_extreme(creatinine, plausible_ranges_labs$creatinine) |
        is_extreme(neutrophil, plausible_ranges_labs$neutrophil) |
        is_extreme(bilirubin, plausible_ranges_labs$bilirubin) |
        is_extreme(troponin, plausible_ranges_labs$troponin),
      TRUE, FALSE
    )
  )

# Filter only rows with extreme lab values,shows only 12545
labs_hourly_data_flagged_only_flagged <- labs_hourly_data_flagged %>%
  filter(extreme_flag == TRUE)
View(labs_hourly_data_flagged_only_flagged)
#Eliminate these flagged 12545 results to eliminate extreme laboratory results that are like administrative errors.
labs_hourly_data_clean <- labs_hourly_data_flagged %>%
  filter(extreme_flag == FALSE )
#Only using first 24 hour data, will also include data immediately before admission.
labs_hourly_24h <- labs_hourly_data_clean %>%
  filter(hr >= 0 &hr <= 24)
nrow(labs_hourly_24h) #[1] 368681


#Missing investigation
# Define the lab columns you want to check missingness for
lab_cols <- c(
  "neutrophil", "creactiveprotein", "whitebloodcell", "partialpressureo2",
  "bicarbonate", "lactate", "troponin", "bloodureanitrogen", "creatinine",
  "alaninetransaminase", "aspartatetransaminase", "hemoglobin",
  "intnormalisedratio", "platelets", "albumin", "chloride", "glucose",
  "sodium", "bilirubin", "hematocrit"
)

labs_long <- labs_hourly_24h %>%
  pivot_longer(
    cols = all_of(lab_cols),
    names_to = "variable",
    values_to = "value"
  )
#Summarising by hr and variable the degree of missingness
missing_by_hour_labs <- labs_long %>%
  group_by(hr, variable) %>%
  summarise(missing_pct = mean(is.na(value)) * 100, .groups = "drop")
#Cleaning up the names of the variables
lab_name_lookup <- c(
  neutrophil = "Neutrophils (10⁹/L)",
  creactiveprotein = "C-Reactive Protein (mg/L)",
  whitebloodcell = "WBC (10⁹/L)",
  partialpressureo2 = "PaO2 (mmHg)",
  bicarbonate = "Bicarbonate (mmol/L)",
  lactate = "Lactate (mmol/L)",
  troponin = "Troponin (ng/mL)",
  bloodureanitrogen = "BUN (mg/dL)",
  creatinine = "Creatinine (mg/dL)",
  alaninetransaminase = "ALT (U/L)",
  aspartatetransaminase = "AST (U/L)",
  hemoglobin = "Hemoglobin (g/dL)",
  intnormalisedratio = "INR",
  platelets = "Platelets (10⁹/L)",
  albumin = "Albumin (g/dL)",
  chloride = "Chloride (mmol/L)",
  glucose = "Glucose (mg/dL)",
  sodium = "Sodium (mmol/L)",
  bilirubin = "Bilirubin (mg/dL)",
  hematocrit = "Hematocrit (%)"
)


missing_by_hour_labs <- missing_by_hour_labs %>%
  mutate(variable = factor(variable,
                           levels = names(lab_name_lookup),
                           labels = lab_name_lookup))
#plotting visualization of the degree of missingness by variables
ggplot(missing_by_hour_labs, aes(x = hr, y = variable, fill = missing_pct)) +
  geom_tile() +
  scale_fill_viridis_c(name = "% Missing") +
  labs(title = "Hourly Missingness for Laboratory Variables",
       x = "Hour Since ICU Admission",
       y = "Lab Variable")

#Creating medians for the labs_hourly_data.
labs_summary <- labs_hourly_24h %>%
  group_by(icustay_id) %>%
  summarise(across(
    c(neutrophil, creactiveprotein, whitebloodcell, partialpressureo2,
      bicarbonate, lactate, troponin, bloodureanitrogen, creatinine,
      alaninetransaminase, aspartatetransaminase, hemoglobin,
      intnormalisedratio, platelets, albumin, chloride, glucose,
      sodium, bilirubin, hematocrit),
    ~median(.x, na.rm = TRUE)
  ))
#Making a copy of the original dataframe to prepare for joining
labs_joining_data <- first_hadm_id
merged_data_icu_labs <- labs_joining_data  %>%
  left_join(labs_summary, by = "icustay_id")
View(merged_data_icu_labs)
#Importing Vitals hourly data
vitals_hourly_data <- read.csv("D:/mimic_data/vitals_hourly.csv")
#Cleaning the column names
vitals_hourly_data <- vitals_hourly_data%>%
  clean_names()
#Factorising the names
vitals_hourly_data <- vitals_hourly_data %>%
  mutate(across(where(is.character), as.factor))
#Making sure only first 24 hours data are used
vitals_hourly_data_24 <- vitals_hourly_data %>%
  filter(hr >= 0 & hr <= 24)
View(vitals_hourly_data_24)
vital_cols <- c(
  "spo2", "fio2", "temperature", "resprate",
  "sysbp", "diasbp", "glucose", "meanarterialpressure"
)

#Missings investigation of the vitals data
vitals_long <- vitals_hourly_data_24 %>%
  pivot_longer(cols = all_of(vital_cols),
               names_to = "variable",
               values_to = "value")
# Compute missingness per hour for each variable
missing_by_hour <- vitals_long %>%
  group_by(hr, variable) %>%
  summarise(
    n_missing = sum(is.na(value)),
    n_total = n(),
    missing_pct = 100 * n_missing / n_total,
    .groups = "drop"
  )

# Map variable names to nicer labels
name_lookup_vitals <- c(
  spo2 = "SpO2",
  fio2 = "FiO2",
  temperature = "Temperature (°C)",
  resprate = "Respiratory Rate",
  sysbp = "Systolic BP",
  diasbp = "Diastolic BP",
  glucose = "Glucose",
  meanarterialpressure = "Mean Arterial Pressure"
)

missing_by_hour <- missing_by_hour %>%
  mutate(
    variable = factor(variable,
                      levels = names(name_lookup_vitals),
                      labels = name_lookup_vitals)
  )

# Plot missingness
ggplot(missing_by_hour,
       aes(x = hr, y = variable, fill = missing_pct)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Missingness Over 24-hour Window for Vitals",
       x = "Hour since ICU admission",
       y = "Variable",
       fill = "% Missing") +
  theme_minimal()


# Mapping original column names to nicer labels
vital_labels <- c(
  spo2 = "SpO2 (%)",
  fio2 = "FiO2 (%)",
  temperature = "Temperature (°C)",
  resprate = "Respiratory Rate (bpm)",
  sysbp = "Systolic BP (mmHg)",
  diasbp = "Diastolic BP (mmHg)",
  glucose = "Glucose (mg/dL)",
  meanarterialpressure = "Mean Arterial Pressure (mmHg)"
)



#Flagging extreme data for vitals
vitals_hourly_data_flagged <- vitals_hourly_data_24 %>%
  mutate(
    extreme_flag = case_when(
      !is.na(spo2) & (spo2 < 50 | spo2 > 100) ~ TRUE,
      !is.na(fio2) & (fio2 < 21 | fio2 > 100) ~ TRUE,
      !is.na(temperature) & (temperature < 32 | temperature > 42) ~ TRUE,
      !is.na(resprate) & (resprate < 5 | resprate > 60) ~ TRUE,
      !is.na(heartrate) & (heartrate < 30 | heartrate > 220) ~ TRUE,
      !is.na(sysbp) & (sysbp < 40 | sysbp > 250) ~ TRUE,
      !is.na(diasbp) & (diasbp < 20 | diasbp > 140) ~ TRUE,
      !is.na(glucose) & (glucose < 40 | glucose > 600) ~ TRUE,
      !is.na(meanarterialpressure) & (meanarterialpressure < 40 | meanarterialpressure > 120) ~ TRUE,
      TRUE ~ FALSE  # all others (including NA) → not extreme
    )
  )
#Total of rows is 7292362
nrow(vitals_hourly_data_24)
#Looking at the extreme ones
vitals_hourly_data_flagged_extreme <- vitals_hourly_data_flagged %>%
  filter(extreme_flag == TRUE)
nrow(vitals_hourly_data_flagged_extreme)
#Cleaning but eliminating the extreme flagged
vitals_hourly_data_cleaned <- vitals_hourly_data_flagged %>%
  filter(extreme_flag == FALSE)
nrow(vitals_hourly_data_cleaned)
#Checking whether there are identical sets of icustay_id and hr in vitals cleaned
duplicate_counts_vitals <- vitals_hourly_data_cleaned %>%
  group_by(icustay_id, hr) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)

# View duplicates
duplicate_counts_vitals #Nill duplicates, noice.

#Creating a median summary of the  vitals by patient. Medians are used to avoid outliers or outright biologically implausible data for now.
vitals_summary <- vitals_hourly_data_cleaned%>%
  group_by(icustay_id) %>%
  summarise(
    #hr_median = median(hr, na.rm = TRUE),
    spo2_median = median(spo2, na.rm = TRUE),
    fio2_median = median(fio2, na.rm = TRUE),
    temp_median = median(temperature, na.rm = TRUE),
    resp_median = median(resprate, na.rm = TRUE),
    sysbp_median = median(sysbp, na.rm = TRUE),
    diasbp_median = median(diasbp, na.rm = TRUE),
    glucose_median = median(glucose, na.rm = TRUE),
    map_median = median(meanarterialpressure, na.rm = TRUE)
  )
#Merging based on icustay_id of the merged data and median vitals.
merged_data_icu_vitals <- merged_data_icu_labs %>%
  left_join(vitals_summary, by = "icustay_id")


#Start from here

#Glascow Coma Score
gcs_hourly_data <- read.csv("D:/mimic_data/gcs_hourly.csv")
#Clean Labs hourly column names
gcs_hourly_data <- gcs_hourly_data%>%
  clean_names()
View(gcs_hourly_data)
#Logic checks to see if gcs = all the other gcs scores added together for each row
gcs_hourly_data_check <- gcs_hourly_data %>%
  mutate(
    gcs_calc = gcseyes + gcsmotor + gcsverbal,
    gcs_mismatch = ifelse(!is.na(gcs) & !is.na(gcs_calc), gcs != gcs_calc, NA)
  )
#Checking what endotrach 1 actually looked like
gcs_hourly_data_check_endo_one <- gcs_hourly_data %>%
  filter(endotrachflag == 1)
gcs_hourly_data_check_endo_one_verbal_more_than_one <- gcs_hourly_data_check_endo_one %>%
  filter(gcsverbal > 1)
summary(gcs_hourly_data_check_endo_one_verbal_more_than_one)
# View how many rows don't match
summary(gcs_hourly_data_check$gcs_mismatch) #   Mode   FALSE    TRUE    NA's
#logical 1507888     275    7179
#Not looking good.Gonna check the mismatched and NA rows
# View all rows where GCS total does NOT match the sum of subscores
gcs_mismatch_rows <- gcs_hourly_data_check %>%
  filter(gcs_mismatch == TRUE)

# View all rows where GCS check could not be performed due to missing data
gcs_na_rows <- gcs_hourly_data_check %>%
  filter(is.na(gcs_mismatch))

# Quick counts
nrow(gcs_mismatch_rows)
nrow(gcs_na_rows)
#Since NAs are a minor number compared to the absolute majority of false, decision is made to eliminate rows with NAs.Mismatched rows will be recalculated.
# Recalculate GCS
# 1. Filter out rows with NAs in subcomponents and recalculate
gcs_hourly_data_clean <- gcs_hourly_data_check %>%
  filter(!is.na(gcseyes) & !is.na(gcsmotor) & !is.na(gcsverbal)) %>%
  mutate(gcs_calc = gcseyes + gcsmotor + gcsverbal)

# 2. Drop the original GCS and rename the recalculated column
gcs_hourly_data_clean <- gcs_hourly_data_clean %>%
  select(-gcs) %>%
  rename(gcs = gcs_calc)

# 3. Count total rows after cleaning
n_rows_clean <- nrow(gcs_hourly_data_clean)
print(paste("Number of rows after cleaning:", n_rows_clean))

# 4. Optional: Create a quick tabulation of GCS totals to inspect distribution
gcs_tab <- table(gcs_hourly_data_clean$gcs)
print(gcs_tab)

# 5. Mismatch check (just to double-check, should all be consistent now)
gcs_hourly_data_clean <- gcs_hourly_data_clean %>%
  mutate(mismatch = (gcs != (gcseyes + gcsmotor + gcsverbal)))

summary(gcs_hourly_data_clean$mismatch)


nrow(gcs_hourly_data_clean) #1508163, number looks correct after removing 7179
#Dropping the mismatch and gsc_mismatch now that they have been resolved
gcs_hourly_data_clean <- gcs_hourly_data_clean %>%
  select(-mismatch)  # remove the mismatch column
gcs_hourly_data_clean <- gcs_hourly_data_clean %>%
  select(-gcs_mismatch)
summary(gcs_hourly_data_clean)
#Checking whether there are duplicates based on icustay_id and hours
duplicate_counts_gcs <- gcs_hourly_data_clean %>%
  group_by(icustay_id, hr) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)

# View number of duplicate rows
nrow(duplicate_counts_gcs)
#Only using first 24 hours of gcs data or immediately before admission
gcs_hourly_24h <- gcs_hourly_data_clean %>%
  filter(hr <= 24 & hr >= 0 )
#Endotrach check
Endocheck <- gcs_hourly_24h %>%
  filter(endotrachflag != 1 & endotrachflag != 0 )
nrow(Endocheck)
#Create median of the gcs based on icustay_id
gcs_summary <- gcs_hourly_24h %>%
  group_by(icustay_id) %>%
  summarise(
    gcs_median = median(gcs, na.rm = TRUE),
    endotrach_median = median(endotrachflag, na.rm = TRUE)
  )
#Endotrach check
endotrach_check <- gcs_summary %>%
  filter(endotrach_median != 0 & endotrach_median != 1)
nrow(endotrach_check) #1196 showing 0.5. This meant some have switched. But even once is sufficient to show that someone was on endotrach. We will turn them all to 1!
gcs_summary_1 <- gcs_summary %>%
  mutate(
    endotrach_median = as.integer(endotrach_median != 0)
  )
#Fully factorised
gcs_summary_1 <- gcs_summary_1 %>%
  mutate(
    endotrach_median = factor(endotrach_median, levels = c(0,1), labels = c("No","Yes"))
  )
#Sanity_check
summary(gcs_summary_1$endotrach_median)#Works No :36436 Yes:15742
#now merge the gcs_summary based on icustay_id to the main files
merged_patient_gcs <- merged_data_icu_vitals %>%
  left_join(gcs_summary_1, by = "icustay_id")


# Coalesce hadm_id from the two sources
merged_patient_gcs <- merged_patient_gcs %>%
  mutate(hadm_id = coalesce(hadm_id.x, hadm_id.y)) %>%
  select(-hadm_id.x, -hadm_id.y)  # drop the originals
#Missingness check by hour for GCS data, starting with defining the columns
gcs_cols <- c(
  "gcseyes",
  "gcsmotor",
  "gcsverbal",
  "endotrachflag",
  "gcs"   # if you have a total score column
)

#Pivot to longer format
gcs_long <- gcs_hourly_24h %>%
  pivot_longer(
    cols = all_of(gcs_cols),
    names_to = "variable",
    values_to = "value"
  )
#Compute missingness per hour per variable
missing_by_hour_gcs <- gcs_long %>%
  group_by(hr, variable) %>%
  summarise(missing_pct = mean(is.na(value)) * 100, .groups = "drop")
#Professionalise variable names
gcs_name_lookup <- c(
  gcseyes = "GCS Eyes",
  gcsmotor = "GCS Motor",
  gcsverbal = "GCS Verbal",
  endotrachflag = "Endotrach Status",
  gcs = "GCS Total"
)
#Optional: Order by overall missingness
gcs_order <- missing_by_hour_gcs %>%
  group_by(variable) %>%
  summarise(avg_missing = mean(missing_pct)) %>%
  arrange(desc(avg_missing)) %>%
  pull(variable)

missing_by_hour_gcs <- missing_by_hour_gcs %>%
  mutate(variable = factor(variable, levels = gcs_order))
#Plot heatmap
ggplot(missing_by_hour_gcs, aes(x = hr, y = variable, fill = missing_pct)) +
  geom_tile() +
  scale_fill_viridis_c(name = "% Missing") +
  labs(title = "Hourly Missingness for GCS Variables (First 24 Hours)",
       x = "Hour Since ICU Admission",
       y = "GCS Component")


# Read ICD-9 diagnosis data
icd_diagnosis <- read.csv("D:/mimic_data/icd9_diag.csv")

# Calculate Charlson comorbidity scores using ICD-9 mapping
charlson_scores <- comorbidity(
  x = icd_diagnosis,
  id = "hadm_id",
  code = "icd9_code",
  map = "charlson_icd9_quan",  # correct mapping
  assign0 = TRUE
)

# Extract total Charlson score
charlson_total <- charlson_scores %>%
  rowwise() %>%
  mutate(charlson = sum(c_across(mi:aids))) %>%  # sum all comorbidity flags
  ungroup() %>%
  select(hadm_id, charlson)

# View result
head(charlson_total)
summary(charlson_total)
#merge total charlson score to merged file based on hadm_id
merged_patient_icd_score <- merged_patient_gcs %>%
  left_join(charlson_total, by = "hadm_id")
#Reading output hourly file
output_hourly_data <- read.csv("D:/mimic_data/output_hourly.csv")
#Making sure that the output data is immediately before or within 24 hours of admission
output_hourly_data_24 <- output_hourly_data %>%
  filter(hr >= 0 & hr <= 24 )
#Given unreasonable urine outputs like -6000 at minimum and max of 555975.0 ,and NAs, we are gonna filter out those that didn't have that.
output_hourly_data_24_reasonable <- output_hourly_data_24 %>%
  filter(urineoutput >= 0, urineoutput <= 10000)  # cap at 10L per 24h
# Find duplicates by icustay_id and hr
duplicates_output <- output_hourly_data_24_reasonable %>%
  group_by(icustay_id, hr) %>%
  filter(n() > 1) %>%
  arrange(icustay_id, hr)

# Check how many duplicate combinations there are
nrow(duplicates_output) #0,yay

#Getting the median and total
urine_24h_summary <- output_hourly_data_24_reasonable %>%
  group_by(icustay_id) %>%
  summarise(
    median_urine_24h = median(urineoutput, na.rm = TRUE),
    total_urine_24h = sum(urineoutput, na.rm = TRUE)
  )#Getting insane scores like total of 98480 ml of urine max a day
#filtering a hard limit
urine_24h_clean <- urine_24h_summary %>%
  filter(median_urine_24h <= 500, total_urine_24h <= 12000)
#merging to main file now
merged_patient_icd_urine <- merged_patient_icd_score %>%
  left_join(urine_24h_clean, by = "icustay_id")
# Read patient weight data
patient_weight_data <- read.csv("D:/mimic_data/pt_weight.csv")
#Clean patient_weight_data column names
patient_weight_data <- patient_weight_data%>%
  clean_names()
summary(patient_weight_data)
#Got lots of implausible range, setting hard limit

# Flag implausible weights but keep NAs
patient_weight_data_cleaned <- patient_weight_data %>%
  mutate(
    avg_weight_naive = ifelse(avg_weight_naive < 30 | avg_weight_naive > 250, NA, avg_weight_naive),
    min_weight       = ifelse(min_weight < 30 | min_weight > 250, NA, min_weight),
    max_weight       = ifelse(max_weight < 30 | max_weight > 250, NA, max_weight)
  )
patient_weight_data_cleaned <- patient_weight_data_cleaned %>%
  filter(!is.na(avg_weight_naive) &
           !is.na(min_weight) &
           !is.na(max_weight))
#Filter based on the first 3 days
patient_weight_data_clean_3days <- patient_weight_data_cleaned %>%
  filter(dy <= 3)


#Median summary
patient_weight_median <- patient_weight_data_clean_3days %>%
  group_by(icustay_id) %>%
  summarise(
    median_avg_weight = median(avg_weight_naive, na.rm = TRUE),
    median_min_weight = median(min_weight, na.rm = TRUE),
    median_max_weight = median(max_weight, na.rm = TRUE)
  )
# Merged back to main data
merged_patient_weight <- merged_patient_icd_urine %>%
  left_join(patient_weight_median, by = "icustay_id")
#Read Mechanical Ventillation data
mechvent_data <- read.csv("D:/mimic_data/pv_mechvent.csv")
#Clean mechvent_data column names
mechvent_data <- mechvent_data%>%
  clean_names()
View(mechvent_data)
#mechvent data only in 24 hours
#Will need to identify the hour of the mechvent data. Based way to do this would be to subtract chart time from intime from the icu_outcome_data.
#To do this will need to filter only icustay_id and intime and then reverse link files to mechvent_data
icu_intime <- icu_outcome_data %>%
  select(icustay_id, intime,outtime,hadm_id)

icu_intime <- icu_intime %>%
  mutate(
    intime  = as.POSIXct(as.character(intime),  format = "%Y-%m-%d %H:%M:%S"),
    outtime = as.POSIXct(as.character(outtime), format = "%Y-%m-%d %H:%M:%S")
  )

#Merge data frame to the main one.
mechvent_data_merged <- mechvent_data %>%
  left_join(icu_intime, by = "icustay_id") %>%
  # Convert charttime properly
  mutate(
    charttime = as.POSIXct(as.character(charttime), format = "%Y-%m-%d %H:%M:%S"),
    hour = round(as.numeric(difftime(charttime, intime, units = "hours")))  # rounded to nearest hour
  )
#Removing those that had less 0 hours or above 24 hours as we only want data within the first 24 hours.
mechvent_data_merged_24 <- mechvent_data_merged %>%
  filter(hour <= 24 & hour >= 0 )# Count duplicates
duplicate_check_vent <- mechvent_data_merged_24 %>%
  group_by(icustay_id, charttime) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)
# View duplicates
duplicate_check_vent #no dups
#Doing eda on mechvent_data in first 24 hours
ggplot(mechvent_data_merged_24, aes( x= minutevolume )) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Minute Volume", x = "Minute Volume", y = "Count")+
  theme_minimal()
summary(mechvent_data_merged_24$minutevolume)
ggplot(mechvent_data_merged_24, aes( x= settidalvolume )) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of settidalvolume", x = "Minute Volume", y = "Count")+
  theme_minimal()
summary(mechvent_data_merged_24$settidalvolume)
ggplot(mechvent_data_merged_24, aes( x= obstidalvolume )) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of obstidalvolume", x = "Minute Volume", y = "Count")+
  theme_minimal()
summary(mechvent_data_merged_24$obstidalvolume)

# Count unique icustay_id only 13136 unique icustay numbers counted. I am just gonna go for binary in main file between ventilated or not.
mechvent_data_merged_24 %>%
  summarise(n_unique_icustay = n_distinct(icustay_id))
#Create binary ventilated variable
# Step 1: Identify which icustay_ids were ventilated in first 24 hours
vent_first24_cat <- mechvent_data_merged_24 %>%
  distinct(icustay_id) %>%                    # unique ventilated patients
  mutate(ventilated_first24 = "Yes")          # label as "Yes"

# Step 2: Convert to factor for categorical use
vent_first24_cat <- vent_first24_cat %>%
  mutate(ventilated_first24 = factor(ventilated_first24,
                                     levels = c("No", "Yes")))

#Joining ventillation status data to main dataframe
merged_patient_vent <- merged_patient_weight %>%
  left_join(vent_first24_cat, by = "icustay_id") %>%
  mutate(ventilated_first24 = ifelse(is.na(ventilated_first24), "No", as.character(ventilated_first24))) %>%
  mutate(ventilated_first24 = factor(ventilated_first24, levels = c("No", "Yes")))

# Check the result
table(merged_patient_vent$ventilated_first24) #Numbers add up

#Cleaning the ventillation numerical datas
# Apply clinical ranges to existing first-24-hour data
clean_vent_data <- mechvent_data_merged_24 %>%
  mutate(
    # === TIDAL VOLUMES - No zeros allowed during ventilation ===
    obstidalvolume = case_when(
      obstidalvolume < 100 ~ NA_real_,      
      obstidalvolume > 2000 ~ NA_real_,
      TRUE ~ obstidalvolume
    ),

    sponttidalvolume = case_when(
      sponttidalvolume < 100 ~ NA_real_,    
      sponttidalvolume > 2000 ~ NA_real_,
      TRUE ~ sponttidalvolume
    ),

    settidalvolume = case_when(
      settidalvolume < 100 ~ NA_real_,      
      settidalvolume > 2000 ~ NA_real_,
      TRUE ~ settidalvolume
    ),

    # === MINUTE VENTILATION - No zeros ===
    minutevolume = case_when(
      minutevolume < 2 ~ NA_real_,          
      minutevolume > 50 ~ NA_real_,
      TRUE ~ minutevolume
    ),

    # === PRESSURES - Realistic minimums for ventilated patients ===
    setpeep = case_when(
      setpeep < 0 ~ NA_real_,
      setpeep > 50 ~ NA_real_,
      TRUE ~ setpeep
    ),

    totalpeep = case_when(
      totalpeep < 0 ~ NA_real_,
      totalpeep > 50 ~ NA_real_,
      TRUE ~ totalpeep
    ),

    meanairwaypressure = case_when(
      meanairwaypressure < 5 ~ NA_real_,    # Should be at least 5 during ventilation
      meanairwaypressure > 50 ~ NA_real_,
      TRUE ~ meanairwaypressure
    ),

    peakinsppressure = case_when(
      peakinsppressure < 10 ~ NA_real_,     # Increased from 5 to 10
      peakinsppressure > 80 ~ NA_real_,
      TRUE ~ peakinsppressure
    ),

    plateaupressure = case_when(
      plateaupressure < 8 ~ NA_real_,       # Increased from 5 to 8
      plateaupressure > 50 ~ NA_real_,
      TRUE ~ plateaupressure
    ),

    # === TIMING PARAMETERS ===
    insptime = case_when(
      insptime < 0.3 ~ NA_real_,            # Increased from 0.1 to 0.3
      insptime > 3.0 ~ NA_real_,
      TRUE ~ insptime
    ),

    # === DURATION ===
    duration_hours = case_when(
      duration_hours < 0.0167 ~ NA_real_,   # At least 1 minute
      duration_hours > 24 ~ NA_real_,
      TRUE ~ duration_hours
    ),

    # === CREATE FLAGS FOR EXTREME BUT PLAUSIBLE VALUES ===
    high_peep_flag = as.numeric(setpeep > 20 & setpeep <= 30),
    high_pressure_flag = as.numeric(peakinsppressure > 60 & peakinsppressure <= 80),
    high_tidal_volume_flag = as.numeric(obstidalvolume > 1000 & obstidalvolume <= 2000)
  )

# Check what percentage of data we're keeping/losing
cat("=== DATA QUALITY IMPACT ASSESSMENT ===\n")

# For each key column, show before/after
key_columns <- c("obstidalvolume", "peakinsppressure", "setpeep", "minutevolume")

for(col in key_columns) {
  before_na <- sum(is.na(mechvent_data_merged_24[[col]]))
  after_na <- sum(is.na(clean_vent_data[[col]]))
  total <- nrow(mechvent_data_merged_24)

  cat(sprintf("\n%s:\n", col))
  cat(sprintf("  Before: %d NA (%0.1f%%)\n", before_na, before_na/total*100))
  cat(sprintf("  After:  %d NA (%0.1f%%)\n", after_na, after_na/total*100))
  cat(sprintf("  Added:  %d new NA\n", after_na - before_na))

  # Show range changes
  if(after_na - before_na > 0) {
    before_range <- range(mechvent_data_merged_24[[col]], na.rm = TRUE)
    after_range <- range(clean_vent_data[[col]], na.rm = TRUE)
    cat(sprintf("  Range: [%0.1f, %0.1f] -> [%0.1f, %0.1f]\n",
                before_range[1], before_range[2], after_range[1], after_range[2]))
  }
}

# Just median summaries of ventilation
ventilation_aggregated <- clean_vent_data %>%
  group_by(icustay_id) %>%
  summarise(across(
    .cols = where(is.numeric),
    .fns = ~median(., na.rm = TRUE),
    .names = "{.col}_median"
  )) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.) | is.nan(.), NA, .)))


#Creating copy of merged_patient_vent
merging_all_vent <- merged_patient_vent
nrow(merged_patient_vent ) #37277
#Joining the rest of the ventillation(numerical) data to the main dataframe.Previously only ventillation status joined
merging_all_vent <- merging_all_vent %>%
  left_join(ventilation_aggregated, by = "icustay_id")
nrow(merging_all_vent) #37277

# Check the result
table(merged_patient_vent$ventilated_first24) #Numbers add up


#Reading vasopressor file
vasopressor_data <- read.csv("D:/mimic_data/vasopressors.csv")
#Cleaning vasopressor data names
vasopressor_data <- vasopressor_data %>%
  clean_names()
vasopressor_data %>%
  summarise(n_unique_icustay = n_distinct(icustay_id))
# Convert starttime to POSIXct if needed
vasopressor_data <- vasopressor_data %>%
  mutate(
    starttime = as.POSIXct(as.character(starttime), format="%Y-%m-%d %H:%M:%S"),
    endtime   = as.POSIXct(as.character(endtime),   format="%Y-%m-%d %H:%M:%S")
  )
#Merge with intime data to check how many hours after entering icu a certain event is

vasopressor_data_merged <- vasopressor_data %>%
  left_join(icu_intime, by = "icustay_id") %>%
  mutate(
    hours_since_intime_start = round(as.numeric(difftime(starttime, intime, units = "hours"))),
    hours_since_intime_end   = round(as.numeric(difftime(endtime, intime, units = "hours")))
  )
vasoppresor_data_merged_first24 <- vasopressor_data_merged %>%
  filter(hours_since_intime_start <= 24 & hours_since_intime_start >= 0 )

#Looking at number of rows filled
sum(!is.na(vasopressor_data$norepinephrine_rate))
#[1] 83528
sum(!is.na(vasopressor_data$norepinephrine_amount))
#[1] 77243
sum(!is.na(vasopressor_data$epinephrine_rate))
#[1] 55251
sum(!is.na(vasopressor_data$epinephrine_amount))
#[1] 36882
sum(!is.na(vasopressor_data$dopamine_rate))
#[1] 132971
sum(!is.na(vasopressor_data$dopamine_amount))
#[1] 87913
sum(!is.na(vasopressor_data$starttime))
#[1] 314896
sum(!is.na(vasopressor_data$endtime))
#[1] 83764
#Data is too messy to make sense of quantitatively given the large amount of missing data regarding amounts. Rates alone make little sense.
important_cols <- c("norepinephrine_rate", "norepinephrine_amount",
                    "epinephrine_rate", "epinephrine_amount",
                    "dopamine_rate", "dopamine_amount",
                    "dobutamine_rate", "dobutamine_amount")

# Find rows where all important columns are 0 or NA
all_zero_or_na <- vasoppresor_data_merged_first24 %>%
  filter(if_all(all_of(important_cols), ~ is.na(.) | . == 0))

# Flag rows that are all 0 or NA across important columns, 8068 rows had 0 or NAs in these important columns. Will eliminate them.
vasoppresor_data_merged_first24 <- vasoppresor_data_merged_first24 %>%
  mutate(all_zero_or_na_flag = if_all(all_of(important_cols), ~ is.na(.) | . == 0))
vasoppresor_data_merged_first24_clean <- vasoppresor_data_merged_first24 %>%
  filter(all_zero_or_na_flag == FALSE )

# Step 1: Create binary flag per patient
vasopressor_patient_flag <- vasoppresor_data_merged_first24_clean %>%
  distinct(icustay_id) %>%                        # one row per patient
  mutate(vasopressor_first24 = "Yes")             # any patient with at least one row gets "Yes"

# Step 2: Joining vasopressor status into merged_patient_vent based on icu stay id
merging_all_vaspressor_or_not <- merging_all_vent
merging_all_vaspressor_or_not <- merging_all_vaspressor_or_not %>%
  left_join(vasopressor_patient_flag, by = "icustay_id") %>%
  mutate(
    vasopressor_first24 = factor(ifelse(is.na(vasopressor_first24), "No", vasopressor_first24),
                                 levels = c("No", "Yes"))
  )

# Step 3: Quick check
table(merged_patient_vent$vasopressor_first24)
#No   Yes
#54734  6798 Matched

# Clean the vasopressor rates based on clinical plausibility in preparation for joining numerical vasopressor data as well
vasopressor_cleaned_range <- vasoppresor_data_merged_first24_clean %>%
  mutate(
    # === NOREPINEPHRINE RATES ===
    # Typical range: 0.01 - 0.30 mcg/kg/min (but we don't have weight)
    # For absolute rates, consider 0.01-5 mcg/min as plausible
    norepinephrine_rate = case_when(
      norepinephrine_rate < 0.01 ~ NA_real_,    # Too low to be therapeutic
      norepinephrine_rate > 0.80 ~ NA_real_,       # Extremely high without weight
      TRUE ~ norepinephrine_rate
    ),

    # === EPINEPHRINE RATES ===
    # Similar potency to norepinephrine
    epinephrine_rate = case_when(
      epinephrine_rate < 0.01 ~ NA_real_,
      epinephrine_rate > 0.80 ~ NA_real_,
      TRUE ~ epinephrine_rate
    ),

    # === DOPAMINE RATES ===
    # Dopamine is used in much higher doses (2-20 mcg/kg/min)
    # Without weight, broader range but 4000 is insane
    dopamine_rate = case_when(
      dopamine_rate < 1 ~ NA_real_,
      dopamine_rate > 30 ~ NA_real_,           # Cap extreme outliers
      TRUE ~ dopamine_rate
    ),

    # === DOBUTAMINE RATES ===
    # Similar to dopamine ranges
    dobutamine_rate = case_when(
      dobutamine_rate < 1 ~ NA_real_,
      dobutamine_rate > 30 ~ NA_real_,
      TRUE ~ dobutamine_rate
    )
  )

# Creating flag on which vasopressor was used, counting how many were used and summarising the max dose by person
vasopressor_simple <- vasopressor_cleaned_range %>%
  group_by(icustay_id) %>%
  summarise(
    vasopressor_first24 = "Yes",

    # Indicators of any use
    norepinephrine_used = as.numeric(any(!is.na(norepinephrine_rate))),
    epinephrine_used    = as.numeric(any(!is.na(epinephrine_rate))),
    dopamine_used       = as.numeric(any(!is.na(dopamine_rate))),
    dobutamine_used     = as.numeric(any(!is.na(dobutamine_rate))),

    # Count how many vasopressors were used
    pressor_count = norepinephrine_used + epinephrine_used + dopamine_used + dobutamine_used,
    multiple_vasopressors = as.numeric(pressor_count > 1),

    # Use MAX dose instead of median to reflect peak severity
    norepinephrine_max = max(norepinephrine_rate, na.rm = TRUE),
    epinephrine_max    = max(epinephrine_rate, na.rm = TRUE),
    dopamine_max       = max(dopamine_rate, na.rm = TRUE),
    dobutamine_max     = max(dobutamine_rate, na.rm = TRUE),

    .groups = 'drop'
  ) %>%
  mutate(across(where(is.numeric),
                ~ ifelse(is.infinite(.) | is.nan(.), NA, .)))

nrow(vasopressor_simple)
#Merging these other vasopressor information
merging_all_vaspressor_information <- merging_all_vaspressor_or_not
merging_all_vaspressor_information  <- merging_all_vaspressor_information  %>%
  left_join(vasopressor_simple , by = "icustay_id")
nrow(merging_all_vaspressor_or_not) #37277
nrow(merging_all_vaspressor_information) #37277
#Exploring rows where vasopressor is used but no vasopressor rate
vasopressor_used_no_rate <- merging_all_vaspressor_information %>%
  filter(vasopressor_first24.x == "Yes" & is.na(norepinephrine_max) & is.na(epinephrine_max) & is.na(dopamine_max)) #374 entries. Most were individuals who had a amount but no rate.
vasopressor_used_no_rate_zero <- merging_all_vaspressor_information %>%
  filter(vasopressor_first24.x == "Yes" & is.na(norepinephrine_max) & is.na(epinephrine_max) & is.na(dopamine_max & pressor_count == 0 )) #85 entries. Most were individuals who had a amount but no rate.
nrow(vasopressor_used_no_rate)
nrow(vasopressor_used_no_rate_zero)
#Reading the blood culture data
bloodculture_data <- read.csv("D:/mimic_data/bloodculture.csv")
#Cleaning blood culture data column names
bloodculture_data <- bloodculture_data %>%
  clean_names()
# Columns to factorize
bloodculture_data <- bloodculture_data %>%
  mutate(
    charttime = ymd_hms(charttime),
    chartdate = as.Date(charttime),
    org_name = factor(org_name),
    ab_name = factor(ab_name),
    antibioticresistance = factor(antibioticresistance)
  )

#Merging intime to correlate icu time
bloodculture_data_withicutime <- bloodculture_data %>%
  left_join(icu_intime, by = "icustay_id")
bloodculture_data_withicutime <- bloodculture_data_withicutime %>%
  # First, ensure proper POSIXct conversion
  mutate(
    charttime = as.POSIXct(as.character(charttime), format = "%Y-%m-%d %H:%M:%S"),
    intime    = as.POSIXct(as.character(intime),    format = "%Y-%m-%d %H:%M:%S")
  ) %>%
  # Now compute hours since ICU admission
  mutate(
    hours_since_intime = as.numeric(difftime(charttime, intime, units = "hours"))
  )
#Rounding to nearest one
bloodculture_data_withicutime <- bloodculture_data_withicutime %>%
  mutate(
    hours_since_intime = round(hours_since_intime)
  )
#See the differences in time I calculated and the hrs recorded by hospital
bloodculture_data_withicutime <- bloodculture_data_withicutime %>%
  mutate(
    hours_difference = hours_since_intime - hr
  )
# There were some wrong hours. Dropping original hour and using mine calculated hour
bloodculture_data_withicutime <- bloodculture_data_withicutime %>%
  # Drop the original hr column
  select(-hr) %>%
  # Rename hours_since_intime to hr
  rename(hr = hours_since_intime)
summary(bloodculture_data_withicutime$hours_differences)
#Will only use first 24 hours and -24 hours since infection status would not change within 24 hours before entering ICU
bloodculture_data_24_filter <- bloodculture_data_withicutime %>%
  filter(hr >= -24 & hr <= 24 )
#Making positiveculture binary
bloodculture_data_24_filter$positiveculture_cat <- factor(
  bloodculture_data_24_filter$positiveculture,
  levels = c(0, 1),
  labels = c("Negative", "Positive")
)
View(bloodculture_data_24_filter)
#Sanity check to see if blood culture has positiveculture = 1 but no label
bloodculture_data_24_filter %>%
  filter(positiveculture == 1 & is.na(org_name)) %>%
  select(icustay_id, positiveculture, org_name) %>%
  distinct() ##Found 0, good
#Checking if there are positiveculture = 0 but has an org name
bloodculture_data_24_filter %>%
  filter(positiveculture == 1 & is.na(org_name)) %>%
  select(icustay_id, positiveculture, org_name) %>%
  distinct() ##Found 0, good
# Check for negative cultures with an organism listed
bloodculture_data_24_filter %>%
  filter(positiveculture == 0 & !is.na(org_name) & org_name != "") %>%
  select(icustay_id, charttime, positiveculture, org_name) %>%
  distinct() ##0
#Summarise number of icustay_id that has a positiveculture in the first 24 hours or 24 hours before icu admission
bloodculture_summary <- bloodculture_data_24_filter %>%
  group_by(icustay_id) %>%
  summarise(
    positive_culture = as.integer(any(positiveculture == 1)),
    .groups = "drop"
  )
# Step 1: Ensure bloodculture_summary is ready
bloodculture_summary <- bloodculture_data_24_filter %>%
  group_by(icustay_id) %>%
  summarise(
    positive_culture = as.integer(any(positiveculture == 1)),
    .groups = "drop"
  )
# Count of positive cultures (positive_culture == 1)
sum(bloodculture_summary$positive_culture == 1, na.rm = TRUE) #7894
# Step 2: Merge into merging_all_vaspressor_information
merging_blood_culture <- merging_all_vaspressor_information
merged_patient_bloodculture <- merging_blood_culture %>%
  left_join(bloodculture_summary, by = "icustay_id") %>%
  # Replace NA with 0 for patients not in bloodculture_summary
  mutate(
    positive_culture = ifelse(is.na(positive_culture), 0, positive_culture)
  )

# Step 3: Optional check
table(merged_patient_bloodculture$positive_culture)
#    0     1
#  50277 11255
# Step 1: Read and clean the antibiotics CSV
antibiotics_data <- read.csv("D:/mimic_data/antibiotics.csv") %>%
  clean_names()


# Step 2: Convert character columns to factors
antibiotics_data <- antibiotics_data %>%
  mutate(across(where(is.character), as.factor))

# Step 3: Convert starttime and endtime to POSIXct
antibiotics_data <- antibiotics_data %>%
  mutate(
    starttime = as.POSIXct(as.character(starttime), format = "%Y-%m-%d %H:%M:%S"),
    endtime   = as.POSIXct(as.character(endtime),   format = "%Y-%m-%d %H:%M:%S")
  )

# Step 4: Prepare ICU intime/outtime (ensure POSIXct conversion)

# Step 5: Merge antibiotics data with ICU times
antibiotics_data_merged <- antibiotics_data %>%
  left_join(icu_intime, by = "icustay_id")

# Step 6: Calculate hours since ICU admission, rounded to nearest hour
antibiotics_data_merged <- antibiotics_data_merged %>%
  mutate(
    hours_since_intime = round(as.numeric(difftime(starttime, intime, units = "hours")))
  )

# Step 7: Optional – drop old hr column if exists and rename hours_since_intime
if("hr" %in% colnames(antibiotics_data_merged)){
  antibiotics_data_merged <- antibiotics_data_merged %>%
    select(-hr) %>%
    rename(hr = hours_since_intime)
} else {
  antibiotics_data_merged <- antibiotics_data_merged %>%
    rename(hr = hours_since_intime)
}
#cleaning those that have units other than dose
antibiotics_data_merged_clean <- antibiotics_data_merged %>%
  mutate(
    amountuom = case_when(
      amountuom %in% c("grams", "g","mcg", "mg", "milligrams", "ml", "milliliters") ~ "dose",
      TRUE ~ amountuom  # keep other values as-is
    )
  )
#Factorising amountuom again to make it viewable
antibiotics_data_merged_clean <- antibiotics_data_merged_clean %>%
  mutate(amountuom = factor(amountuom))
#MOre cleaning to eliminate those that have both 0 or NA in amount and totalamount
antibiotics_data_merged_clean1 <- antibiotics_data_merged_clean %>%
  filter(
    !((is.na(amount) | amount == 0) & (is.na(totalamount) | totalamount == 0))
  )
#Eliminate those that have rewritten order or no status description of administrating the medicine
antibiotics_data_merged_clean2 <- antibiotics_data_merged_clean1 %>%
  # Step 1: remove rows with "rewritten"
  filter(statusdescription != "Rewritten") %>%
  # Step 2: create a flag for missing or empty status
  mutate(
    status_missing_flag = ifelse(is.na(statusdescription) | statusdescription == "", 1, 0)
  )
nrow(antibiotics_data_merged_clean)
nrow(antibiotics_data_merged_clean1)
#[1] 164796
nrow(antibiotics_data_merged_clean2) #[1] 159202
#Only using antibiotics_data of the first 24 hours before or after icu admission
antibiotics_24_hours <- antibiotics_data_merged_clean2 %>%
  filter(hr >= -24 & hr <= 24 )
summary(antibiotics_24_hours$hr)
summary(antibiotics_24_hours)
# Step 1: Create a flag for patients who received antibiotics in first 24 hours
# Step 1: create one row per patient with a "Yes" category
antibiotic_flag <- antibiotics_24_hours %>%
  distinct(icustay_id) %>%
  mutate(antibiotic = "Yes")
nrow(antibiotic_flag) #[1] 12380
# Step 2: merge and fill "No" for others
merged_patient_antibiotic <- merged_patient_bloodculture %>%
  left_join(antibiotic_flag, by = "icustay_id") %>%
  mutate(
    antibiotic = factor(ifelse(is.na(antibiotic), "No", antibiotic),
                        levels = c("No", "Yes"))
  )
#Joining admission data
#Cleaning it
admissions_data_basic_cleaning <- admissions_data %>%
  mutate(language = case_when(
    language == "" | is.na(language) ~ "Unknown Language",
    TRUE ~ as.character(language)  # Ensure everything is character first
  )) %>%
  mutate(language = as.factor(language))  # Convert back to factor if you want
summary(admissions_data_basic_cleaning)
nrow(admissions_data) #58976
nrow(admissions_data_basic_cleaning) #58976
#Cleaning the religions
admissions_data_cleaned_part_2 <- admissions_data_basic_cleaning %>%
  mutate(religion = case_when(
    religion == "" | is.na(religion) ~ "Unknown/Not Specified",
    religion %in% c("NOT SPECIFIED", "UNOBTAINABLE") ~ "Unknown/Not Specified",
    religion %in% c("7TH DAY ADVENTIST", "BAPTIST", "EPISCOPALIAN", "GREEK ORTHODOX",
                    "LUTHERAN", "METHODIST", "PROTESTANT QUAKER") ~ "Other Christian",
    religion %in% c("ROMANIAN EAST. ORTH", "UNITARIAN-UNIVERSALIST") ~ "Other",
    TRUE ~ religion
  ))%>%
  mutate(religion = as.factor(religion))
summary(admissions_data_cleaned_part_2)
nrow(admissions_data_cleaned_part_2) #58976

#Cleaning marital status, consolidating unknowns, blanks, marital status
admissions_data_cleaned_part_3 <- admissions_data_cleaned_part_2 %>%
  mutate(marital_status = case_when(
    marital_status == "" | is.na(marital_status) ~ "UNKNOWN",
    marital_status == "UNKNOWN (DEFAULT)" ~ "UNKNOWN",
    marital_status %in% c("LIFE PARTNER", "MARRIED") ~ "PARTNERED",
    marital_status %in% c("DIVORCED", "SEPARATED", "WIDOWED") ~ "PREVIOUSLY PARTNERED",
    marital_status == "SINGLE" ~ "SINGLE",
    TRUE ~ marital_status
  ))%>%
  mutate(marital_status = as.factor(marital_status))


summary(admissions_data_cleaned_part_3$ethnicity)
#Cleaning ethnicities to consolidate them
admissions_data_cleaned_part_4 <- admissions_data_cleaned_part_3 %>%
  mutate(ethnicity = case_when(
    ethnicity == "" | is.na(ethnicity) ~ "UNKNOWN",
    ethnicity %in% c("PATIENT DECLINED TO ANSWER", "UNABLE TO OBTAIN", "UNKNOWN/NOT SPECIFIED") ~ "UNKNOWN",

    # Hispanic/Latino (all subgroups + South American)
    str_detect(ethnicity, "HISPANIC|LATINO") | ethnicity == "SOUTH AMERICAN" ~ "HISPANIC/LATINO",

    # Asian (all subgroups)
    str_detect(ethnicity, "^ASIAN") ~ "ASIAN",

    # Black (all subgroups)
    str_detect(ethnicity, "BLACK|AFRICAN") ~ "BLACK/AFRICAN AMERICAN",

    # White (all subgroups)
    str_detect(ethnicity, "^WHITE") ~ "WHITE",

    # Combine smaller groups
    str_detect(ethnicity, "AMERICAN INDIAN|ALASKA NATIVE") ~ "NATIVE AMERICAN",
    ethnicity == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ "PACIFIC ISLANDER",
    ethnicity == "MULTI RACE ETHNICITY" ~ "MULTIRACIAL",

    # Catch-all for remaining
    TRUE ~ "OTHER"
  ))%>%
  mutate(ethnicity = as.factor(ethnicity))
summary(admissions_data_cleaned_part_4$ethnicity)

summary(admissions_data_cleaned_part_4)

#Cleaning diagnosis
summary(admissions_data_cleaned_part_4$diagnosis)
#Consolidating diagnosis
admissions_data_cleaned_part_5 <- admissions_data_cleaned_part_4 %>%
  mutate(diagnosis = case_when(
    # 1. CARDIAC CONDITIONS ----
    str_detect(diagnosis, "CORONARY ARTERY DISEASE|CAD|ACS|UNSTABLE ANGINA") ~ "CORONARY ARTERY DISEASE",
    str_detect(diagnosis, "MYOCARDIAL INFARCTION|STEMI|ST ELEVATED") ~ "MYOCARDIAL INFARCTION",
    str_detect(diagnosis, "CONGESTIVE HEART FAILURE|CHF") ~ "HEART FAILURE",
    str_detect(diagnosis, "CHEST PAIN") & !str_detect(diagnosis, "CATH") ~ "CHEST PAIN",
    str_detect(diagnosis, "CATH|\\+ETT") ~ "CARDIAC PROCEDURE",

    # 2. NEUROLOGICAL ----
    str_detect(diagnosis, "STROKE|CVA|CEREBROVASCULAR|TIA") ~ "STROKE/TIA",
    str_detect(diagnosis, "INTRACRANIAL|SUBARACHNOID|SUBdURAL|HEAD BLEED|HEMORRHAGE") ~ "INTRACRANIAL HEMORRHAGE",
    str_detect(diagnosis, "SEIZURE") ~ "SEIZURE",
    str_detect(diagnosis, "ALTERED MENTAL STATUS|UNRESPONSIVE") ~ "ALTERED MENTAL STATUS",

    # 3. GASTROINTESTINAL ----
    str_detect(diagnosis, "GI BLEED|GASTROINTESTINAL BLEED") ~ "GI BLEED",
    str_detect(diagnosis, "ABDOMINAL PAIN") ~ "ABDOMINAL PAIN",
    str_detect(diagnosis, "PANCREATITIS") ~ "PANCREATITIS",
    str_detect(diagnosis, "BOWEL OBSTRUCTION|SMALL BOWEL") ~ "BOWEL OBSTRUCTION",
    str_detect(diagnosis, "CHOLECYSTITIS|CHOLANGITIS") ~ "GALLBLADDER DISEASE",

    # 4. RESPIRATORY ----
    str_detect(diagnosis, "PNEUMONIA") ~ "PNEUMONIA",
    str_detect(diagnosis, "ASTHMA|COPD") ~ "ASTHMA/COPD",
    str_detect(diagnosis, "RESPIRATORY FAILURE|DISTRESS|DYSPNEA|SHORTNESS OF BREATH") ~ "RESPIRATORY DISTRESS",
    str_detect(diagnosis, "PULMONARY EMBOL") ~ "PULMONARY EMBOLISM",

    # 5. INFECTIOUS ----
    str_detect(diagnosis, "SEPSIS") & !str_detect(diagnosis, "UROSEPSIS") ~ "SEPSIS",
    str_detect(diagnosis, "UROSEPSIS") ~ "UROSEPSIS",
    str_detect(diagnosis, "URINARY TRACT INFECTION|PYELONEPHRITIS") ~ "URINARY TRACT INFECTION",
    str_detect(diagnosis, "CELLULITIS") ~ "CELLULITIS",
    str_detect(diagnosis, "FEVER") ~ "FEVER",

    # 6. TRAUMA ----
    str_detect(diagnosis, "S/P FALL|FALL") ~ "FALL",
    str_detect(diagnosis, "MOTOR VEHICLE ACCIDENT|MVA|BLUNT TRAUMA|TRAUMA") ~ "TRAUMA",
    str_detect(diagnosis, "HIP FRACTURE") ~ "HIP FRACTURE",

    # 7. METABOLIC/ENDOCRINE ----
    str_detect(diagnosis, "DIABETIC KETOACIDOSIS|DKA") ~ "DIABETIC KETOACIDOSIS",
    str_detect(diagnosis, "HYPONATREMIA|HYPERKALEMIA|HYPOGLYCEMIA|DEHYDRATION") ~ "ELECTROLYTE/METABOLIC",

    # 8. SURGICAL/PROCEDURAL ----
    str_detect(diagnosis, "/SDA") ~ "SURGICAL ADMISSION",

    # 9. Keep major distinct categories ----
    diagnosis %in% c("NEWBORN", "OVERDOSE", "ANEMIA", "LIVER FAILURE",
                     "RENAL FAILURE", "ACUTE RENAL FAILURE", "CARDIAC ARREST") ~ diagnosis,

    # 10. Default ----
    TRUE ~ "OTHER"
  ))%>%
  mutate(diagnosis = as.factor(diagnosis))
summary(admissions_data_cleaned_part_5$diagnosis)
nrow(admissions_data_cleaned_part_5)#58976
summary(admissions_data_cleaned_part_5)
#Checking whether hadm_id is unique
length(unique(admissions_data_cleaned_part_5$hadm_id)) #58976 with unique hadm_id. No dups
length(unique(admissions_data_cleaned_part_5$subject_id)) #46520 people with single hospital admission
#Joining the cleaned admissions data to main data frame
merged_patient_admissions <- merged_patient_antibiotic  %>%
  left_join(admissions_data_cleaned_part_5, by = "hadm_id")

#Final copy
summarised_copy <- merged_patient_admissions
summary(summarised_copy)
#Running statistic tests
#Statistic link between icu care unit and long icu stay
chisq.test(summarised_copy$first_careunit,summarised_copy$long_icu_stay) #p-value < 2.2e-16
#Test for icu stay and gender
chisq.test(summarised_copy$gender,summarised_copy$long_icu_stay) #p-value = 0.3879
#Test for neutrophil
wilcox.test(neutrophil ~ long_icu_stay, data = summarised_copy) #p-value < 2.2e-16

#Dropping irrelevant or future values
no_future_values <- summarised_copy %>%
  select(-dischtime.x,-high_pressure_flag_median,-hadm_id_median,-high_peep_flag_median,-high_tidal_volume_flag_median,-high_peep_flag_median,-duration_hours_median, -subject_id.x,-n_within_hadm,-has_chartevents_data,-edregtime,-edouttime,-discharge_location,-deathtime,-admittime.y,-subject_id.y,-row_id.y.y,-order_by_intime,-row_id.x.x,-dischtime.y,,-vasopressor_first24.y,-icustay_id,-row_id.x,-dbsource,-hadm_id,-last_careunit,-first_wardid,-last_wardid,-outtime.x,-los.x,-row_id.y,-admittime.x,-dischtime.y,-dob.x,-outtime.y,-los.y,-hosp_deathtime,-icu_expire_flag,-hospital_expire_flag.x,-hospital_expire_flag.y,-dod.x,-expire_flag.x,-ttd_days,-dob.y,-dod.y,-dod_hosp,-dod_ssn,-expire_flag.y,-intime,-dod_x_intime,-dod_hosp_intime,-dod_ssn_intime,-hosp_death_intime)
summary(no_future_values)
View(no_future_values)
#Creating separate dataframe to do EDAs
eda_copy <- no_future_values 
#Factorize some binary variables first for EDA
#Factorising norepinephrine used or not
eda_copy$norepinephrine_used <- factor(
  eda_copy$norepinephrine_used,
  levels = c("0", "1"),
  labels = c("No", "Yes")
)
#Factorising epinephrine used or not
eda_copy$epinephrine_used <- factor(
  eda_copy$epinephrine_used,
  levels = c("0", "1"),
  labels = c("No", "Yes")
)
#Factorising dopamine used or not
eda_copy$dopamine_used  <- factor(
  eda_copy$dopamine_used ,
  levels = c("0", "1"),
  labels = c("No", "Yes")
)
#Factorising dobutamine used or not
eda_copy$dobutamine_used  <- factor(
  eda_copy$dobutamine_used ,
  levels = c("0", "1"),
  labels = c("No", "Yes")
)
#Factorising multiple vasopressors used or not
eda_copy$multiple_vasopressors  <- factor(
  eda_copy$multiple_vasopressors ,
  levels = c("0", "1"),
  labels = c("No", "Yes")
)
#Factorising positive culture or not
eda_copy$positive_culture  <- factor(
  eda_copy$positive_culture ,
  levels = c("0", "1"),
  labels = c("No", "Yes")
)
#Final edas to look at what links to what
library(corrplot)
library(vcd)  # for assocstats

# Make sure your outcome is logical or factor
eda_copy$long_icu_stay <- as.logical(eda_copy$long_icu_stay)

# Select numeric variables only
numeric_cols <- eda_copy %>% select(where(is.numeric))



# Function to perform Wilcoxon test safely (handles NAs)
wilcox_test_safe <- function(x, y) {
  if (all(is.na(x))) return(NA)  # skip columns that are all NA
  res <- wilcox.test(x ~ y, na.action = na.omit)
  return(res$p.value)
}

# Apply Wilcoxon test to all numeric columns
wilcox_results <- sapply(numeric_cols, function(x) wilcox_test_safe(x, no_future_values$long_icu_stay))

# Combine with median differences for context
median_diff <- sapply(numeric_cols, function(x) {
  med_true <- median(x[eda_copy$long_icu_stay == TRUE], na.rm = TRUE)
  med_false <- median(x[eda_copy$long_icu_stay == FALSE], na.rm = TRUE)
  med_true - med_false
})

# Create a results dataframe
wilcox_summary <- data.frame(
  variable = names(numeric_cols),
  p_value = wilcox_results,
  median_diff = median_diff
)

# Sort by p-value
wilcox_summary <- wilcox_summary %>% arrange(p_value)

# Identify numeric variables only
numeric_vars <- sapply(no_future_values, is.numeric)
numeric_var_names <- names(numeric_vars[numeric_vars == TRUE])

# Filter Wilcoxon results for numeric variables only
numeric_wilcox <- wilcox_summary %>%
  arrange(p_value) %>%
  mutate(
    median_diff = round(median_diff, 4),  # round median_diff
    p_value_numeric = as.numeric(p_value),  # ensure numeric
    p_value_formatted = ifelse(p_value_numeric < 1e-16, "<1e-16", signif(p_value_numeric, 4))
  )


# Print nicely
numeric_wilcox %>%
  select(variable, median_diff, p_value_formatted) %>%
  kable(caption = "Wilcoxon Test Results: Numeric Variables vs Long ICU Stay")
# Create flextable
ft <- flextable(
  numeric_wilcox %>% 
    select(variable, median_diff, p_value = p_value_formatted)
)

# Export to Word
doc <- read_docx()
doc <- body_add_flextable(doc, ft)

print(doc, target = "wilcoxon_results.docx")

#Plotting interesting variables individually that median wise had no difference
#Starting with ventnum
ggplot(eda_copy, aes(x = long_icu_stay, y = ventnum_median, fill = long_icu_stay)) +
  geom_boxplot(outlier.size = 1, alpha = 0.6) +
  #geom_jitter(width = 0.2, alpha = 0.3) +  # show individual points
  labs(
    title = "Boxplot of ventillation Number by ICU Stay",
    x = "ICU Stay",
    y = "ventillation Number",
    fill = "ICU Stay"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
#Violin plot version
ggplot(eda_copy, aes(x = long_icu_stay, y = ventnum_median, fill = long_icu_stay)) +
  geom_violin(na.rm = TRUE) +
  theme_minimal() +
  labs(x = "Long ICU Stay", y = "Ventnum", title = "Violin Plot of Ventnum by ICU Stay")
#Plotting Chloride next
ggplot(eda_copy, aes(x = long_icu_stay, y = chloride, fill = long_icu_stay)) +
  geom_boxplot(outlier.size = 1, alpha = 0.6) +
  #geom_jitter(width = 0.2, alpha = 0.3) +  # show individual points
  labs(
    title = "Boxplot of Chloride levels by ICU Stay",
    x = "ICU Stay",
    y = "Chloride levels mmol/L",
    fill = "ICU Stay"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
#Violin plot of chloride
ggplot(eda_copy, aes(x = long_icu_stay, y = chloride, fill = long_icu_stay)) +
  geom_violin(na.rm = TRUE) +
  theme_minimal() +
  labs(x = "Long ICU Stay", y = "Chloride", title = "Violin Plot of Chloride by ICU Stay")

#Plotting timelowaprv_median
ggplot(eda_copy, aes(x = long_icu_stay, y = timelowaprv_median, fill = long_icu_stay)) +
  geom_boxplot(outlier.size = 1, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.3) +  # show individual points
  labs(
    title = "Boxplot of Time spent at lower airway Pressure by ICU Stay",
    x = "ICU Stay",
    y = "Time spent at lower airway Pressure by Seconds",
    fill = "ICU Stay"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
#Violin plot of time spent at lower airway pressure
ggplot(eda_copy, aes(x = long_icu_stay, y = ventnum_median, fill = long_icu_stay)) +
  geom_violin(na.rm = TRUE) +
  theme_minimal() +
  labs(x = "Long ICU Stay", y = "timelowaprv_median", title = "Violin Plot of timelowaprv_median by ICU Stay")

#Low aprv 
ggplot(eda_copy, aes(x = long_icu_stay, y = pressurelowaprv_median, fill = long_icu_stay)) +
  geom_boxplot(outlier.size = 1, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.3) +  # show individual points
  labs(
    title = "Boxplot of Lower airway Pressure by ICU Stay",
    x = "ICU Stay",
    y = "cmH2O",
    fill = "ICU Stay"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
#High aprv 
ggplot(eda_copy, aes(x = long_icu_stay, y = pressurehighaprv_median, fill = long_icu_stay)) +
  geom_boxplot(outlier.size = 1, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.3) +  # show individual points
  labs(
    title = "Boxplot of High airway Pressure by ICU Stay",
    x = "ICU Stay",
    y = "cmH2O",
    fill = "ICU Stay"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
#Sodium plot
ggplot(eda_copy, aes(x = long_icu_stay, y = sodium, fill = long_icu_stay)) +
  geom_boxplot(outlier.size = 1, alpha = 0.6) +
  #geom_jitter(width = 0.2, alpha = 0.3) +  # show individual points
  labs(
    title = "Boxplot of Sodium levels by ICU Stay",
    x = "ICU Stay",
    y = "mmol/L",
    fill = "ICU Stay"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
#Fio2 plot
ggplot(eda_copy, aes(x = long_icu_stay, y = fio2_median, fill = long_icu_stay)) +
  geom_boxplot(outlier.size = 1, alpha = 0.6) +
  #geom_jitter(width = 0.2, alpha = 0.3) +  # show individual points
  labs(
    title = "Boxplot of FIO2 levels by ICU Stay",
    x = "ICU Stay",
    y = "%",
    fill = "ICU Stay"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
#Setpeep median plot
ggplot(eda_copy, aes(x = long_icu_stay, y = setpeep_median, fill = long_icu_stay)) +
  geom_boxplot(outlier.size = 1, alpha = 0.6) +
  #geom_jitter(width = 0.2, alpha = 0.3) +  # show individual points
  labs(
    title = "Boxplot of Set Peep levels by ICU Stay",
    x = "ICU Stay",
    y = "cmH2O",
    fill = "ICU Stay"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
#INR median plot
ggplot(eda_copy, aes(x = long_icu_stay, y = intnormalisedratio , fill = long_icu_stay)) +
  geom_boxplot(outlier.size = 1, alpha = 0.6) +
  #geom_jitter(width = 0.2, alpha = 0.3) +  # show individual points
  labs(
    title = "Boxplot of INR levels by ICU Stay",
    x = "ICU Stay",
    y = "INR level",
    fill = "ICU Stay"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
# Inspiratory time
ggplot(eda_copy, aes(x = long_icu_stay, y = insptime_median , fill = long_icu_stay)) +
  geom_boxplot(outlier.size = 1, alpha = 0.6) +
  #geom_jitter(width = 0.2, alpha = 0.3) +  # show individual points
  labs(
    title = "Boxplot of Inspiratory time by ICU Stay",
    x = "ICU Stay",
    y = "Seconds",
    fill = "ICU Stay"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
#Dobutamine max rate plot
ggplot(eda_copy, aes(x = long_icu_stay, y = dobutamine_max , fill = long_icu_stay)) +
  geom_boxplot(outlier.size = 1, alpha = 0.6) +
  #geom_jitter(width = 0.2, alpha = 0.3) +  # show individual points
  labs(
    title = "Boxplot of Dobutamine Maximum Rate by ICU Stay",
    x = "ICU Stay",
    y = "mcg/min",
    fill = "ICU Stay"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
#Vasopressor Count plot
ggplot(eda_copy, aes(x = long_icu_stay, y = pressor_count , fill = long_icu_stay)) +
  geom_boxplot(outlier.size = 1, alpha = 0.6) +
  #geom_jitter(width = 0.2, alpha = 0.3) +  # show individual points
  labs(
    title = "Boxplot of Vasopressor Count by ICU Stay",
    x = "ICU Stay",
    y = "Count",
    fill = "ICU Stay"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
#Age plot
ggplot(eda_copy, aes(x = long_icu_stay, y = age_years , fill = long_icu_stay)) +
  geom_boxplot(outlier.size = 1, alpha = 0.6) +
  #geom_jitter(width = 0.2, alpha = 0.3) +  # show individual points
  labs(
    title = "Boxplot of Age by ICU Stay",
    x = "ICU Stay",
    y = "Years",
    fill = "ICU Stay"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
#Plot of Higher airway Pressure by ICU Stay
ggplot(eda_copy, aes(x = long_icu_stay, y = timehighaprv_median  , fill = long_icu_stay)) +
  geom_boxplot(outlier.size = 1, alpha = 0.6) +
  #geom_jitter(width = 0.2, alpha = 0.3) +  # show individual points
  labs(
    title = "Boxplot of Time spent at Higher airway Pressure by ICU Stay",
    x = "ICU Stay",
    y = "Seconds",
    fill = "ICU Stay"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")



#Now testing categoricals and binaries
chisq.test(eda_copy$first_careunit,eda_copy$long_icu_stay)
chisq.test(eda_copy$admission_type,eda_copy$long_icu_stay)
chisq.test(eda_copy$language ,eda_copy$long_icu_stay)
chisq.test(eda_copy$admission_location ,eda_copy$long_icu_stay)
chisq.test(eda_copy$diagnosis ,eda_copy$long_icu_stay)
chisq.test(eda_copy$gender ,eda_copy$long_icu_stay)
chisq.test(eda_copy$endotrach_median ,eda_copy$long_icu_stay)
chisq.test(eda_copy$ventilated_first24 ,eda_copy$long_icu_stay)
chisq.test(eda_copy$vasopressor_first24.x ,eda_copy$long_icu_stay)
chisq.test(eda_copy$norepinephrine_used ,eda_copy$long_icu_stay)
chisq.test(eda_copy$epinephrine_used ,eda_copy$long_icu_stay)
chisq.test(eda_copy$dopamine_used ,eda_copy$long_icu_stay)
chisq.test(eda_copy$dobutamine_used ,eda_copy$long_icu_stay)
chisq.test(eda_copy$multiple_vasopressors,eda_copy$long_icu_stay)
chisq.test(eda_copy$insurance,eda_copy$long_icu_stay)
chisq.test(eda_copy$religion,eda_copy$long_icu_stay)
chisq.test(eda_copy$marital_status ,eda_copy$long_icu_stay)
chisq.test(eda_copy$ethnicity ,eda_copy$long_icu_stay)
#Creating box plots for visualization 
# 1. Identify numeric columns
num_cols <- names(eda_copy)[sapply(eda_copy, is.numeric)]

# 2. Create output folder
dir.create("individual_plots", showWarnings = FALSE)

# 3. Loop through each numeric variable and save its boxplot
for (var in num_cols) {
  
  p_var <- ggplot(eda_copy, aes(
    x = long_icu_stay,
    y = .data[[var]],
    fill = long_icu_stay
  )) +
    geom_boxplot(outlier.alpha = 0.15) +
    theme_bw() +
    labs(
      title = paste("Boxplot of", var, "by Long ICU Stay"),
      x = "Long ICU Stay",
      y = var
    ) +
    theme(legend.position = "none")
  
  # save each plot to folder
  ggsave(
    filename = paste0("individual_plots/", var, ".png"),
    plot = p_var,
    width = 7,
    height = 5,
    dpi = 300
  )
}

#Visualizations for categorical/binary variables
# Ensure the outcome is a factor
eda_copy$long_icu_stay <- factor(eda_copy$long_icu_stay,
                                 levels = c(FALSE, TRUE),
                                 labels = c("Short icu stay", "Long icu stay"))

# List of categorical variables for EDA (lowercase labels)
cat_vars <- c(
  "first_careunit", "admission_type", "admission_location", "diagnosis",
  "gender", "endotrach_median", "ventilated_first24","vasopressor_first24.x", "norepinephrine_used",
  "epinephrine_used", "dopamine_used", "dobutamine_used",
  "multiple_vasopressors", "insurance", "religion"
)

# Loop through each variable and make a bar plot
for (var in cat_vars) {
  p <- eda_copy %>%
    filter(!is.na(.data[[var]])) %>%  # Remove NAs for plotting
    ggplot(aes(x = .data[[var]], fill = long_icu_stay)) +
    geom_bar(position = "fill") +  # Proportions instead of raw counts
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = paste("proportion of icu stay by", var),
      x = var,
      y = "percentage",
      fill = "icu stay"
    ) +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}

#Missing analysis for each variable
# Summarize missing values for all variables
# Summarize missing values for all variables
clean_labels <- c(
  first_careunit = "First Care Unit",
  age_years = "Age (Years)",
  gender = "Gender",
  long_icu_stay = "Long ICU Stay",
  neutrophil = "Neutrophils",
  creactiveprotein = "C-Reactive Protein",
  whitebloodcell = "White Blood Cell",
  partialpressureo2 = "Partial Pressure of O2",
  bicarbonate = "Bicarbonate",
  lactate = "Lactate",
  troponin = "Troponin",
  bloodureanitrogen = "Blood Urea Nitrogen",
  creatinine = "Creatinine",
  alaninetransaminase = "Alanine Transaminase",
  aspartatetransaminase = "Aspartate Transaminase",
  hemoglobin = "Hemoglobin",
  intnormalisedratio = "International Normalized Ratio (INR)",
  platelets = "Platelets",
  albumin = "Albumin",
  chloride = "Chloride",
  glucose = "Glucose",
  sodium = "Sodium",
  bilirubin = "Bilirubin",
  hematocrit = "Hematocrit",
  spo2_median = "SpO2 (Median)",
  fio2_median = "FiO2 (Median)",
  temp_median = "Temperature (Median)",
  resp_median = "Respiratory Rate (Median)",
  sysbp_median = "Systolic BP (Median)",
  diasbp_median = "Diastolic BP (Median)",
  glucose_median = "Glucose (Median)",
  map_median = "MAP (Median)",
  gcs_median = "GCS (Median)",
  endotrach_median = "Endotracheal Tube Use (Median)",
  charlson = "Charlson Comorbidity Index",
  median_urine_24h = "Urine (Median, 24h)",
  total_urine_24h = "Total Urine (24h)",
  median_avg_weight = "Average Weight (Median)",
  median_min_weight = "Minimum Weight (Median)",
  median_max_weight = "Maximum Weight (Median)",
  ventilated_first24 = "Ventilated in First 24h",
  ventnum_median = "Ventilation Count (Median)",
  minutevolume_median = "Minute Volume (Median)",
  settidalvolume_median = "Set Tidal Volume (Median)",
  obstidalvolume_median = "Observed Tidal Volume (Median)",
  sponttidalvolume_median = "Spontaneous Tidal Volume (Median)",
  setpeep_median = "Set PEEP (Median)",
  totalpeep_median = "Total PEEP (Median)",
  pressurehighaprv_median = "APRV Pressure High (Median)",
  pressurelowaprv_median = "APRV Pressure Low (Median)",
  timehighaprv_median = "APRV Time High (Median)",
  timelowaprv_median = "APRV Time Low (Median)",
  meanairwaypressure_median = "Mean Airway Pressure (Median)",
  peakinsppressure_median = "Peak Inspiratory Pressure (Median)",
  neginspforce_median = "Negative Inspiratory Force (Median)",
  insptime_median = "Inspiratory Time (Median)",
  plateaupressure_median = "Plateau Pressure (Median)",
  hour_median = "Hour (Median)",
  vasopressor_first24.x = "Vasopressors in First 24h",
  norepinephrine_used = "Norepinephrine Used",
  epinephrine_used = "Epinephrine Used",
  dopamine_used = "Dopamine Used",
  dobutamine_used = "Dobutamine Used",
  pressor_count = "Pressor Count",
  multiple_vasopressors = "Multiple Vasopressors",
  norepinephrine_max = "Norepinephrine Max Dose",
  epinephrine_max = "Epinephrine Max Dose",
  dopamine_max = "Dopamine Max Dose",
  dobutamine_max = "Dobutamine Max Dose",
  positive_culture = "Positive Culture",
  antibiotic = "Antibiotic Use",
  admission_type = "Admission Type",
  admission_location = "Admission Location",
  insurance = "Insurance",
  language = "Language",
  religion = "Religion",
  marital_status = "Marital Status",
  ethnicity = "Ethnicity",
  diagnosis = "Diagnosis"
)
#Removing hour as a variable
#eda_copy <- eda_copy %>%
  #select(-hour_median)

# ------------------------------
# 2. CREATE MISSING SUMMARY
# ------------------------------
missing_summary_eda <- eda_copy %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_count"
  ) %>%
  mutate(
    total = nrow(eda_copy),
    missing_pct = missing_count / total * 100
  ) %>%
  filter(missing_pct > 20) %>%
  arrange(desc(missing_pct))


# ------------------------------
# 3. APPLY MANUAL CLEAN LABELS
# ------------------------------
missing_summary_eda$variable_clean <- clean_labels[missing_summary_eda$variable]


# ------------------------------
# 4. PLOT of missing analysis
# ------------------------------
ggplot(missing_summary_eda,
       aes(x = reorder(variable_clean, -missing_pct),
           y = missing_pct,
           fill = variable_clean)) +       # <-- FIXED
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(missing_pct, 1)), hjust = -0.1) +
  coord_flip() +
  labs(
    title = "Variables with >20% Missing Values",
    x = "Variable",
    y = "Percentage Missing",
    fill = "Variable"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")        # Optional: hide legend

#Barplot of outcomes
# Univariate bar plot
ggplot(eda_copy, aes(x = long_icu_stay, fill = long_icu_stay)) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5) +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  labs(
    title = "Long ICU Stay, True or False distribution",
    x = "ICU Stay",
    y = "Count",
    fill = "ICU Stay"
  ) +
  theme_minimal(base_size = 13)
#Preprocessing the main dataframe to remove less relevant variables
preprocessing_df <- no_future_values %>%
  select(-hour_median,-glucose_median,-median_min_weight,-median_max_weight,-sodium,-chloride,-ventnum_median,-fio2_median,-religion,-gender,-age_years,-pressor_count,-religion,-timelowaprv_median,-timehighaprv_median,-pressurelowaprv_median,-pressurehighaprv_median)
#Create a list of binary variables in preparation for converint to numerals for xgboost
# Convert logicals and Yes/No binaries to 0/1
library(fastDummies)

# Convert logicals and Yes/No binaries to 0/1
preprocessing_df <- preprocessing_df %>%
  mutate(across(where(is.logical), ~ as.integer(.))) %>%  # TRUE/FALSE -> 1/0
  mutate(across(where(is.character), ~ case_when(
    . %in% c("Yes", "yes") ~ 1,
    . %in% c("No", "no")   ~ 0,
    TRUE ~ as.character(.)
  )))

# Identify remaining categorical variables (factor or character with >2 levels)
cat_vars <- preprocessing_df %>% 
  select(where(is.factor), where(is.character)) %>% 
  names()

# One-hot encode remaining categorical variables
preprocessing_df_ohe <- fastDummies::dummy_cols(
  preprocessing_df,
  select_columns = cat_vars,
  remove_selected_columns = TRUE
)

# Check structure
str(preprocessing_df_ohe)

# Export
write.csv(preprocessing_df_ohe, "icu_mortality_prediction_data_version_2.csv", row.names = FALSE)

#Version without large missing values at all
no_insiginificants <- no_future_values %>%
  select(-hour_median,-glucose_median,-median_min_weight,-median_max_weight,-alaninetransaminase,-sodium,-chloride,-aspartatetransaminase,-lactate,-bilirubin,-albumin,-ventnum_median,-fio2_median,-troponin,-neutrophil,-partialpressureo2,-religion,-setpeep_median,-minutevolume_median,-neutrophil,-meanairwaypressure_median,-obstidalvolume_median,-peakinsppressure_median,-settidalvolume_median,-fio2_median,-plateaupressure_median,-troponin,-sponttidalvolume_median,-pressor_count,-norepinephrine_max,-norepinephrine_used,-multiple_vasopressors,-epinephrine_used,-epinephrine_max,-dopamine_used,-dopamine_max,-dobutamine_used,-dobutamine_max,-insptime_median,-totalpeep_median,-norepinephrine_used,-norepinephrine_max,-creactiveprotein,-neginspforce_median,-pressurehighaprv_median,-timelowaprv_median,-timehighaprv_median,-pressurelowaprv_median)
#Preprocessing part 2
preprocessing_df_two <- no_insiginificants %>%
  mutate(across(where(is.logical), ~ as.integer(.))) %>%  # TRUE/FALSE -> 1/0
  mutate(across(where(is.character), ~ case_when(
    . %in% c("Yes", "yes") ~ 1,
    . %in% c("No", "no")   ~ 0,
    TRUE ~ as.character(.)
  )))

# Identify remaining categorical variables (factor or character with >2 levels)
cat_vars_two <- preprocessing_df_two %>% 
  select(where(is.factor), where(is.character)) %>% 
  names()

# One-hot encode remaining categorical variables
preprocessing_df_end_two <- fastDummies::dummy_cols(
  preprocessing_df_two,
  select_columns = cat_vars_two,
  remove_selected_columns = TRUE
)

write.csv(preprocessing_df_end_two, "icu_mortality_prediction_data_version_3.csv", row.names = FALSE)

#Only eliminate hours median, total urine and glucose median,median_max_weight,median_min_weight since there are other representative variables. 
no_hours_median <- no_future_values %>%
  select(-hour_median,-glucose_median,-median_min_weight,-median_max_weight)
no_elimination <- no_hours_median  %>%
  mutate(across(where(is.logical), ~ as.integer(.))) %>%  # TRUE/FALSE -> 1/0
  mutate(across(where(is.character), ~ case_when(
    . %in% c("Yes", "yes") ~ 1,
    . %in% c("No", "no")   ~ 0,
    TRUE ~ as.character(.)
  )))

# Identify remaining categorical variables (factor or character with >2 levels)
cat_vars_original <- no_elimination %>% 
  select(where(is.factor), where(is.character)) %>% 
  names()

# One-hot encode remaining categorical variables
preprocessing_df_end_original <- fastDummies::dummy_cols(
  no_elimination,
  select_columns = cat_vars_original,
  remove_selected_columns = TRUE
)

write.csv(preprocessing_df_end_original, "icu_mortality_prediction_data_version_1.csv", row.names = FALSE)
