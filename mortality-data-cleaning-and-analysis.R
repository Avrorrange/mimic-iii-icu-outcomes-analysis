
#Importing the libraries needed
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
library(vcd)
# Read the admissions csv file to examine patient admissions
admissions_data <- read.csv("D:/mimic_data/admissions.csv")

# Clean admissions column names
admissions_data <- admissions_data %>%
  clean_names()
View(admissions_data)
#Convert all character data into factor
admissions_data <- admissions_data %>%
  mutate(across(where(is.character), as.factor))
summary(admissions_data)
#Checking duplications of admissions data
duplicated(admissions_data$subject_id)
anyDuplicated(admissions_data$subject_id)
table(admissions_data$subject_id)

# See which values appear more than once
table(admissions_data$subject_id)[table(admissions_data$subject_id) > 1]


#Importing Labs hourly data
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
  troponin = c(0.01, 30)        # adjust units as needed
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
nrow(labs_hourly_data_flagged_only_flagged)
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

# Unique ICU stays in cleaned data
unique_icustays_clean_labs <- n_distinct(labs_hourly_data_clean$icustay_id)
unique_icustays_clean_labs #60271

#ICU stay file importation
icu_stays_data <- read.csv("D:/mimic_data/icustays.csv")
icu_stays_data <- icu_stays_data%>%
  clean_names()
View(icu_stays_data)
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
#Importing ivu outcome data
icu_outcome_data <- read.csv("D:/mimic_data/pt_icu_outcome.csv",
                             stringsAsFactors = FALSE)

# Clean icu outcome column names
icu_outcome_data <- icu_outcome_data%>%
  clean_names()
#Setting appropriate time and dates for different time variables in icu outcome data
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
  mutate(  #factorising different expire flags
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
#Patient data importation
patient_data <- read.csv("D:/mimic_data/patients.csv")
# Clean Patient data column names
patient_data <- patient_data%>%
  clean_names()
patient_data <- patient_data %>%
  mutate(across(where(is.character), as.factor))
View(patient_data)
#Vitals hourly data importation
vitals_hourly_data <- read.csv("D:/mimic_data/vitals_hourly.csv")
vitals_hourly_data <- vitals_hourly_data%>%
  clean_names()
#Factorising characters
vitals_hourly_data <- vitals_hourly_data %>%
  mutate(across(where(is.character), as.factor))
#Ensuring only first 24 hour data are used
vitals_hourly_data_24 <- vitals_hourly_data %>%
  filter(hr >= 0 & hr <= 24)
View(vitals_hourly_data_24)
#linking icu outcome data to icustays
merged_stays_icu_outcome <- merge(icu_stays_data, icu_outcome_data , by = "icustay_id", all.x = TRUE)
sum(duplicated(icu_outcome_data$icustay_id))
#Checking subject_ids etc are identical
merged_stays_icu_outcome %>%
  filter(subject_id.x != subject_id.y)
#Checking whether the intimes and outcomes are identical
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
#discrepency checks to see whether the times of the merging dataframes are the same
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
any(discrepancies_merged_outtime) #None

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
#Aged under 18
#Not dead but dead in hospital? 8200 such cases
filtered_under_18 <- merged_patient_icu %>%
  filter(age_years < 18 )
#Only using first 24 hour data, will also include data immediately before admission.
labs_hourly_24h <- labs_hourly_data_clean %>%
  filter(hr <= 24)
nrow(labs_hourly_24h) #[1] 368681
#Creating medians for the labs_hourly_data based on icu stay id.
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
# Given there are large numbers of columns with over 20% missing,we are deleting those.
#Shows which columns have significant missings.
missing_summary <- labs_summary %>%
  summarise(across(everything(), ~ mean(is.na(.))*100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "percent_missing") %>%
  arrange(desc(percent_missing))

missing_summary
#Joining labs summary to main icu data
merged_data_icu_labs <- merged_patient_icu %>%
  left_join(labs_summary, by = "icustay_id")
# Select columns to plot (exclude icustay_id and hr)
vitals_plot_data <- vitals_hourly_data %>%
  select(-icustay_id, -hr)

# Convert to long format for ggplot
vitals_long <- vitals_plot_data %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  )

# Basic boxplot (log scale to handle extreme outliers)
ggplot(vitals_long, aes(x = variable, y = value)) +
  geom_boxplot(outlier.colour = "red", outlier.alpha = 0.3) +
  scale_y_continuous(trans = "pseudo_log", breaks = scales::pretty_breaks(n = 10)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Distribution of Hourly Vitals Before Cleaning",
    y = "Value (log scale for extremes)",
    x = "Variable"
  )

#Flagging extreme data for vitals
vitals_hourly_data_flagged <- vitals_hourly_data %>%
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
nrow(vitals_hourly_data)
#Looking at the extreme ones
vitals_hourly_data_flagged_extreme <- vitals_hourly_data_flagged %>%
  filter(extreme_flag == TRUE)
nrow(vitals_hourly_data_flagged_extreme) #522662
#Cleaning but eliminating the extreme flagged vital data
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
#Only using first 24 hours or immediately before admission data
vitals_hourly_24h <- vitals_hourly_data_cleaned %>%
   filter(hr <= 24)
summary(vitals_hourly_24h)
#Creating a median summary of the  vitals by patient icu id. Medians are used to avoid outliers or outright biologically implausible data for now.
vitals_summary <- vitals_hourly_24h %>%
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

#Importing Glascow Coma Score file
gcs_hourly_data <- read.csv("D:/mimic_data/gcs_hourly.csv")
#Clean gcs hourly column names
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
  filter(hr <= 24)
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
  filter(hr <= 24 )
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
#filtering a hard limit based on sanity check
urine_24h_clean <- urine_24h_summary %>%
  filter(median_urine_24h <= 500, total_urine_24h <= 12000) #0 for anuria and 12000 for polyuria
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
#Will need to identify the hour of the mechvent data. Based way to do this would be to subtract chart time from intime from the icu_outcome_data.
#To do this will need to filter only icustay_id and intime and then reverse link files to mechvent_data
icu_intime <- icu_outcome_data %>%
  select(icustay_id, intime,outtime,hadm_id)

icu_intime <- icu_intime %>%
  mutate(
    intime  = as.POSIXct(as.character(intime),  format = "%Y-%m-%d %H:%M:%S"),
    outtime = as.POSIXct(as.character(outtime), format = "%Y-%m-%d %H:%M:%S")
  )

#Merge
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


merged_patient_vent <- merged_patient_weight %>%
  left_join(vent_first24_cat, by = "icustay_id") %>%
  mutate(ventilated_first24 = ifelse(is.na(ventilated_first24), "No", as.character(ventilated_first24))) %>%
  mutate(ventilated_first24 = factor(ventilated_first24, levels = c("No", "Yes")))

# Check the result
table(merged_patient_vent$ventilated_first24) #Numbers add up
#Reading vasopressor file
vasopressor_data <- read.csv("D:/mimic_data/vasopressors.csv")
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

# Step 2: Merge into merged_patient_vent
merged_patient_vent <- merged_patient_vent %>%
  left_join(vasopressor_patient_flag, by = "icustay_id") %>%
  mutate(
    vasopressor_first24 = factor(ifelse(is.na(vasopressor_first24), "No", vasopressor_first24),
                                 levels = c("No", "Yes"))
  )

# Step 3: Quick check
table(merged_patient_vent$vasopressor_first24)
#No   Yes
#54734  6798 Matched

#Reading the blood culture data
bloodculture_data <- read.csv("D:/mimic_data/bloodculture.csv")
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
#Will only use data 24 hours before and after of ICU stay
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
sum(bloodculture_summary$positive_culture == 1, na.rm = TRUE) #11255
# Step 2: Merge into merged_patient_vent
merged_patient_bloodculture <- merged_patient_vent %>%
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
#summary(merged_patient_bloodculture$antibiotic)
#No   Yes
#49152 12380 Sanity checked
#Proceed to cleaning the  admissions_data
View(admissions_data)
summary(admissions_data) #Many individuals with blank language status, blank marital status. Gonna just change it to unknown
summary(admissions_data$language)
summary(admissions_data$religion)
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
#sanity check
nrow(merged_patient_antibiotic)
nrow(merged_patient_admissions)
View(merged_patient_admissions)
#Checking whether hospital expired flags are identical, but first have to factorise y flag.
expire_flag_identical_or_not <- merged_patient_admissions
expire_flag_identical_or_not <- expire_flag_identical_or_not %>%
  mutate(hospital_expire_flag.y = as.factor(hospital_expire_flag.y))
expire_flag_identical_or_not <- expire_flag_identical_or_not %>%
  filter(hospital_expire_flag.x != hospital_expire_flag.y)
nrow(nrow(expire_flag_identical_or_not)) #GOOD.

#importing transfer data
transfer_data <- read.csv("D:/mimic_data/transfers.csv")
transfer_clean_names <- transfer_data %>%
  clean_names()
View(transfer_clean_names)
#Counting number of ids per hadm_id
transfer_clean_names_with_count <- transfer_clean_names %>%
  group_by(hadm_id) %>%
  mutate(
    number_of_icustay_ids = n_distinct(icustay_id[!is.na(icustay_id) & icustay_id != ""])
  ) %>%
  ungroup()
#Checking if there are individuals with different current and prev care units
transfer_clean_names_with_flags <- transfer_clean_names_with_count %>%
  mutate(units_changed = case_when(
    is.na(prev_careunit) | prev_careunit == "" ~ FALSE,
    is.na(curr_careunit) | curr_careunit == "" ~ FALSE,
    prev_careunit == curr_careunit ~ FALSE,
    TRUE ~ TRUE  # Different and both non-missing
  ))
#Filtering only those who had multiple transfers
filtered_transfers <- transfer_clean_names_with_flags %>%
  filter(units_changed == TRUE | number_of_icustay_ids > 1)
#Creating a unique flag for all those hadm_ids who did change icu
icu_changed_hadm_ids <- filtered_transfers %>%
  distinct(hadm_id) %>%
  mutate(icu_changed = TRUE)
#Now join back to main dataframe
merged_patient_admissions_with_icu_transfers <- merged_patient_admissions %>%
  left_join(icu_changed_hadm_ids, by = "hadm_id") %>%
  mutate(icu_changed = ifelse(is.na(icu_changed), FALSE, TRUE))
#Fully merged copy, removing under 18s
under_merged_18 <- merged_patient_admissions_with_icu_transfers %>%
  filter(age_years < 18 )
nrow(under_merged_18) #Total 8200 people
fully_merged_18 <- merged_patient_admissions_with_icu_transfers %>%
  filter(age_years >= 18 )
nrow(fully_merged_18) #53332, removed 8200 people
#Counting number of times people have the same hadm_id
more_than_one_hadm_id <- fully_merged_18 %>%
  group_by(hadm_id) %>%
   summarise(count_hadmi = n ())
#Merge this count back to main dataframe
fully_merged_18_hadm_id_count <- fully_merged_18 %>%
  left_join(more_than_one_hadm_id, by = "hadm_id")
#Sanity check
nrow(fully_merged_18_hadm_id_count) #[1] 53332, no changes in numbers, good
View(fully_merged_18_hadm_id_count)
#Next, eliminate any hadmi with more than one count since we do not want bias results of individuals having multiple icus
fully_merged_18_hadm_id_count_more_than_one <- fully_merged_18_hadm_id_count %>%
  filter(count_hadmi > 1)
#Counting number of icu_stays with identical hadmi_ids
nrow(fully_merged_18_hadm_id_count_more_than_one) #6788
#Will eliminate these 6788 rows
fully_merged_18_hadm_id_count_only_one <- fully_merged_18_hadm_id_count %>%
  filter(count_hadmi == 1 )
nrow(fully_merged_18_hadm_id_count_only_one) #[1] 46544
#Sanity check to see if there are any icustay_ids where first icu is not the same as last
first_care_not_last_care_unit <- fully_merged_18_hadm_id_count_only_one %>%
  filter( first_careunit != last_careunit )
nrow(first_care_not_last_care_unit)#[1] 3263
#Will also eliminate these
fully_merged_first_care_equals_last <- fully_merged_18_hadm_id_count_only_one %>%
  filter( first_careunit == last_careunit )
nrow(fully_merged_first_care_equals_last) #[1] 43281

View(fully_merged_first_care_equals_last)
#Now change if there are any more individuals with the icu_changed variable having TRUE
any_more_icu_change_true <- fully_merged_first_care_equals_last %>%
  filter(icu_changed == TRUE )  # ✅ == is for comparison
nrow(any_more_icu_change_true) #293
#Checking these rows
looking_row <- transfer_clean_names_with_flags %>% filter(hadm_id == 166606 )
View(looking_row) #This individual has for example changed from CCU to SICU
looking_row <- transfer_clean_names_with_flags %>% filter(hadm_id == 145540 )
View(looking_row) #This individual has for example changed from TSICU to SICU
looking_row <- transfer_clean_names_with_flags %>% filter(hadm_id == 103074 )
View(looking_row) #This individual has for example changed from MICU to SICU
#Checking number of rows in fully_merged_first_care_equals_last
nrow(fully_merged_first_care_equals_last)#43281
#Next is to completely filter out those who have icu_changed == TRUE
fully_merged_first_care_equals_last <- fully_merged_first_care_equals_last %>%
  filter(icu_changed == FALSE )
nrow(fully_merged_first_care_equals_last) #42988
summary(fully_merged_first_care_equals_last)
#Coalecsing the hospital_expire_flag since we already found that they are the same and simplifying things
#Have to make sure that hospital_expire_flag.y is also a factor like hospital_expire_flag.x first
fully_merged_first_care_equals_last <- fully_merged_first_care_equals_last %>%
  mutate(hospital_expire_flag.y = as.factor(hospital_expire_flag.y))
#Also sanity check first
fully_merged_first_care_equals_last_hospital_deaths_reconciliation <- fully_merged_first_care_equals_last %>%
  filter(hospital_expire_flag.x != hospital_expire_flag.y)
summary(fully_merged_first_care_equals_last$hospital_expire_flag.x) # 0 34151, 1 1165, NA 7672
summary(fully_merged_first_care_equals_last$hospital_expire_flag.y) #0 38467 1 4521
#There is discrepency in NA, but since the filter showed that the ones that are not NA matched, we simply just coalesce and use hospital_expire_flag.y data since it's more comprehensive.
fully_merged_first_care_equals_last_1 <- fully_merged_first_care_equals_last %>%
  mutate(
    hospital_expire_flag = coalesce(hospital_expire_flag.x, hospital_expire_flag.y)
  ) %>%
  select(-hospital_expire_flag.x, -hospital_expire_flag.y)

#Factorize Death in hospital
fully_merged_first_care_equals_last_1$hospital_expire_flag <-
  as.factor(fully_merged_first_care_equals_last_1$hospital_expire_flag)
#Factorize expire flag
fully_merged_first_care_equals_last_1$expire_flag.x  <-
  as.factor(fully_merged_first_care_equals_last_1$expire_flag.x)
#Checking whether there are logical defects in deaths
death_in_hospital_not_expired <- fully_merged_first_care_equals_last_1 %>%
  filter( hospital_expire_flag == 1 & expire_flag.x == 0)
nrow(death_in_hospital_not_expired) #0
#Checking expired but alive in hospital
alive_in_hospital_expired <- fully_merged_first_care_equals_last_1 %>%
  filter( hospital_expire_flag == 0 & expire_flag.x == 1) #[1] 13802 I think this makes sense as patient could have subsequently died after leaving hospital.
nrow(alive_in_hospital_expired)
#Checking whether some of these NAs are listed as expired elsewhere
dead_unknown_hospital <- fully_merged_first_care_equals_last_1 %>%
  filter(is.na(hospital_expire_flag) & expire_flag.x == 1)
nrow(dead_unknown_hospital) #0
summary(dead_unknown_hospital$dod_hosp)

#Coalesce dischtimes and admitime first
fully_merged_first_care_equals_last_1 <- fully_merged_first_care_equals_last_1 %>%
 select(-dischtime.y,-admittime.y)

# Quick check for zombies, but have to
dead_before_disch <-  fully_merged_first_care_equals_last_1 %>%
  filter(dod.x <= dischtime.x & hospital_expire_flag == 0)
nrow(dead_before_disch)#39 zombies lol.In all seriousness, they died on the same date, just on different time.
# Identify rows with NA hospital_death_flag
na_hospital_death <- fully_merged_first_care_equals_last_1 %>%
  filter(is.na(hospital_expire_flag))
nrow(na_hospital_death) # [1] 0

#Now remove all the rest of the NAs in case there are.
fully_merged_death_status_cleaning <- fully_merged_first_care_equals_last_1 %>%
  filter(!is.na(hospital_expire_flag))
summary(fully_merged_first_care_equals_last$hospital_expire_flag)
summary(fully_merged_death_status_cleaning$hospital_expire_flag) #Fully cleaned
nrow(fully_merged_death_status_cleaning) #[1] none really distroyed, 42988
#Mutating dischtime.x to dischtime
fully_merged_death_status_cleaning <- fully_merged_death_status_cleaning %>%
  rename(dischtime = dischtime.x)
# Convert death-time columns to POSIXct
cols_to_date <- c("dod.x", "hosp_deathtime", "dod_hosp", "dod_ssn", "dischtime")

# Convert all relevant columns to Date
fully_merged_death_status_cleaning[cols_to_date] <- lapply(
  fully_merged_death_status_cleaning[cols_to_date],
  function(x) {
    if (is.factor(x)) x <- as.character(x)  # first convert factors to character
    as.Date(x)  # then convert to Date
  }
)



# Now run the sanity check
nrow(fully_merged_death_status_cleaning) #42988
sanity_check <- fully_merged_death_status_cleaning %>%
  filter(
    hospital_expire_flag == 0 &
      (
        (!is.na(dod.x)        & dod.x        <= dischtime) |
          (!is.na(hosp_deathtime) & hosp_deathtime <= dischtime) |
          (!is.na(dod_hosp)    & dod_hosp    <= dischtime) |
          (!is.na(dod_ssn)     & dod_ssn     <= dischtime)
      )
  )

nrow(sanity_check) #40
# Remove inconsistent rows flagged by the sanity check
fully_merged_death_status_cleaning <- anti_join(
  fully_merged_death_status_cleaning,
  sanity_check,
  by = "icustay_id"
)

nrow(fully_merged_death_status_cleaning) #42948
#Dropping identity information,future information and nonessential not already represented like age
fully_merged_no_identity <- fully_merged_death_status_cleaning %>%
  select(-icustay_id,-dod_ssn,-admittime.x,-dischtime,-icu_expire_flag,-hosp_deathtime,-first_wardid,-last_careunit,-last_wardid,-dbsource,-hadm_id, -subject_id.x, -subject_id.y, -row_id.x, -row_id.y, -row_id.x.x, -row_id.y.y,
         -dob.x, -dob.y, -dod.x, -dod.y, -deathtime,-los.y,-outtime.y,-outtime.x,-ttd_days,-los.x,-edregtime,-edouttime,-discharge_location,-intime,-ttd_days,-expire_flag.x,-expire_flag.y,-dod_hosp,-count_hadmi,-icu_changed,-has_chartevents_data)
#Renaming first_careunit to icuunit
fully_merged_no_identity <- fully_merged_no_identity %>%
  rename(icuunit = first_careunit)
summary(fully_merged_no_identity)
#Doing violin plots for numerical values vs outcome, hospital death.
library(tidyverse)

# Ensure outcome is a factor for plotting
fully_merged_no_identity <- fully_merged_no_identity %>%
  mutate(hospital_expire_flag = as.factor(hospital_expire_flag))

# Identify numeric variables
numeric_vars <- fully_merged_no_identity %>%
  select(where(is.numeric)) %>%
  names()

# Create a list to store the plots
violin_plots <- list()

for (var in numeric_vars) {
  
  p <- ggplot(fully_merged_no_identity, 
              aes(x = hospital_expire_flag, 
                  y = .data[[var]], 
                  fill = hospital_expire_flag)) +
    geom_violin(trim = FALSE, alpha = 0.6) +
    geom_boxplot(width = 0.1, outlier.size = 0.5, alpha = 0.5) +   # shows median
    labs(
      title = paste("Violin Plot of", var, "vs Hospital Expire Flag"),
      x = "Hospital Expiration (0 = Alive, 1 = Expired)",
      y = var
    ) +
    theme_bw() +
    theme(legend.position = "none")
  
  violin_plots[[var]] <- p
}

# To show the numerical violin plots:
violin_plots[["age_years"]]
violin_plots[["neutrophil"]]
violin_plots[["creactiveprotein"]]
violin_plots[["whitebloodcell"]]
violin_plots[["partialpressureo2"]]
violin_plots[["bicarbonate"]]
violin_plots[["lactate"]]
violin_plots[["troponin"]]
violin_plots[["bloodureanitrogen"]]
violin_plots[["creatinine"]]
violin_plots[["alaninetransaminase"]]
violin_plots[["aspartatetransaminase"]]
violin_plots[["hemoglobin"]]
violin_plots[["intnormalisedratio"]]
violin_plots[["platelets"]]
violin_plots[["albumin"]]
violin_plots[["chloride"]]
violin_plots[["glucose"]]
violin_plots[["sodium"]]
violin_plots[["bilirubin"]]
violin_plots[["hematocrit"]]
violin_plots[["spo2_median"]]
violin_plots[["temp_median"]]
violin_plots[["resp_median"]]
violin_plots[["sysbp_median"]]
violin_plots[["diasbp_median"]]
violin_plots[["glucose_median"]]
violin_plots[["map_median"]]
violin_plots[["gcs_median"]]
violin_plots[["charlson"]]
violin_plots[["median_urine_24h"]]
violin_plots[["total_urine_24h"]]
violin_plots[["median_avg_weight"]]
violin_plots[["median_avg_weight"]]
#Plots for categorical values
# Identify categorical variables
categorical_vars <- fully_merged_no_identity %>%
  select(where(~ is.character(.) | is.factor(.))) %>%
  select(-hospital_expire_flag) %>%   # exclude outcome
  names()

# Create list to store the plots
cat_plots <- list()

for (var in categorical_vars) {
  
  p <- fully_merged_no_identity %>%
    ggplot(aes(x = .data[[var]], fill = hospital_expire_flag)) +
    geom_bar(position = "fill") +   # proportion bars
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = paste("Proportion of Hospital Expire Flag by", var),
      x = var,
      y = "Percent",
      fill = "Expired (1 = Yes)"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  cat_plots[[var]] <- p
}

# Plots for each categorical variable
cat_plots[["icuunit"]]
cat_plots[["gender"]]
cat_plots[["endotrach_median"]]
cat_plots[["ventilated_first24"]]
cat_plots[["vasopressor_first24"]]
cat_plots[["positive_culture"]]
cat_plots[["antibiotic"]]
cat_plots[["admission_type"]]
cat_plots[["admission_location"]]
cat_plots[["insurance"]]
cat_plots[["language"]]
cat_plots[["religion"]]
cat_plots[["marital_status"]]
cat_plots[["ethnicity"]]
cat_plots[["diagnosis"]]
cat_plots[["diagnosis"]]
#Running chi square test between categorical values 
chisq.test(fully_merged_no_identity$icuunit, fully_merged_no_identity$hospital_expire_flag)
chisq.test(fully_merged_no_identity$gender, fully_merged_no_identity$hospital_expire_flag)
chisq.test(fully_merged_no_identity$endotrach_median, fully_merged_no_identity$hospital_expire_flag)
chisq.test(fully_merged_no_identity$ventilated_first24, fully_merged_no_identity$hospital_expire_flag)
chisq.test(fully_merged_no_identity$vasopressor_first24, fully_merged_no_identity$hospital_expire_flag)
#Changing positive culture to factor first before chi square test
fully_merged_no_identity <- fully_merged_no_identity%>%
  mutate(positive_culture = as.factor(positive_culture))
#More chisquared tests
chisq.test(fully_merged_no_identity$positive_culture , fully_merged_no_identity$hospital_expire_flag)
chisq.test(fully_merged_no_identity$admission_type , fully_merged_no_identity$hospital_expire_flag)
chisq.test(fully_merged_no_identity$admission_location , fully_merged_no_identity$hospital_expire_flag)
chisq.test(fully_merged_no_identity$insurance , fully_merged_no_identity$hospital_expire_flag)
chisq.test(fully_merged_no_identity$language , fully_merged_no_identity$hospital_expire_flag)
chisq.test(fully_merged_no_identity$religion , fully_merged_no_identity$hospital_expire_flag)
chisq.test(fully_merged_no_identity$marital_status , fully_merged_no_identity$hospital_expire_flag)
chisq.test(fully_merged_no_identity$ethnicity , fully_merged_no_identity$hospital_expire_flag)
chisq.test(fully_merged_no_identity$diagnosis , fully_merged_no_identity$hospital_expire_flag)
#Wilcox and T-tests feor numerical data
t.test(neutrophil ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(neutrophil ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(creactiveprotein ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(creactiveprotein ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(whitebloodcell ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(whitebloodcell ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(partialpressureo2 ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(partialpressureo2 ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(bicarbonate ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(bicarbonate ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(lactate ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(lactate ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(troponin ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(troponin ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(bloodureanitrogen ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(bloodureanitrogen ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(creatinine ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(creatinine ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(alaninetransaminase ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(alaninetransaminase ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(aspartatetransaminase ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(aspartatetransaminase ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(hemoglobin ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(hemoglobin ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(intnormalisedratio ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(intnormalisedratio ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(platelets ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(platelets ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(albumin ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(albumin ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(chloride ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(chloride ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(glucose ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(glucose ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(sodium ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(sodium ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(bilirubin ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(bilirubin ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(hematocrit ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(hematocrit ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(spo2_median ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(spo2_median ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(fio2_median ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(fio2_median ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(temp_median ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(temp_median ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(resp_median ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(resp_median ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(sysbp_median ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(sysbp_median ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(diasbp_median ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(diasbp_median ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(glucose_median ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(glucose_median ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(map_median ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(map_median ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(gcs_median ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(gcs_median ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(charlson ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(charlson ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(median_urine_24h ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(median_urine_24h ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
t.test(median_avg_weight ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
wilcox.test(median_avg_weight ~ hospital_expire_flag, data = fully_merged_death_status_cleaning) #p-value < 2.2e-16
# Going to run correlation tests between continuous variables and hospital death tests. 
#List all continuous variables to be tested
vars <- c(
  "neutrophil", "creactiveprotein", "whitebloodcell", "partialpressureo2",
  "bicarbonate", "lactate", "troponin", "bloodureanitrogen", "creatinine",
  "alaninetransaminase", "aspartatetransaminase", "hemoglobin",
  "intnormalisedratio", "platelets", "albumin", "chloride", "glucose",
  "sodium", "bilirubin", "hematocrit", "spo2_median", "fio2_median",
  "temp_median", "resp_median", "sysbp_median", "diasbp_median",
  "glucose_median", "map_median", "gcs_median", "charlson",
  "median_urine_24h", "median_avg_weight"
)

# Function to safely perform both tests with NA removal
run_tests <- function(var) {
  df <- fully_merged_death_status_cleaning[, c(var, "hospital_expire_flag")]
  df <- na.omit(df)

  # Skip if one group empty
  if (length(unique(df$hospital_expire_flag)) < 2) {
    return(tibble(
      variable = var,
      t_test_p = NA_real_,
      wilcox_p = NA_real_
    ))
  }

  t_p <- t.test(df[[var]] ~ df$hospital_expire_flag)$p.value
  w_p <- wilcox.test(df[[var]] ~ df$hospital_expire_flag)$p.value

  tibble(
    variable = var,
    t_test_p = t_p,
    wilcox_p = w_p
  )
}

# Run for all variables
results <- purrr::map_dfr(vars, run_tests)

results


#Now doing multicolinearity analysis

# First, see ALL column names
cat("All column names:\n")
print(names(fully_merged_no_identity))

# Check which columns are numeric
numeric_cols <- names(fully_merged_no_identity)[sapply(fully_merged_no_identity, is.numeric)]
cat("\nNumeric columns:\n")
print(numeric_cols)

# Let's just work with what we actually have
continuous_predictors <- numeric_cols

# Remove variables with too many NAs
continuous_predictors <- continuous_predictors[sapply(continuous_predictors, function(x) {
  sum(!is.na(fully_merged_no_identity[[x]])) > 10000
})]

cat("\nContinuous predictors after removing high-NA variables:\n")
print(continuous_predictors)

# Now check correlations
cor_matrix <- fully_merged_no_identity[, continuous_predictors] %>%
  cor(use = "complete.obs")

# Find highly correlated pairs
high_corr <- which(abs(cor_matrix) > 0.3 & upper.tri(cor_matrix), arr.ind = TRUE)
high_corr_pairs <- data.frame(
  var1 = rownames(cor_matrix)[high_corr[, 1]],
  var2 = colnames(cor_matrix)[high_corr[, 2]],
  correlation = cor_matrix[high_corr]
) %>%
  arrange(desc(abs(correlation)))

cat("\nHighly correlated continuous predictors (|r| > 0.4):\n")
print(high_corr_pairs)

# Now analysing categorical variables
#Get categorical predictors
categorical_predictors <- names(fully_merged_no_identity)[sapply(fully_merged_no_identity, function(x) is.factor(x) | is.character(x))]
cat("Categorical predictors:\n")
print(categorical_predictors)

# Check associations between key clinical categorical variables using Cramer's V

# Focus on the main clinical categorical variables first
key_clinical_cats <- c("endotrach_median", "ventilated_first24", "vasopressor_first24",
                       "positive_culture", "antibiotic", "icuunit", "admission_type")

# Only use variables that actually exist
key_clinical_cats <- key_clinical_cats[key_clinical_cats %in% categorical_predictors]

cat("\nKey clinical categorical variables to check:\n")
print(key_clinical_cats)

# Calculate Cramer's V for all pairs
cat_associations <- combn(key_clinical_cats, 2, simplify = FALSE) %>%
  map_dfr(function(pair) {
    tbl <- table(fully_merged_no_identity[[pair[1]]], fully_merged_no_identity[[pair[2]]])
    cramer_v <- assocstats(tbl)$cramer
    tibble(
      var1 = pair[1],
      var2 = pair[2],
      cramers_v = cramer_v
    )
  })

cat("\nCramer's V between clinical categorical variables:\n")
print(cat_associations %>% arrange(desc(cramers_v)))

# Check which have high association (Cramer's V > 0.5 indicates strong association)
high_assoc <- cat_associations %>%
  filter(cramers_v > 0.5)

cat("\nStrongly associated categorical pairs (Cramer's V > 0.5):\n")
print(high_assoc)

#Numerical missing tests
# 1. Select numeric columns, excluding redundant ones
numeric_cols <- c(
  "age_years", "neutrophil", "creactiveprotein", "whitebloodcell",
  "partialpressureo2", "bicarbonate", "lactate", "troponin",
  "bloodureanitrogen", "creatinine", "alaninetransaminase", "aspartatetransaminase",
  "hemoglobin", "intnormalisedratio", "platelets", "albumin",
  "chloride", "glucose", "sodium", "bilirubin",
  "hematocrit", "spo2_median", "fio2_median", "temp_median",
  "resp_median", "sysbp_median", "diasbp_median", "glucose_median",
  "map_median", "gcs_median", "charlson", "median_urine_24h",
  "median_avg_weight"
)

# 2. Create readable names
readable_names <- c(
  "Age (years)", "Neutrophil", "C-Reactive Protein", "White Blood Cell",
  "PaO2", "Bicarbonate", "Lactate", "Troponin",
  "BUN", "Creatinine", "ALT", "AST",
  "Hemoglobin", "INR", "Platelets", "Albumin",
  "Chloride", "Glucose", "Sodium", "Bilirubin",
  "Hematocrit", "SpO2 (median)", "FiO2 (median)", "Temperature (median)",
  "Respiration Rate (median)", "Systolic BP (median)", "Diastolic BP (median)", "Glucose (median)",
  "MAP (median)", "GCS (median)", "Charlson Index", "Urine Output (24h median)",
  "Weight (median)"
)

# 3. Compute % missing
missing_df <- tibble(
  variable = numeric_cols,
  readable_name = readable_names,
  missing_pct = sapply(fully_merged_no_identity[numeric_cols], function(x) {
    mean(is.na(x)) * 100
  })
) %>%
  arrange(desc(missing_pct))

# 4. Plotting the degree of missingness for numerical variables
ggplot(missing_df, aes(x = reorder(readable_name, -missing_pct), y = missing_pct)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Percentage of Missing Values per Variable",
    x = "Numeric Variable",
    y = "Missing Percentage (%)"
  ) +
  theme_minimal(base_size = 14)

#Now continue testing continous vs categorical association

# 1️⃣ Define predictorsbased on the numerical variables and categorical/binary variables
numeric_cols <- names(fully_merged_no_identity)[sapply(fully_merged_no_identity, is.numeric)]
categorical_cols <- names(fully_merged_no_identity)[sapply(fully_merged_no_identity, function(x) is.factor(x) | is.character(x))]

# 2️⃣Testing  Numeric-Numeric correlations again
cor_matrix <- cor(fully_merged_no_identity[, numeric_cols], use = "pairwise.complete.obs")

num_cor <- expand.grid(var1 = rownames(cor_matrix), var2 = colnames(cor_matrix)) %>%
  as_tibble() %>%
  mutate(correlation = as.vector(cor_matrix)) %>%
  filter(abs(correlation) > 0.7 & var1 != var2)

# 3️⃣ Numeric-Categorical associations
num_cat_assoc <- map_dfr(numeric_cols, function(cont_var) {
  map_dfr(categorical_cols, function(cat_var) {
    x <- fully_merged_no_identity[[cont_var]]
    g <- fully_merged_no_identity[[cat_var]]

    complete_idx <- complete.cases(x, g)
    x <- x[complete_idx]
    g <- g[complete_idx]

    if(length(x) == 0 | length(unique(g)) < 2) return(NULL)

    g <- as.factor(g)

    if(length(unique(g)) == 2) {
      # Binary categorical → point-biserial correlation
      effect_size <- abs(cor(x, as.numeric(g) - 1))
      method <- "point_biserial"
    } else {
      # Multi-level categorical → eta-squared ANOVA
      fit <- aov(x ~ g)
      ss <- summary(fit)[[1]]$"Sum Sq"
      effect_size <- ss[1] / sum(ss)
      method <- "eta_squared"
    }

    tibble(
      var1 = cont_var,
      var2 = cat_var,
      effect_size = effect_size,
      method = method
    )
  })
}) %>%
  filter((method == "point_biserial" & effect_size > 0.7) | (method == "eta_squared" & effect_size > 0.14))

# 4️⃣ Results
multicollinearity_results <- list(
  numeric_numeric = num_cor,
  numeric_categorical = num_cat_assoc
)

# Preview
multicollinearity_results$numeric_numeric %>% arrange(desc(abs(correlation)))
multicollinearity_results$numeric_categorical %>% arrange(desc(effect_size))


#We are dropping the variables with over 20% and some duplicates like tota-urine_24h, min weight, max weight
dropping_large_missing_columns <- fully_merged_no_identity %>%
  select(-lactate,-partialpressureo2,-median_min_weight,-median_max_weight,-total_urine_24h,-neutrophil,-alaninetransaminase,-aspartatetransaminase,-bilirubin,-albumin,-troponin,-fio2_median,-creactiveprotein)
nrow(fully_merged_no_identity)#42948
nrow(dropping_large_missing_columns)#42948
#Next, we also remove one copy of variables that are highly correlated with one another based on higher p-value,higher level of missingness and lower clinical relevance
dropping_large_missing_columns_two <- dropping_large_missing_columns %>%
  select(-hematocrit,-sysbp_median,-bloodureanitrogen,-endotrach_median,-bicarbonate,-diasbp_median,-glucose_median,-sodium)
#ELIMINATE ALL Nas.
# Remove rows with any NAs across all columns
fully_merged_no_identity_clean <- na.omit(dropping_large_missing_columns_two)

# Check the dimensions before and after
cat("Before removing NAs:", dim(fully_merged_no_identity), "\n") #42948 52
cat("After removing NAs:", dim(fully_merged_no_identity_clean), "\n") # 32917 31
nrow(fully_merged_no_identity_clean) #32917

# Make sure categorical variables are factors
cleaned_df <- fully_merged_no_identity_clean %>%
  mutate(
    positive_culture = as.factor(positive_culture),
  )
#Now we are ready to begin a logistic regression. We start the first model with all the predictors
# Select predictors (exclude outcome) using all the variables
predictors <- setdiff(names(cleaned_df), "hospital_expire_flag")

# Build formula
formula <- as.formula(paste("hospital_expire_flag ~", paste(predictors, collapse = " + ")))

# Fit logistic regression
logmod <- glm(formula, data = cleaned_df, family = binomial)

# Summary of model
summary(logmod) #AIC: 14672

# Run logistic regression part 2 with out highly significant variantes
log_model <- glm(
  hospital_expire_flag ~ antibiotic+icuunit+age_years+whitebloodcell+creatinine+hemoglobin+intnormalisedratio+platelets+chloride+glucose+spo2_median+temp_median+resp_median+map_median+gcs_median+charlson+median_urine_24h+median_avg_weight+ventilated_first24+vasopressor_first24+admission_type+insurance+religion+marital_status+diagnosis,
  data = cleaned_df,
  family = binomial(link = "logit")
)
# View summary
summary(log_model) #AIC: 14961


#Model 2 Dropping creatinine,religion
log_model2 <- glm(
  hospital_expire_flag ~ icuunit+age_years+whitebloodcell+hemoglobin+intnormalisedratio+platelets+chloride+glucose+spo2_median+temp_median+resp_median+map_median+gcs_median+charlson+median_urine_24h+median_avg_weight+ventilated_first24+vasopressor_first24+admission_type+insurance+marital_status+diagnosis,
  data = cleaned_df,
  family = binomial(link = "logit")
)
# View summary
summary(log_model2) #AIC: 15009

# Predict probabilities on the dataset log_model. Think this is the most reasonable
# Predict probabilities on the dataset
pred_probs <- predict(log_model, type = "response")

# Compute ROC curve
roc_obj <- roc(cleaned_df$hospital_expire_flag, pred_probs)

# Plotting the ROC curve

plot(1 - roc_obj$specificities, roc_obj$sensitivities,
     type = "l",
     col = "#1c61b6",
     lwd = 2,
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)",
     main = "ROC Curve Final Model"
)
abline(a = 0, b = 1, lty = 2, col = "gray")

# Get and print AUC
auc_value <- auc(roc_obj)
print(auc_value)  # Example: 0.8947
# Get AUC
auc_value <- auc(roc_obj)
print(auc_value) #Area under the curve: 0.8947

# Extract coefficients
coef_vals <- coef(log_model)
betas <- coef(log_model)
print(betas)

# Compute odds ratios
odds_ratios <- exp(coef_vals)

# Compute 95% confidence intervals
conf_int <- exp(confint(log_model))

# Combine into a neat table
or_table <- data.frame(
  Variable = names(odds_ratios),
  OR = odds_ratios,
  CI_lower = conf_int[,1],
  CI_upper = conf_int[,2]
)

or_table




