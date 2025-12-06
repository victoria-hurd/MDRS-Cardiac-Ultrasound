# AUTHOR:       Victoria Hurd
# DATE CREATED: 11/25/25
# LAST EDITED:  11/25/25
# PROJECT:      MDRS Teleguidance Study
# TASK:         Scoring Survey Data
# OUTPUTS:      Scored and Cleaned Survey Data Excel Sheet

# ------------------------------------------------------------------------------
### USER INPUTS ###

# Name of raw knowledge assessment data from Qualtrics
dataFile = 'MDRS Post-Scan Surveys_manuallycleaned.xlsx'
# Name of graded datafile to be outputted
outputFile = "surveyData.xlsx"
# Path to stored data
dataPath = '/Users/vickihurd/Library/CloudStorage/OneDrive-SharedLibraries-UCB-O365/Ultrasound - Documents/Aim 2 - MDRS Cardiac Teleguidance Study/Results/'
# Raw data folder
rawDataFolder = 'Raw Data/'
# Output data folder
outputDataFolder = 'Clean Data/'
# Path to Git Repo
gitPath='/Users/vickihurd/GitHub/MDRS-Cardiac-Ultrasound/'

# ------------------------------------------------------------------------------
### ADMIN ###

# Read in all pertinent libraries
library(readxl)
library(dplyr)
library(writexl)
library(stringr)
# Sets repo path
setwd(gitPath)
# Source supporting functions
files.sources = list.files(paste(gitPath,'lib/', sep = ""))
files.sources = paste(paste(gitPath,'lib/', sep = ""), files.sources, sep = "")
sapply(files.sources, source)

# ------------------------------------------------------------------------------
### READ ###

# Read in raw data, as export from Qualtrics in .xlsx
df <- read_excel(paste(dataPath,rawDataFolder,dataFile,sep = ""))

# ------------------------------------------------------------------------------
### CLEAN ###

# Clean Qualtrics data
df <- cleanQualtrics(df)

# Remove first row
df <- df[-1, ]
# Also drop duration (first col)
df <- df[ ,-1]

# Convert dates from Excel native numeric to simple %m/%d/Y via supporting func
# Apply supporting function to entire recorded date column
df$RecordedDate <- convertDate(df$RecordedDate)

# Get mission week based on converted and standardized recorded date
df$Mission <- sapply(df$RecordedDate, getWeek)

# Remove row if day of the week isn't Weds or Thurs, remove mission if nan
string1 <- "Wednesday"
string2 <- "Thursday"
df <- df %>%
  filter(str_detect(`Day of the Week`, paste(string1, string2, sep = "|"))) %>%
  filter(!is.nan(Mission))

# Rename label columns
colnames(df)[colnames(df) == "Who"] <- "Role"
colnames(df)[colnames(df) == "RecordedDate"] <- "Date"
colnames(df)[colnames(df) == "Day of the Week"] <- "Day"
colnames(df)[colnames(df) == "Scan Order"] <- "Order"
labelCols = c("ID","Mission","Role","Date")

# Get standardized participant ID based on role/week
df$ID <- mapply(getParticipantID, df$Role, df$Mission)

# Check that the days/orders are equal for all 4 categories (12 in each)
table(df$Day,df$Order)

# Rename conditions and define factor levels, then get Scan Number based on 
# day of the week and first/second scan of the day
df <- df %>%
  mutate(Condition = ifelse(Condition == "Solo Self-Scan", "Unassisted", Condition)) %>%
  mutate(Condition = ifelse(Condition != "Unassisted", "Teleguided", Condition)) %>%
  mutate(Condition = factor(Condition, levels = c("Unassisted", "Teleguided"))) %>%
  mutate(
    Timepoint = case_when(
      Day == "Wednesday" & Order == "First" ~ 1,
      Day == "Wednesday" & Order == "Second" ~ 2,
      Day == "Thursday" & Order == "First" ~ 3,
      Day == "Thursday" & Order == "Second" ~ 4,
      TRUE ~ 0 # Default case for unmatched conditions
    )
  )


# Reorder columns so labels (ID, week, date, duration, role, timing) are first
labelCols = c(labelCols,"Timepoint","Condition")
df <- df %>%
  select(all_of(labelCols), everything())

# Sort by participant ID
df <- arrange(df, ID)

# Check that the timepoints per role are equal for all (3 in each)
table(df$Role,df$Timepoint)

# Fill in default slider values for NASA TLX and SUS
# NASA TLX default: 50
# SUS default: 3
nasaDefault <- 50
susDefault <- 3
# Get NASA TLX/SUS columns based on string
nasaCols <- colnames(df)[grepl("NASA", colnames(df))]
susCols <-  colnames(df)[grepl("SUS", colnames(df))]
# Place default values
df[ ,nasaCols] <- df %>%
  select(all_of(nasaCols)) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  replace(is.na(.), nasaDefault)
df[ ,susCols] <- df %>%
  select(all_of(susCols)) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  replace(is.na(.), susDefault)

# ------------------------------------------------------------------------------
### SUS Scoring ###
susCols <- colnames(df)[grepl("SUS", colnames(df))]

df <- df %>%
  # Scale to either (5-x) or (x-1) based on Brooke (1995)
  mutate(SUS_1 = SUS_1 - 1,
         SUS_2 = 5 - SUS_2,
         SUS_3 = SUS_3 - 1,
         SUS_4 = 5 - SUS_4,
         SUS_5 = SUS_5 - 1,
         SUS_6 = 5 - SUS_6,
         SUS_7 = SUS_7 - 1,
         SUS_8 = 5 - SUS_8,
         SUS_9 = SUS_9 - 1,
         SUS_10 = 5 - SUS_10) %>% 
  mutate(susScore = rowSums(across(susCols)) * 2.5) %>% 
  select(-susCols)

# ------------------------------------------------------------------------------
### NASA TLX Weighting & Scoring ###
# For all "Q" questions, sum number of times each metric appears. Put in new col
# We can then delete the original columns
# Start by renaming slider columns to something intuitive, 1 col per construct
df <- df %>%
  rename(
    "NASA TLX Mental Demand" = "NASA TLX_1",
    "NASA TLX Physical Demand" = "NASA TLX_2",
    "NASA TLX Temporal Demand" = "NASA TLX_3",
    "NASA TLX Performance" = "NASA TLX_4",
    "NASA TLX Effort" = "NASA TLX_5",
    "NASA TLX Frustration" = "NASA TLX_6"
  )
# Grab column names for the weights and sliders
pairwiseCols <- colnames(df)[grepl("Q", colnames(df))]
sliderCols <- colnames(df)[grepl("NASA", colnames(df))]

dfTallies <- df %>%
  select(all_of(pairwiseCols)) %>%
  mutate(MentalDemandTally = rowSums(. == "Mental Demand",na.rm = TRUE)) %>%
  mutate(PhysicalDemandTally = rowSums(. == "Physical Demand",na.rm = TRUE)) %>%
  mutate(TemporalDemandTally = rowSums(. == "Temporal Demand",na.rm = TRUE)) %>%
  mutate(PerformanceTally = rowSums(. == "Performance",na.rm = TRUE)) %>%
  mutate(EffortTally = rowSums(. == "Effort",na.rm = TRUE)) %>%
  mutate(FrustrationTally = rowSums(. == "Frustration",na.rm = TRUE)) %>%
  select(c("MentalDemandTally","PhysicalDemandTally","TemporalDemandTally",
         "PerformanceTally","EffortTally","FrustrationTally"))

# Replace pairwise questions with tally weightings
df <- cbind(df,dfTallies) %>%
  select(all_of(c(labelCols,weightingCols,sliderCols,"susScore")))

weightingCols <- colnames(df)[grepl("Tally", colnames(df))]

talliesStats <- df %>%
  select(all_of(weightingCols)) %>%
  mutate(totalsCheck = rowSums(across(weightingCols))) %>%
  summarize(across(
    .cols = is.numeric, 
    .fns = list(mean = mean, mode = getMode)))

# Use the weightings to sum the total workload, place total workload into col
df <- df %>%
  mutate(totalWorkload = 
           ((MentalDemandTally * `NASA TLX Mental Demand`) + 
           (PhysicalDemandTally * `NASA TLX Physical Demand`) + 
           (TemporalDemandTally * `NASA TLX Temporal Demand`) + 
           (PerformanceTally * `NASA TLX Performance`) + 
           (EffortTally * `NASA TLX Effort`) + 
           (FrustrationTally * `NASA TLX Frustration`)) /15)  %>% 
  relocate(susScore, .after = last_col())
  
# ------------------------------------------------------------------------------
### OUTPUT ###

# Output graded knowledge assessments
write_xlsx(df, paste(dataPath,outputDataFolder,outputFile,sep=''))
  