# AUTHOR:       Victoria Hurd
# DATE CREATED: 4/16/26
# LAST EDITED:  4/24/26
# PROJECT:      MDRS Ultrasound Task Data Analysis
# TASK:         Create master dataframe will all collected data
# OUTPUTS:      Saves single dataframe with all data

# ------------------------------------------------------------------------------
### USER INPUTS ###

# Path to Git Repo
homePath='/Users/vickihurd/GitHub/MDRS-Cardiac-Ultrasound/'
# Path to stored data
dataFolder = '/Users/vickihurd/Library/CloudStorage/OneDrive-SharedLibraries-UCB-O365/Ultrasound - Documents/Aim 2 - MDRS Cardiac Teleguidance Study/Data Analysis/Data/'
rawDataFolder = 'Raw Data/'
# Individual Names per Data Stream
surveysFile = 'Post-Scan Surveys/MDRS Post-Scan Surveys_manuallycleaned.xlsx'
trainingFile = 'Knowledge Assessments/MDRS Ultrasound Knowledge Assessment_December 2, 2025_10.55.xlsx'
trainingKeyFile = 'Knowledge Assessments/MDRS Ultrasound Knowledge Assessment_KEY.xlsx'
demographicsFile = 'Demographics/MDRS Ultrasound Study Demographics Survey_January 19, 2026_11.03.xlsx'
imageQualityFile = 'Grader Data/MDRS Teleguidance_concatenated_results.xlsx'
acqTimesFile = 'acquisitionTimes.xlsx'
# Output Paths and Filenames
cleanDataFolder = 'Clean Data/'
outputMasterFile = "MasterDatafile.rds"
outputTrainingFile = "TrainingData.xlsx"
outputNASATLXFile = "NASATLXData.xlsx"
outputSUSFile = "SUSData.xlsx"
outputTimesFile = "AcqTimesData.xlsx"
outputDemographicsFile = "DemographicsData.xlsx"
outputImageQualityFile = "ImageQualityData.xlsx"
outputCombinedImageQualityFile = "ImageQualityDataCombinedGraders.xlsx"
outputMasterFile = "MasterDataframe.xlsx"

# ------------------------------------------------------------------------------
### ADMIN ###
# Set working dir
setwd(homePath)

# Read in all pertinent libraries
library(readxl)
library(dplyr)
library(tidyr)

# Load custom functions into memory
source("cleaningFuncs.R")
source("imageQualityGradingFuncs.R")

# ------------------------------------------------------------------------------
### DATA READ ###
dfSurveys <- read_excel(paste(dataFolder,rawDataFolder,surveysFile,sep = ""))
dfTraining <- read_excel(paste(dataFolder,rawDataFolder,trainingFile,sep = ""))
dfTrainingKey <- read_excel(paste(dataFolder,rawDataFolder,trainingKeyFile,sep = ""))
dfDemographics <- read_excel(paste(dataFolder,rawDataFolder,demographicsFile,sep = ""))
dfImageQuality <- read_excel(paste(dataFolder,rawDataFolder,imageQualityFile,sep = ""))
dfAcqTimes <- read_excel(paste(dataFolder,rawDataFolder,acqTimesFile,sep = ""))

# ------------------------------------------------------------------------------
### DATA CLEAN ###

# Clean training data
dfTraining <- cleanTrainingData(dfTraining,dfTrainingKey,
                            paste(dataFolder,cleanDataFolder,outputTrainingFile,sep = ""))
# Re-format training data - per participant, need before and after score
dfTraining <- formatTrainingData(dfTraining)

# Clean survey data
dfSurveys <- cleanSurveyData(dfSurveys,
                        paste(dataFolder,cleanDataFolder,outputTrainingFile,sep = ""))
dfNASATLX <- scoreNASATLX(dfSurveys,
                          paste(dataFolder,cleanDataFolder,outputNASATLXFile,sep = ""))
dfSUS <- scoreSUS(dfSurveys,
                  paste(dataFolder,cleanDataFolder,outputSUSFile,sep = ""))

# Clean acquisition Times
dfAcqTimes <- cleanAcqTimes(dfAcqTimes,
                      paste(dataFolder,cleanDataFolder,outputTimesFile,sep = ""))

# Clean demographics data
dfDemographics <- cleanDemographics(dfDemographics,
                            paste(dataFolder,cleanDataFolder,outputDemographicsFile,sep = ""))

# Clean grader data
dfImageQuality <- dfImageQuality %>%
  separate(original_filename, into = c("Role", "Mission","Day","Condition"), sep = "_", remove = FALSE) %>%
  mutate(Condition = gsub(".mp4", "", Condition)) %>%
  mutate(Order = as.numeric(gsub("\\D", "", Condition))) %>%
  select(-all_of(c(c("Role", "Mission","Day","Condition"))))

dfImageQuality <- cleanImageQuality(dfImageQuality,
                                    paste(dataFolder,cleanDataFolder,outputImageQualityFile,sep = ""))

dfImageQuality <- dfImageQuality %>%
  mutate(Day = ifelse(Day == "Thurs", "Thursday", Day)) %>%
  mutate(Day = ifelse(Day == "Weds", "Wednesday", Day))

dfImageQuality$Mission <- as.numeric(dfImageQuality$Mission)

# Add Timepoint to Image Quality based on Order/Day for Medic and Acq Time for everyone else
dfImageQuality <- dfImageQuality %>%
  left_join(dfAcqTimes, by = c("ID","Mission","Role","Day","Condition")) %>%
  select(-"acqTime") %>%
  mutate(Timepoint = case_when(
      !is.na(Order) & Day == "Wednesday" & Order == 1 ~ 1,
      !is.na(Order) & Day == "Wednesday" & Order == 2 ~ 2,
      !is.na(Order) & Day == "Thursday"  & Order == 1 ~ 3,
      !is.na(Order) & Day == "Thursday"  & Order == 2 ~ 4,
      TRUE ~ Timepoint)) %>%
  select(-"Order")



# ------------------------------------------------------------------------------
### GRADE IMAGE QUALITY ###

# Grade, find medians/sums/means, combine per unique video ID
dfImageQuality <- combineAcrossGraders(dfImageQuality,
                                       paste(dataFolder,cleanDataFolder,outputCombinedImageQualityFile,sep = ""))

# ------------------------------------------------------------------------------
### CLEAN IMAGE QUALITY ###

# Additional exams taken in week 3. For now remove anything from tuesday I guess
# Consider taking first exams completed instead in the future
table(dfImageQuality$Role,dfImageQuality$Mission)
dfImageQuality <- dfImageQuality %>%
  filter(Day != "Tues")

# ------------------------------------------------------------------------------
### CREATE MASTER SPREADSHEET ###
# Match data across all variables to each line
table(dfAcqTimes$Role,dfAcqTimes$Mission)
table(dfSUS$Role,dfSUS$Mission)
table(dfNASATLX$Role,dfNASATLX$Mission)
table(dfImageQuality$Role,dfImageQuality$Mission)
table(dfDemographics$Role,dfDemographics$Mission)
table(dfTraining$Role,dfTraining$Mission)

dfDemographics$Mission <- as.numeric(dfDemographics$Mission)

key_cols <- c("ID", "Mission","Role","Timepoint","Day","Condition")
df_final <- dfAcqTimes %>%
  left_join(dfImageQuality, by = key_cols) %>%
  left_join(dfSUS, by = key_cols) %>%
  left_join(dfNASATLX, by = key_cols) %>%
  left_join(dfTraining, by = c("ID", "Mission","Role")) %>%
  left_join(dfDemographics, by = c("ID", "Mission","Role"))

# Save final dataframe with everything
write_xlsx(df_final, paste(dataFolder,cleanDataFolder,outputMasterFile,sep = ""))
