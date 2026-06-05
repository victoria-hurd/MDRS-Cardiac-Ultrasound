# AUTHOR:       Victoria Hurd
# DATE CREATED: 12/02/25
# LAST EDITED:  12/17/25
# PROJECT:      MDRS Teleguidance Study
# TASK:         Cleaning Demographics Data
# OUTPUTS:      Cleaned Demographics Data Excel Sheet

# ------------------------------------------------------------------------------
### USER INPUTS ###

# Name of raw knowledge assessment data from Qualtrics
dataFile = 'MDRS Ultrasound Study Demographics Survey_December 17, 2025_10.29.xlsx'
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


# Convert dates from Excel native numeric to simple %m/%d/Y via supporting func
# Apply supporting function to entire recorded date column
df$RecordedDate <- convertDate(df$RecordedDate)

# Rename columns
df <- df %>%
  rename(Mission = `Crew Number: `,
         Date = RecordedDate,
         Role = `Mission Role:`,
         Duration = `Duration (in seconds)`)

# Clean mission into just week numerics
pattern <- "\\s*\\([^)]+\\)"
df$Mission <- gsub(pattern, "", df$Mission)
df$Mission <- gsub("\\D+", "", df$Mission)

# Change duration to mins
df$Duration <- round(as.numeric(df$Duration)/60)

# ------------------------------------------------------------------------------
### TABULATE ###

table(df$Mission,df$Role)
table(df$Sex,df$Gender)

print(mean(as.numeric(df$Duration)))
