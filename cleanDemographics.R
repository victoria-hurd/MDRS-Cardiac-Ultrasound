# AUTHOR:       Victoria Hurd
# DATE CREATED: 12/02/25
# LAST EDITED:  12/03/25
# PROJECT:      MDRS Teleguidance Study
# TASK:         Cleaning Demographics Data
# OUTPUTS:      Cleaned Demographics Data Excel Sheet

# ------------------------------------------------------------------------------
### USER INPUTS ###

# Name of raw knowledge assessment data from Qualtrics
dataFile = 'MDRS Ultrasound Study Demographics Survey_November 24, 2025_14.03.xlsx'
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