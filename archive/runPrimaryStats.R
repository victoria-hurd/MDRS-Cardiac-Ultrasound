# AUTHOR:       Victoria Hurd
# DATE CREATED: 11/25/25
# LAST EDITED:  11/25/25
# PROJECT:      MDRS Teleguidance Study
# TASK:         Concatenating cleaned data, running stats, generating plots
# OUTPUTS:      Single Excel sheet w/ all data, stats printouts, plot generation 

# ------------------------------------------------------------------------------
### USER INPUTS ###

# Path to Git Repo
gitPath='/Users/vickihurd/GitHub/MDRS-Cardiac-Ultrasound/'
# Path to stored data
dataPath = '/Users/vickihurd/Library/CloudStorage/OneDrive-SharedLibraries-UCB-O365/Ultrasound - Documents/Aim 2 - MDRS Cardiac Teleguidance Study/Results/Clean Data/'

# Name of cleaned image quality datafile from [cleanQualityData.R]
qualityFile = ''
# Name of cleaned survey datafile from [cleanSurveys.R]
surveyFile = ''
# Name of manually-inputted stopwatch datafile
timeFile = ''
# Name of cleaned knowledge assessment datafile from GradeKnowledgeAssessments.R
trainingFile = 'KnowledgeAssessmentData.xlsx'
# Name of cleaned demographics datafile from [cleanDemographics.R]
demographicsFile = ''
# Name of final concatenated datafile to be outputted
outputFile = 'MDRSTeleguidanceAllData.xlsx'

# ------------------------------------------------------------------------------
### ADMIN ###

# Read in all pertinent libraries
library(readxl)
library(dplyr)
library(writexl)
# Sets repo path
setwd(gitPath)
# Source supporting functions
files.sources = list.files(paste(gitPath,'lib/', sep = ""))
files.sources = paste(paste(gitPath,'lib/', sep = ""), files.sources, sep = "")
sapply(files.sources, source)

# ------------------------------------------------------------------------------
### READ ###

# Read in cleaned datafiles
#qualityData <- read_excel(paste(dataPath,qualityFile,sep = ""))
#surveyData <- read_excel(paste(dataPath,surveyFile,sep = ""))
#timeData <- read_excel(paste(dataPath,timeFile,sep = ""))
trainingData <- read_excel(paste(dataPath,trainingFile,sep = ""))
#demographicsData <- read_excel(paste(dataPath,demographicsFile,sep = ""))

# ------------------------------------------------------------------------------
### CONCATENATE DATA ###

# Concatenate into one large dataset, saving column names from each file
#labelCols = c("ID","Mission","Role","Date","Scan","Condition")
#qualityCols = <everything else in quality dataset>
#timeCols = <everything else in time dataset>
#trainingCols = c("Before Score","After Score")
#demographicsCols = <everything else in demographics dataset>

# ------------------------------------------------------------------------------
### RUN STATS ###


# ------------------------------------------------------------------------------
### CREATE PLOTS ###


# ------------------------------------------------------------------------------
### OUTPUTS ###

# Tabulate - look at before vs after scores
table(trainingData$Timing,trainingData$Score)
table(trainingData$Role,trainingData$Score)
