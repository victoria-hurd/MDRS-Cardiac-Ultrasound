# AUTHOR:       Victoria Hurd
# DATE CREATED: 11/24/25
# LAST EDITED:  11/25/25
# PROJECT:      MDRS Teleguidance Study
# TASK:         Grading Knowledge Assessments

# OUTPUTS:      Graded Data Excel Sheet

# ------------------------------------------------------------------------------
### USER INPUTS ###
# Name of raw knowledge assessment data from Qualtrics
dataFile = 'MDRS Ultrasound Knowledge Assessment_November 24, 2025_14.03.xlsx'
# Name of answer key Excel sheet
keyFile = 'MDRS Ultrasound Knowledge Assessment_KEY.xlsx'
# Path to stored data
dataPath = '/Users/vickihurd/Library/CloudStorage/OneDrive-SharedLibraries-UCB-O365/Ultrasound - Documents/Aim 2 - MDRS Cardiac Teleguidance Study/Results/Raw Data/'
# Path to Git Repo
gitPath='/Users/vickihurd/GitHub/MDRS-Cardiac-Ultrasound/'

# ------------------------------------------------------------------------------
### ADMIN ###
# Read in all pertinent libraries
library(readxl)
library(dplyr)
# Sets repo path
setwd(gitPath)
# Source supporting functions
files.sources = list.files(paste(gitPath,'lib/', sep = ""))
files.sources = paste(paste(gitPath,'lib/', sep = ""), files.sources, sep = "")
sapply(files.sources, source)

# ------------------------------------------------------------------------------
### READ ###

# Read in raw data, as export from Qualtrics in .xlsx
data <- read_excel(paste(dataPath,dataFile,sep = ""))

# Read in key 
KEY <- read_excel(paste(dataPath,keyFile,sep = ""))

# ------------------------------------------------------------------------------
### CLEAN ###

# Clean up unnecessary rows and columns with correlated supporting func
data <- cleanQualtrics(data)
KEY <- cleanQualtrics(KEY)

# Convert dates from Excel native numeric to simple %m/%d/Y via supporting func
# Apply supporting function to entire recorded date column
data$RecordedDate <- convertDate(data$RecordedDate)

# Get mission week based on converted and standardized recorded date
data$Mission <- sapply(data$RecordedDate, getWeek)

# Rename label columns
colnames(data)[colnames(data) == "Duration (in seconds)"] <- "Duration"
colnames(data)[colnames(data) == "QID30"] <- "Role"
colnames(data)[colnames(data) == "QID28"] <- "Timing"
colnames(data)[colnames(data) == "RecordedDate"] <- "Date"

# Get standardized participant ID based on role/week
data$ID <- mapply(getParticipantID, data$Role, data$Mission)

# Reorder columns so labels (ID, week, date, duration, role, timing) are first
data <- data %>%
  select("ID","Mission","Role","Date","Timing","Duration", everything())

# Sort by participant ID
data <- arrange(data, ID)

# Remove erroneous entries
# There are three ID3 entries from week 1 - remove the one in row 7
# We know we can remove since it's an incorrect date, diff IP address, diff lat/long
data <- data[-c(7), ]
# There are three ID5 entries from week 1 - remove the one taken earlier in the day
# We know we can remove since participant wasn't scheduled to take earlier in the day
data <- data[-c(9), ]
#data <- data[-c(8), ]

# Clean answer key - add columns, reorder them
# Add columns to KEY with 0s to signify key
colnames(KEY)[colnames(KEY) == "Duration (in seconds)"] <- "Duration"
colnames(KEY)[colnames(KEY) == "QID30"] <- "Role"
colnames(KEY)[colnames(KEY) == "QID28"] <- "Timing"
colnames(KEY)[colnames(KEY) == "RecordedDate"] <- "Date"
KEY$Date <- 0
KEY$Mission <- 0
KEY$ID <- 0 
KEY$Role <- 'KEY'
KEY <- KEY %>%
  select("ID","Mission","Role","Date","Timing","Duration", everything())


# ------------------------------------------------------------------------------
### GRADE ###


# ------------------------------------------------------------------------------
### OUTPUT ###


# Output graded knowledge assessments
