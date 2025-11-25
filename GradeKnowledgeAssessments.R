# AUTHOR:       Victoria Hurd
# DATE CREATED: 11/24/25
# LAST EDITED:  11/25/25
# PROJECT:      MDRS Teleguidance Study
# TASK:         Grading Knowledge Assessments
# OUTPUTS:      Graded Data Excel Sheet

# NOTES: 1) Make sure raw data folder and output data folder have been created
#        2) Double-check the name of the data file and key file in user inputs
#        3) Double-check path to data folder and git repo

# ------------------------------------------------------------------------------
### USER INPUTS ###

# Name of raw knowledge assessment data from Qualtrics
dataFile = 'MDRS Ultrasound Knowledge Assessment_November 25, 2025_09.47.xlsx'
# Name of answer key Excel sheet
keyFile = 'MDRS Ultrasound Knowledge Assessment_KEY.xlsx'
# Name of graded datafile to be outputted
outputFile = "KnowledgeAssessmentData.xlsx"
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
# Sets repo path
setwd(gitPath)
# Source supporting functions
files.sources = list.files(paste(gitPath,'lib/', sep = ""))
files.sources = paste(paste(gitPath,'lib/', sep = ""), files.sources, sep = "")
sapply(files.sources, source)

# ------------------------------------------------------------------------------
### READ ###

# Read in raw data, as export from Qualtrics in .xlsx
data <- read_excel(paste(dataPath,rawDataFolder,dataFile,sep = ""))

# Read in key 
KEY <- read_excel(paste(dataPath,rawDataFolder,keyFile,sep = ""))

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
labelCols = c("ID","Mission","Role","Date","Timing","Duration")

# Get standardized participant ID based on role/week
data$ID <- mapply(getParticipantID, data$Role, data$Mission)

# Reorder columns so labels (ID, week, date, duration, role, timing) are first
data <- data %>%
  select(all_of(labelCols), everything())

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

# If any entries are NA, make them blank strings for grading comparison
data[is.na(data)] <- ''

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
  select(all_of(labelCols), everything())


# ------------------------------------------------------------------------------
### GRADE ###

# Create binary graded dataframe and summarize T/F totals at the end
# Compare strings for all questions to the key
graded <- data.frame(t(apply(data, 1, function(row) row == KEY)))

# Add the column names back
colnames(graded) <- colnames(data)

# Add the labels back
graded[,labelCols] <- data[,labelCols]

# Summarize total number of true at end
graded$Score = rowSums(graded[!(colnames(graded) %in% labelCols)])


# ------------------------------------------------------------------------------
### OUTPUT ###

# Tabulate and display to check cleaning outputs - should be 2 per role per week
table(data$Role,data$Mission)

# Tabulate and display to check score per question
# Helps us catch if KEY has issues
true_counts <- sapply(graded[!(colnames(graded) %in% labelCols)], 
                      function(col) sum(col == TRUE, na.rm = TRUE))
false_counts <- sapply(graded[!(colnames(graded) %in% labelCols)], 
                       function(col) sum(col == FALSE, na.rm = TRUE))
result_table <- t(data.frame(TRUE_Count = true_counts, 
                             FALSE_Count = false_counts))
print(result_table)

# Output graded knowledge assessments
write_xlsx(graded, paste(dataPath,outputDataFolder,outputFile,sep=''))

