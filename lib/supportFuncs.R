# AUTHOR:       Victoria Hurd
# DATE CREATED: 11/24/25
# LAST EDITED:  11/24/25
# PROJECT:      MDRS Teleguidance Study
# TASK:         Supporting Functions for Data Analysis

# SUPPORTING FUNCTIONS:
#   1) getParticpantID: takes week and role (already deidentified) to generate 
#   unique ID for data analysis
#   2) getWeek: gets mission week (1, 2, or 3) based on survey submit date
#   3) convertDate: converts date from numerical native Excel to R date object 
#   4) cleanQualtrics: cleans raw survey data from Qualtrics

# ------------------------------------------------------------------------------
### ADMIN ###

# Sets repo path
path='/Users/vickihurd/Library/CloudStorage/OneDrive-SharedLibraries-UCB-O365/Ultrasound - Documents/Aim 2 - MDRS Cardiac Teleguidance Study/Results'
setwd(path)
# Read in all pertinent libraries
library(readxl)

# ------------------------------------------------------------------------------

getParticipantID <- function(crewRole,weekNumber) {
  # Leverage 2 switch statements to generate a standardized participant ID 
  # crewRole provides a number 1 to 6, weekNumber is multiplicative factor
  # EX. Crew Medic (role #3) for week 2 (factor of 1) = 3 + 6*1 = ID #9
  
  # Exceptions are handled with catch outputs (i.e. "Role?" and "Week?")
  
  # Use role for numbers 1 to 6
  roleNumber = switch(crewRole, "Co-Commander A" = 1,
                      "Co-Commander B" = 2, 
                      "Crew Medic" = 3, 
                      "Crew Engineer" = 4, 
                      "Crew Scientist" = 5, 
                      "GreenHab Officer" = 6,
                      "Role?") 
  
  # Use week number for multiplication
  multiplyFactor = switch(weekNumber, "1" = 0, 
                          "2" = 1, 
                          "3" = 2,
                          "Week?") 
  
  # Calculate Participant ID
  ID <- roleNumber + (6*multiplyFactor)
  
  # Return ID
  ID 
}


getWeek <- function(submitDate) {
  # Leverage 3 if-else statements to identify week number
  # Input date should be direct from Qualtrics in format = "%m/%d/%Y
  # Use RecordedDate
  
  # Ensure date datatypes, input 
  submitDate <- as.Date(submitDate)
  print(submitDate)
  missionStartDates <- c("2025-11-09","2025-11-16","2025-11-30")
  missionEndDates <- c("2025-11-15","2025-11-22","2025-12-06")
  
  # Check if target_date is between start_date and end_date
  if (submitDate >= as.Date(missionStartDates[1]) & submitDate <= as.Date(missionEndDates[1])){
    weekNumber <- 1
  } else if (submitDate >= as.Date(missionStartDates[2]) & submitDate <= as.Date(missionEndDates[2])){
    weekNumber <- 2
  } else if (submitDate >= as.Date(missionStartDates[3]) & submitDate <= as.Date(missionEndDates[3])){
    weekNumber <- 3
  } else {
    weekNumber <- NaN
  }
  
  # Return weekNumber
  weekNumber 
}

convertDate <- function(date) {
  # Convert dates to R Date object, using Excel origin as argument, rounding
  # numeric date from Excel sheet to 10 decimal places since we only need the day
  
  ExcelOrigin <- '1899-12-30' # Define Excel origin time
  date <- as.Date(round(as.numeric(date),10), origin = ExcelOrigin) # convert
  
  # Return date
  return(date)
}

cleanQualtrics <- function(df) {
  # Clean up Qualtrics exported data, including removing top row and 
  # unnecessary columns that we won't use
  
  # Remove first row
  df <- df[-1, ]
  
  # Remove unnecessary columns
  df <- subset(df, select = -c(StartDate,EndDate,Status,IPAddress,Progress,
                                   Finished,ResponseId,RecipientFirstName,
                                   RecipientLastName,RecipientEmail,
                                   ExternalReference,LocationLatitude,
                                   LocationLongitude,DistributionChannel,
                                   UserLanguage,Q_DataPolicyViolations))
  
  # Return dataframe
  return(df)
}
