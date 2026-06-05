# AUTHOR:       Victoria Hurd
# DATE CREATED: 11/24/25
# LAST EDITED:  4/17/26
# PROJECT:      MDRS Ultrasound Task Data Analysis
# TASK:         Helper Functions for Cleaning Data Streams

library(dplyr)
library(tidyr)
library(writexl)
library(stringr)

# ------------------------------------------------------------------------------
### EXAMPLE ###
example <- function(df) {
    # New func
  return(df)
}
# ------------------------------------------------------------------------------
### CLEAN QUALTRICS DATA ###
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

# ------------------------------------------------------------------------------
### CREATE PARTICIPANT ID ###
createParticipantID <- function(crewRole,weekNumber) {
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
  return(ID) 
}

# ------------------------------------------------------------------------------
### GET WEEK FROM DATE ###
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

# ------------------------------------------------------------------------------
### CONVERT EXCEL DATE TO R DATE ###
convertDate <- function(date) {
  # Convert dates to R Date object, using Excel origin as argument, rounding
  # numeric date from Excel sheet to 10 decimal places since we only need the day
  
  ExcelOrigin <- '1899-12-30' # Define Excel origin time
  date <- as.Date(round(as.numeric(date),10), origin = ExcelOrigin) # convert
  
  # Return date
  return(date)
}

# ------------------------------------------------------------------------------
### CLEAN TRAINING DATA ###
cleanTrainingData <- function(data,KEY,outputPath) {
  # CLEAN #
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
  data$ID <- mapply(createParticipantID, data$Role, data$Mission)
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
  
  # GRADE #
  # Create binary graded dataframe and summarize T/F totals at the end
  # Compare strings for all questions to the key
  graded <- data.frame(t(apply(data, 1, function(row) row == KEY)))
  # Add the column names back
  colnames(graded) <- colnames(data)
  # Add the labels back
  graded[,labelCols] <- data[,labelCols]
  # Summarize total number of true at end
  graded$Score = rowSums(graded[!(colnames(graded) %in% labelCols)])
  
  # OUTPUT #
  write_xlsx(graded, outputPath) # Save plain training data
  return(graded)
  
}

# ------------------------------------------------------------------------------
### FORMAT TRAINING DATA ###
formatTrainingData <- function(df) {
  # Pivot wider such that each participant has a before and after score
  df <- pivot_wider(df, 
                    names_from = Timing, 
                    values_from = Score, 
                    id_cols = c(ID, Mission, Role))
  # Rename training cols for clarity
  colnames(df)[colnames(df) == "Before"] <- "Training_Before"
  colnames(df)[colnames(df) == "After"] <- "Training_After"
  
  return(df)
}

# ------------------------------------------------------------------------------
### CLEAN SURVEYS ###
cleanSurveyData <- function(df,outputPath) {
  
  # CLEAN #
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
  labelCols = c("ID","Mission","Role","Date","Day")
  # Get standardized participant ID based on role/week
  df$ID <- mapply(createParticipantID, df$Role, df$Mission)
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

  return(df)

}

# ------------------------------------------------------------------------------
### SCORE NASA TLX ###
scoreNASATLX <- function(df,outputPath) {
  labelCols = c("ID","Mission","Role","Timepoint","Day","Condition")
  # Fill in default slider values for NASA TLX
  # NASA TLX default: 50
  nasaDefault <- 50
  # Get NASA TLX] columns based on string
  nasaCols <- colnames(df)[grepl("NASA", colnames(df))]
  # Place default values
  df[ ,nasaCols] <- df %>%
    select(all_of(nasaCols)) %>%
    mutate(across(where(is.character), as.numeric)) %>%
    replace(is.na(.), nasaDefault)
  
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
  weightingCols <- colnames(dfTallies)[grepl("Tally", colnames(dfTallies))]
  df <- cbind(df,dfTallies) %>%
    select(all_of(c(labelCols,weightingCols,sliderCols)))
  
  # Use the weightings to sum the total workload, place total workload into col
  df <- df %>%
    mutate(totalWorkload = 
             ((MentalDemandTally * `NASA TLX Mental Demand`) + 
                (PhysicalDemandTally * `NASA TLX Physical Demand`) + 
                (TemporalDemandTally * `NASA TLX Temporal Demand`) + 
                (PerformanceTally * `NASA TLX Performance`) + 
                (EffortTally * `NASA TLX Effort`) + 
                (FrustrationTally * `NASA TLX Frustration`)) /15)
  
  # Rename to include NASA TLX in each tally
  df <- df %>%
    rename(
      "NASA TLX Mental Demand Tally" = "MentalDemandTally",
      "NASA TLX Physical Demand Tally" = "PhysicalDemandTally",
      "NASA TLX Temporal Demand Tally" = "TemporalDemandTally",
      "NASA TLX Performance Tally" = "PerformanceTally",
      "NASA TLX Effort Tally" = "EffortTally",
      "NASA TLX Frustration Tally" = "FrustrationTally",
      "NASA TLX Total Workload" = "totalWorkload"
    )
  
  # OUTPUT #
  write_xlsx(df, outputPath) # Save plain TLX data
  return(df)
}
  
# ------------------------------------------------------------------------------
### SCORE SUS ###
scoreSUS <- function(df, outputPath) {
  # Fill in default slider values for SUS
  # SUS default: 3
  susDefault <- 3
  # Get SUS columns based on string
  susCols <-  colnames(df)[grepl("SUS", colnames(df))]
  # Place default values
  df[ ,susCols] <- df %>%
    select(all_of(susCols)) %>%
    mutate(across(where(is.character), as.numeric)) %>%
    replace(is.na(.), susDefault)
  # SUS Scoring
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
    mutate(susScore = rowSums(across(susCols)) * 2.5)
  
  # Reorder columns so labels are first, and only grab final SUS score
  labelCols = c("ID","Mission","Role","Timepoint","Day","Condition")
  df <- df %>%
    select(all_of(labelCols),susScore)
  
  # OUTPUT #
  write_xlsx(df, outputPath) # Save plain TLX data
  return(df)
}
  
# ------------------------------------------------------------------------------
### CLEAN ACQUISITION TIMES ###
cleanAcqTimes <- function(df,outputPath) {
  labelCols = c("ID","Mission","Role","Timepoint","Day","Condition")
  # Multiply minutes by seconds, add seconds, make new column
  df <- df %>%
    mutate(acqTime = timeToAcquireMin * 60 + timeToAcquireSec) %>%
    select(all_of(labelCols),acqTime)
  
  # OUTPUT #
  write_xlsx(df, outputPath) # Save plain acqTime data
  return(df)
}

# ------------------------------------------------------------------------------
### CLEAN DEMOGRAPHICS ###
cleanDemographics <- function(df,outputPath) {
  # Run through Qualtrics cleaning
  df <- cleanQualtrics(df)
  # Rename key demographic columns
  df <- df %>% 
    rename_with(~"Crew", contains("Crew Number"))
  df <- df %>%
    rename(
      "Role" = "Mission Role:",
      "DemoG_age" = "Q1",
      "DemoG_sex" = "Q2",
      "DemoG_gender" = "Q3",
      "DemoG_race_ethnicity" = "Q4"
    )
  # Get mission week based on first number mission question col
  df$Mission <- str_extract(df$Crew, "\\d+")
  # CCB for week 3 incorrectly called themselves CCA - quick fix
  df <- df %>%
    mutate(Role = if_else(Role == "Co-Commander A" & Mission == 3 & DemoG_sex == "Male","Co-Commander B", Role))
  # Get standardized participant ID based on role/week
  df$ID <- mapply(createParticipantID, df$Role, df$Mission)
  df <- df %>%
    select(where(~ !all(is.na(.)))) %>%
    select(-c("Duration (in seconds)","RecordedDate","Crew")) %>%
    select(all_of(c("ID","Mission","Role")),everything())
  # Rename astronaut requirement columns
  # Schooling
  df <- df %>%
    rename(
      "DemoG_profession" = "Q5",
      "DemoG_years_work_experience" = "Q6",
      "DemoG_education_completed" = "Q7",
      "DemoG_bachelors_degree" = "Q7_3_TEXT",
      "DemoG_masters_degree" = "Q7_4_TEXT",
      "DemoG_phd_equiv_degree" = "Q7_5_TEXT",
      "DemoG_additional_degree" = "Q7_8_TEXT",
      "DemoG_multiple_degrees" = "Q8",
      "DemoG_multiple_degrees_description" = "Q8_1_TEXT",
      "DemoG_current_degree" = "Q9",
      "DemoG_current_degrees_description" = "Q9_1_TEXT"
    )
  # Piloting
  df <- df %>%
    rename(
      "DemoG_test_pilot_enrollment" = "Q10",
      "DemoG_test_pilot_school_complete" = "Q11",
      "DemoG_pilot_in_command_time" = "Q13",
      "DemoG_hours_pilot_time" = "Q14"
    )
  # Medical experience
  df <- df %>%
    rename(
      "DemoG_human_anatomy_course" = "Q15",
      "DemoG_anatomy_course_level" = "Q16",
      "DemoG_licensed_physician" = "Q17",
      "DemoG_physician_completed" = "Q18",
      "DemoG_physician_specialty_chosen" = "Q19",
      "DemoG_physician_specialty_description" = "Q19_2_TEXT",
      "DemoG_residency_completed" = "Q20",
      "DemoG_fellowship_completed" = "Q21",
      "DemoG_fellowship_description" = "Q21_2_TEXT",
      "DemoG_years_as_attending" = "Q22",
      "DemoG_additional_medical_licenses" = "Q23",
      "DemoG_additional_medical_licenses_other" = "Q23_7_TEXT",
      "DemoG_additional_medical_training" = "Q24"
    )
  # Ultrasound experience
  df <- df %>%
    rename(
      "DemoG_ultrasound_on_yourself" = "Q25",
      "DemoG_trained_completing_ultrasounds" = "Q26",
      "DemoG_ultrasound_training_description" = "Q27",
      "DemoG_training_by_physician" = "Q28",
      "DemoG_training_by_physician_other" = "Q28_5_TEXT",
      "DemoG_performed_ultrasound_in_past" = "Q29",
      "DemoG_number_ultrasounds_performed" = "Q29_2_TEXT",
      "DemoG_performed_cardiac_ultrasound_in_past" = "Q30",
      "DemoG_percentage_cardiac_ultrasounds" = "Q30_2_TEXT",
      "DemoG_cardiac_ultrasound_description" = "Q31",
      "DemoG_trained_others_in_medicine" = "Q32",
      "DemoG_medical_teaching_hours" = "Q32_2_TEXT"
    )
  
  # OUTPUT #
  write_xlsx(df, outputPath) # Save plain demographics data
  return(df)
}

# ------------------------------------------------------------------------------
### CLEAN IMAGE QUALITY ###
cleanImageQuality <- function(df,outputPath) {
  # Separate filename into usable labels
  df <- dfImageQuality %>%
    separate(original_filename, into = c("Role", "Mission","Day","Condition"), sep = "_")
  # Get mission week based on first number mission question col
  df$Mission <- str_extract(df$Mission, "\\d+")
  
  # Standardize Role
  df <- df %>%
    mutate(Role = ifelse(Role == "CCA", "Co-Commander A", Role)) %>%
    mutate(Role = ifelse(Role == "CCB", "Co-Commander B", Role)) %>%
    mutate(Role = ifelse(Role == "Medic", "Crew Medic", Role)) %>%
    mutate(Role = ifelse(Role == "Eng", "Crew Engineer", Role)) %>%
    mutate(Role = ifelse(Role == "Sci", "Crew Scientist", Role)) %>%
    mutate(Role = ifelse(Role == "GH", "GreenHab Officer", Role))
  # Create ID for each individual video (each video graded 4 times, 2x per grader)
  df <- df %>%
    group_by(Mission,Role,Day,Condition) %>%
    mutate(video_ID = cur_group_id()) %>%
    ungroup()
  # Standardize Condition
  df <- df %>%
    mutate(Condition = sub("(Solo|Tele).*$", "\\1", Condition)) %>%
    mutate(Condition = ifelse(Condition == "Solo", "Unassisted", Condition)) %>%
    mutate(Condition = ifelse(Condition != "Unassisted", "Teleguided", Condition)) %>%
    mutate(Condition = factor(Condition, levels = c("Unassisted", "Teleguided")))
  # Get standardized participant ID based on role/week
  df$ID <- mapply(createParticipantID, df$Role, df$Mission)
  df <- df %>%
    select(-c("deidentified_filename","review_type")) %>%
    select(all_of(c("ID","Mission","Role","Condition","Day","video_ID")),everything())
  
  # Rename columns
  df <- df %>% 
    # ACEP scale
    rename_with(~ "ACEP_Score", .cols = contains("ACEP")) %>% 
    # other scale
    rename_with(~ "Cardiac_Scale", .cols = contains("criteria")) %>% 
    # Landmark Quality scale
    rename_with(~ "LQ_Orientation", .cols = contains("orientation")) %>%
    rename_with(~ "LQ_Depth", .cols = contains("depth")) %>% 
    rename_with(~ "LQ_Gain", .cols = contains("gain")) %>% 
    rename_with(~ "LQ_Cardiac_Movement", .cols = contains("cardiac movement")) %>% 
    rename_with(~ "LQ_LV", .cols = contains("left ventricle")) %>% 
    rename_with(~ "LQ_RV", .cols = contains("right ventricle")) %>% 
    rename_with(~ "LQ_LA", .cols = contains("left atrium")) %>% 
    rename_with(~ "LQ_RA", .cols = contains("right atrium")) %>% 
    rename_with(~ "LQ_Mitral", .cols = contains("mitral")) %>% 
    rename_with(~ "LQ_Tricuspid", .cols = contains("tricuspid")) %>% 
    rename_with(~ "LQ_Foreshortened", .cols = contains("foreshort")) %>% 
    rename_with(~ "LQ_Vertical_IV_Septum", .cols = contains("interventricular")) %>% 
    rename_with(~ "LQ_Vertical_IA_Septum", .cols = contains("interatrial")) %>% 
    rename_with(~ "LQ_Aortic_Valve", .cols = contains("aortic valve")) %>% 
    # Diagnostic Utility
    rename_with(~ "DU_VGE", .cols = contains("venous gas emboli")) %>%
    rename_with(~ "DU_Cardiac_Scale", .cols = contains("Acute Coronary Syndrome")) %>% 
    rename_with(~ "DU_Atrial_Fibrillation_Flutter", .cols = contains("Atrial Fibrillation")) %>% 
    rename_with(~ "DU_Severe_Hypovolemia", .cols = contains("hypovolemia")) %>% 
    rename_with(~ "DU_Respiratory_Failure", .cols = contains("respiratory failure")) %>% 
    rename_with(~ "DU_Sepsis_Cardiomyopathy", .cols = contains("sepsis")) %>% 
    rename_with(~ "DU_Cardiogenic_Shock", .cols = contains("cardiogenic")) %>%
    rename_with(~ "DU_Cardiac_Arrest", .cols = contains("cardiac arrest")) %>%
    rename_with(~ "DU_Chest_Blunt_Force_Trauma", .cols = contains("blunt force")) %>% 
    rename_with(~ "DU_Hypovolemic_Shock", .cols = contains("hypovolemic shock")) %>% 
    rename_with(~ "DU_Venous_Thromboembolism", .cols = contains("thromboembolism"))
    
  # Use only first number for ACEP and secondary cardiac quality scale
  df$ACEP_Score <- str_extract(df$ACEP_Score, "\\d+")
  df$Cardiac_Scale <- str_extract(df$Cardiac_Scale, "\\d+")
  
  # OUTPUT #
  write_xlsx(df, outputPath) # Save plain image quality data
  return(df)
}


