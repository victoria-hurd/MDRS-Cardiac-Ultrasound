# AUTHOR:       Victoria Hurd
# DATE CREATED: 4/16/26
# LAST EDITED:  4/23/26
# PROJECT:      MDRS Ultrasound Task Data Analysis
# TASK:         Create master dataframe will all collected data
# OUTPUTS:      Saves single dataframe with all data

library(stringr)

# ------------------------------------------------------------------------------
### GET ACEP SCORE (MEDIAN) ###
getACEPMedian <- function(df) {
  # Group by video ID, take median of scores, save to ACEP_Median
  df <- df %>%
    group_by(video_ID) %>%
    mutate(ACEP_Median = median(as.numeric(ACEP_Score), na.rm = TRUE)) %>%
    select(-ACEP_Score)
}

# ------------------------------------------------------------------------------
### GET CARDIAC SCALE SCORE (MEDIAN) ###

getKimuraMedian <- function(df) {
  # Group by video ID, take median of scores, save to Kimura_Median
  df <- df %>%
    group_by(video_ID) %>%
    mutate(Kimura_Median = median(as.numeric(Cardiac_Scale), na.rm = TRUE)) %>%
    select(-Cardiac_Scale)
}
# ------------------------------------------------------------------------------
## GRADE LANDMARK QUALITY ###
gradeLandmarkQuality <- function(df) {
  # Get relevant columns
  # Note that for aortic valve and foreshortening, a Yes means a 0 score
  cols <- colnames(df)[startsWith(colnames(df), "LQ_")]
  negativeScoreCols <-c("LQ_Foreshortened", "LQ_Aortic_Valve")
  cols <- cols[!cols %in% negativeScoreCols]
  # For relevant columns, give 0 or 1 
  df <- df %>%
    mutate(across(all_of(cols), ~ as.numeric(str_detect(., regex("yes", ignore_case = TRUE)))))
  df <- df %>%
    mutate(across(all_of(negativeScoreCols), ~ as.numeric(str_detect(., regex("no", ignore_case = TRUE)))))
  return(df)
}
# ------------------------------------------------------------------------------
### SCORE LANDMARK QUALITY (SUM) ###

scoreLandmarkQuality <- function(df) {
  # Get relevant columns
  # Note that for aortic valve and foreshortening, a Yes means a 0 score
  cols <- colnames(df)[startsWith(colnames(df), "LQ_")]
  negativeScoreCols <-c("LQ_Foreshortened", "LQ_Aortic_Valve")
  cols <- cols[!cols %in% negativeScoreCols]
  # For relevant columns, give 0 or 1 
  df <- df %>%
    mutate(across(all_of(cols), ~ as.numeric(str_detect(., regex("yes", ignore_case = TRUE)))))
  df <- df %>%
    mutate(across(all_of(negativeScoreCols), ~ as.numeric(str_detect(., regex("no", ignore_case = TRUE)))))
  # Score across all, then remove cols
  df <- df %>%
    mutate(LQ_Score = rowSums(across(all_of(c(cols,negativeScoreCols))))) %>%
    select(-all_of(c(cols,negativeScoreCols)))
  
  return(df)
}


# ------------------------------------------------------------------------------
### GET LANDMARK QUALITY SCORE (MEAN OF SUMS) ###

getLandmarkQualityMean <- function(df) {
  # Group by video ID, take mean of scores, save to LQ_Mean
  df <- df %>%
    group_by(video_ID) %>%
    mutate(LQ_Mean = mean(as.numeric(LQ_Score), na.rm = TRUE)) %>%
    select(-LQ_Score)
}

# ------------------------------------------------------------------------------
### SCORE DIAGNOSTIC UTILITY (SUM) ###

scoreDiagnosticUtility <- function(df) {
  # Get relevant columns
  cols <- colnames(df)[startsWith(colnames(df), "DU_")]
  # For relevant columns, give 0 or 1 
  df <- df %>%
    mutate(across(all_of(cols), ~ as.numeric(str_detect(., regex("yes", ignore_case = TRUE)))))
  # Score across all, then remove cols
  df <- df %>%
    mutate(DU_Score = rowSums(across(all_of(cols)))) %>%
    select(-all_of(cols))
  
  return(df)
}


# ------------------------------------------------------------------------------
### GET DIAGNOSTIC UTILITY SCORE (MEAN OF SUMS) ###

getDiagnosticUtilityMean <- function(df) {
  # Group by video ID, take mean of scores, save to LQ_Mean
  df <- df %>%
    group_by(video_ID) %>%
    mutate(DU_Mean = mean(as.numeric(DU_Score), na.rm = TRUE)) %>%
    select(-DU_Score)
}

# ------------------------------------------------------------------------------
### REMOVE EMBEDDED GRADE DATA ###
removeEmbedded <- function(df) {
  df <- df %>%
    select(-all_of(c("replay_enabled","autoplay_enabled","review_duration_sec",
                     "total_watch_time_sec","pause_count","play_count",
                     "scrub_count","times_replayed")))
}

# ------------------------------------------------------------------------------
### COMBINE ACROSS GRADERS ###
combineAcrossGraders <- function(df,outputPath) {
  # Grade 
  df <- getACEPMedian(df)
  df <- getKimuraMedian(df)
  df <- scoreLandmarkQuality(df)
  df <- scoreDiagnosticUtility(df)
  df <- getLandmarkQualityMean(df)
  df <- getDiagnosticUtilityMean(df)
  df <- removeEmbedded(df)
  # Combine - keep first occurrence since all the same. Remove grader data
  df <- df %>%
    group_by(video_ID) %>%
    slice(1) %>%
    ungroup() %>%
    select(-all_of(c("video_ID","assigned_grader","repeat_num")))
  
  # Output
  write_xlsx(df, outputPath)
  return(df)
  
}
