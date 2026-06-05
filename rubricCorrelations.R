# AUTHOR:       Victoria Hurd
# DATE CREATED: 5/6/26
# LAST EDITED:  5/6/26
# PROJECT:      MDRS Ultrasound Task Data Analysis
# TASK:         Run rubric stats

# ------------------------------------------------------------------------------
### USER INPUTS ###

# Path to Git Repo
homePath='/Users/vickihurd/GitHub/MDRS-Cardiac-Ultrasound/'
# Path to stored data
dataFolder = '/Users/vickihurd/Library/CloudStorage/OneDrive-SharedLibraries-UCB-O365/Ultrasound - Documents/Aim 2 - MDRS Cardiac Teleguidance Study/Data Analysis/Data/Clean Data/'
dataFile = 'MasterDataframe.xlsx'
dataFile2 = 'ImageQualityData.xlsx'

# ------------------------------------------------------------------------------
### ADMIN ###
# Set working dir
setwd(homePath)
# Read in all pertinent libraries
library(readxl)
library(dplyr)
library(tidyr)
library(lavaan)
# Load custom functions into memory
source("statsSupportFuncs.R")
source("imageQualityGradingFuncs.R")

# ------------------------------------------------------------------------------
### DATA READ ###
df <- read_excel(paste(dataFolder,dataFile,sep = ""))
df2 <- read_excel(paste(dataFolder,dataFile2,sep = ""))

# ------------------------------------------------------------------------------
### DATA CLEAN ###
# Mutate nonmedics role
df <- ensureVarTypes(df)
df <- df %>%
  filter(Role != "Crew Medic") %>% 
  rename(totalWL = `NASA TLX Total Workload`)

# add weighted tlx
df$W_MentalDemand <- df$`NASA TLX Mental Demand Tally`*df$`NASA TLX Mental Demand`
df$W_PhysicalDemand <- df$`NASA TLX Physical Demand Tally`*df$`NASA TLX Physical Demand`
df$W_TemporalDemand <- df$`NASA TLX Temporal Demand Tally`*df$`NASA TLX Temporal Demand`
df$W_Effort <- df$`NASA TLX Effort Tally`*df$`NASA TLX Effort`
df$W_Performance <- df$`NASA TLX Performance Tally`*df$`NASA TLX Performance`
df$W_Frustration <- df$`NASA TLX Frustration Tally`*df$`NASA TLX Frustration`

# ------------------------------------------------------------------------------
### CFA ###

# Define the model
# =~ operator defines the latent variable (left) by observed variables (right)
my_model <- '
  ACEP  =~ ACEP_Median
  Kimura =~ Kimura_Median
  LQ =~ LQ_Mean
  DU =~ DU_Mean
'
# 2. Fit the model using HolzingerSwineford1939 data
fit <- cfa(my_model, data = df)

# 3. View the results
summary(fit, fit.measures = TRUE, standardized = TRUE)
varTable(fit)

# ------------------------------------------------------------------------------
### CFA ###
 
# Define the model
# =~ operator defines the latent variable (left) by observed variables (right)
my_model <- '
  ACEP  =~ ACEP_Score
  Kimura =~ Cardiac_Scale
  LQ =~ LQ_Gain+LQ_Cardiac_Movement+LQ_LV+LQ_RV+LQ_LA+LQ_RA+LQ_Mitral+LQ_Tricuspid+LQ_Foreshortened+LQ_Vertical_IV_Septum+LQ_Vertical_IA_Septum+LQ_Aortic_Valve               
  DU =~ DU_VGE+DU_Cardiac_Scale+DU_Atrial_Fibrillation_Flutter+DU_Severe_Hypovolemia+DU_Respiratory_Failure+DU_Sepsis_Cardiomyopathy+DU_Cardiogenic_Shock+DU_Cardiac_Arrest+DU_Chest_Blunt_Force_Trauma+DU_Hypovolemic_Shock+DU_Venous_Thromboembolism
'
# 2. Fit the model using HolzingerSwineford1939 data
fit <- cfa(my_model, data = df2)

# 3. View the results
summary(fit, fit.measures = TRUE, standardized = TRUE)
varTable(fit)



# ------------------------------------------------------------------------------
### CFA ###
LQ_cols <- grep("LQ", names(df2), value = TRUE)
DU_cols <- grep("DU", names(df2), value = TRUE)
df2 <- gradeLandmarkQuality(df2)
# center = TRUE (subtracts mean)
# scale. = TRUE (standardizes data; crucial if variables have different units)
pca_result <- prcomp(df2[, LQ_cols], center = TRUE, scale. = TRUE)
summary(pca_result) 
pca_result$rotation


## START OVER

fa(df[, c("ACEP_Median","Kimura_Median","LQ_Mean","DU_Mean")], nfactors=1)

a <- alpha(df2[, LQ_cols])

a$item.stats

library(psych)

fa(df2[, LQ_cols], nfactors = 1)

library(mirt)

model <- mirt(df2[, LQ_cols], 1, itemtype = "2PL")
summary(model)