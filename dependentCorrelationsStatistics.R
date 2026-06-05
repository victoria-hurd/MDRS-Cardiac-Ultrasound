# AUTHOR:       Victoria Hurd
# DATE CREATED: 5/6/26
# LAST EDITED:  5/6/26
# PROJECT:      MDRS Ultrasound Task Data Analysis
# TASK:         Run stats for Workload

# ------------------------------------------------------------------------------
### USER INPUTS ###

# Path to Git Repo
homePath='/Users/vickihurd/GitHub/MDRS-Cardiac-Ultrasound/'
# Path to stored data
dataFolder = '/Users/vickihurd/Library/CloudStorage/OneDrive-SharedLibraries-UCB-O365/Ultrasound - Documents/Aim 2 - MDRS Cardiac Teleguidance Study/Data Analysis/Data/Clean Data/'
dataFile = 'MasterDataframe.xlsx'

# ------------------------------------------------------------------------------
### ADMIN ###
# Set working dir
setwd(homePath)
# Read in all pertinent libraries
library(readxl)
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(emmeans)
# Load custom functions into memory
source("statsSupportFuncs.R")

# ------------------------------------------------------------------------------
### DATA READ ###
df <- read_excel(paste(dataFolder,dataFile,sep = ""))

# ------------------------------------------------------------------------------
### DATA CLEAN ###
# Pull out medics
df <- df %>%
  filter(Role != "Crew Medic")
# Ensure variable types
df <- ensureVarTypes(df)
# Add training increase
df$Training_Increase <- df$Training_After - df$Training_Before
# add weighted tlx
df$W_MentalDemand <- df$`NASA TLX Mental Demand Tally`*df$`NASA TLX Mental Demand`
df$W_PhysicalDemand <- df$`NASA TLX Physical Demand Tally`*df$`NASA TLX Physical Demand`
df$W_TemporalDemand <- df$`NASA TLX Temporal Demand Tally`*df$`NASA TLX Temporal Demand`
df$W_Effort <- df$`NASA TLX Effort Tally`*df$`NASA TLX Effort`
df$W_Performance <- df$`NASA TLX Performance Tally`*df$`NASA TLX Performance`
df$W_Frustration <- df$`NASA TLX Frustration Tally`*df$`NASA TLX Frustration`

# ------------------------------------------------------------------------------
### Image Quality ###

model <- manova(cbind(ACEP_Median,Kimura_Median,LQ_Mean,DU_Mean) ~ acqTime + `NASA TLX Total Workload` + susScore + W_MentalDemand +W_PhysicalDemand + W_TemporalDemand + W_Effort + W_Frustration + W_Performance, data = df)
summary.aov(model)

# ------------------------------------------------------------------------------
### Workload ###

model <- manova(cbind(`NASA TLX Total Workload`,W_MentalDemand,W_PhysicalDemand,W_TemporalDemand,W_Effort,W_Frustration,W_Performance) ~ acqTime + susScore, data = df)
summary.aov(model)

# ------------------------------------------------------------------------------
### Usability ###

model <- lm(susScore ~ acqTime + `NASA TLX Total Workload` + W_MentalDemand +W_PhysicalDemand + W_TemporalDemand + W_Effort + W_Frustration + W_Performance, data = df)
summary.aov(model)

# ------------------------------------------------------------------------------
### acqTime ###

model <- lm(acqTime ~ susScore + `NASA TLX Total Workload` + W_MentalDemand +W_PhysicalDemand + W_TemporalDemand + W_Effort + W_Frustration + W_Performance, data = df)
summary.aov(model)
