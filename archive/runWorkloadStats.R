# AUTHOR:       Victoria Hurd
# DATE CREATED: 11/25/25
# LAST EDITED:  12/05/25
# PROJECT:      MDRS Teleguidance Study
# TASK:         Running stats, generating plots for training efficacy
# OUTPUTS:      Stats printouts and plots

# ------------------------------------------------------------------------------
### USER INPUTS ###

# Path to Git Repo
gitPath='/Users/vickihurd/GitHub/MDRS-Cardiac-Ultrasound/'
# Path to stored data
dataPath = '/Users/vickihurd/Library/CloudStorage/OneDrive-SharedLibraries-UCB-O365/Ultrasound - Documents/Aim 2 - MDRS Cardiac Teleguidance Study/Results/Clean Data/'
# Name of cleaned knowledge assessment datafile from scoreSurveyData.R
dataFile = 'surveyData.xlsx'

# ------------------------------------------------------------------------------
### ADMIN ###

# Read in all pertinent libraries
library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(outliers)
library(car)
library(afex)
library(lme4)
library(emmeans)
# Sets repo path
setwd(gitPath)
# Source supporting functions
files.sources = list.files(paste(gitPath,'lib/', sep = ""))
files.sources = paste(paste(gitPath,'lib/', sep = ""), files.sources, sep = "")
sapply(files.sources, source)

# ------------------------------------------------------------------------------
### READ ###

# Read in cleaned datafiles
df <- read_excel(paste(dataPath,dataFile,sep = ""))

# ------------------------------------------------------------------------------
### DATA SETUP ###

# Enforce datatypes so that labels are type factor
df <- df %>%
  mutate(Role = ifelse(Role == "Crew Medic", "Medic", Role)) %>%
  mutate(Role = ifelse(Role != "Medic", "Non-Medic", Role)) %>%
  mutate(
    ID = as.factor(ID),
    Mission = as.factor(Mission),
    Role = as.factor(Role),
    Timepoint = as.factor(Timepoint),
    Condition =factor(Condition,levels = c("Unassisted","Teleguided")))
    
str(df)

# Define categories of column names
labelCols <- c("ID","Mission","Role","Date","Timepoint","Condition")
dataCols <- colnames(df)[!colnames(df) %in% labelCols]
tlxCols <-  dataCols[dataCols != 'susScore']
weightingCols <- colnames(df)[grepl("Tally", colnames(df))]
sliderCols <- colnames(df)[grepl("NASA", colnames(df))]

# ------------------------------------------------------------------------------
### TABULATIONS & DESCRIPTIVE STATISTICS ###
means <- apply(df[, dataCols], 2, mean, na.rm = TRUE)
sds <- apply(df[, dataCols], 2, sd, na.rm = TRUE)
modes <- apply(df[, weightingCols], 2, getMode)

groupedMeans <- df %>%
  group_by(Role,Condition) %>%
  summarise_at(vars(totalWorkload), list(mean = mean))
groupedSDs <- df %>%
  group_by(Role,Condition) %>%
  summarise_at(vars(totalWorkload), list(sd = sd))
timeGroupedMeans <- df %>%
  group_by(Role,Condition,Timepoint) %>%
  summarise_at(vars(totalWorkload), list(mean = mean))
timeGroupedroupedSDs <- df %>%
  group_by(Role,Condition,Timepoint) %>%
  summarise_at(vars(totalWorkload), list(sd = sd))

df %>%
  group_by(Role,Timepoint) %>%
  summarise_at(vars(totalWorkload), list(mean = mean))
df %>%
  group_by(Role,Timepoint) %>%
  summarise_at(vars(totalWorkload), list(sd = sd))
df %>%
  group_by(Timepoint) %>%
  summarise_at(vars(totalWorkload), list(mean = mean))
df %>%
  group_by(Timepoint) %>%
  summarise_at(vars(totalWorkload), list(sd = sd))

means
sds
groupedMeans
groupedSDs
timeGroupedMeans
timeGroupedroupedSDs

# ------------------------------------------------------------------------------
### VISUALIZATIONS ###

# Typical workload weights/ratings plot using means

# ------------------------------------------------------------------------------
### ASSUMPTION CHECKS ###

# Assumptions: 
#   1) Independent samples
#   2) Data normality within each group
#   3) Equal variance between groups
#   4) No outliers
#   5) Continuous data

# QQPlot for Normality
# Normally-distributed data will appear within gray band if normally distributed
df %>%
  filter(Role != "Medic") %>%
  filter(Condition == "Unassisted") %>%
  ggqqplot(x="totalWorkload", title = "Unassisted Non-Medic Total Workload: QQPlot")

df %>%
  filter(Role != "Medic") %>%
  filter(Condition == "Teleguided") %>%
  ggqqplot(x="totalWorkload", title = "Teleguided Non-Medic Total Workload: QQPlot")

df %>%
  filter(Role == "Medic") %>%
  ggqqplot(x="totalWorkload", title = "Medic Total Workload: QQPlot")

df %>%
  filter(Role == "Non-Medic") %>%
  ggqqplot(x="totalWorkload", title = "Non-Medic Total Workload: QQPlot")

# Shapiro-Wilk for Normality
# Note that p-value > 0.05 implies that the distribution of training data isn't  
# significantly different from a theoretical normal distribution
df %>%
  filter(Role != "Medic") %>%
  filter(Condition == "Unassisted") %>%
  shapiro_test(totalWorkload)

df %>%
  filter(Role != "Medic") %>%
  filter(Condition == "Teleguided") %>%
  shapiro_test(totalWorkload)

df %>%
  filter(Role == "Medic") %>%
  shapiro_test(totalWorkload)

df %>%
  filter(Role == "Non-Medic") %>%
  shapiro_test(totalWorkload)

# Levene's Test for Equal Variances
df %>%
  filter(Role != "Medic") %>%
  leveneTest(totalWorkload ~ Condition, data = .)
df %>%
  leveneTest(totalWorkload ~ Role, data = .)

# Grubb's Test for Outliers
# Tests for outliers beyond a theoretical normal distribution
# Note that p-value > 0.05 implies that the distribution of training data isn't  
# significantly different from a theoretical normal distribution
# "Type = 11" specifies Grubbs test for two opposite outliers
grubbs.test(df$totalWorkload[df$Role != "Medic"], 
            type = 11)
grubbs.test(df$totalWorkload[df$Role == "Medic"], 
            type = 11)


# ------------------------------------------------------------------------------
### RUN STATS ###

# HYPOTHESIS 1
# Null hypothesis: 
# Teleguided/unassisted WL scores are the same (condition has no effect on WL)
# Alternative hypothesis: 
# Teleguided WL scores are different from unassisted scores
df %>%
  filter(Role != "Medic") %>%
  #filter(Mission != "2") %>%
  t.test(totalWorkload ~ Condition, data = .,var.equal = TRUE)
df %>%
  filter(Role != "Medic") %>%
  #filter(Mission != "2") %>%
  t.test(`NASA TLX Performance` ~ Condition, data = .,var.equal = TRUE)


# HYPOTHESIS 2
# Null hypothesis: 
# Medic/NonMedic WL scores are the same (Role has no effect on WL)
# Alternative hypothesis: 
# Medic WL scores are different from Non-Medic scores
t.test(totalWorkload ~ Role, data = df, var.equal = FALSE)

# HYPOTHESIS 3
# Null hypothesis: 
# Regression coefficients for the specified fixed effects = zero 
# (Condition and Timepoint coeffs are = 0)
# Alternative hypothesis: 
# Regression coefficient for Condition and/or Timepoint != zero
df$roleCondition <- interaction(df$Role, df$Condition, sep = "_")
modelRoleConditionTime <- lmer(totalWorkload ~ roleCondition * Timepoint + (1 | ID), 
                           data = df[df$Role != "Medic", ])

emmRoleCondition <- emmeans(modelRoleConditionTime, c("roleCondition"))
emmTime <- emmeans(modelRoleConditionTime, c("Timepoint"))
emmRoleConditionTime <- emmeans(modelRoleConditionTime, c("Timepoint","roleCondition"))

summary(modelRoleConditionTime)
emmRoleCondition
pairs(emmRoleCondition)
contrast(emmRoleCondition)
pairs(emmTime)
contrast(emmTime)
pairs(emmRoleConditionTime)
contrast(emmRoleConditionTime)



plot(modelRoleConditionTime)
qqnorm(residuals(modelRoleConditionTime))
qqnorm(data.frame(ranef(modelRoleConditionTime))$condval)

res.std <- rstandard(residuals(modelRoleConditionTime))
plot(residuals(modelRoleConditionTime), ylab="Standardized Residuals")
ggplot(as.data.frame(residuals(modelRoleConditionTime)), aes(sample = residuals(modelRoleConditionTime))) +
  geom_qq() +
  geom_qq_line()

# ------------------------------------------------------------------------------
### FINAL PLOTS WITH STATISTICAL RESULTS ###
