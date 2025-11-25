# AUTHOR:       Victoria Hurd
# DATE CREATED: 11/25/25
# LAST EDITED:  11/25/25
# PROJECT:      MDRS Teleguidance Study
# TASK:         Running stats, generating plots for training efficacy
# OUTPUTS:      Stats printouts and plots

# ------------------------------------------------------------------------------
### USER INPUTS ###

# Path to Git Repo
gitPath='/Users/vickihurd/GitHub/MDRS-Cardiac-Ultrasound/'
# Path to stored data
dataPath = '/Users/vickihurd/Library/CloudStorage/OneDrive-SharedLibraries-UCB-O365/Ultrasound - Documents/Aim 2 - MDRS Cardiac Teleguidance Study/Results/Clean Data/'
# Name of cleaned knowledge assessment datafile from GradeKnowledgeAssessments.R
dataFile = 'KnowledgeAssessmentData.xlsx'

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
# Sets repo path
setwd(gitPath)

# ------------------------------------------------------------------------------
### READ ###

# Read in cleaned datafiles
df <- read_excel(paste(dataPath,dataFile,sep = ""))

# ------------------------------------------------------------------------------
### DATA SETUP ###

# Enforce datatypes
df$Timing <- as.factor(df$Timing)
df$ID <- as.factor(df$ID)
df$Mission <- as.factor(df$Mission)
df$Role <- as.factor(df$Role)

# Pivot wider so that each particpant ID only has one row with before/after score
df_wide <- df %>%
  select(ID,Mission,Role,Timing,Score) %>%
  pivot_wider(names_from = Timing, values_from = Score)

# ------------------------------------------------------------------------------
### TABULATIONS ###

# Tabulate - look at before vs after scores
means <- df_wide %>%
  summarise(numSamples = length(After),
    Before = mean(Before, na.rm = TRUE),
    After = mean(After, na.rm = TRUE)) %>%
  pivot_longer(
    cols = c(Before, After),
    names_to = "Timing",
    values_to = "Score")

# Tabulate - look at before vs after scores for non-medics
meansNonmedics <- df_wide %>%
  filter(Role != "Crew Medic") %>%
  summarise(numSamples = length(After),
            Before = mean(Before, na.rm = TRUE),
            After = mean(After, na.rm = TRUE)) %>%
  pivot_longer(
    cols = c(Before, After),
    names_to = "Timing",
    values_to = "Score")


# Tabulate - look at before vs after scores for medics
meansMedics <- df_wide %>%
  filter(Role == "Crew Medic") %>%
  summarise(numSamples = length(After),
            Before = mean(Before, na.rm = TRUE),
            After = mean(After, na.rm = TRUE)) %>%
  pivot_longer(
    cols = c(Before, After),
    names_to = "Timing",
    values_to = "Score")

# Print tabulation Results
print(means)
print(meansNonmedics)
print(meansMedics)

# Tabulations per Question
# test <- df %>%
#   select(Timing, Role, Q20_1,Q20_2,Q20_3,Q20_4,Q20_5,Q20_6) %>%
#   mutate(Role = ifelse(Role == "Crew Medic", "Medic", Role)) %>%
#   mutate(Role = ifelse(Role != "Medic", "Non-Medic", Role)) %>%
#   summarise(Non-MedicBefore = sum(Before, na.rm = TRUE),
#             MedicBefore = sum(Before, na.rm = TRUE),
#             Non-MedicAfter = sum(After, na.rm = TRUE),
#             MedicAfter = sum(After, na.rm = TRUE)) %>%
#   pivot_longer(
#     cols = c(Before, After),
#     names_to = "Timing",
#     values_to = "Score")

# Labelling Question
#labellingCols <- names(subset(df, select = c(Q20_1,Q20_2,Q20_3,Q20_4,Q20_5,Q20_6)))

# ------------------------------------------------------------------------------
### ASSUMPTION CHECKS ###

# QQPlot - normality
df %>%
  filter(Role != "Crew Medic") %>%
  ggqqplot(x="Score", title = "Training Data QQPlot: Non-Medics")

df %>%
  ggqqplot(x="Score",title = "Training Data QQPlot: All Data")

# Shapiro-Wilk for Normality
# Note that p-value > 0.05 implies that the distribution of training data isn't  
# significantly different from a theoretical normal distribution
# Non-Medics only
df %>%
  filter(Role != "Crew Medic") %>%
  shapiro_test(Score)
# Overall
df %>%
  shapiro_test(Score)

# ------------------------------------------------------------------------------
### RUN STATS ###

# Null hypothesis: 
# Nonmedic before scores and after scores are the same (training module has no effect on "after" scores)
# Alternative hypothesis: 
# Nonmedic scores after training module are higher than those taken before (training module improves score)
  
# Assumptions: 
#   1) Normally-distributed data (checked visually & via Shapiro Wilk)
#   2)  (checked via)

df %>%
  filter(Role != "Crew Medic") %>%
  t.test(Score ~ Timing, data = ., alternative = 'greater', paired = TRUE)

df %>%
  t.test(Score ~ Timing, data = ., alternative = 'greater', paired = TRUE)

# If not normal, use paired two-samples Wilcoxon Test

# ------------------------------------------------------------------------------
### CREATE PLOTS ###

# Create density plots of final score based on role
df %>%
  ggplot(aes(x=Score, color=Timing)) + 
  geom_density() +
  geom_vline(data=means,aes(xintercept=Score,color=Timing),linetype="dashed") + 
  scale_x_continuous(breaks = seq(0,26,2), limits = c(0,26)) +
  labs(x="Total Score (out of 26)",
       title="Assessment Score Density: All Participants")

df %>%
  filter(Role != "Crew Medic") %>%
  ggplot(aes(x=Score, color=Timing)) + 
  geom_density() +
  geom_vline(data=meansNonmedics,aes(xintercept=Score,color=Timing),linetype="dashed") + 
  scale_x_continuous(breaks = seq(0,26,2), limits = c(0,26)) +
  labs(x="Total Score (out of 26)",
       title="Assessment Score Density: Non-Medics")

df %>%
  filter(Role == "Crew Medic") %>%
  ggplot(aes(x=Score, color=Timing)) + 
  geom_density() +
  geom_vline(data=meansMedics,aes(xintercept=Score,color=Timing),linetype="dashed") + 
  scale_x_continuous(breaks = seq(0,26,2), limits = c(0,26)) +
  labs(x="Total Score (out of 26)",
       title="Assessment Score Density: Medics")
  
# Boxplots showing medic vs non-medic performance before/after training module
df %>%
  mutate(Role = ifelse(Role == "Crew Medic", "Medic", Role)) %>%
  mutate(Role = ifelse(Role != "Medic", "Non-Medic", Role)) %>%
  mutate(Timing = factor(Timing, levels = c("Before", "After"))) %>%
  mutate(Role = factor(Role, levels = c("Non-Medic", "Medic"))) %>%
  ggplot(aes(x=Timing, y=Score, fill=Role)) + 
  geom_boxplot() +
  scale_x_discrete(labels = c("Before Training","After Training")) +
  scale_y_continuous(breaks = seq(0,26,2), limits = c(0,26)) +
  labs(x = "Testing Condition",
       y="Total Score (out of 26)",
       title="Assessment Scores Before vs. After Training Module")

# Boxplots of all questions 
#names(subset(df, select = -c(ID,Date,Mission,Role,Timing,Duration,Score)))
names(subset(df, select = c(ID,Date,Mission,Role,Timing,Duration,Score)))

# Boxplots of label question (6 parts)
labelCols <- names(subset(df, select = c(Q20_1,Q20_2,Q20_3,Q20_4,Q20_5,Q20_6)))
