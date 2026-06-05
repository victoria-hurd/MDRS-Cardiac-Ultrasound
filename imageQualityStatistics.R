# AUTHOR:       Victoria Hurd
# DATE CREATED: 4/24/26
# LAST EDITED:  4/29/26
# PROJECT:      MDRS Ultrasound Task Data Analysis
# TASK:         Run LMMs for image quality

# ------------------------------------------------------------------------------
### USER INPUTS ###

# Path to Git Repo
homePath='/Users/vickihurd/GitHub/MDRS-Cardiac-Ultrasound/'
# Path to stored data
dataFolder = '/Users/vickihurd/Library/CloudStorage/OneDrive-SharedLibraries-UCB-O365/Ultrasound - Documents/Aim 2 - MDRS Cardiac Teleguidance Study/Data Analysis/Data/Clean Data/'
dataFile = 'MasterDataframe.xlsx'

# ------------------------------®------------------------------------------------
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
library(MASS)
library(ggplot2)
library(ggpubr)
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

# ------------------------------------------------------------------------------
### LMM for ACEP ###
# Run model
varStr <- "ACEP"
model <- lmer(ACEP_Median ~ Condition + Mission + Timepoint + 
                DemoG_age + DemoG_sex + 
                Training_Before + Training_After + Training_Increase +
                (1 | ID), data = df)
summary(model)
# Check assumptions
checkAssumptionsLMM(model,varStr)
# Show model results
anova(model)
# Get mean/SD for significant groupings
df %>%
  group_by(Condition) %>%
  get_summary_stats(ACEP_Median, type = "mean_sd")
pairs(emmeans(model, ~ Condition))
# Results: 
# All assumptions pass
# 1) ACEP Median score is significantly higher in teleguided condition (***)
pairs_ACEPvsCondition <- pValFormatPlotCondition(pairs(emmeans(model, ~ Condition), adjust = "tukey"))

# ------------------------------------------------------------------------------
### LMM for KIMURA ###

# Run model
varStr <- "Kimura"
model <- lmer(Kimura_Median ~ Condition * Mission + Timepoint + 
                DemoG_age + DemoG_sex + 
                Training_Before + Training_After + Training_Increase +
                (1 | ID), data = df)
isSingular(model) # Above model is singular/overfitted. 
# Removing factor that causes singularity but maintains most complex model (age)
model <- lmer(Kimura_Median ~ Condition * Mission + Timepoint + 
                DemoG_sex + 
                Training_Before + Training_After + Training_Increase +
                (1 | ID), data = df)
# Check if singular/overfitted
isSingular(model)
# Check assumptions
checkAssumptionsLMM(model,varStr)
# Show model results
anova(model)
# Get mean/SD for significant groupings
df %>%
  group_by(Condition) %>%
  get_summary_stats(Kimura_Median, type = "mean_sd")
df %>%
  group_by(Mission) %>%
  get_summary_stats(Kimura_Median, type = "mean_sd")
# Get pairwise comparisons
pairs(emmeans(model, ~ Condition), adjust = "tukey")

# Results: 
# All assumptions pass
# 1) Kimura Median score is significantly higher in teleguided condition (**)
# 2) Significant interaction between mission and condition (*)
# Get pval for plots
pairs_KimuravsCondition <- pValFormatPlotCondition(pairs(emmeans(model, ~ Condition), adjust = "tukey"))

# ------------------------------------------------------------------------------
### LMM for Landmark Quality ###

# Run model
varStr <- "Landmark Quality"
model <- lmer(LQ_Mean ~ Condition * Mission + Timepoint + 
                DemoG_age + DemoG_sex + 
                Training_Before + Training_After + Training_Increase +
                (1 | ID), data = df)
# Check assumptions
checkAssumptionsLMM(model,varStr)
# Show model results
anova(model)
# Check if singular/overfitted
isSingular(model)
# Get mean/SD for significant groupings
df %>%
  group_by(Condition) %>%
  get_summary_stats(LQ_Mean, type = "mean_sd")

# Results: 
# All assumptions pass
# 1) LQ Mean score is significantly higher in teleguided condition (***)
pairs_LQvsCondition <- pValFormatPlotCondition(pairs(emmeans(model, ~ Condition), adjust = "tukey"))

# ------------------------------------------------------------------------------
### LMM for Diagnostic Utility ###

# Run model
varStr <- "Diagnostic Utility"
model <- lmer(DU_Mean ~ Condition * Mission + Timepoint + 
                #DemoG_age + DemoG_sex + 
                Training_Before + Training_After + Training_Increase +
                (1 | ID), data = df)
# Check assumptions
checkAssumptionsLMM(model,varStr)

# Show model results
anova(model)
# Check if singular/overfitted
isSingular(model)
# Get mean/SD for significant groupings
df %>%
  group_by(Condition) %>%
  get_summary_stats(DU_Mean, type = "mean_sd")
df %>%
  group_by(Mission) %>%
  get_summary_stats(DU_Mean, type = "mean_sd")
# Pairwise
pairs(emmeans(model, ~ Mission), adjust = "tukey")

# Results: 
# Assumptions? Borderline
# 1) DU Mean score is significantly higher in teleguided condition (*)
# 2) Significant condition * mission interaction effect (*)
pairs_DUvsCondition <- pValFormatPlotCondition(pairs(emmeans(model, ~ Condition), adjust = "tukey"))

# ------------------------------------------------------------------------------
### PLOTS ###
# ACEP
df %>%
  filter(Role != "Crew Medic") %>%
  mutate(Condition = factor(Condition, levels = c("Unassisted", "Teleguided"))) %>%
  ggplot(aes(x=Condition, y=ACEP_Median)) + 
  geom_boxplot(aes(fill = Condition)) +
  scale_y_continuous(breaks = seq(1,5,1), limits = c(1,5.2)) +
  labs(x = "Testing Condition",
       y=expression("Median ACEP Score"),
       title="Image Quality vs \nTeleguidance Condition") + 
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),
        text = element_text(size = 18),
        axis.title.x = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "grey",
                                    fill = NA,
                                    linewidth = 0.5),
        panel.grid.major = element_line(color = "grey",
                                        linewidth = 0.25,
                                        linetype = 2),) +
  guides(fill = "none") +
  stat_pvalue_manual(pairs_ACEPvsCondition, label = "pformatted", y.position = 5.175,
                     size = 5)
  
# Kimura
df %>%
  filter(Role != "Crew Medic") %>%
  mutate(Condition = factor(Condition, levels = c("Unassisted", "Teleguided"))) %>%
  ggplot(aes(x=Condition, y=Kimura_Median)) + 
  geom_boxplot(aes(fill = Condition)) +
  scale_y_continuous(breaks = seq(0,4,1), limits = c(0,4.2)) +
  labs(x = "Testing Condition",
       y=expression("Median Kimura Score"),
       title="Image Quality vs \nTeleguidance Condition") + 
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),
        text = element_text(size = 18),
        axis.title.x = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "grey",
                                    fill = NA,
                                    linewidth = 0.5),
        panel.grid.major = element_line(color = "grey",
                                        linewidth = 0.25,
                                        linetype = 2),) +
  guides(fill = "none") +
  stat_pvalue_manual(pairs_KimuravsCondition, label = "pformatted", y.position = 4.175,
                     size = 5)

# LQ
df %>%
  filter(Role != "Crew Medic") %>%
  mutate(Condition = factor(Condition, levels = c("Unassisted", "Teleguided"))) %>%
  ggplot(aes(x=Condition, y=LQ_Mean)) + 
  geom_boxplot(aes(fill = Condition)) +
  scale_y_continuous(breaks = seq(0,14,1), limits = c(0,14.2)) +
  labs(x = "Testing Condition",
       y=expression("Mean Landmark Quality Score"),
       title="Image Quality vs \nTeleguidance Condition") + 
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),
        text = element_text(size = 18),
        axis.title.x = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "grey",
                                    fill = NA,
                                    linewidth = 0.5),
        panel.grid.major = element_line(color = "grey",
                                        linewidth = 0.25,
                                        linetype = 2),) +
  guides(fill = "none") +
  stat_pvalue_manual(pairs_LQvsCondition, label = "pformatted", y.position = 14.175,
                     size = 5)

# DU
df %>%
  filter(Role != "Crew Medic") %>%
  mutate(Condition = factor(Condition, levels = c("Unassisted", "Teleguided"))) %>%
  ggplot(aes(x=Condition, y=DU_Mean)) + 
  geom_boxplot(aes(fill = Condition)) +
  scale_y_continuous(breaks = seq(0,11,1), limits = c(0,11.2)) +
  labs(x = "Testing Condition",
       y=expression("Mean Diagnostic Utility Score"),
       title="Image Quality vs \nTeleguidance Condition") + 
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),
        text = element_text(size = 18),
        axis.title.x = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "grey",
                                    fill = NA,
                                    linewidth = 0.5),
        panel.grid.major = element_line(color = "grey",
                                        linewidth = 0.25,
                                        linetype = 2),) +
  guides(fill = "none") +
  stat_pvalue_manual(pairs_DUvsCondition, label = "pformatted", y.position = 11.175,
                     size = 5)