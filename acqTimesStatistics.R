# AUTHOR:       Victoria Hurd
# DATE CREATED: 4/24/26
# LAST EDITED:  4/29/26
# PROJECT:      MDRS Ultrasound Task Data Analysis
# TASK:         Run stats for acquisition times

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
# Pull out medics
df <- df %>%
  filter(Role != "Crew Medic")
# Ensure variable types
df <- ensureVarTypes(df)
# Add training increase
df$Training_Increase <- df$Training_After - df$Training_Before

# ------------------------------------------------------------------------------
### LMM for Acquisition Times ###

# Run model
varStr <- "Acquisition Time"
model <- lmer(acqTime ~ Condition * Mission + Timepoint + 
                DemoG_age + DemoG_sex + 
                Training_Before + Training_After + Training_Increase +
                (1 | ID), data = df)
isSingular(model) # Above model is singular/overfitted. 
# Removing factor that causes singularity but maintains most complex model (age)
model <- lmer(acqTime ~ Condition * Mission + Timepoint + 
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
  group_by(Mission) %>%
  get_summary_stats(acqTime, type = "mean_sd")
df %>%
  group_by(Timepoint) %>%
  get_summary_stats(acqTime, type = "mean_sd")
# Get groupwise results
pairs(emmeans(model, ~ Timepoint), adjust = "tukey")
pairs(emmeans(model, ~ Mission), adjust = "tukey")

pairsAcqMission <- as.data.frame(pairs(emmeans(model, ~ Mission), adjust = "tukey"))
pairsAcqTimepoint<- as.data.frame(pairs(emmeans(model, ~ Timepoint), adjust = "tukey"))
# Results: 
# All assumptions pass (note outlier removed and age not included as fixed effect)
# 1) acquisition time is significantly higher in mission 2 compared to both 1 and 3 (*)
# 2) acquisition time is significantly lower at exam 4 compared to exam 1 (**)

# ------------------------------------------------------------------------------
### P VALUE FORMAT ###
# Define which groups to compare and what the manual p-value is
pValsAcqMission1vs2 <- data.frame(group1 = "1", group2 = "2")
pValsAcqMission1vs2 <- pValFormatVals(pValsAcqMission1vs2,pairsAcqMission$p.value[1])
pValsAcqMission2vs3 <- data.frame(group1 = "2",group2 = "3")
pValsAcqMission2vs3 <- pValFormatVals(pValsAcqMission2vs3,pairsAcqMission$p.value[3])

pValsAcqTimepoint1vs2 <- data.frame(group1 = "1", group2 = "2")
pValsAcqTimepoint1vs4 <- data.frame(group1 = "1", group2 = "4")
pValsAcqTimepoint1vs2 <- pValFormatVals(pValsAcqTimepoint1vs2,pairsAcqTimepoint$p.value[1])
pValsAcqTimepoint1vs4 <- pValFormatVals(pValsAcqTimepoint1vs4,pairsAcqTimepoint$p.value[3])

# ------------------------------------------------------------------------------
### PLOTS ###
# ACQ VS CONDITION
df %>%
  filter(Role != "Crew Medic") %>%
  mutate(Condition = factor(Condition, levels = c("Unassisted", "Teleguided"))) %>%
  ggplot(aes(x=Condition, y=acqTime)) + 
  geom_boxplot(aes(fill = Condition)) +
  #scale_y_continuous(breaks = seq(0,15,1), limits = c(0,11.2)) +
  labs(x = "Testing Condition",
       y=expression("Acquisition Time " ~ italic("(seconds)")),
       title="Acquisition Time vs \nTeleguidance Condition") + 
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
  guides(fill = "none") 

# ACQ VS MISSION
df %>%
  filter(Role != "Crew Medic") %>%
  mutate(Mission = factor(Mission, levels = c("1", "2","3"))) %>%
  ggplot(aes(x=Mission, y=acqTime)) + 
  geom_boxplot(aes(fill = Mission)) +
  #scale_y_continuous(breaks = seq(0,15,1), limits = c(0,11.2)) +
  labs(x = "Mission Number",
       y=expression("Acquisition Time " ~ italic("(seconds)")),
       title="Acquisition Time vs \nMission Number") + 
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),
        text = element_text(size = 18),
        #axis.title.x = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "grey",
                                    fill = NA,
                                    linewidth = 0.5),
        panel.grid.major = element_line(color = "grey",
                                        linewidth = 0.25,
                                        linetype = 2),) +
  guides(fill = "none")  +
  stat_pvalue_manual(pValsAcqMission1vs2, label = "pformatted", y.position = 700,
                     size = 5) +
  stat_pvalue_manual(pValsAcqMission2vs3, label = "pformatted", y.position = 800,
                     size = 5)

df %>%
  filter(Role != "Crew Medic") %>%
  mutate(Timepoint = factor(Timepoint, levels = c("1", "2","3","4"))) %>%
  ggplot(aes(x=Timepoint, y=acqTime)) + 
  geom_boxplot(aes(fill = Timepoint)) +
  labs(x = "Exam Number",
       y=expression("Acquisition Time " ~ italic("(seconds)")),
       title="Acquisition Time vs \nExam Number") + 
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),
        text = element_text(size = 18),
        panel.background = element_blank(),
        panel.border = element_rect(color = "grey",
                                    fill = NA,
                                    linewidth = 0.5),
        panel.grid.major = element_line(color = "grey",
                                        linewidth = 0.25,
                                        linetype = 2),) +
  guides(fill = "none")  +
  #stat_pvalue_manual(pValsAcqTimepoint1vs2, label = "pformatted", y.position = 700,size = 5) +
  stat_pvalue_manual(pValsAcqTimepoint1vs4, label = "pformatted", y.position = 800,
                     size = 5)
