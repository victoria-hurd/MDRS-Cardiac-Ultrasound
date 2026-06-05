# AUTHOR:       Victoria Hurd
# DATE CREATED: 4/24/26
# LAST EDITED:  4/24/26
# PROJECT:      MDRS Ultrasound Task Data Analysis
# TASK:         Run ANOVAs for usability

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
### LMM for Usability ###

# Run model
varStr <- "System Usability"
model <- lmer(susScore ~ Condition * Mission + Timepoint +
                DemoG_age + DemoG_sex +
                Training_Before + Training_After + Training_Increase +
                (1 | ID), data = df)
# Check assumptions
checkAssumptionsLMM(model,varStr)
# Show model results
anova(model)

# ------------------------------------------------------------------------------
### PLOTS ###
#  VS MISSION
df %>%
  filter(Role != "Crew Medic") %>%
  mutate(Mission = factor(Mission, levels = c("1", "2","3"))) %>%
  ggplot(aes(x=Mission, y=susScore)) + 
  geom_boxplot(aes(fill = Mission)) +
  scale_y_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
  labs(x = "Mission Number",
       y=expression("System Usability"),
       title="System Usability vs \nMission Number") + 
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),
        text = element_text(size = 18),
        panel.background = element_blank(),
        panel.border = element_rect(color = "grey",
                                    fill = NA,
                                    linewidth = 0.5),
        panel.grid.major = element_line(color = "grey",
                                        linewidth = 0.25,
                                        linetype = 2),) +
  guides(fill = "none")

df %>%
  filter(Role != "Crew Medic") %>%
  mutate(Condition = factor(Condition, levels = c("Unassisted", "Teleguided"))) %>%
  ggplot(aes(x=Condition, y=susScore)) + 
  geom_boxplot(aes(fill = Condition)) +
  labs(x = "Condition",
       y=expression("System Usability"),
       title="System Usability vs \nTeleguided Condition") + 
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),
        text = element_text(size = 18),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        panel.border = element_rect(color = "grey",
                                    fill = NA,
                                    linewidth = 0.5),
        panel.grid.major = element_line(color = "grey",
                                        linewidth = 0.25,
                                        linetype = 2),) +
  guides(fill = "none")
