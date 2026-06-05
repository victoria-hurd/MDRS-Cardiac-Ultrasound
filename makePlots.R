# AUTHOR:       Victoria Hurd
# DATE CREATED: 4/24/26
# LAST EDITED:  4/29/26
# PROJECT:      MDRS Ultrasound Task Data Analysis
# TASK:         Make Plots

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
library(ggplot2)

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

# ------------------------------------------------------------------------------
### Image Quality Plots ###
stat.test <- df %>%
  filter(Role != "Crew Medic") %>%
  t_test(Score ~ Timing, alternative = 'less') %>%
  add_significance() %>% 
  add_xy_position(add_xy_position(x = "Timing"))
stat.test
library(ggpubr)
df %>%
  filter(Role != "Crew Medic") %>%
  mutate(Condition = factor(Condition, levels = c("Unassisted", "Teleguided"))) %>%
  ggplot(aes(x=Condition, y=ACEP_Median)) + 
  geom_boxplot(aes(fill = Condition)) +
  #scale_x_discrete(labels = c("Pre-Training","Post-Training")) +
  #scale_y_continuous(breaks = seq(0,25,5), limits = c(0,25)) +
  labs(x = "Testing Condition",
       y=expression("Median ACEP Score " ~ italic("(out of 5)")),
       title="Image Quality vs \nTeleguidance Condition") + 
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),
        text = element_text(size = 20),
        axis.title.x = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "grey",
                                    fill = NA,
                                    linewidth = 0.5),
        panel.grid.major = element_line(color = "grey",
                                        linewidth = 0.25,
                                        linetype = 2),) +
  guides(fill = "none") +
  stat_compare_means("p.signif")

# ------------------------------------------------------------------------------
### Image Quality Plots ###

