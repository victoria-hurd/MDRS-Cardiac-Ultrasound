# AUTHOR:       Victoria Hurd
# DATE CREATED: 11/25/25
# LAST EDITED:  12/04/25
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
library(outliers)
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
            BeforeMean = mean(Before, na.rm = TRUE),
            AfterMean = mean(After, na.rm = TRUE),
            BeforeSD = sd(Before, na.rm = TRUE),
            AfterSD = sd(After, na.rm = TRUE)) %>%
  pivot_longer(
    cols = c(BeforeMean,AfterMean,BeforeSD,AfterSD),
    names_to = "Timing",
    values_to = "Score")

# Tabulate - look at before vs after scores for non-medics
meansNonmedics <- df_wide %>%
  filter(Role != "Crew Medic") %>%
  summarise(numSamples = length(After),
            BeforeMean = mean(Before, na.rm = TRUE),
            AfterMean = mean(After, na.rm = TRUE),
            BeforeSD = sd(Before, na.rm = TRUE),
            AfterSD = sd(After, na.rm = TRUE)) %>%
  pivot_longer(
    cols = c(BeforeMean,AfterMean,BeforeSD,AfterSD),
    names_to = "Timing",
    values_to = "Score")


# Tabulate - look at before vs after scores for medics
meansMedics <- df_wide %>%
  filter(Role == "Crew Medic") %>%
  summarise(numSamples = length(After),
            BeforeMean = mean(Before, na.rm = TRUE),
            AfterMean = mean(After, na.rm = TRUE),
            BeforeSD = sd(Before, na.rm = TRUE),
            AfterSD = sd(After, na.rm = TRUE)) %>%
  pivot_longer(
    cols = c(BeforeMean,AfterMean,BeforeSD,AfterSD),
    names_to = "Timing",
    values_to = "Score")

# Print tabulation Results
print(means)
print(meansNonmedics)
print(meansMedics)

# ------------------------------------------------------------------------------
### VISUALIZATIONS ###

# Create density plots of final score based on role
df %>%
  ggplot(aes(x=Score, color=Timing)) + 
  geom_density() +
  geom_vline(data=means,aes(xintercept=Score,color=Timing),linetype="dashed") + 
  scale_x_continuous(breaks = seq(0,25,1), limits = c(0,25)) +
  labs(x="Total Score (out of 25)",
       title="Assessment Score Density: All Participants")

df %>%
  filter(Role != "Crew Medic") %>%
  ggplot(aes(x=Score, color=Timing)) + 
  geom_density() +
  geom_vline(data=meansNonmedics,aes(xintercept=Score,color=Timing),linetype="dashed") + 
  scale_x_continuous(breaks = seq(0,25,1), limits = c(0,25)) +
  labs(x="Total Score (out of 25)",
       title="Assessment Score Density: Non-Medics")

df %>%
  filter(Role == "Crew Medic") %>%
  ggplot(aes(x=Score, color=Timing)) + 
  geom_density() +
  geom_vline(data=meansMedics,aes(xintercept=Score,color=Timing),linetype="dashed") + 
  scale_x_continuous(breaks = seq(0,25,1), limits = c(0,25)) +
  labs(x="Total Score (out of 25)",
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
  scale_y_continuous(breaks = seq(0,25,1), limits = c(0,25)) +
  labs(x = "Testing Condition",
       y="Total Score (out of 25)",
       title="Assessment Scores Before vs. After Training Module")

# ------------------------------------------------------------------------------
### ASSUMPTION CHECKS ###

# For paired t-tests, assumption checks are performed on difference b/w pairs
# Subtract before score from after score - difference is used for checks
# Assumptions: 
#   1) Continuous dependent variable (interval/ratio).
#   2) Normally-distributed data (check visually & via Shapiro Wilk)
#   3) Independent subjects with paired observations
#   4) No outliers in dependent variable (differences between pairs)
# If fails assumptions, would have to use paired two-samples Wilcoxon Test

# Get differences
df_assumptions <- df_wide %>%
  mutate(Improvement = After-Before)

# QQPlot for Normality
# Normally-distributed data will appear within gray band if normally distributed
df_assumptions %>%
  filter(Role != "Crew Medic") %>%
  ggqqplot(x="Improvement", title = "Training Improvement in Non-Medics: QQPlot")

# Shapiro-Wilk for Normality
# Note that p-value > 0.05 implies that the distribution of training data isn't  
# significantly different from a theoretical normal distribution
df_assumptions %>%
  filter(Role != "Crew Medic") %>%
  shapiro_test(Improvement)

# Grubb's Test for Outliers
# Tests for outliers beyond a theoretical normal distribution
# Note that p-value > 0.05 implies that the distribution of training data isn't  
# significantly different from a theoretical normal distribution
# "Type = 11" specifies Grubbs test for two opposite outliers
grubbs.test(df_assumptions$Improvement[df_assumptions$Role != "Crew Medic"], 
            type = 11)

# Let's also see if the after scores are normally distributed: 
df_assumptions %>%
  filter(Role != "Crew Medic") %>%
  shapiro_test(After)
# Note: after scores are also normally distributed around rounded score of 20 

# ------------------------------------------------------------------------------
### RUN STATS ###

# Null hypothesis: 
# Nonmedic before scores and after scores are the same (training module has no effect on "after" scores)
# Alternative hypothesis: 
# Nonmedic scores after training module are higher than those taken before (training module improves score)

df %>%
  filter(Role != "Crew Medic") %>%
  t.test(Score ~ Timing, data = ., alternative = 'greater', paired = TRUE)

# ------------------------------------------------------------------------------
### FINAL PLOTS WITH STATISTICAL RESULTS ###
stat.test <- df %>%
  filter(Role != "Crew Medic") %>%
  mutate(Timing = factor(Timing, levels = c("Before", "After"))) %>%
  t_test(Score ~ Timing, alternative = 'less', paired=TRUE) %>%
  add_significance() %>% 
  add_xy_position(add_xy_position(x = "Timing"))
stat.test

df %>%
  filter(Role != "Crew Medic") %>%
  mutate(Timing = factor(Timing, levels = c("Before", "After"))) %>%
  ggplot(aes(x=Timing, y=Score)) + 
  geom_boxplot(aes(fill = Timing)) +
  scale_x_discrete(labels = c("Pre-Training","Post-Training")) +
  scale_y_continuous(breaks = seq(0,25,5), limits = c(0,25)) +
  labs(x = "Testing Condition",
       y=expression("Total Score " ~ italic("(out of 25)")),
       title="Training Module Efficacy:\nPre- & Post-Assessment Scores") + 
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
  stat_pvalue_manual(stat.test, label = "p = {p} {p.signif}", size = 5)
  