# AUTHOR:       Victoria Hurd
# DATE CREATED: 4/24/26
# LAST EDITED:  4/29/26
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
# Pull out medics
df <- df %>%
  filter(Role != "Crew Medic")
# Ensure variable types
df <- ensureVarTypes(df)
# Add training increase
df$Training_Increase <- df$Training_After - df$Training_Before

# ------------------------------------------------------------------------------
### LMM for Total Workload ###

# Run model
varStr <- "Total Workload"
model <- lmer(`NASA TLX Total Workload` ~ Condition * Mission + Timepoint +
                DemoG_age + DemoG_sex +
                Training_Before + Training_After + Training_Increase +
                (1 | ID), data = df)
isSingular(model)
# Check assumptions
checkAssumptionsLMM(model,varStr)
# Show model results
anova(model)
# Get mean/SD for significant groupings
df %>%
  group_by(Mission) %>%
  get_summary_stats(`NASA TLX Total Workload`, type = "mean_sd")
df %>%
  group_by(Timepoint) %>%
  get_summary_stats(`NASA TLX Total Workload`, type = "mean_sd")
df %>%
  group_by(Condition) %>%
  get_summary_stats(`NASA TLX Total Workload`, type = "mean_sd")
# Get groupwise results
pairs(emmeans(model, ~ Mission), adjust = "tukey")
pairs(emmeans(model, ~ Timepoint), adjust = "tukey")
pairsTotalMission <- as.data.frame(pairs(emmeans(model, ~ Mission), adjust = "tukey"))
pairsTotalTimepoint<- as.data.frame(pairs(emmeans(model, ~ Timepoint), adjust = "tukey"))

# ------------------------------------------------------------------------------
### LMM for Weighted Mental Demand ###

df$W_MentalDemand <- df$`NASA TLX Mental Demand Tally`*df$`NASA TLX Mental Demand`
# Run model
varStr <- "Weighted Mental Demand"
model <- lmer(W_MentalDemand ~ Condition * Mission + Timepoint +
                DemoG_age + DemoG_sex +
                Training_Before + Training_After + Training_Increase +
                (1 | ID), data = df)
# Check assumptions
checkAssumptionsLMM(model,varStr)
# Show model results
anova(model)
# Get mean/SD for significant groupings
df %>%
  group_by(DemoG_sex) %>%
  get_summary_stats(W_MentalDemand, type = "mean_sd")
# Get groupwise results
pairs(emmeans(model, ~ DemoG_sex), adjust = "tukey")
pairsMentalSex <- as.data.frame(pairs(emmeans(model, ~ DemoG_sex), adjust = "tukey"))

# ------------------------------------------------------------------------------
### LMM for Weighted Physical Demand ###

df$W_PhysicalDemand <- df$`NASA TLX Physical Demand Tally`*df$`NASA TLX Physical Demand`
# Run model
varStr <- "Weighted Physical Demand"
model <- lmer((W_PhysicalDemand) ~ Condition * Mission + Timepoint +
                DemoG_age + DemoG_sex +
                Training_Before + Training_After + Training_Increase +
                (1 | ID), data = df)
isSingular(model)
# Check assumptions
checkAssumptionsLMM(model,varStr)
# Show model results
anova(model)

# ------------------------------------------------------------------------------
### LMM for Weighted Temporal Demand ###

df$W_TemporalDemand <- df$`NASA TLX Temporal Demand Tally`*df$`NASA TLX Temporal Demand`
# Run model
varStr <- "Weighted Temporal Demand"
model <- lmer(W_TemporalDemand ~ Condition * Mission + Timepoint +
                DemoG_age + DemoG_sex +
                Training_Before + Training_After + Training_Increase +
                (1 | ID), data = df)
isSingular(model)
# Check assumptions
checkAssumptionsLMM(model,varStr)
# Show model results
anova(model)
# Get mean/SD for significant groupings
df %>%
  group_by(Timepoint) %>%
  get_summary_stats(W_TemporalDemand, type = "mean_sd")
# Get groupwise results
pairs(emmeans(model, ~ Timepoint), adjust = "tukey")

# ------------------------------------------------------------------------------
### LMM for Weighted Performance Demand ###

df$W_Performance <- df$`NASA TLX Performance Tally`*df$`NASA TLX Performance`
# Run model
varStr <- "Weighted NASA TLX Performance"
model <- lmer(W_Performance ~ Condition * Mission + Timepoint +
                DemoG_age + DemoG_sex +
                Training_Before + Training_After + Training_Increase +
                (1 | ID), data = df)
isSingular(model)
# Check assumptions
checkAssumptionsLMM(model,varStr)
# Show model results
anova(model)

# ------------------------------------------------------------------------------
### LMM for Weighted Effort ###

df$W_Effort <- df$`NASA TLX Effort Tally`*df$`NASA TLX Effort`
# Run model
varStr <- "Weighted NASA TLX Effort"
model <- lmer(W_Effort ~ Condition + Mission + Timepoint +
                DemoG_age + DemoG_sex +
                Training_Before + Training_After + Training_Increase +
                (1 | ID), data = df)
isSingular(model)
# Check assumptions
checkAssumptionsLMM(model,varStr)
# Show model results
anova(model)
# Get mean/SD for significant groupings
df %>%
  group_by(Training_Before) %>%
  get_summary_stats(W_Effort, type = "mean_sd")

# ------------------------------------------------------------------------------
### LMM for Weighted Frustration ###

df$W_Frustration <- df$`NASA TLX Frustration Tally`*df$`NASA TLX Frustration`
# Run model
varStr <- "Weighted NASA TLX Frustration"
model <- lmer(W_Frustration ~ Condition + Mission + Timepoint +
                DemoG_age + DemoG_sex +
                Training_Before + Training_After + Training_Increase +
                (1 | ID), data = df)
# Check assumptions
checkAssumptionsLMM(model,varStr)
# Show model results
anova(model)
# Get mean/SD for significant groupings
df %>%
  group_by(Training_After) %>%
  get_summary_stats(W_Frustration, type = "mean_sd")
df %>%
  group_by(Mission) %>%
  get_summary_stats(W_Frustration, type = "mean_sd")
df %>%
  group_by(Condition) %>%
  get_summary_stats(W_Frustration, type = "mean_sd")
# Get groupwise results
pairs(emmeans(model, ~ Mission), adjust = "tukey")
pairs(emmeans(model, ~ Condition), adjust = "tukey")
pairsFrustrationCondition <- as.data.frame(pairs(emmeans(model, ~ Condition), adjust = ""))
pairsFrustrationMission <- as.data.frame(pairs(emmeans(model, ~ Mission), adjust = "tukey"))
# ------------------------------------------------------------------------------
### P VALUE FORMAT ###
# Define which groups to compare and what the manual p-value is
pValsTotalMission1vs2 <- data.frame(group1 = "1", group2 = "2")
pValsTotalMission1vs2 <- pValFormatVals(pValsTotalMission1vs2,pairsTotalMission$p.value[1])

pValsTotalTimepoint1vs4 <- data.frame(group1 = "1", group2 = "4")
pValsTotalTimepoint1vs4 <- pValFormatVals(pValsTotalTimepoint1vs4,pairsTotalTimepoint$p.value[3])

pValsMentalvsSex <- data.frame(group1 = "Female", group2 = "Male")
pValsMentalvsSex <- pValFormatVals(pValsMentalvsSex,pairsMentalSex$p.value[1])

# ------------------------------------------------------------------------------
### PLOTS ###
# ACQ VS MISSION
df %>%
  filter(Role != "Crew Medic") %>%
  mutate(Mission = factor(Mission, levels = c("1", "2","3"))) %>%
  ggplot(aes(x=Mission, y=`NASA TLX Total Workload`)) + 
  geom_boxplot(aes(fill = Mission)) +
  scale_y_continuous(breaks = seq(0,100,20), limits = c(0,100.2)) +
  labs(x = "Mission Number",
       y=expression("NASA TLX Total Workload"),
       title="Acquisition Time vs \nMission Number") + 
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
  stat_pvalue_manual(pValsTotalMission1vs2, label = "pformatted", y.position = 95,
                     size = 5)

df %>%
  filter(Role != "Crew Medic") %>%
  mutate(Timepoint = factor(Timepoint, levels = c("1", "2","3","4"))) %>%
  ggplot(aes(x=Timepoint, y=`NASA TLX Total Workload`)) + 
  geom_boxplot(aes(fill = Timepoint)) +
  scale_y_continuous(breaks = seq(0,100,20), limits = c(0,100.2)) +
  labs(x = "Exam Number",
       y = expression("NASA TLX Total Workload"),
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
  stat_pvalue_manual(pValsTotalTimepoint1vs4, label = "pformatted", y.position = 95,
                     size = 5)

df %>%
  filter(Role != "Crew Medic") %>%
  mutate(Sex = factor(DemoG_sex, levels = c("Female","Male"))) %>%
  ggplot(aes(x=DemoG_sex, y=W_MentalDemand)) + 
  geom_boxplot(aes(fill = DemoG_sex)) +
  #scale_y_continuous(breaks = seq(0,500,20), limits = c(500)) +
  labs(x = "Sex",
       y = expression("Weighted Mental Demand"),
       title="Weighted Mental Demand vs \nParticipant Sex") + 
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
  guides(fill = "none")  +
  stat_pvalue_manual(pValsMentalvsSex, label = "pformatted", y.position = 400,
                     size = 5)

df %>%
  filter(Role != "Crew Medic") %>%
  mutate(Timepoint = factor(Timepoint, levels = c("1","2","3","4"))) %>%
  ggplot(aes(x=Timepoint, y=W_TemporalDemand)) + 
  geom_boxplot(aes(fill = Timepoint)) +
  #scale_y_continuous(breaks = seq(0,500,20), limits = c(500)) +
  labs(x = "Exam Number",
       y = expression("Weighted Temporal Demand"),
       title="Weighted Temporal Demand vs \nExam Number") + 
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

df %>%
  filter(Role != "Crew Medic") %>%
  ggplot(aes(x=Training_Before, y=W_Effort)) + 
  geom_point() +
  scale_x_continuous(breaks = seq(0,25,1), limits = c(5,17)) +
  labs(x = "Pre-Training Knowledge Assessment Score",
       y = expression("Weighted Effort"),
       title="Weighted Effort vs \nPrior Knowledge") + 
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),
        text = element_text(size = 18),
        panel.background = element_blank(),
        panel.border = element_rect(color = "grey",
                                    fill = NA, 
                                    linewidth = 0.5),
        panel.grid.major = element_line(color = "grey",
                                        linewidth = 0.25,
                                        linetype = 2),) +
  guides(fill = "none") +
  geom_smooth(method = "lm", se = FALSE)

df %>%
  filter(Role != "Crew Medic") %>%
  ggplot(aes(x=Training_After, y=W_Frustration)) + 
  geom_point() +
  #scale_x_continuous(breaks = seq(0,25,1), limits = c(5,17)) +
  labs(x = "Post-Training Knowledge Assessment Score",
       y = expression("Weighted Frustration"),
       title="Weighted Frustration vs \nPost-Training Knowledge") + 
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),
        text = element_text(size = 18),
        panel.background = element_blank(),
        panel.border = element_rect(color = "grey",
                                    fill = NA, 
                                    linewidth = 0.5),
        panel.grid.major = element_line(color = "grey",
                                        linewidth = 0.25,
                                        linetype = 2),) +
  guides(fill = "none") +
  geom_smooth(method = "lm", se = FALSE)

df %>%
  filter(Role != "Crew Medic") %>%
  mutate(Condition = factor(Condition, levels = c("Unassisted", "Teleguided"))) %>%
  ggplot(aes(x=Condition, y=W_Frustration)) + 
  geom_boxplot(aes(fill = Condition)) +
  #scale_y_continuous(breaks = seq(0,15,1), limits = c(0,11.2)) +
  labs(x = "Testing Condition",
       y=expression("Weighted Frustration"),
       title="Weighted Frustration vs \nTeleguidance Condition") + 
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

df %>%
  filter(Role != "Crew Medic") %>%
  mutate(Mission = factor(Mission, levels = c("1", "2","3"))) %>%
  ggplot(aes(x=Mission, y=W_Frustration)) + 
  geom_boxplot(aes(fill = Mission)) +
  #scale_y_continuous(breaks = seq(0,100,20), limits = c(0,100.2)) +
  labs(x = "Mission Number",
       y=expression("Weighted Frustration"),
       title="Weighted Frustration vs \nMission Number") + 
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