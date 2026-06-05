# AUTHOR:       Victoria Hurd
# DATE CREATED: 11/25/25
# LAST EDITED:  12/05/25
# PROJECT:      MDRS Teleguidance Study
# TASK:         Running stats, generating plots for system usability
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
  summarise_at(vars(susScore), list(mean = mean))
groupedSDs <- df %>%
  group_by(Role,Condition) %>%
  summarise_at(vars(susScore), list(sd = sd))
timeGroupedMeans <- df %>%
  group_by(Role,Condition,Timepoint) %>%
  summarise_at(vars(susScore), list(mean = mean))
timeGroupedroupedSDs <- df %>%
  group_by(Role,Condition,Timepoint) %>%
  summarise_at(vars(susScore), list(sd = sd))

df %>%
  group_by(Role,Timepoint) %>%
  summarise_at(vars(susScore), list(mean = mean))
df %>%
  group_by(Role,Timepoint) %>%
  summarise_at(vars(susScore), list(sd = sd))
df %>%
  group_by(Timepoint) %>%
  summarise_at(vars(susScore), list(mean = mean))
df %>%
  group_by(Timepoint) %>%
  summarise_at(vars(susScore), list(sd = sd))

means
sds
groupedMeans
groupedSDs
timeGroupedMeans
timeGroupedroupedSDs

# ------------------------------------------------------------------------------
### VISUALIZATIONS ###
# Combine factors into a new variable
df$roleCondition <- interaction(df$Role, df$Condition, sep = "_")
sampleNum <- df %>%
  group_by(roleCondition) %>%
  transmute(count = n()) %>%
  distinct(roleCondition, .keep_all = TRUE)

# Create plot with role/condition splits
ggplot(df, aes(x = roleCondition, y = susScore)) +
  geom_boxplot(aes(fill = roleCondition)) +
  scale_x_discrete(labels = c("Medics\n(n=10)","Non-Medics:\nUnassisted\n(n=30)","Non-Medics:\nTeleguided\n(n=30)")) +
  scale_y_continuous(breaks = seq(0,100,10), limits = c(0,100)) +
  labs(x = "Role and Condition",
       y=expression("SUS Score" ~ italic("(out of 100)")),
       title="Perceived System Usability\nvs. Mission Role & Scan Condition") + 
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
  guides(fill = "none")

# Create plot with role/condition/timepoint splits
# Facet by role
ggplot(df, aes(x = Timepoint, y = susScore)) +
  geom_boxplot(aes(fill = Condition)) +
  scale_x_discrete(labels = c("Scan #1","Scan #2","Scan #3","Scan #4")) +
  scale_y_continuous(breaks = seq(0,100,10), limits = c(0,100)) +
  labs(x = "Timepoint",
       y=expression("SUS Score" ~ italic("(out of 100)")),
       title="Perceived System Usability\nvs. Mission Role & Scan Condition") + 
  theme(plot.title = element_text(hjust = 0.5,face = "bold"),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.background = element_blank(),
        panel.border = element_rect(color = "grey",
                                    fill = NA,
                                    linewidth = 0.5),
        panel.grid.major = element_line(color = "grey",
                                        linewidth = 0.25,
                                        linetype = 2),) +
  facet_wrap(~ Role)

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
  ggqqplot(x="susScore", title = "Unassisted Non-Medic SUS Scores: QQPlot")

df %>%
  filter(Role != "Medic") %>%
  filter(Condition == "Teleguided") %>%
  ggqqplot(x="susScore", title = "Teleguided Non-Medic SUS Scores: QQPlot")

df %>%
  filter(Role == "Medic") %>%
  ggqqplot(x="susScore", title = "Medic SUS Scores: QQPlot")

df %>%
  filter(Role == "Non-Medic") %>%
  ggqqplot(x="susScore", title = "Non-Medic SUS Scores: QQPlot")

# Shapiro-Wilk for Normality
# Note that p-value > 0.05 implies that the distribution of training data isn't  
# significantly different from a theoretical normal distribution
df %>%
  filter(Role != "Medic") %>%
  filter(Condition == "Unassisted") %>%
  shapiro_test(susScore)

df %>%
  filter(Role != "Medic") %>%
  filter(Condition == "Teleguided") %>%
  shapiro_test(susScore)

df %>%
  filter(Role == "Medic") %>%
  shapiro_test(susScore)

df %>%
  filter(Role == "Non-Medic") %>%
  shapiro_test(susScore)

# Levene's Test for Equal Variances
df %>%
  filter(Role != "Medic") %>%
  leveneTest(susScore ~ Condition, data = .)
df %>%
  leveneTest(susScore ~ Role, data = .)

# Grubb's Test for Outliers
# Tests for outliers beyond a theoretical normal distribution
# Note that p-value > 0.05 implies that the distribution of training data isn't  
# significantly different from a theoretical normal distribution
# "Type = 11" specifies Grubbs test for two opposite outliers
grubbs.test(df$susScore[df$Role != "Medic"], 
            type = 11)
grubbs.test(df$susScore[df$Role == "Medic"], 
            type = 11)

# ------------------------------------------------------------------------------
### RUN STATS ###
# HYPOTHESIS 1
# Null hypothesis: 
# Teleguided/unassisted SUS scores are the same (condition has no effect on SUS)
# Alternative hypothesis: 
# Teleguided SUS scores are different from unassisted scores
df %>%
  filter(Role != "Medic") %>%
  #filter(Mission != "2") %>%
  t.test(susScore ~ Condition, data = .,var.equal = TRUE)

# HYPOTHESIS 2
# Null hypothesis: 
# Medic/NonMedic SUS scores are the same (Role has no effect on SUS)
# Alternative hypothesis: 
# Medic SUS scores are different from Non-Medic scores
t.test(susScore ~ Role, data = df, var.equal = FALSE)

# HYPOTHESIS 3
# Null hypothesis: 
# All 8 group mean SUS are the same (Teleguided/Unassisted at all 4 timepoints)
# Alternative hypothesis: 
# At least one of the group means is different
model2wayRMANOVA <- aov_ez(
  id = "ID",
  dv = "susScore",
  within = c("Day","Condition"),
  data = df[df$Role != "Medic", ],
  type = 3L # Type 3 sum of squares is common
)

modelRoleConditionTime <- lmer(susScore ~ roleCondition * Timepoint + (1 | ID), 
                               data = df)
emmRoleConditionTime <- emmeans(modelRoleConditionTime, c("roleCondition"))
#modelLinear <- lmer(susScore ~ Condition * Timepoint + (1 | ID), data = df[df$Role != "Medic", ])

#model2wayRMANOVA
#summary(modelLinear)

summary(modelRoleConditionTime)
emmRoleConditionTime

# ------------------------------------------------------------------------------
### FINAL PLOTS WITH STATISTICAL RESULTS ###
h1stat.test <- df %>%
  filter(Role != "Medic") %>%
  t.test(susScore ~ Condition, data = ., var.equal = TRUE) %>%
  add_significance()
h1stat.test

h2stat.test <- df %>%
  t.test(susScore ~ Role, data = ., var.equal = FALSE) %>%
  add_significance()
h2stat.test

# Teleguided vs Unassisted in nonmedics only
df %>%
  filter(Role != "Medic") %>%
  ggplot(., aes(x = Condition, y = susScore)) +
  geom_boxplot(aes(fill = Condition)) +
  scale_x_discrete(labels = c("Non-Medics:\nUnassisted\n(n=30)","Non-Medics:\nTeleguided\n(n=30)")) +
  scale_y_continuous(breaks = seq(0,100,10), limits = c(0,100)) +
  labs(x = "Condition",
       y=expression("SUS Score" ~ italic("(out of 100)")),
       title="Perceived System Usability\nvs. Scan Condition") + 
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
  annotate("text", x = 1.5, y = 65, label = paste("p = ",round(h1stat.test$p.value,3)),size = 5)

# Medics vs nonmedic SUS scores
df %>%
  ggplot(., aes(x = Role, y = susScore)) +
  geom_boxplot(aes(fill = Role)) +
  scale_x_discrete(labels = c("Medics\n(n=10)","Non-Medics\n(n=60)")) +
  scale_y_continuous(breaks = seq(0,100,10), limits = c(0,100)) +
  labs(x = "Role",
       y=expression("SUS Score" ~ italic("(out of 100)")),
       title="Perceived System Usability\nvs. Assigned Crew Role") + 
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
  annotate("text", x = 1.5, y = 65, label = paste("p = ",round(h2stat.test$p.value,3)),size = 5)


         