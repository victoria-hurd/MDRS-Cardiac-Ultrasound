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
library(car)
# Load custom functions into memory
source("statsSupportFuncs.R")

# ------------------------------------------------------------------------------
### DATA READ ###
df <- read_excel(paste(dataFolder,dataFile,sep = ""))

# ------------------------------------------------------------------------------
### DATA CLEAN ###
# add weighted tlx
df$W_MentalDemand <- df$`NASA TLX Mental Demand Tally`*df$`NASA TLX Mental Demand`
df$W_PhysicalDemand <- df$`NASA TLX Physical Demand Tally`*df$`NASA TLX Physical Demand`
df$W_TemporalDemand <- df$`NASA TLX Temporal Demand Tally`*df$`NASA TLX Temporal Demand`
df$W_Effort <- df$`NASA TLX Effort Tally`*df$`NASA TLX Effort`
df$W_Performance <- df$`NASA TLX Performance Tally`*df$`NASA TLX Performance`
df$W_Frustration <- df$`NASA TLX Frustration Tally`*df$`NASA TLX Frustration`
# Mutate nonmedics role
df <- df %>%
  mutate(Role = ifelse(Role != "Crew Medic", "Non-Medic", Role))
# Ensure variable types
df <- ensureVarTypes(df)
# Add training increase
df$Training_Increase <- df$Training_After - df$Training_Before
# Filter out teleguided
df_filtered <- df %>%
  filter(Condition != "Teleguided")
# Get teleguided vs unassisted vs medics
df_2 <- df %>% 
  unite(col = "rolecondition", Role, Condition, sep = " ", remove = TRUE)
# No week 2 medic included
df_noWeek2 <- df_filtered %>%
  filter(Mission != "2")
df_noID9 <- df_filtered %>%
  filter(ID != "9")

df_noID9 <- df_filtered %>%
  filter(ID != "9")

# ------------------------------------------------------------------------------
### TOTAL WORKLOAD ###

# Try without 9
shapiro.test(df_noID9$`NASA TLX Total Workload`)
df_noID9 %>% group_by(Role) %>% shapiro_test(`NASA TLX Total Workload`)
leveneTest(`NASA TLX Total Workload` ~ Role, data = df_noID9)
# Welch's
t.test(`NASA TLX Total Workload` ~ Role, data = df_noID9)

# lower workload in medics overall
df_filtered %>%
  group_by(Role) %>%
  get_summary_stats(`NASA TLX Total Workload`, type = "mean_sd")

### ONE WAY ANOVAS
model <- aov(ACEP_Median ~ rolecondition, data = df_2)
summary(model)
TukeyHSD(model)
shapiro.test(residuals(model))
leveneTest(model)

model <- aov(`NASA TLX Total Workload` ~ rolecondition, data = df_2)
summary(model)
TukeyHSD(model)
shapiro.test(residuals(model))
leveneTest(model)

model <- aov(acqTime ~ rolecondition, data = df_2)
summary(model)
TukeyHSD(model)
shapiro.test(residuals(model))
leveneTest(model)

# ------------------------------------------------------------------------------
### MENTAL DEMAND ###

# Try without 9
shapiro.test(df_noID9$W_MentalDemand)
leveneTest(W_MentalDemand ~ Role, data = df_noID9)
# Welch's
t.test(W_MentalDemand ~ Role, data = df_noID9)

# lower workload in medics overall
df_filtered %>%
  group_by(Role) %>%
  get_summary_stats(W_MentalDemand, type = "mean_sd")

# ------------------------------------------------------------------------------
### PHYSICAL DEMAND ###

# Try without 9
shapiro.test(df_noID9$W_PhysicalDemand)
leveneTest(W_PhysicalDemand ~ Role, data = df_noID9)
# Welch's
t.test(W_PhysicalDemand ~ Role, data = df_noID9)

# lower workload in medics overall
df_filtered %>%
  group_by(Role) %>%
  get_summary_stats(W_PhysicalDemand, type = "mean_sd")

# ------------------------------------------------------------------------------
### TEMPORAL DEMAND ###

# Try without 9
shapiro.test(df_noID9$W_TemporalDemand)
leveneTest(W_TemporalDemand ~ Role, data = df_noID9)
# Welch's
t.test(W_TemporalDemand ~ Role, data = df_noID9)

# lower workload in medics overall
df_filtered %>%
  group_by(Role) %>%
  get_summary_stats(W_TemporalDemand, type = "mean_sd")

# ------------------------------------------------------------------------------
### PERFORMANCE ###

# Try without 9
shapiro.test(df_noID9$W_Performance)
leveneTest(W_Performance ~ Role, data = df_noID9)
# Welch's
t.test(W_Performance ~ Role, data = df_noID9)

# lower workload in medics overall
df_filtered %>%
  group_by(Role) %>%
  get_summary_stats(W_Performance, type = "mean_sd")

# ------------------------------------------------------------------------------
### EFFORT ###

# Try without 9
shapiro.test(df_noID9$W_Effort)
leveneTest(W_Effort ~ Role, data = df_noID9)
# Welch's
t.test(W_Effort ~ Role, data = df_noID9)

# lower workload in medics overall
df_filtered %>%
  group_by(Role) %>%
  get_summary_stats(W_Effort, type = "mean_sd")

# ------------------------------------------------------------------------------
### FRUSTRATION ###

# Try without 9
shapiro.test(df_noID9$W_Frustration)
leveneTest(W_Frustration ~ Role, data = df_noID9)
# Welch's
t.test(W_Frustration ~ Role, data = df_noID9)

# lower workload in medics overall
df_filtered %>%
  group_by(Role) %>%
  get_summary_stats(W_Frustration, type = "mean_sd")

# ------------------------------------------------------------------------------
### ACEP ###

shapiro.test(df$ACEP_Median)
leveneTest(ACEP_Median ~ Role, data = df)
# not normal, use mann whitney U
wilcox.test(ACEP_Median ~ Role, data = df)
# Try without week 2
shapiro.test(df_noWeek2$ACEP_Median)
leveneTest(ACEP_Median ~ Role, data = df_noWeek2)
# not normal, use mann whitney U
wilcox.test(ACEP_Median ~ Role, data = df_noWeek2, exact = FALSE)
# Try without 9
shapiro.test(df_noID9$ACEP_Median)
leveneTest(ACEP_Median ~ Role, data = df_noID9)
# not normal, use mann whitney U
wilcox.test(ACEP_Median ~ Role, data = df_noID9, exact = FALSE)

t.test(ACEP_Median ~ Role, data = df_noID9)
# Image quality significantly higher for medics
df_noID9 %>%
  group_by(Role) %>%
  get_summary_stats(ACEP_Median, type = "mean_sd")

# ------------------------------------------------------------------------------
### Kimura ###

# Try without 9
shapiro.test(df_noID9$Kimura_Median)
leveneTest(Kimura_Median ~ Role, data = df_noID9)
# Welch's
t.test(Kimura_Median ~ Role, data = df_noID9)
# Image quality significantly higher for medics
df_noID9 %>%
  group_by(Role) %>%
  get_summary_stats(Kimura_Median, type = "mean_sd")

# ------------------------------------------------------------------------------
### LQ ###

# Try without 9
shapiro.test(df_noID9$LQ_Mean)
leveneTest(LQ_Mean ~ Role, data = df_noID9)
# Welch's
t.test(LQ_Mean ~ Role, data = df_noID9)
# Image quality significantly higher for medics
df_noID9 %>%
  group_by(Role) %>%
  get_summary_stats(LQ_Mean, type = "mean_sd")

# ------------------------------------------------------------------------------
### DU ###

# Try without 9
shapiro.test(df_noID9$DU_Mean)
leveneTest(DU_Mean ~ Role, data = df_noID9)
# Welch's
t.test(DU_Mean ~ Role, data = df_noID9)
df_noID9 %>%
  group_by(Role) %>%
  get_summary_stats(DU_Mean, type = "mean_sd")

# ------------------------------------------------------------------------------
### AcqTime ###

# Try without 9
shapiro.test(df_noID9$acqTime)
leveneTest(acqTime ~ Role, data = df_noID9)
# Welch's
t.test(acqTime ~ Role, data = df_noID9)
df_noID9 %>%
  group_by(Role) %>%
  get_summary_stats(acqTime, type = "mean_sd")

# ------------------------------------------------------------------------------
### Usability ###

# Try without 9
shapiro.test(df_noID9$susScore)
leveneTest(susScore ~ Role, data = df_noID9)
# Welch's
t.test(susScore ~ Role, data = df_noID9)
df_noID9 %>%
  group_by(Role) %>%
  get_summary_stats(susScore, type = "mean_sd")

# ------------------------------------------------------------------------------
### PLOTS ###
df_noID9 %>%
  mutate(Role = factor(Role, levels = c("Non-Medic", "Crew Medic"))) %>%
  ggplot(aes(x=Role, y=`NASA TLX Total Workload`)) + 
  geom_boxplot(aes(fill = Role)) +
  scale_y_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
  labs(x = "Role",
       y=expression("NASA TLX Total Workload"),
       title="Total Workload vs \nCrew Role") + 
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

df_noID9 %>%
  mutate(Role = factor(Role, levels = c("Non-Medic", "Crew Medic"))) %>%
  ggplot(aes(x=Role, y=W_MentalDemand)) + 
  geom_boxplot(aes(fill = Role)) +
  #scale_y_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
  labs(x = "Role",
       y=expression("Weighted Mental Demand"),
       title=" Mental Demand vs \nCrew Role") + 
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

df_noID9 %>%
  mutate(Role = factor(Role, levels = c("Non-Medic", "Crew Medic"))) %>%
  ggplot(aes(x=Role, y=W_PhysicalDemand)) + 
  geom_boxplot(aes(fill = Role)) +
  #scale_y_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
  labs(x = "Role",
       y=expression("Weighted Physical Demand"),
       title="Physical Demand vs \nCrew Role") + 
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

df_noID9 %>%
  mutate(Role = factor(Role, levels = c("Non-Medic", "Crew Medic"))) %>%
  ggplot(aes(x=Role, y=W_TemporalDemand)) + 
  geom_boxplot(aes(fill = Role)) +
  #scale_y_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
  labs(x = "Role",
       y=expression("Weighted Temporal Demand"),
       title="Temporal Demand vs \nCrew Role") + 
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

df_noID9 %>%
  mutate(Role = factor(Role, levels = c("Non-Medic", "Crew Medic"))) %>%
  ggplot(aes(x=Role, y=W_Effort)) + 
  geom_boxplot(aes(fill = Role)) +
  #scale_y_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
  labs(x = "Role",
       y=expression("Weighted Effort"),
       title="Effort vs \nCrew Role") + 
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

df_noID9 %>%
  mutate(Role = factor(Role, levels = c("Non-Medic", "Crew Medic"))) %>%
  ggplot(aes(x=Role, y=W_Performance)) + 
  geom_boxplot(aes(fill = Role)) +
  #scale_y_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
  labs(x = "Role",
       y=expression("Weighted Performance"),
       title="Performance vs \nCrew Role") + 
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

df_noID9 %>%
  mutate(Role = factor(Role, levels = c("Non-Medic", "Crew Medic"))) %>%
  ggplot(aes(x=Role, y=W_Frustration)) + 
  geom_boxplot(aes(fill = Role)) +
  #scale_y_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
  labs(x = "Role",
       y=expression("Weighted Frustration"),
       title="Frustration vs \nCrew Role") + 
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

df_noID9 %>%
  mutate(Role = factor(Role, levels = c("Non-Medic", "Crew Medic"))) %>%
  ggplot(aes(x=Role, y=susScore)) + 
  geom_boxplot(aes(fill = Role)) +
  labs(x = "Role",
       y=expression("System Usability"),
       title="System Usability vs \nCrew Role") + 
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

df_noID9 %>%
  mutate(Role = factor(Role, levels = c("Non-Medic", "Crew Medic"))) %>%
  ggplot(aes(x=Role, y=acqTime)) + 
  geom_boxplot(aes(fill = Role)) +
  labs(x = "Role",
       y=expression("Acquisition Time " ~ italic("(seconds)")),
       title="Acquisition Time vs \nCrew Role") + 
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

# Image Quality
df_noID9 %>%
  mutate(Role = factor(Role, levels = c("Non-Medic", "Crew Medic"))) %>%
  ggplot(aes(x=Role, y=ACEP_Median)) + 
  geom_boxplot(aes(fill = Role)) +
  scale_y_continuous(breaks = seq(1,5,1), limits = c(1,5.2)) +
  labs(x = "Role",
       y=expression("Median ACEP Score"),
       title="Image Quality vs \nCrew Role") + 
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

df_noID9 %>%
  mutate(Role = factor(Role, levels = c("Non-Medic", "Crew Medic"))) %>%
  ggplot(aes(x=Role, y=Kimura_Median)) + 
  geom_boxplot(aes(fill = Role)) +
  scale_y_continuous(breaks = seq(0,4,1), limits = c(0,4.2)) +
  labs(x = "Role",
       y=expression("Median Kimura Score"),
       title="Image Quality vs \nCrew Role") + 
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

df_noID9 %>%
  mutate(Role = factor(Role, levels = c("Non-Medic", "Crew Medic"))) %>%
  ggplot(aes(x=Role, y=LQ_Mean)) + 
  geom_boxplot(aes(fill = Role)) +
  scale_y_continuous(breaks = seq(0,14,1), limits = c(0,14.2)) +
  labs(x = "Role",
       y=expression("Mean Landmark Quality Score"),
       title="Image Quality vs \nCrew Role") + 
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

df_noID9 %>%
  mutate(Role = factor(Role, levels = c("Non-Medic", "Crew Medic"))) %>%
  ggplot(aes(x=Role, y=DU_Mean)) + 
  geom_boxplot(aes(fill = Role)) +
  scale_y_continuous(breaks = seq(0,11,1), limits = c(0,11.2)) +
  labs(x = "Role",
       y=expression("Mean Diagnostic Utility Score"),
       title="Image Quality vs \nCrew Role") + 
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
