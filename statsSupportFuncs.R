# AUTHOR:       Victoria Hurd
# DATE CREATED: 5/13/26
# LAST EDITED:  5/13/26
# PROJECT:      MDRS Ultrasound Task Data Analysis
# TASK:         Stats Functions

library(car)
library(rstatix)
library(outliers)

# ------------------------------------------------------------------------------
### LMM ASSUMPTIONS CHECK ###
checkAssumptionsLMM <- function(model,varStr){
  # QQPlot for Normality
  # Normally-distributed data will appear within gray band if normally distributed
  qqPlot(residuals(model), main = paste("Normality of Residuals:",varStr))
  # Residuals vs Fitted for Homoskedasticity
  # Homoskedasticity will manifest as consisent, uniform points across x and y
  plot(model, main = paste("Homoskedasticity of Residuals:",varStr))# homoskedasticity of residuals
}

# ------------------------------------------------------------------------------
### VERIFY VARIABLE TYPES ###
ensureVarTypes <- function(df) {
  # Make categorical variables type factor
  df$Condition <- factor(df$Condition, levels = c("Unassisted","Teleguided"))
  df$Mission <- factor(df$Mission, levels = c("1","2","3"))
  df$Timepoint <- as.factor(df$Timepoint)
  df$Role <- as.factor(df$Role)
  df$ID <- as.factor(df$ID)
  df$DemoG_sex <- as.factor(df$DemoG_sex)
  df$DemoG_gender <- as.factor(df$DemoG_gender)
  # Make continuous variables type numeric
  df$ACEP_Median <- as.numeric(df$ACEP_Median)
  df$Kimura_Median <- as.numeric(df$Kimura_Median)
  df$LQ_Mean <- as.numeric(df$LQ_Mean)
  df$DU_Mean <- as.numeric(df$DU_Mean)
  df$DemoG_age <- as.numeric(df$DemoG_age)
  df$Training_Before <- as.numeric(df$Training_Before)
  df$Training_After <- as.numeric(df$Training_After)
  df$acqTime <- as.numeric(df$acqTime)
  df$susScore <- as.numeric(df$susScore)
  df$`NASA TLX Total Workload` <- as.numeric(df$`NASA TLX Total Workload`)
  
  return(df)
}

# ------------------------------------------------------------------------------
### CREATE PVAL FOR PLOTS (CONDITION) ###
pValFormatPlotCondition <- function(emmobject) {
  df <- as.data.frame(emmobject)
  df$group1 <- "Unassisted"
  df$group2 <- "Teleguided"
  df$psignif <- symnum(df$p.value, corr=FALSE, 
                                            cutpoints = c(0, .001, .01, .05, 1), 
                                            symbols = c("***", "**", "*", "ns"))
  df <- df %>%
    mutate(pformatted = if_else(p.value < 0.001, "p < 0.001 ***", 
                                paste("p =",as.character(signif(p.value,2)),
                                      psignif)))
  
  return(df)
}

# ------------------------------------------------------------------------------
### CREATE PVAL FOR PLOTS (MISSION) ###
pValFormatPlotCondition <- function(emmobject) {
  df <- as.data.frame(emmobject)
  df$group1 <- "1"
  df$group2 <- "2"
  df$psignif <- symnum(df$p.value, corr=FALSE, 
                       cutpoints = c(0, .001, .01, .05, 1), 
                       symbols = c("***", "**", "*", "ns"))
  df <- df %>%
    mutate(pformatted = if_else(p.value < 0.001, "p < 0.001 ***", 
                                paste("p =",as.character(signif(p.value,2)),
                                      psignif)))
  
  return(df)
}

# ------------------------------------------------------------------------------
### CREATE PVAL FOR PLOTS (TIMEPOINT) ###
pValFormatVals <- function(df,pval) {
  df$psignif <- symnum(pval, corr=FALSE, 
                       cutpoints = c(0, .001, .01, .05, 1), 
                       symbols = c("***", "**", "*", "ns"))
  df <- df %>%
    mutate(pformatted = if_else(pval < 0.001, "p < 0.001 ***", 
                                paste("p =",as.character(signif(pval,2)),
                                      psignif)))
  
  return(df)
}