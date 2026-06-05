
df <- read_excel(paste(dataFolder,dataFile2,sep = ""))
df <- gradeLandmarkQuality(df)
df <- gradeDiagnosticUtility(df)
# =========================
# LOAD PACKAGES
# =========================

library(tidyverse)
library(psych)
library(mirt)
library(dplyr)
library(tidyr)
# =========================
# SELECT ITEM COLUMNS
# =========================

lq_items <- grep("LQ", names(df), value = TRUE)

du_items <- grep("DU", names(df), value = TRUE)

# Optional:
# make sure all are numeric
lq_items <- df[,lq_items] %>% mutate(across(everything(), as.numeric))
du_items <- df[,du_items] %>% mutate(across(everything(), as.numeric))

# =========================
# CRONBACH ALPHA
# =========================

# ---- LQ ----
lq_alpha <- psych::alpha(lq_items)

print(lq_alpha)

# Item statistics
lq_alpha$item.stats

# Alpha if item deleted
lq_alpha$alpha.drop


# ---- DU ----
du_alpha <- psych::alpha(du_items)

print(du_alpha)

# Item statistics
du_alpha$item.stats

# Alpha if item deleted
du_alpha$alpha.drop

# =========================
# INTERPRETATION NOTES
# =========================

# Look for:
# - low r.drop values
# - items that increase alpha if deleted
# These may be weak or redundant items.

# =========================
# PARALLEL ANALYSIS
# =========================

# Helps determine how many latent factors exist

fa.parallel(lq_items, fa = "fa")
fa.parallel(du_items, fa = "fa")

# =========================
# EXPLORATORY FACTOR ANALYSIS
# =========================

# Start with 1-factor solution
# (change nfactors based on parallel analysis)

lq_fa <- fa(
  lq_items,
  nfactors = 1,
  rotate = "oblimin"
)

print(lq_fa$loadings)

du_fa <- fa(
  du_items,
  nfactors = 1,
  rotate = "oblimin"
)

print(du_fa$loadings)

# =========================
# INTERPRETING LOADINGS
# =========================

# Rough guide:
# >0.70 = very strong
# >0.50 = strong
# >0.30 = moderate
# <0.30 = weak

# Weak-loading items may not contribute much
# to the latent image quality construct.

# =========================
# IRT MODELS
# =========================

# 2PL model:
# estimates discrimination + difficulty

# ---- LQ ----
lq_irt <- mirt(
  lq_items,
  1,
  itemtype = "2PL"
)

summary(lq_irt)

coef(lq_irt, IRTpars = TRUE)


# ---- DU ----
du_irt <- mirt(
  du_items,
  1,
  itemtype = "2PL"
)

summary(du_irt)

coef(du_irt, IRTpars = TRUE)

# =========================
# OPTIONAL:
# ITEM INFORMATION PLOTS
# =========================

plot(lq_irt, type = "infotrace")

plot(du_irt, type = "infotrace")

# =========================
# MOST IMPORTANT OUTPUTS
# =========================

# In the IRT output:
#
# 'a' parameter = discrimination
#
# Higher 'a' values mean:
# - better at distinguishing image quality
# - more informative items
#
# Low 'a' values may indicate
# weak or unnecessary criteria.