# AUTHOR:       Victoria Hurd
# DATE CREATED: 4/23/26
# LAST EDITED:  4/23/26
# PROJECT:      MDRS Ultrasound Task Data Analysis
# TASK:         Helper Functions

library(dplyr)
library(tidyr)

# ------------------------------------------------------------------------------
### GET MODE ###
getMode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

