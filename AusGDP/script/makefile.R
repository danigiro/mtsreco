# C matrix
rm(list=ls())
source("./script/lin_comb_mat.R")

# Base forecasts income side
rm(list=ls())
side <- "inc"
source("./script/base.R")

# Base forecasts expenditure side
rm(list=ls())
side <- "exp"
source("./script/base.R")

# Base forecasts income + expenditure side
rm(list=ls())
source("./script/base_aus.R")

# Reco forecasts income side
rm(list=ls())
side <- "inc"
source("./script/reco_point.R")

# Reco forecasts expenditure side
rm(list=ls())
side <- "exp"
source("./script/reco_point.R")

# Reo forecasts income + expenditure side
rm(list=ls())
side <- "aus"
source("./script/reco_point.R")

# Reco prob forecasts income side
rm(list=ls())
side <- "inc"
source("./script/reco_prob.R")

# Reco prob forecasts expenditure side
rm(list=ls())
side <- "exp"
source("./script/reco_prob.R")

# Reco prob forecasts income + expenditure side
rm(list=ls())
side <- "aus"
source("./script/reco_prob.R")

# Reco prob scores boot
rm(list=ls())
type <- "boot"
source("./script/prob_scores.R")

# Reco prob scores gaussian
rm(list=ls())
type <- "gaussian"
source("./script/prob_scores.R")