# Data import packages
library(xlsx)
# Data manipulation packages
library(outliers)
library(dplyr)
# Plotting packages
library(lattice)
library(ggplot2)
library(gridExtra)
library(GGally)
# Analysis packages
library(polycor)
library(nlme)
library(gmodels)
library(lme4)
library(ggm)
library(pastecs)
library(car)
library(effsize)
# Psychophysics packages
library(quickpsy)
library(MPDiR)
library(psyphy)
# Data report packages
library(knitr)

#----------------------------------------------------------------------------------------
# 1. Import list of files
# Import file names in working directory
sqr1.trialERP.files <- list.files('./Data/Data_BVA', pattern = "1_All ROIS Alpha Sqr")
sqr2.trialERP.files <- list.files('./Data/Data_BVA', pattern = "2_All ROIS Alpha Sqr")
rand1.trialERP.files <- list.files('./Data/Data_BVA', pattern = "1_All ROIS Alpha Rand")
rand2.trialERP.files <- list.files('./Data/Data_BVA', pattern = "2_All ROIS Alpha Rand")
# Concatenate all lists in a single list
list_fnames <- c(sqr1.trialERP.files, sqr2.trialERP.files, rand1.trialERP.files, 
                 rand2.trialERP.files)

# 2. Function to extract trial ERPs
extract.trialERPs <- function(trialERPs.file) {
  trialERPs.data <- read.delim(paste('./Data/Data_BVA/', trialERPs.file, sep = ""), 
                               sep = "\t", dec = ",", header = TRUE)
  trial.data <- trialERPs.data[1:150,]
}

# function to extract all trials





