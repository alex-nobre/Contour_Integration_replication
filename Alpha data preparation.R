# Data import packages
library(xlsx)
# Data manipulation packages
library(outliers)
library(dplyr)
library(tidyr)
library(reshape2)
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
library(multcomp)
# Psychophysics packages
library(quickpsy)
library(MPDiR)
library(psyphy)
# Data report packages
library(knitr)

#------------------------------------------------------------------------------------------
# 1. Import list of files
# Import file names in working directory
sqr1.alpha.files <- list.files('./Data/Data_BVA', 
                               pattern = "1_Alpha ROI Sqr_FFTBandExport_pre")
sqr2.alpha.files <- list.files('./Data/Data_BVA', 
                               pattern = "2_Alpha ROI Sqr_FFTBandExport_pre")
rand1.alpha.files <- list.files('./Data/Data_BVA', 
                                pattern = "1_Alpha ROI Rand_FFTBandExport_pre")
rand2.alpha.files <- list.files('./Data/Data_BVA', 
                                pattern = "2_Alpha ROI Rand_FFTBandExport_pre")
# Concatenate all lists in a single list
list_fnames <- c(sqr1.alpha.files, sqr2.alpha.files, rand1.alpha.files, 
                 rand2.alpha.files)

# 2. Function to extract alpha power
extract.alpha <- function(alpha.file) {
  alpha.data <- read.delim(paste('./Data/Data_BVA/', alpha.file, sep = ""), sep = "\t", 
                           dec = ",", header = TRUE)
  alpha.power <- alpha.data[,2]
}

# 3. Extract alpha power for each segment in each subject
sqr1.alpha <- lapply(sqr1.alpha.files, extract.alpha)
sqr2.alpha <- lapply(sqr2.alpha.files, extract.alpha)
rand1.alpha <- lapply(rand1.alpha.files, extract.alpha)
rand2.alpha <- lapply(rand2.alpha.files, extract.alpha)

# 4. Compute mean power for each subject
sqr1.mean.alpha <- sapply(sqr1.alpha, mean)
sqr2.mean.alpha <- sapply(sqr2.alpha, mean)
rand1.mean.alpha <- sapply(rand1.alpha, mean)
rand2.mean.alpha <- sapply(rand2.alpha, mean)