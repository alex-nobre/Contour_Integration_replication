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

# 2. function to extract a single trial
trial.ERPs <- function(ERP.data) {
  C1 <- ERP.data[15:25,1]
  P1 <- ERP.data[26:36,2]
  N1 <- ERP.data[37:50,3]
  occ.window_1 <- ERP.data[55:65,4]
  occ.window_2 <- ERP.data[75:85,4]
  left.window_1 <- ERP.data[55:65,5]
  left.window_2 <- ERP.data[75:85,5]
  right.window_1 <- ERP.data[55:65,6]
  right.window_2 <- ERP.data[75:85,6]
  right.left.window_1 <- ERP.data[55:65,7]
  right.left.window_2 <- ERP.data[75:85,7]
  N2 <- ERP.data[70:90,8]
  P3 <- ERP.data[87:137,9]
  LP <- ERP.data[105:135,10]
  mean.C1 <- mean(C1)
  mean.P1 <- mean(P1)
  mean.N1 <- mean(N1)
  mean.occ_1 <- mean(occ.window_1)
  mean.occ_2 <- mean(occ.window_2)
  mean.left_1 <- mean(left.window_1)
  mean.left_2 <- mean(left.window_2)
  mean.right_1 <- mean(right.window_1)
  mean.right_2 <- mean(right.window_2)
  mean.right.left_1 <- mean(right.left.window_1)
  mean.right.left_2 <- mean(right.left.window_2)
  mean.N2 <- mean(N2)
  mean.P3 <- mean(P3)
  mean.LP <- mean(LP)
  means.trial <- c(mean.C1, mean.P1, mean.N1, mean.occ_1, mean.occ_2, mean.left_1, 
                  mean.left_2, mean.right_1, mean.right_2, mean.right.left_1, 
                  mean.right.left_2, mean.N2, mean.P3, mean.LP)
}


# 3. Function to extract all trial ERPs in a subject
subject.trialERPs <- function(trialERPs.file) {
  trialERPs.data <- read.delim(paste('./Data/Data_BVA/', trialERPs.file, sep = ""), 
                               sep = " ", dec = ",", header = TRUE)
  number.lines <- length(readLines(paste('./Data/Data_BVA/', trialERPs.file, sep = ""))) - 1
  trial.number <- 0 # keeps track of number of trials already computed in file
  trials.list <- list() #list to store data for subject
  while (number.lines > 0) {
    linespan <- trial.number * 150 + (1:150)
    trial.data <- trial.ERPs(trialERPs.data[linespan,])
    trial.number <- trial.number + 1
    trials.list[[trial.number]] <- trial.data
    number.lines <- number.lines - 150
  }
  return(trials.list)
}

# 4. Apply function to all subjects in a condition
sqr1.trial.dat <- lapply(sqr1.trialERP.files, subject.trialERPs)
sqr2.trial.dat <- lapply(sqr2.trialERP.files, subject.trialERPs)
rand1.trial.dat <- lapply(rand1.trialERP.files, subject.trialERPs)
rand1.trial.dat <- lapply(rand2.trialERP.files, subject.trialERPs)




