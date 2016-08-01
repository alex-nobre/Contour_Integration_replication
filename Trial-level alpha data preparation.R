# Data import packages
library(xlsx)
# Data manipulation packages
library(outliers)
library(dplyr)
library(tidyr)
library(reshape2)
# Plotting packages
library(lattice)
library(Hmisc)
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

#------------------------------------Functions-------------------------------------
SEG.LENGTH <- 175 # number of time points in each segment

# 2. Function to extract ERPs from a single segment in BVA export file
trial.ERPs <- function(ERP.data) {
  C1 <- ERP.data[40:50,1]
  P1 <- ERP.data[51:61,2]
  N1 <- ERP.data[62:75,3]
  occ.window_1 <- ERP.data[80:90,4]
  occ.window_2 <- ERP.data[100:110,4]
  left.window_1 <- ERP.data[80:90,5]
  left.window_2 <- ERP.data[100:110,5]
  right.window_1 <- ERP.data[80:90,6]
  right.window_2 <- ERP.data[100:110,6]
  right.left.window_1 <- ERP.data[80:90,7]
  right.left.window_2 <- ERP.data[100:110,7]
  N2 <- ERP.data[95:115,8]
  P3 <- ERP.data[112:162,9]
  LP <- ERP.data[130:160,10]
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


# 3. Function to extract all trial ERPs in a single subject
subject.trialERPs <- function(trialERPs.file) {
  trialERPs.data <- read.delim(paste('./Data/Data_BVA/', trialERPs.file, sep = ""), 
                               sep = " ", dec = ",", header = TRUE)
  number.lines <- length(readLines(paste('./Data/Data_BVA/', trialERPs.file, sep = ""))) - 1
  trial.number <- 0 # keeps track of number of trials already computed in file
  trials.list <- list() #list to store data for subject
  while (number.lines > 0) {
    linespan <- trial.number * SEG.LENGTH + (1:SEG.LENGTH) #lines for current segment
    segment <- trialERPs.data[linespan,]  #select lines
    baseline.value <- colMeans(segment[1:25,]) #mean of baseline interval
    segment <- sweep(segment, 2, baseline.value, "-") #baseline correction
    trial.data <- trial.ERPs(segment) #apply function in 2. to current segment
    trial.number <- trial.number + 1
    trials.list[[trial.number]] <- trial.data #append to data list
    number.lines <- number.lines - SEG.LENGTH #subtract n of lines read from total
  }
  return(trials.list)
}

#--------------------------------Extract trial-level ERPs---------------------------
# 4. Apply function to all subjects in a condition
sqr1.trial.dat <- lapply(sqr1.trialERP.files, subject.trialERPs)
sqr2.trial.dat <- lapply(sqr2.trialERP.files, subject.trialERPs)
rand1.trial.dat <- lapply(rand1.trialERP.files, subject.trialERPs)
rand2.trial.dat <- lapply(rand2.trialERP.files, subject.trialERPs)

# 5. Function to get indices for ERP trials from alpha median split in a single subject
alpha.median.index <- function(alpha.trials) {
  low.alpha.trials <- which(alpha.trials < median(alpha.trials))
  high.alpha.trials <- which(alpha.trials > median(alpha.trials))
  return(list(low.alpha.trials, high.alpha.trials))
}
  
# 6. Apply to alpha files lists
sqr1.alpha.indices <- lapply(sqr1.alpha, alpha.median.index)
sqr2.alpha.indices <- lapply(sqr2.alpha, alpha.median.index)
rand1.alpha.indices <- lapply(rand1.alpha, alpha.median.index)
rand2.alpha.indices <- lapply(rand2.alpha, alpha.median.index)

# 7. Split ERPs according to alpha median split for each subject
# 7.1. functions to apply to a list, taking as arguments a list (i.e. subject) 
# of numeric vectors (segments) composed of ROI values, and a list of indices, 
# outputting ERPs split in two lists
# 7.1.1. ERPs in low-alpha trials
split.low.ERPs <- function(segments.list, indices.list) {
  low.alpha.segments <- segments.list[indices.list[[1]]]
}
# 7.1.2. ERPs in high-alpha trials
split.high.ERPs <- function(segments.list, indices.list) {
  high.alpha.segments <- segments.list[indices.list[[2]]]
}
# 7.3. Apply functions to trials lists
# Square, session 1
low.alpha.ERPs.sqr1 <- mapply(split.low.ERPs, sqr1.trial.dat, sqr1.alpha.indices)
high.alpha.ERPs.sqr1 <- mapply(split.high.ERPs, sqr1.trial.dat, sqr1.alpha.indices)
# Square, session 2
low.alpha.ERPs.sqr2 <- mapply(split.low.ERPs, sqr2.trial.dat, sqr2.alpha.indices)
high.alpha.ERPs.sqr2 <- mapply(split.high.ERPs, sqr2.trial.dat, sqr2.alpha.indices)
# Random, session 1
low.alpha.ERPs.rand1 <- mapply(split.low.ERPs, rand1.trial.dat, rand1.alpha.indices)
high.alpha.ERPs.rand1 <- mapply(split.high.ERPs, rand1.trial.dat, rand1.alpha.indices)
# Random, session 1
low.alpha.ERPs.rand2 <- mapply(split.low.ERPs, rand2.trial.dat, rand2.alpha.indices)
high.alpha.ERPs.rand2 <- mapply(split.high.ERPs, rand2.trial.dat, rand2.alpha.indices)


# 8. Split ERPs by ROI for each subject
# 8.1. Function to get ERPs by ROIs for each segment
split.trialERPs.roi <- function(segment.erps) {
  C1 <- unlist(lapply(segment.erps, "[[", 1))
  P1 <- unlist(lapply(segment.erps, "[[", 2))
  N1 <- unlist(lapply(segment.erps, "[[", 3))
  occ.window_1 <- unlist(lapply(segment.erps, "[[", 4))
  occ.window_2 <- unlist(lapply(segment.erps, "[[", 5))
  left.window_1 <- unlist(lapply(segment.erps, "[[", 6))
  left.window_2 <- unlist(lapply(segment.erps, "[[", 7))
  right.window_1 <- unlist(lapply(segment.erps, "[[", 8))
  right.window_2 <- unlist(lapply(segment.erps, "[[", 9))
  right.left.window_1 <- unlist(lapply(segment.erps, "[[", 10))
  right.left.window_2 <- unlist(lapply(segment.erps, "[[",11))
  N2 <- unlist(lapply(segment.erps, "[[", 12))
  P3 <- unlist(lapply(segment.erps, "[[", 13))
  LP <- unlist(lapply(segment.erps, "[[", 14))
  trials.by.ROIs <- list(C1, P1, N1, occ.window_1, occ.window_2, left.window_1, 
                         left.window_2, right.window_1, right.window_2, 
                         right.left.window_1, right.left.window_2, N2, P3, LP)
}

# 8.2. Apply function to list of all subjects, by configuration, session and alpha power
# 8.2.1. Square, session 1
low.alpha.ROIs.sqr1 <- lapply(low.alpha.ERPs.sqr1, split.trialERPs.roi)
high.alpha.ROIs.sqr1 <- lapply(high.alpha.ERPs.sqr1, split.trialERPs.roi)
# 8.2.2. Square, session 2
low.alpha.ROIs.sqr2 <- lapply(low.alpha.ERPs.sqr2, split.trialERPs.roi)
high.alpha.ROIs.sqr2 <- lapply(high.alpha.ERPs.sqr2, split.trialERPs.roi)
# 8.2.3. Random, session 1
low.alpha.ROIs.rand1 <- lapply(low.alpha.ERPs.rand1, split.trialERPs.roi)
high.alpha.ROIs.rand1 <- lapply(high.alpha.ERPs.rand1, split.trialERPs.roi)
# 8.2.4. Random, session 2
low.alpha.ROIs.rand2 <- lapply(low.alpha.ERPs.rand2, split.trialERPs.roi)
high.alpha.ROIs.rand2 <- lapply(high.alpha.ERPs.rand2, split.trialERPs.roi)

# 9. Compute means
# 8.2.1. Square, session 1
low.alpha.ROIs.sqr1.means <- lapply(low.alpha.ROIs.sqr1, function (x) {lapply(x, mean)})
high.alpha.ROIs.sqr1.means <- lapply(high.alpha.ROIs.sqr1, function (x) {lapply(x, mean)})
# 8.2.2. Square, session 2
low.alpha.ROIs.sqr2.means <- lapply(low.alpha.ROIs.sqr2, function (x) {lapply(x, mean)})
high.alpha.ROIs.sqr2.means <- lapply(high.alpha.ROIs.sqr2, function (x) {lapply(x, mean)})
# 8.2.3. Random, session 1
low.alpha.ROIs.rand1.means <- lapply(low.alpha.ROIs.rand1, function (x) {lapply(x, mean)})
high.alpha.ROIs.rand1.means <- lapply(high.alpha.ROIs.rand1, function (x) {lapply(x, mean)})
# 8.2.4. Random, session 2
low.alpha.ROIs.rand2.means <- lapply(low.alpha.ROIs.rand2, function (x) {lapply(x, mean)})
high.alpha.ROIs.rand2.means <- lapply(high.alpha.ROIs.rand2, function (x) {lapply(x, mean)})

# 10. Append to data frame with average ERPs
rep_data2 <- rep_data
# 10.1 Low alpha
# 10.1.1. Square, session 1
rep_data2$low.alpha.C1.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 1))
rep_data2$low.alpha.P1.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 2))
rep_data2$low.alpha.N1.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 3))
rep_data2$low.alpha.occ.nd1.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 4))
rep_data2$low.alpha.occ.nd2.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 5))
rep_data2$low.alpha.left.nd1.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 6))
rep_data2$low.alpha.left.nd2.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 7))
rep_data2$low.alpha.right.nd1.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 8))
rep_data2$low.alpha.right.nd2.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 9))
rep_data2$low.alpha.RL.nd1.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 10))
rep_data2$low.alpha.RL.nd2.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 11))
rep_data2$low.alpha.N2.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 12))
rep_data2$low.alpha.P3.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 13))
rep_data2$low.alpha.LP.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 14))
# 10.1.2. Square, session 2
rep_data2$low.alpha.C1.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 1))
rep_data2$low.alpha.P1.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 2))
rep_data2$low.alpha.N1.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 3))
rep_data2$low.alpha.occ.nd1.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 4))
rep_data2$low.alpha.occ.nd2.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 5))
rep_data2$low.alpha.left.nd1.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 6))
rep_data2$low.alpha.left.nd2.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 7))
rep_data2$low.alpha.right.nd1.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 8))
rep_data2$low.alpha.right.nd2.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 9))
rep_data2$low.alpha.RL.nd1.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 10))
rep_data2$low.alpha.RL.nd2.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 11))
rep_data2$low.alpha.N2.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 12))
rep_data2$low.alpha.P3.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 13))
rep_data2$low.alpha.LP.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 14))
# 10.1.3. Random, session 1
rep_data2$low.alpha.C1.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 1))
rep_data2$low.alpha.P1.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 2))
rep_data2$low.alpha.N1.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 3))
rep_data2$low.alpha.occ.nd1.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 4))
rep_data2$low.alpha.occ.nd2.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 5))
rep_data2$low.alpha.left.nd1.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 6))
rep_data2$low.alpha.left.nd2.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 7))
rep_data2$low.alpha.right.nd1.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 8))
rep_data2$low.alpha.right.nd2.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 9))
rep_data2$low.alpha.RL.nd1.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 10))
rep_data2$low.alpha.RL.nd2.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 11))
rep_data2$low.alpha.N2.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 12))
rep_data2$low.alpha.P3.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 13))
rep_data2$low.alpha.LP.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 14))
# 10.1.4. Random, session 2
rep_data2$low.alpha.C1.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 1))
rep_data2$low.alpha.P1.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 2))
rep_data2$low.alpha.N1.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 3))
rep_data2$low.alpha.occ.nd1.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 4))
rep_data2$low.alpha.occ.nd2.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 5))
rep_data2$low.alpha.left.nd1.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 6))
rep_data2$low.alpha.left.nd2.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 7))
rep_data2$low.alpha.right.nd1.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 8))
rep_data2$low.alpha.right.nd2.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 9))
rep_data2$low.alpha.RL.nd1.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 10))
rep_data2$low.alpha.RL.nd2.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 11))
rep_data2$low.alpha.N2.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 12))
rep_data2$low.alpha.P3.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 13))
rep_data2$low.alpha.LP.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 14))

# 10.2. High alpha
# 10.2.1. Square, session 1
rep_data2$high.alpha.C1.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 1))
rep_data2$high.alpha.P1.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 2))
rep_data2$high.alpha.N1.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 3))
rep_data2$high.alpha.occ.nd1.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 4))
rep_data2$high.alpha.occ.nd2.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 5))
rep_data2$high.alpha.left.nd1.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 6))
rep_data2$high.alpha.left.nd2.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 7))
rep_data2$high.alpha.right.nd1.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 8))
rep_data2$high.alpha.right.nd2.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 9))
rep_data2$high.alpha.RL.nd1.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 10))
rep_data2$high.alpha.RL.nd2.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 11))
rep_data2$high.alpha.N2.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 12))
rep_data2$high.alpha.P3.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 13))
rep_data2$high.alpha.LP.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 14))
# 10.2.2. Square, session 2
rep_data2$high.alpha.C1.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 1))
rep_data2$high.alpha.P1.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 2))
rep_data2$high.alpha.N1.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 3))
rep_data2$high.alpha.occ.nd1.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 4))
rep_data2$high.alpha.occ.nd2.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 5))
rep_data2$high.alpha.left.nd1.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 6))
rep_data2$high.alpha.left.nd2.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 7))
rep_data2$high.alpha.right.nd1.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 8))
rep_data2$high.alpha.right.nd2.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 9))
rep_data2$high.alpha.RL.nd1.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 10))
rep_data2$high.alpha.RL.nd2.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 11))
rep_data2$high.alpha.N2.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 12))
rep_data2$high.alpha.P3.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 13))
rep_data2$high.alpha.LP.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 14))
# 10.2.3. Random, session 1
rep_data2$high.alpha.C1.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 1))
rep_data2$high.alpha.P1.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 2))
rep_data2$high.alpha.N1.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 3))
rep_data2$high.alpha.occ.nd1.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 4))
rep_data2$high.alpha.occ.nd2.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 5))
rep_data2$high.alpha.left.nd1.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 6))
rep_data2$high.alpha.left.nd2.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 7))
rep_data2$high.alpha.right.nd1.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 8))
rep_data2$high.alpha.right.nd2.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 9))
rep_data2$high.alpha.RL.nd1.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 10))
rep_data2$high.alpha.RL.nd2.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 11))
rep_data2$high.alpha.N2.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 12))
rep_data2$high.alpha.P3.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 13))
rep_data2$high.alpha.LP.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 14))
# 10.2.4. Random, session 2
rep_data2$high.alpha.C1.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 1))
rep_data2$high.alpha.P1.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 2))
rep_data2$high.alpha.N1.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 3))
rep_data2$high.alpha.occ.nd1.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 4))
rep_data2$high.alpha.occ.nd2.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 5))
rep_data2$high.alpha.left.nd1.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 6))
rep_data2$high.alpha.left.nd2.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 7))
rep_data2$high.alpha.right.nd1.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 8))
rep_data2$high.alpha.right.nd2.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 9))
rep_data2$high.alpha.RL.nd1.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 10))
rep_data2$high.alpha.RL.nd2.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 11))
rep_data2$high.alpha.N2.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 12))
rep_data2$high.alpha.P3.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 13))
rep_data2$high.alpha.LP.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 14))

# 11. Create new data frame for analysis with alpha as factor
rep_data2 <- rep_data2[,-c(4:55)]

rep_data_alpha <- reshape(rep_data2, varying = 4:ncol(rep_data2), sep = "_", 
                         direction = "long", 
                         new.row.names = NULL)
rep_data_alpha[,ncol(rep_data_alpha)]<- NULL
names(rep_data_alpha)[names(rep_data_alpha) == "time"] <- "session"
rep_data_alpha2 <- rep_data_alpha %>%
  unite(sqr,contains("sqr")) %>%
  unite(rand,contains("rand")) %>%
  gather(configuration,values,sqr:rand) %>%
  separate(values,c("low.alpha.C1","low.alpha.P1","low.alpha.N1","low.alpha.occ.nd1",
                    "low.alpha.occ.nd2","low.alpha.left.nd1","low.alpha.left.nd2",
                    "low.alpha.right.nd1","low.alpha.right.nd2","low.alpha.RL.nd1",
                    "low.alpha.RL.nd2","low.alpha.N2","low.alpha.P3","low.alpha.LP",
                    "high.alpha.C1","high.alpha.P1","high.alpha.N1","high.alpha.occ.nd1",
                    "high.alpha.occ.nd2","high.alpha.left.nd1","high.alpha.left.nd2",
                    "high.alpha.right.nd1","high.alpha.right.nd2","high.alpha.RL.nd1",
                    "high.alpha.RL.nd2","high.alpha.N2","high.alpha.P3","high.alpha.LP"),
           sep = "_",
           convert = TRUE)

rep_data_alpha3 <- rep_data_alpha2 %>%
  unite(low.alpha,contains("low.alpha")) %>%
  unite(high.alpha,contains("high.alpha")) %>%
  gather(alpha.power,values,low.alpha:high.alpha) %>%
  separate(values,c("C1","P1","N1","occ.nd1","occ.nd2","left.nd1","left.nd2",
                    "right.nd1","right.nd2","RL.nd1","RL.nd2","N2","P3","LP"),
           sep = "_",
           convert = TRUE)

rep_data_alpha3$group <- factor(rep_data_alpha3$group)
rep_data_alpha3$group.original <- factor(rep_data_alpha3$group.original)
rep_data_alpha3$session <- factor(rep_data_alpha3$session)
rep_data_alpha3$configuration <- factor(rep_data_alpha3$configuration)
rep_data_alpha3$alpha.power <- factor(rep_data_alpha3$alpha.power)
