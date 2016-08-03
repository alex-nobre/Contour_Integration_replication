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
library(lsmeans)
# Psychophysics packages
library(quickpsy)
library(MPDiR)
library(psyphy)
# Data report packages
library(knitr)

#----------------------------------------------------------------------------------------
# 1. Import list of files
# Import file names in working directory
sqr1.trialERP.files <- list.files('./Data/Data_BVA', 
                                  pattern = "1_All ROIS Alpha Sqr")
sqr2.trialERP.files <- list.files('./Data/Data_BVA', 
                                  pattern = "2_All ROIS Alpha Sqr")
rand1.trialERP.files <- list.files('./Data/Data_BVA', 
                                   pattern = "1_All ROIS Alpha Rand")
rand2.trialERP.files <- list.files('./Data/Data_BVA', 
                                   pattern = "2_All ROIS Alpha Rand")
# Concatenate all lists in a single list
list_fnames <- c(sqr1.trialERP.files, sqr2.trialERP.files, rand1.trialERP.files, 
                 rand2.trialERP.files)

#------------------------------------Functions-------------------------------------
SEG.LENGTH <- 175 # number of time points in each segment

# 2. Function to extract all time point values from a single segment in BVA export file
eeg.segments <- function(ERP.data) {
  C1 <- ERP.data[,1]
  P1 <- ERP.data[,2]
  N1 <- ERP.data[,3]
  occ <- ERP.data[,4]
  left <- ERP.data[,5]
  right <- ERP.data[,6]
  right.left <- ERP.data[,7]
  N2 <- ERP.data[,8]
  P3 <- ERP.data[,9]
  LP <- ERP.data[,10]
  trial.time.points <- list(C1, P1, N1, occ, left, right, right.left, N2, P3, LP)
}


# 4. Function to extract all single-trial ERPs in a single subject
subject.trialERPs <- function(trialERPs.file) {
  trialERPs.data <- read.delim(paste('./Data/Data_BVA/', trialERPs.file, sep = ""), 
                               sep = " ", dec = ",", header = TRUE)
  number.lines <- length(readLines(paste('./Data/Data_BVA/', trialERPs.file, sep = ""))) - 1
  trial.number <- 0 # keeps track of number of trials already computed in file
  trials.list <- list() #list to store data for subject
  while (number.lines > 0) {
    linespan <- trial.number * SEG.LENGTH + (1:SEG.LENGTH) #lines for current segment
    segment <- trialERPs.data[linespan,]  #select lines
    trial.data <- eeg.segments(segment) #apply trial.ERPs to current segment
    trial.number <- trial.number + 1
    trials.list[[trial.number]] <- trial.data #append to data list
    number.lines <- number.lines - SEG.LENGTH #subtract n of lines read from total
  }
  return(trials.list)
}

#-------------------------Extract time point values and averaging----------------------
# 5. Function to split time point values by ROI for each segment in each subject
split.segments.roi <- function(segment.erps) {
  C1 <- lapply(segment.erps, "[[", 1)
  P1 <- lapply(segment.erps, "[[", 2)
  N1 <- lapply(segment.erps, "[[", 3)
  occ <- lapply(segment.erps, "[[", 4)
  left <- lapply(segment.erps, "[[", 5)
  right <- lapply(segment.erps, "[[", 6)
  right.left <- lapply(segment.erps, "[[", 7)
  N2 <- lapply(segment.erps, "[[", 8)
  P3 <- lapply(segment.erps, "[[", 9)
  LP <- lapply(segment.erps, "[[", 10)
  segments.by.ROIs <- list(C1, P1, N1, occ, left, right, right.left, N2, P3, LP)
}

# 6. Function for averaging
average.segments <- function(subj.segments) {
  segments.by.roi <- split.segments.roi(subj.segments)
  points.averages.rois <- lapply(lapply(segments.by.roi, function(l)do.call(rbind, l)), 
                                 colMeans)
}

# 7. Extract, split and average segments for each subject in each condition
sqr1.averages <- lapply(sqr1.trialERP.files, average.segments)
sqr2.averages <- lapply(sqr2.trialERP.files, average.segments)
rand1.averages <- lapply(rand1.trialERP.files, average.segments)
rand2.averages <- lapply(rand2.trialERP.files, average.segments)



#--------------------------------Extract trial-level ERPs---------------------------
# 4. Apply function to all subjects in a condition
sqr1.trial.dat <- lapply(sqr1.trialERP.files, subject.trialERPs)
sqr2.trial.dat <- lapply(sqr2.trialERP.files, subject.trialERPs)
rand1.trial.dat <- lapply(rand1.trialERP.files, subject.trialERPs)
rand2.trial.dat <- lapply(rand2.trialERP.files, subject.trialERPs)

# 8. Function to get indices for ERP trials from alpha median split in a single subject
alpha.median.index <- function(alpha.trials) {
  low.alpha.trials <- which(alpha.trials < median(alpha.trials))
  high.alpha.trials <- which(alpha.trials > median(alpha.trials))
  return(list(low.alpha.trials, high.alpha.trials))
}

# 9. Apply to alpha files lists
sqr1.alpha.indices <- lapply(sqr1.alpha, alpha.median.index)
sqr2.alpha.indices <- lapply(sqr2.alpha, alpha.median.index)
rand1.alpha.indices <- lapply(rand1.alpha, alpha.median.index)
rand2.alpha.indices <- lapply(rand2.alpha, alpha.median.index)

# 7. Split segments according to alpha median split for each subject
# 7.1. functions to apply to a list, taking as arguments a list (i.e. subject) 
# of numeric vectors (segments) composed of ROI values, and a list of indices, 
# outputting segments split in two lists
# 7.1.1. segments in low-alpha trials
split.low.segments <- function(segments.list, indices.list) {
  low.alpha.segments <- segments.list[indices.list[[1]]]
}
# 7.1.2. segments in high-alpha trials
split.high.segments <- function(segments.list, indices.list) {
  high.alpha.segments <- segments.list[indices.list[[2]]]
}
# 7.3. Apply functions to trials lists
# Square, session 1
low.alpha.segments.sqr1 <- mapply(split.low.segments, sqr1.trial.dat, sqr1.alpha.indices)
high.alpha.segments.sqr1 <- mapply(split.high.segments, sqr1.trial.dat, sqr1.alpha.indices)
# Square, session 2
low.alpha.segments.sqr2 <- mapply(split.low.segments, sqr2.trial.dat, sqr2.alpha.indices)
high.alpha.segments.sqr2 <- mapply(split.high.segments, sqr2.trial.dat, sqr2.alpha.indices)
# Random, session 1
low.alpha.segments.rand1 <- mapply(split.low.segments, rand1.trial.dat, rand1.alpha.indices)
high.alpha.segments.rand1 <- mapply(split.high.segments, rand1.trial.dat, rand1.alpha.indices)
# Random, session 1
low.alpha.segments.rand2 <- mapply(split.low.segments, rand2.trial.dat, rand2.alpha.indices)
high.alpha.segments.rand2 <- mapply(split.high.segments, rand2.trial.dat, rand2.alpha.indices)

# # Split segments by ROI for each subject
# # Square, session 1
# low.alpha.segments.sqr1.roi <- lapply(low.alpha.segments.sqr1, split.segments.roi)
# high.alpha.segments.sqr1.roi <- lapply(high.alpha.segments.sqr1, split.segments.roi)
# # Square, session 2
# low.alpha.segments.sqr2.roi <- lapply(low.alpha.segments.sqr2, split.segments.roi)
# high.alpha.segments.sqr2.roi <- lapply(high.alpha.segments.sqr2, split.segments.roi)
# # Random, session 1
# low.alpha.segments.rand1.roi <- lapply(low.alpha.segments.rand1, split.segments.roi)
# high.alpha.segments.rand1.roi <- lapply(high.alpha.segments.rand1, split.segments.roi)
# # Random, session 1
# low.alpha.segments.rand2.roi <- lapply(low.alpha.segments.rand2, split.segments.roi)
# high.alpha.segments.rand2.roi <- lapply(high.alpha.segments.rand2, split.segments.roi)

# Split segments by ROI for each subject and average
# Square, session 1
low.alpha.segments.sqr1.average <- lapply(low.alpha.segments.sqr1, average.segments)
high.alpha.segments.sqr1.average <- lapply(high.alpha.segments.sqr1, average.segments)
# Square, session 2
low.alpha.segments.sqr2.average <- lapply(low.alpha.segments.sqr2, average.segments)
high.alpha.segments.sqr2.average <- lapply(high.alpha.segments.sqr2, average.segments)
# Random, session 1
low.alpha.segments.rand1.average <- lapply(low.alpha.segments.rand1, average.segments)
high.alpha.segments.rand1.average <- lapply(high.alpha.segments.rand1, average.segments)
# Random, session 1
low.alpha.segments.rand2.average <- lapply(low.alpha.segments.rand2, average.segments)
high.alpha.segments.rand2.average <- lapply(high.alpha.segments.rand2, average.segments)

# 10. Function for baseline correction and mean amplitude computation
trial.ERPs <- function(averaged.segments) {
  baseline.value <- lapply(averaged.segments, function(v) {mean(v[1:25])}) #mean of baseline interval
  averaged.segments <- Map("-", averaged.segments, baseline.value) #baseline correction
  mean.C1 <- mean(averaged.segments[[1]][40:50])
  mean.P1 <- mean(averaged.segments[[2]][51:61])
  mean.N1 <- mean(averaged.segments[[3]][62:75])
  mean.occ_1 <- mean(averaged.segments[[4]][80:90])
  mean.occ_2 <- mean(averaged.segments[[4]][100:110])
  mean.left_1 <- mean(averaged.segments[[5]][80:90])
  mean.left_2 <- mean(averaged.segments[[5]][100:110])
  mean.right_1 <- mean(averaged.segments[[6]][80:90])
  mean.right_2 <- mean(averaged.segments[[6]][100:110])
  mean.right.left_1 <- mean(averaged.segments[[7]][80:90])
  mean.right.left_2 <- mean(averaged.segments[[7]][100:110])
  mean.N2 <- mean(averaged.segments[[8]][95:115])
  mean.P3 <- mean(averaged.segments[[9]][112:162])
  mean.LP <- mean(averaged.segments[[10]][130:160])
  means.trial <- c(mean.C1, mean.P1, mean.N1, mean.occ_1, mean.occ_2, mean.left_1,
                   mean.left_2, mean.right_1, mean.right_2, mean.right.left_1,
                   mean.right.left_2, mean.N2, mean.P3, mean.LP)
}

# Split segments by ROI for each subject and average
# Square, session 1
low.alpha.segments.sqr1.corrected <- lapply(low.alpha.segments.sqr1.average, trial.ERPs)
high.alpha.segments.sqr1.corrected <- lapply(high.alpha.segments.sqr1.average, trial.ERPs)
# Square, session 2
low.alpha.segments.sqr2.corrected <- lapply(low.alpha.segments.sqr2.average, trial.ERPs)
high.alpha.segments.sqr2.corrected <- lapply(high.alpha.segments.sqr2.average, trial.ERPs)
# Random, session 1
low.alpha.segments.rand1.corrected <- lapply(low.alpha.segments.rand1.average, trial.ERPs)
high.alpha.segments.rand1.corrected <- lapply(high.alpha.segments.rand1.average, trial.ERPs)
# Random, session 1
low.alpha.segments.rand2.corrected <- lapply(low.alpha.segments.rand2.average, trial.ERPs)
high.alpha.segments.rand2.corrected <- lapply(high.alpha.segments.rand2.average, trial.ERPs)


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
low.alpha.ROIs.sqr1 <- lapply(low.alpha.segments.sqr1.corrected, split.trialERPs.roi)
high.alpha.ROIs.sqr1 <- lapply(high.alpha.segments.sqr1.corrected, split.trialERPs.roi)
# 8.2.2. Square, session 2
low.alpha.ROIs.sqr2 <- lapply(low.alpha.segments.sqr2.corrected, split.trialERPs.roi)
high.alpha.ROIs.sqr2 <- lapply(high.alpha.segments.sqr2.corrected, split.trialERPs.roi)
# 8.2.3. Random, session 1
low.alpha.ROIs.rand1 <- lapply(low.alpha.segments.rand1.corrected, split.trialERPs.roi)
high.alpha.ROIs.rand1 <- lapply(high.alpha.segments.rand1.corrected, split.trialERPs.roi)
# 8.2.4. Random, session 2
low.alpha.ROIs.rand2 <- lapply(low.alpha.segments.rand2.corrected, split.trialERPs.roi)
high.alpha.ROIs.rand2 <- lapply(high.alpha.segments.rand2.corrected, split.trialERPs.roi)

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




#---------------------------------------test----------------------------------------------
tpoints <- subject.trialERPs("01_1_All ROIS Alpha Sqr .dat")
sqr1.trial.dat <- lapply(sqr1.trialERP.files, subject.trialERPs)
tsplit1 <- split.segments.roi(tpoints) #for 1 subject
tsplit1[[1]] # all 207 segments, 1st ROI, 1 subject
# average segments - should result in 175 values (time points)
lapply(lapply(tsplit1, function(l)do.call(rbind, l)), colMeans)

tbindmeans1 <- lapply(lapply(tsplit1, function(l)do.call(rbind, l)), colMeans)
  
colMeans(do.call(rbind, tsplit1[[1]]))

# Split segments of extracted data
sqr1.segments.by.roi <- lapply(sqr1.trial.dat, split.segments.roi)


