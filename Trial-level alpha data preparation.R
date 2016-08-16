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


# 3. Function to extract all single-trial ERPs in a single subject
subject.trialERPs <- function(trialERPs.file) {
  trialERPs.data <- read.delim(paste('./Data/Data_BVA/', trialERPs.file, sep = ""), 
                               sep = " ", dec = ",", header = TRUE)
  number.lines <- length(readLines(paste('./Data/Data_BVA/', trialERPs.file, sep = ""))) - 1
  trial.number <- 0 # keeps track of number of trials already computed in file
  trials.list <- list() #list to store data for subject
  while (number.lines > 0) {
    linespan <- trial.number * SEG.LENGTH + (1:SEG.LENGTH) #lines for current segment
    segment <- trialERPs.data[linespan,]  #select lines
    trial.data <- eeg.segments(segment) #apply eeg.segments to current segment
    trial.number <- trial.number + 1
    trials.list[[trial.number]] <- trial.data #append to data list
    number.lines <- number.lines - SEG.LENGTH #subtract n of lines read from total
  }
  return(trials.list)
}

#-------------------------Extract time point values and averaging----------------------
# 4. Function to split time point values by ROI for each segment in each subject
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

# 5. Function for averaging at the subject level
average.segments <- function(subj.segments) {
  segments.by.roi <- split.segments.roi(subj.segments)
  points.averages.rois <- lapply(lapply(segments.by.roi, function(l)do.call(rbind, l)), 
                                 colMeans)
}

#--------------------------------Extract trial-level ERPs---------------------------
# 6. Apply segment extraction function to all subjects in a condition
sqr1.trial.dat <- lapply(sqr1.trialERP.files, subject.trialERPs)
sqr2.trial.dat <- lapply(sqr2.trialERP.files, subject.trialERPs)
rand1.trial.dat <- lapply(rand1.trialERP.files, subject.trialERPs)
rand2.trial.dat <- lapply(rand2.trialERP.files, subject.trialERPs)

# 7. Function to get indices for ERP trials from alpha median split in a single subject
alpha.median.index <- function(alpha.trials) {
  low.alpha.trials <- which(alpha.trials < median(alpha.trials))
  high.alpha.trials <- which(alpha.trials > median(alpha.trials))
  return(list(low.alpha.trials, high.alpha.trials))
}

# 8. Apply to alpha files lists
sqr1.alpha.indices <- lapply(sqr1.alpha, alpha.median.index)
sqr2.alpha.indices <- lapply(sqr2.alpha, alpha.median.index)
rand1.alpha.indices <- lapply(rand1.alpha, alpha.median.index)
rand2.alpha.indices <- lapply(rand2.alpha, alpha.median.index)

# 9. Split segments according to alpha median split for each subject
# 9.1. functions to apply to a list, taking as arguments a list (i.e. subject) 
# of numeric vectors (segments) composed of ROI values, and a list of indices, 
# outputting segments split in two lists
# 9.1.1. segments in low-alpha trials
split.low.segments <- function(segments.list, indices.list) {
  low.alpha.segments <- segments.list[indices.list[[1]]]
}
# 9.1.2. segments in high-alpha trials
split.high.segments <- function(segments.list, indices.list) {
  high.alpha.segments <- segments.list[indices.list[[2]]]
}
# 9.3. Apply functions to trials lists
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


# 10. Split segments by ROI for each subject and average
# 10.1. Square, session 1
low.alpha.segments.sqr1.average <- lapply(low.alpha.segments.sqr1, average.segments)
high.alpha.segments.sqr1.average <- lapply(high.alpha.segments.sqr1, average.segments)
# 10.2. Square, session 2
low.alpha.segments.sqr2.average <- lapply(low.alpha.segments.sqr2, average.segments)
high.alpha.segments.sqr2.average <- lapply(high.alpha.segments.sqr2, average.segments)
# 10.3. Random, session 1
low.alpha.segments.rand1.average <- lapply(low.alpha.segments.rand1, average.segments)
high.alpha.segments.rand1.average <- lapply(high.alpha.segments.rand1, average.segments)
# 10.4. Random, session 1
low.alpha.segments.rand2.average <- lapply(low.alpha.segments.rand2, average.segments)
high.alpha.segments.rand2.average <- lapply(high.alpha.segments.rand2, average.segments)

# 11. Function for baseline correction and mean amplitude computation
baseline.correction <- function(averaged.segments) {
  baseline.value <- lapply(averaged.segments, function(v) {mean(v[1:25])}) #mean of baseline interval
  averaged.segments <- Map("-", averaged.segments, 
                           baseline.value) #baseline correction
}

# 12. Baseline correction by session, configuration and alpha power
# 12.1. Square, session 1
low.alpha.segments.sqr1.corrected <- lapply(low.alpha.segments.sqr1.average, 
                                            baseline.correction)
high.alpha.segments.sqr1.corrected <- lapply(high.alpha.segments.sqr1.average, 
                                             baseline.correction)
# 12.2. Square, session 2
low.alpha.segments.sqr2.corrected <- lapply(low.alpha.segments.sqr2.average, 
                                            baseline.correction)
high.alpha.segments.sqr2.corrected <- lapply(high.alpha.segments.sqr2.average, 
                                             baseline.correction)
# 12.3.Random, session 1
low.alpha.segments.rand1.corrected <- lapply(low.alpha.segments.rand1.average, 
                                             baseline.correction)
high.alpha.segments.rand1.corrected <- lapply(high.alpha.segments.rand1.average, 
                                              baseline.correction)
# 12.4. Random, session 1
low.alpha.segments.rand2.corrected <- lapply(low.alpha.segments.rand2.average, 
                                             baseline.correction)
high.alpha.segments.rand2.corrected <- lapply(high.alpha.segments.rand2.average, 
                                              baseline.correction)


# 13. Function to get ERPs Group ERPs by ROI for across subjects
split.ROIs <- function(corrected.segments) {
  C1 <- lapply(corrected.segments, "[[", 1)
  P1 <- lapply(corrected.segments, "[[", 2)
  N1 <- lapply(corrected.segments, "[[", 3)
  occ <- lapply(corrected.segments, "[[", 4)
  left <- lapply(corrected.segments, "[[", 5)
  right <- lapply(corrected.segments, "[[", 6)
  right.left <- lapply(corrected.segments, "[[", 7)
  N2 <- lapply(corrected.segments, "[[", 8)
  P3 <- lapply(corrected.segments, "[[", 9)
  LP <- lapply(corrected.segments, "[[", 10)
  trials.by.ROIs <- list(C1, P1, N1, occ, left,
                         right, right.left, N2, P3, LP)
}

# 14. Join segments by ROI
# 14.1. Square, session 1
low.alpha.ROIs.sqr1 <- split.ROIs(low.alpha.segments.sqr1.corrected)
high.alpha.ROIs.sqr1 <- split.ROIs(high.alpha.segments.sqr1.corrected)
# 14.2. Square, session 2
low.alpha.ROIs.sqr2 <- split.ROIs(low.alpha.segments.sqr2.corrected)
high.alpha.ROIs.sqr2 <- split.ROIs(high.alpha.segments.sqr2.corrected)
# 14.3. Random, session 1
low.alpha.ROIs.rand1 <- split.ROIs(low.alpha.segments.rand1.corrected)
high.alpha.ROIs.rand1 <- split.ROIs(high.alpha.segments.rand1.corrected)
# 14.4. Random, session 1
low.alpha.ROIs.rand2 <- split.ROIs(low.alpha.segments.rand2.corrected)
high.alpha.ROIs.rand2 <- split.ROIs(high.alpha.segments.rand2.corrected)

# 15. Function to compute grand average
compute.grand.average <- function(subject.averages) {
  grand.average <- colMeans(matrix(unlist(subject.averages), 
                                   nrow = length(subject.averages), byrow = T))
}

# 16. Compute grand averages by condition
# 16.1. Square, session 1
low.alpha.grand.averages.sqr1 <- lapply(low.alpha.ROIs.sqr1, compute.grand.average)
high.alpha.grand.averages.sqr1 <- lapply(high.alpha.ROIs.sqr1, compute.grand.average)
# 16.2. Square, session 2
low.alpha.grand.averages.sqr2 <- lapply(low.alpha.ROIs.sqr2, compute.grand.average)
high.alpha.grand.averages.sqr2 <- lapply(high.alpha.ROIs.sqr2, compute.grand.average)
# 16.3. Random, session 1
low.alpha.grand.averages.rand1 <- lapply(low.alpha.ROIs.rand1, compute.grand.average)
high.alpha.grand.averages.rand1 <- lapply(high.alpha.ROIs.rand1, compute.grand.average)
# 16.4. Random, session 2
low.alpha.grand.averages.rand2 <- lapply(low.alpha.ROIs.rand2, compute.grand.average)
high.alpha.grand.averages.rand2 <- lapply(high.alpha.ROIs.rand2, compute.grand.average)


# 17. Function to extract ERPs from subjects' data
extract.ERPs <- function(ROIs.by.subject) {
  C1 <- ROIs.by.subject[[1]]
  P1 <- ROIs.by.subject[[2]]
  N1 <- ROIs.by.subject[[3]]
  occ.window_1 <- ROIs.by.subject[[4]]
  occ.window_2 <- ROIs.by.subject[[4]]
  left.window_1 <- ROIs.by.subject[[5]]
  left.window_2 <- ROIs.by.subject[[5]]
  right.window_1 <- ROIs.by.subject[[6]]
  right.window_2 <- ROIs.by.subject[[6]]
  right.left.window_1 <- ROIs.by.subject[[7]]
  right.left.window_2 <- ROIs.by.subject[[7]]
  N2 <- ROIs.by.subject[[8]]
  P3 <- ROIs.by.subject[[9]]
  LP <- ROIs.by.subject[[10]]
  C1 <- mean(C1[40:50])
  P1 <- mean(P1[55:60])
  N1 <- mean(N1[62:75])
  occ.window_1 <- mean(occ.window_1[82:95])
  occ.window_2 <- mean(occ.window_2[100:110])
  left.window_1 <- mean(left.window_1[80:90])
  left.window_2 <- mean(left.window_2[100:110])
  right.window_1 <- mean(right.window_1[82:95])
  right.window_2 <- mean(right.window_2[100:110])
  right.left.window_1 <- mean(right.left.window_1[80:90])
  right.left.window_2 <- mean(right.left.window_2[100:110])
  N2 <- mean(N2[95:115])
  P3 <- mean(P3[112:162])
  LP <- mean(LP[130:160])
  ERPs.means <- list(C1, P1, N1, occ.window_1, occ.window_2, left.window_1, 
                     left.window_2, right.window_1, right.window_2, 
                     right.left.window_1, right.left.window_2, N2, P3, LP)
}



# 18. Compute ERP mean amplitudes by session, configuration and alpha power
# 18.1. Square, session 1
low.alpha.ROIs.sqr1.means <- lapply(low.alpha.segments.sqr1.corrected, 
                                    extract.ERPs)
high.alpha.ROIs.sqr1.means <- lapply(high.alpha.segments.sqr1.corrected, 
                                     extract.ERPs)
# 18.2. Square, session 2
low.alpha.ROIs.sqr2.means <- lapply(low.alpha.segments.sqr2.corrected, 
                                    extract.ERPs)
high.alpha.ROIs.sqr2.means <- lapply(high.alpha.segments.sqr2.corrected, 
                                     extract.ERPs)
# 18.3. Random, session 1
low.alpha.ROIs.rand1.means <- lapply(low.alpha.segments.rand1.corrected, 
                                     extract.ERPs)
high.alpha.ROIs.rand1.means <- lapply(high.alpha.segments.rand1.corrected, 
                                      extract.ERPs)
# 18.4. Random, session 1
low.alpha.ROIs.rand2.means <- lapply(low.alpha.segments.rand2.corrected, 
                                     extract.ERPs)
high.alpha.ROIs.rand2.means <- lapply(high.alpha.segments.rand2.corrected, 
                                      extract.ERPs)

#---------------------------------------test----------------------------------------------
# tpoints <- subject.trialERPs("01_1_All ROIS Alpha Sqr .dat")
# sqr1.trial.dat <- lapply(sqr1.trialERP.files, subject.trialERPs)
# tsplit1 <- split.segments.roi(tpoints) #for 1 subject
# tsplit1[[1]] # all 207 segments, 1st ROI, 1 subject
# # average segments - should result in 175 values (time points)
# lapply(lapply(tsplit1, function(l)do.call(rbind, l)), colMeans)
# 
# tbindmeans1 <- lapply(lapply(tsplit1, function(l)do.call(rbind, l)), colMeans)
#   
# colMeans(do.call(rbind, tsplit1[[1]]))
# 
# # Split segments of extracted data
# sqr1.segments.by.roi <- lapply(sqr1.trial.dat, split.segments.roi)
# 
# dev.off()
