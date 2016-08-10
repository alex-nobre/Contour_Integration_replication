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
library(psyphy)
# Data report packages
library(knitr)

#---------------------------------------------------------------------------------------------
# 0. Read files with AR excluded trials
AR.trials.rand <- function(AR.file) {
  excluded.trials <- read.table(AR.file, 
                          header = TRUE, sep = "\t", row.names = NULL)
}

# 1.1. Function to trials with target
# 1.1.1. All conditions
extract.all.trials <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12, row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  subj.data
}

# 1.2. Extract trials for eeg analysis
# 1.2.1. Random
eeg.trials.rand <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12)
  colnames(subj.data)[5] <- "target.presence"
  non.target.trials.rand <- subset(subj.data, target.presence == 0 & Configuration == 0)
  row.names(non.target.trials.rand) <- seq_len(nrow(non.target.trials.rand))
  non.target.trials.rand
}


# Bin trials by target
bin.trials <- function(all.trials) {
  all.trials$bin <- NA
  bin.label <- 0
  for (i in 1:nrow(all.trials)){
    if (all.trials[i,"target.presence"] == 0) {
      all.trials[i,"bin"] <- bin.label
    } else if (all.trials[i,"target.presence"] == 1) {
      bin.label <- bin.label + 1
      all.trials[i,"bin"] <- bin.label
    }
  }
  # create new column
  # start label "bin" with 0 (bin0)
  # run through target presence column
  # if value is 1
  #   add 1 to label (bin1, bin2 etc.)
  #   add label to new column (so that this row can used later to select the bin by its threshold)
  # if value is 0:
  #   add label to new column
}

# Retrieve  intensisites - but should already have been retrieved in behavioral data prep!!!!!

# median of intensities
intensities.1[[1]]
median.intensity <- median(intensities.1[[1]])

# Create bin groups - substitute data frame name for object name in function!!
low.threshold.bins <- binned.data[which(questionnaire.ERPs$threshold.1 < 
                                                   median(questionnaire.ERPs$threshold.1)),]$bin
high.threshold.bins <- binned.data[which(questionnaire.ERPs$threshold.1 > 
                                                    median(questionnaire.ERPs$threshold.1)),]$bin


# median split
median.split.by.int <- function(binned.trials) {
  binned.trials$bin.group <- NA
  
  # create column for bin group
  # select rows with intensity < median.intensity
  # retrieve bins from those rows
  # add group value in bin group column for rows with those bins
  # repeat for rows with intensity > median.intensity
}

# # 3.1. Compute medians

# 
# # 3.2. Create factor for median split group
# questionnaire.ERPs$split.group <- numeric(nrow(questionnaire.ERPs))
# questionnaire.ERPs[which(questionnaire.ERPs$Subject %in% 
#                            low.threshold.group),]$split.group <- 'low.thresh'
# questionnaire.ERPs[which(questionnaire.ERPs$Subject %in% 
#                            high.threshold.group),]$split.group <- 'high.thresh'
# questionnaire.ERPs$split.group <- factor(questionnaire.ERPs$split.group)

# Select random and no target/no response trials
subset.no.target.rand <- function(median.split.data) {
  no.target.trials.rand <- subset(median.split.data, target.presence == 0 & Configuration == 0)
}
# Select square and no target/no response trials
subset.no.target.sqr <- function(median.split.data) {
  no.target.trials.sqr <- subset(median.split.data, target.presence == 0 & Configuration == 1)
}

# These files should have the same number of trials as the BVA segments, so the AR exclusion
# can be applied directly over them (check)
remove.AR.trials <- function(trials.subset) {
  trials.subset <- trials.subset
}




# test------------------------------------------------------------------------
# Remove AR trials
teegt <- eeg.trials.rand("Implicit segregation IB_01_1.txt")
teegt <- teegt[-tartrials]
row.names(teegt) <- seq_len(nrow(teegt))
teegt <- teegt[-c(1, 89, 98, 174, 263),]

# a <- list(c(19, 44, 81, 89, 151, 166, 180, 194), c(36, 74, 82, 107, 110, 119, 124, 134, 135, 159, 186, 188, 200, 201, 217, 233, 235, 236, 240, 253, 256, 259, 261, 265, 267, 268, 269, 270, 271))
# b <- list(c(1, 89, 98, 174, 263), c(29, 36, 150, 158, 170, 198))
# tdata <- data.frame(a, b)
# 
# tdata <- tdata[-c(1,2),]


