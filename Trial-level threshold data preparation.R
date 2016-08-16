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

#-----------------------------------------Functions-----------------------------------------
# 1.1. Read behavioral data file
extract.all.trials <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12, row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  subj.data[subj.data$Decrement == "NaN",]$Decrement <- NA
  subj.data[subj.data$RT == "NaN",]$RT <- NA
  subj.data
}


# 2. Bin trials by target
bin.trials <- function(all.trials) {
  # create new column for bins
  all.trials$bin <- NA
  # start label "bin" with 0
  bin.label <- 0
  # run through target presence column
  for (i in 1:nrow(all.trials)){
    if (all.trials[i,"target.presence"] == 0) {
      all.trials[i,"bin"] <- bin.label # add label to new column
    } else if (all.trials[i,"target.presence"] == 1) {
      bin.label <- bin.label + 1 # add 1 to label (bin1, bin2 etc.)
      all.trials[i,"bin"] <- bin.label # add label to new column (so that this row can be used later to select the bin by its threshold)
    }
  }
  # return binned data
  all.trials
}


# 3. Median split
median.split.by.int <- function(binned.data) {
  # create column for bin group
  binned.data$bin.group <- NA
  # Convert "NaN" values from Psychopy to NA values for R
  # binned.data[binned.data$Decrement == "NaN",]$Decrement <- NA
  # Create lists of bins by median split WITH ASSIGNMENT OF MEDIAN TO GROUPS
  # low.decrement.bins <- binned.data[(binned.data$target.presence == 1 & 
  #                                      binned.data$Decrement < 
  #                                 median(binned.data[binned.data$target.presence == 1,]$Decrement)),]$bin
  # high.decrement.bins <- binned.data[(binned.data$target.presence == 1 & 
  #                                       binned.data$Decrement > 
  #                                 median(binned.data[binned.data$target.presence == 1,]$Decrement)),]$bin
  if (nrow(binned.data[binned.data$target.presence == 1,]) %% 2 != 0) {
    low.decrement.bins <- binned.data[(binned.data$target.presence == 1 &  binned.data$Decrement <
                                   median(c(binned.data[binned.data$target.presence == 1,]$Decrement[1], 
                                            binned.data[binned.data$target.presence == 1,]$Decrement))),]$bin
    high.decrement.bins <- binned.data[(binned.data$target.presence == 1 & binned.data$Decrement >
                                    median(c(binned.data[binned.data$target.presence == 1,]$Decrement[1], 
                                             binned.data[binned.data$target.presence == 1,]$Decrement))),]$bin
  } else if (nrow(binned.data[binned.data$target.presence == 1,]) %% 2 == 0) {
    low.decrement.bins <- binned.data[(binned.data$target.presence == 1 &  binned.data$Decrement <
                                   median(binned.data[binned.data$target.presence == 1,]$Decrement)),]$bin
    high.decrement.bins <- binned.data[(binned.data$target.presence == 1 & binned.data$Decrement >
                                    median(binned.data[binned.data$target.presence == 1,]$Decrement)),]$bin 
  }
  # assing bin group values using lists
  binned.data[which(binned.data$bin %in% 
                      low.decrement.bins),]$bin.group <- 'low.bin'
  binned.data[which(binned.data$bin %in% 
                      high.decrement.bins),]$bin.group <- 'high.bin'
  # Assign trials before 1st target to a bin.group if 1st trial is a target-absent trial
  if (is.na(binned.data[1, "Decrement"]) == TRUE) { # 1st trial is target-absent
    if (binned.data[binned.data$target.presence == 1,]$Decrement[1] < # Decrement in 1st bin < median
        median(binned.data[binned.data$target.presence == 1,]$Decrement)) {
      binned.data[binned.data$bin == 0,]$bin.group <- "low.bin" 
    } else if (binned.data[binned.data$target.presence == 1,]$Decrement[1] > # Decrement in 1st bin > median
               median(binned.data[binned.data$target.presence == 1,]$Decrement)) {
      binned.data[binned.data$bin == 0,]$bin.group <- "high.bin"
    }
  }
  # return median-split data frame
  binned.data
}


# 4. Select random and no target/no response trials
subset.no.target.rand <- function(median.split.data) {
  no.target.trials.rand <- subset(median.split.data, target.presence == 0 & 
                                    Configuration == 0 & Resp == 0)
}

# 5. Select square and no target/no response trials
subset.no.target.sqr <- function(median.split.data) {
  no.target.trials.sqr <- subset(median.split.data, target.presence == 0 & 
                                   Configuration == 1 & Resp == 0)
}

# 6. Remove trials excluded during AR in BVA
remove.AR.trials <- function(trials.subset, AR.trial.list) {
  # Rename (renumber) rows for correspondence with BVA segment numbers in gen artif removal
  row.names(trials.subset) <- seq_len(nrow(trials.subset))
  # Make index to retrive correct item in AR list with subject and phase number
  data.index <- paste(trials.subset[1,"SubID"],trials.subset[1,"Phase"], sep = "_")
  # Remove rows corresponding to numbers in ge.artif list
  trials.subset <- trials.subset[-AR.trial.list[[data.index]]$gen.artif,]
  # Rename (renumber) rows for correspondence with BVA segment numbers in blinks removal
  row.names(trials.subset) <- seq_len(nrow(trials.subset))
  # Remove rows corresponding to numbers in remove.blinks list
  trials.subset <- trials.subset[-AR.trial.list[[data.index]]$remove.blinks,]
}

# 7. Single function
threshold.alpha.data <- function(behav.file, config.type) {
  all.trials <- extract.all.trials(behav.file)
  binned.trials <- bin.trials(all.trials)
  med.split.trials <- median.split.by.int(binned.trials)
  if (config.type == 'square') {
    sqr.trials <- subset.no.target.sqr(med.split.trials)
    binned.trials <- remove.AR.trials(sqr.trials, sqr.AR.list)
  } else if (config.type == 'random') {
    rand.trials <- subset.no.target.rand(med.split.trials)
    binned.trials <- remove.AR.trials(rand.trials, rand.AR.list)
  }
  row.names(binned.trials) <- seq_len(nrow(binned.trials))
  binned.trials
}

#--------------------------------------Prepare data----------------------------------------
# 1. Bin data for all conditions
bin.trials.sqr1 <- mapply(threshold.alpha.data, behav_ses_1, 'square', SIMPLIFY = FALSE)
bin.trials.sqr2 <- mapply(threshold.alpha.data, behav_ses_2, 'square', SIMPLIFY = FALSE)
bin.trials.rand1 <- mapply(threshold.alpha.data, behav_ses_1, 'square', SIMPLIFY = FALSE)
bin.trials.rand2 <- mapply(threshold.alpha.data, behav_ses_2, 'square', SIMPLIFY = FALSE)

# 2. Get indices for each bin
# 2.1. Square 1
low.bin.indices.sqr1 <- lapply(bin.trials.sqr1, function(data) {
                                which(data$bin.group == "low.bin")})
high.bin.indices.sqr1 <- lapply(bin.trials.sqr1, function(data) {
                                which(data$bin.group == "high.bin")})
# 2.2. Square 2
low.bin.indices.sqr2 <- lapply(bin.trials.sqr2, function(data) {
                                which(data$bin.group == "low.bin")})
high.bin.indices.sqr2 <- lapply(bin.trials.sqr2, function(data) {
                                which(data$bin.group == "high.bin")})
# 2.3. Random 1
low.bin.indices.rand1 <- lapply(bin.trials.rand1, function(data) {
                                which(data$bin.group == "low.bin")})
high.bin.indices.rand1 <- lapply(bin.trials.rand1, function(data) {
                                which(data$bin.group == "high.bin")})
# 2.4. Random 2
low.bin.indices.rand2 <- lapply(bin.trials.rand2, function(data) {
                                which(data$bin.group == "low.bin")})
high.bin.indices.rand2 <- lapply(bin.trials.rand2, function(data) {
                                which(data$bin.group == "high.bin")})


# 3. ERPs
# 3.1. Square 1
low.bin.segments.sqr1 <- mapply(function(segments, indices) {segments[indices]},
                                sqr1.trial.dat, low.bin.indices.sqr1)
high.bin.segments.sqr1 <- mapply(function(segments, indices) {segments[indices]},
                                sqr1.trial.dat, high.bin.indices.sqr1)
# 3.2. Square 2
low.bin.segments.sqr2 <- mapply(function(segments, indices) {segments[indices]},
                                sqr2.trial.dat, low.bin.indices.sqr2)
high.bin.segments.sqr2 <- mapply(function(segments, indices) {segments[indices]},
                                sqr2.trial.dat, high.bin.indices.sqr2)
# 3.3. Random 1
low.bin.segments.rand1 <- mapply(function(segments, indices) {segments[indices]},
                                rand1.trial.dat, low.bin.indices.rand1)
high.bin.segments.rand1 <- mapply(function(segments, indices) {segments[indices]},
                                 rand1.trial.dat, high.bin.indices.rand1)
# 3.4. Random 2
low.bin.segments.rand2 <- mapply(function(segments, indices) {segments[indices]},
                                 rand2.trial.dat, low.bin.indices.rand2)
high.bin.segments.rand2 <- mapply(function(segments, indices) {segments[indices]},
                                  rand2.trial.dat, high.bin.indices.rand2)


# 4. Split segments by ROI for each subject and average
# 4.1. Square, session 1
low.bin.segments.sqr1.average <- lapply(low.bin.segments.sqr1, average.segments)
high.bin.segments.sqr1.average <- lapply(high.bin.segments.sqr1, average.segments)
# 4.2. Square, session 2
low.bin.segments.sqr2.average <- lapply(low.bin.segments.sqr2, average.segments)
high.bin.segments.sqr2.average <- lapply(high.bin.segments.sqr2, average.segments)
# 4.3. Random, session 1
low.bin.segments.rand1.average <- lapply(low.bin.segments.rand1, average.segments)
high.bin.segments.rand1.average <- lapply(high.bin.segments.rand1, average.segments)
# 4.4. Random, session 1
low.bin.segments.rand2.average <- lapply(low.bin.segments.rand2, average.segments)
high.bin.segments.rand2.average <- lapply(high.bin.segments.rand2, average.segments)


# 5. Baseline correction by session, configuration and alpha power
# 5.1. Square, session 1
low.bin.segments.sqr1.corrected <- lapply(low.bin.segments.sqr1.average, 
                                            baseline.correction)
high.bin.segments.sqr1.corrected <- lapply(high.bin.segments.sqr1.average, 
                                             baseline.correction)
# 5.2. Square, session 2
low.bin.segments.sqr2.corrected <- lapply(low.bin.segments.sqr2.average, 
                                            baseline.correction)
high.bin.segments.sqr2.corrected <- lapply(high.bin.segments.sqr2.average, 
                                             baseline.correction)
# 5.3.Random, session 1
low.bin.segments.rand1.corrected <- lapply(low.bin.segments.rand1.average, 
                                             baseline.correction)
high.bin.segments.rand1.corrected <- lapply(high.bin.segments.rand1.average, 
                                              baseline.correction)
# 5.4. Random, session 1
low.bin.segments.rand2.corrected <- lapply(low.bin.segments.rand2.average, 
                                             baseline.correction)
high.bin.segments.rand2.corrected <- lapply(high.bin.segments.rand2.average, 
                                              baseline.correction)

# 6. Join segments by ROI
# 6.1. Square, session 1
low.bin.ROIs.sqr1 <- split.ROIs(low.bin.segments.sqr1.corrected)
high.bin.ROIs.sqr1 <- split.ROIs(high.bin.segments.sqr1.corrected)
# 6.2. Square, session 2
low.bin.ROIs.sqr2 <- split.ROIs(low.bin.segments.sqr2.corrected)
high.bin.ROIs.sqr2 <- split.ROIs(high.bin.segments.sqr2.corrected)
# 6.3. Random, session 1
low.bin.ROIs.rand1 <- split.ROIs(low.bin.segments.rand1.corrected)
high.bin.ROIs.rand1 <- split.ROIs(high.bin.segments.rand1.corrected)
# 6.4. Random, session 1
low.bin.ROIs.rand2 <- split.ROIs(low.bin.segments.rand2.corrected)
high.bin.ROIs.rand2 <- split.ROIs(high.bin.segments.rand2.corrected)


# 7. Compute grand averages by condition
# 7.1. Square, session 1
low.bin.grand.averages.sqr1 <- lapply(low.bin.ROIs.sqr1, compute.grand.average)
high.bin.grand.averages.sqr1 <- lapply(high.bin.ROIs.sqr1, compute.grand.average)
# 7.2. Square, session 2
low.bin.grand.averages.sqr2 <- lapply(low.bin.ROIs.sqr2, compute.grand.average)
high.bin.grand.averages.sqr2 <- lapply(high.bin.ROIs.sqr2, compute.grand.average)
# 7.3. Random, session 1
low.bin.grand.averages.rand1 <- lapply(low.bin.ROIs.rand1, compute.grand.average)
high.bin.grand.averages.rand1 <- lapply(high.bin.ROIs.rand1, compute.grand.average)
# 7.4. Random, session 2
low.bin.grand.averages.rand2 <- lapply(low.bin.ROIs.rand2, compute.grand.average)
high.bin.grand.averages.rand2 <- lapply(high.bin.ROIs.rand2, compute.grand.average)

# 8. Compute ERP mean amplitudes by session, configuration and alpha power
# 8.1. Square, session 1
low.bin.ROIs.sqr1.means <- lapply(low.bin.segments.sqr1.corrected, 
                                    extract.ERPs)
high.bin.ROIs.sqr1.means <- lapply(high.bin.segments.sqr1.corrected, 
                                     extract.ERPs)
# 8.2. Square, session 2
low.bin.ROIs.sqr2.means <- lapply(low.bin.segments.sqr2.corrected, 
                                    extract.ERPs)
high.bin.ROIs.sqr2.means <- lapply(high.bin.segments.sqr2.corrected, 
                                     extract.ERPs)
# 8.3. Random, session 1
low.bin.ROIs.rand1.means <- lapply(low.bin.segments.rand1.corrected, 
                                     extract.ERPs)
high.bin.ROIs.rand1.means <- lapply(high.bin.segments.rand1.corrected, 
                                      extract.ERPs)
# 8.4. Random, session 1
low.bin.ROIs.rand2.means <- lapply(low.bin.segments.rand2.corrected, 
                                     extract.ERPs)
high.bin.ROIs.rand2.means <- lapply(high.bin.segments.rand2.corrected, 
                                      extract.ERPs)


#-------------------------------Create data set for binned ERPs------------------------
# 4. Create new data frame for analysis with alpha as factor
# 4.1. Remove base and trial-alpha ERPs
rep_data6 <- rep_data2[,-c(4:length(rep_data2))]

#---------------------------Trial level alpha data frame-------------------------
# 1. Append to data frame with average ERPs
rep_data2 <- rep_data

# 1.1 Low bin
# 1.1.1. Square, session 1
rep_data6$low.bin.C1.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 1))
rep_data6$low.bin.P1.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 2))
rep_data6$low.bin.N1.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 3))
rep_data6$low.bin.occ.nd1.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 4))
rep_data6$low.bin.occ.nd2.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 5))
rep_data6$low.bin.left.nd1.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 6))
rep_data6$low.bin.left.nd2.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 7))
rep_data6$low.bin.right.nd1.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 8))
rep_data6$low.bin.right.nd2.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 9))
rep_data6$low.bin.RL.nd1.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 10))
rep_data6$low.bin.RL.nd2.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 11))
rep_data6$low.bin.N2.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 12))
rep_data6$low.bin.P3.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 13))
rep_data6$low.bin.LP.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 14))
# 1.1.2. Square, session 2
rep_data6$low.bin.C1.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 1))
rep_data6$low.bin.P1.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 2))
rep_data6$low.bin.N1.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 3))
rep_data6$low.bin.occ.nd1.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 4))
rep_data6$low.bin.occ.nd2.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 5))
rep_data6$low.bin.left.nd1.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 6))
rep_data6$low.bin.left.nd2.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 7))
rep_data6$low.bin.right.nd1.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 8))
rep_data6$low.bin.right.nd2.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 9))
rep_data6$low.bin.RL.nd1.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 10))
rep_data6$low.bin.RL.nd2.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 11))
rep_data6$low.bin.N2.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 12))
rep_data6$low.bin.P3.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 13))
rep_data6$low.bin.LP.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 14))
# 1.1.3. Random, session 1
rep_data6$low.bin.C1.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 1))
rep_data6$low.bin.P1.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 2))
rep_data6$low.bin.N1.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 3))
rep_data6$low.bin.occ.nd1.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 4))
rep_data6$low.bin.occ.nd2.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 5))
rep_data6$low.bin.left.nd1.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 6))
rep_data6$low.bin.left.nd2.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 7))
rep_data6$low.bin.right.nd1.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 8))
rep_data6$low.bin.right.nd2.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 9))
rep_data6$low.bin.RL.nd1.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 10))
rep_data6$low.bin.RL.nd2.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 11))
rep_data6$low.bin.N2.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 12))
rep_data6$low.bin.P3.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 13))
rep_data6$low.bin.LP.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 14))
# 1.1.4. Random, session 2
rep_data6$low.bin.C1.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 1))
rep_data6$low.bin.P1.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 2))
rep_data6$low.bin.N1.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 3))
rep_data6$low.bin.occ.nd1.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 4))
rep_data6$low.bin.occ.nd2.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 5))
rep_data6$low.bin.left.nd1.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 6))
rep_data6$low.bin.left.nd2.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 7))
rep_data6$low.bin.right.nd1.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 8))
rep_data6$low.bin.right.nd2.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 9))
rep_data6$low.bin.RL.nd1.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 10))
rep_data6$low.bin.RL.nd2.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 11))
rep_data6$low.bin.N2.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 12))
rep_data6$low.bin.P3.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 13))
rep_data6$low.bin.LP.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 14))

# 1.2. High bin
# 1.2.1. Square, session 1
rep_data6$high.bin.C1.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 1))
rep_data6$high.bin.P1.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 2))
rep_data6$high.bin.N1.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 3))
rep_data6$high.bin.occ.nd1.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 4))
rep_data6$high.bin.occ.nd2.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 5))
rep_data6$high.bin.left.nd1.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 6))
rep_data6$high.bin.left.nd2.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 7))
rep_data6$high.bin.right.nd1.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 8))
rep_data6$high.bin.right.nd2.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 9))
rep_data6$high.bin.RL.nd1.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 10))
rep_data6$high.bin.RL.nd2.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 11))
rep_data6$high.bin.N2.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 12))
rep_data6$high.bin.P3.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 13))
rep_data6$high.bin.LP.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 14))
# 1.2.2. Square, session 2
rep_data6$high.bin.C1.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 1))
rep_data6$high.bin.P1.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 2))
rep_data6$high.bin.N1.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 3))
rep_data6$high.bin.occ.nd1.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 4))
rep_data6$high.bin.occ.nd2.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 5))
rep_data6$high.bin.left.nd1.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 6))
rep_data6$high.bin.left.nd2.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 7))
rep_data6$high.bin.right.nd1.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 8))
rep_data6$high.bin.right.nd2.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 9))
rep_data6$high.bin.RL.nd1.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 10))
rep_data6$high.bin.RL.nd2.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 11))
rep_data6$high.bin.N2.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 12))
rep_data6$high.bin.P3.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 13))
rep_data6$high.bin.LP.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 14))
# 1.2.3. Random, session 1
rep_data6$high.bin.C1.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 1))
rep_data6$high.bin.P1.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 2))
rep_data6$high.bin.N1.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 3))
rep_data6$high.bin.occ.nd1.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 4))
rep_data6$high.bin.occ.nd2.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 5))
rep_data6$high.bin.left.nd1.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 6))
rep_data6$high.bin.left.nd2.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 7))
rep_data6$high.bin.right.nd1.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 8))
rep_data6$high.bin.right.nd2.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 9))
rep_data6$high.bin.RL.nd1.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 10))
rep_data6$high.bin.RL.nd2.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 11))
rep_data6$high.bin.N2.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 12))
rep_data6$high.bin.P3.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 13))
rep_data6$high.bin.LP.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 14))
# 1.2.4. Random, session 2
rep_data6$high.bin.C1.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 1))
rep_data6$high.bin.P1.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 2))
rep_data6$high.bin.N1.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 3))
rep_data6$high.bin.occ.nd1.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 4))
rep_data6$high.bin.occ.nd2.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 5))
rep_data6$high.bin.left.nd1.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 6))
rep_data6$high.bin.left.nd2.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 7))
rep_data6$high.bin.right.nd1.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 8))
rep_data6$high.bin.right.nd2.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 9))
rep_data6$high.bin.RL.nd1.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 10))
rep_data6$high.bin.RL.nd2.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 11))
rep_data6$high.bin.N2.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 12))
rep_data6$high.bin.P3.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 13))
rep_data6$high.bin.LP.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 14))

# 4.2. Reshape by session
rep.data.bin <- reshape(rep_data6, varying = 4:ncol(rep_data6), sep = "_", 
                          direction = "long", 
                          new.row.names = NULL)
rep.data.bin[,ncol(rep.data.bin)]<- NULL
names(rep.data.bin)[names(rep.data.bin) == "time"] <- "session"

# 4.3. Tidyr by configuration
rep.data.bin2 <- rep.data.bin %>%
  unite(sqr,contains("sqr")) %>%
  unite(rand,contains("rand")) %>%
  gather(configuration,values,sqr:rand) %>%
  separate(values,c("low.bin.C1","low.bin.P1","low.bin.N1","low.bin.occ.nd1",
                    "low.bin.occ.nd2","low.bin.left.nd1","low.bin.left.nd2",
                    "low.bin.right.nd1","low.bin.right.nd2","low.bin.RL.nd1",
                    "low.bin.RL.nd2","low.bin.N2","low.bin.P3","low.bin.LP",
                    "high.bin.C1","high.bin.P1","high.bin.N1","high.bin.occ.nd1",
                    "high.bin.occ.nd2","high.bin.left.nd1","high.bin.left.nd2",
                    "high.bin.right.nd1","high.bin.right.nd2","high.bin.RL.nd1",
                    "high.bin.RL.nd2","high.bin.N2","high.bin.P3","high.bin.LP"),
           sep = "_",
           convert = TRUE)

# 4.4. Tidyr by intensity bin
rep.data.bin3 <- rep.data.bin2 %>%
  unite(low.bin,contains("low.bin")) %>%
  unite(high.bin,contains("high.bin")) %>%
  gather(intensity.bin,values,low.bin:high.bin) %>%
  separate(values,c("C1","P1","N1","occ.nd1","occ.nd2","left.nd1","left.nd2",
                    "right.nd1","right.nd2","RL.nd1","RL.nd2","N2","P3","LP"),
           sep = "_",
           convert = TRUE)

# 4.5. Coerce to factors
rep.data.bin3$group <- factor(rep.data.bin3$group)
rep.data.bin3$group.original <- factor(rep.data.bin3$group.original)
rep.data.bin3$session <- factor(rep.data.bin3$session)
rep.data.bin3$configuration <- factor(rep.data.bin3$configuration)
rep.data.bin3$intensity.bin <- factor(rep.data.bin3$intensity.bin)

# Alpha power
















# # test------------------------------------------------------------------------
# # TEEGT TEST
# #extract trials
teegt <- extract.all.trials("Implicit segregation IB_20_1.txt")
View(teegt)

# bin data
tbins <- bin.trials(teegt)
View(tbins)

is.na(tbins[1,"Decrement"] == FALSE)
# median split
tmed <- median.split.by.int(tbins)
View(tmed)

# subset rand
tmed.rand <- subset.no.target.rand(tmed)
View(tmed.rand)
row.names(tmed.rand) <- seq_len(nrow(tmed.rand))

# subset sqr
tmed.sqr <- subset.no.target.sqr(tmed)
View(tmed.sqr)
row.names(tmed.sqr) <- seq_len(nrow(tmed.sqr))

# index for list names
data.index <- paste(tmed.rand[1,"SubID"],tmed.rand[1,"Phase"], sep = "_")

# AR rand
tmed.rand.AR <- tmed.rand[-rand.AR.list[[data.index]]$gen.artif,]
tmed.rand.AR.n <- tmed.rand[-rand.AR.list[[names(rand.AR.list[1])]]$gen.artif,]
View(tmed.rand.AR)
row.names(tmed.rand.AR) <- seq_len(nrow(tmed.rand.AR))
tmed.rand.AR.2 <- tmed.rand.AR[-rand.AR.list[[data.index]]$remove.blinks,]
View(tmed.rand.AR.2)

tmed.rand.AR.2 <- tmed.rand.AR[-(nrow(tmed.sqr.AR) + 1),]

# AR sqr
tmed.sqr.AR <- tmed.sqr[-sqr.AR.list[[data.index]]$gen.artif,]
View(tmed.sqr.AR)
row.names(tmed.sqr.AR) <- seq_len(nrow(tmed.sqr.AR))
tmed.sqr.AR.2 <- tmed.sqr.AR[-sqr.AR.list[[data.index]]$remove.blinks,]
View(tmed.sqr.AR.2)

tmed.rand.AR.3 <- remove.AR.trials(tmed.rand, rand.AR.list)

tmed.rand.AR.3 == tmed.rand.AR.2

names(rand.AR.list[1])

# Remove AR trials
teegt <- extract.all.trials("Implicit segregation IB_01_1.txt")
teegt <- teegt[-tartrials]
row.names(teegt) <- seq_len(nrow(teegt))
teegt <- teegt[-c(1, 89, 98, 174, 263),]
tartrials <- AR.trials.rand("Trials removed in AR rand.txt")

# MEDIAN SPLIT 1 - 1ST SEGMENTS NA AND NO CONDITION FOR INCLUSION OF MEDIAN
tbins$bin.group <- NA
# Create bin groups - substitute data frame name for object name in function!!
tbins[tbins$Decrement == "NaN",]$Decrement <- NA
low.decrement.bins <- tbins[(tbins$target.presence == 1 & tbins$Decrement <
                               median(tbins[tbins$target.presence == 1,]$Decrement)),]$bin
high.decrement.bins <- tbins[(tbins$target.presence == 1 & tbins$Decrement >
                                median(tbins[tbins$target.presence == 1,]$Decrement)),]$bin
tbins[which(tbins$bin %in%
                      low.decrement.bins),]$bin.group <- 'low.bin'
tbins[which(tbins$bin %in%
                      high.decrement.bins),]$bin.group <- 'high.bin'
tbins[tbins$bin == 0,]$bin.group <- "low.bin"
binned.data

#MEDIAN SPLIT 2 - 1ST SEGMENT INCLUDED AND CONDITIONS FOR INCLUSION OF MEDIAN
tbins$bin.group <- NA
# Create bin groups - substitute data frame name for object name in function!!
tbins[tbins$Decrement == "NaN",]$Decrement <- NA
if (nrow(tbins[tbins$target.presence == 1,]) %% 2 != 0) {
  low.decrement.bins <- tbins[(tbins$target.presence == 1 &  tbins$Decrement <
                                 median(c(tbins[tbins$target.presence == 1,]$Decrement[1],
                                          tbins[tbins$target.presence == 1,]$Decrement))),]$bin
  high.decrement.bins <- tbins[(tbins$target.presence == 1 & tbins$Decrement >
                                  median(c(tbins[tbins$target.presence == 1,]$Decrement[1],
                                           tbins[tbins$target.presence == 1,]$Decrement))),]$bin
} else if (nrow(tbins[tbins$target.presence == 1,]) %% 2 == 0) {
  low.decrement.bins <- tbins[(tbins$target.presence == 1 &  tbins$Decrement <
                                 median(tbins[tbins$target.presence == 1,]$Decrement)),]$bin
  high.decrement.bins <- tbins[(tbins$target.presence == 1 & tbins$Decrement >
                                  median(tbins[tbins$target.presence == 1,]$Decrement)),]$bin
}
tbins[which(tbins$bin %in%
              low.decrement.bins),]$bin.group <- 'low.bin'
tbins[which(tbins$bin %in%
              high.decrement.bins),]$bin.group <- 'high.bin'
if (tbins[tbins$target.presence == 1,]$Decrement[1] <
  median(tbins[tbins$target.presence == 1,]$Decrement)) {
  tbins[tbins$bin == 0,]$bin.group <- "low.bin"
} else if (tbins[tbins$target.presence == 1,]$Decrement[1] >
           median(tbins[tbins$target.presence == 1,]$Decrement)) {
  tbins[tbins$bin == 0,]$bin.group <- "high.bin"
}



# tests for concatenation of first decrement value and decrements vector using sort and median
sort(tbins[tbins$target.presence == 1,]$Decrement)
sort(c(tbins[tbins$target.presence == 1,]$Decrement[1],
     tbins[tbins$target.presence == 1,]$Decrement))
tbins[tbins$target.presence == 1,]$Decrement[1]
median(c(tbins[tbins$target.presence == 1,]$Decrement[1],
       tbins[tbins$target.presence == 1,]$Decrement))

median(tbins[tbins$target.presence == 1,]$Decrement)



# tests for slicing decrement values of target-present trials
tbins[tbins[tbins$target.presence == 1,]$Decrement <
        median(tbins[tbins$target.presence == 1,]$Decrement),]

trm1 <- median(tbins[tbins$target.presence == 1,]$Decrement)

trm2 <- tbins[tbins$target.presence == 1,]$Decrement

tbins[(tbins$target.presence == 1 & tbins$Decrement < median(tbins[tbins$target.presence == 1,]$Decrement)),]$bin



# tests to make column of vectors as lists instead of factor - NOT USED!!!
# tartrials <- read.table("Trials removed in AR rand.txt",
#            header = TRUE, sep = "\t", row.names = NULL,
#            colClasses = c("character", "list", "list"))
# View(tartrials)
# class(tartrials[,2])
# as.numeric(tartrials[tartrials$Subject.Session == "01_1",2][[1]][1])
# tartrials[,2]
# tartrials[,2] <- as.list(tartrials[,2])

