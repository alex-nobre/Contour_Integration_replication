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
bin.trials.rand1 <- mapply(threshold.alpha.data, behav_ses_1, 'random', SIMPLIFY = FALSE)
bin.trials.rand2 <- mapply(threshold.alpha.data, behav_ses_2, 'random', SIMPLIFY = FALSE)

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


# 3. Divide ERPs by low and high bins
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


# 9. Divide alpha power according to low and high bins
# 9.1. Square 1
low.bin.alpha.sqr1 <- mapply(function(segments, indices) {segments[indices]},
                             sqr1.alpha, low.bin.indices.sqr1)
high.bin.alpha.sqr1 <- mapply(function(segments, indices) {segments[indices]},
                              sqr1.alpha, high.bin.indices.sqr1)
# 9.2. Square 2
low.bin.alpha.sqr2 <- mapply(function(segments, indices) {segments[indices]},
                             sqr2.alpha, low.bin.indices.sqr2)
high.bin.alpha.sqr2 <- mapply(function(segments, indices) {segments[indices]},
                              sqr2.alpha, high.bin.indices.sqr2)
# 9.3. Random 1
low.bin.alpha.rand1 <- mapply(function(segments, indices) {segments[indices]},
                              rand1.alpha, low.bin.indices.rand1)
high.bin.alpha.rand1 <- mapply(function(segments, indices) {segments[indices]},
                               rand1.alpha, high.bin.indices.rand1)
# 9.4. Random 2
low.bin.alpha.rand2 <- mapply(function(segments, indices) {segments[indices]},
                              rand2.alpha, low.bin.indices.rand2)
high.bin.alpha.rand2 <- mapply(function(segments, indices) {segments[indices]},
                               rand2.alpha, high.bin.indices.rand2)

# 10. Compute mean power for each subject
# 10.1. Square, session 1
low.bin.mean.alpha.sqr1 <- sapply(low.bin.alpha.sqr1, mean)
high.bin.mean.alpha.sqr1 <- sapply(high.bin.alpha.sqr1, mean)
# 10.2. Square, session 2
low.bin.mean.alpha.sqr2 <- sapply(low.bin.alpha.sqr2, mean)
high.bin.mean.alpha.sqr2 <- sapply(high.bin.alpha.sqr2, mean)
# 10.3. Random, session 1
low.bin.mean.alpha.rand1 <- sapply(low.bin.alpha.rand1, mean)
high.bin.mean.alpha.rand1 <- sapply(high.bin.alpha.rand1, mean)
# 10.4. Random, session 2
low.bin.mean.alpha.rand2 <- sapply(low.bin.alpha.rand2, mean)
high.bin.mean.alpha.rand2 <- sapply(high.bin.alpha.rand2, mean)

# 11. Add to long data frame
# 11.1. Raw alpha values
rep.data.bin3$alpha <- numeric(nrow(rep.data.bin3))
# Square, session 1
rep.data.bin3[rep.data.bin3$configuration == 'sqr' & 
                rep.data.bin3$session == 1 & 
                rep.data.bin3$intensity.bin == "low.bin",]$alpha <- low.bin.mean.alpha.sqr1
rep.data.bin3[rep.data.bin3$configuration == 'sqr' & 
                rep.data.bin3$session == 1 & 
                rep.data.bin3$intensity.bin == "high.bin",]$alpha <- high.bin.mean.alpha.sqr1
# Square, session 2
rep.data.bin3[rep.data.bin3$configuration == 'sqr' & 
                rep.data.bin3$session == 2 & 
                rep.data.bin3$intensity.bin == "low.bin",]$alpha <- low.bin.mean.alpha.sqr2
rep.data.bin3[rep.data.bin3$configuration == 'sqr' & 
                rep.data.bin3$session == 2 & 
                rep.data.bin3$intensity.bin == "high.bin",]$alpha <- high.bin.mean.alpha.sqr2
# Random, session 1
rep.data.bin3[rep.data.bin3$configuration == 'rand' & 
                rep.data.bin3$session == 1 & 
                rep.data.bin3$intensity.bin == "low.bin",]$alpha <- low.bin.mean.alpha.rand1
rep.data.bin3[rep.data.bin3$configuration == 'rand' & 
                rep.data.bin3$session == 1 & 
                rep.data.bin3$intensity.bin == "high.bin",]$alpha <- high.bin.mean.alpha.rand1
# Random, session 2
rep.data.bin3[rep.data.bin3$configuration == 'rand' & 
                rep.data.bin3$session == 2 & 
                rep.data.bin3$intensity.bin == "low.bin",]$alpha <- low.bin.mean.alpha.rand2
rep.data.bin3[rep.data.bin3$configuration == 'rand' & 
                rep.data.bin3$session == 2 & 
                rep.data.bin3$intensity.bin == "high.bin",]$alpha <- high.bin.mean.alpha.rand2
# 11.2. Log alpha values
rep.data.bin3$log.alpha <- log(rep.data.bin3$alpha)













# # test------------------------------------------------------------------------
# # TEEGT TEST
# #extract trials
teegt <- extract.all.trials("Implicit segregation IB_30_2.txt")
View(teegt)

# bin data
tbins <- bin.trials(teegt)
View(tbins)

which(is.na(tmed[1,"bin.group"]))
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

