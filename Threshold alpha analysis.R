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
# 1.1. Read behavioral data file
extract.all.trials <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12, row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  subj.data[subj.data$Decrement == "NaN",]$Decrement <- NA
  subj.data[subj.data$RT == "NaN",]$RT <- NA
  subj.data
}

# # 1.2. Extract trials for eeg analysis
# # 1.2.1. Random
# eeg.trials.rand <- function(behav.file) {
#   subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
#                           header = TRUE, sep = "\t", skip = 12)
#   colnames(subj.data)[5] <- "target.presence"
#   non.target.trials.rand <- subset(subj.data, target.presence == 0 & Configuration == 0)
#   row.names(non.target.trials.rand) <- seq_len(nrow(non.target.trials.rand))
#   non.target.trials.rand
# }


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
  # Assign trials before 1st target to a bin.group based on position relative to median
  if (binned.data[binned.data$target.presence == 1,]$Decrement[1] < 
      median(binned.data[binned.data$target.presence == 1,]$Decrement)) {
    binned.data[binned.data$bin == 0,]$bin.group <- "low.bin"
  } else if (binned.data[binned.data$target.presence == 1,]$Decrement[1] > 
             median(binned.data[binned.data$target.presence == 1,]$Decrement)) {
    binned.data[binned.data$bin == 0,]$bin.group <- "high.bin"
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




# test------------------------------------------------------------------------
# Remove AR trials
teegt <- extract.all.trials("Implicit segregation IB_01_1.txt")
teegt <- teegt[-tartrials]
row.names(teegt) <- seq_len(nrow(teegt))
teegt <- teegt[-c(1, 89, 98, 174, 263),]
tartrials <- AR.trials.rand("Trials removed in AR rand.txt")


# teegt test
teegt <- extract.all.trials("Implicit segregation IB_01_1.txt")
View(teegt)

tbins <- bin.trials(teegt)
View(tbins)

tmed <- median.split.by.int(tbins)
View(tmed)

tmed.rand <- subset.no.target.rand(tmed)
View(tmed.rand)
row.names(tmed.rand) <- seq_len(nrow(tmed.rand))

tmed.sqr <- subset.no.target.sqr(tmed)
View(tmed.sqr)
row.names(tmed.sqr) <- seq_len(nrow(tmed.sqr))

data.index <- paste(tmed.rand[1,"SubID"],tmed.rand[1,"Phase"], sep = "_")

tmed.rand[1,"Phase"]

tmed.rand.AR <- tmed.rand[-rand.AR.list[[data.index]]$gen.artif,]
tmed.rand.AR.n <- tmed.rand[-rand.AR.list[[names(rand.AR.list[1])]]$gen.artif,]
View(tmed.rand.AR)
row.names(tmed.rand.AR) <- seq_len(nrow(tmed.rand.AR))
tmed.rand.AR.2 <- tmed.rand.AR[-rand.AR.list[[data.index]]$remove.blinks,]
View(tmed.rand.AR.2)

tmed.sqr.AR <- tmed.sqr[-sqr.AR.list[[data.index]]$gen.artif,]
View(tmed.sqr.AR)
row.names(tmed.sqr.AR) <- seq_len(nrow(tmed.sqr.AR))
tmed.sqr.AR.2 <- tmed.sqr.AR[-sqr.AR.list[[data.index]]$remove.blinks,]
View(tmed.sqr.AR.2)

tmed.rand.AR.3 <- remove.AR.trials(tmed.rand, rand.AR.list)

tmed.rand.AR.3 == tmed.rand.AR.2

names(rand.AR.list[1])

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

