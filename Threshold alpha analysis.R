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
  # Create lists of bins by median split
  binned.data[binned.data$Decrement == "NaN",]$Decrement <- NA
  low.decrement.bins <- binned.data[(binned.data$target.presence == 1 & binned.data$Decrement < 
                                  median(binned.data[binned.data$target.presence == 1,]$Decrement)),]$bin
  high.decrement.bins <- binned.data[(binned.data$target.presence == 1 & binned.data$Decrement > 
                                  median(binned.data[binned.data$target.presence == 1,]$Decrement)),]$bin
  # assing bin group values using lists
  binned.data[which(binned.data$bin %in% 
                      low.decrement.bins),]$bin.group <- 'low.bin'
  binned.data[which(binned.data$bin %in% 
                      high.decrement.bins),]$bin.group <- 'high.bin'
  # assign 0 bin to low.bin group
  binned.data[binned.data$bin == 0,]$bin.group <- "low.bin"
  # return median-split data frame
  binned.data
}


# 4. Select random and no target/no response trials
subset.no.target.rand <- function(median.split.data) {
  no.target.trials.rand <- subset(median.split.data, target.presence == 0 & 
                                    Configuration == 0)
}

# 5. Select square and no target/no response trials
subset.no.target.sqr <- function(median.split.data) {
  no.target.trials.sqr <- subset(median.split.data, target.presence == 0 & 
                                   Configuration == 1)
}

# 6. Remove trials excluded during AR in BVA
remove.AR.trials <- function(trials.subset, AR.trial.list) {
  # Rename (renumber) rows for correspondence with BVA segment numbers in gen artif removal
  row.names(trials.subset) <- seq_len(nrow(trials.subset))
  # Make index to retrive correct item in AR list with subject and phase number
  data.index <- paste(trials.subset[1,"SubID"],trials.subset[1,"Phase"], sep = "_")
  # Remove rows corresponding to numbers in ge.artif list
  trials.subset <- trials.subset[-AR.trial.list[[names(AR.trial.list[data.index])]]$gen.artif,]
  # Rename (renumber) rows for correspondence with BVA segment numbers in blinks removal
  row.names(trials.subset) <- seq_len(nrow(trials.subset))
  # Remove rows corresponding to numbers in remove.blinks list
  trials.subset <- trials.subset[-AR.trial.list[[names(AR.trial.list[data.index])]]$remove.blinks,]
}




# test------------------------------------------------------------------------
# Remove AR trials
teegt <- extract.all.trials("Implicit segregation IB_01_1.txt")
teegt <- teegt[-tartrials]
row.names(teegt) <- seq_len(nrow(teegt))
teegt <- teegt[-c(1, 89, 98, 174, 263),]


tartrials <- AR.trials.rand("Trials removed in AR rand.txt")
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

data.index <- paste(tmed.rand[1,"SubID"],tmed.rand[1,"Phase"], sep = "_")

tmed.rand[1,"Phase"]

tmed.rand.AR <- tmed.rand[-rand.AR.list[["01_1"]]$gen.artif,]
tmed.rand.AR.n <- tmed.rand[-rand.AR.list[[names(rand.AR.list[1])]]$gen.artif,]
View(tmed.rand.AR)
row.names(tmed.rand.AR) <- seq_len(nrow(tmed.rand.AR))
tmed.rand.AR.2 <- tmed.rand.AR[-rand.AR.list[["01_1"]]$remove.blinks,]
View(tmed.rand.AR.2)

names(rand.AR.list[1])

# tbins$bin.group <- NA
# # Create bin groups - substitute data frame name for object name in function!!
# tbins[tbins$Decrement == "NaN",]$Decrement <- NA
# low.decrement.bins <- tbins[(tbins$target.presence == 1 & tbins$Decrement < 
#                                median(tbins[tbins$target.presence == 1,]$Decrement)),]$bin
# high.decrement.bins <- tbins[(tbins$target.presence == 1 & tbins$Decrement > 
#                                 median(tbins[tbins$target.presence == 1,]$Decrement)),]$bin
# tbins[which(tbins$bin %in% 
#                       low.decrement.bins),]$bin.group <- 'low.bin'
# tbins[which(tbins$bin %in% 
#                       high.decrement.bins),]$bin.group <- 'high.bin'
# tbins[tbins$bin == 0,]$bin.group <- "low.bin"
# binned.data
# 
# tbins[tbins[tbins$target.presence == 1,]$Decrement < 
#         median(tbins[tbins$target.presence == 1,]$Decrement),]
# 
# 
# trm1 <- median(tbins[tbins$target.presence == 1,]$Decrement)
# 
# trm2 <- tbins[tbins$target.presence == 1,]$Decrement
# 
# tbins[(tbins$target.presence == 1 & tbins$Decrement < median(tbins[tbins$target.presence == 1,]$Decrement)),]$bin

# a <- list(c(19, 44, 81, 89, 151, 166, 180, 194), c(36, 74, 82, 107, 110, 119, 124, 134, 135, 159, 186, 188, 200, 201, 217, 233, 235, 236, 240, 253, 256, 259, 261, 265, 267, 268, 269, 270, 271))
# b <- list(c(1, 89, 98, 174, 263), c(29, 36, 150, 158, 170, 198))
# tdata <- data.frame(a, b)
# 
# tdata <- tdata[-c(1,2),]

tartrials <- read.table("Trials removed in AR rand.txt", 
           header = TRUE, sep = "\t", row.names = NULL,
           colClasses = c("character", "list", "list"))

View(tartrials)
class(tartrials[,2])


as.numeric(tartrials[tartrials$Subject.Session == "01_1",2][[1]][1])

tartrials[,2]


tartrials[,2] <- as.list(tartrials[,2])

