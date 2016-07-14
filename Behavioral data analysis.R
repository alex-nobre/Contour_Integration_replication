library(pastecs)
library(ggplot2)
library(nlme)
library(gmodels)
library(lme4)
library(gridExtra)
library(outliers)
library(lattice)
library(car)
library(effsize)
library(MPDiR)
library(xlsx)
library(polycor)
library(ggm)
library(GGally)
library(dplyr)
library(lattice)
library(psyphy)
library(ggplot2)
library(gridExtra)
library(knitr)
library(quickpsy)
library(timeSeries)

# 0. Save graphical defaults
defaults <- par()

#-----------------------------------Prepare data----------------------------
# 1. Import questionnaire data and rename columns
# 1.1. Read excel files
questionnaire.ses1 <- read.xlsx(paste('./Data/Questionnaire_data/', 
                                      "Questionnaire data ses1.xlsx", sep = ""), 
                                sheetName = "Questionnaire data", header = TRUE)
questionnaire.ses2 <- read.xlsx(paste('./Data/Questionnaire_data/', 
                                      "Questionnaire data ses2.xlsx", sep = ""), 
                                sheetName = "Questionnaire data", header = TRUE)

# 1.2. Eliminate lines of subjects 27 & 28
questionnaire.ses1 <- questionnaire.ses1[-c(25,26),]
questionnaire.ses2 <- questionnaire.ses2[-c(25,26),]

# 1.3. Exclude unnecessary columns
questionnaire.ses1[,c(1,16,17)] <- NULL # exclude subject, group & annotations columns
questionnaire.ses2[,c(1,16,17)] <- NULL # exclude subject, group & annotations columns

# 1.4. Assign column names
colnames(questionnaire.ses1) <- c("Recall.ses1",	"Block.ses1", "conf.1.ses1",	
                                  "conf.2.ses1", "conf.3.ses1",	"conf.4.ses1",	
                                  "conf.5.ses1",	"conf.6.ses1",	"freq.1.ses1",	
                                  "freq.2.ses1",	"freq.3.ses1","freq.4.ses1",	
                                  "freq.5.ses1",	"freq.6.ses1")
colnames(questionnaire.ses2) <- c("Recall.ses2",	"Block.ses2", "conf.1.ses2",	
                                  "conf.2.ses2", "conf.3.ses2", "conf.4.ses2", 
                                  "conf.5.ses2",	"conf.6.ses2",	"freq.1.ses2",	
                                  "freq.2.ses2",	"freq.3.ses2",	"freq.4.ses2",	
                                  "freq.5.ses2",	"freq.6.ses2")

# 2. Get list of psychopy file names
behav_ses_1 <- list.files('./Data/Data_Psychopy', pattern = "Implicit segregation IB_.*\\_1")
behav_ses_2 <- list.files('./Data/Data_Psychopy', pattern = "Implicit segregation IB_.*\\_2")
behav_ses_3 <- list.files('./Data/Data_Psychopy', pattern = "Implicit segregation IB_.*\\_3")

#-------------------------Compute Psychophysical measures----------------------------
# 0. Compute accuracy
# 1.1. Functions to read files and accuracy
# 1.1.1. Sessions 1 & 2 (task: dim disc detection)
# 1.1.1.1. Square and random configurations
# Accuracy square
compute.accuracy.sqr <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  target.trials.sqr <- subset(subj.data, target.presence == 1 & Configuration == 1)
  accuracy.sqr <- as.vector(target.trials.sqr$correct)
}

# Accuracy random
compute.accuracy.rand <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  target.trials.rand <- subset(subj.data, target.presence == 1 & Configuration == 0)
  accuracy.rand <- as.vector(target.trials.rand$correct)
}

# 1.1.1.2. Both configs
# Accuracy both conditions
compute.accuracy <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  target.trials <- subset(subj.data, target.presence == 1)
  accuracy <- as.vector(target.trials$correct)
}

# # 1.1.2. Session 3 (task: diamond detection)
compute.accuracy.ses3 <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  diamond.trials <- subset(subj.data, Configuration == 2)
  accuracy.ses3 <- as.vector(diamond.trials$correct)
}

# 1.2. Extract intensities for each subject by session and configuration
# 1.2.1. Square
accuracies.sqr.1 <- lapply(behav_ses_1, compute.accuracy.sqr)
accuracies.sqr.2 <- lapply(behav_ses_2, compute.accuracy.sqr)
accuracies.sqr.3 <- lapply(behav_ses_3, compute.accuracy.sqr)
# 1.2.1. Random
accuracies.rand.1 <- lapply(behav_ses_1, compute.accuracy.rand)
accuracies.rand.2 <- lapply(behav_ses_2, compute.accuracy.rand)
accuracies.rand.3 <- lapply(behav_ses_3, compute.accuracy.rand)
# 1.2.1. Both configurations
accuracies.1 <- lapply(behav_ses_1, compute.accuracy)
accuracies.2 <- lapply(behav_ses_2, compute.accuracy)
accuracies.3 <- lapply(behav_ses_3, compute.accuracy)

accuracies.1 <- numeric()

for (i in 1:32) {
  accuracies.1[i] <- compute.accuracy(behav_ses_1[[i]])
}

# accuracies.sub1 <- compute.accuracy(behav_ses_1[[1]])
# accuracies.sub2 <- compute.accuracy(behav_ses_1[[2]])
# accuracies.sub3 <- compute.accuracy(behav_ses_1[[3]])
# accuracies.sub4 <- compute.accuracy(behav_ses_1[[4]])
# accuracies.sub5 <- compute.accuracy(behav_ses_1[[5]])
# accuracies.sub6 <- compute.accuracy(behav_ses_1[[6]])
# accuracies.sub7 <- compute.accuracy(behav_ses_1[[7]])
# accuracies.sub8 <- compute.accuracy(behav_ses_1[[8]])
# accuracies.sub9 <- compute.accuracy(behav_ses_1[[9]])
# accuracies.sub10 <- compute.accuracy(behav_ses_1[[10]])
# accuracies.sub11 <- compute.accuracy(behav_ses_1[[11]])
# accuracies.sub12 <- compute.accuracy(behav_ses_1[[12]])
# accuracies.sub13 <- compute.accuracy(behav_ses_1[[13]])
# accuracies.sub14 <- compute.accuracy(behav_ses_1[[14]])
# accuracies.sub15 <- compute.accuracy(behav_ses_1[[15]])
# accuracies.sub16 <- compute.accuracy(behav_ses_1[[16]])
# accuracies.sub17 <- compute.accuracy(behav_ses_1[[17]])
# accuracies.sub18 <- compute.accuracy(behav_ses_1[[18]])
# accuracies.sub19 <- compute.accuracy(behav_ses_1[[19]])
# accuracies.sub20 <- compute.accuracy(behav_ses_1[[20]])
# accuracies.sub21 <- compute.accuracy(behav_ses_1[[21]])
# accuracies.sub22 <- compute.accuracy(behav_ses_1[[22]])
# accuracies.sub23 <- compute.accuracy(behav_ses_1[[23]])
# accuracies.sub24 <- compute.accuracy(behav_ses_1[[24]])
# accuracies.sub25 <- compute.accuracy(behav_ses_1[[25]])
# accuracies.sub26 <- compute.accuracy(behav_ses_1[[26]])
# accuracies.sub27 <- compute.accuracy(behav_ses_1[[27]])
# accuracies.sub28 <- compute.accuracy(behav_ses_1[[28]])
# accuracies.sub29 <- compute.accuracy(behav_ses_1[[29]])
# accuracies.sub30 <- compute.accuracy(behav_ses_1[[30]])
# accuracies.sub31 <- compute.accuracy(behav_ses_1[[31]])
# accuracies.sub32 <- compute.accuracy(behav_ses_1[[32]])
# 
# accuracies.1 <- c(accuracies.sub1, accuracies.sub2, accuracies.sub3, accuracies.sub4,
#                   accuracies.sub5, accuracies.sub6, accuracies.sub7, accuracies.sub8,
#                   accuracies.sub9, accuracies.sub10, accuracies.sub11, accuracies.sub12,
#                   accuracies.sub13, accuracies.sub14, accuracies.sub15, accuracies.sub16,
#                   accuracies.sub17, accuracies.sub18, accuracies.sub19, accuracies.sub20,
#                   accuracies.sub21, accuracies.sub22, accuracies.sub23, accuracies.sub24,
#                   accuracies.sub25, accuracies.sub26, accuracies.sub27, accuracies.sub28,
#                   accuracies.sub29, accuracies.sub30, accuracies.sub31, accuracies.sub32)
# 
# intensities.sub1 <- extract.intensities(behav_ses_1[[1]])
# intensities.sub2 <- extract.intensities(behav_ses_1[[2]])
# intensities.sub3 <- extract.intensities(behav_ses_1[[3]])
# intensities.sub4 <- extract.intensities(behav_ses_1[[4]])
# intensities.sub5 <- extract.intensities(behav_ses_1[[5]])
# intensities.sub6 <- extract.intensities(behav_ses_1[[6]])
# intensities.sub7 <- extract.intensities(behav_ses_1[[7]])
# intensities.sub8 <- extract.intensities(behav_ses_1[[8]])
# intensities.sub9 <- extract.intensities(behav_ses_1[[9]])
# intensities.sub10 <- extract.intensities(behav_ses_1[[10]])
# intensities.sub11 <- extract.intensities(behav_ses_1[[11]])
# intensities.sub12 <- extract.intensities(behav_ses_1[[12]])
# intensities.sub13 <- extract.intensities(behav_ses_1[[13]])
# intensities.sub14 <- extract.intensities(behav_ses_1[[14]])
# intensities.sub15 <- extract.intensities(behav_ses_1[[15]])
# intensities.sub16 <- extract.intensities(behav_ses_1[[16]])
# intensities.sub17 <- extract.intensities(behav_ses_1[[17]])
# intensities.sub18 <- extract.intensities(behav_ses_1[[18]])
# intensities.sub19 <- extract.intensities(behav_ses_1[[19]])
# intensities.sub20 <- extract.intensities(behav_ses_1[[20]])
# intensities.sub21 <- extract.intensities(behav_ses_1[[21]])
# intensities.sub22 <- extract.intensities(behav_ses_1[[22]])
# intensities.sub23 <- extract.intensities(behav_ses_1[[23]])
# intensities.sub24 <- extract.intensities(behav_ses_1[[24]])
# intensities.sub25 <- extract.intensities(behav_ses_1[[25]])
# intensities.sub26 <- extract.intensities(behav_ses_1[[26]])
# intensities.sub27 <- extract.intensities(behav_ses_1[[27]])
# intensities.sub28 <- extract.intensities(behav_ses_1[[28]])
# intensities.sub29 <- extract.intensities(behav_ses_1[[29]])
# intensities.sub30 <- extract.intensities(behav_ses_1[[30]])
# intensities.sub31 <- extract.intensities(behav_ses_1[[31]])
# intensities.sub32 <- extract.intensities(behav_ses_1[[32]])
# 
# intensities.1 <- c(intensities.sub1, intensities.sub2, intensities.sub3, intensities.sub4,
#                   intensities.sub5, intensities.sub6, intensities.sub7, intensities.sub8,
#                   intensities.sub9, intensities.sub10, intensities.sub11, intensities.sub12,
#                   intensities.sub13, intensities.sub14, intensities.sub15, intensities.sub16,
#                   intensities.sub17, intensities.sub18, intensities.sub19, intensities.sub20,
#                   intensities.sub21, intensities.sub22, intensities.sub23, intensities.sub24,
#                   intensities.sub25, intensities.sub26, intensities.sub27, intensities.sub28,
#                   intensities.sub29, intensities.sub30, intensities.sub31, intensities.sub32)

testAcc <- c(accuracies.sub1, accuracies.sub2, accuracies.sub3)

# 1. Compute d'
# 1.1. Functions to read files and accuracy and compute proportion of hits & false alarms 
# in both configs
# 1.1.1. Sessions 1 & 2 (task: dim disc detection)
# 1.1.1.1. Square and random configurations
# P(Hits) square
compute.Ph.sqr <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  target.trials.sqr <- subset(subj.data, target.presence == 1 & Configuration == 1)
  Ph.sqr <- mean(target.trials.sqr$correct)
}
# P(Hits) random
compute.Ph.rand <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  target.trials.rand <- subset(subj.data, target.presence == 1 & Configuration == 0)
  Ph.rand <- mean(target.trials.rand$correct)
}
# P(False alarms) square
compute.Pfa.sqr <- function(behav.file) {
    subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                            header = TRUE, sep = "\t", skip = 12, 
                            row.names = NULL)
    colnames(subj.data)[5] <- "target.presence"
    notarget.trials.sqr <- subset(subj.data, target.presence == 0 & Configuration == 1)
    Pfa.sqr <- mean(notarget.trials.sqr$Resp)
}
# P(False alarms) random
compute.Pfa.rand <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  notarget.trials.rand <- subset(subj.data, target.presence == 0 & Configuration == 0)
  Pfa.rand <- mean(notarget.trials.rand$Resp)
}

# 1.1.1.2. Both
# P(Hits)
compute.Ph <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  target.trials <- subset(subj.data, target.presence == 1)
  Ph <- mean(target.trials$correct)
}

# P(False alarms)
compute.Pfa <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  notarget.trials <- subset(subj.data, target.presence == 0)
  Pfa <- mean(notarget.trials$Resp)
}

# 1.1.2. Session 3 (task: diamond detection)
# P(Hits)
compute.Ph.ses3 <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  diamond.trials <- subset(subj.data, Configuration == 2)
  Ph <- mean(diamond.trials$correct)
}
# P(False alarms)
compute.Pfa.ses3 <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  nodiamond.trials <- subset(subj.data, Configuration == 0 | Configuration == 1)
  Pfa <- mean(nodiamond.trials$Resp)
}

# 1.2. Compute proportions of hits for each configurations
#square
ses1.Ph.sqr <- lapply(behav_ses_1, compute.Ph.sqr)
ses2.Ph.sqr <- lapply(behav_ses_2, compute.Ph.sqr)
#random
ses1.Ph.rand <- lapply(behav_ses_1, compute.Ph.rand)
ses2.Ph.rand <- lapply(behav_ses_2, compute.Ph.rand)
# Both
ses1.Ph <- lapply(behav_ses_1, compute.Ph)
ses2.Ph <- lapply(behav_ses_2, compute.Ph)
ses3.Ph <- lapply(behav_ses_3, compute.Ph.ses3)

# 1.3. Compute proportions of false alarms for both configurations
#square
ses1.Pfa.sqr <- lapply(behav_ses_1, compute.Pfa.sqr)
ses2.Pfa.sqr <- lapply(behav_ses_2, compute.Pfa.sqr)
#random
ses1.Pfa.rand <- lapply(behav_ses_1, compute.Pfa.rand)
ses2.Pfa.rand <- lapply(behav_ses_2, compute.Pfa.rand)
# Both
ses1.Pfa <- lapply(behav_ses_1, compute.Pfa)
ses2.Pfa <- lapply(behav_ses_2, compute.Pfa)
ses3.Pfa <- lapply(behav_ses_3, compute.Pfa.ses3)

# 1.4. Compute proportion correct rejections, misses, correct - UPDATE WITH CONFIGS!!!!
ses1.Pcr <- lapply(ses1.Pfa, function(x) 1-x)
ses1.Pm <- lapply(ses1.Ph, function(y) 1-y)
ses1.Pc <- mapply(function(a, b) (a + 9 * b)/10, ses1.Ph, ses1.Pcr)

# 1.5. Function to compute d'
compute.dprime <- function(hit, fa) {
  z.hit <- qnorm(hit)
  z.fa <- qnorm(fa)
  if (is.infinite(z.fa) == T) {
    z.fa = 0
  }
  dprime <- z.hit - z.fa
}
# 1.6. Compute d'
#square
ses.sqr.dprime_1 <- mapply(compute.dprime, ses1.Ph.sqr, ses1.Pfa.sqr)
ses.sqr.dprime_2 <- mapply(compute.dprime, ses2.Ph.sqr, ses2.Pfa.sqr)
#random
ses.rand.dprime_1 <- mapply(compute.dprime, ses1.Ph.rand, ses1.Pfa.rand)
ses.rand.dprime_2 <- mapply(compute.dprime, ses2.Ph.rand, ses2.Pfa.rand)
# Both
ses.dprime_1 <- mapply(compute.dprime, ses1.Ph, ses1.Pfa)
ses.dprime_2 <- mapply(compute.dprime, ses2.Ph, ses2.Pfa)
ses.dprime_3 <- mapply(compute.dprime, ses3.Ph, ses3.Pfa)
ses.dprime_3[28] <- 3.090232

# 2. Retrieve RT
# 2.1. Functions for retrieving RTs
#square
square.RT <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  square.trials <- subset(subj.data, Configuration == 1 & Resp == 1 & 
                            correct == 1)
  RT.sqr <- mean(square.trials$RT)
}
#random
random.RT <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  random.trials <- subset(subj.data, Configuration == 0 & Resp == 1 & 
                            correct == 1)
  RT.rand <- mean(random.trials$RT)
}
#both
retrieve.RT <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  response.trials <- subset(subj.data, Resp == 1 & 
                              correct == 1)
  RT <- mean(response.trials$RT)
}
# 2.2. Compute RTs for each session
#square
RT.sqr_1 <- sapply(as.list(behav_ses_1), square.RT)
RT.sqr_2 <- sapply(as.list(behav_ses_2), square.RT)
#random
RT.rand_1 <- sapply(as.list(behav_ses_1), random.RT)
RT.rand_2 <- sapply(as.list(behav_ses_2), random.RT)
#both
RT_1 <- sapply(as.list(behav_ses_1), retrieve.RT)
RT_2 <- sapply(as.list(behav_ses_2), retrieve.RT)
RT_3 <- sapply(as.list(behav_ses_3), retrieve.RT)

# 3. Retrieve  intensisites
# 3.1. Function to extract intensities
# 3.1.1. All conditions
extract.intensities <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12, row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  target.trials <- subset(subj.data, target.presence == 1)
  decrement <- as.vector(target.trials$Decrement)
  intensities <- 1 - decrement
}

# 3.1.2. Square 
extract.intensities.sqr <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12, row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  target.trials.sqr <- subset(subj.data, target.presence == 1 & Configuration == 1)
  decrement.sqr <- as.vector(target.trials.sqr$Decrement)
  intensities.sqr <- 1 - decrement.sqr
}

# 3.1.3. Random
extract.intensities.rand <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12, row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  target.trials.rand <- subset(subj.data, target.presence == 1 & Configuration == 0)
  decrement.rand <- as.vector(target.trials.rand$Decrement)
  intensities.rand <- 1 - decrement.rand
}

# 3.2. Functions to extract intensity values by block
# 3.2.1. Function to extract intensities of a block
block.intensities <- function(x) {
  return(as.vector(x$Decrement))
}

# 3.2.2. Function to extract intensities of whole session, separated per block
session.intensities <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12, row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  target.trials <- subset(subj.data, target.presence == 1)
  block0 <- subset(target.trials, Block == 0)
  block1 <- subset(target.trials, Block == 1)
  block2 <- subset(target.trials, Block == 2)
  block3 <- subset(target.trials, Block == 3)
  block4 <- subset(target.trials, Block == 4)
  block5 <- subset(target.trials, Block == 5)
  block6 <- subset(target.trials, Block == 6)
  block7 <- subset(target.trials, Block == 7)
  block8 <- subset(target.trials, Block == 8)
  block9 <- subset(target.trials, Block == 9)
  list.blocks <- list(block0, block1, block2, block3, block4, block5, block6,
                      block7, block8, block9)
  # Call function defined above
  decrements <- lapply(list.blocks, block.intensities)
  intensities <- mapply('-', 1, decrements)
}

# 3.2.3. Extract block intensities for all subjects
blockintensities.1 <- lapply(behav_ses_1, session.intensities)
blockintensities.2 <- lapply(behav_ses_2, session.intensities)

# 3.3. Extract intensities for each subject by session and configuration
# 3.3.1. Square
intensities.sqr.1 <- lapply(behav_ses_1, extract.intensities.sqr)
intensities.sqr.2 <- lapply(behav_ses_2, extract.intensities.sqr)
intensities.sqr.3 <- lapply(behav_ses_3, extract.intensities.sqr)
# 3.3.1. Random
intensities.rand.1 <- lapply(behav_ses_1, extract.intensities.rand)
intensities.rand.2 <- lapply(behav_ses_2, extract.intensities.rand)
intensities.rand.3 <- lapply(behav_ses_3, extract.intensities.rand)
# 3.3.1. Both configurations
intensities.1 <- lapply(behav_ses_1, extract.intensities)
intensities.2 <- lapply(behav_ses_2, extract.intensities)
intensities.3 <- lapply(behav_ses_3, extract.intensities)

# 3.4. Extract  final thresholds for each session
# 3.4.1. Square
threshold.sqr_1 <- sapply(intensities.sqr.1, function(x) { return( x[length(x)] ) })
threshold.sqr_2 <- sapply(intensities.sqr.2, function(x) { return( x[length(x)] ) })
threshold.sqr_3 <- sapply(intensities.sqr.3, function(x) { return( x[length(x)] ) })
# 3.4.1. Random
threshold.rand_1 <- sapply(intensities.rand.1, function(x) { return( x[length(x)] ) })
threshold.rand_2 <- sapply(intensities.rand.2, function(x) { return( x[length(x)] ) })
threshold.rand_3 <- sapply(intensities.rand.3, function(x) { return( x[length(x)] ) })
# 3.4.1. Both configurations
threshold.1 <- sapply(intensities.1, function(x) { return( x[length(x)] ) })
threshold.2 <- sapply(intensities.2, function(x) { return( x[length(x)] ) })
threshold.3 <- sapply(intensities.3, function(x) { return( x[length(x)] ) })

# 4. Prepare data frame
# 4.1. Bind dprime values to data frame
rep_data3 <- cbind(rep_data2, ses.sqr.dprime_1, ses.sqr.dprime_2, ses.rand.dprime_1, 
                   ses.rand.dprime_2, ses.dprime_1, ses.dprime_2)
# 4.2. Bind RT values to data frame
rep_data4 <- cbind(rep_data3, RT.sqr_1, RT.sqr_2, RT.rand_1, RT.rand_2, RT_1, RT_2)

# 4.3. Bind threshold, intensities and accuracy values to data frame
# 4.3.1. Thresholds
rep_data4 <- cbind(rep_data4, threshold.sqr_1, threshold.sqr_2, threshold.rand_1, 
                   threshold.rand_2, threshold.1, threshold.2)
# 4.3.1. Intensities
# 4.3.1.1. Both configurations
rep_data4$intensities.1 <- intensities.1
rep_data4$intensities.2 <- intensities.2
# 4.3.1.2. Square
rep_data4$intensities.sqr.1 <- intensities.sqr.1
rep_data4$intensities.sqr.2 <- intensities.sqr.2
# 4.3.1.3. Random
rep_data4$intensities.rand.1 <- intensities.rand.1
rep_data4$intensities.rand.2 <- intensities.rand.2
# 4.3.2. Accuracies
# 4.3.2.1. Both configurations
rep_data4$accuracies.1 <- accuracies.1
rep_data4$accuracies.2 <- accuracies.2
# 4.3.2.2. Square
rep_data4$accuracies.sqr.1 <- accuracies.sqr.1
rep_data4$accuracies.sqr.2 <- accuracies.sqr.2
# 4.3.2.3. Random
rep_data4$accuracies.rand.1 <- accuracies.rand.1
rep_data4$accuracies.rand.2 <- accuracies.rand.2


# 4.4.Convert from wide to long
# Extracts session number to column
rep_data_long <- reshape(rep_data4, varying = c("C1.sqr_1", "C1.sqr_2", "C1.rand_1",
                                                "C1.rand_2", "P1.sqr_1", "P1.sqr_2", 
                                                "P1.rand_1", "P1.rand_2", "N1.sqr_1", 
                                                "N1.sqr_2", "N1.rand_1", "N1.rand_2",
                                                "occ.sqr.nd1_1", "occ.sqr.nd2_1",
                                                "occ.sqr.nd1_2", "occ.sqr.nd2_2",
                                                "occ.rand.nd1_1", "occ.rand.nd2_1",
                                                "occ.rand.nd1_2", "occ.rand.nd2_2",
                                                "left.sqr.nd1_1", "left.sqr.nd2_1",
                                                "left.sqr.nd1_2", "left.sqr.nd2_2",
                                                "left.rand.nd1_1", "left.rand.nd2_1", 
                                                "left.rand.nd1_2", "left.rand.nd2_2",
                                                "right.sqr.nd1_1", "right.sqr.nd2_1",
                                                "right.sqr.nd1_2", "right.sqr.nd2_2",
                                                "right.rand.nd1_1", "right.rand.nd2_1",
                                                "right.rand.nd1_2", "right.rand.nd2_2",
                                                "RL.sqr.nd1_1", "RL.sqr.nd2_1", 
                                                "RL.sqr.nd1_2", "RL.sqr.nd2_2",
                                                "RL.rand.nd1_1", "RL.rand.nd2_1", 
                                                "RL.rand.nd1_2", "RL.rand.nd2_2",
                                                "N2.sqr_1", "N2.sqr_2", "N2.rand_1", 
                                                "N2.rand_2", "LP.sqr_1", "LP.sqr_2", 
                                                "LP.rand_1", "LP.rand_2",
                                                "ses.sqr.dprime_1", "ses.sqr.dprime_2",
                                                "ses.rand.dprime_1", "ses.rand.dprime_2",
                                                "RT.sqr_1", "RT.sqr_2",
                                                "RT.rand_1", "RT.rand_2",
                                                "threshold.sqr_1", "threshold.sqr_2",
                                                "threshold.rand_1", "threshold.rand_2"),  
                         direction = "long", idvar = "Subject", sep = "_")
# Rename session column
names(rep_data_long)[names(rep_data_long) == "time"] <- "session"

# Rename columns to separate configuration number using sep = "_"
names(rep_data_long)[names(rep_data_long) == "C1.sqr"] <- "C1_1"
names(rep_data_long)[names(rep_data_long) == "C1.rand"] <- "C1_2"
names(rep_data_long)[names(rep_data_long) == "P1.sqr"] <- "P1_1"
names(rep_data_long)[names(rep_data_long) == "P1.rand"] <- "P1_2"
names(rep_data_long)[names(rep_data_long) == "N1.sqr"] <- "N1_1"
names(rep_data_long)[names(rep_data_long) == "N1.rand"] <- "N1_2"
names(rep_data_long)[names(rep_data_long) == "occ.sqr.nd1"] <- "occ.nd1_1"
names(rep_data_long)[names(rep_data_long) == "occ.sqr.nd2"] <- "occ.nd2_1"
names(rep_data_long)[names(rep_data_long) == "occ.rand.nd1"] <- "occ.nd1_2"
names(rep_data_long)[names(rep_data_long) == "occ.rand.nd2"] <- "occ.nd2_2"
names(rep_data_long)[names(rep_data_long) == "left.sqr.nd1"] <- "left.nd1_1"
names(rep_data_long)[names(rep_data_long) == "left.sqr.nd2"] <- "left.nd2_1"
names(rep_data_long)[names(rep_data_long) == "left.rand.nd1"] <- "left.nd1_2"
names(rep_data_long)[names(rep_data_long) == "left.rand.nd2"] <- "left.nd2_2"
names(rep_data_long)[names(rep_data_long) == "right.sqr.nd1"] <- "right.nd1_1"
names(rep_data_long)[names(rep_data_long) == "right.sqr.nd2"] <- "right.nd2_1"
names(rep_data_long)[names(rep_data_long) == "right.rand.nd1"] <- "right.nd1_2"
names(rep_data_long)[names(rep_data_long) == "right.rand.nd2"] <- "right.nd2_2"
names(rep_data_long)[names(rep_data_long) == "RL.sqr.nd1"] <- "RL.nd1_1"
names(rep_data_long)[names(rep_data_long) == "RL.sqr.nd2"] <- "RL.nd2_1"
names(rep_data_long)[names(rep_data_long) == "RL.rand.nd1"] <- "RL.nd1_2"
names(rep_data_long)[names(rep_data_long) == "RL.rand.nd2"] <- "RL.nd2_2"
names(rep_data_long)[names(rep_data_long) == "N2.sqr"] <- "N2_1"
names(rep_data_long)[names(rep_data_long) == "N2.rand"] <- "N2_2"
names(rep_data_long)[names(rep_data_long) == "LP.sqr"] <- "LP_1"
names(rep_data_long)[names(rep_data_long) == "LP.rand"] <- "LP_2"
names(rep_data_long)[names(rep_data_long) == "ses.sqr.dprime"] <- "ses.dprime_1"
names(rep_data_long)[names(rep_data_long) == "ses.rand.dprime"] <- "ses.dprime_2"
names(rep_data_long)[names(rep_data_long) == "RT.sqr"] <- "RT_1"
names(rep_data_long)[names(rep_data_long) == "RT.rand"] <- "RT_2"
names(rep_data_long)[names(rep_data_long) == "threshold.sqr"] <- "threshold_1"
names(rep_data_long)[names(rep_data_long) == "threshold.rand"] <- "threshold_2"
# Extracts configuration number to column
rep_data_long2 <- reshape(rep_data_long, varying = c("C1_1", "C1_2", "P1_1", "P1_2",
                                                     "N1_1", "N1_2", "occ.nd1_1", 
                                                     "occ.nd2_1", "occ.nd1_2", 
                                                     "occ.nd2_2", "left.nd1_1", 
                                                     "left.nd2_1", "left.nd1_2", 
                                                     "left.nd2_2", "right.nd1_1", 
                                                     "right.nd2_1", "right.nd1_2", 
                                                     "right.nd2_2", "RL.nd1_1", 
                                                     "RL.nd2_1", "RL.nd1_2", 
                                                     "RL.nd2_2", "N2_1", "N2_2",
                                                     "LP_1", "LP_2",
                                                     "ses.dprime_1", "ses.dprime_2",
                                                     "RT_1", "RT_2",
                                                     "threshold_1", "threshold_2"),  
                          direction = "long", idvar = " Subject", sep = "_")
# Rename configuration column name
names(rep_data_long2)[names(rep_data_long2) == "time"] <- "configuration"
# Rename configuration levels
rep_data_long2$configuration[rep_data_long2$configuration == 1] <- "sqr"
rep_data_long2$configuration[rep_data_long2$configuration == 2] <- "rand"
# Rename d' column
names(rep_data_long2)[names(rep_data_long2) == "ses.dprime"] <- "dprime"

# Convert configuration, group and session to factors
rep_data_long2$group <- factor(rep_data_long2$group)
rep_data_long2$group.original <- factor(rep_data_long2$group.original)
rep_data_long2$session <- factor(rep_data_long2$session)
rep_data_long2$configuration <- factor(rep_data_long2$configuration)

# Threshold
threshold.lineplot <- ggplot(rep_data_long2, aes(x = group.original, 
                                                 y = threshold,
                                                 colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "Quest Threshold", x = "group", y = "Threshold", 
       colour = "configuration") 

# 2. Compare RT across conditions
contrasts(rep_data_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long2$group.original) <- c(-1, 1) # setting contrasts for group.original
RT_baseline <- lme(RT ~ 1, random = ~1|Subject/configuration/session, 
                   data = rep_data_long2, method = "ML") #baseline
RT_config <- update(RT_baseline, .~. + configuration)
RT_session <- update(RT_config, .~. + session)
RT_group.original <- update(RT_session, .~. + group.original)
RT_config_session <- update(RT_group.original, .~. + configuration:session)
RT_session_group.original <- update(RT_config_session, .~. + session:group.original)
RT_config_group.original <- update(RT_session_group.original, .~. + configuration:group.original)
RT_lme <- update(RT_config_group.original, .~. + configuration:session:group.original)
anova(RT_baseline, RT_config, RT_session, RT_group.original, 
      RT_config_session, RT_session_group.original, RT_config_group.original, 
      RT_lme)

#---------------------------Psychophysical analysis-------------------------
# RT
summary(rep_data4$RT_1)
summary(rep_data4$RT_2)
#Scatterplot
plot(rep_data4$Subject, rep_data4$RT_1, main = "RT in session 1",
     xlab = "RT", pch = 16, col = 6)
# Histograms
hist(rep_data4$RT_1, main = "RT in session 1",
     xlab = "RT", col = 7)

hist(rep_data4$RT_2, main = "RT in session 1",
     xlab = "RT", col = 12)

# Function to bind values for each subject
nested.columns <- function(adata, avector, aname) {
  adata$column <- avector
}

nested.data <- function(nested.list) {
  return(lapply(nested.columns, nested.list, cbind))
  blocksnames <- paste("block.intensities.", seq_along(0:9), sep = "")
}

# Create data frame for intensities
intensities.data <- data.frame()
intensities.columsn <- data.frame()
intensities.data <- rbind(blockintensities.1[[1]], blockintensities.1[[2]], 
                          blockintensities.1[[3]], blockintensities.1[[4]], 
                          blockintensities.1[[5]], blockintensities.1[[6]], 
                          blockintensities.1[[7]], blockintensities.1[[8]], 
                          blockintensities.1[[9]], blockintensities.1[[10]])
colnames(intensities.data) <- c("block.intensities.0", "block.intensities.1", 
                                "block.intensities.2", "block.intensities.3", 
                                "block.intensities.4", "block.intensities.5", 
                                "block.intensities.6", "block.intensities.7",  
                                "block.intensities.8", "block.intensities.9")

# Bind to dataset
rep_data4 <- cbind(rep_data4, intensities.data)

rep_data4$group <- factor(rep_data4$group)
rep_data4$group.original <- factor(rep_data4$group.original)

# 1. Plot intensities
# 1.1. Function to plot intensities
plot.intensities <- function(intlist, subject) {
  plot(seq(1:length(intlist[[subject]])), intlist[[subject]], ylim = c(-0.5, 0.5),
       xlab = "Trial", ylab = "Intensities", main = "Intensities by trial", type = 'l')
}

# 1.2. Plot intensities for all subjects
pdf('timeseries.pdf')
for (x in 1:32) {
  par(new = TRUE)
  plot(seq(1:length(intensities.1[[x]])), intensities.1[[x]], ylim = c(-0.5, 0.5),
       xlab = "Trial", ylab = "Intensities", main = "Intensities by trial", type = 'l',
       col = x)
}
dev.off()
par(new = FALSE)


# 2. Plot block end intensities values
plot(x = rep_data4$Subject, y = rep_data4$threshold.1, ylim = c(-0.5, 0.5), 
     col = rep_data4$group.original, pch = 16, main = "Main task thresholds",
     xlab = "Subject", ylab = "Threshold")
legend(0, -0.2, legend = levels(rep_data4$group.original), 
       col = c("black", "red"),
       pch = 16)
abline(h = mean(rep_data4[rep_data4$group.original == 'aware', ]$threshold.1))
abline(h = mean(rep_data4[rep_data4$group.original == 'unaware', ]$threshold.1), 
       col = 'red')

# 3. Median split analysis of ERPs
# 3.1. Compute medians
low.threshold.group <- rep_data4[which(rep_data4$threshold.1 < 
                                         median(rep_data4$threshold.1)),]$Subject
high.threshold.group <- rep_data4[which(rep_data4$threshold.1 > 
                                          median(rep_data4$threshold.1)),]$Subject

# 3.2. Create factor for median split group
rep_data4$split.group <- numeric(32)
rep_data4[which(rep_data4$Subject %in% 
                  low.threshold.group),]$split.group <- 'low.thresh'
rep_data4[which(rep_data4$Subject %in% 
                  high.threshold.group),]$split.group <- 'high.thresh'
rep_data4$split.group <- factor(rep_data4$split.group)

# 3.3. Plot median split group x awareness group
table(rep_data4$split.group, rep_data4$group.original)
plot(rep_data4$split.group, rep_data4$group.original, main = "Median split group x awareness",
     xlab = "median split group", ylab = "awareness group")

# 3.4. Chi-square
CrossTable(rep_data4$split.group, rep_data4$group.original, fisher = TRUE,
           chisq = TRUE, expected = TRUE, sresid = TRUE, format = 'SPSS')

# 6. Nd2 ERPs between median-split groups
rep_data_long2$split.group <- numeric(nrow(rep_data_long2))
rep_data_long2[which(rep_data_long2$Subject %in% 
                       low.threshold.group),]$split.group <- 'low.thresh'
rep_data_long2[which(rep_data_long2$Subject %in% 
                       high.threshold.group),]$split.group <- 'high.thresh'
rep_data_long2$split.group <- factor(rep_data_long2$split.group)
t.test(RL.nd2 ~ split.group, data = rep_data_long2)

# 3.6. Compare nd2 across conditions
# 3.6.1. Line plot
# 3.6.1.1. Left nd2
leftnd2.split.lineplot <- ggplot(rep_data_long2, aes(x = split.group, 
                                                     y = left.nd2,
                                                     colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "Left nd2 x median split group", x = "Median split group", 
       y = "Left nd2", 
       colour = "configuration")

# 3.6.1.1. Right nd2
rightnd2.split.lineplot <- ggplot(rep_data_long2, aes(x = split.group, 
                                                      y = right.nd2,
                                                      colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "right nd2 x median split group", x = "Median split group", 
       y = "right nd2", 
       colour = "configuration")

# 3.6.2. ANOVA
# 3.6.2.1. Left nd2
contrasts(rep_data_long2$configuration) <- c(-1, 1) # contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # contrasts for session
contrasts(rep_data_long2$split.group) <- c(-1, 1) # contrasts for split.group
left.nd2_baseline <- lme(left.nd2 ~ 1, random = ~1|Subject/configuration/session, 
                         data = rep_data_long2, method = "ML") #baseline
left.nd2_config <- update(left.nd2_baseline, .~. + configuration)
left.nd2_session <- update(left.nd2_config, .~. + session)
left.nd2_split.group <- update(left.nd2_session, .~. + split.group)
left.nd2_config_session <- update(left.nd2_split.group, .~. + configuration:session)
left.nd2_session_split.group <- update(left.nd2_config_session, .~. + 
                                         session:split.group)
left.nd2_config_split.group <- update(left.nd2_session_split.group, .~. + 
                                        configuration:split.group)
left.nd2_lme <- update(left.nd2_config_split.group, .~. + 
                         configuration:session:split.group)
anova(left.nd2_baseline, left.nd2_config, left.nd2_session, left.nd2_split.group, 
      left.nd2_config_session, left.nd2_session_split.group, 
      left.nd2_config_split.group, left.nd2_lme)

# 3.6.2.1. Right nd2
contrasts(rep_data_long2$configuration) <- c(-1, 1) # contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # contrasts for session
contrasts(rep_data_long2$split.group) <- c(-1, 1) # contrasts for split.group
right.nd2_baseline <- lme(right.nd2 ~ 1, random = ~1|Subject/configuration/session, 
                          data = rep_data_long2, method = "ML") #baseline
right.nd2_config <- update(right.nd2_baseline, .~. + configuration)
right.nd2_session <- update(right.nd2_config, .~. + session)
right.nd2_split.group <- update(right.nd2_session, .~. + split.group)
right.nd2_config_session <- update(right.nd2_split.group, .~. + configuration:session)
right.nd2_session_split.group <- update(right.nd2_config_session, .~. + 
                                          session:split.group)
right.nd2_config_split.group <- update(right.nd2_session_split.group, .~. + 
                                         configuration:split.group)
right.nd2_lme <- update(right.nd2_config_split.group, .~. + 
                          configuration:session:split.group)
anova(right.nd2_baseline, right.nd2_config, right.nd2_session, right.nd2_split.group, 
      right.nd2_config_session, right.nd2_session_split.group, 
      right.nd2_config_split.group, right.nd2_lme)

# 4. Psychometrical curve fitting
length.int <- lapply(intensities.1, length)
psychsubjects <- mapply(rep, seq_along(intensities.1), length.int)
psychData <- data.frame('subjects' =  unlist(psychsubjects), 'intensities' = unlist(intensities.1), 
                        'accuracy' = unlist(accuracies.1))
psychData$subjects <- factor(psychData$subjects)
psychometricFit <- quickpsy(psychData, x = intensities, k = accuracy,
                            grouping = .(subjects))

#---------------------------------Questionnaire data------------------------------
# 1. Prepare data
# 1.1. Bind questionnaire data to experiment data
questionnaire.ERPs <- cbind(rep_data4, questionnaire.ses1, questionnaire.ses2)

# 1.6. Coerce groups as factors
questionnaire.ERPs$group <- factor(questionnaire.ERPs$group)
questionnaire.ERPs$group.original <- factor(questionnaire.ERPs$group.original)

# 1. Variances and means
summary(questionnaire.ERPs[,c(25:30)])
lapply(questionnaire.ERPs[,c(25:30)], sd)

by(questionnaire.ERPs[,25:30], questionnaire.ERPs$group, 
   stat.desc, basic = FALSE)


# Heat maps - questionnaire measures
# Session 1
cross.Freq <- count(questionnaire.ERPs, freq.4.ses1, conf.4.ses1)
kable(head(cross.Freq))
conf.x.freq <- ggplot(cross.Freq, aes(conf.4.ses1, freq.4.ses1)) +
  geom_tile(aes(fill = n), colour = "black") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Confidence x frequency session 1", x = "Confidence", y = "Frequency") 

# Histograms - questionnaire measures
par(mfrow = c(1, 2))
# session 1
hist(questionnaire.ERPs$conf.4.ses1, main = "Confidence rating square session 1",
     xlab = "Confidence rated", col = "red", breaks = c(0,1,2,3,4,5))
hist(questionnaire.ERPs$freq.4.ses1, main = "Frequency rating square session 1",
     xlab = "Frequency rated", col = "green", breaks = c(0,1,2,3,4,5))
# session 2
hist(questionnaire.ERPs$conf.4.ses2, main = "Confidence ratings square session 2",
     xlab = "Confidence rated", col = "red", breaks = c(0,1,2,3,4,5))
hist(questionnaire.ERPs$freq.4.ses2, main = "Frequency ratings square session 1",
     xlab = "Frequency rated", col = "green", breaks = c(0,1,2,3,4,5))

# 1.3. Test correlations between awareness ratings
# 1.3.1. Session 1
cor.test(questionnaire.ERPs$conf.4.ses1, questionnaire.ERPs$freq.4.ses1, 
         method = "pearson")
# 1.3.2. session 2
cor.test(questionnaire.ERPs$conf.4.ses2, questionnaire.ERPs$freq.4.ses2, 
         method = "pearson")

boxplot(RT_1 ~ group, questionnaire.ERPs)

# 2. Compute Correlational measures
# 2.1. Combined measure of awareness: RT x confidence ratings
# 2.1.1. Session 1
questionnaire.ERPs$aware.index.1 <- questionnaire.ERPs$RT_1 * 
  questionnaire.ERPs$conf.4.ses1
# 2.1.2. Session 2
questionnaire.ERPs$aware.index.2 <- questionnaire.ERPs$RT_2 * 
  questionnaire.ERPs$conf.4.ses2

# 2.1.3. Scatterplot - aware index session 1
plot(x = questionnaire.ERPs$Subject, y = questionnaire.ERPs$aware.index.1, 
     col = questionnaire.ERPs$group.original, pch = 16, main = "Awareness index session 1",
     xlab = "Subject", ylab = "Awareness index")
legend(2, 0, legend = levels(questionnaire.ERPs$group.original), 
       col = c("black", "red"),
       pch = 16)

# 2.1.4. Line plots with error bars - Aware index per group
aware.index.1.lines <- ggplot(questionnaire.ERPs, aes(x = group.original, 
                                                      y = aware.index.1)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2)
labs(title = "Aware index session 1", x = "group", y = "aware index")

# 2.1.5. Scatterplot - aware index session 2
plot(questionnaire.ERPs$Subject, questionnaire.ERPs$aware.index.2, 
     col = questionnaire.ERPs$group.original, pch = 16, 
     main = "aware index session 1")
legend(2,1.1, legend = levels(questionnaire.ERPs$group.original), 
       col = c("black", "red"),
       pch = 16)

# 2.2. Combined measure of awareness: RT x threshold value
# 2.2.1. Session 1
questionnaire.ERPs$aware.threshold.1 <- questionnaire.ERPs$threshold.1 * 
  questionnaire.ERPs$conf.4.ses1
# 2.2.1. Session 2
questionnaire.ERPs$aware.threshold.1 <- questionnaire.ERPs$threshold.1 * 
  questionnaire.ERPs$conf.4.ses1


# 2.2 Compute RT means across sessions
questionnaire.ERPs$RT_1_2 <- rowMeans(questionnaire.ERPs[,19:20])

# 2.3. Compute differences
# session 1
questionnaire.ERPs$occ_diff_1 <- questionnaire.ERPs$occ.sqr.nd1_1 - 
  questionnaire.ERPs$occ.rand.nd1_1
questionnaire.ERPs$left_diff_1 <- questionnaire.ERPs$left.sqr.nd2_1 - 
  questionnaire.ERPs$left.rand.nd2_1
questionnaire.ERPs$right_diff_1 <- questionnaire.ERPs$right.sqr.nd2_1 - 
  questionnaire.ERPs$right.rand.nd2_1
# session 2
questionnaire.ERPs$occ_diff_2 <- questionnaire.ERPs$occ.sqr.nd1_2 - 
  questionnaire.ERPs$occ.rand.nd1_2
questionnaire.ERPs$left_diff_2 <- questionnaire.ERPs$left.sqr.nd2_2 - 
  questionnaire.ERPs$left.rand.nd2_2
questionnaire.ERPs$right_diff_2 <- questionnaire.ERPs$right.sqr.nd2_2 - 
  questionnaire.ERPs$right.rand.nd2_2

# 3. Correlations: ERP differences x awareness measures
# 3.1. ERP Differences x confidence ratings
# 3.1.1. Both groups
# 3.1.1.1. Session 1
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ratings1$conf.4.ses1, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ratings1$conf.4.ses1, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ratings1$conf.4.ses1, 
         method = "pearson")

# 3.1.1.2. session 2
cor.test(questionnaire.ERPs$occ_diff_2, questionnaire.ratings1$conf.4.ses2, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_2, questionnaire.ratings1$conf.4.ses2, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_2, questionnaire.ratings1$conf.4.ses2, 
         method = "pearson")

# 3.1.2. By group
# 3.1.2.1. Session 1
cor.test(aware$conf.4.ses1, aware$occ_diff_1)
cor.test(aware$conf.4.ses1, aware$left_diff_1)
cor.test(aware$conf.4.ses1, aware$right_diff_1)
cor.test(unaware$conf.4.ses1, unaware$occ_diff_1)
cor.test(unaware$conf.4.ses1, unaware$left_diff_1)
cor.test(unaware$conf.4.ses1, unaware$right_diff_1)
# 3.1.2.1. Session 2
cor.test(aware$conf.4.ses2, aware$occ_diff_2)
cor.test(aware$conf.4.ses2, aware$left_diff_2)
cor.test(aware$conf.4.ses2, aware$right_diff_2)
cor.test(unaware$conf.4.ses2, unaware$occ_diff_2)
cor.test(unaware$conf.4.ses2, unaware$left_diff_2)
cor.test(unaware$conf.4.ses2, unaware$right_diff_2)

# 3.1. ERP Differences x frequency ratings
# 3.1.1. Both groups
# 3.1.1.1. Session 1
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ERPs$freq.4.ses1, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ERPs$freq.4.ses1, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ERPs$freq.4.ses1, 
         method = "pearson")
# 3.1.1.2. session 2
cor.test(questionnaire.ERPs$occ_diff_2, questionnaire.ERPs$freq.4.ses2, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_2, questionnaire.ERPs$freq.4.ses2, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_2, questionnaire.ERPs$freq.4.ses2, 
         method = "pearson")

# 3.2.2. By group
# 3.2.2.1. Session 1
cor.test(aware$freq.4.ses1, aware$occ_diff_1)
cor.test(aware$freq.4.ses1, aware$left_diff_1)
cor.test(aware$freq.4.ses1, aware$right_diff_1)
cor.test(unaware$freq.4.ses1, unaware$occ_diff_1)
cor.test(unaware$freq.4.ses1, unaware$left_diff_1)
cor.test(unaware$freq.4.ses1, unaware$right_diff_1)
# 3.2.2.1. Session 2
cor.test(aware$freq.4.ses2, aware$occ_diff_2)
cor.test(aware$freq.4.ses2, aware$left_diff_2)
cor.test(aware$freq.4.ses2, aware$right_diff_2)
cor.test(unaware$freq.4.ses2, unaware$occ_diff_2)
cor.test(unaware$freq.4.ses2, unaware$left_diff_2)
cor.test(unaware$freq.4.ses2, unaware$right_diff_2)

# 3.2. ERP Differences x combined awareness index
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ERPs$aware.index.1, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ERPs$aware.index.1, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ERPs$aware.index.1, 
         method = "pearson")

#------------------------------------Correlations----------------------------
# 1. Behavioral data correlations matrix
ggpairs(questionnaire.ratings1, mapping = aes(color=Recall))

# 1.2. Group subsets
aware <- subset(questionnaire.ERPs, group.original == 'aware')
unaware <- subset(questionnaire.ERPs, group.original == 'unaware')



# 2. Correlations: psychophysical x awareness measures
# 2.1. d' x Confidence ratings - session 1
cor.test(questionnaire.ERPs$ses.dprime_1, questionnaire.ERPs$conf.4.ses1, 
         method = "pearson")
cor.test(questionnaire.ERPs$RT_1, questionnaire.ERPs$conf.4.ses1, 
         method = "pearson")

# 2.2. d' x Frequency ratings - session 1
cor.test(questionnaire.ERPs$ses.dprime_1, questionnaire.ERPs$freq.4.ses1, 
         method = "pearson")
cor.test(questionnaire.ERPs$RT_1, questionnaire.ERPs$freq.4.ses1, 
         method = "pearson")

# 2.3. RT x confidence ratings by group
# 2.3.1. Session 1
cor.test(aware$RT_1, aware$conf.4.ses1)
cor.test(unaware$RT_1, unaware$conf.4.ses1)
# 2.3.1. Session 2
cor.test(aware$RT_2, aware$conf.4.ses2)
cor.test(unaware$RT_2, unaware$conf.4.ses2)



# 4. Correlations: ERP differences x psychophysical measures
# 4.1. Correlations: ERP Differences x reaction time
# 4.1.1. Both groups
# 4.1.1.1 Session 1
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ERPs$RT_1, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ERPs$RT_1, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ERPs$RT_1, 
         method = "pearson")
# 4.1.1.2 Session 2
cor.test(questionnaire.ERPs$occ_diff_2, questionnaire.ERPs$RT_2, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_2, questionnaire.ERPs$RT_2, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_2, questionnaire.ERPs$RT_2, 
         method = "pearson")
# 4.1.1.3 Average
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ERPs$RT_1_2, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ERPs$RT_1_2, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ERPs$RT_1_2, 
         method = "pearson")

# 4.1.2. By group
# 4.1.2.1. Session 1
cor.test(aware$RT_1, aware$occ_diff_1)
cor.test(aware$RT_1, aware$left_diff_1)
cor.test(aware$RT_1, aware$right_diff_1)
cor.test(unaware$RT_1, unaware$occ_diff_1)
cor.test(unaware$RT_1, unaware$left_diff_1)
cor.test(unaware$RT_1, unaware$right_diff_1)
# 4.1.2.1. Session 2
cor.test(aware$RT_2, aware$occ_diff_2)
cor.test(aware$RT_2, aware$left_diff_2)
cor.test(aware$RT_2, aware$right_diff_2)
cor.test(unaware$RT_2, unaware$occ_diff_2)
cor.test(unaware$RT_2, unaware$left_diff_2)
cor.test(unaware$RT_2, unaware$right_diff_2)

# 4.2. ERP Differences x d'
# 4.2.1. Both groups
# 4.2.1.1. Session 1
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")
# 4.2.1.2. Session 2
cor.test(questionnaire.ERPs$occ_diff_2, questionnaire.ERPs$ses.dprime_2, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_2, questionnaire.ERPs$ses.dprime_2, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_2, questionnaire.ERPs$ses.dprime_2, 
         method = "pearson")

# 4.2.2. By group
# 4.1.2.1. Session 1
cor.test(aware$ses.dprime_1, aware$occ_diff_1)
cor.test(aware$ses.dprime_1, aware$left_diff_1)
cor.test(aware$ses.dprime_1, aware$right_diff_1)
cor.test(unaware$ses.dprime_1, unaware$occ_diff_1)
cor.test(unaware$ses.dprime_1, unaware$left_diff_1)
cor.test(unaware$ses.dprime_1, unaware$right_diff_1)
# 4.1.2.1. Session 2
cor.test(aware$ses.dprime_2, aware$occ_diff_2)
cor.test(aware$ses.dprime_2, aware$left_diff_2)
cor.test(aware$ses.dprime_2, aware$right_diff_2)
cor.test(unaware$ses.dprime_2, unaware$occ_diff_2)
cor.test(unaware$ses.dprime_2, unaware$left_diff_2)
cor.test(unaware$ses.dprime_2, unaware$right_diff_2)

# 4.3. ERP Differences x % correct - FINISH!
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")

# 4.3. ERP Differences x thresolds - FINISH!
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")

#--------------------------------Comparisons--------------------------------------
# 1. Compare d' across conditions
contrasts(rep_data_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long2$group.original) <- c(-1, 1) # setting contrasts for group.original
dprime_baseline <- lme(dprime ~ 1, random = ~1|Subject/configuration/session, 
                       data = rep_data_long2, method = "ML") #baseline
dprime_config <- update(dprime_baseline, .~. + configuration)
dprime_session <- update(dprime_config, .~. + session)
dprime_group.original <- update(dprime_session, .~. + group.original)
dprime_config_session <- update(dprime_group.original, .~. + configuration:session)
dprime_session_group.original <- update(dprime_config_session, .~. + session:group.original)
dprime_config_group.original <- update(dprime_session_group.original, .~. + configuration:group.original)
dprime_lme <- update(dprime_config_group.original, .~. + configuration:session:group.original)
anova(dprime_baseline, dprime_config, dprime_session, dprime_group.original, 
      dprime_config_session, dprime_session_group.original, dprime_config_group.original, 
      dprime_lme)

# 2. Compare RT across conditions
contrasts(rep_data_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long2$group.original) <- c(-1, 1) # setting contrasts for group.original
RT_baseline <- lme(RT ~ 1, random = ~1|Subject/configuration/session, 
                   data = rep_data_long2, method = "ML") #baseline
RT_config <- update(RT_baseline, .~. + configuration)
RT_session <- update(RT_config, .~. + session)
RT_group.original <- update(RT_session, .~. + group.original)
RT_config_session <- update(RT_group.original, .~. + configuration:session)
RT_session_group.original <- update(RT_config_session, .~. + session:group.original)
RT_config_group.original <- update(RT_session_group.original, .~. + configuration:group.original)
RT_lme <- update(RT_config_group.original, .~. + configuration:session:group.original)
anova(RT_baseline, RT_config, RT_session, RT_group.original, 
      RT_config_session, RT_session_group.original, RT_config_group.original, 
      RT_lme)

# 3. Compare threshold across conditions
contrasts(rep_data_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long2$group.original) <- c(-1, 1) # setting contrasts for group.original
threshold_baseline <- lme(threshold ~ 1, random = ~1|Subject/configuration/session, 
                          data = rep_data_long2, method = "ML") #baseline
threshold_config <- update(threshold_baseline, .~. + configuration)
threshold_session <- update(threshold_config, .~. + session)
threshold_group.original <- update(threshold_session, .~. + group.original)
threshold_config_session <- update(threshold_group.original, .~. + configuration:session)
threshold_session_group.original <- update(threshold_config_session, .~. + session:group.original)
threshold_config_group.original <- update(threshold_session_group.original, .~. + configuration:group.original)
threshold_lme <- update(threshold_config_group.original, .~. + configuration:session:group.original)
anova(threshold_baseline, threshold_config, threshold_session, threshold_group.original, 
      threshold_config_session, threshold_session_group.original, threshold_config_group.original, 
      threshold_lme)

# 4. Thresholds.1 across conditions
t.test(thresholds.1 ~ group.original, data = questionnaire.ERPs)
t.test(thresholds.2 ~ group.original, data = questionnaire.ERPs)

# 5. Awareness ratings between session ins unaware group
# 5.1. Compare means in confidence ratings
t.test(questionnaire.ERPs$conf.4.ses1, questionnaire.ERPs$conf.4.ses2, paired = TRUE)
# 5.2. Compute effect sizes
cohen.d(questionnaire.ERPs$conf.4.ses1, questionnaire.ERPs$conf.4.ses2, paired = TRUE)

# 5.3. Compare means in frequency ratings
t.test(questionnaire.ERPs$freq.4.ses1, questionnaire.ERPs$freq.4.ses2, paired = TRUE)
# 5.2. Compute effect sizes
cohen.d(questionnaire.ERPs$freq.4.ses1, questionnaire.ERPs$freq.4.ses2, paired = TRUE)

# 5.1. Compare means in awareness index
t.test(questionnaire.ERPs$aware.index.1, questionnaire.ERPs$aware.index.2, 
       paired = TRUE)
# 5.2. Compute effect sizes
cohen.d(questionnaire.ERPs$aware.index.1, questionnaire.ERPs$aware.index.2, 
        paired = TRUE)


