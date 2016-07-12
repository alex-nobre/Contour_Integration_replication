library(pastecs)
library(ggplot2)
library(nlme)
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

#-----------------------------------Prepare data----------------------------
# Get list of psychopy file names
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
# 2.1. Functions for retrieving RT
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

# 3.2. Extract intensities for each subject by session and configuration
# 3.2.1. Square
intensities.sqr.1 <- lapply(behav_ses_1, extract.intensities.sqr)
intensities.sqr.2 <- lapply(behav_ses_2, extract.intensities.sqr)
intensities.sqr.3 <- lapply(behav_ses_3, extract.intensities.sqr)
# 3.2.1. Random
intensities.rand.1 <- lapply(behav_ses_1, extract.intensities.rand)
intensities.rand.2 <- lapply(behav_ses_2, extract.intensities.rand)
intensities.rand.3 <- lapply(behav_ses_3, extract.intensities.rand)
# 3.2.1. Both configurations
intensities.1 <- lapply(behav_ses_1, extract.intensities)
intensities.2 <- lapply(behav_ses_2, extract.intensities)
intensities.3 <- lapply(behav_ses_3, extract.intensities)

# 3.3. Extract  final thresholds for each session
# 3.3.1. Square
threshold.sqr_1 <- sapply(intensities.sqr.1, function(x) { return( x[length(x)] ) })
threshold.sqr_2 <- sapply(intensities.sqr.2, function(x) { return( x[length(x)] ) })
threshold.sqr_3 <- sapply(intensities.sqr.3, function(x) { return( x[length(x)] ) })
# 3.3.1. Random
threshold.rand_1 <- sapply(intensities.rand.1, function(x) { return( x[length(x)] ) })
threshold.rand_2 <- sapply(intensities.rand.2, function(x) { return( x[length(x)] ) })
threshold.rand_3 <- sapply(intensities.rand.3, function(x) { return( x[length(x)] ) })
# 3.3.1. Both configurations
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

length.int <- lapply(intensities.1, length)
psychsubjects <- mapply(rep, seq_along(intensities.1), length.int)
psychData <- data.frame('subjects' =  unlist(psychsubjects), 'intensities' = unlist(intensities.1), 
                        'accuracy' = unlist(accuracies.1))
psychData$subjects <- factor(psychData$subjects)
psychometricFit <- quickpsy(psychData, x = intensities, k = accuracy,
                            grouping = .(subjects))


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


#---------------------------Prepare questionnaire data---------------------------
# 1. Import questionnaire data and rename columns
questionnaire.ses1 <- read.xlsx(paste('./Data/Questionnaire_data/', "Questionnaire data ses1.xlsx", sep = ""), 
                                sheetName = "Questionnaire data", header = TRUE)
questionnaire.ses2 <- read.xlsx(paste('./Data/Questionnaire_data/', "Questionnaire data ses2.xlsx", sep = ""), 
                                sheetName = "Questionnaire data", header = TRUE)
questionnaire.ses1 <- questionnaire.ses1[-c(25,26),]
questionnaire.ses2 <- questionnaire.ses2[-c(25,26),]
questionnaire.ses1[,c(1,16,17)] <- NULL # exclude subject, group & annotations columns
questionnaire.ses2[,c(1,16,17)] <- NULL # exclude subject, group & annotations columns
colnames(questionnaire.ses1) <- c("Recall.ses1",	"Block.ses1", "4.1.ses1",	"4.2.ses1", "4.3.ses1",	
                             "4.4.ses1",	"4.5.ses1",	"4.6.ses1",	"5.1.ses1",	"5.2.ses1",	"5.3.ses1",	
                             "5.4.ses1",	"5.5.ses1",	"5.6.ses1")
colnames(questionnaire.ses2) <- c("Recall.ses2",	"Block.ses2", "4.1.ses2",	"4.2.ses2", "4.3.ses2",	
                                  "4.4.ses2",	"4.5.ses2",	"4.6.ses2",	"5.1.ses2",	"5.2.ses2",	"5.3.ses2",	
                                  "5.4.ses2",	"5.5.ses2",	"5.6.ses2")
questionnaire.ERPs <- cbind(rep_data4, questionnaire.ses1, questionnaire.ses2)
questionnaire.ERPs$group <- factor(questionnaire.ERPs$group)

# 2. Correlational measures
# 2.1. Combined measure of awareness
questionnaire.ERPs$aware.index <- questionnaire.ERPs$ses.dprime_1 * 
  questionnaire.ERPs$`4.4`
# 2.2 Compute RT means across sessions
questionnaire.ERPs$RT_1_2 <- rowMeans(questionnaire.ERPs[,19:20])
# 2.3. Compute differences
# session 1
questionnaire.ERPs$occ_diff_1 <- questionnaire.ERPs$occ.sqr_1 - 
  questionnaire.ERPs$occ.rand_1
questionnaire.ERPs$left_diff_1 <- questionnaire.ERPs$left.sqr_1 - 
  questionnaire.ERPs$left.rand_1
questionnaire.ERPs$right_diff_1 <- questionnaire.ERPs$right.sqr_1 - 
  questionnaire.ERPs$right.rand_1
# session 2
questionnaire.ERPs$occ_diff_2 <- questionnaire.ERPs$occ.sqr_2 - 
  questionnaire.ERPs$occ.rand_2
questionnaire.ERPs$left_diff_2 <- questionnaire.ERPs$left.sqr_2 - 
  questionnaire.ERPs$left.rand_2
questionnaire.ERPs$right_diff_2 <- questionnaire.ERPs$right.sqr_2 - 
  questionnaire.ERPs$right.rand_2

# 4. Plots
defaults <- par()
par(xpd = TRUE)
# Line plots with error bars - Confidence in square per group
aware.index.lines <- ggplot(rep_data2, aes(x = group, y = aware.index)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2)
  labs(title = "Occ average reference", x = "group", y = "Nd1 occ amplitude", 
       colour = "configuration")
  
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
  
# Histogram - questionnaire measures
hist(questionnaire.ERPs$`4.4`)
hist(questionnaire.ERPs$`5.4`)
boxplot(RT_1 ~ group, questionnaire.ERPs)
  
# Scatterplot - d' session 1
plot(questionnaire.ERPs$Subject, questionnaire.ERPs$ses.dprime_1, 
     col = questionnaire.ERPs$group, pch = 16, main = "d' session 1", 
     xlab = "Subjects", ylab  = "d'")
legend(2,1.1, legend = levels(questionnaire.ERPs$group), col = c("black", "red"),
       pch = 16)

# Scatterplot - aware index
plot(questionnaire.ERPs$Subject, questionnaire.ERPs$aware.index, 
     col = questionnaire.ERPs$group, pch = 16, main = "aware index")
legend(12, 15, legend = levels(questionnaire.ERPs$group), col = c("black", "red"),
       pch = 16)

# Heat maps/ - ordinal variables
cross.Freq <- count(questionnaire.ERPs, `5.4`, `4.4`)
kable(head(cross.Freq))
conf.x.freq <- ggplot(cross.Freq, aes(`4.4`,`5.4`)) +
  geom_tile(aes(fill = n), colour = "black") +
  scale_fill_gradient(low = "white", high = "steelblue")

plot(questionnaire.ERPs$`5.4`, questionnaire.ERPs$`4.4`)

# 3. Descriptives
summary(questionnaire.ERPs[,c(25:30)])
lapply(questionnaire.ERPs[,c(25:30)], sd)

by(questionnaire.ERPs[,25:30], questionnaire.ERPs$group, 
   stat.desc, basic = FALSE)

#--------------------------------Comparisons--------------------------------------
# 1. Compare d' across conditions
contrasts(rep_data_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long2$group) <- c(-1, 1) # setting contrasts for group
dprime_baseline <- lme(dprime ~ 1, random = ~1|Subject/configuration/session, 
                       data = rep_data_long2, method = "ML") #baseline
dprime_config <- update(dprime_baseline, .~. + configuration)
dprime_session <- update(dprime_config, .~. + session)
dprime_group <- update(dprime_session, .~. + group)
dprime_config_session <- update(dprime_group, .~. + configuration:session)
dprime_session_group <- update(dprime_config_session, .~. + session:group)
dprime_config_group <- update(dprime_session_group, .~. + configuration:group)
dprime_lme <- update(dprime_config_group, .~. + configuration:session:group)
anova(dprime_baseline, dprime_config, dprime_session, dprime_group, 
      dprime_config_session, dprime_session_group, dprime_config_group, 
      dprime_lme)

# 2. Compare RT across conditions
contrasts(rep_data_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long2$group) <- c(-1, 1) # setting contrasts for group
RT_baseline <- lme(RT ~ 1, random = ~1|Subject/configuration/session, 
                   data = rep_data_long2, method = "ML") #baseline
RT_config <- update(RT_baseline, .~. + configuration)
RT_session <- update(RT_config, .~. + session)
RT_group <- update(RT_session, .~. + group)
RT_config_session <- update(RT_group, .~. + configuration:session)
RT_session_group <- update(RT_config_session, .~. + session:group)
RT_config_group <- update(RT_session_group, .~. + configuration:group)
RT_lme <- update(RT_config_group, .~. + configuration:session:group)
anova(RT_baseline, RT_config, RT_session, RT_group, 
      RT_config_session, RT_session_group, RT_config_group, 
      RT_lme)

# 2. Compare threshold across conditions
contrasts(rep_data_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long2$group) <- c(-1, 1) # setting contrasts for group
threshold_baseline <- lme(threshold ~ 1, random = ~1|Subject/configuration/session, 
                   data = rep_data_long2, method = "ML") #baseline
threshold_config <- update(threshold_baseline, .~. + configuration)
threshold_session <- update(threshold_config, .~. + session)
threshold_group <- update(threshold_session, .~. + group)
threshold_config_session <- update(threshold_group, .~. + configuration:session)
threshold_session_group <- update(threshold_config_session, .~. + session:group)
threshold_config_group <- update(threshold_session_group, .~. + configuration:group)
threshold_lme <- update(threshold_config_group, .~. + configuration:session:group)
anova(threshold_baseline, threshold_config, threshold_session, threshold_group, 
      threshold_config_session, threshold_session_group, threshold_config_group, 
      threshold_lme)

# 2. Compare thresholds.1 across conditions
t.test(thresholds.1 ~ group.original, data = questionnaire.ERPs)
t.test(thresholds.2 ~ group.original, data = questionnaire.ERPs)

#------------------------------------Correlations----------------------------
# 1. Behavioral data correlations matrix
questionnaire.ratings1 <- questionnaire.ses1[,c(2,7,13)]
questionnaire.ratings2 <- questionnaire.ses2[,c(2,7,13)]
ggpairs(questionnaire.ratings1, mapping = aes(color=Recall))
# subsets
aware <- subset(questionnaire.ERPs, group.original == 'aware')
unaware <- subset(questionnaire.ERPs, group.original == 'unaware')
# Test correlations between measures
cor.test(questionnaire.ERPs$`5.4`, questionnaire.ERPs$`4.4`, method = "pearson")

# 2. Correlations: psychophysical x awareness measures
# 2.1. d' x Confidence ratings
cor.test(questionnaire.ERPs$ses.dprime_1, questionnaire.ERPs$`4.4`, 
         method = "pearson")
cor.test(questionnaire.ERPs$RT_1, questionnaire.ERPs$`4.4`, 
         method = "pearson")
# 2.2. d' x Frequency ratings
cor.test(questionnaire.ERPs$ses.dprime_1, questionnaire.ERPs$`5.4`, 
         method = "pearson")
cor.test(questionnaire.ERPs$RT_1, questionnaire.ERPs$`5.4`, 
         method = "pearson")
# 2.3. RT x confidence ratings by group
cor.test(aware$RT_1, aware$`4.4`)
cor.test(unaware$RT_1, unaware$`4.4`)
cor.test(aware$RT_2, aware$`4.4`)
cor.test(unaware$RT_2, unaware$`4.4`)

# 3. Correlations: ERPs x awareness measures
# 3.1. ERP Differences x confidence ratings
# 3.1.1. Both groups
# 3.1.1.1. Session 1
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ratings1$`4.4`, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ratings1$`4.4`, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ratings1$`4.4`, 
         method = "pearson")
# 3.1.1.2. session 2
cor.test(questionnaire.ERPs$occ_diff_2, questionnaire.ratings1$`4.4`, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_2, questionnaire.ratings1$`4.4`, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_2, questionnaire.ratings1$`4.4`, 
         method = "pearson")
# 3.1.2. By group
# 3.1.2.1. Session 1
cor.test(aware$`4.4`, aware$occ_diff_1)
cor.test(aware$`4.4`, aware$left_diff_1)
cor.test(aware$`4.4`, aware$right_diff_1)
cor.test(unaware$`4.4`, unaware$occ_diff_1)
cor.test(unaware$`4.4`, unaware$left_diff_1)
cor.test(unaware$`4.4`, unaware$right_diff_1)
# 3.1.2.1. Session 2
cor.test(aware$`4.4`, aware$occ_diff_2)
cor.test(aware$`4.4`, aware$left_diff_2)
cor.test(aware$`4.4`, aware$right_diff_2)
cor.test(unaware$`4.4`, unaware$occ_diff_2)
cor.test(unaware$`4.4`, unaware$left_diff_2)
cor.test(unaware$`4.4`, unaware$right_diff_2)

# 3.1. ERP Differences x frequency ratings
# 3.1.1. Both groups
# 3.1.1.1. Session 1
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ratings1$`5.4`, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ratings1$`5.4`, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ratings1$`5.4`, 
         method = "pearson")
# 3.1.1.2. session 2
cor.test(questionnaire.ERPs$occ_diff_2, questionnaire.ratings1$`5.4`, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_2, questionnaire.ratings1$`5.4`, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_2, questionnaire.ratings1$`5.4`, 
         method = "pearson")
# 3.2.2. By group
# 3.2.2.1. Session 1
cor.test(aware$`5.4`, aware$occ_diff_1)
cor.test(aware$`5.4`, aware$left_diff_1)
cor.test(aware$`5.4`, aware$right_diff_1)
cor.test(unaware$`5.4`, unaware$occ_diff_1)
cor.test(unaware$`5.4`, unaware$left_diff_1)
cor.test(unaware$`5.4`, unaware$right_diff_1)
# 3.2.2.1. Session 2
cor.test(aware$`5.4`, aware$occ_diff_2)
cor.test(aware$`5.4`, aware$left_diff_2)
cor.test(aware$`5.4`, aware$right_diff_2)
cor.test(unaware$`5.4`, unaware$occ_diff_2)
cor.test(unaware$`5.4`, unaware$left_diff_2)
cor.test(unaware$`5.4`, unaware$right_diff_2)

# 3.2. ERP Differences x combined awareness index
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ERPs$aware.index, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ERPs$aware.index, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ERPs$aware.index, 
         method = "pearson")

# 4. Correlations: ERP differences x behavioral measures
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

# 4.3. ERP Differences x % correct
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")
