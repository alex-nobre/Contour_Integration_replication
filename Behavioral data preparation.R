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
colnames(questionnaire.ses1) <- c("Recall_1",	"Block_1", "conf.1_1",	
                                  "conf.2_1", "conf.3_1",	"conf.4_1",	
                                  "conf.5_1",	"conf.6_1",	"freq.1_1",	
                                  "freq.2_1",	"freq.3_1","freq.4_1",	
                                  "freq.5_1",	"freq.6_1")
colnames(questionnaire.ses2) <- c("Recall_2",	"Block_2", "conf.1_2",	
                                  "conf.2_2", "conf.3_2", "conf.4_2", 
                                  "conf.5_2",	"conf.6_2",	"freq.1_2",	
                                  "freq.2_2",	"freq.3_2",	"freq.4_2",	
                                  "freq.5_2",	"freq.6_2")

# 2. Get list of psychopy file names
behav_ses_1 <- list.files('./Data/Data_Psychopy', pattern = "Implicit segregation IB_.*\\_1")
behav_ses_2 <- list.files('./Data/Data_Psychopy', pattern = "Implicit segregation IB_.*\\_2")
behav_ses_3 <- list.files('./Data/Data_Psychopy', pattern = "Implicit segregation IB_.*\\_3")

#-------------------------Compute Psychophysical measures----------------------------
# 1. Compute d'
# 1.1. Functions to read files and accuracy and compute proportion of hits & false alarms 
# in both configs
# 1.1.1. Sessions 1 & 2 (task: dim disc detection)
# 1.1.1.1. Square and random configurations
# P(Hits) square
extract.h.sqr <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  target.trials.sqr <- subset(subj.data, target.presence == 1 & Configuration == 1)
  h.sqr <- as.vector(target.trials.sqr$correct)
}
# P(Hits) random
extract.h.rand <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  target.trials.rand <- subset(subj.data, target.presence == 1 & Configuration == 0)
  h.rand <- as.vector(target.trials.rand$correct)
}
# P(False alarms) square
extract.fa.sqr <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  notarget.trials.sqr <- subset(subj.data, target.presence == 0 & Configuration == 1)
  fa.sqr <- as.vector(notarget.trials.sqr$Resp)
}
# P(False alarms) random
extract.fa.rand <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  notarget.trials.rand <- subset(subj.data, target.presence == 0 & Configuration == 0)
  fa.rand <- as.vector(notarget.trials.rand$Resp)
}

# 1.1.1.2. Both
# P(Hits)
extract.h <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  target.trials <- subset(subj.data, target.presence == 1)
  h <- as.vector(target.trials$correct)
}

# P(False alarms)
extract.fa <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  notarget.trials <- subset(subj.data, target.presence == 0)
  fa <- as.vector(notarget.trials$Resp)
}

# 1.1.2. Session 3 (task: diamond detection)
# P(Hits)
extract.h.ses3 <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  diamond.trials <- subset(subj.data, Configuration == 2)
  h <- as.vector(diamond.trials$correct)
}
# P(False alarms)
extract.fa.ses3 <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  nodiamond.trials <- subset(subj.data, Configuration == 0 | Configuration == 1)
  fa <- as.vector(nodiamond.trials$Resp)
}

# 1.2. Extract hits and misses for each configuration
#square
hit.sqr_1 <- lapply(as.list(behav_ses_1), extract.h.sqr)
hit.sqr_2 <- lapply(as.list(behav_ses_2), extract.h.sqr)
#random
hit.rand_1 <- lapply(as.list(behav_ses_1), extract.h.rand)
hit.rand_2 <- lapply(as.list(behav_ses_2), extract.h.rand)
# Both
hit_1 <- lapply(as.list(behav_ses_1), extract.h)
hit_2 <- lapply(as.list(behav_ses_2), extract.h)
hit_3 <- lapply(as.list(behav_ses_3), extract.h.ses3)

# 1.3. Extract false alarms and correct rejections for both configurations
#square
fa.sqr_1 <- lapply(as.list(behav_ses_1), extract.fa.sqr)
fa.sqr_2 <- lapply(as.list(behav_ses_2), extract.fa.sqr)
#random
fa.rand_1 <- lapply(as.list(behav_ses_1), extract.fa.rand)
fa.rand_2 <- lapply(as.list(behav_ses_2), extract.fa.rand)
# Both
fa_1 <- lapply(as.list(behav_ses_1), extract.fa)
fa_2 <- lapply(as.list(behav_ses_2), extract.fa)
fa_3 <- lapply(as.list(behav_ses_3), extract.fa.ses3)

# 1.4. Compute proportions (hits, false alarms, correct rejections, misses, correct)
# 1.4.1. Square
# 1.4.1.1. Session 1
Ph.sqr_1 <- sapply(hit.sqr_1, mean)
Pfa.sqr_1 <- sapply(fa.sqr_1, mean)
Pcr.sqr_1 <- sapply(Pfa.sqr_1, function(x) 1-x)
Pm.sqr_1 <- sapply(Ph.sqr_1, function(y) 1-y)
Pc.sqr_1 <- mapply(function(a, b) (a + 9 * b)/10, Ph.sqr_1, Pcr.sqr_1)
# 1.4.1.1. Session 2
Ph.sqr_2 <- sapply(hit.sqr_2, mean)
Pfa.sqr_2 <- sapply(fa.sqr_2, mean)
Pcr.sqr_2 <- sapply(Pfa.sqr_2, function(x) 1-x)
Pm.sqr_2 <- sapply(Ph.sqr_2, function(y) 1-y)
Pc.sqr_2 <- mapply(function(a, b) (a + 9 * b)/10, Ph.sqr_2, Pcr.sqr_2)
# 1.4.2. Random
# 1.4.2.1. Session 1
Ph.rand_1 <- sapply(hit.rand_1, mean)
Pfa.rand_1 <- sapply(fa.rand_1, mean)
Pcr.rand_1 <- sapply(Pfa.rand_1, function(x) 1-x)
Pm.rand_1 <- sapply(Ph.rand_1, function(y) 1-y)
Pc.rand_1 <- mapply(function(a, b) (a + 9 * b)/10, Ph.rand_1, Pcr.rand_1)
# 1.4.2.1. Session 2
Ph.rand_2 <- sapply(hit.rand_2, mean)
Pfa.rand_2 <- sapply(fa.rand_2, mean)
Pcr.rand_2 <- sapply(Pfa.rand_2, function(x) 1-x)
Pm.rand_2 <- sapply(Ph.rand_2, function(y) 1-y)
Pc.rand_2 <- mapply(function(a, b) (a + 9 * b)/10, Ph.rand_2, Pcr.rand_2)
# 1.4.3. Both
# 1.4.3.1. Session 1
Ph_1 <- sapply(hit_1, mean)
Pfa_1 <- sapply(fa_1, mean)
Pcr_1 <- sapply(Pfa_1, function(x) 1-x)
Pm_1 <- sapply(Ph_1, function(y) 1-y)
Pc_1 <- mapply(function(a, b) (a + 9 * b)/10, Ph_1, Pcr_1)
# 1.4.3.2. Session 2
Ph_2 <- sapply(hit_2, mean)
Pfa_2 <- sapply(fa_2, mean)
Pcr_2 <- sapply(Pfa_2, function(x) 1-x)
Pm_2 <- sapply(Ph_2, function(y) 1-y)
Pc_2 <- mapply(function(a, b) (a + 9 * b)/10, Ph_2, Pcr_2)
# 1.4.3.3. Session 3
Ph_3 <- sapply(hit_3, mean)
Pfa_3 <- sapply(fa_3, mean)
Pcr_3 <- sapply(Pfa_3, function(x) 1-x)
Pm_3 <- sapply(Ph_3, function(y) 1-y)
Pc_3 <- mapply(function(a, b) (a + 9 * b)/10, Ph_3, Pcr_3)

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
dprime.sqr_1 <- mapply(compute.dprime, Ph.sqr_1, Pfa.sqr_1)
dprime.sqr_2 <- mapply(compute.dprime, Ph.sqr_2, Pfa.sqr_2)
#random
dprime.rand_1 <- mapply(compute.dprime, Ph.rand_1, Pfa.rand_1)
dprime.rand_2 <- mapply(compute.dprime, Ph.rand_2, Pfa.rand_2)
# Both
dprime_1 <- mapply(compute.dprime, Ph_1, Pfa_1)
dprime_2 <- mapply(compute.dprime, Ph_2, Pfa_2)
dprime_3 <- mapply(compute.dprime, Ph_3, Pfa_3)
dprime_3[28] <- 3.090232

# 2. Retrieve RT
# 2.1. Functions for retrieving RTs
#square
square.RT <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  square.trials <- subset(subj.data, Configuration == 1 & Resp == 1 & 
                            correct == 1)
  RT.sqr <- as.vector(square.trials$RT)
}
#random
random.RT <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  random.trials <- subset(subj.data, Configuration == 0 & Resp == 1 & 
                            correct == 1)
  RT.rand <- as.vector(random.trials$RT)
}
#both
retrieve.RT <- function(behav.file) {
  subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
                          header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  response.trials <- subset(subj.data, Resp == 1 & 
                              correct == 1)
  RT <- as.vector(response.trials$RT)
}

# 2.2. Compute RTs for each session
#square
RT.sqr_1 <- lapply(as.list(behav_ses_1), square.RT)
RT.sqr_2 <- lapply(as.list(behav_ses_2), square.RT)
RT.sqr_3 <- lapply(as.list(behav_ses_3), square.RT)
#random
RT.rand_1 <- lapply(as.list(behav_ses_1), random.RT)
RT.rand_2 <- lapply(as.list(behav_ses_2), random.RT)
RT.rand_3 <- lapply(as.list(behav_ses_3), random.RT)
#both
RT_1 <- lapply(as.list(behav_ses_1), retrieve.RT)
RT_2 <- lapply(as.list(behav_ses_2), retrieve.RT)
RT_3 <- lapply(as.list(behav_ses_3), retrieve.RT)

# # 2.3. Remove outliers (RTs above or below 3sds from the mean)
# # 2.3.1. Function to remove outliers
# remove.outliers <- function(vectorx) {
#   vec.mean <- mean(vectorx)
#   vec.sd <- sd(vectorx)
#   for (i in 1:length(vectorx)) {
#     if (vectorx[i] < vec.mean - 2*vec.sd  | vectorx[i] > 2*vec.sd + vec.mean) {
#       vectorx[-i]
#     }
#   }
#   return(vectorx)
# }
# # 2.3.2 Boxplots to detect outliers
# testout<- remove.outliers(RT_1[[21]])
# boxplot(RT_1[[21]], col = 3)
# 
# # 2.3.3. Apply function to RT lists
# RT_1 <- lapply(RT_1, remove.outliers)
# RT_2 <- lapply(RT_2, remove.outliers)
# RT_3 <- lapply(RT_3, remove.outliers)

# 2.4. Compute mean RTs
# 2.4.1. Square
RT.mean.sqr_1 <- sapply(RT.sqr_1, mean)
RT.mean.sqr_2 <- sapply(RT.sqr_2, mean)
RT.mean.sqr_3 <- sapply(RT.sqr_3, mean)
# 2.4.2. Random
RT.mean.rand_1 <- sapply(RT.rand_1, mean)
RT.mean.rand_2 <- sapply(RT.rand_2, mean)
RT.mean.rand_3 <- sapply(RT.rand_3, mean)
# 2.4.3. Both
RT.mean_1 <- sapply(RT_1, mean)
RT.mean_2 <- sapply(RT_2, mean)
RT.mean_3 <- sapply(RT_3, mean)

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

# # 3.2. Functions to extract intensity values by block
# # 3.2.1. Function to extract intensities of a block
# block.intensities <- function(x) {
#   return(as.vector(x$Decrement))
# }
# 
# # 3.2.2. Function to extract intensities of whole session, separated per block
# session.intensities <- function(behav.file) {
#   subj.data <- read.table(paste('./Data/Data_Psychopy/', behav.file, sep = ""), 
#                           header = TRUE, sep = "\t", skip = 12, row.names = NULL)
#   colnames(subj.data)[5] <- "target.presence"
#   target.trials <- subset(subj.data, target.presence == 1)
#   block0 <- subset(target.trials, Block == 0)
#   block1 <- subset(target.trials, Block == 1)
#   block2 <- subset(target.trials, Block == 2)
#   block3 <- subset(target.trials, Block == 3)
#   block4 <- subset(target.trials, Block == 4)
#   block5 <- subset(target.trials, Block == 5)
#   block6 <- subset(target.trials, Block == 6)
#   block7 <- subset(target.trials, Block == 7)
#   block8 <- subset(target.trials, Block == 8)
#   block9 <- subset(target.trials, Block == 9)
#   list.blocks <- list(block0, block1, block2, block3, block4, block5, block6,
#                       block7, block8, block9)
#   # Call function defined above
#   decrements <- lapply(list.blocks, block.intensities)
#   intensities <- mapply('-', 1, decrements)
# }
# 
# # 3.2.3. Extract block intensities for all subjects
# blockintensities.1 <- lapply(behav_ses_1, session.intensities)
# blockintensities.2 <- lapply(behav_ses_2, session.intensities)

# 3.3. Extract intensities for each subject by session and configuration
# 3.3.1. Square
intensities.sqr_1 <- lapply(behav_ses_1, extract.intensities.sqr)
intensities.sqr_2 <- lapply(behav_ses_2, extract.intensities.sqr)
intensities.sqr_3 <- lapply(behav_ses_3, extract.intensities.sqr)
# 3.3.1. Random
intensities.rand_1 <- lapply(behav_ses_1, extract.intensities.rand)
intensities.rand_2 <- lapply(behav_ses_2, extract.intensities.rand)
intensities.rand_3 <- lapply(behav_ses_3, extract.intensities.rand)
# 3.3.1. Both configurations
intensities_1 <- lapply(behav_ses_1, extract.intensities)
intensities_2 <- lapply(behav_ses_2, extract.intensities)
intensities_3 <- lapply(behav_ses_3, extract.intensities)

# 3.4. Extract  final thresholds for each session
# 3.4.1. Square
threshold.sqr_1 <- sapply(intensities.sqr_1, function(x) { return( x[length(x)] ) })
threshold.sqr_2 <- sapply(intensities.sqr_2, function(x) { return( x[length(x)] ) })
threshold.sqr_3 <- sapply(intensities.sqr_3, function(x) { return( x[length(x)] ) })
# 3.4.1. Random
threshold.rand_1 <- sapply(intensities.rand_1, function(x) { return( x[length(x)] ) })
threshold.rand_2 <- sapply(intensities.rand_2, function(x) { return( x[length(x)] ) })
threshold.rand_3 <- sapply(intensities.rand_3, function(x) { return( x[length(x)] ) })
# 3.4.1. Both configurations
threshold_1 <- sapply(intensities_1, function(x) { return( x[length(x)] ) })
threshold_2 <- sapply(intensities_2, function(x) { return( x[length(x)] ) })
threshold_3 <- sapply(intensities_3, function(x) { return( x[length(x)] ) })
