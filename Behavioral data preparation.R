
library(reshape2)

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
#testAcc <- c(accuracies.sub1, accuracies.sub2, accuracies.sub3)

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
ses1.h.sqr <- lapply(as.list(behav_ses_1), extract.h.sqr)
ses2.h.sqr <- lapply(as.list(behav_ses_2), extract.h.sqr)
#random
ses1.h.rand <- lapply(as.list(behav_ses_1), extract.h.rand)
ses2.h.rand <- lapply(as.list(behav_ses_2), extract.h.rand)
# Both
ses1.h <- lapply(as.list(behav_ses_1), extract.h)
ses2.h <- lapply(as.list(behav_ses_2), extract.h)
ses3.h <- lapply(as.list(behav_ses_3), extract.h.ses3)

# 1.3. Extract false alarms and correct rejections for both configurations
#square
ses1.fa.sqr <- lapply(as.list(behav_ses_1), extract.fa.sqr)
ses2.fa.sqr <- lapply(as.list(behav_ses_2), extract.fa.sqr)
#random
ses1.fa.rand <- lapply(as.list(behav_ses_1), extract.fa.rand)
ses2.fa.rand <- lapply(as.list(behav_ses_2), extract.fa.rand)
# Both
ses1.fa <- lapply(as.list(behav_ses_1), extract.fa)
ses2.fa <- lapply(as.list(behav_ses_2), extract.fa)
ses3.fa <- lapply(as.list(behav_ses_3), extract.fa.ses3)

# 1.4. Compute proportions (hits, false alarms, correct rejections, misses, correct)
# 1.4.1. Square
# 1.4.1.1. Session 1
ses1.Ph.sqr <- sapply(ses1.h.sqr, mean)
ses1.Pfa.sqr <- sapply(ses1.fa.sqr, mean)
ses1.Pcr.sqr <- sapply(ses1.Pfa.sqr, function(x) 1-x)
ses1.Pm.sqr <- sapply(ses1.Ph.sqr, function(y) 1-y)
ses1.Pc.sqr <- mapply(function(a, b) (a + 9 * b)/10, ses1.Ph.sqr, ses1.Pcr.sqr)
# 1.4.1.1. Session 2
ses2.Ph.sqr <- sapply(ses2.h.sqr, mean)
ses2.Pfa.sqr <- sapply(ses2.fa.sqr, mean)
ses2.Pcr.sqr <- sapply(ses2.Pfa.sqr, function(x) 1-x)
ses2.Pm.sqr <- sapply(ses2.Ph.sqr, function(y) 1-y)
ses2.Pc.sqr <- mapply(function(a, b) (a + 9 * b)/10, ses2.Ph.sqr, ses2.Pcr.sqr)
# 1.4.2. Random
# 1.4.2.1. Session 1
ses1.Ph.rand <- sapply(ses1.h.rand, mean)
ses1.Pfa.rand <- sapply(ses1.fa.rand, mean)
ses1.Pcr.rand <- sapply(ses1.Pfa.rand, function(x) 1-x)
ses1.Pm.rand <- sapply(ses1.Ph.rand, function(y) 1-y)
ses1.Pc.rand <- mapply(function(a, b) (a + 9 * b)/10, ses1.Ph.rand, ses1.Pcr.rand)
# 1.4.2.1. Session 2
ses2.Ph.rand <- sapply(ses2.h.rand, mean)
ses2.Pfa.rand <- sapply(ses2.fa.rand, mean)
ses2.Pcr.rand <- sapply(ses2.Pfa.rand, function(x) 1-x)
ses2.Pm.rand <- sapply(ses2.Ph.rand, function(y) 1-y)
ses2.Pc.rand <- mapply(function(a, b) (a + 9 * b)/10, ses2.Ph.rand, ses2.Pcr.rand)
# 1.4.3. Both
# 1.4.3.1. Session 1
ses1.Ph <- sapply(ses1.h, mean)
ses1.Pfa <- sapply(ses1.fa, mean)
ses1.Pcr <- sapply(ses1.Pfa, function(x) 1-x)
ses1.Pm <- sapply(ses1.Ph, function(y) 1-y)
ses1.Pc <- mapply(function(a, b) (a + 9 * b)/10, ses1.Ph, ses1.Pcr)
# 1.4.3.2. Session 2
ses2.Ph <- sapply(ses2.h, mean)
ses2.Pfa <- sapply(ses2.fa, mean)
ses2.Pcr <- sapply(ses2.Pfa, function(x) 1-x)
ses2.Pm <- sapply(ses2.Ph, function(y) 1-y)
ses2.Pc <- mapply(function(a, b) (a + 9 * b)/10, ses2.Ph, ses2.Pcr)
# 1.4.3.3. Session 3
ses3.Ph <- sapply(ses3.h, mean)
ses3.Pfa <- sapply(ses3.fa, mean)
ses3.Pcr <- sapply(ses3.Pfa, function(x) 1-x)
ses3.Pm <- sapply(ses3.Ph, function(y) 1-y)
ses3.Pc <- mapply(function(a, b) (a + 9 * b)/10, ses3.Ph, ses3.Pcr)

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

# 2.3. Remove outliers (RTs above or below 3sds from the mean)
# 2.3.1. Function to remove outliers
remove.outliers <- function(vectorx) {
  vec.mean <- mean(vectorx)
  vec.sd <- sd(vectorx)
  for (i in 1:length(vectorx)) {
    if (vectorx[i] < vec.mean - 2*vec.sd  | vectorx[i] > 2*vec.sd + vec.mean) {
      vectorx[-i]
    }
  }
  return(vectorx)
}
# 2.3.2 Boxplots to detect outliers
testout<- remove.outliers(RT_1[[21]])
boxplot(RT_1[[21]], col = 3)

# 2.3.3. Apply function to RT lists
RT_1 <- lapply(RT_1, remove.outliers)
RT_2 <- lapply(RT_2, remove.outliers)
RT_3 <- lapply(RT_3, remove.outliers)

# 2.4. Compute mean RTs
# 2.4.1. Square
RT.mean.sqr.1 <- sapply(RT.sqr_1, mean)
RT.mean.sqr.2 <- sapply(RT.sqr_2, mean)
RT.mean.sqr.3 <- sapply(RT.sqr_3, mean)
# 2.4.2. Random
RT.mean.rand.1 <- sapply(RT.rand_1, mean)
RT.mean.rand.2 <- sapply(RT.rand_2, mean)
RT.mean.rand.3 <- sapply(RT.rand_3, mean)
# 2.4.3. Both
RT.mean.1 <- sapply(RT_1, mean)
RT.mean.2 <- sapply(RT_2, mean)
RT.mean.3 <- sapply(RT_3, mean)

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
rep_data4 <- cbind(rep_data3, RT.mean.sqr.1, RT.mean.sqr.2, RT.mean.sqr.3, 
                   RT.mean.rand.1, RT.mean.rand.2, RT.mean.rand.3, RT.mean.1, 
                   RT.mean.2, RT.mean.3)

# 4.3. Bind session threshold, proportion correct, intensities per trial and 
# accuracy values per trial to data frame
# 4.3.1. Thresholds
rep_data4 <- cbind(rep_data4, threshold.sqr_1, threshold.sqr_2, threshold.rand_1, 
                   threshold.rand_2, threshold.1, threshold.2)

# 4.3.2. Proportion correct, hits, misses, false alarms and correct rejections
rep_data4 <- cbind(ses1.Ph.sqr, ses2.Ph.sqr, ses1.Ph.rand, ses2.Ph.rand, ses1.Ph, 
                   ses2.Ph, ses3.Ph, ses1.Pfa.sqr, ses2.Pfa.sqr, ses1.Pfa.rand, 
                   ses2.Pfa.rand, ses1.Pfa, ses2.Pfa, ses3.Pfa, ses1.Pm.sqr, 
                   ses2.Pm.sqr, ses1.Pm.rand, ses2.Pm.rand, ses1.Pm, ses2.Pm,
                   ses3.Pm, ses1.Pcr.sqr, ses2.Pcr.sqr, ses1.Pcr.rand, ses2.Pcr.rand,
                   ses1.Pcr, ses2.Pcr, ses3.Pcr, ses1.Pc.sqr, ses2.Pc.sqr, 
                   ses1.Pc.rand, ses2.Pc.rand, ses1.Pc, ses2.Pc, ses3.Pc)

# 4.3.3. Intensities
# 4.3.3.1. Both configurations
rep_data4$intensities.1 <- intensities.1
rep_data4$intensities.2 <- intensities.2
# 4.3.3.2. Square
rep_data4$intensities.sqr.1 <- intensities.sqr.1
rep_data4$intensities.sqr.2 <- intensities.sqr.2
# 4.3.3.3. Random
rep_data4$intensities.rand.1 <- intensities.rand.1
rep_data4$intensities.rand.2 <- intensities.rand.2

# 4.3.4. Accuracies
# 4.3.4.1. Both configurations
rep_data4$accuracies.1 <- accuracies.1
rep_data4$accuracies.2 <- accuracies.2
# 4.3.4.2. Square
rep_data4$accuracies.sqr.1 <- accuracies.sqr.1
rep_data4$accuracies.sqr.2 <- accuracies.sqr.2
# 4.3.4.3. Random
rep_data4$accuracies.rand.1 <- accuracies.rand.1
rep_data4$accuracies.rand.2 <- accuracies.rand.2

# 4.4. Bind questionnaire data to experiment data
questionnaire.ERPs <- cbind(rep_data4, questionnaire.ses1, questionnaire.ses2)

# 4.5. Coerce groups as factors
questionnaire.ERPs$group <- factor(questionnaire.ERPs$group)
questionnaire.ERPs$group.original <- factor(questionnaire.ERPs$group.original)

# 4.6.Convert from wide to long
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