# Data import packages
library(xlsx)
# Data manipulation packages
library(outliers)
library(dplyr)
library(tidyr)
library(reshape)
library(reshape2)
# Plotting packages
library(lattice)
library(ggplot2)
library(gridExtra)
library(GGally)
# Analysis packages
library(nlme)
library(gmodels)
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

# Import file names in working directory
sqr1_fnames <- list.files('./Data/Data_BVA', pattern = "1_All ROIs Sqr")
sqr2_fnames <- list.files('./Data/Data_BVA', pattern = "2_All ROIs Sqr")
rand1_fnames <- list.files('./Data/Data_BVA', pattern = "1_All ROIs Rand")
rand2_fnames <- list.files('./Data/Data_BVA', pattern = "2_All ROIs Rand")
# Concatenate all lists in a single list
list_fnames <- c(sqr1_fnames, sqr2_fnames, rand1_fnames, rand2_fnames)

#------------------------Individual windows extraction by file------------------------
# 1. Function to extract ROIs means
rois.means <- function(eeg.file) {
  eeg.data <- read.delim(paste('./Data/Data_BVA/', eeg.file, sep = ""), sep = " ", 
                          dec = ",", header = TRUE)
  C1 <- eeg.data[40:50,1]
  P1 <- eeg.data[51:61,2]
  N1 <- eeg.data[62:75,3]
  occ_window_1 <- eeg.data[80:90,4]
  occ_window_2 <- eeg.data[100:110,4]
  left_window_1 <- eeg.data[80:90,5]
  left_window_2 <- eeg.data[100:110,5]
  right_window_1 <- eeg.data[80:90,6]
  right_window_2 <- eeg.data[100:110,6]
  right_left_window_1 <- eeg.data[80:90,7]
  right_left_window_2 <- eeg.data[100:110,7]
  N2 <- eeg.data[95:115,8]
  LP <- eeg.data[130:160,9]
  mean_C1 <- mean(C1)
  mean_P1 <- mean(P1)
  mean_N1 <- mean(N1)
  mean_occ_1 <- mean(occ_window_1)
  mean_occ_2 <- mean(occ_window_2)
  mean_left_1 <- mean(left_window_1)
  mean_left_2 <- mean(left_window_2)
  mean_right_1 <- mean(right_window_1)
  mean_right_2 <- mean(right_window_2)
  mean_right_left_1 <- mean(right_left_window_1)
  mean_right_left_2 <- mean(right_left_window_2)
  mean_N2 <- mean(N2)
  mean_LP <- mean(LP)
  means_egg <- c(mean_C1, mean_P1, mean_N1, mean_occ_1, mean_occ_2, mean_left_1, 
                  mean_left_2, mean_right_1, mean_right_2, mean_right_left_1, 
                  mean_right_left_2, mean_N2, mean_LP)
}


# Compute means for each file
sqr1_means <- lapply(sqr1_fnames, rois.means)
sqr2_means <- lapply(sqr2_fnames, rois.means)
rand1_means <- lapply(rand1_fnames, rois.means)
rand2_means <- lapply(rand2_fnames, rois.means)


# Coerce lists to data frames
sqr1_dat <- data.frame(matrix(unlist(sqr1_means), nrow = length(sqr1_means), byrow = T))
sqr2_dat <- data.frame(matrix(unlist(sqr2_means), nrow = length(sqr2_means), byrow = T))
rand1_dat <- data.frame(matrix(unlist(rand1_means), nrow = length(rand1_means), byrow = T))
rand2_dat <- data.frame(matrix(unlist(rand2_means), nrow = length(rand2_means), byrow = T))

# Rename columms
colnames(sqr1_dat) <- c("C1.sqr_1", "P1.sqr_1", "N1.sqr_1", "occ.sqr.nd1_1",
                        "occ.sqr.nd2_1", "left.sqr.nd1_1", "left.sqr.nd2_1",
                        "right.sqr.nd1_1", "right.sqr.nd2_1", "RL.sqr.nd1_1",
                        "RL.sqr.nd2_1", "N2.sqr_1", "LP.sqr_1")
colnames(sqr2_dat) <- c("C1.sqr_2", "P1.sqr_2", "N1.sqr_2", "occ.sqr.nd1_2",
                        "occ.sqr.nd2_2", "left.sqr.nd1_2", "left.sqr.nd2_2",
                        "right.sqr.nd1_2", "right.sqr.nd2_2", "RL.sqr.nd1_2",
                        "RL.sqr.nd2_2", "N2.sqr_2", "LP.sqr_2")
colnames(rand1_dat) <- c("C1.rand_1", "P1.rand_1", "N1.rand_1", "occ.rand.nd1_1",
                         "occ.rand.nd2_1", "left.rand.nd1_1", "left.rand.nd2_1",
                         "right.rand.nd1_1", "right.rand.nd2_1", "RL.rand.nd1_1",
                         "RL.rand.nd2_1", "N2.rand_1", "LP.rand_1")
colnames(rand2_dat) <- c("C1.rand_2", "P1.rand_2", "N1.rand_2", "occ.rand.nd1_2",
                         "occ.rand.nd2_2", "left.rand.nd1_2", "left.rand.nd2_2",
                         "right.rand.nd1_2", "right.rand.nd2_2", "RL.rand.nd1_2",
                         "RL.rand.nd2_2", "N2.rand_2", "LP.rand_2")

# Merge data frames
rep_data <- cbind(sqr1_dat, sqr2_dat, rand1_dat, rand2_dat)

# Create group vectors
group <- c("unaware", "aware", "unaware", "unaware", "unaware",
                    "aware", "unaware", "aware", "aware", "aware", "aware",
                    "unaware", "aware", "unaware", "unaware", "aware", "aware",
                    "aware", "aware", "unaware", "aware", "unaware", "unaware",
                    "unaware", "unaware", "unaware", "unaware",
                    "unaware", "aware", "aware", "unaware", "unaware")
group.original <- c("unaware", "aware", "unaware", "unaware", "aware",
                             "aware", "unaware", "aware", "aware", "aware", "aware",
                             "unaware", "aware", "aware", "unaware", "aware", "aware",
                             "aware", "aware", "unaware", "aware", "unaware", "unaware",
                             "unaware", "unaware", "unaware",
                             "unaware", "aware", "aware", "aware", "aware",
                             "unaware")

# Add subject number
Subject <- c(1,2,3,4,6:11, 13:26, 29:30, 32:37)
rep_data <- cbind(Subject, group, group.original, rep_data)

# Remove subjects with too many artifacts
row.names(rep_data) <- 1:32

# #---------------------------------With subject 5------------------------------------
# # Create group vectors
# group <- c("unaware", "aware", "unaware", "unaware", "unaware", "unaware",
#            "aware", "unaware", "aware", "aware", "aware", "aware",
#            "unaware", "aware", "unaware", "unaware", "aware", "aware",
#            "aware", "aware", "unaware", "aware", "unaware", "unaware",
#            "unaware", "unaware", "unaware", "unaware",
#            "unaware", "aware", "aware", "unaware", "unaware")
# group.original <- c("unaware", "aware", "unaware", "unaware", "unaware", "aware",
#                     "aware", "unaware", "aware", "aware", "aware", "aware",
#                     "unaware", "aware", "aware", "unaware", "aware", "aware",
#                     "aware", "aware", "unaware", "aware", "unaware", "unaware",
#                     "unaware", "unaware", "unaware",
#                     "unaware", "aware", "aware", "aware", "aware",
#                     "unaware")
# 
# # Add subject number
# Subject <- c(1:11, 13:26, 29:30, 32:37)
# rep_data <- cbind(Subject, group, group.original, rep_data)
# 
# # Remove subjects with too many artifacts
# row.names(rep_data) <- 1:33
# #--------------------------------Include location as factor--------------------------------
#
# # Rename columns to separate electrode location number using sep = "_"
# names(rep_data_long2)[names(rep_data_long2) == "left.nd2"] <- "nd2_1"
# names(rep_data_long2)[names(rep_data_long2) == "right.nd2"] <- "nd2_2"
# # Extracts configuration number to column
# rep_data_long2 <- reshape(rep_data_long2, varying = c("nd2_1", "nd2_2"),
#                           direction = "long", idvar = " Subject", sep = "_")
# # Rename configuration column name
# names(rep_data_long2)[names(rep_data_long2) == "time"] <- "location"
#
# # Rename configuration levels
# rep_data_long2$location[rep_data_long2$location == 1] <- "left"
# rep_data_long2$location[rep_data_long2$location == 2] <- "right"
#
# #Remove redundant subject column
# rep_data_long2[,ncol(rep_data_long2)]<- NULL
#
# # # Convert configuration, group and session to factors
# rep_data_long2$group <- factor(rep_data_long2$group)
# rep_data_long2$group.original <- factor(rep_data_long2$group.original)
# rep_data_long2$session <- factor(rep_data_long2$session)
# rep_data_long2$configuration <- factor(rep_data_long2$configuration)
# rep_data_long2$location <- factor(rep_data_long2$location)
# rep_data_long2$alpha.group <- factor(rep_data_long2$alpha.group)