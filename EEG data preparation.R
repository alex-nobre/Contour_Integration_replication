library(reshape)

# Import file names in working directory
sqr1_fnames <- list.files('./Data/Data_BVA', pattern = "1_All ROIs Sqr")
sqr2_fnames <- list.files('./Data/Data_BVA', pattern = "2_All ROIs Sqr")
rand1_fnames <- list.files('./Data/Data_BVA', pattern = "1_All ROIs Rand")
rand2_fnames <- list.files('./Data/Data_BVA', pattern = "2_All ROIs Rand")
# Concatenate all lists in a single list
list_fnames <- c(sqr1_fnames, sqr2_fnames, rand1_fnames, rand2_fnames)

#-------------Individual windows extraction by file----------------
# Sqr, session 1
sqr_ses1_means <- function(sqr1_file) {
  sqr1_data <- read.delim(paste('./Data/Data_BVA/', sqr1_file, sep = ""), sep = " ", 
                          dec = ",", header = TRUE)
  # # Remove last two columns
  # sqr1_data$Right <- NULL
  # sqr1_data$Occ.par.1 <- NULL
  # # Rename last column
  # names(sqr1_data)[names(sqr1_data) == "Occ.par"] <- "Right"
  # Compute means for each region and append to main data frame
  C1 <- sqr1_data[40:50,1]
  P1 <- sqr1_data[51:61,2]
  N1 <- sqr1_data[62:75,3]
  occ_window_1 <- sqr1_data[80:90,4]
  occ_window_2 <- sqr1_data[100:110,4]
  left_window_1 <- sqr1_data[80:90,5]
  left_window_2 <- sqr1_data[100:110,5]
  right_window_1 <- sqr1_data[80:90,6]
  right_window_2 <- sqr1_data[100:110,6]
  right_left_window_1 <- sqr1_data[80:90,7]
  right_left_window_2 <- sqr1_data[100:110,7]
  N2 <- sqr1_data[95:115,8]
  LP <- sqr1_data[130:160,9]
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
  means_sqr1 <- c(mean_C1, mean_P1, mean_N1, mean_occ_1, mean_occ_2, mean_left_1, 
                  mean_left_2, mean_right_1, mean_right_2, mean_right_left_1, 
                  mean_right_left_2, mean_N2, mean_LP)
}

# Sqr, session 2
sqr_ses2_means <- function(sqr2_file) {
  sqr2_data <- read.delim(paste('./Data/Data_BVA/', sqr2_file, sep = ""), sep = " ", 
                          dec = ",", header = TRUE)
  # # Remove last two columns
  # sqr2_data$Right <- NULL
  # sqr2_data$Occ.par.1 <- NULL
  # # Rename last column
  # names(sqr2_data)[names(sqr2_data) == "Occ.par"] <- "Right"
  # Compute means for each region and append to main data frame
  C1 <- sqr2_data[40:50,1]
  P1 <- sqr2_data[51:61,2]
  N1 <- sqr2_data[62:75,3]
  occ_window_1 <- sqr2_data[80:90,4]
  occ_window_2 <- sqr2_data[100:110,4]
  left_window_1 <- sqr2_data[80:90,5]
  left_window_2 <- sqr2_data[100:110,5]
  right_window_1 <- sqr2_data[80:90,6]
  right_window_2 <- sqr2_data[100:110,6]
  right_left_window_1 <- sqr2_data[80:90,7]
  right_left_window_2 <- sqr2_data[100:110,7]
  N2 <- sqr2_data[95:115,8]
  LP <- sqr2_data[130:160,9]
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
  means_sqr2 <- c(mean_C1, mean_P1, mean_N1, mean_occ_1, mean_occ_2, mean_left_1, 
                  mean_left_2, mean_right_1, mean_right_2, mean_right_left_1, 
                  mean_right_left_2, mean_N2, mean_LP)
}

# Random, session 1
rand_ses1_means <- function(rand1_file) {
  rand1_data <- read.delim(paste('./Data/Data_BVA/', rand1_file, sep = ""), sep = " ", 
                           dec = ",", header = TRUE)
  # # Remove last two columns
  # rand1_data$Right <- NULL
  # rand1_data$Occ.par.1 <- NULL
  # # Rename last column
  # names(rand1_data)[names(rand1_data) == "Occ.par"] <- "Right"
  # Compute means for each region and append to main data frame
  C1 <- rand1_data[40:50,1]
  P1 <- rand1_data[51:61,2]
  N1 <- rand1_data[62:75,3]
  occ_window_1 <- rand1_data[80:90,4]
  occ_window_2 <- rand1_data[100:110,4]
  left_window_1 <- rand1_data[80:90,5]
  left_window_2 <- rand1_data[100:110,5]
  right_window_1 <- rand1_data[80:90,6]
  right_window_2 <- rand1_data[100:110,6]
  right_left_window_1 <- rand1_data[80:90,7]
  right_left_window_2 <- rand1_data[100:110,7]
  N2 <- rand1_data[95:115,8]
  LP <- rand1_data[130:160,9]
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
  means_rand1 <- c(mean_C1, mean_P1, mean_N1, mean_occ_1, mean_occ_2, mean_left_1, 
                  mean_left_2, mean_right_1, mean_right_2, mean_right_left_1, 
                  mean_right_left_2, mean_N2, mean_LP)
}

# Random, session 2
rand_ses2_means <- function(rand2_file) {
  rand2_data <- read.delim(paste('./Data/Data_BVA/', rand2_file, sep = ""), sep = " ", 
                           dec = ",", header = TRUE)
  # # Remove last two columns
  # rand2_data$Right <- NULL
  # rand2_data$Occ.par.1 <- NULL
  # # Rename last column
  # names(rand2_data)[names(rand2_data) == "Occ.par"] <- "Right"
  # Compute means for each region and append to main data frame
  C1 <- rand2_data[40:50,1]
  P1 <- rand2_data[51:61,2]
  N1 <- rand2_data[62:75,3]
  occ_window_1 <- rand2_data[80:90,4]
  occ_window_2 <- rand2_data[100:110,4]
  left_window_1 <- rand2_data[80:90,5]
  left_window_2 <- rand2_data[100:110,5]
  right_window_1 <- rand2_data[80:90,6]
  right_window_2 <- rand2_data[100:110,6]
  right_left_window_1 <- rand2_data[80:90,7]
  right_left_window_2 <- rand2_data[100:110,7]
  N2 <- rand2_data[95:115,8]
  LP <- rand2_data[130:160,9]
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
  means_rand2 <- c(mean_C1, mean_P1, mean_N1, mean_occ_1, mean_occ_2, mean_left_1, 
                  mean_left_2, mean_right_1, mean_right_2, mean_right_left_1, 
                  mean_right_left_2, mean_N2, mean_LP)
}

# Compute means for each file
sqr1_means <- lapply(sqr1_fnames, sqr_ses1_means)
sqr2_means <- lapply(sqr2_fnames, sqr_ses2_means)
rand1_means <- lapply(rand1_fnames, rand_ses1_means)
rand2_means <- lapply(rand2_fnames, rand_ses2_means)

# Coerce lists to data frames
sqr1_dat <- data.frame(matrix(unlist(sqr1_means), nrow = 34, byrow = T))
sqr2_dat <- data.frame(matrix(unlist(sqr2_means), nrow = 34, byrow = T))
rand1_dat <- data.frame(matrix(unlist(rand1_means), nrow = 34, byrow = T))
rand2_dat <- data.frame(matrix(unlist(rand2_means), nrow = 34, byrow = T))

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

# Add group variable
rep_data$group <- c("unaware", "aware", "unaware", "unaware", "unaware",
                    "aware", "unaware", "aware", "aware", "aware", "aware",
                    "unaware", "aware", "unaware", "unaware", "aware", "aware",
                    "aware", "aware", "unaware", "aware", "unaware", "unaware",
                    "unaware", "aware", "unaware", "unaware", "unaware", "unaware",
                    "unaware", "aware", "aware", "unaware", "unaware")
rep_data$group.original <- c("unaware", "aware", "unaware", "unaware", "aware", 
                             "aware", "unaware", "aware", "aware", "aware", "aware", 
                             "unaware", "aware", "aware", "unaware", "aware", "aware", 
                             "aware", "aware", "unaware", "aware", "unaware", "unaware", 
                             "unaware", "aware", "unaware", "unaware", "unaware", 
                             "unaware", "aware", "aware", "aware", "aware", 
                             "unaware")

# Add subject number
Subject <- c(1,2,3,4,6:11, 13:30, 32:37)
rep_data <- cbind(Subject, rep_data)

# Remove subjects with too many artifacts
rep_data2 <- rep_data[-c(25,26),]
row.names(rep_data2) <- 1:32

# Convert from wide to long
# Extracts session number to column
rep_data_long <- reshape(rep_data2, varying = c("C1.sqr_1", "C1.sqr_2", "C1.rand_1",
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
                                                "LP.rand_1", "LP.rand_2"),  
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
                                                     "LP_1", "LP_2"),  
                          direction = "long", idvar = " Subject", sep = "_")
# Rename configuration column name
names(rep_data_long2)[names(rep_data_long2) == "time"] <- "configuration"
# Rename configuration levels
rep_data_long2$configuration[rep_data_long2$configuration == 1] <- "sqr"
rep_data_long2$configuration[rep_data_long2$configuration == 2] <- "rand"

# Rename columns to separate electrode location number using sep = "_"
names(rep_data_long)[names(rep_data_long) == "occ.nd1"] <- "nd1_1"
names(rep_data_long)[names(rep_data_long) == "occ.nd2"] <- "nd2_1"
names(rep_data_long)[names(rep_data_long) == "left.nd1"] <- "nd1_2"
names(rep_data_long)[names(rep_data_long) == "left.nd2"] <- "nd2_2"
names(rep_data_long)[names(rep_data_long) == "right.nd1"] <- "nd1_3"
names(rep_data_long)[names(rep_data_long) == "right.nd2"] <- "nd2_3"
names(rep_data_long)[names(rep_data_long) == "RL.nd1"] <- "nd1_4"
names(rep_data_long)[names(rep_data_long) == "RL.nd2"] <- "nd2_4"
# Extracts configuration number to column
rep_data_long2 <- reshape(rep_data_long, varying = c("nd1_1", "nd2_1",
                                                     "nd1_2", "nd2_2",
                                                     "nd1_3", "nd2_3",
                                                     "nd1_4", "nd2_4"),  
                          direction = "long", idvar = " Subject", sep = "_")
# Rename configuration column name
names(rep_data_long2)[names(rep_data_long2) == "time"] <- "location"
# Rename configuration levels
rep_data_long2$configuration[rep_data_long2$location == 1] <- "occ"
rep_data_long2$configuration[rep_data_long2$location == 2] <- "left"
rep_data_long2$configuration[rep_data_long2$location == 3] <- "right"
rep_data_long2$configuration[rep_data_long2$location == 4] <- "RL"
# Remove redundant subject column
rep_data_long2[,ncol(rep_data_long2)]<- NULL

# Convert configuration, group and session to factors
rep_data_long2$group <- factor(rep_data_long2$group)
rep_data_long2$group.original <- factor(rep_data_long2$group.original)
rep_data_long2$session <- factor(rep_data_long2$session)
rep_data_long2$configuration <- factor(rep_data_long2$configuration)
rep_data_long2$location <- factor(rep_data_long2$location)