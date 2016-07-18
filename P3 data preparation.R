library(reshape)

# Import file names in working directory
sqr3_fnames_P3 <- list.files('./Data/Data_BVA', pattern = "3_All ROIs and P3 Sqr")
rand3_fnames_P3 <- list.files('./Data/Data_BVA', pattern = "3_All ROIs and P3 Rand")
# Concatenate all lists in a single list
list_fnames_P3 <- c(sqr3_fnames_P3, rand3_fnames_P3)

#-------------Individual windows extraction by file----------------
# Sqr, session 3
sqr_ses3_means_P3 <- function(sqr3_file) {
  sqr3_data <- read.delim(paste('./Data/Data_BVA/', sqr3_file, sep = ""), sep = " ", 
                          dec = ",", header = TRUE)
  C1 <- sqr3_data[40:50,1]
  P1 <- sqr3_data[51:61,2]
  N1 <- sqr3_data[62:75,3]
  occ_window_1 <- sqr3_data[80:90,4]
  occ_window_2 <- sqr3_data[100:110,4]
  left_window_1 <- sqr3_data[80:90,5]
  left_window_2 <- sqr3_data[100:110,5]
  right_window_1 <- sqr3_data[80:90,6]
  right_window_2 <- sqr3_data[100:110,6]
  right_left_window_1 <- sqr3_data[80:90,7]
  right_left_window_2 <- sqr3_data[100:110,7]
  N2 <- sqr3_data[95:115,8]
  P3 <- sqr3_data[112:162,9]
  P3_400 <- sqr3_data[120:145,10]
  P3_400_LONG <- sqr3_data[125:150,11]
  P3_500 <- sqr3_data[150:175,12]
  LP <- sqr3_data[130:160,13]
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
  mean_P3 <- mean(P3)
  mean_P3_400 <- mean(P3_400)
  mean_P3_400_LONG <- mean(P3_400_LONG)
  mean_P3_500 <- mean(P3_500)
  mean_LP <- mean(LP)
  means_sqr3 <- c(mean_C1, mean_P1, mean_N1, mean_occ_1, mean_occ_2, mean_left_1, 
                  mean_left_2, mean_right_1, mean_right_2, mean_right_left_1, 
                  mean_right_left_2, mean_N2, mean_P3, mean_P3_400, mean_P3_400_LONG,
                  mean_P3_500, mean_LP)
}

# Random, session 3
rand_ses3_means_P3 <- function(rand3_file) {
  rand3_data <- read.delim(paste('./Data/Data_BVA/', rand3_file, sep = ""), sep = " ", 
                           dec = ",", header = TRUE)
  C1 <- rand3_data[40:50,1]
  P1 <- rand3_data[51:61,2]
  N1 <- rand3_data[62:75,3]
  occ_window_1 <- rand3_data[80:90,4]
  occ_window_2 <- rand3_data[100:110,4]
  left_window_1 <- rand3_data[80:90,5]
  left_window_2 <- rand3_data[100:110,5]
  right_window_1 <- rand3_data[80:90,6]
  right_window_2 <- rand3_data[100:110,6]
  right_left_window_1 <- rand3_data[80:90,7]
  right_left_window_2 <- rand3_data[100:110,7]
  N2 <- rand3_data[95:115,8]
  P3 <- rand3_data[112:162,9]
  P3_400 <- rand3_data[120:145,10]
  P3_400_LONG <- rand3_data[125:150,11]
  P3_500 <- rand3_data[150:175,12]
  LP <- rand3_data[130:160,13]
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
  mean_P3 <- mean(P3)
  mean_P3_400 <- mean(P3_400)
  mean_P3_400_LONG <- mean(P3_400_LONG)
  mean_P3_500 <- mean(P3_500)
  mean_LP <- mean(LP)
  means_rand3 <- c(mean_C1, mean_P1, mean_N1, mean_occ_1, mean_occ_2, mean_left_1, 
                  mean_left_2, mean_right_1, mean_right_2, mean_right_left_1, 
                  mean_right_left_2, mean_N2, mean_P3, mean_P3_400, mean_P3_400_LONG,
                  mean_P3_500, mean_LP)
}

# Compute means for each file
sqr3_means_P3 <- lapply(sqr3_fnames_P3, sqr_ses3_means_P3)
rand3_means_P3 <- lapply(rand3_fnames_P3, rand_ses3_means_P3)

# Coerce lists to data frames
sqr3_P3_dat <- data.frame(matrix(unlist(sqr3_means_P3), nrow = length(sqr3_means_P3), 
                              byrow = T))
rand3_P3_dat <- data.frame(matrix(unlist(rand3_means_P3), nrow = length(rand3_means_P3), 
                               byrow = T))

# Rename columms
colnames(sqr3_P3_dat) <- c("C1.sqr", "P1.sqr", "N1.sqr", "occ.sqr.nd1", 
                        "occ.sqr.nd2", "left.sqr.nd1", "left.sqr.nd2", 
                        "right.sqr.nd1", "right.sqr.nd2", "RL.sqr.nd1", 
                        "RL.sqr.nd2", "N2.sqr", "P3.sqr", "P3.400.sqr", 
                        "P3.400.LONG.sqr","P3.500.sqr", "LP.sqr")
colnames(rand3_P3_dat) <- c("C1.rand", "P1.rand", "N1.rand", "occ.rand.nd1", 
                         "occ.rand.nd2", "left.rand.nd1", "left.rand.nd2", 
                         "right.rand.nd1", "right.rand.nd2", "RL.rand.nd1", 
                         "RL.rand.nd2", "N2.rand", "P3.rand", "P3.400.rand", 
                         "P3.400.LONG.rand", "P3.500.rand","LP.rand")

# Merge data frames
rep_data_P3 <- cbind(sqr3_P3_dat,rand3_P3_dat)

# Add group variable
rep_data_P3$group <- c("aware", "unaware", "unaware", "unaware",
                    "aware", "unaware", "aware", "aware", "aware", "aware",
                    "unaware", "aware", "unaware", "unaware", "aware", "aware",
                    "aware", "aware", "unaware", "aware", "unaware", "unaware",
                    "unaware", "unaware", "unaware", "unaware",
                    "aware", "aware", "unaware", "unaware")
rep_data_P3$group.original <- c("aware", "unaware", "unaware", "aware", 
                             "aware", "unaware", "aware", "aware", "aware", "aware", 
                             "unaware", "aware", "aware", "unaware", "aware", "aware", 
                             "aware", "aware", "unaware", "aware", "unaware", "unaware", 
                             "unaware", "unaware", "unaware", 
                             "unaware", "aware", "aware", "aware", 
                             "unaware")

# Add subject number
Subject <- c(2,3,4,6:11, 13:26, 29:30, 32, 34:37)
rep_data_P3 <- cbind(Subject, rep_data_P3)

row.names(rep_data_P3) <- 1:30

# Rename columns to separate configuration number using sep = "_"
names(rep_data_P3)[names(rep_data_P3) == "C1.sqr"] <- "C1_1"
names(rep_data_P3)[names(rep_data_P3) == "C1.rand"] <- "C1_2"
names(rep_data_P3)[names(rep_data_P3) == "P1.sqr"] <- "P1_1"
names(rep_data_P3)[names(rep_data_P3) == "P1.rand"] <- "P1_2"
names(rep_data_P3)[names(rep_data_P3) == "N1.sqr"] <- "N1_1"
names(rep_data_P3)[names(rep_data_P3) == "N1.rand"] <- "N1_2"
names(rep_data_P3)[names(rep_data_P3) == "occ.sqr.nd1"] <- "occ.nd1_1"
names(rep_data_P3)[names(rep_data_P3) == "occ.sqr.nd2"] <- "occ.nd2_1"
names(rep_data_P3)[names(rep_data_P3) == "occ.rand.nd1"] <- "occ.nd1_2"
names(rep_data_P3)[names(rep_data_P3) == "occ.rand.nd2"] <- "occ.nd2_2"
names(rep_data_P3)[names(rep_data_P3) == "left.sqr.nd1"] <- "left.nd1_1"
names(rep_data_P3)[names(rep_data_P3) == "left.sqr.nd2"] <- "left.nd2_1"
names(rep_data_P3)[names(rep_data_P3) == "left.rand.nd1"] <- "left.nd1_2"
names(rep_data_P3)[names(rep_data_P3) == "left.rand.nd2"] <- "left.nd2_2"
names(rep_data_P3)[names(rep_data_P3) == "right.sqr.nd1"] <- "right.nd1_1"
names(rep_data_P3)[names(rep_data_P3) == "right.sqr.nd2"] <- "right.nd2_1"
names(rep_data_P3)[names(rep_data_P3) == "right.rand.nd1"] <- "right.nd1_2"
names(rep_data_P3)[names(rep_data_P3) == "right.rand.nd2"] <- "right.nd2_2"
names(rep_data_P3)[names(rep_data_P3) == "RL.sqr.nd1"] <- "RL.nd1_1"
names(rep_data_P3)[names(rep_data_P3) == "RL.sqr.nd2"] <- "RL.nd2_1"
names(rep_data_P3)[names(rep_data_P3) == "RL.rand.nd1"] <- "RL.nd1_2"
names(rep_data_P3)[names(rep_data_P3) == "RL.rand.nd2"] <- "RL.nd2_2"
names(rep_data_P3)[names(rep_data_P3) == "N2.sqr"] <- "N2_1"
names(rep_data_P3)[names(rep_data_P3) == "N2.rand"] <- "N2_2"
names(rep_data_P3)[names(rep_data_P3) == "P3.sqr"] <- "P3_1"
names(rep_data_P3)[names(rep_data_P3) == "P3.rand"] <- "P3_2"
names(rep_data_P3)[names(rep_data_P3) == "P3.400.sqr"] <- "P3.400_1"
names(rep_data_P3)[names(rep_data_P3) == "P3.400.rand"] <- "P3.400_2" 
names(rep_data_P3)[names(rep_data_P3) == "P3.400.LONG.sqr"] <- "P3.400.long_1"
names(rep_data_P3)[names(rep_data_P3) == "P3.400.LONG.rand"] <- "P3.400.long_2"
names(rep_data_P3)[names(rep_data_P3) == "P3.500.sqr"] <- "P3.500_1"
names(rep_data_P3)[names(rep_data_P3) == "P3.500.rand"] <- "P3.500_2"
names(rep_data_P3)[names(rep_data_P3) == "LP.sqr"] <- "LP_1"
names(rep_data_P3)[names(rep_data_P3) == "LP.rand"] <- "LP_2"
# Extracts configuration number to column
rep_data_P3_long <- reshape(rep_data_P3, varying = c("C1_1", "C1_2", "P1_1", "P1_2",
                                                     "N1_1", "N1_2", "occ.nd1_1", 
                                                     "occ.nd2_1", "occ.nd1_2", 
                                                     "occ.nd2_2", "left.nd1_1", 
                                                     "left.nd2_1", "left.nd1_2", 
                                                     "left.nd2_2", "right.nd1_1", 
                                                     "right.nd2_1", "right.nd1_2", 
                                                     "right.nd2_2", "RL.nd1_1", 
                                                     "RL.nd2_1", "RL.nd1_2", 
                                                     "RL.nd2_2", "N2_1", "N2_2",
                                                     "P3_1", "P3_2", "P3.400_1",
                                                     "P3.400_2", "P3.400.long_1",
                                                     "P3.400.long_2", "P3.500_1", 
                                                     "P3.500_2", "LP_1", "LP_2"),  
                          direction = "long", idvar = " Subject", sep = "_")
# Rename configuration column name
names(rep_data_P3_long)[names(rep_data_P3_long) == "time"] <- "configuration"
# Rename configuration levels
rep_data_P3_long$configuration[rep_data_P3_long$configuration == 1] <- "sqr"
rep_data_P3_long$configuration[rep_data_P3_long$configuration == 2] <- "rand"


# Remove redundant subject column
rep_data_P3_long[,ncol(rep_data_P3_long)]<- NULL

# Convert configuration, group and session to factors
rep_data_P3_long$group <- factor(rep_data_P3_long$group)
rep_data_P3_long$group.original <- factor(rep_data_P3_long$group.original)
rep_data_P3_long$configuration <- factor(rep_data_P3_long$configuration)