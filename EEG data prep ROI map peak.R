library(reshape)

# Import file names in working directory
ROIpeak_sqr1_fnames <- list.files(getwd(), pattern = "_1_ROI map Sqr")
ROIpeak_sqr2_fnames <- list.files(getwd(), pattern = "_2_ROI map Sqr")
ROIpeak_rand1_fnames <- list.files(getwd(), pattern = "_1_ROI map Rand")
ROIpeak_rand2_fnames <- list.files(getwd(), pattern = "_2_ROI map Rand")
# Concatenate all lists in a single list
ROIpeak_list_fnames <- c(ROIpeak_sqr1_fnames, ROIpeak_sqr2_fnames, 
                         ROIpeak_rand1_fnames, ROIpeak_rand2_fnames)

#-------------Individual windows extraction by file----------------
# Sqr, session 1
ROIpeak_sqr_ses1_means <- function(sqr1_file) {
  sqr1_data <- read.delim(sqr1_file, sep = " ", dec = ",",
                          header = TRUE)
  # Compute means for each window
  ROI_P1_occ_right <- sqr1_data[57,1]
  ROI_N1_occ <- sqr1_data[67,2]
  ROI_P2_right <- sqr1_data[80:95,3]
  ROI_P2_anterior <- sqr1_data[85,4]
  ROI_N2_occ <- sqr1_data[107,5]
  LP_left <- sqr1_data[140:173,6]
  LP_central <- sqr1_data[140:173,7]
  mean_window1 <- mean(ROI_P1_occ_right)
  mean_window2 <- mean(ROI_N1_occ)
  mean_window3 <- mean(ROI_P2_right)
  mean_window4 <- mean(ROI_P2_anterior)
  mean_window5 <- mean(ROI_N2_occ)
  mean_window6 <- mean(LP_left)
  mean_window7 <- mean(LP_central)
  means_sqr1 <- c(mean_window1, mean_window2, mean_window3, mean_window4, mean_window5,
                  mean_window6, mean_window7)
}

# Sqr, session 2
ROIpeak_sqr_ses2_means <- function(sqr2_file) {
  sqr2_data <- read.delim(sqr2_file, sep = " ", dec = ",",
                          header = TRUE)
  # Compute means for each window
  ROI_P1_occ_right <- sqr2_data[57,1]
  ROI_N1_occ <- sqr2_data[67,2]
  ROI_P2_right <- sqr2_data[80:95,3]
  ROI_P2_anterior <- sqr2_data[85,4]
  ROI_N2_occ <- sqr2_data[107,5]
  LP_left <- sqr2_data[140:173,6]
  LP_central <- sqr2_data[140:173,7]
  mean_window1 <- mean(ROI_P1_occ_right)
  mean_window2 <- mean(ROI_N1_occ)
  mean_window3 <- mean(ROI_P2_right)
  mean_window4 <- mean(ROI_P2_anterior)
  mean_window5 <- mean(ROI_N2_occ)
  mean_window6 <- mean(LP_left)
  mean_window7 <- mean(LP_central)
  means_sqr2 <- c(mean_window1, mean_window2, mean_window3, mean_window4, mean_window5,
                  mean_window6, mean_window7)
}

# Random, session 1
ROIpeak_rand_ses1_means <- function(rand1_file) {
  rand1_data <- read.delim(rand1_file, sep = " ", dec = ",",
                           header = TRUE)
  # Compute means for each window
  ROI_P1_occ_right <- rand1_data[57,1]
  ROI_N1_occ <- rand1_data[67,2]
  ROI_P2_right <- rand1_data[80:95,3]
  ROI_P2_anterior <- rand1_data[85,4]
  ROI_N2_occ <- rand1_data[107,5]
  LP_left <- rand1_data[140:173,6]
  LP_central <- rand1_data[140:173,7]
  mean_window1 <- mean(ROI_P1_occ_right)
  mean_window2 <- mean(ROI_N1_occ)
  mean_window3 <- mean(ROI_P2_right)
  mean_window4 <- mean(ROI_P2_anterior)
  mean_window5 <- mean(ROI_N2_occ)
  mean_window6 <- mean(LP_left)
  mean_window7 <- mean(LP_central)
  means_rand1 <- c(mean_window1, mean_window2, mean_window3, mean_window4, mean_window5,
                   mean_window6, mean_window7)
}

# Random, session 2
ROIpeak_rand_ses2_means <- function(rand2_file) {
  rand2_data <- read.delim(rand2_file, sep = " ", dec = ",",
                           header = TRUE)
  # Compute means for each window
  ROI_P1_occ_right <- rand2_data[57,1]
  ROI_N1_occ <- rand2_data[67,2]
  ROI_P2_right <- rand2_data[80:95,3]
  ROI_P2_anterior <- rand2_data[85,4]
  ROI_N2_occ <- rand2_data[107,5]
  LP_left <- rand2_data[140:173,6]
  LP_central <- rand2_data[140:173,7]
  mean_window1 <- mean(ROI_P1_occ_right)
  mean_window2 <- mean(ROI_N1_occ)
  mean_window3 <- mean(ROI_P2_right)
  mean_window4 <- mean(ROI_P2_anterior)
  mean_window5 <- mean(ROI_N2_occ)
  mean_window6 <- mean(LP_left)
  mean_window7 <- mean(LP_central)
  means_rand2 <- c(mean_window1, mean_window2, mean_window3, mean_window4, mean_window5,
                   mean_window6, mean_window7)
}

# Compute means for each file
ROIpeak_sqr1_means <- lapply(ROIpeak_sqr1_fnames, ROIpeak_sqr_ses1_means)
ROIpeak_sqr2_means <- lapply(ROIpeak_sqr2_fnames, ROIpeak_sqr_ses2_means)
ROIpeak_rand1_means <- lapply(ROIpeak_rand1_fnames, ROIpeak_rand_ses1_means)
ROIpeak_rand2_means <- lapply(ROIpeak_rand2_fnames, ROIpeak_rand_ses2_means)

# Coerce lists to data frames
ROIpeak_sqr1_dat <- data.frame(matrix(unlist(ROIpeak_sqr1_means), nrow = 34, byrow = T))
ROIpeak_sqr2_dat <- data.frame(matrix(unlist(ROIpeak_sqr2_means), nrow = 34, byrow = T))
ROIpeak_rand1_dat <- data.frame(matrix(unlist(ROIpeak_rand1_means), nrow = 34, byrow = T))
ROIpeak_rand2_dat <- data.frame(matrix(unlist(ROIpeak_rand2_means), nrow = 34, byrow = T))


# Rename columms
colnames(ROIpeak_sqr1_dat) <- c("P1_occ_right.1", "N1_occ.1", "P2_right.1",
                                "P2_anterior.1", "N2_occ.1", 
                                "LP_left.1", "LP_central.1")
colnames(ROIpeak_sqr2_dat) <- c("P1_occ_right.2", "N1_occ.2", "P2_right.2",
                                "P2_anterior.2", "N2_occ.2", 
                                "LP_left.2", "LP_central.2")
colnames(ROIpeak_rand1_dat) <- c("P1_occ_right.1", "N1_occ.1", "P2_right.1",
                                 "P2_anterior.1", "N2_occ.1", 
                                 "LP_left.1", "LP_central.1")
colnames(ROIpeak_rand2_dat) <- c("P1_occ_right.2", "N1_occ.2", "P2_right.2",
                                 "P2_anterior.2", "N2_occ.2", 
                                 "LP_left.2", "LP_central.2")


# Add group variable to square and random datasets
ROIpeak_sqr1_dat$group <- c("unaware", "aware", "unaware", "unaware", "unaware",
                            "aware", "unaware", "aware", "aware", "aware", "aware",
                            "unaware", "aware", "unaware", "unaware", "aware", "aware",
                            "aware", "aware", "unaware", "aware", "unaware", "unaware",
                            "unaware", "aware", "unaware", "unaware", "unaware", "unaware",
                            "unaware", "aware", "aware", "unaware", "unaware")
ROIpeak_sqr1_dat$group.original <- c("unaware", "aware", "unaware", "unaware", "aware", 
                                     "aware", "unaware", "aware", "aware", "aware", "aware", 
                                     "unaware", "aware", "aware", "unaware", "aware", "aware", 
                                     "aware", "aware", "unaware", "aware", "unaware", "unaware", 
                                     "unaware", "aware", "unaware", "unaware", "unaware", 
                                     "unaware", "aware", "aware", "aware", "aware", 
                                     "unaware")

# Add group variable
ROIpeak_rand1_dat$group <- c("unaware", "aware", "unaware", "unaware", "unaware",
                             "aware", "unaware", "aware", "aware", "aware", "aware",
                             "unaware", "aware", "unaware", "unaware", "aware", "aware",
                             "aware", "aware", "unaware", "aware", "unaware", "unaware",
                             "unaware", "aware", "unaware", "unaware", "unaware", "unaware",
                             "unaware", "aware", "aware", "unaware", "unaware")
ROIpeak_rand1_dat$group.original <- c("unaware", "aware", "unaware", "unaware", "aware", 
                                      "aware", "unaware", "aware", "aware", "aware", "aware", 
                                      "unaware", "aware", "aware", "unaware", "aware", "aware", 
                                      "aware", "aware", "unaware", "aware", "unaware", "unaware", 
                                      "unaware", "aware", "unaware", "unaware", "unaware", 
                                      "unaware", "aware", "aware", "aware", "aware", 
                                      "unaware")

# Add subject number
subject <- c(1,2,3,4,6:11, 13:30, 32:37)
configuration <- rep("sqr", 34)
ROIpeak_sqr1_dat <- cbind(subject, configuration, ROIpeak_sqr1_dat)

# Merge square data frames
ROIpeak_sqr_dat <- cbind(ROIpeak_sqr1_dat, ROIpeak_sqr2_dat)
ROIpeak_sqr_dat <- ROIpeak_sqr_dat[-c(25,26),]
# Extracts session number to column
ROIpeak_sqr_dat_long <- reshape(ROIpeak_sqr_dat, 
                                varying = c("P1_occ_right.1", "N1_occ.1", "P2_right.1",
                                            "P2_anterior.1", "N2_occ.1", 
                                            "LP_left.1", "LP_central.1", 
                                            "P1_occ_right.2", "N1_occ.2", "P2_right.2",
                                            "P2_anterior.2", "N2_occ.2", 
                                            "LP_left.2", "LP_central.2"),  
                                direction = "long", 
                                idvar = c("subject", "configuration", "group", 
                                          "group.original"), sep = ".")
# Rename session column
names(ROIpeak_sqr_dat_long)[names(ROIpeak_sqr_dat_long) == "time"] <- "session"


configuration <- rep("rand", 34)
ROIpeak_rand1_dat <- cbind(subject, configuration, ROIpeak_rand1_dat)
# Merge random data frames
ROIpeak_rand_dat <- cbind(ROIpeak_rand1_dat, ROIpeak_rand2_dat)
ROIpeak_rand_dat <- ROIpeak_rand_dat[-c(25,26),]
# Extracts session number to column
ROIpeak_rand_dat_long <- reshape(ROIpeak_rand_dat,
                                 varying = c("P1_occ_right.1", "N1_occ.1", 
                                             "P2_right.1", "P2_anterior.1", 
                                             "N2_occ.1", "LP_left.1", 
                                             "LP_central.1", "P1_occ_right.2", 
                                             "N1_occ.2", "P2_right.2", 
                                             "P2_anterior.2", "N2_occ.2", 
                                             "LP_left.2", "LP_central.2"), 
                                 direction = "long", idvar = c("subject", 
                                                               "configuration", 
                                                               "group", 
                                                               "group.original"),
                                 sep = ".")
# Rename session column
names(ROIpeak_rand_dat_long)[names(ROIpeak_rand_dat_long) == "time"] <- "session"

# Rbind aquare and random data frames
ROIpeak_data_long <- rbind(ROIpeak_sqr_dat_long, ROIpeak_rand_dat_long)

# Coerce IVs to factors
ROIpeak_data_long$group <- factor(ROIpeak_data_long$group)
ROIpeak_data_long$group.original <- factor(ROIpeak_data_long$group.original)
ROIpeak_data_long$session <- factor(ROIpeak_data_long$session)
ROIpeak_data_long$configuration <- factor(ROIpeak_data_long$configuration)
