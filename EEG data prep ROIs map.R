library(reshape)

# Import file names in working directory
ROImap_sqr1_fnames <- list.files(getwd(), pattern = "_1_ROI map Sqr")
ROImap_sqr2_fnames <- list.files(getwd(), pattern = "_2_ROI map Sqr")
ROImap_rand1_fnames <- list.files(getwd(), pattern = "_1_ROI map Rand")
ROImap_rand2_fnames <- list.files(getwd(), pattern = "_2_ROI map Rand")
# Concatenate all lists in a single list
ROImap_list_fnames <- c(ROImap_sqr1_fnames, ROImap_sqr2_fnames, ROImap_rand1_fnames, 
                 ROImap_rand2_fnames)

#-------------Individual windows extraction by file----------------
# Sqr, session 1
ROImap_sqr_ses1_means <- function(sqr1_file) {
  sqr1_data <- read.delim(sqr1_file, sep = " ", dec = ",",
                          header = TRUE)
  # Compute means for each window
  ROI_P1_occ_right <- sqr1_data[50:63,1]
  ROI_N1_occ <- sqr1_data[62:88,2]
  ROI_P2_right <- sqr1_data[80:95,3]
  ROI_P2_anterior <- sqr1_data[80:95,4]
  ROI_N2_occ <- sqr1_data[95:115,5]
  LP_left <- sqr1_data[140:173,6]
  LP_central <- sqr1_data[137:175,7]
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
ROImap_sqr_ses2_means <- function(sqr2_file) {
  sqr2_data <- read.delim(sqr2_file, sep = " ", dec = ",",
                          header = TRUE)
  # Compute means for each window
  ROI_P1_occ_right <- sqr2_data[50:63,1]
  ROI_N1_occ <- sqr2_data[62:88,2]
  ROI_P2_right <- sqr2_data[80:95,3]
  ROI_P2_anterior <- sqr2_data[80:95,4]
  ROI_N2_occ <- sqr2_data[95:115,5]
  LP_left <- sqr2_data[140:173,6]
  LP_central <- sqr2_data[137:175,7]
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
ROImap_rand_ses1_means <- function(rand1_file) {
  rand1_data <- read.delim(rand1_file, sep = " ", dec = ",",
                           header = TRUE)
  # Compute means for each window
  ROI_P1_occ_right <- rand1_data[50:63,1]
  ROI_N1_occ <- rand1_data[62:88,2]
  ROI_P2_right <- rand1_data[80:95,3]
  ROI_P2_anterior <- rand1_data[80:95,4]
  ROI_N2_occ <- rand1_data[95:115,5]
  LP_left <- rand1_data[140:173,6]
  LP_central <- rand1_data[137:175,7]
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
ROImap_rand_ses2_means <- function(rand2_file) {
  rand2_data <- read.delim(rand2_file, sep = " ", dec = ",",
                           header = TRUE)
  # Compute means for each window
  ROI_P1_occ_right <- rand2_data[50:63,1]
  ROI_N1_occ <- rand2_data[62:88,2]
  ROI_P2_right <- rand2_data[80:95,3]
  ROI_P2_anterior <- rand2_data[80:95,4]
  ROI_N2_occ <- rand2_data[95:115,5]
  LP_left <- rand2_data[140:173,6]
  LP_central <- rand2_data[137:175,7]
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
ROImap_sqr1_means <- lapply(ROImap_sqr1_fnames, ROImap_sqr_ses1_means)
ROImap_sqr2_means <- lapply(ROImap_sqr2_fnames, ROImap_sqr_ses2_means)
ROImap_rand1_means <- lapply(ROImap_rand1_fnames, ROImap_rand_ses1_means)
ROImap_rand2_means <- lapply(ROImap_rand2_fnames, ROImap_rand_ses2_means)

# Coerce lists to data frames
ROImap_sqr1_dat <- data.frame(matrix(unlist(ROImap_sqr1_means), nrow = 34, byrow = T))
ROImap_sqr2_dat <- data.frame(matrix(unlist(ROImap_sqr2_means), nrow = 34, byrow = T))
ROImap_rand1_dat <- data.frame(matrix(unlist(ROImap_rand1_means), nrow = 34, byrow = T))
ROImap_rand2_dat <- data.frame(matrix(unlist(ROImap_rand2_means), nrow = 34, byrow = T))


# Rename columms
colnames(ROImap_sqr1_dat) <- c("P1_occ_right.1", "N1_occ.1", "P2_right.1",
                               "P2_anterior.1", "N2_occ.1", 
                               "LP_left.1", "LP_central.1")
colnames(ROImap_sqr2_dat) <- c("P1_occ_right.2", "N1_occ.2", "P2_right.2",
                               "P2_anterior.2", "N2_occ.2", 
                               "LP_left.2", "LP_central.2")
colnames(ROImap_rand1_dat) <- c("P1_occ_right.1", "N1_occ.1", "P2_right.1",
                                "P2_anterior.1", "N2_occ.1", 
                                "LP_left.1", "LP_central.1")
colnames(ROImap_rand2_dat) <- c("P1_occ_right.2", "N1_occ.2", "P2_right.2",
                                "P2_anterior.2", "N2_occ.2", 
                                "LP_left.2", "LP_central.2")


# Add group variable to square and random datasets
ROImap_sqr1_dat$group <- c("unaware", "aware", "unaware", "unaware", "unaware",
                    "aware", "unaware", "aware", "aware", "aware", "aware",
                    "unaware", "aware", "unaware", "unaware", "aware", "aware",
                    "aware", "aware", "unaware", "aware", "unaware", "unaware",
                    "unaware", "aware", "unaware", "unaware", "unaware", "unaware",
                    "unaware", "aware", "aware", "unaware", "unaware")
ROImap_sqr1_dat$group.original <- c("unaware", "aware", "unaware", "unaware", "aware", 
                             "aware", "unaware", "aware", "aware", "aware", "aware", 
                             "unaware", "aware", "aware", "unaware", "aware", "aware", 
                             "aware", "aware", "unaware", "aware", "unaware", "unaware", 
                             "unaware", "aware", "unaware", "unaware", "unaware", 
                             "unaware", "aware", "aware", "aware", "aware", 
                             "unaware")

# Add group variable
ROImap_rand1_dat$group <- c("unaware", "aware", "unaware", "unaware", "unaware",
                           "aware", "unaware", "aware", "aware", "aware", "aware",
                           "unaware", "aware", "unaware", "unaware", "aware", "aware",
                           "aware", "aware", "unaware", "aware", "unaware", "unaware",
                           "unaware", "aware", "unaware", "unaware", "unaware", "unaware",
                           "unaware", "aware", "aware", "unaware", "unaware")
ROImap_rand1_dat$group.original <- c("unaware", "aware", "unaware", "unaware", "aware", 
                                    "aware", "unaware", "aware", "aware", "aware", "aware", 
                                    "unaware", "aware", "aware", "unaware", "aware", "aware", 
                                    "aware", "aware", "unaware", "aware", "unaware", "unaware", 
                                    "unaware", "aware", "unaware", "unaware", "unaware", 
                                    "unaware", "aware", "aware", "aware", "aware", 
                                    "unaware")

# Add subject number
subject <- c(1,2,3,4,6:11, 13:30, 32:37)
configuration <- rep("sqr", 34)
ROImap_sqr1_dat <- cbind(subject, configuration, ROImap_sqr1_dat)

# Merge square data frames
ROImap_sqr_dat <- cbind(ROImap_sqr1_dat, ROImap_sqr2_dat)
ROImap_sqr_dat <- ROImap_sqr_dat[-c(25,26),]
# Extracts session number to column
ROImap_sqr_dat_long <- reshape(ROImap_sqr_dat, 
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
names(ROImap_sqr_dat_long)[names(ROImap_sqr_dat_long) == "time"] <- "session"


configuration <- rep("rand", 34)
ROImap_rand1_dat <- cbind(subject, configuration, ROImap_rand1_dat)
# Merge random data frames
ROImap_rand_dat <- cbind(ROImap_rand1_dat, ROImap_rand2_dat)
ROImap_rand_dat <- ROImap_rand_dat[-c(25,26),]
# Extracts session number to column
ROImap_rand_dat_long <- reshape(ROImap_rand_dat,
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
names(ROImap_rand_dat_long)[names(ROImap_rand_dat_long) == "time"] <- "session"

# Rbind aquare and random data frames
ROImap_data_long <- rbind(ROImap_sqr_dat_long, ROImap_rand_dat_long)

# Coerce IVs to factors
ROImap_data_long$group <- factor(ROImap_data_long$group)
ROImap_data_long$group.original <- factor(ROImap_data_long$group.original)
ROImap_data_long$session <- factor(ROImap_data_long$session)
ROImap_data_long$configuration <- factor(ROImap_data_long$configuration)
