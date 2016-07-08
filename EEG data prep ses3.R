
# Import file names in working directory
sqr3_fnames <- list.files(getwd(), pattern = "3_ROI Evaluator Sqr_sqr")
rand3_fnames <- list.files(getwd(), pattern = "3_ROI Evaluator Rand_rand")

#-------------Individual windows extraction by file----------------
# Sqr, session 3
sqr_ses3_means <- function(sqr3_file) {
  sqr3_data <- read.delim(sqr3_file, sep = " ", dec = ",",
                          header = TRUE)
  # Remove last two columns
  sqr3_data$Right <- NULL
  sqr3_data$Occ.par.1 <- NULL
  # Rename last column
  names(sqr3_data)[names(sqr3_data) == "Occ.par"] <- "Right"
  # Compute means for each region and append to main data frame
  nd1_window <- sqr3_data[70:80,1]
  nd2_window_left <- sqr3_data[80:90,2]
  nd2_window_right <- sqr3_data[80:90,3]
  SN_window <- sqr3_data[90:100,3]
  pd1_window <- sqr3_data[110:120,3]
  pd1_window <- sqr3_data[130:140,3]
  mean_occ <- mean(nd1_window)
  mean_left <- mean(nd2_window_left)
  mean_right <- mean(nd2_window_right)
  means_sqr1 <- c(mean_occ, mean_left, mean_right)
}

# Random, session 3
rand_ses3_means <- function(rand3_file) {
  rand3_data <- read.delim(rand3_file, sep = " ", dec = ",",
                           header = TRUE)
  # Remove last two columns
  rand1_data$Right <- NULL
  rand1_data$Occ.par.1 <- NULL
  # Rename last column
  names(rand3_data)[names(rand3_data) == "Occ.par"] <- "Right"
  nd1_window <- rand3_data[70:80,1]
  nd2_window_left <- rand3_data[80:90,2]
  nd2_window_right <- rand3_data[80:90,3]
  mean_occ <- mean(nd1_window)
  mean_left <- mean(nd2_window_left)
  mean_right <- mean(nd2_window_right)
  means_rand3 <- c(mean_occ, mean_left, mean_right)
}

