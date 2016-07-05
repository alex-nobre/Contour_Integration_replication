
# Import file names in working directory
sqr1_fnames <- list.files(getwd(), pattern = "1_ROI Evaluator Sqr Cz")
sqr2_fnames <- list.files(getwd(), pattern = "2_ROI Evaluator Sqr Cz")
rand1_fnames <- list.files(getwd(), pattern = "1_ROI Evaluator Rand Cz")
rand2_fnames <- list.files(getwd(), pattern = "2_ROI Evaluator Rand Cz")
# Concatenate all lists in a single list
list_fnames <- c(sqr1_fnames, sqr2_fnames, rand1_fnames, rand2_fnames)

#-----------------Extract occ values for each session/config-------------
# Sqr, session 1
sqr_ses1_points <- function(sqr1_file) {
  sqr1_data <- read.delim(sqr1_file, sep = " ", dec = ",",
                          header = TRUE)
  # Remove last two columns
  sqr1_data$Right <- NULL
  sqr1_data$Occ.par.1 <- NULL
  # Rename last column
  names(sqr1_data)[names(sqr1_data) == "Occ.par"] <- "Right"
  # Remove left & right
  sqr1_data$Left <- NULL
  sqr1_data$Right <- NULL
  # Coerces table to data frame
  sqr1_occ_points <- sqr1_data$Occ
}
# Sqr, session 2
sqr_ses2_points <- function(sqr2_file) {
  sqr2_data <- read.delim(sqr2_file, sep = " ", dec = ",",
                          header = TRUE)
  # Remove last two columns
  sqr2_data$Right <- NULL
  sqr2_data$Occ.par.1 <- NULL
  # Rename last column
  names(sqr2_data)[names(sqr2_data) == "Occ.par"] <- "Right"
  # Remove left & right
  sqr2_data$Left <- NULL
  sqr2_data$Right <- NULL
  # Coerces table to data frame
  sqr2_occ_points <- sqr2_data$Occ
}
# Rand, session 1
rand_ses1_points <- function(rand1_file) {
  rand1_data <- read.delim(rand1_file, sep = " ", dec = ",",
                          header = TRUE)
  # Remove last two columns
  rand1_data$Right <- NULL
  rand1_data$Occ.par.1 <- NULL
  # Rename last column
  names(rand1_data)[names(rand1_data) == "Occ.par"] <- "Right"
  # Remove left & right
  rand1_data$Left <- NULL
  rand1_data$Right <- NULL
  # Coerces table to data frame
  rand1_occ_points <- rand1_data$Occ
}
# Rand, session 2
rand_ses2_points <- function(rand2_file) {
  rand2_data <- read.delim(rand2_file, sep = " ", dec = ",",
                           header = TRUE)
  # Remove last two columns
  rand2_data$Right <- NULL
  rand2_data$Occ.par.1 <- NULL
  # Rename last column
  names(rand2_data)[names(rand2_data) == "Occ.par"] <- "Right"
  # Remove left & right
  rand2_data$Left <- NULL
  rand2_data$Right <- NULL
  # Coerces table to data frame
  rand2_occ_points <- rand2_data$Occ
}
# Extract occ time point values for each file
sqr1_points_cz <- lapply(sqr1_fnames, sqr_ses1_points)
sqr2_points_cz <- lapply(sqr2_fnames, sqr_ses2_points)
rand1_points_cz <- lapply(rand1_fnames, rand_ses1_points)
rand2_points_cz <- lapply(rand2_fnames, rand_ses2_points)

# Coerce lists to data frames
sqr1_points_cz_dat <- data.frame(matrix(unlist(sqr1_points_cz), 
                                     nrow = 34, byrow = T))
sqr2_points_cz_dat <- data.frame(matrix(unlist(sqr2_points_cz), 
                                     nrow = 34, byrow = T))
rand1_points_cz_dat <- data.frame(matrix(unlist(rand1_points_cz), 
                                     nrow = 34, byrow = T))
rand2_points_cz_dat <- data.frame(matrix(unlist(rand2_points_cz), 
                                      nrow = 34, byrow = T))

# Remove subs 27 & 28
sqr1_points_cz_dat <- sqr1_points_cz_dat[-c(25, 26),]
sqr2_points_cz_dat <- sqr2_points_cz_dat[-c(25, 26),]
rand1_points_cz_dat <- rand1_points_cz_dat[-c(25, 26),]
rand2_points_cz_dat <- rand2_points_cz_dat[-c(25, 26),]

# Change columns names
col_headers <- seq(-100, 596, by = 4)
colnames(sqr1_points_cz_dat) <- col_headers
colnames(sqr2_points_cz_dat) <- col_headers
colnames(rand1_points_cz_dat) <- col_headers
colnames(rand2_points_cz_dat) <- col_headers

# Compute means for all columsn
sqr1_points_cz_means <- colMeans(sqr1_points_cz_dat)
sqr2_points_cz_means <- colMeans(sqr2_points_cz_dat)
rand1_points_cz_means <- colMeans(rand1_points_cz_dat)
rand2_points_cz_means <- colMeans(rand2_points_cz_dat)

#------------------------Plot Nd1 Grand Averages------------------------------
labels <- levels(factor(rep_data_cz_long2$configuration))
# Nd1 - session 1
plot(col_headers, sqr1_points_cz_means, type = "l", xlab = "Time points", 
     ylab = "mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd1 Grand averages session 1")
lines(col_headers, rand1_points_cz_means, type = "l", col = "blue")
abline(h = 0, col = "black", lty = 2)
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
# Nd1 - session 2
plot(col_headers, sqr2_points_cz_means, type = "l", xlab = "Time points", 
     ylab = "mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd1 Grand averages session 2")
lines(col_headers, rand2_points_cz_means, type = "l", col = "blue")
abline(h = 0, col = "black", lty = 2)
legend('topright', legend = labels, col = c("blue", "red"), 
       lwd=c(2,2))

# Nd2
# Nd1 - session 1
# plot(col_headers, sqr1_points_means, type = "l", xlab = "Time points", 
#      ylab = "mean amplitude", col = "red", ylim = c(-2.5, 1.0),
#      main = "NdGrand averages session 1")
# lines(col_headers, rand1_points_means, type = "l", col = "blue")
# legend('topright', legend = c("ses 1", "ses 2"), col = c("blue", "red"), lwd=c(2,2))
# # Nd1 - session 2
# plot(col_headers, sqr2_points_means, type = "l", xlab = "Time points", 
#      ylab = "mean amplitude", col = "deeppink", ylim = c(-2.5, 1.0),
#      main = "Grand averages session 2")
# lines(col_headers, rand2_points_means, type = "l", col = "cyan3")
# legend('topright', legend = c("ses 1", "ses 2"), col = c("cyan3", "deeppink"), lwd=c(2,2))

# Make separate plots for session, group and window