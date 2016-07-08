# Import file names in working directory
sqr1_fnames <- list.files(getwd(), pattern = "1_ROI Evaluator Sqr Cz")
sqr2_fnames <- list.files(getwd(), pattern = "2_ROI Evaluator Sqr Cz")
rand1_fnames <- list.files(getwd(), pattern = "1_ROI Evaluator Rand Cz")
rand2_fnames <- list.files(getwd(), pattern = "2_ROI Evaluator Rand Cz")
# Concatenate all lists in a single list
list_fnames <- c(sqr1_fnames, sqr2_fnames, rand1_fnames, rand2_fnames)

#--------------Extract square left values for each session/group------------
awareness <- rep_data_cz2$group
# Sqr , session 1
left_sqr_ses1_points <- function(sqr1_file) {
  sqr1_data <- read.delim(sqr1_file, sep = " ", dec = ",",
                          header = TRUE)
  # Remove last two columns
  sqr1_data$Right <- NULL
  sqr1_data$Occ.par.1 <- NULL
  # Rename last column
  names(sqr1_data)[names(sqr1_data) == "Occ.par"] <- "Right"
  # Remove left & right
  sqr1_data$Occ <- NULL
  sqr1_data$Right <- NULL
  # Coerces table to data frame
  sqr1_left_points <- sqr1_data$Left
}
# Sqr, session 2
left_sqr_ses2_points <- function(sqr2_file) {
  sqr2_data <- read.delim(sqr2_file, sep = " ", dec = ",",
                          header = TRUE)
  # Remove last two columns
  sqr2_data$Right <- NULL
  sqr2_data$Occ.par.1 <- NULL
  # Rename last column
  names(sqr2_data)[names(sqr2_data) == "Occ.par"] <- "Right"
  # Remove left & right
  sqr2_data$Occ <- NULL
  sqr2_data$Right <- NULL
  # Coerces table to data frame
  sqr2_left_points <- sqr2_data$Left
}
# Rand, session 1
left_rand_ses1_points <- function(rand1_file) {
  rand1_data <- read.delim(rand1_file, sep = " ", dec = ",",
                           header = TRUE)
  # Remove last two columns
  rand1_data$Right <- NULL
  rand1_data$Occ.par.1 <- NULL
  # Rename last column
  names(rand1_data)[names(rand1_data) == "Occ.par"] <- "Right"
  # Remove left & right
  rand1_data$Occ <- NULL
  rand1_data$Right <- NULL
  # Coerces table to data frame
  rand1_left_points <- rand1_data$Left
}
# Rand, session 2
left_rand_ses2_points <- function(rand2_file) {
  rand2_data <- read.delim(rand2_file, sep = " ", dec = ",",
                           header = TRUE)
  # Remove last two columns
  rand2_data$Right <- NULL
  rand2_data$Occ.par.1 <- NULL
  # Rename last column
  names(rand2_data)[names(rand2_data) == "Occ.par"] <- "Right"
  # Remove left & right
  rand2_data$Occ <- NULL
  rand2_data$Right <- NULL
  # Coerces table to data frame
  rand2_left_points <- rand2_data$Left
}

# Extract left time point values for each file
left_sqr1_points_cz <- lapply(sqr1_fnames, left_sqr_ses1_points)
left_sqr2_points_cz <- lapply(sqr2_fnames, left_sqr_ses2_points)
left_rand1_points_cz <- lapply(rand1_fnames, left_rand_ses1_points)
left_rand2_points_cz <- lapply(rand2_fnames, left_rand_ses2_points)

# Coerce lists to data frames
left_sqr1_points_cz_dat <- data.frame(matrix(unlist(left_sqr1_points_cz), 
                                          nrow = 34, byrow = T))
left_sqr2_points_cz_dat <- data.frame(matrix(unlist(left_sqr2_points_cz), 
                                          nrow = 34, byrow = T))
left_rand1_points_cz_dat <- data.frame(matrix(unlist(left_rand1_points_cz), 
                                         nrow = 34, byrow = T))
left_rand2_points_cz_dat <- data.frame(matrix(unlist(left_rand2_points_cz), 
                                         nrow = 34, byrow = T))

# Remove subs 27 & 28
left_sqr1_points_cz_dat <- left_sqr1_points_cz_dat[-c(25, 26),]
left_sqr2_points_cz_dat <- left_sqr2_points_cz_dat[-c(25, 26),]
left_rand1_points_cz_dat <- left_rand1_points_cz_dat[-c(25, 26),]
left_rand2_points_cz_dat <- left_rand2_points_cz_dat[-c(25, 26),]

# Add group column
left_sqr1_points_cz_dat <- cbind(left_sqr1_points_cz_dat, awareness)
left_sqr2_points_cz_dat <- cbind(left_sqr2_points_cz_dat, awareness)
left_rand1_points_cz_dat <- cbind(left_rand1_points_cz_dat, awareness)
left_rand2_points_cz_dat <- cbind(left_rand2_points_cz_dat, awareness)

# Change columns names
col_headers <- c(seq(-100, 596, by = 4), "group")
colnames(left_sqr1_points_cz_dat) <- col_headers
colnames(left_sqr2_points_cz_dat) <- col_headers
colnames(left_rand1_points_cz_dat) <- col_headers
colnames(left_rand2_points_cz_dat) <- col_headers

# Subset for means
left_sqr1_points_cz_aware <- subset(left_sqr1_points_cz_dat, 
                                    group == "aware")
left_sqr2_points_cz_aware <- subset(left_sqr2_points_cz_dat, 
                                    group == "aware")
left_sqr1_points_cz_unaware <- subset(left_sqr1_points_cz_dat, 
                                      group == "unaware")
left_sqr2_points_cz_unaware <- subset(left_sqr2_points_cz_dat, 
                                      group == "unaware")
left_rand1_points_cz_aware <- subset(left_rand1_points_cz_dat, 
                                    group == "aware")
left_rand2_points_cz_aware <- subset(left_rand2_points_cz_dat, 
                                    group == "aware")
left_rand1_points_cz_unaware <- subset(left_rand1_points_cz_dat, 
                                      group == "unaware")
left_rand2_points_cz_unaware <- subset(left_rand2_points_cz_dat, 
                                      group == "unaware")


# Remove group column
left_sqr1_points_cz_aware$group <- NULL
left_sqr2_points_cz_aware$group <- NULL
left_sqr1_points_cz_unaware$group <- NULL
left_sqr2_points_cz_unaware$group <- NULL
left_rand1_points_cz_aware$group <- NULL
left_rand2_points_cz_aware$group <- NULL
left_rand1_points_cz_unaware$group <- NULL
left_rand2_points_cz_unaware$group <- NULL

# Compute means for all columns
left_sqr1_points_cz_aware_means <- colMeans(left_sqr1_points_cz_aware)
left_sqr2_points_cz_aware_means <- colMeans(left_sqr2_points_cz_aware)
left_sqr1_points_cz_unaware_means <- colMeans(left_sqr1_points_cz_unaware)
left_sqr2_points_cz_unaware_means <- colMeans(left_sqr2_points_cz_unaware)
left_rand1_points_cz_aware_means <- colMeans(left_rand1_points_cz_aware)
left_rand2_points_cz_aware_means <- colMeans(left_rand2_points_cz_aware)
left_rand1_points_cz_unaware_means <- colMeans(left_rand1_points_cz_unaware)
left_rand2_points_cz_unaware_means <- colMeans(left_rand2_points_cz_unaware)

#------------------------Plot Left Nd2 Grand Averages------------------------------
# Configuration comparison
labels <- levels(factor(rep_data_cz_long2$configuration))
col_headers <- seq(-100, 596, by = 4)
# Session 1
plot(col_headers, left_sqr1_points_cz_aware_means, type = "l", 
     xlab = "Time points", ylab = "mean amplitude", col = "red", 
     ylim = c(-3.0, 1.0), main = "Nd2 left aware square Grand av.")
lines(col_headers, left_rand1_points_cz_aware_means, type = "l", 
      col = "blue")
abline(h = 0, col = "black", lty = 2)
legend('topright', legend = labels, col = c("red", "blue"), lwd=c(2,2))

# Session comparison
labels <- c("ses 1", "ses 2")
col_headers <- seq(-100, 596, by = 4)
# Aware group
plot(col_headers, left_sqr1_points_cz_aware_means, type = "l", 
     xlab = "Time points", ylab = "mean amplitude", col = "red", 
     ylim = c(-3.0, 1.0), main = "Nd2 left aware square Grand av.")
lines(col_headers, left_sqr2_points_cz_aware_means, type = "l", 
      col = "blue")
abline(h = 0, col = "black", lty = 2)
legend('topright', legend = labels, col = c("red", "blue"), lwd=c(2,2))
# Unaware group
plot(col_headers, left_sqr1_points_cz_unaware_means, type = "l", 
     xlab = "Time points", ylab = "mean amplitude", col = "red", 
     ylim = c(-3.0, 1.0), main = "Nd2 left unaware square Grand av.")
lines(col_headers, left_sqr2_points_cz_unaware_means, type = "l", 
      col = "blue")
abline(h = 0, col = "black", lty = 2)
legend('topright', legend = labels, col = c("red", "blue"), lwd=c(2,2))

# Group comparison
labels <- c("aware", "unaware")
col_headers <- seq(-100, 596, by = 4)
# Session 1
plot(col_headers, left_sqr1_points_cz_aware_means, type = "l", 
     xlab = "Time points", ylab = "mean amplitude", col = "red", 
     ylim = c(-3.0, 1.0), main = "Nd2 left aware square Grand av.")
lines(col_headers, left_sqr1_points_cz_unaware_means, type = "l", 
      col = "blue")
legend('topright', legend = labels, col = c("red", "blue"), lwd=c(2,2))