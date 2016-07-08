# Import file names in working directory
sqr1_fnames <- list.files(getwd(), pattern = "1_ROI Evaluator Sqr_sqr")
sqr2_fnames <- list.files(getwd(), pattern = "2_ROI Evaluator Sqr_sqr")
rand1_fnames <- list.files(getwd(), pattern = "1_ROI Evaluator Rand_rand")
rand2_fnames <- list.files(getwd(), pattern = "2_ROI Evaluator Rand_rand")
# Concatenate all lists in a single list
list_fnames <- c(sqr1_fnames, sqr2_fnames, rand1_fnames, rand2_fnames)

#--------------Extract square right values for each session/group------------
# awareness <- rep_data2$group
awareness <- rep_data3$group
# Sqr , session 1
right_sqr_ses1_points <- function(sqr1_file) {
  sqr1_data <- read.delim(sqr1_file, sep = " ", dec = ",",
                          header = TRUE)
  # Remove last two columns
  sqr1_data$Right <- NULL
  sqr1_data$Occ.par.1 <- NULL
  # Rename last column
  names(sqr1_data)[names(sqr1_data) == "Occ.par"] <- "Right"
  # Remove left & right
  sqr1_data$Occ <- NULL
  sqr1_data$Left <- NULL
  # Coerces table to data frame
  sqr1_right_points <- sqr1_data$Right
}
# Sqr, session 2
right_sqr_ses2_points <- function(sqr2_file) {
  sqr2_data <- read.delim(sqr2_file, sep = " ", dec = ",",
                          header = TRUE)
  # Remove last two columns
  sqr2_data$Right <- NULL
  sqr2_data$Occ.par.1 <- NULL
  # Rename last column
  names(sqr2_data)[names(sqr2_data) == "Occ.par"] <- "Right"
  # Remove left & right
  sqr2_data$Occ <- NULL
  sqr2_data$Left <- NULL
  # Coerces table to data frame
  sqr2_right_points <- sqr2_data$Right
}
# Rand, session 1
right_rand_ses1_points <- function(rand1_file) {
  rand1_data <- read.delim(rand1_file, sep = " ", dec = ",",
                           header = TRUE)
  # Remove last two columns
  rand1_data$Right <- NULL
  rand1_data$Occ.par.1 <- NULL
  # Rename last column
  names(rand1_data)[names(rand1_data) == "Occ.par"] <- "Right"
  # Remove left & right
  rand1_data$Occ <- NULL
  rand1_data$Left <- NULL
  # Coerces table to data frame
  rand1_right_points <- rand1_data$Right
}
# Rand, session 2
right_rand_ses2_points <- function(rand2_file) {
  rand2_data <- read.delim(rand2_file, sep = " ", dec = ",",
                           header = TRUE)
  # Remove last two columns
  rand2_data$Right <- NULL
  rand2_data$Occ.par.1 <- NULL
  # Rename last column
  names(rand2_data)[names(rand2_data) == "Occ.par"] <- "Right"
  # Remove left & right
  rand2_data$Occ <- NULL
  rand2_data$Left <- NULL
  # Coerces table to data frame
  rand2_right_points <- rand2_data$Right
}

# Extract right time point values for each file
right_sqr1_points <- lapply(sqr1_fnames, right_sqr_ses1_points)
right_sqr2_points <- lapply(sqr2_fnames, right_sqr_ses2_points)
right_rand1_points <- lapply(rand1_fnames, right_rand_ses1_points)
right_rand2_points <- lapply(rand2_fnames, right_rand_ses2_points)

# Coerce lists to data frames
right_sqr1_points_dat <- data.frame(matrix(unlist(right_sqr1_points), 
                                          nrow = 34, byrow = T))
right_sqr2_points_dat <- data.frame(matrix(unlist(right_sqr2_points), 
                                          nrow = 34, byrow = T))
right_rand1_points_dat <- data.frame(matrix(unlist(right_rand1_points), 
                                      nrow = 34, byrow = T))
right_rand2_points_dat <- data.frame(matrix(unlist(right_rand2_points), 
                                      nrow = 34, byrow = T))

# Remove subs 27 & 28
right_sqr1_points_dat <- right_sqr1_points_dat[-c(25, 26),]
right_sqr2_points_dat <- right_sqr2_points_dat[-c(25, 26),]
right_rand1_points_dat <- right_rand1_points_dat[-c(25, 26),]
right_rand2_points_dat <- right_rand2_points_dat[-c(25, 26),]

# Add group column
right_sqr1_points_dat <- cbind(right_sqr1_points_dat, awareness)
right_sqr2_points_dat <- cbind(right_sqr2_points_dat, awareness)
right_rand1_points_dat <- cbind(right_rand1_points_dat, awareness)
right_rand2_points_dat <- cbind(right_rand2_points_dat, awareness)

# Change columns names
col_headers <- c(seq(-100, 596, by = 4), "group")
colnames(right_sqr1_points_dat) <- col_headers
colnames(right_sqr2_points_dat) <- col_headers
colnames(right_rand1_points_dat) <- col_headers
colnames(right_rand2_points_dat) <- col_headers

# Subset for means
right_sqr1_points_aware <- subset(right_sqr1_points_dat, group == "aware")
right_sqr2_points_aware <- subset(right_sqr2_points_dat, group == "aware")
right_sqr1_points_unaware <- subset(right_sqr1_points_dat, group == "unaware")
right_sqr2_points_unaware <- subset(right_sqr2_points_dat, group == "unaware")
right_rand1_points_aware <- subset(right_rand1_points_dat, group == "aware")
right_rand2_points_aware <- subset(right_rand2_points_dat, group == "aware")
right_rand1_points_unaware <- subset(right_rand1_points_dat, group == "unaware")
right_rand2_points_unaware <- subset(right_rand2_points_dat, group == "unaware")

# Remove group column
right_sqr1_points_aware$group <- NULL
right_sqr2_points_aware$group <- NULL
right_sqr1_points_unaware$group <- NULL
right_sqr2_points_unaware$group <- NULL
right_rand1_points_aware$group <- NULL
right_rand2_points_aware$group <- NULL
right_rand1_points_unaware$group <- NULL
right_rand2_points_unaware$group <- NULL

# Compute means for all columns
right_sqr1_points_aware_means <- colMeans(right_sqr1_points_aware)
right_sqr2_points_aware_means <- colMeans(right_sqr2_points_aware)
right_sqr1_points_unaware_means <- colMeans(right_sqr1_points_unaware)
right_sqr2_points_unaware_means <- colMeans(right_sqr2_points_unaware)
right_rand1_points_aware_means <- colMeans(right_rand1_points_aware)
right_rand2_points_aware_means <- colMeans(right_rand2_points_aware)
right_rand1_points_unaware_means <- colMeans(right_rand1_points_unaware)
right_rand2_points_unaware_means <- colMeans(right_rand2_points_unaware)

#------------------------Plot Right Nd2 Grand Averages------------------------------
labels <- levels(factor(rep_data_long2$configuration))
col_headers <- seq(-100, 596, by = 4)
# 1.1. Aware - session 1
pdf("Nd2 right aware - sqr x rand.pdf")
par(mfrow = c(2,1))
plot(col_headers, right_sqr1_points_aware_means, type = "l", xlab = "Time points", 
     ylab = "mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd2 right session 1 aware - sqr x rand")
lines(col_headers, right_rand1_points_aware_means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
# 1.2. Unaware - session 2
plot(col_headers, right_sqr2_points_aware_means, type = "l", xlab = "Time points", 
     ylab = "mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd2 right session 2 aware - sqr x rand")
lines(col_headers, right_rand2_points_aware_means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
dev.off()

labels <- levels(factor(rep_data_long2$configuration))
col_headers <- seq(-100, 596, by = 4)
# 1.3. Unaware - session 1
pdf("Nd2 right unaware - sqr x rand.pdf")
par(mfrow = c(2,1))
plot(col_headers, right_sqr1_points_unaware_means, type = "l", xlab = "Time points", 
     ylab = "mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd2 right session 1 unaware - sqr x rand")
lines(col_headers, right_rand1_points_unaware_means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
# 1.2. Unaware - session 2
plot(col_headers, right_sqr2_points_unaware_means, type = "l", xlab = "Time points", 
     ylab = "mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd2 right session 2 unaware - sqr x rand")
lines(col_headers, right_rand2_points_unaware_means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
dev.off()

# 2. Session comparison
labels <- c("ses 1", "ses 2")
col_headers <- seq(-100, 596, by = 4)
# 1.1. Square - aware
plot(col_headers, right_sqr1_points_aware_means, type = "l", 
     xlab = "Time points", ylab = "mean amplitude", col = "red", 
     ylim = c(-2.5, 1.0), main = "Nd2 right aware square Grand av.")
lines(col_headers, right_sqr2_points_aware_means, type = "l", col = "blue")
legend('topright', legend = labels, col = c("red", "blue"), lwd=c(2,2))
# 1.2 Square - unaware
plot(col_headers, right_sqr1_points_unaware_means, type = "l", 
     xlab = "Time points", ylab = "mean amplitude", col = "deeppink", 
     ylim = c(-2.5, 1.0), main = "Nd2 right unaware square Grand av.")
lines(col_headers, right_sqr2_points_unaware_means, type = "l", col = "cyan3")
legend('topright', legend = labels, col = c("deeppink", "cyan3"), lwd=c(2,2))

# 2. Group comparison
labels <- c("aware", "unaware")
col_headers <- seq(-100, 596, by = 4)
# 2.1 Square - session 1
pdf("Nd2 right square - aware x unaware.pdf")
par(mfrow = c(2,1))
plot(col_headers, right_sqr1_points_aware_means, type = "l", 
     xlab = "Time points", ylab = "mean amplitude", col = "red", 
     ylim = c(-2.5, 1.0), main = "Nd2 right session 1 square - aware x unaware")
lines(col_headers, right_sqr1_points_unaware_means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("red", "blue"), lwd=c(2,2))
# dev.off()
# 2.2. Square - session 2
plot(col_headers, right_sqr2_points_aware_means, type = "l", 
     xlab = "Time points", ylab = "mean amplitude", col = "red", 
     ylim = c(-2.5, 1.0), main = "Nd2 right session 2 square - aware x unaware")
lines(col_headers, right_sqr2_points_unaware_means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("red", "blue"), lwd=c(2,2))
dev.off()

# 2.3. Random - session 1
pdf("Nd2 right random - aware x unaware.pdf")
par(mfrow = c(2,1))
plot(col_headers, right_rand1_points_aware_means, type = "l", 
     xlab = "Time points", ylab = "mean amplitude", col = "red", 
     ylim = c(-2.5, 1.0), main = "Nd2 right session 1 random - aware x unaware")
lines(col_headers, right_rand1_points_unaware_means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("red", "blue"), lwd=c(2,2))
# dev.off()
# 2.2. Random - session 2
plot(col_headers, right_rand2_points_aware_means, type = "l", 
     xlab = "Time points", ylab = "mean amplitude", col = "red", 
     ylim = c(-2.5, 1.0), main = "Nd2 right session 2 random - aware x unaware")
lines(col_headers, right_rand2_points_unaware_means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("red", "blue"), lwd=c(2,2))
dev.off()