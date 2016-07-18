
# Import file names in working directory
sqr1_fnames <- list.files(getwd(), pattern = "1_ROI Evaluator Sqr_sqr")
sqr2_fnames <- list.files(getwd(), pattern = "2_ROI Evaluator Sqr_sqr")
rand1_fnames <- list.files(getwd(), pattern = "1_ROI Evaluator Rand_rand")
rand2_fnames <- list.files(getwd(), pattern = "2_ROI Evaluator Rand_rand")

# Concatenate all lists in a single list
list_fnames <- c(sqr1_fnames, sqr2_fnames, rand1_fnames, rand2_fnames)

#-----------------Extract occ values for each session/config-------------
awareness <- rep_data2$group
# awareness <- rep_data3$group
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
sqr1_points <- lapply(sqr1_fnames, sqr_ses1_points)
sqr2_points <- lapply(sqr2_fnames, sqr_ses2_points)
rand1_points <- lapply(rand1_fnames, rand_ses1_points)
rand2_points <- lapply(rand2_fnames, rand_ses2_points)

# Coerce lists to data frames
sqr1_points_dat <- data.frame(matrix(unlist(sqr1_points), 
                                     nrow = 34, byrow = T))
sqr2_points_dat <- data.frame(matrix(unlist(sqr2_points), 
                                     nrow = 34, byrow = T))
rand1_points_dat <- data.frame(matrix(unlist(rand1_points), 
                                      nrow = 34, byrow = T))
rand2_points_dat <- data.frame(matrix(unlist(rand2_points), 
                                      nrow = 34, byrow = T))

# Remove subs 27 & 28
sqr1_points_dat <- sqr1_points_dat[-c(25, 26),]
sqr2_points_dat <- sqr2_points_dat[-c(25, 26),]
rand1_points_dat <- rand1_points_dat[-c(25, 26),]
rand2_points_dat <- rand2_points_dat[-c(25, 26),]

# Add group column
sqr1_points_dat <- cbind(sqr1_points_dat, awareness)
sqr2_points_dat <- cbind(sqr2_points_dat, awareness)
rand1_points_dat <- cbind(rand1_points_dat, awareness)
rand2_points_dat <- cbind(rand2_points_dat, awareness)

# Change columns names
col_headers <- c(seq(-100, 596, by = 4), "group")
# col_headers <- seq(-100, 596, by = 4)
colnames(sqr1_points_dat) <- col_headers
colnames(sqr2_points_dat) <- col_headers
colnames(rand1_points_dat) <- col_headers
colnames(rand2_points_dat) <- col_headers

# Subset for means
sqr1_points_aware <- subset(sqr1_points_dat, group == "aware")
sqr2_points_aware <- subset(sqr2_points_dat, group == "aware")
sqr1_points_unaware <- subset(sqr1_points_dat, group == "unaware")
sqr2_points_unaware <- subset(sqr2_points_dat, group == "unaware")
rand1_points_aware <- subset(rand1_points_dat, group == "aware")
rand2_points_aware <- subset(rand2_points_dat, group == "aware")
rand1_points_unaware <- subset(rand1_points_dat, group == "unaware")
rand2_points_unaware <- subset(rand2_points_dat, group == "unaware")

# Remove group column
sqr1_points_aware$group <- NULL
sqr2_points_aware$group <- NULL
sqr1_points_unaware$group <- NULL
sqr2_points_unaware$group <- NULL
rand1_points_aware$group <- NULL
rand2_points_aware$group <- NULL
rand1_points_unaware$group <- NULL
rand2_points_unaware$group <- NULL

# # Compute means for all columns
# sqr1_points_means <- colMeans(sqr1_points_dat)
# sqr2_points_means <- colMeans(sqr2_points_dat)
# rand1_points_means <- colMeans(rand1_points_dat)
# rand2_points_means <- colMeans(rand2_points_dat)

# Compute means for group subsets
sqr1_points_aware_means <- colMeans(sqr1_points_aware)
sqr2_points_aware_means <- colMeans(sqr2_points_aware)
sqr1_points_unaware_means <- colMeans(sqr1_points_unaware)
sqr2_points_unaware_means <- colMeans(sqr2_points_unaware)
rand1_points_aware_means <- colMeans(rand1_points_aware)
rand2_points_aware_means <- colMeans(rand2_points_aware)
rand1_points_unaware_means <- colMeans(rand1_points_unaware)
rand2_points_unaware_means <- colMeans(rand2_points_unaware)

#------------------------Plot Occ Nd1 Grand Averages------------------------------
labels <- levels(factor(rep_data_long2$configuration))
col_headers <- seq(-100, 596, by = 4)
# 1. Configuration comparison
# 1.1 Both groups - session 1
pdf("Nd1 occ both groups - sqr x rand.pdf")
par(mfrow = c(2,1))
plot(col_headers, sqr1_points_means, type = "l", xlab = "Time points", 
     ylab = "mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd1 occ session 1 both groups - sqr x rand")
lines(col_headers, rand1_points_means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
# 1.2. Both groups - session 2
plot(col_headers, sqr2_points_means, type = "l", xlab = "Time points", 
     ylab = "mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd1 occ session 2 both groups - sqr x rand")
lines(col_headers, rand2_points_means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
dev.off()

labels <- levels(factor(rep_data_long2$configuration))
col_headers <- seq(-100, 596, by = 4)
# 1.3. Aware - session 1
pdf("Nd1 occ aware - sqr x rand.pdf")
par(mfrow = c(2,1))
plot(col_headers, sqr1_points_aware_means, type = "l", xlab = "Time points", 
     ylab = "mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd1 occ session 1 aware - sqr x rand")
lines(col_headers, rand1_points_aware_means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
# 1.2. Unaware - session 2
plot(col_headers, sqr2_points_aware_means, type = "l", xlab = "Time points", 
     ylab = "mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd1 occ session 2 aware - sqr x rand")
lines(col_headers, rand2_points_aware_means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
dev.off()

labels <- levels(factor(rep_data_long2$configuration))
col_headers <- seq(-100, 596, by = 4)
# 1.3. Unaware - session 1
pdf("Nd1 occ unaware - sqr x rand.pdf")
par(mfrow = c(2,1))
plot(col_headers, sqr1_points_unaware_means, type = "l", xlab = "Time points", 
     ylab = "mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd1 occ session 1 unaware - sqr x rand")
lines(col_headers, rand1_points_unaware_means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
# 1.2. Unaware - session 2
plot(col_headers, sqr2_points_unaware_means, type = "l", xlab = "Time points", 
     ylab = "mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd1 occ session 2 unaware - sqr x rand")
lines(col_headers, rand2_points_unaware_means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
dev.off()

# 2. Group comparison
labels <- c("aware", "unaware")
col_headers <- seq(-100, 596, by = 4)
# 2.1. Square - session 1
pdf("Nd1 occ square - aware x unaware.pdf")
par(mfrow = c(2,1))
plot(col_headers, sqr1_points_aware_means, type = "l", xlab = "Time points", 
     ylab = "mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd1 occ session 1 square - aware x unaware")
lines(col_headers, sqr1_points_unaware_means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("red", "blue"), lwd=c(2,2))
# 2.2. Square - session 2
plot(col_headers, sqr2_points_aware_means, type = "l", xlab = "Time points", 
     ylab = "mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd1 occ session 2 square - aware x unaware")
lines(col_headers, sqr2_points_unaware_means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("red", "blue"), lwd=c(2,2))
dev.off()

# 2.3. Random - session 1
pdf("Nd1 occ random - aware x unaware.pdf")
par(mfrow = c(2,1))
plot(col_headers, rand1_points_aware_means, type = "l", xlab = "Time points", 
     ylab = "mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd1 occ session 1 random - aware x unaware")
lines(col_headers, rand1_points_unaware_means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("red", "blue"), lwd=c(2,2))
# 2.2. Random - session 2
plot(col_headers, rand2_points_aware_means, type = "l", xlab = "Time points", 
     ylab = "mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd1 occ session 2 random - aware x unaware")
lines(col_headers, rand2_points_unaware_means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("red", "blue"), lwd=c(2,2))
dev.off()