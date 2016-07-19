
# Data import packages
library(xlsx)
# Data manipulation packages
library(outliers)
library(dplyr)
# Plotting packages
library(lattice)
library(ggplot2)
library(gridExtra)
library(GGally)
# Analysis packages
library(polycor)
library(nlme)
library(gmodels)
library(lme4)
library(ggm)
library(pastecs)
library(car)
library(effsize)
# Psychophysics packages
library(quickpsy)
library(MPDiR)
library(psyphy)
# Data report packages
library(knitr)

# 1. Sessions 1 & 2
# 1.1. Import file names in working directory
sqr1_fnames <- list.files('./Data/Data_BVA', pattern = "1_All ROIs Sqr")
sqr2_fnames <- list.files('./Data/Data_BVA', pattern = "2_All ROIs Sqr")
rand1_fnames <- list.files('./Data/Data_BVA', pattern = "1_All ROIs Rand")
rand2_fnames <- list.files('./Data/Data_BVA', pattern = "2_All ROIs Rand")
# 1.2. Concatenate all lists in a single list
list_fnames <- c(sqr1_fnames, sqr2_fnames, rand1_fnames, rand2_fnames)

# 2. Session 3
# 2.1. Import file names in working directory
sqr3_fnames_P3 <- list.files('./Data/Data_BVA', pattern = "3_All ROIs and P3 Sqr")
rand3_fnames_P3 <- list.files('./Data/Data_BVA', pattern = "3_All ROIs and P3 Rand")
# 2.2. Concatenate all lists in a single list
list_fnames_P3 <- c(sqr3_fnames_P3, rand3_fnames_P3)

defaults <- par()
#-----------------Extract occ values for each session/config-------------

# Function to extract data points - sessions 1 & 2
eeg.points <- function(eeg.file) {
  eeg.data <- read.delim(paste('./Data/Data_BVA/', eeg.file, sep = ""), sep = " ", 
                          dec = ",", header = TRUE)
  C1 <- eeg.data$C1
  P1 <- eeg.data$P1
  N1 <- eeg.data$N1
  Occ <- eeg.data$Occ
  Left <- eeg.data$Left
  Right <- eeg.data$Right
  Right_Left <- eeg.data$Right_Left
  N2 <- eeg.data$N2
  LP <- eeg.data$LP
  eeg.timedomain <- list(P1, N1, Occ, Left, Right, Right_Left, N2, LP)
}

eeg.C1.points <- function(eeg.file) {
  eeg.data <- read.delim(paste('./Data/Data_BVA/', eeg.file, sep = ""), sep = " ", 
                         dec = ",", header = TRUE)
  C1 <- eeg.data$C1
}

eeg.P1.points <- function(eeg.file) {
  eeg.data <- read.delim(paste('./Data/Data_BVA/', eeg.file, sep = ""), sep = " ", 
                         dec = ",", header = TRUE)
  P1 <- eeg.data$P1
}

eeg.N1.points <- function(eeg.file) {
  eeg.data <- read.delim(paste('./Data/Data_BVA/', eeg.file, sep = ""), sep = " ", 
                         dec = ",", header = TRUE)
  N1 <- eeg.data$N1
}

eeg.Occ.points <- function(eeg.file) {
  eeg.data <- read.delim(paste('./Data/Data_BVA/', eeg.file, sep = ""), sep = " ", 
                         dec = ",", header = TRUE)
  Occ <- eeg.data$Occ
}

eeg.Left.points <- function(eeg.file) {
  eeg.data <- read.delim(paste('./Data/Data_BVA/', eeg.file, sep = ""), sep = " ", 
                         dec = ",", header = TRUE)
  Left <- eeg.data$Left
}

eeg.Right.points <- function(eeg.file) {
  eeg.data <- read.delim(paste('./Data/Data_BVA/', eeg.file, sep = ""), sep = " ", 
                         dec = ",", header = TRUE)
  Right <- eeg.data$Right
}

eeg.Right_Left.points <- function(eeg.file) {
  eeg.data <- read.delim(paste('./Data/Data_BVA/', eeg.file, sep = ""), sep = " ", 
                         dec = ",", header = TRUE)
  Right_Left <- eeg.data$Right_Left
}

eeg.N2.points <- function(eeg.file) {
  eeg.data <- read.delim(paste('./Data/Data_BVA/', eeg.file, sep = ""), sep = " ", 
                         dec = ",", header = TRUE)
  N2 <- eeg.data$N2
}

eeg.LP.points <- function(eeg.file) {
  eeg.data <- read.delim(paste('./Data/Data_BVA/', eeg.file, sep = ""), sep = " ", 
                         dec = ",", header = TRUE)
  LP <- eeg.data$LP
}

# # Sqr, session 1
# sqr_ses1_points <- function(sqr1_file) {
#   sqr1_data <- read.delim(sqr1_file, sep = " ", dec = ",",
#                           header = TRUE)
#   # Remove last two columns
#   sqr1_data$Right <- NULL
#   sqr1_data$Occ.par.1 <- NULL
#   # Rename last column
#   names(sqr1_data)[names(sqr1_data) == "Occ.par"] <- "Right"
#   # Remove left & right
#   sqr1_data$Left <- NULL
#   sqr1_data$Right <- NULL
#   # Coerces table to data frame
#   sqr1_occ_points <- sqr1_data$Occ
# }
# # Sqr, session 2
# sqr_ses2_points <- function(sqr2_file) {
#   sqr2_data <- read.delim(sqr2_file, sep = " ", dec = ",",
#                           header = TRUE)
#   # Remove last two columns
#   sqr2_data$Right <- NULL
#   sqr2_data$Occ.par.1 <- NULL
#   # Rename last column
#   names(sqr2_data)[names(sqr2_data) == "Occ.par"] <- "Right"
#   # Remove left & right
#   sqr2_data$Left <- NULL
#   sqr2_data$Right <- NULL
#   # Coerces table to data frame
#   sqr2_occ_points <- sqr2_data$Occ
# }
# # Rand, session 1
# rand_ses1_points <- function(rand1_file) {
#   rand1_data <- read.delim(rand1_file, sep = " ", dec = ",",
#                            header = TRUE)
#   # Remove last two columns
#   rand1_data$Right <- NULL
#   rand1_data$Occ.par.1 <- NULL
#   # Rename last column
#   names(rand1_data)[names(rand1_data) == "Occ.par"] <- "Right"
#   # Remove left & right
#   rand1_data$Left <- NULL
#   rand1_data$Right <- NULL
#   # Coerces table to data frame
#   rand1_occ_points <- rand1_data$Occ
# }
# # Rand, session 2
# rand_ses2_points <- function(rand2_file) {
#   rand2_data <- read.delim(rand2_file, sep = " ", dec = ",",
#                            header = TRUE)
#   # Remove last two columns
#   rand2_data$Right <- NULL
#   rand2_data$Occ.par.1 <- NULL
#   # Rename last column
#   names(rand2_data)[names(rand2_data) == "Occ.par"] <- "Right"
#   # Remove left & right
#   rand2_data$Left <- NULL
#   rand2_data$Right <- NULL
#   # Coerces table to data frame
#   rand2_occ_points <- rand2_data$Occ
# }


# Extract occ time point values for each file
sqr1.points.C1 <- lapply(sqr1_fnames, eeg.C1.points)
sqr1.points.P1 <- lapply(sqr1_fnames, eeg.P1.points)
sqr1.points.N1 <- lapply(sqr1_fnames, eeg.N1.points)
sqr1.points.Occ <- lapply(sqr1_fnames, eeg.Occ.points)
sqr1.points.Left <- lapply(sqr1_fnames, eeg.Left.points)
sqr1.points.Right <- lapply(sqr1_fnames, eeg.Right.points)
sqr1.points.Right_Left <- lapply(sqr1_fnames, eeg.Right_Left.points)
sqr1.points.N2 <- lapply(sqr1_fnames, eeg.N2.points)
sqr1.points.LP <- lapply(sqr1_fnames, eeg.LP.points)
sqr2.points.C1 <- lapply(sqr2_fnames, eeg.C1.points)
sqr2.points.P1 <- lapply(sqr2_fnames, eeg.P1.points)
sqr2.points.N1 <- lapply(sqr2_fnames, eeg.N1.points)
sqr2.points.Occ <- lapply(sqr2_fnames, eeg.Occ.points)
sqr2.points.Left <- lapply(sqr2_fnames, eeg.Left.points)
sqr2.points.Right <- lapply(sqr2_fnames, eeg.Right.points)
sqr2.points.Right_Left <- lapply(sqr2_fnames, eeg.Right_Left.points)
sqr2.points.N2 <- lapply(sqr2_fnames, eeg.N2.points)
sqr2.points.LP <- lapply(sqr2_fnames, eeg.LP.points)
rand1.points.C1 <- lapply(rand1_fnames, eeg.C1.points)
rand1.points.P1 <- lapply(rand1_fnames, eeg.P1.points)
rand1.points.N1 <- lapply(rand1_fnames, eeg.N1.points)
rand1.points.Occ <- lapply(rand1_fnames, eeg.Occ.points)
rand1.points.Left <- lapply(rand1_fnames, eeg.Left.points)
rand1.points.Right <- lapply(rand1_fnames, eeg.Right.points)
rand1.points.Right_Left <- lapply(rand1_fnames, eeg.Right_Left.points)
rand1.points.N2 <- lapply(rand1_fnames, eeg.N2.points)
rand1.points.LP <- lapply(rand1_fnames, eeg.LP.points)
rand2.points.C1 <- lapply(rand2_fnames, eeg.C1.points)
rand2.points.P1 <- lapply(rand2_fnames, eeg.P1.points)
rand2.points.N1 <- lapply(rand2_fnames, eeg.N1.points)
rand2.points.Occ <- lapply(rand2_fnames, eeg.Occ.points)
rand2.points.Left <- lapply(rand2_fnames, eeg.Left.points)
rand2.points.Right <- lapply(rand2_fnames, eeg.Right.points)
rand2.points.Right_Left <- lapply(rand2_fnames, eeg.Right_Left.points)
rand2.points.N2 <- lapply(rand2_fnames, eeg.N2.points)
rand2.points.LP <- lapply(rand2_fnames, eeg.LP.points)


# Coerce lists to data frames
sqr1.points.C1.dat <- data.frame(matrix(unlist(sqr1.points.C1), 
                                     nrow = length(sqr1.points.C1), byrow = T))
sqr1.points.P1.dat <- data.frame(matrix(unlist(sqr1.points.P1), 
                                        nrow = length(sqr1.points.P1), byrow = T))
sqr1.points.N1.dat <- data.frame(matrix(unlist(sqr1.points.N1), 
                                        nrow = length(sqr1.points.N1), byrow = T))
sqr1.points.Occ.dat <- data.frame(matrix(unlist(sqr1.points.Occ), 
                                        nrow = length(sqr1.points.Occ), byrow = T))
sqr1.points.Left.dat <- data.frame(matrix(unlist(sqr1.points.Left), 
                                        nrow = length(sqr1.points.Left), byrow = T))
sqr1.points.Right.dat <- data.frame(matrix(unlist(sqr1.points.Right), 
                                        nrow = length(sqr1.points.Right), byrow = T))
sqr1.points.Right_Left.dat <- data.frame(matrix(unlist(sqr1.points.Right_Left), 
                                        nrow = length(sqr1.points.Right_Left), 
                                        byrow = T))
sqr1.points.N2.dat <- data.frame(matrix(unlist(sqr1.points.N2), 
                                        nrow = length(sqr1.points.N2), byrow = T))
sqr1.points.LP.dat <- data.frame(matrix(unlist(sqr1.points.LP), 
                                        nrow = length(sqr1.points.LP), byrow = T))
sqr2.points.C1.dat <- data.frame(matrix(unlist(sqr2.points.C1), 
                                        nrow = length(sqr2.points.C1), byrow = T))
sqr2.points.P1.dat <- data.frame(matrix(unlist(sqr2.points.P1), 
                                        nrow = length(sqr2.points.P1), byrow = T))
sqr2.points.N1.dat <- data.frame(matrix(unlist(sqr2.points.N1), 
                                        nrow = length(sqr2.points.N1), byrow = T))
sqr2.points.Occ.dat <- data.frame(matrix(unlist(sqr2.points.Occ), 
                                         nrow = length(sqr2.points.Occ), byrow = T))
sqr2.points.Left.dat <- data.frame(matrix(unlist(sqr2.points.Left), 
                                          nrow = length(sqr2.points.Left), byrow = T))
sqr2.points.Right.dat <- data.frame(matrix(unlist(sqr2.points.Right), 
                                           nrow = length(sqr2.points.Right), byrow = T))
sqr2.points.Right_Left.dat <- data.frame(matrix(unlist(sqr2.points.Right_Left), 
                                                nrow = length(sqr2.points.Right_Left), 
                                                byrow = T))
sqr2.points.N2.dat <- data.frame(matrix(unlist(sqr2.points.N2), 
                                        nrow = length(sqr2.points.N2), byrow = T))
sqr2.points.LP.dat <- data.frame(matrix(unlist(sqr2.points.LP), 
                                        nrow = length(sqr2.points.LP), byrow = T))
rand1.points.C1.dat <- data.frame(matrix(unlist(rand1.points.C1), 
                                        nrow = length(rand1.points.C1), byrow = T))
rand1.points.P1.dat <- data.frame(matrix(unlist(rand1.points.P1), 
                                        nrow = length(rand1.points.P1), byrow = T))
rand1.points.N1.dat <- data.frame(matrix(unlist(rand1.points.N1), 
                                        nrow = length(rand1.points.N1), byrow = T))
rand1.points.Occ.dat <- data.frame(matrix(unlist(rand1.points.Occ), 
                                         nrow = length(rand1.points.Occ), byrow = T))
rand1.points.Left.dat <- data.frame(matrix(unlist(rand1.points.Left), 
                                          nrow = length(rand1.points.Left), byrow = T))
rand1.points.Right.dat <- data.frame(matrix(unlist(rand1.points.Right), 
                                           nrow = length(rand1.points.Right), byrow = T))
rand1.points.Right_Left.dat <- data.frame(matrix(unlist(rand1.points.Right_Left), 
                                                nrow = length(rand1.points.Right_Left), 
                                                byrow = T))
rand1.points.N2.dat <- data.frame(matrix(unlist(rand1.points.N2), 
                                        nrow = length(rand1.points.N2), byrow = T))
rand1.points.LP.dat <- data.frame(matrix(unlist(rand1.points.LP), 
                                        nrow = length(rand1.points.LP), byrow = T))
rand2.points.C1.dat <- data.frame(matrix(unlist(rand2.points.C1), 
                                        nrow = length(rand2.points.C1), byrow = T))
rand2.points.P1.dat <- data.frame(matrix(unlist(rand2.points.P1), 
                                        nrow = length(rand2.points.P1), byrow = T))
rand2.points.N1.dat <- data.frame(matrix(unlist(rand2.points.N1), 
                                        nrow = length(rand2.points.N1), byrow = T))
rand2.points.Occ.dat <- data.frame(matrix(unlist(rand2.points.Occ), 
                                         nrow = length(rand2.points.Occ), byrow = T))
rand2.points.Left.dat <- data.frame(matrix(unlist(rand2.points.Left), 
                                          nrow = length(rand2.points.Left), byrow = T))
rand2.points.Right.dat <- data.frame(matrix(unlist(rand2.points.Right), 
                                           nrow = length(rand2.points.Right), byrow = T))
rand2.points.Right_Left.dat <- data.frame(matrix(unlist(rand2.points.Right_Left), 
                                                nrow = length(rand2.points.Right_Left), 
                                                byrow = T))
rand2.points.N2.dat <- data.frame(matrix(unlist(rand2.points.N2), 
                                        nrow = length(rand2.points.N2), byrow = T))
rand2.points.LP.dat <- data.frame(matrix(unlist(rand2.points.LP), 
                                        nrow = length(rand2.points.LP), byrow = T))


group.original <- rep_data2$group.original
group <- rep_data2$group

# Add group column
sqr1.points.C1.dat <- cbind(sqr1.points.C1.dat, group.original, group)
sqr1.points.P1.dat <- cbind(sqr1.points.P1.dat, group.original, group)
sqr1.points.N1.dat <- cbind(sqr1.points.N1.dat, group.original, group)
sqr1.points.Occ.dat <- cbind(sqr1.points.Occ.dat, group.original, group)
sqr1.points.Left.dat <- cbind(sqr1.points.Left.dat, group.original, group)
sqr1.points.Right.dat <- cbind(sqr1.points.Right.dat, group.original, group)
sqr1.points.Right_Left.dat <- cbind(sqr1.points.Right_Left.dat, group.original, group)
sqr1.points.N2.dat <- cbind(sqr1.points.N2.dat, group.original, group)
sqr1.points.LP.dat <- cbind(sqr1.points.LP.dat, group.original, group)
sqr2.points.C1.dat <- cbind(sqr2.points.C1.dat, group.original, group)
sqr2.points.P1.dat <- cbind(sqr2.points.P1.dat, group.original, group)
sqr2.points.N1.dat <- cbind(sqr2.points.N1.dat, group.original, group)
sqr2.points.Occ.dat <- cbind(sqr2.points.Occ.dat, group.original, group)
sqr2.points.Left.dat <- cbind(sqr2.points.Left.dat, group.original, group)
sqr2.points.Right.dat <- cbind(sqr2.points.Right.dat, group.original, group)
sqr2.points.Right_Left.dat <- cbind(sqr2.points.Right_Left.dat, group.original, group)
sqr2.points.N2.dat <- cbind(sqr2.points.N2.dat, group.original, group)
sqr2.points.LP.dat <- cbind(sqr2.points.LP.dat, group.original, group)
rand1.points.C1.dat <- cbind(rand1.points.C1.dat, group.original, group)
rand1.points.P1.dat <- cbind(rand1.points.P1.dat, group.original, group)
rand1.points.N1.dat <- cbind(rand1.points.N1.dat, group.original, group)
rand1.points.Occ.dat <- cbind(rand1.points.Occ.dat, group.original, group)
rand1.points.Left.dat <- cbind(rand1.points.Left.dat, group.original, group)
rand1.points.Right.dat <- cbind(rand1.points.Right.dat, group.original, group)
rand1.points.Right_Left.dat <- cbind(rand1.points.Right_Left.dat, group.original, group)
rand1.points.N2.dat <- cbind(rand1.points.N2.dat, group.original, group)
rand1.points.LP.dat <- cbind(rand1.points.LP.dat, group.original, group)
rand2.points.C1.dat <- cbind(rand2.points.C1.dat, group.original, group)
rand2.points.P1.dat <- cbind(rand2.points.P1.dat, group.original, group)
rand2.points.N1.dat <- cbind(rand2.points.N1.dat, group.original, group)
rand2.points.Occ.dat <- cbind(rand2.points.Occ.dat, group.original, group)
rand2.points.Left.dat <- cbind(rand2.points.Left.dat, group.original, group)
rand2.points.Right.dat <- cbind(rand2.points.Right.dat, group.original, group)
rand2.points.Right_Left.dat <- cbind(rand2.points.Right_Left.dat, group.original, group)
rand2.points.N2.dat <- cbind(rand2.points.N2.dat, group.original, group)
rand2.points.LP.dat <- cbind(rand2.points.LP.dat, group.original, group)

# Change columns names
col_headers <- c(seq(-100, 596, by = 4), "group,original", "group")

colnames(sqr1.points.C1.dat) <- col_headers
colnames(sqr1.points.P1.dat) <- col_headers
colnames(sqr1.points.N1.dat) <- col_headers
colnames(sqr1.points.Occ.dat) <- col_headers
colnames(sqr1.points.Left.dat) <- col_headers
colnames(sqr1.points.Right.dat) <- col_headers
colnames(sqr1.points.Right_Left.dat) <- col_headers
colnames(sqr1.points.N2.dat) <- col_headers
colnames(sqr1.points.LP.dat) <- col_headers
colnames(sqr2.points.C1.dat) <- col_headers
colnames(sqr2.points.P1.dat) <- col_headers
colnames(sqr2.points.N1.dat) <- col_headers
colnames(sqr2.points.Occ.dat) <- col_headers
colnames(sqr2.points.Left.dat) <- col_headers
colnames(sqr2.points.Right.dat) <- col_headers
colnames(sqr2.points.Right_Left.dat) <- col_headers
colnames(sqr2.points.N2.dat) <- col_headers
colnames(sqr2.points.LP.dat) <- col_headers
colnames(rand1.points.C1.dat) <- col_headers
colnames(rand1.points.P1.dat) <- col_headers
colnames(rand1.points.N1.dat) <- col_headers
colnames(rand1.points.Occ.dat) <- col_headers
colnames(rand1.points.Left.dat) <- col_headers
colnames(rand1.points.Right.dat) <- col_headers
colnames(rand1.points.Right_Left.dat) <- col_headers
colnames(rand1.points.N2.dat) <- col_headers
colnames(rand1.points.LP.dat) <- col_headers
colnames(rand2.points.C1.dat) <- col_headers
colnames(rand2.points.P1.dat) <- col_headers
colnames(rand2.points.N1.dat) <- col_headers
colnames(rand2.points.Occ.dat) <- col_headers
colnames(rand2.points.Left.dat) <- col_headers
colnames(rand2.points.Right.dat) <- col_headers
colnames(rand2.points.Right_Left.dat) <- col_headers
colnames(rand2.points.N2.dat) <- col_headers
colnames(rand2.points.LP.dat) <- col_headers

# Subset for means
sqr1.points.C1.aware <- subset(sqr1.points.C1.dat, group.original == "aware")
sqr1.points.P1.aware <- subset(sqr1.points.P1.dat, group.original == "aware")
sqr1.points.N1.aware <- subset(sqr1.points.N1.dat, group.original == "aware")
sqr1.points.Occ.aware <- subset(sqr1.points.Occ.dat, group.original == "aware")
sqr1.points.Left.aware <- subset(sqr1.points.Left.dat, group.original == "aware")
sqr1.points.Right.aware <- subset(sqr1.points.Right.dat, group.original == "aware")
sqr1.points.Right_Left.aware <- subset(sqr1.points.Right_Left.dat, 
                                       group.original == "aware")
sqr1.points.N2.aware <- subset(sqr1.points.N2.dat, group.original == "aware")
sqr1.points.LP.aware <- subset(sqr1.points.LP.dat, group.original == "aware")
sqr2.points.C1.aware <- subset(sqr2.points.C1.dat, group.original == "aware")
sqr2.points.P1.aware <- subset(sqr2.points.P1.dat, group.original == "aware")
sqr2.points.N1.aware <- subset(sqr2.points.N1.dat, group.original == "aware")
sqr2.points.Occ.aware <- subset(sqr2.points.Occ.dat, group.original == "aware")
sqr2.points.Left.aware <- subset(sqr2.points.Left.dat, group.original == "aware")
sqr2.points.Right.aware <- subset(sqr2.points.Right.dat, group.original == "aware")
sqr2.points.Right_Left.aware <- subset(sqr2.points.Right_Left.dat, 
                                       group.original == "aware")
sqr2.points.N2.aware <- subset(sqr2.points.N2.dat, group.original == "aware")
sqr2.points.LP.aware <- subset(sqr2.points.LP.dat, group.original == "aware")
rand1.points.C1.aware <- subset(rand1.points.C1.dat, group.original == "aware")
rand1.points.P1.aware <- subset(rand1.points.P1.dat, group.original == "aware")
rand1.points.N1.aware <- subset(rand1.points.N1.dat, group.original == "aware")
rand1.points.Occ.aware <- subset(rand1.points.Occ.dat, group.original == "aware")
rand1.points.Left.aware <- subset(rand1.points.Left.dat, group.original == "aware")
rand1.points.Right.aware <- subset(rand1.points.Right.dat, group.original == "aware")
rand1.points.Right_Left.aware <- subset(rand1.points.Right_Left.dat, 
                                       group.original == "aware")
rand1.points.N2.aware <- subset(rand1.points.N2.dat, group.original == "aware")
rand1.points.LP.aware <- subset(rand1.points.LP.dat, group.original == "aware")
rand2.points.C1.aware <- subset(rand2.points.C1.dat, group.original == "aware")
rand2.points.P1.aware <- subset(rand2.points.P1.dat, group.original == "aware")
rand2.points.N1.aware <- subset(rand2.points.N1.dat, group.original == "aware")
rand2.points.Occ.aware <- subset(rand2.points.Occ.dat, group.original == "aware")
rand2.points.Left.aware <- subset(rand2.points.Left.dat, group.original == "aware")
rand2.points.Right.aware <- subset(rand2.points.Right.dat, group.original == "aware")
rand2.points.Right_Left.aware <- subset(rand2.points.Right_Left.dat, 
                                       group.original == "aware")
rand2.points.N2.aware <- subset(rand2.points.N2.dat, group.original == "aware")
rand2.points.LP.aware <- subset(rand2.points.LP.dat, group.original == "aware")

sqr1.points.C1.unaware <- subset(sqr1.points.C1.dat, group.original == "unaware")
sqr1.points.P1.unaware <- subset(sqr1.points.P1.dat, group.original == "unaware")
sqr1.points.N1.unaware <- subset(sqr1.points.N1.dat, group.original == "unaware")
sqr1.points.Occ.unaware <- subset(sqr1.points.Occ.dat, group.original == "unaware")
sqr1.points.Left.unaware <- subset(sqr1.points.Left.dat, group.original == "unaware")
sqr1.points.Right.unaware <- subset(sqr1.points.Right.dat, group.original == "unaware")
sqr1.points.Right_Left.unaware <- subset(sqr1.points.Right_Left.dat, 
                                       group.original == "unaware")
sqr1.points.N2.unaware <- subset(sqr1.points.N2.dat, group.original == "unaware")
sqr1.points.LP.unaware <- subset(sqr1.points.LP.dat, group.original == "unaware")
sqr2.points.C1.unaware <- subset(sqr2.points.C1.dat, group.original == "unaware")
sqr2.points.P1.unaware <- subset(sqr2.points.P1.dat, group.original == "unaware")
sqr2.points.N1.unaware <- subset(sqr2.points.N1.dat, group.original == "unaware")
sqr2.points.Occ.unaware <- subset(sqr2.points.Occ.dat, group.original == "unaware")
sqr2.points.Left.unaware <- subset(sqr2.points.Left.dat, group.original == "unaware")
sqr2.points.Right.unaware <- subset(sqr2.points.Right.dat, group.original == "unaware")
sqr2.points.Right_Left.unaware <- subset(sqr2.points.Right_Left.dat, 
                                       group.original == "unaware")
sqr2.points.N2.unaware <- subset(sqr2.points.N2.dat, group.original == "unaware")
sqr2.points.LP.unaware <- subset(sqr2.points.LP.dat, group.original == "unaware")
rand1.points.C1.unaware <- subset(rand1.points.C1.dat, group.original == "unaware")
rand1.points.P1.unaware <- subset(rand1.points.P1.dat, group.original == "unaware")
rand1.points.N1.unaware <- subset(rand1.points.N1.dat, group.original == "unaware")
rand1.points.Occ.unaware <- subset(rand1.points.Occ.dat, group.original == "unaware")
rand1.points.Left.unaware <- subset(rand1.points.Left.dat, group.original == "unaware")
rand1.points.Right.unaware <- subset(rand1.points.Right.dat, group.original == "unaware")
rand1.points.Right_Left.unaware <- subset(rand1.points.Right_Left.dat, 
                                        group.original == "unaware")
rand1.points.N2.unaware <- subset(rand1.points.N2.dat, group.original == "unaware")
rand1.points.LP.unaware <- subset(rand1.points.LP.dat, group.original == "unaware")
rand2.points.C1.unaware <- subset(rand2.points.C1.dat, group.original == "unaware")
rand2.points.P1.unaware <- subset(rand2.points.P1.dat, group.original == "unaware")
rand2.points.N1.unaware <- subset(rand2.points.N1.dat, group.original == "unaware")
rand2.points.Occ.unaware <- subset(rand2.points.Occ.dat, group.original == "unaware")
rand2.points.Left.unaware <- subset(rand2.points.Left.dat, group.original == "unaware")
rand2.points.Right.unaware <- subset(rand2.points.Right.dat, group.original == "unaware")
rand2.points.Right_Left.unaware <- subset(rand2.points.Right_Left.dat, 
                                        group.original == "unaware")
rand2.points.N2.unaware <- subset(rand2.points.N2.dat, group.original == "unaware")
rand2.points.LP.unaware <- subset(rand2.points.LP.dat, group.original == "unaware")


# Remove group column
sqr1.points.C1.aware$group <- NULL
sqr1.points.P1.aware$group <- NULL
sqr1.points.N1.aware$group <- NULL
sqr1.points.Occ.aware$group <- NULL
sqr1.points.Left.aware$group <- NULL
sqr1.points.Right.aware$group <- NULL
sqr1.points.Right_Left.aware$group <- NULL
sqr1.points.N2.aware$group <- NULL
sqr1.points.LP.aware$group <- NULL

sqr2.points.C1.aware$group <- NULL
sqr2.points.P1.aware$group <- NULL
sqr2.points.N1.aware$group <- NULL
sqr2.points.Occ.aware$group <- NULL
sqr2.points.Left.aware$group <- NULL
sqr2.points.Right.aware$group <- NULL
sqr2.points.Right_Left.aware$group <- NULL
sqr2.points.N2.aware$group <- NULL
sqr2.points.LP.aware$group <- NULL

rand1.points.C1.aware$group <- NULL
rand1.points.P1.aware$group <- NULL
rand1.points.N1.aware$group <- NULL
rand1.points.Occ.aware$group <- NULL
rand1.points.Left.aware$group <- NULL
rand1.points.Right.aware$group <- NULL
rand1.points.Right_Left.aware$group <- NULL
rand1.points.N2.aware$group <- NULL
rand1.points.LP.aware$group <- NULL

rand2.points.C1.aware$group <- NULL
rand2.points.P1.aware$group <- NULL
rand2.points.N1.aware$group <- NULL
rand2.points.Occ.aware$group <- NULL
rand2.points.Left.aware$group <- NULL
rand2.points.Right.aware$group <- NULL
rand2.points.Right_Left.aware$group <- NULL
rand2.points.N2.aware$group <- NULL
rand2.points.LP.aware$group <- NULL


sqr1.points.C1.aware$group.original <- NULL
sqr1.points.P1.aware$group.original <- NULL
sqr1.points.N1.aware$group.original <- NULL
sqr1.points.Occ.aware$group.original <- NULL
sqr1.points.Left.aware$group.original <- NULL
sqr1.points.Right.aware$group.original <- NULL
sqr1.points.Right_Left.aware$group.original <- NULL
sqr1.points.N2.aware$group.original <- NULL
sqr1.points.LP.aware$group.original <- NULL

sqr2.points.C1.aware$group.original <- NULL
sqr2.points.P1.aware$group.original <- NULL
sqr2.points.N1.aware$group.original <- NULL
sqr2.points.Occ.aware$group.original <- NULL
sqr2.points.Left.aware$group.original <- NULL
sqr2.points.Right.aware$group.original <- NULL
sqr2.points.Right_Left.aware$group.original <- NULL
sqr2.points.N2.aware$group.original <- NULL
sqr2.points.LP.aware$group.original <- NULL

rand1.points.C1.aware$group.original <- NULL
rand1.points.P1.aware$group.original <- NULL
rand1.points.N1.aware$group.original <- NULL
rand1.points.Occ.aware$group.original <- NULL
rand1.points.Left.aware$group.original <- NULL
rand1.points.Right.aware$group.original <- NULL
rand1.points.Right_Left.aware$group.original <- NULL
rand1.points.N2.aware$group.original <- NULL
rand1.points.LP.aware$group.original <- NULL

rand2.points.C1.aware$group.original <- NULL
rand2.points.P1.aware$group.original <- NULL
rand2.points.N1.aware$group.original <- NULL
rand2.points.Occ.aware$group.original <- NULL
rand2.points.Left.aware$group.original <- NULL
rand2.points.Right.aware$group.original <- NULL
rand2.points.Right_Left.aware$group.original <- NULL
rand2.points.N2.aware$group.original <- NULL
rand2.points.LP.aware$group.original <- NULL



sqr1.points.C1.unaware$group <- NULL
sqr1.points.P1.unaware$group <- NULL
sqr1.points.N1.unaware$group <- NULL
sqr1.points.Occ.unaware$group <- NULL
sqr1.points.Left.unaware$group <- NULL
sqr1.points.Right.unaware$group <- NULL
sqr1.points.Right_Left.unaware$group <- NULL
sqr1.points.N2.unaware$group <- NULL
sqr1.points.LP.unaware$group <- NULL

sqr2.points.C1.unaware$group <- NULL
sqr2.points.P1.unaware$group <- NULL
sqr2.points.N1.unaware$group <- NULL
sqr2.points.Occ.unaware$group <- NULL
sqr2.points.Left.unaware$group <- NULL
sqr2.points.Right.unaware$group <- NULL
sqr2.points.Right_Left.unaware$group <- NULL
sqr2.points.N2.unaware$group <- NULL
sqr2.points.LP.unaware$group <- NULL

rand1.points.C1.unaware$group <- NULL
rand1.points.P1.unaware$group <- NULL
rand1.points.N1.unaware$group <- NULL
rand1.points.Occ.unaware$group <- NULL
rand1.points.Left.unaware$group <- NULL
rand1.points.Right.unaware$group <- NULL
rand1.points.Right_Left.unaware$group <- NULL
rand1.points.N2.unaware$group <- NULL
rand1.points.LP.unaware$group <- NULL

rand2.points.C1.unaware$group <- NULL
rand2.points.P1.unaware$group <- NULL
rand2.points.N1.unaware$group <- NULL
rand2.points.Occ.unaware$group <- NULL
rand2.points.Left.unaware$group <- NULL
rand2.points.Right.unaware$group <- NULL
rand2.points.Right_Left.unaware$group <- NULL
rand2.points.N2.unaware$group <- NULL
rand2.points.LP.unaware$group <- NULL


sqr1.points.C1.unaware$group.original <- NULL
sqr1.points.P1.unaware$group.original <- NULL
sqr1.points.N1.unaware$group.original <- NULL
sqr1.points.Occ.unaware$group.original <- NULL
sqr1.points.Left.unaware$group.original <- NULL
sqr1.points.Right.unaware$group.original <- NULL
sqr1.points.Right_Left.unaware$group.original <- NULL
sqr1.points.N2.unaware$group.original <- NULL
sqr1.points.LP.unaware$group.original <- NULL

sqr2.points.C1.unaware$group.original <- NULL
sqr2.points.P1.unaware$group.original <- NULL
sqr2.points.N1.unaware$group.original <- NULL
sqr2.points.Occ.unaware$group.original <- NULL
sqr2.points.Left.unaware$group.original <- NULL
sqr2.points.Right.unaware$group.original <- NULL
sqr2.points.Right_Left.unaware$group.original <- NULL
sqr2.points.N2.unaware$group.original <- NULL
sqr2.points.LP.unaware$group.original <- NULL

rand1.points.C1.unaware$group.original <- NULL
rand1.points.P1.unaware$group.original <- NULL
rand1.points.N1.unaware$group.original <- NULL
rand1.points.Occ.unaware$group.original <- NULL
rand1.points.Left.unaware$group.original <- NULL
rand1.points.Right.unaware$group.original <- NULL
rand1.points.Right_Left.unaware$group.original <- NULL
rand1.points.N2.unaware$group.original <- NULL
rand1.points.LP.unaware$group.original <- NULL

rand2.points.C1.unaware$group.original <- NULL
rand2.points.P1.unaware$group.original <- NULL
rand2.points.N1.unaware$group.original <- NULL
rand2.points.Occ.unaware$group.original <- NULL
rand2.points.Left.unaware$group.original <- NULL
rand2.points.Right.unaware$group.original <- NULL
rand2.points.Right_Left.unaware$group.original <- NULL
rand2.points.N2.unaware$group.original <- NULL
rand2.points.LP.unaware$group.original <- NULL
# # Compute means for all columns
# sqr1_points_means <- colMeans(sqr1_points_dat)
# sqr2_points_means <- colMeans(sqr2_points_dat)
# rand1_points_means <- colMeans(rand1_points_dat)
# rand2_points_means <- colMeans(rand2_points_dat)

sqr1.points.C1.means <- colMeans(sqr1.points.C1.dat)
sqr1.points.P1.means <- colMeans(sqr1.points.P1.dat)
sqr1.points.N1.means <- colMeans(sqr1.points.N1.dat)
sqr1.points.Occ.means <- colMeans(sqr1.points.Occ.dat)
sqr1.points.Left.means <- colMeans(sqr1.points.Left.dat)
sqr1.points.Right.means <- colMeans(sqr1.points.Right.dat)
sqr1.points.Right_Left.means <- colMeans(sqr1.points.Right_Left.dat)
sqr1.points.N2.means <- colMeans(sqr1.points.N2.dat)
sqr1.points.LP.means <- colMeans(sqr1.points.LP.dat)

# Compute means for group subsets
sqr1.points.C1.aware.means <- colMeans(sqr1.points.C1.aware)
sqr1.points.P1.aware.means <- colMeans(sqr1.points.P1.aware)
sqr1.points.N1.aware.means <- colMeans(sqr1.points.N1.aware)
sqr1.points.Occ.aware.means <- colMeans(sqr1.points.Occ.aware)
sqr1.points.Left.aware.means <- colMeans(sqr1.points.Left.aware)
sqr1.points.Right.aware.means <- colMeans(sqr1.points.Right.aware)
sqr1.points.Right_Left.aware.means <- colMeans(sqr1.points.Right_Left.aware)
sqr1.points.N2.aware.means <- colMeans(sqr1.points.N2.aware)
sqr1.points.LP.aware.means <- colMeans(sqr1.points.LP.aware)

sqr2.points.C1.aware.means <- colMeans(sqr2.points.C1.aware)
sqr2.points.P1.aware.means <- colMeans(sqr2.points.P1.aware)
sqr2.points.N1.aware.means <- colMeans(sqr2.points.N1.aware)
sqr2.points.Occ.aware.means <- colMeans(sqr2.points.Occ.aware)
sqr2.points.Left.aware.means <- colMeans(sqr2.points.Left.aware)
sqr2.points.Right.aware.means <- colMeans(sqr2.points.Right.aware)
sqr2.points.Right_Left.aware.means <- colMeans(sqr2.points.Right_Left.aware)
sqr2.points.N2.aware.means <- colMeans(sqr2.points.N2.aware)
sqr2.points.LP.aware.means <- colMeans(sqr2.points.LP.aware)

rand1.points.C1.aware.means <- colMeans(rand1.points.C1.aware)
rand1.points.P1.aware.means <- colMeans(rand1.points.P1.aware)
rand1.points.N1.aware.means <- colMeans(rand1.points.N1.aware)
rand1.points.Occ.aware.means <- colMeans(rand1.points.Occ.aware)
rand1.points.Left.aware.means <- colMeans(rand1.points.Left.aware)
rand1.points.Right.aware.means <- colMeans(rand1.points.Right.aware)
rand1.points.Right_Left.aware.means <- colMeans(rand1.points.Right_Left.aware)
rand1.points.N2.aware.means <- colMeans(rand1.points.N2.aware)
rand1.points.LP.aware.means <- colMeans(rand1.points.LP.aware)

rand2.points.C1.aware.means <- colMeans(rand2.points.C1.aware)
rand2.points.P1.aware.means <- colMeans(rand2.points.P1.aware)
rand2.points.N1.aware.means <- colMeans(rand2.points.N1.aware)
rand2.points.Occ.aware.means <- colMeans(rand2.points.Occ.aware)
rand2.points.Left.aware.means <- colMeans(rand2.points.Left.aware)
rand2.points.Right.aware.means <- colMeans(rand2.points.Right.aware)
rand2.points.Right_Left.aware.means <- colMeans(rand2.points.Right_Left.aware)
rand2.points.N2.aware.means <- colMeans(rand2.points.N2.aware)
rand2.points.LP.aware.means <- colMeans(rand2.points.LP.aware)


sqr1.points.C1.unaware.means <- colMeans(sqr1.points.C1.unaware)
sqr1.points.P1.unaware.means <- colMeans(sqr1.points.P1.unaware)
sqr1.points.N1.unaware.means <- colMeans(sqr1.points.N1.unaware)
sqr1.points.Occ.unaware.means <- colMeans(sqr1.points.Occ.unaware)
sqr1.points.Left.unaware.means <- colMeans(sqr1.points.Left.unaware)
sqr1.points.Right.unaware.means <- colMeans(sqr1.points.Right.unaware)
sqr1.points.Right_Left.unaware.means <- colMeans(sqr1.points.Right_Left.unaware)
sqr1.points.N2.unaware.means <- colMeans(sqr1.points.N2.unaware)
sqr1.points.LP.unaware.means <- colMeans(sqr1.points.LP.unaware)

sqr2.points.C1.unaware.means <- colMeans(sqr2.points.C1.unaware)
sqr2.points.P1.unaware.means <- colMeans(sqr2.points.P1.unaware)
sqr2.points.N1.unaware.means <- colMeans(sqr2.points.N1.unaware)
sqr2.points.Occ.unaware.means <- colMeans(sqr2.points.Occ.unaware)
sqr2.points.Left.unaware.means <- colMeans(sqr2.points.Left.unaware)
sqr2.points.Right.unaware.means <- colMeans(sqr2.points.Right.unaware)
sqr2.points.Right_Left.unaware.means <- colMeans(sqr2.points.Right_Left.unaware)
sqr2.points.N2.unaware.means <- colMeans(sqr2.points.N2.unaware)
sqr2.points.LP.unaware.means <- colMeans(sqr2.points.LP.unaware)

rand1.points.C1.unaware.means <- colMeans(rand1.points.C1.unaware)
rand1.points.P1.unaware.means <- colMeans(rand1.points.P1.unaware)
rand1.points.N1.unaware.means <- colMeans(rand1.points.N1.unaware)
rand1.points.Occ.unaware.means <- colMeans(rand1.points.Occ.unaware)
rand1.points.Left.unaware.means <- colMeans(rand1.points.Left.unaware)
rand1.points.Right.unaware.means <- colMeans(rand1.points.Right.unaware)
rand1.points.Right_Left.unaware.means <- colMeans(rand1.points.Right_Left.unaware)
rand1.points.N2.unaware.means <- colMeans(rand1.points.N2.unaware)
rand1.points.LP.unaware.means <- colMeans(rand1.points.LP.unaware)

rand2.points.C1.unaware.means <- colMeans(rand2.points.C1.unaware)
rand2.points.P1.unaware.means <- colMeans(rand2.points.P1.unaware)
rand2.points.N1.unaware.means <- colMeans(rand2.points.N1.unaware)
rand2.points.Occ.unaware.means <- colMeans(rand2.points.Occ.unaware)
rand2.points.Left.unaware.means <- colMeans(rand2.points.Left.unaware)
rand2.points.Right.unaware.means <- colMeans(rand2.points.Right.unaware)
rand2.points.Right_Left.unaware.means <- colMeans(rand2.points.Right_Left.unaware)
rand2.points.N2.unaware.means <- colMeans(rand2.points.N2.unaware)
rand2.points.LP.unaware.means <- colMeans(rand2.points.LP.unaware)

#------------------------Plot Occ Nd1 Grand Averages------------------------------
labels <- levels(factor(rep_data_long2$configuration))
col_headers <- seq(-100, 596, by = 4)
# 1. Configuration comparison
# 1.1 Both groups - session 1
pdf("C1 - both groups - sqr x rand.pdf")
par(mfrow = c(2,1))
plot(col_headers, sqr1.points.means, type = "l", xlab = "Time points", 
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