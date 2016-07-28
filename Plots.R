
# Data import packages
library(xlsx)
# Data manipulation packages
library(outliers)
library(dplyr)
library(tidyr)
library(reshape2)
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
library(multcomp)
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

# Import file names in working directory
sqr3_fnames_P3 <- list.files('./Data/Data_BVA', pattern = "3_All ROIs and P3 Sqr")
rand3_fnames_P3 <- list.files('./Data/Data_BVA', pattern = "3_All ROIs and P3 Rand")
# Concatenate all lists in a single list
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

eeg.P3.points <- function(eeg.file) {
  eeg.data <- read.delim(paste('./Data/Data_BVA/', eeg.file, sep = ""), sep = " ", 
                         dec = ",", header = TRUE)
  P3 <- eeg.data$P3
}


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

sqr3.points.P3 <- lapply(sqr3_fnames_P3, eeg.P3.points)
rand3.points.P3 <- lapply(rand3_fnames_P3, eeg.P3.points)

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


sqr3.points.P3.dat <- data.frame(matrix(unlist(sqr3.points.P3), 
                                        nrow = length(sqr3.points.P3), byrow = T))
rand3.points.P3.dat <- data.frame(matrix(unlist(rand3.points.P3), 
                                         nrow = length(rand3.points.P3), byrow = T))

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

group.original <- group.original[-1]
group.original <- group.original[-28]
group <- group[-1]
group <- group[-28]

sqr3.points.P3.dat <- cbind(sqr3.points.P3.dat, group.original, group)
rand3.points.P3.dat <- cbind(rand3.points.P3.dat, group.original, group)

# Change columns names
col_headers <- c(seq(-100, 596, by = 4), "group.original", "group")

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

colnames(sqr3.points.P3.dat) <- col_headers
colnames(rand3.points.P3.dat) <- col_headers


# Remove group column
sqr1.points.C1.dat$group <- NULL
sqr1.points.P1.dat$group <- NULL
sqr1.points.N1.dat$group <- NULL
sqr1.points.Occ.dat$group <- NULL
sqr1.points.Left.dat$group <- NULL
sqr1.points.Right.dat$group <- NULL
sqr1.points.Right_Left.dat$group <- NULL
sqr1.points.N2.dat$group <- NULL
sqr1.points.LP.dat$group <- NULL

sqr2.points.C1.dat$group <- NULL
sqr2.points.P1.dat$group <- NULL
sqr2.points.N1.dat$group <- NULL
sqr2.points.Occ.dat$group <- NULL
sqr2.points.Left.dat$group <- NULL
sqr2.points.Right.dat$group <- NULL
sqr2.points.Right_Left.dat$group <- NULL
sqr2.points.N2.dat$group <- NULL
sqr2.points.LP.dat$group <- NULL

rand1.points.C1.dat$group <- NULL
rand1.points.P1.dat$group <- NULL
rand1.points.N1.dat$group <- NULL
rand1.points.Occ.dat$group <- NULL
rand1.points.Left.dat$group <- NULL
rand1.points.Right.dat$group <- NULL
rand1.points.Right_Left.dat$group <- NULL
rand1.points.N2.dat$group <- NULL
rand1.points.LP.dat$group <- NULL

rand2.points.C1.dat$group <- NULL
rand2.points.P1.dat$group <- NULL
rand2.points.N1.dat$group <- NULL
rand2.points.Occ.dat$group <- NULL
rand2.points.Left.dat$group <- NULL
rand2.points.Right.dat$group <- NULL
rand2.points.Right_Left.dat$group <- NULL
rand2.points.N2.dat$group <- NULL
rand2.points.LP.dat$group <- NULL


sqr3.points.P3.dat$group <- NULL
rand3.points.P3.dat$group <- NULL


sqr1.points.C1.dat$group.original <- NULL
sqr1.points.P1.dat$group.original <- NULL
sqr1.points.N1.dat$group.original <- NULL
sqr1.points.Occ.dat$group.original <- NULL
sqr1.points.Left.dat$group.original <- NULL
sqr1.points.Right.dat$group.original <- NULL
sqr1.points.Right_Left.dat$group.original <- NULL
sqr1.points.N2.dat$group.original <- NULL
sqr1.points.LP.dat$group.original <- NULL

sqr2.points.C1.dat$group.original <- NULL
sqr2.points.P1.dat$group.original <- NULL
sqr2.points.N1.dat$group.original <- NULL
sqr2.points.Occ.dat$group.original <- NULL
sqr2.points.Left.dat$group.original <- NULL
sqr2.points.Right.dat$group.original <- NULL
sqr2.points.Right_Left.dat$group.original <- NULL
sqr2.points.N2.dat$group.original <- NULL
sqr2.points.LP.dat$group.original <- NULL

rand1.points.C1.dat$group.original <- NULL
rand1.points.P1.dat$group.original <- NULL
rand1.points.N1.dat$group.original <- NULL
rand1.points.Occ.dat$group.original <- NULL
rand1.points.Left.dat$group.original <- NULL
rand1.points.Right.dat$group.original <- NULL
rand1.points.Right_Left.dat$group.original <- NULL
rand1.points.N2.dat$group.original <- NULL
rand1.points.LP.dat$group.original <- NULL

rand2.points.C1.dat$group.original <- NULL
rand2.points.P1.dat$group.original <- NULL
rand2.points.N1.dat$group.original <- NULL
rand2.points.Occ.dat$group.original <- NULL
rand2.points.Left.dat$group.original <- NULL
rand2.points.Right.dat$group.original <- NULL
rand2.points.Right_Left.dat$group.original <- NULL
rand2.points.N2.dat$group.original <- NULL
rand2.points.LP.dat$group.original <- NULL


sqr3.points.P3.dat$group.original <- NULL
rand3.points.P3.dat$group.original <- NULL



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

sqr2.points.C1.means <- colMeans(sqr2.points.C1.dat)
sqr2.points.P1.means <- colMeans(sqr2.points.P1.dat)
sqr2.points.N1.means <- colMeans(sqr2.points.N1.dat)
sqr2.points.Occ.means <- colMeans(sqr2.points.Occ.dat)
sqr2.points.Left.means <- colMeans(sqr2.points.Left.dat)
sqr2.points.Right.means <- colMeans(sqr2.points.Right.dat)
sqr2.points.Right_Left.means <- colMeans(sqr2.points.Right_Left.dat)
sqr2.points.N2.means <- colMeans(sqr2.points.N2.dat)
sqr2.points.LP.means <- colMeans(sqr2.points.LP.dat)

rand1.points.C1.means <- colMeans(rand1.points.C1.dat)
rand1.points.P1.means <- colMeans(rand1.points.P1.dat)
rand1.points.N1.means <- colMeans(rand1.points.N1.dat)
rand1.points.Occ.means <- colMeans(rand1.points.Occ.dat)
rand1.points.Left.means <- colMeans(rand1.points.Left.dat)
rand1.points.Right.means <- colMeans(rand1.points.Right.dat)
rand1.points.Right_Left.means <- colMeans(rand1.points.Right_Left.dat)
rand1.points.N2.means <- colMeans(rand1.points.N2.dat)
rand1.points.LP.means <- colMeans(rand1.points.LP.dat)

rand2.points.C1.means <- colMeans(rand2.points.C1.dat)
rand2.points.P1.means <- colMeans(rand2.points.P1.dat)
rand2.points.N1.means <- colMeans(rand2.points.N1.dat)
rand2.points.Occ.means <- colMeans(rand2.points.Occ.dat)
rand2.points.Left.means <- colMeans(rand2.points.Left.dat)
rand2.points.Right.means <- colMeans(rand2.points.Right.dat)
rand2.points.Right_Left.means <- colMeans(rand2.points.Right_Left.dat)
rand2.points.N2.means <- colMeans(rand2.points.N2.dat)
rand2.points.LP.means <- colMeans(rand2.points.LP.dat)


sqr3.points.P3.means <- colMeans(sqr3.points.P3.dat)
rand3.points.P3.means <- colMeans(rand3.points.P3.dat)



#------------------------Plot Occ Nd1 Grand Averages------------------------------
labels <- levels(factor(rep_data_long2$configuration))
labels <- c("random", "square")
col_headers <- seq(-100, 596, by = 4)

# 1. Configuration comparison
# C1
# 1.1 Both groups - session 1
pdf("C1 - both groups - sqr x rand.pdf")
par(mfrow = c(2,1))
plot(col_headers, sqr1.points.C1.means, type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "C1 ROI session 1 - sqr x rand")
lines(col_headers, rand1.points.C1.means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
# 1.2. Both groups - session 2
plot(col_headers, sqr2.points.C1.means, type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "C1 ROI session 2 - sqr x rand")
lines(col_headers, rand2.points.C1.means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
dev.off()

# P1
# 1.3. Aware - session 1
pdf("Nd1 occ aware - sqr x rand.pdf")
par(mfrow = c(2,1))
plot(col_headers, sqr1.points.Occ.means, type = "l", xlab = "Time points", 
     ylab = "mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd1 occ session 1 sqr x rand")
lines(col_headers, rand1.points.Occ.means, type = "l", col = "blue")
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

# N1
# 1.3. Aware - session 1
pdf("Nd1 occ aware - sqr x rand.pdf")
par(mfrow = c(2,1))
plot(col_headers, sqr1.points.Occ.means, type = "l", xlab = "Time points", 
     ylab = "mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd1 occ session 1 sqr x rand")
lines(col_headers, rand1.points.Occ.means, type = "l", col = "blue")
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

# Occ
pdf("Nd1 both - sqr x rand.pdf")
par(mfrow = c(2,1))
plot(col_headers, sqr1.points.Occ.means, type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd1 session 1 - sqr x rand")
lines(col_headers, rand1.points.Occ.means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
# 1.2. Both groups - session 2
plot(col_headers, sqr2.points.Occ.means, type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd1 session 2 - sqr x rand")
lines(col_headers, rand2.points.Occ.means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
dev.off()

# Left
plot(col_headers, sqr1.points.Left.means, type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd2 (left) session 1 - sqr x rand")
lines(col_headers, rand1.points.Left.means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
# 1.2. Both groups - session 2
plot(col_headers, sqr2.points.Left.means, type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd2 (left) session 2 - sqr x rand")
lines(col_headers, rand2.points.Left.means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))

# Right
plot(col_headers, sqr1.points.Right.means, type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd2 (Right) session 1 - sqr x rand")
lines(col_headers, rand1.points.Right.means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
# 1.2. Both groups - session 2
plot(col_headers, sqr2.points.Right.means, type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd2 (Right) session 2 - sqr x rand")
lines(col_headers, rand2.points.Right.means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))

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


# P3
pdf("Nd1 both - sqr x rand.pdf")
par(mfrow = c(2,1))
plot(col_headers, sqr3.points.P3.means, type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-1.0, 1.5),
     main = "P3 session 3 - sqr x rand")
lines(col_headers, rand3.points.P3.means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
dev.off()


# 1.2. Aware
# Occ
pdf("Nd1 both - sqr x rand.pdf")
par(mfrow = c(2,1))
plot(col_headers, sqr1.points.Occ.aware.means, type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd1 session 1 - sqr x rand")
lines(col_headers, rand1.points.Occ.aware.means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
# 1.2. Both groups - session 2
plot(col_headers, sqr2.points.Occ.aware.means, type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd1 session 2 - sqr x rand")
lines(col_headers, rand2.points.Occ.aware.means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
dev.off()

# Left
plot(col_headers, sqr1.points.Left.aware.means, type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd2 (left) session 1 - sqr x rand")
lines(col_headers, rand1.points.Left.aware.means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
# 1.2. Both groups - session 2
plot(col_headers, sqr2.points.Left.aware.means, type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd2 (left) session 2 - sqr x rand")
lines(col_headers, rand2.points.Left.aware.means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))

# Right
plot(col_headers, sqr1.points.Right.aware.means, type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd2 (Right) session 1 - sqr x rand")
lines(col_headers, rand1.points.Right.aware.means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
# 1.2. Both groups - session 2
plot(col_headers, sqr2.points.Right.aware.means, type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd2 (Right) session 2 - sqr x rand")
lines(col_headers, rand2.points.Right.aware.means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))


# 1.3. Unaware
# Occ
pdf("Nd1 both - sqr x rand.pdf")
par(mfrow = c(2,1))
plot(col_headers, sqr1.points.Occ.unaware.means, type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd1 session 1 - sqr x rand")
lines(col_headers, rand1.points.Occ.unaware.means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
# 1.2. Both groups - session 2
plot(col_headers, sqr2.points.Occ.unaware.means, type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd1 session 2 - sqr x rand")
lines(col_headers, rand2.points.Occ.unaware.means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
dev.off()

# Left
plot(col_headers, sqr1.points.Left.unaware.means, type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd2 (left) session 1 - sqr x rand")
lines(col_headers, rand1.points.Left.unaware.means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
# 1.2. Both groups - session 2
plot(col_headers, sqr2.points.Left.unaware.means, type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd2 (left) session 2 - sqr x rand")
lines(col_headers, rand2.points.Left.unaware.means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))

# Right
plot(col_headers, sqr1.points.Right.unaware.means, type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd2 (Right) session 1 - sqr x rand")
lines(col_headers, rand1.points.Right.unaware.means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))
# 1.2. Both groups - session 2
plot(col_headers, sqr2.points.Right.unaware.means, type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.0),
     main = "Nd2 (Right) session 2 - sqr x rand")
lines(col_headers, rand2.points.Right.unaware.means, type = "l", col = "blue")
abline(h = 0, lty = 2, col = "black")
legend('topright', legend = labels, col = c("blue", "red"), lwd=c(2,2))


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