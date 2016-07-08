library(reshape)

# Import file names in working directory
P100RL_sqr1_fnames <- list.files(getwd(), pattern = "1_P100 RL Sqr")
P100RL_sqr2_fnames <- list.files(getwd(), pattern = "2_P100 RL Sqr")
P100RL_rand1_fnames <- list.files(getwd(), pattern = "1_P100 RL Rand")
P100RL_rand2_fnames <- list.files(getwd(), pattern = "2_P100 RL Rand")
# Concatenate all lists in a single list
P100RL_list_fnames <- c(P100RL_sqr1_fnames, P100RL_sqr2_fnames, P100RL_rand1_fnames, 
                        P100RL_rand2_fnames)

#-------------Individual windows extraction by file----------------
# Sqr, session 1
P100RL_sqr_ses1_means <- function(sqr1_file) {
  sqr1_data <- read.delim(sqr1_file, sep = " ", dec = ",",
                          header = TRUE)
  # Compute mean of window amplitudes
  P100RL <- sqr1_data[50:63,1]
  mean.P100RL.sqr1 <- mean(P100RL)
}

# Sqr, session 2
P100RL_sqr_ses2_means <- function(sqr2_file) {
  sqr2_data <- read.delim(sqr2_file, sep = " ", dec = ",",
                          header = TRUE)
  # Compute mean of window amplitudes
  P100RL <- sqr2_data[50:63,1]
  mean.P100RL.sqr2 <- mean(P100RL)
}

# Random, session 1
P100RL_rand_ses1_means <- function(rand1_file) {
  rand1_data <- read.delim(rand1_file, sep = " ", dec = ",",
                           header = TRUE)
  # Compute mean of window amplitudes
  P100RL <- rand1_data[50:63,1]
  mean.P100RL.rand1 <- mean(P100RL)
}

# Random, session 2
P100RL_rand_ses2_means <- function(rand2_file) {
  rand2_data <- read.delim(rand2_file, sep = " ", dec = ",",
                           header = TRUE)
  # Compute mean of window amplitudes
  P100RL <- rand2_data[50:63,1]
  mean.P100RL.rand2 <- mean(P100RL)
}

# Compute means for each file
P100RL_sqr1_means <- lapply(P100RL_sqr1_fnames, P100RL_sqr_ses1_means)
P100RL_sqr2_means <- lapply(P100RL_sqr2_fnames, P100RL_sqr_ses2_means)
P100RL_rand1_means <- lapply(P100RL_rand1_fnames, P100RL_rand_ses1_means)
P100RL_rand2_means <- lapply(P100RL_rand2_fnames, P100RL_rand_ses2_means)

# Coerce lists to data frames
P100RL_sqr1_dat <- data.frame(matrix(unlist(P100RL_sqr1_means), nrow = 34, 
                                     byrow = T))
P100RL_sqr2_dat <- data.frame(matrix(unlist(P100RL_sqr2_means), nrow = 34, 
                                     byrow = T))
P100RL_rand1_dat <- data.frame(matrix(unlist(P100RL_rand1_means), nrow = 34, 
                                      byrow = T))
P100RL_rand2_dat <- data.frame(matrix(unlist(P100RL_rand2_means), nrow = 34, 
                                      byrow = T))


# Rename columms
colnames(P100RL_sqr1_dat) <- "P100RL_1"
colnames(P100RL_sqr2_dat) <- "P100RL_2"
colnames(P100RL_rand1_dat) <- "P100RL_1"
colnames(P100RL_rand2_dat) <- "P100RL_2"


# Add group variable to square and random datasets
P100RL_sqr1_dat$group <- c("unaware", "aware", "unaware", "unaware", "unaware",
                           "aware", "unaware", "aware", "aware", "aware", "aware",
                           "unaware", "aware", "unaware", "unaware", "aware", "aware",
                           "aware", "aware", "unaware", "aware", "unaware", "unaware",
                           "unaware", "aware", "unaware", "unaware", "unaware", "unaware",
                           "unaware", "aware", "aware", "unaware", "unaware")
P100RL_sqr1_dat$group.original <- c("unaware", "aware", "unaware", "unaware", "aware", 
                                    "aware", "unaware", "aware", "aware", "aware", "aware", 
                                    "unaware", "aware", "aware", "unaware", "aware", "aware", 
                                    "aware", "aware", "unaware", "aware", "unaware", "unaware", 
                                    "unaware", "aware", "unaware", "unaware", "unaware", 
                                    "unaware", "aware", "aware", "aware", "aware", 
                                    "unaware")

# Add group variable
P100RL_rand1_dat$group <- c("unaware", "aware", "unaware", "unaware", "unaware",
                            "aware", "unaware", "aware", "aware", "aware", "aware",
                            "unaware", "aware", "unaware", "unaware", "aware", "aware",
                            "aware", "aware", "unaware", "aware", "unaware", "unaware",
                            "unaware", "aware", "unaware", "unaware", "unaware", "unaware",
                            "unaware", "aware", "aware", "unaware", "unaware")
P100RL_rand1_dat$group.original <- c("unaware", "aware", "unaware", "unaware", "aware", 
                                     "aware", "unaware", "aware", "aware", "aware", "aware", 
                                     "unaware", "aware", "aware", "unaware", "aware", "aware", 
                                     "aware", "aware", "unaware", "aware", "unaware", "unaware", 
                                     "unaware", "aware", "unaware", "unaware", "unaware", 
                                     "unaware", "aware", "aware", "aware", "aware", 
                                     "unaware")

# Square
# Add subject number
subject <- c(1,2,3,4,6:11, 13:30, 32:37)
configuration <- rep("sqr", 34)
P100RL_sqr1_dat <- cbind(subject, configuration, P100RL_sqr1_dat)

# Merge square data frames and remove subs 27 & 28
P100RL_sqr_dat <- cbind(P100RL_sqr1_dat, P100RL_sqr2_dat)
P100RL_sqr_dat <- P100RL_sqr_dat[-c(25,26),]

# Extracts session number to column
P100RL_sqr_dat_long <- reshape(P100RL_sqr_dat, 
                               varying = c("P100RL_1", "P100RL_2"),  
                               direction = "long", 
                               idvar = c("subject", "configuration", "group", 
                                         "group.original"), sep = "_")
# Rename session column
names(P100RL_sqr_dat_long)[names(P100RL_sqr_dat_long) == "time"] <- "session"


# Random
configuration <- rep("rand", 34)
P100RL_rand1_dat <- cbind(subject, configuration, P100RL_rand1_dat)
# Merge random data frames and remove subs 27 & 28
P100RL_rand_dat <- cbind(P100RL_rand1_dat, P100RL_rand2_dat)
P100RL_rand_dat <- P100RL_rand_dat[-c(25,26),]
# Extracts session number to column
P100RL_rand_dat_long <- reshape(P100RL_rand_dat,
                                varying = c("P100RL_1", "P100RL_2"), 
                                direction = "long", idvar = c("subject", 
                                                              "configuration", 
                                                              "group", 
                                                              "group.original"),
                                sep = "_")
# Rename session column
names(P100RL_rand_dat_long)[names(P100RL_rand_dat_long) == "time"] <- "session"

# Rbind aquare and random data frames
P100RL_data_long <- rbind(P100RL_sqr_dat_long, P100RL_rand_dat_long)

# Coerce IVs to factors
P100RL_data_long$group <- factor(P100RL_data_long$group)
P100RL_data_long$group.original <- factor(P100RL_data_long$group.original)
P100RL_data_long$session <- factor(P100RL_data_long$session)
P100RL_data_long$configuration <- factor(P100RL_data_long$configuration)
