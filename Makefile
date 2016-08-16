##############################################
# Makefile for Contour integration replication
# Alexandre Nobre
# Updated: 20 July 2016
##############################################


# Base ERPs
source("./EEG data preparation.R")

# Alpha power
source("./Alpha data preparation.R")

# Trial-level ERPs
source("./Trial-level alpha data preparation.R")

# Behavioral data
source("./Behavioral data preparation.R")

# Artifact rejection lists
source("./ARlist.R")

# Trial-level threshold ERPs and alpha
source("./Trial-level threshold data preparation.R")

# Build dataframes
source("./Create datasets.R")
