##############################################
# Makefile for Contour integration replication
# Alexandre Nobre
# Updated: 20 July 2016
##############################################

# Key variables to define
RDIR = .

# Build datat sets
# Averaged ERPs
source("./EEG data preparation.R")
# Alpha power
source("./Alpha data preparation.R")
# Single trial ERPs
source("./Trial level alpha data preparation.R")
# Behavioral data
source("./Behavioral data preparation.R")

