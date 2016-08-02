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


# 1. Import list of files
# Import file names in working directory
sqr1.alpha.files <- list.files('./Data/Data_BVA', pattern = "1_Alpha ROI Sqr_FFTBandExport_pre")
sqr2.alpha.files <- list.files('./Data/Data_BVA', pattern = "2_Alpha ROI Sqr_FFTBandExport_pre")
rand1.alpha.files <- list.files('./Data/Data_BVA', pattern = "1_Alpha ROI Rand_FFTBandExport_pre")
rand2.alpha.files <- list.files('./Data/Data_BVA', pattern = "2_Alpha ROI Rand_FFTBandExport_pre")
# Concatenate all lists in a single list
list_fnames <- c(sqr1.alpha.files, sqr2.alpha.files, rand1.alpha.files, 
                 rand2.alpha.files)

# 2. Function to extract alpha power
extract.alpha <- function(alpha.file) {
  alpha.data <- read.delim(paste('./Data/Data_BVA/', alpha.file, sep = ""), sep = "\t", 
                           dec = ",", header = TRUE)
  alpha.power <- alpha.data[,2]
}

# 3. Extract alpha power for each segment in each subject
sqr1.alpha <- lapply(sqr1.alpha.files, extract.alpha)
sqr2.alpha <- lapply(sqr2.alpha.files, extract.alpha)
rand1.alpha <- lapply(rand1.alpha.files, extract.alpha)
rand2.alpha <- lapply(rand2.alpha.files, extract.alpha)

# 4. Compute mean power for each subject
sqr1.mean.alpha <- sapply(sqr1.alpha, mean)
sqr2.mean.alpha <- sapply(sqr2.alpha, mean)
rand1.mean.alpha <- sapply(rand1.alpha, mean)
rand2.mean.alpha <- sapply(rand2.alpha, mean)

# 5. Add to long data frame
rep_data_long3$alpha <- numeric(nrow(rep_data_long3))
rep_data_long3[rep_data_long3$configuration == 'sqr' & 
                      rep_data_long3$session == 1,]$alpha <- sqr1.mean.alpha
rep_data_long3[rep_data_long3$configuration == 'sqr' & 
                 rep_data_long3$session == 2,]$alpha <- sqr2.mean.alpha
rep_data_long3[rep_data_long3$configuration == 'rand' & 
                 rep_data_long3$session == 1,]$alpha <- rand1.mean.alpha
rep_data_long3[rep_data_long3$configuration == 'rand' & 
                 rep_data_long3$session == 2,]$alpha <- rand2.mean.alpha


# 6. Compute log
rep_data_long3$log.alpha <- log(rep_data_long3$alpha)

# 7. Add to wide data frame
questionnaire.ERPs$alpha.sqr.1 <- sqr1.mean.alpha
questionnaire.ERPs$alpha.sqr.2 <- sqr2.mean.alpha
questionnaire.ERPs$alpha.rand.1 <- rand1.mean.alpha
questionnaire.ERPs$alpha.rand.2 <- rand2.mean.alpha
# Compute means across conditions
questionnaire.ERPs$mean.alpha <- 
  rowMeans(questionnaire.ERPs[,which(colnames(questionnaire.ERPs) == 
                                       'alpha.sqr.1'): 
                                which(colnames(questionnaire.ERPs) == 
                                        'alpha.rand.2')])

# 8. Compute log
questionnaire.ERPs$log.alpha.sqr.1 <- log(questionnaire.ERPs$alpha.sqr.1)
questionnaire.ERPs$log.alpha.sqr.2 <- log(questionnaire.ERPs$alpha.sqr.2)
questionnaire.ERPs$log.alpha.rand.1 <- log(questionnaire.ERPs$alpha.rand.1)
questionnaire.ERPs$log.alpha.rand.2 <- log(questionnaire.ERPs$alpha.rand.2)
# Compute means across conditions
questionnaire.ERPs$mean.log.alpha <- 
  rowMeans(questionnaire.ERPs[,which(colnames(questionnaire.ERPs) == 
                                       'log.alpha.sqr.1'): 
                                which(colnames(questionnaire.ERPs) == 
                                        'log.alpha.rand.2')])


# 9. Dataset with behvaioral data
# 9.1. Build groups based on median-split of alpha power
low.alpha.group <- questionnaire.ERPs[which(questionnaire.ERPs$mean.alpha < 
                                     median(questionnaire.ERPs$mean.alpha)),]$Subject
high.alpha.group <- questionnaire.ERPs[which(questionnaire.ERPs$mean.alpha > 
                                      median(questionnaire.ERPs$mean.alpha)),]$Subject

# low.alpha.group <- questionnaire.ERPs[which(questionnaire.ERPs$alpha.sqr.1 < 
#                                               median(questionnaire.ERPs$alpha.sqr.1)),]$Subject
# high.alpha.group <- questionnaire.ERPs[which(questionnaire.ERPs$alpha.sqr.1 > 
#                                                median(questionnaire.ERPs$alpha.sqr.1)),]$Subject

# 9.2. Create factor for median split group
questionnaire.ERPs$alpha.group <- numeric(nrow(questionnaire.ERPs))
questionnaire.ERPs[which(questionnaire.ERPs$Subject %in% 
                  low.alpha.group),]$alpha.group <- 'low.alpha'
questionnaire.ERPs[which(questionnaire.ERPs$Subject %in% 
                  high.alpha.group),]$alpha.group <- 'high.alpha'
questionnaire.ERPs$alpha.group <- factor(questionnaire.ERPs$alpha.group)



#-----------------------Dataset without behavioral responses-----------
# 10. Append to dataset without behavioral responses
rep_data2$alpha.sqr.1 <- sqr1.mean.alpha
rep_data2$alpha.sqr.2 <- sqr2.mean.alpha
rep_data2$alpha.rand.1 <- rand1.mean.alpha
rep_data2$alpha.rand.2 <- rand2.mean.alpha
# Compute means across conditions
rep_data2$mean.alpha <- rowMeans(rep_data2[,56:59])

# 10.1. Build groups based on median-split of alpha power
low.alpha.group <- rep_data2[which(rep_data2$mean.alpha < 
                                     median(rep_data2$mean.alpha)),]$Subject
high.alpha.group <- rep_data2[which(rep_data2$mean.alpha > 
                                      median(rep_data2$mean.alpha)),]$Subject

# 10.2. Create factor for median split group
rep_data2$alpha.group <- numeric(nrow(rep_data2))
rep_data2[which(rep_data2$Subject %in% 
                  low.alpha.group),]$alpha.group <- 'low.alpha'
rep_data2[which(rep_data2$Subject %in% 
                  high.alpha.group),]$alpha.group <- 'high.alpha'
rep_data2$alpha.group <- factor(rep_data2$alpha.group)