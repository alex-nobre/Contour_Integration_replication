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
rep_data_long2$alpha <- numeric(nrow(rep_data_long2))
rep_data_long2[rep_data_long2$configuration == 'sqr' & 
                      rep_data_long2$session == 1,]$alpha <- sqr1.mean.alpha
rep_data_long2[rep_data_long2$configuration == 'sqr' & 
                 rep_data_long2$session == 2,]$alpha <- sqr2.mean.alpha
rep_data_long2[rep_data_long2$configuration == 'rand' & 
                 rep_data_long2$session == 1,]$alpha <- rand1.mean.alpha
rep_data_long2[rep_data_long2$configuration == 'rand' & 
                 rep_data_long2$session == 2,]$alpha <- rand2.mean.alpha
# Compute log
rep_data_long2$log.alpha <- log(rep_data_long2$alpha)

# 6. Add to wide data frame
rep_data4$alpha.sqr.1 <- sqr1.mean.alpha
rep_data4$alpha.sqr.2 <- sqr2.mean.alpha
rep_data4$alpha.rand.1 <- rand1.mean.alpha
rep_data4$alpha.rand.2 <- rand2.mean.alpha

# Compute log
rep_data4$log.alpha.sqr.1 <- log(rep_data4$alpha.sqr.1)
rep_data4$log.alpha.sqr.2 <- log(rep_data4$alpha.sqr.2)
rep_data4$log.alpha.rand.1 <- log(rep_data4$alpha.rand.1)
rep_data4$log.alpha.rand.2 <- log(rep_data4$alpha.rand.2)

# 7. T-tests for alpha mean power - aware x unaware
# New grouping
# alpha
t.test(rep_data_long2[group == "aware",]$alpha, 
       rep_data_long2[group == "unaware",]$alpha)
# log alpha
t.test(rep_data_long2[group == "aware",]$log.alpha, 
       rep_data_long2[group == "unaware",]$log.alpha)
# effect sizes
cohen.d(rep_data_long2[group.original == "aware",]$log.alpha, 
        rep_data_long2[group.original == "unaware",]$log.alpha)

# 8. Plots
# Alpha, new grouping
alpha.line <- ggplot(rep_data_long2, aes(x = group, y = alpha, 
                                       colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "Alpha mean power", x = "new group", y = "Alpha mean", 
       colour = "configuration")
alpha.line

# Alpha, original grouping
alpha.line <- ggplot(rep_data_long2, aes(x = group.original, y = alpha, 
                                         colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "Alpha mean power", x = "original group", y = "Alpha mean", 
       colour = "configuration")
alpha.line

# Log alpha, new grouping
log.alpha.line <- ggplot(rep_data_long2, aes(x = group, y = log.alpha, 
                                             colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "log.alpha mean power new group", x = "new group", 
       y = "log.alpha mean", 
       colour = "configuration")
log.alpha.line

# Log alpha, original grouping
log.alpha.line <- ggplot(rep_data_long2, aes(x = group.original, y = log.alpha, 
                                             colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "log.alpha mean power original group", x = "original group", 
       y = "log.alpha mean", 
       colour = "configuration")
log.alpha.line

# Scatterplots
# Alpha
defaults <- par()
par(mfrow = c(2, 2))
pdf()
plot(rep_data4$Subject, rep_data4$alpha.sqr.1, col = 2, pch = 16, 
     main = "Square, session 1", xlab = 'Subject', ylab = "mean alpha power")
plot(rep_data4$Subject, rep_data4$alpha.sqr.2, col = 3, pch = 16,
     main = "Square, session 2", xlab = 'Subject', ylab = "mean alpha power")
plot(rep_data4$Subject, rep_data4$alpha.rand.1, col = 4, pch = 16,
     main = "Random, session 1", xlab = 'Subject', ylab = "mean alpha power")
plot(rep_data4$Subject, rep_data4$alpha.rand.2, col = 6, pch = 16,
     main = "Random, session 2", xlab = 'Subject', ylab = "mean alpha power")

# Log alpha
plot(rep_data4$Subject, rep_data4$log.alpha.sqr.1, col = 2, pch = 16, 
     main = "Square, session 1", xlab = 'Subject', ylab = "mean alpha power")
plot(rep_data4$Subject, rep_data4$log.alpha.sqr.2, col = 3, pch = 16,
     main = "Square, session 2", xlab = 'Subject', ylab = "mean alpha power")
plot(rep_data4$Subject, rep_data4$log.alpha.rand.1, col = 4, pch = 16,
     main = "Random, session 1", xlab = 'Subject', ylab = "mean alpha power")
plot(rep_data4$Subject, rep_data4$log.alpha.rand.2, col = 6, pch = 16,
     main = "Random, session 2", xlab = 'Subject', ylab = "mean alpha power")