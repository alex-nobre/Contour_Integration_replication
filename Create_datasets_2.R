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
# Data report packages
library(knitr)

# 1. Append to data frame with average ERPs
rep.data2 <- rep_data

#===============================================================================#
#============================Base ERPs long data frame==========================#
#===============================================================================#

# 2.1. Reshape by session
rep.data.long <- reshape(rep.data2, varying = 4:ncol(rep.data2), sep = "_", 
                         direction = "long", 
                         new.row.names = NULL)
rep.data.long[,ncol(rep.data.long)]<- NULL #eliminate extra column added by reshape
names(rep.data.long)[names(rep.data.long) == "time"] <- "session"

# 2.2. Reshape by configuration
rep.data.long2 <- rep.data.long %>%
  unite(sqr,contains("sqr")) %>%
  unite(rand,contains("rand")) %>%
  gather(configuration,values,sqr:rand) %>%
  separate(values,c("C1","P1","N1","occ.nd1","occ.nd2","left.nd1","left.nd2",
                    "right.nd1","right.nd2","RL.nd1","RL.nd2","N2","LP"),
           sep = "_",
           convert = TRUE)

# 2.3. Coerce to factors
rep.data.long2$group <- factor(rep.data.long2$group)
rep.data.long2$group.original <- factor(rep.data.long2$group.original)
rep.data.long2$session <- factor(rep.data.long2$session)
rep.data.long2$configuration <- factor(rep.data.long2$configuration)

# 2.4. Create data frame with location as factor
rep.data.long3 <- rep.data.long2[,c(1:5,12,14,
                                    (ncol(rep.data.long2)-2):ncol(rep.data.long2))]

#================================================================================#
#=================================Add alpha data=================================#
#================================================================================#

# 3. Add to long data frame
# 3.1. Raw alpha values
rep.data.long2$alpha <- numeric(nrow(rep.data.long2))
rep.data.long2[rep.data.long2$configuration == 'sqr' & 
                 rep.data.long2$session == 1,]$alpha <- sqr1.mean.alpha
rep.data.long2[rep.data.long2$configuration == 'sqr' & 
                 rep.data.long2$session == 2,]$alpha <- sqr2.mean.alpha
rep.data.long2[rep.data.long2$configuration == 'rand' & 
                 rep.data.long2$session == 1,]$alpha <- rand1.mean.alpha
rep.data.long2[rep.data.long2$configuration == 'rand' & 
                 rep.data.long2$session == 2,]$alpha <- rand2.mean.alpha
# 3.2. Log alpha values
rep.data.long2$log.alpha <- log(rep.data.long2$alpha)

#================================================================================#
#============================Dataset with behavioral data========================#
#================================================================================#

#====================4.1. Bind dprime values to wide data frame==================#

rep.data4 <- cbind(rep.data2, dprime.sqr_1, dprime.sqr_2, dprime.rand_1,
                   dprime.rand_2, dprime_1, dprime_2)

#========================4.2.1. Bind RT values to data frame=====================#

rep.data5 <- cbind(rep.data4, RT.mean.sqr_1, RT.mean.sqr_2, RT.mean.sqr_3,
                   RT.mean.rand_1, RT.mean.rand_2, RT.mean.rand_3, RT.mean_1,
                   RT.mean_2, RT.mean_3,
                   RT.sd.sqr_1, RT.sd.sqr_2, RT.sd.sqr_3,
                   RT.sd.rand_1, RT.sd.rand_2, RT.sd.rand_3, RT.sd_1,
                   RT.sd_2, RT.sd_3)


#===========4.3. Bind session threshold, proportion correct to data frame========#

# 4.3.1. Thresholds
rep.data5 <- cbind(rep.data5, threshold.sqr_1, threshold.sqr_2, threshold.rand_1, 
                    threshold.rand_2, threshold_1, threshold_2)

# 4.3.2. Proportion correct, hits, misses, false alarms and correct rejections
rep.data5 <- cbind(rep.data5, Ph.sqr_1, Ph.sqr_2, Ph.rand_1, Ph.rand_2, Ph_1, 
                   Ph_2, Ph_3, Pfa.sqr_1, Pfa.sqr_2, Pfa.rand_1, 
                   Pfa.rand_2, Pfa_1, Pfa_2, Pfa_3, Pm.sqr_1, 
                   Pm.sqr_2, Pm.rand_1, Pm.rand_2, Pm_1, Pm_2,
                   Pm_3, Pcr.sqr_1, Pcr.sqr_2, Pcr.rand_1, Pcr.rand_2,
                   Pcr_1, Pcr_2, Pcr_3, Pc.sqr_1, Pc.sqr_2, 
                   Pc.rand_1, Pc.rand_2, Pc_1, Pc_2, Pc_3)


# 4.4. Bind questionnaire data to experiment data
questionnaire.ERPs <- cbind(rep.data5, questionnaire.ses1, questionnaire.ses2)

# 4.5. Coerce groups as factors
questionnaire.ERPs$group <- factor(questionnaire.ERPs$group)
questionnaire.ERPs$group.original <- factor(questionnaire.ERPs$group.original)

#---------------------Create long data frame with behavioral data--------------
# 4.6. reshape by session
questionnaire.ERPs.long <- questionnaire.ERPs[,c(which(colnames(questionnaire.ERPs) ==
                                                         "dprime.sqr_1"):
                                                   which(colnames(questionnaire.ERPs) ==
                                                           "Pc_3"),
                                                 which(colnames(questionnaire.ERPs) ==
                                                         "conf.4_1"),
                                                 which(colnames(questionnaire.ERPs) ==
                                                         "freq.4_1"),
                                                 which(colnames(questionnaire.ERPs) ==
                                                         "conf.4_2"),
                                                 which(colnames(questionnaire.ERPs) ==
                                                         "freq.4_2"))]

# 4.6.1. remove columsn for third session to reshape
questionnaire.ERPs.long <- questionnaire.ERPs.long[,
                                                   -which(colnames(questionnaire.ERPs.long) %in% 
                                                            ls(pattern = "_3"))]
questionnaire.ERPs.long[,c("dprime", "dprime_1", "dprime_2", "RT.mean", "RT.mean_1", 
                           "RT.mean_2", "RT.sd_1", "RT.sd_2", "threshold", 
                           "threshold_1", "threshold_2",
                           "Ph", "Ph_1", "Ph_2", "Pfa", "Pfa_1", "Pfa_2", 
                           "Pm", "Pm_1", "Pm_2", "Pcr", "Pcr_1", "Pcr_2", 
                           "Pc", "Pc_1", "Pc_2")] <- NULL

# 4.6.2. reshape
questionnaire.ERPs.long <- reshape(questionnaire.ERPs.long, 
                                   varying = 1:ncol(questionnaire.ERPs.long), 
                                   sep = "_", 
                                   direction = "long", 
                                   new.row.names = NULL)
#eliminate extra column added by reshape
questionnaire.ERPs.long[,ncol(questionnaire.ERPs.long)]<- NULL 
names(questionnaire.ERPs.long)[names(questionnaire.ERPs.long) == "time"] <- 
  "behav.session"

# 4.6.3. Reshape by configuration
questionnaire.ERPs.long2 <- questionnaire.ERPs.long %>%
  unite(sqr,contains("sqr")) %>%
  unite(rand,contains("rand")) %>%
  gather(behav.configuration,values,sqr:rand) %>%
  separate(values,c("dprime", "RT.mean", "RT.sd", "threshold", 
                    "Ph", "Pfa", "Pm", "Pcr","Pc"),
           sep = "_",
           convert = TRUE)

# 4.6.4. Bind to long data frame
rep.data.long2 <- cbind(rep.data.long2, questionnaire.ERPs.long2)
rep.data.long2$behav.session <- NULL
rep.data.long2$behav.configuration <- NULL

# 4.6.5. coerce to factors
rep.data.long2$group <- factor(rep.data.long2$group)
rep.data.long2$group.original <- factor(rep.data.long2$group.original)
rep.data.long2$session <- factor(rep.data.long2$session)
rep.data.long2$configuration <- factor(rep.data.long2$configuration)

# 4.6.6. Remove intermediary data frames
rm(questionnaire.ERPs.long, questionnaire.ERPs.long2)

#--------------------Create alpha group data by median split--------------------
# 6. Add to wide data frame
# 6.1. Raw alpha values by condition 
questionnaire.ERPs$alpha.sqr.1 <- sqr1.mean.alpha
questionnaire.ERPs$alpha.sqr.2 <- sqr2.mean.alpha
questionnaire.ERPs$alpha.rand.1 <- rand1.mean.alpha
questionnaire.ERPs$alpha.rand.2 <- rand2.mean.alpha
# 6.2. Compute means of raw alpha across conditions
# All conditions
questionnaire.ERPs$mean.alpha <- 
  rowMeans(questionnaire.ERPs[,which(colnames(questionnaire.ERPs) == 
                                       'alpha.sqr.1'): 
                                which(colnames(questionnaire.ERPs) == 
                                        'alpha.rand.2')])
# Session 1
questionnaire.ERPs$mean.alpha.1 <- 
  rowMeans(questionnaire.ERPs[,c(which(colnames(questionnaire.ERPs) == 
                                         'alpha.sqr.1'), 
                                 which(colnames(questionnaire.ERPs) == 
                                         'alpha.rand.1'))])
# Session 2
questionnaire.ERPs$mean.alpha.2 <- 
  rowMeans(questionnaire.ERPs[,c(which(colnames(questionnaire.ERPs) == 
                                         'alpha.sqr.2'), 
                                 which(colnames(questionnaire.ERPs) == 
                                         'alpha.rand.2'))])
# Square
questionnaire.ERPs$mean.alpha.sqr <- 
  rowMeans(questionnaire.ERPs[,c(which(colnames(questionnaire.ERPs) == 
                                         'alpha.sqr.1'), 
                                 which(colnames(questionnaire.ERPs) == 
                                         'alpha.sqr.2'))])
# Random
questionnaire.ERPs$mean.alpha.rand <- 
  rowMeans(questionnaire.ERPs[,c(which(colnames(questionnaire.ERPs) == 
                                         'alpha.rand.1'), 
                                 which(colnames(questionnaire.ERPs) == 
                                         'alpha.rand.2'))])

# 6.3 Log alpha values by condition
# 6.3.1. By configuration and session
questionnaire.ERPs$log.alpha.sqr.1 <- log(questionnaire.ERPs$alpha.sqr.1)
questionnaire.ERPs$log.alpha.sqr.2 <- log(questionnaire.ERPs$alpha.sqr.2)
questionnaire.ERPs$log.alpha.rand.1 <- log(questionnaire.ERPs$alpha.rand.1)
questionnaire.ERPs$log.alpha.rand.2 <- log(questionnaire.ERPs$alpha.rand.2)
# 6.3.2. By Session
questionnaire.ERPs$log.alpha.1 <- log(questionnaire.ERPs$mean.alpha.1)
questionnaire.ERPs$log.alpha.2 <- log(questionnaire.ERPs$mean.alpha.2)
# 6.3.3. By configuration
questionnaire.ERPs$log.alpha.sqr <- log(questionnaire.ERPs$mean.alpha.sqr)
questionnaire.ERPs$log.alpha.rand <- log(questionnaire.ERPs$mean.alpha.rand)
# 6.2. Compute means of log alpha across conditions
questionnaire.ERPs$mean.log.alpha <- 
  rowMeans(questionnaire.ERPs[,which(colnames(questionnaire.ERPs) == 
                                       'log.alpha.sqr.1'): 
                                which(colnames(questionnaire.ERPs) == 
                                        'log.alpha.rand.2')])


# 7. Build groups based on median-split of alpha power in wide data frame
# 7.1. Create groups vectors
low.alpha.group <- questionnaire.ERPs[which(questionnaire.ERPs$mean.alpha < 
                                              median(questionnaire.ERPs$mean.alpha)),]$Subject
high.alpha.group <- questionnaire.ERPs[which(questionnaire.ERPs$mean.alpha > 
                                               median(questionnaire.ERPs$mean.alpha)),]$Subject

# 7.2. Create factor for median split group
questionnaire.ERPs$alpha.group <- numeric(nrow(questionnaire.ERPs))
questionnaire.ERPs[which(questionnaire.ERPs$Subject %in% 
                           low.alpha.group),]$alpha.group <- 'low.alpha'
questionnaire.ERPs[which(questionnaire.ERPs$Subject %in% 
                           high.alpha.group),]$alpha.group <- 'high.alpha'
questionnaire.ERPs$alpha.group <- factor(questionnaire.ERPs$alpha.group)

# 8. Append meadian-split-based group to long data frame
rep.data.long2$alpha.group <- rep(questionnaire.ERPs$alpha.group, 4)

#===========================================================================#
#==========================Correlational measures===========================#
#===========================================================================#

#=====5. Compute differences between square and random configurations======#
# 5.1. Session 1
questionnaire.ERPs$occ.diff.1 <- questionnaire.ERPs$occ.sqr.nd1_1 -
  questionnaire.ERPs$occ.rand.nd1_1
questionnaire.ERPs$left.diff.1 <- questionnaire.ERPs$left.sqr.nd2_1 -
  questionnaire.ERPs$left.rand.nd2_1
questionnaire.ERPs$right.diff.1 <- questionnaire.ERPs$right.sqr.nd2_1 -
  questionnaire.ERPs$right.rand.nd2_1
# 5.2. Session 2
questionnaire.ERPs$occ.diff.2 <- questionnaire.ERPs$occ.sqr.nd1_2 -
  questionnaire.ERPs$occ.rand.nd1_2
questionnaire.ERPs$left.diff.2 <- questionnaire.ERPs$left.sqr.nd2_2 -
  questionnaire.ERPs$left.rand.nd2_2
questionnaire.ERPs$right.diff.2 <- questionnaire.ERPs$right.sqr.nd2_2 -
  questionnaire.ERPs$right.rand.nd2_2

# 5.. Append to long data frame
rep.data.long2$right.diff <- numeric(nrow(rep.data.long2))
rep.data.long2[rep.data.long2$session == 1,]$right.diff <- questionnaire.ERPs$right.diff.1
rep.data.long2[rep.data.long2$session == 2,]$right.diff <- questionnaire.ERPs$right.diff.2