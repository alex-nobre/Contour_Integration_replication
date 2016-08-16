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

#---------------------------Trial level alpha data frame-------------------------
# 1. Append to data frame with average ERPs
rep_data2 <- rep_data
# 1.1 Low alpha
# 1.1.1. Square, session 1
rep_data2$low.alpha.C1.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 1))
rep_data2$low.alpha.P1.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 2))
rep_data2$low.alpha.N1.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 3))
rep_data2$low.alpha.occ.nd1.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 4))
rep_data2$low.alpha.occ.nd2.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 5))
rep_data2$low.alpha.left.nd1.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 6))
rep_data2$low.alpha.left.nd2.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 7))
rep_data2$low.alpha.right.nd1.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 8))
rep_data2$low.alpha.right.nd2.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 9))
rep_data2$low.alpha.RL.nd1.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 10))
rep_data2$low.alpha.RL.nd2.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 11))
rep_data2$low.alpha.N2.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 12))
rep_data2$low.alpha.P3.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 13))
rep_data2$low.alpha.LP.sqr_1 <- unlist(lapply(low.alpha.ROIs.sqr1.means, "[[", 14))
# 1.1.2. Square, session 2
rep_data2$low.alpha.C1.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 1))
rep_data2$low.alpha.P1.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 2))
rep_data2$low.alpha.N1.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 3))
rep_data2$low.alpha.occ.nd1.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 4))
rep_data2$low.alpha.occ.nd2.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 5))
rep_data2$low.alpha.left.nd1.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 6))
rep_data2$low.alpha.left.nd2.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 7))
rep_data2$low.alpha.right.nd1.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 8))
rep_data2$low.alpha.right.nd2.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 9))
rep_data2$low.alpha.RL.nd1.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 10))
rep_data2$low.alpha.RL.nd2.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 11))
rep_data2$low.alpha.N2.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 12))
rep_data2$low.alpha.P3.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 13))
rep_data2$low.alpha.LP.sqr_2 <- unlist(lapply(low.alpha.ROIs.sqr2.means, "[[", 14))
# 1.1.3. Random, session 1
rep_data2$low.alpha.C1.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 1))
rep_data2$low.alpha.P1.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 2))
rep_data2$low.alpha.N1.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 3))
rep_data2$low.alpha.occ.nd1.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 4))
rep_data2$low.alpha.occ.nd2.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 5))
rep_data2$low.alpha.left.nd1.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 6))
rep_data2$low.alpha.left.nd2.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 7))
rep_data2$low.alpha.right.nd1.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 8))
rep_data2$low.alpha.right.nd2.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 9))
rep_data2$low.alpha.RL.nd1.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 10))
rep_data2$low.alpha.RL.nd2.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 11))
rep_data2$low.alpha.N2.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 12))
rep_data2$low.alpha.P3.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 13))
rep_data2$low.alpha.LP.rand_1 <- unlist(lapply(low.alpha.ROIs.rand1.means, "[[", 14))
# 1.1.4. Random, session 2
rep_data2$low.alpha.C1.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 1))
rep_data2$low.alpha.P1.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 2))
rep_data2$low.alpha.N1.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 3))
rep_data2$low.alpha.occ.nd1.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 4))
rep_data2$low.alpha.occ.nd2.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 5))
rep_data2$low.alpha.left.nd1.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 6))
rep_data2$low.alpha.left.nd2.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 7))
rep_data2$low.alpha.right.nd1.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 8))
rep_data2$low.alpha.right.nd2.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 9))
rep_data2$low.alpha.RL.nd1.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 10))
rep_data2$low.alpha.RL.nd2.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 11))
rep_data2$low.alpha.N2.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 12))
rep_data2$low.alpha.P3.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 13))
rep_data2$low.alpha.LP.rand_2 <- unlist(lapply(low.alpha.ROIs.rand2.means, "[[", 14))

# 1.2. High alpha
# 1.2.1. Square, session 1
rep_data2$high.alpha.C1.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 1))
rep_data2$high.alpha.P1.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 2))
rep_data2$high.alpha.N1.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 3))
rep_data2$high.alpha.occ.nd1.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 4))
rep_data2$high.alpha.occ.nd2.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 5))
rep_data2$high.alpha.left.nd1.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 6))
rep_data2$high.alpha.left.nd2.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 7))
rep_data2$high.alpha.right.nd1.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 8))
rep_data2$high.alpha.right.nd2.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 9))
rep_data2$high.alpha.RL.nd1.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 10))
rep_data2$high.alpha.RL.nd2.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 11))
rep_data2$high.alpha.N2.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 12))
rep_data2$high.alpha.P3.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 13))
rep_data2$high.alpha.LP.sqr_1 <- unlist(lapply(high.alpha.ROIs.sqr1.means, "[[", 14))
# 1.2.2. Square, session 2
rep_data2$high.alpha.C1.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 1))
rep_data2$high.alpha.P1.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 2))
rep_data2$high.alpha.N1.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 3))
rep_data2$high.alpha.occ.nd1.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 4))
rep_data2$high.alpha.occ.nd2.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 5))
rep_data2$high.alpha.left.nd1.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 6))
rep_data2$high.alpha.left.nd2.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 7))
rep_data2$high.alpha.right.nd1.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 8))
rep_data2$high.alpha.right.nd2.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 9))
rep_data2$high.alpha.RL.nd1.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 10))
rep_data2$high.alpha.RL.nd2.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 11))
rep_data2$high.alpha.N2.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 12))
rep_data2$high.alpha.P3.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 13))
rep_data2$high.alpha.LP.sqr_2 <- unlist(lapply(high.alpha.ROIs.sqr2.means, "[[", 14))
# 1.2.3. Random, session 1
rep_data2$high.alpha.C1.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 1))
rep_data2$high.alpha.P1.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 2))
rep_data2$high.alpha.N1.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 3))
rep_data2$high.alpha.occ.nd1.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 4))
rep_data2$high.alpha.occ.nd2.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 5))
rep_data2$high.alpha.left.nd1.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 6))
rep_data2$high.alpha.left.nd2.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 7))
rep_data2$high.alpha.right.nd1.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 8))
rep_data2$high.alpha.right.nd2.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 9))
rep_data2$high.alpha.RL.nd1.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 10))
rep_data2$high.alpha.RL.nd2.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 11))
rep_data2$high.alpha.N2.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 12))
rep_data2$high.alpha.P3.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 13))
rep_data2$high.alpha.LP.rand_1 <- unlist(lapply(high.alpha.ROIs.rand1.means, "[[", 14))
# 1.2.4. Random, session 2
rep_data2$high.alpha.C1.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 1))
rep_data2$high.alpha.P1.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 2))
rep_data2$high.alpha.N1.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 3))
rep_data2$high.alpha.occ.nd1.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 4))
rep_data2$high.alpha.occ.nd2.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 5))
rep_data2$high.alpha.left.nd1.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 6))
rep_data2$high.alpha.left.nd2.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 7))
rep_data2$high.alpha.right.nd1.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 8))
rep_data2$high.alpha.right.nd2.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 9))
rep_data2$high.alpha.RL.nd1.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 10))
rep_data2$high.alpha.RL.nd2.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 11))
rep_data2$high.alpha.N2.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 12))
rep_data2$high.alpha.P3.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 13))
rep_data2$high.alpha.LP.rand_2 <- unlist(lapply(high.alpha.ROIs.rand2.means, "[[", 14))


#-----------------------------------Base ERPs long data frame-----------------------------
# 2.1. reshape by session
rep_data_long <- reshape(rep_data2, varying = 4:ncol(rep_data2), sep = "_", 
                         direction = "long", 
                         new.row.names = NULL)
rep_data_long[,ncol(rep_data_long)]<- NULL #eliminate extra column added by reshape
names(rep_data_long)[names(rep_data_long) == "time"] <- "session"

# 2.2. reshape by configuration
rep.data.long2 <- rep_data_long %>%
  unite(sqr,contains("sqr")) %>%
  unite(rand,contains("rand")) %>%
  gather(configuration,values,sqr:rand) %>%
  separate(values,c("C1","P1","N1","occ.nd1","occ.nd2","left.nd1","left.nd2",
                    "right.nd1","right.nd2","RL.nd1","RL.nd2","N2","LP",
                    "low.alpha.C1","low.alpha.P1","low.alpha.N1","low.alpha.occ.nd1",
                    "low.alpha.occ.nd2","low.alpha.left.nd1","low.alpha.left.nd2",
                    "low.alpha.right.nd1","low.alpha.right.nd2","low.alpha.RL.nd1",
                    "low.alpha.RL.nd2","low.alpha.N2","low.alpha.P3","low.alpha.LP",
                    "high.alpha.C1","high.alpha.P1","high.alpha.N1","high.alpha.occ.nd1",
                    "high.alpha.occ.nd2","high.alpha.left.nd1","high.alpha.left.nd2",
                    "high.alpha.right.nd1","high.alpha.right.nd2","high.alpha.RL.nd1",
                    "high.alpha.RL.nd2","high.alpha.N2","high.alpha.P3","high.alpha.LP"),
           sep = "_",
           convert = TRUE)

# 2.3. coerce to factors
rep.data.long2$group <- factor(rep.data.long2$group)
rep.data.long2$group.original <- factor(rep.data.long2$group.original)
rep.data.long2$session <- factor(rep.data.long2$session)
rep.data.long2$configuration <- factor(rep.data.long2$configuration)

# 2.4. Create data frame with location as factor
rep.data.long3 <- rep.data.long2[,c(1:5,12,14,
                                    (ncol(rep.data.long2)-2):ncol(rep.data.long2))]

#---------------------------------Add alpha data --------------------------------
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

#-----------------------Trial-level alpha power data set------------------------
# 4. Create new data frame for analysis with alpha as factor
# 4.1. Remove base ERPs
rep_data3 <- rep_data2[,-c(4:55)]

# 4.2. Reshape by session
rep_data_alpha <- reshape(rep_data3, varying = 4:ncol(rep_data3), sep = "_", 
                          direction = "long", 
                          new.row.names = NULL)
rep_data_alpha[,ncol(rep_data_alpha)]<- NULL
names(rep_data_alpha)[names(rep_data_alpha) == "time"] <- "session"

# 4.3. Tidyr by configuration
rep_data_alpha2 <- rep_data_alpha %>%
  unite(sqr,contains("sqr")) %>%
  unite(rand,contains("rand")) %>%
  gather(configuration,values,sqr:rand) %>%
  separate(values,c("low.alpha.C1","low.alpha.P1","low.alpha.N1","low.alpha.occ.nd1",
                    "low.alpha.occ.nd2","low.alpha.left.nd1","low.alpha.left.nd2",
                    "low.alpha.right.nd1","low.alpha.right.nd2","low.alpha.RL.nd1",
                    "low.alpha.RL.nd2","low.alpha.N2","low.alpha.P3","low.alpha.LP",
                    "high.alpha.C1","high.alpha.P1","high.alpha.N1","high.alpha.occ.nd1",
                    "high.alpha.occ.nd2","high.alpha.left.nd1","high.alpha.left.nd2",
                    "high.alpha.right.nd1","high.alpha.right.nd2","high.alpha.RL.nd1",
                    "high.alpha.RL.nd2","high.alpha.N2","high.alpha.P3","high.alpha.LP"),
           sep = "_",
           convert = TRUE)

# 4.4. Tidyr by alpha power
rep_data_alpha3 <- rep_data_alpha2 %>%
  unite(low.alpha,contains("low.alpha")) %>%
  unite(high.alpha,contains("high.alpha")) %>%
  gather(alpha.power,values,low.alpha:high.alpha) %>%
  separate(values,c("C1","P1","N1","occ.nd1","occ.nd2","left.nd1","left.nd2",
                    "right.nd1","right.nd2","RL.nd1","RL.nd2","N2","P3","LP"),
           sep = "_",
           convert = TRUE)

# 4.5. Coerce to factors
rep_data_alpha3$group <- factor(rep_data_alpha3$group)
rep_data_alpha3$group.original <- factor(rep_data_alpha3$group.original)
rep_data_alpha3$session <- factor(rep_data_alpha3$session)
rep_data_alpha3$configuration <- factor(rep_data_alpha3$configuration)
rep_data_alpha3$alpha.power <- factor(rep_data_alpha3$alpha.power)

#-------------------------Dataset with behavioral data-------------------------
# 5.1. Bind dprime values to wide data frame
rep_data4 <- cbind(rep_data2, ses.sqr.dprime_1, ses.sqr.dprime_2, ses.rand.dprime_1, 
                   ses.rand.dprime_2, ses.dprime_1, ses.dprime_2)
# 5.2.1. Bind RT values to data frame
rep_data5 <- cbind(rep_data4, RT.mean.sqr.1, RT.mean.sqr.2, RT.mean.sqr.3, 
                   RT.mean.rand.1, RT.mean.rand.2, RT.mean.rand.3, RT.mean.1, 
                   RT.mean.2, RT.mean.3)
# 5.2.2. Compute RT means across sessions
questionnaire.ERPs$RT.1.2 <- rowMeans(questionnaire.ERPs[,
                                      which(colnames(questionnaire.ERPs) == "RT.mean.1"):
                                      which(colnames(questionnaire.ERPs) == "RT.mean.2")])

# 5.3. Bind session threshold, proportion correct to data frame
# 5.3.1. Thresholds
rep_data5 <- cbind(rep_data5, threshold.sqr_1, threshold.sqr_2, threshold.rand_1, 
                   threshold.rand_2, threshold.1, threshold.2)

# 5.3.2. Proportion correct, hits, misses, false alarms and correct rejections
rep_data5 <- cbind(rep_data5, ses1.Ph.sqr, ses2.Ph.sqr, ses1.Ph.rand, ses2.Ph.rand, ses1.Ph, 
                   ses2.Ph, ses3.Ph, ses1.Pfa.sqr, ses2.Pfa.sqr, ses1.Pfa.rand, 
                   ses2.Pfa.rand, ses1.Pfa, ses2.Pfa, ses3.Pfa, ses1.Pm.sqr, 
                   ses2.Pm.sqr, ses1.Pm.rand, ses2.Pm.rand, ses1.Pm, ses2.Pm,
                   ses3.Pm, ses1.Pcr.sqr, ses2.Pcr.sqr, ses1.Pcr.rand, ses2.Pcr.rand,
                   ses1.Pcr, ses2.Pcr, ses3.Pcr, ses1.Pc.sqr, ses2.Pc.sqr, 
                   ses1.Pc.rand, ses2.Pc.rand, ses1.Pc, ses2.Pc, ses3.Pc)



# 5.4. Bind questionnaire data to experiment data
questionnaire.ERPs <- cbind(rep_data5, questionnaire.ses1, questionnaire.ses2)

# 5.5. Coerce groups as factors
questionnaire.ERPs$group <- factor(questionnaire.ERPs$group)
questionnaire.ERPs$group.original <- factor(questionnaire.ERPs$group.original)


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
questionnaire.ERPs$log.alpha.sqr.1 <- log(questionnaire.ERPs$alpha.sqr.1)
questionnaire.ERPs$log.alpha.sqr.2 <- log(questionnaire.ERPs$alpha.sqr.2)
questionnaire.ERPs$log.alpha.rand.1 <- log(questionnaire.ERPs$alpha.rand.1)
questionnaire.ERPs$log.alpha.rand.2 <- log(questionnaire.ERPs$alpha.rand.2)
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
rep_data_long3$alpha.group <- rep(questionnaire.ERPs$alpha.group, 4)
rep_data_alpha3$alpha.group <- factor(rep(as.vector(rep_data_long3$alpha.group), 2))
levels(rep_data_alpha3$alpha.group) <- c("high alpha group", "low alpha group")

#-------------------------------Correlational measures----------------------------
# # 9. Compute Correlational measures
# # 9.1. Combined index of awareness: RT x confidence ratings
# # 9.1.1. Session 1
# questionnaire.ERPs$aware.index.1 <- questionnaire.ERPs$RT.mean.1 *
#   questionnaire.ERPs$conf.4.ses1
# # 9.1.9. Session 2
# questionnaire.ERPs$aware.index.2 <- questionnaire.ERPs$RT.mean.2 *
#   questionnaire.ERPs$conf.4.ses2
# 
# # 9.9. Combined index of awareness: RT x threshold value
# # 9.9.1. Session 1
# questionnaire.ERPs$aware.threshold.1 <- questionnaire.ERPs$threshold.1 *
#   questionnaire.ERPs$conf.4.ses1
# # 9.9.1. Session 2
# questionnaire.ERPs$aware.threshold.1 <- questionnaire.ERPs$threshold.1 *
#   questionnaire.ERPs$conf.4.ses1


# 9.2. Compute differences
# session 1
questionnaire.ERPs$occ.diff.1 <- questionnaire.ERPs$low.alpha.occ.nd1.sqr_1 -
  questionnaire.ERPs$low.alpha.occ.nd1.rand_1
questionnaire.ERPs$left.diff.1 <- questionnaire.ERPs$low.alpha.left.nd2.sqr_1 -
  questionnaire.ERPs$low.alpha.left.nd2.rand_1
questionnaire.ERPs$right.diff.1 <- questionnaire.ERPs$low.alpha.right.nd2.sqr_1 -
  questionnaire.ERPs$low.alpha.right.nd2.rand_1
# session 2
questionnaire.ERPs$occ.diff.2 <- questionnaire.ERPs$low.alpha.occ.nd1.sqr_2 -
  questionnaire.ERPs$low.alpha.occ.nd1.rand_2
questionnaire.ERPs$left.diff.2 <- questionnaire.ERPs$low.alpha.left.nd2.sqr_2 -
  questionnaire.ERPs$low.alpha.left.nd2.rand_2
questionnaire.ERPs$right.diff.2 <- questionnaire.ERPs$low.alpha.right.nd2.sqr_2 -
  questionnaire.ERPs$low.alpha.right.nd2.rand_2

#-------------------------------Create data set for binned ERPs------------------------
# 10.1. Remove base and trial-alpha ERPs
rep_data6 <- rep_data2[,-c(4:length(rep_data2))]

# 10.1 Low bin
# 10.1.1. Square, session 1
rep_data6$low.bin.C1.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 1))
rep_data6$low.bin.P1.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 2))
rep_data6$low.bin.N1.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 3))
rep_data6$low.bin.occ.nd1.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 4))
rep_data6$low.bin.occ.nd2.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 5))
rep_data6$low.bin.left.nd1.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 6))
rep_data6$low.bin.left.nd2.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 7))
rep_data6$low.bin.right.nd1.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 8))
rep_data6$low.bin.right.nd2.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 9))
rep_data6$low.bin.RL.nd1.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 10))
rep_data6$low.bin.RL.nd2.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 11))
rep_data6$low.bin.N2.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 12))
rep_data6$low.bin.P3.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 13))
rep_data6$low.bin.LP.sqr_1 <- unlist(lapply(low.bin.ROIs.sqr1.means, "[[", 14))
# 10.1.2. Square, session 2
rep_data6$low.bin.C1.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 1))
rep_data6$low.bin.P1.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 2))
rep_data6$low.bin.N1.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 3))
rep_data6$low.bin.occ.nd1.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 4))
rep_data6$low.bin.occ.nd2.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 5))
rep_data6$low.bin.left.nd1.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 6))
rep_data6$low.bin.left.nd2.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 7))
rep_data6$low.bin.right.nd1.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 8))
rep_data6$low.bin.right.nd2.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 9))
rep_data6$low.bin.RL.nd1.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 10))
rep_data6$low.bin.RL.nd2.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 11))
rep_data6$low.bin.N2.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 12))
rep_data6$low.bin.P3.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 13))
rep_data6$low.bin.LP.sqr_2 <- unlist(lapply(low.bin.ROIs.sqr2.means, "[[", 14))
# 10.1.3. Random, session 1
rep_data6$low.bin.C1.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 1))
rep_data6$low.bin.P1.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 2))
rep_data6$low.bin.N1.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 3))
rep_data6$low.bin.occ.nd1.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 4))
rep_data6$low.bin.occ.nd2.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 5))
rep_data6$low.bin.left.nd1.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 6))
rep_data6$low.bin.left.nd2.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 7))
rep_data6$low.bin.right.nd1.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 8))
rep_data6$low.bin.right.nd2.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 9))
rep_data6$low.bin.RL.nd1.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 10))
rep_data6$low.bin.RL.nd2.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 11))
rep_data6$low.bin.N2.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 12))
rep_data6$low.bin.P3.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 13))
rep_data6$low.bin.LP.rand_1 <- unlist(lapply(low.bin.ROIs.rand1.means, "[[", 14))
# 10.1.4. Random, session 2
rep_data6$low.bin.C1.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 1))
rep_data6$low.bin.P1.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 2))
rep_data6$low.bin.N1.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 3))
rep_data6$low.bin.occ.nd1.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 4))
rep_data6$low.bin.occ.nd2.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 5))
rep_data6$low.bin.left.nd1.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 6))
rep_data6$low.bin.left.nd2.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 7))
rep_data6$low.bin.right.nd1.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 8))
rep_data6$low.bin.right.nd2.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 9))
rep_data6$low.bin.RL.nd1.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 10))
rep_data6$low.bin.RL.nd2.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 11))
rep_data6$low.bin.N2.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 12))
rep_data6$low.bin.P3.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 13))
rep_data6$low.bin.LP.rand_2 <- unlist(lapply(low.bin.ROIs.rand2.means, "[[", 14))

# 10.2. High bin
# 10.2.1. Square, session 1
rep_data6$high.bin.C1.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 1))
rep_data6$high.bin.P1.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 2))
rep_data6$high.bin.N1.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 3))
rep_data6$high.bin.occ.nd1.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 4))
rep_data6$high.bin.occ.nd2.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 5))
rep_data6$high.bin.left.nd1.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 6))
rep_data6$high.bin.left.nd2.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 7))
rep_data6$high.bin.right.nd1.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 8))
rep_data6$high.bin.right.nd2.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 9))
rep_data6$high.bin.RL.nd1.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 10))
rep_data6$high.bin.RL.nd2.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 11))
rep_data6$high.bin.N2.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 12))
rep_data6$high.bin.P3.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 13))
rep_data6$high.bin.LP.sqr_1 <- unlist(lapply(high.bin.ROIs.sqr1.means, "[[", 14))
# 10.2.2. Square, session 2
rep_data6$high.bin.C1.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 1))
rep_data6$high.bin.P1.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 2))
rep_data6$high.bin.N1.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 3))
rep_data6$high.bin.occ.nd1.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 4))
rep_data6$high.bin.occ.nd2.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 5))
rep_data6$high.bin.left.nd1.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 6))
rep_data6$high.bin.left.nd2.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 7))
rep_data6$high.bin.right.nd1.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 8))
rep_data6$high.bin.right.nd2.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 9))
rep_data6$high.bin.RL.nd1.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 10))
rep_data6$high.bin.RL.nd2.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 11))
rep_data6$high.bin.N2.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 12))
rep_data6$high.bin.P3.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 13))
rep_data6$high.bin.LP.sqr_2 <- unlist(lapply(high.bin.ROIs.sqr2.means, "[[", 14))
# 10.2.3. Random, session 1
rep_data6$high.bin.C1.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 1))
rep_data6$high.bin.P1.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 2))
rep_data6$high.bin.N1.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 3))
rep_data6$high.bin.occ.nd1.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 4))
rep_data6$high.bin.occ.nd2.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 5))
rep_data6$high.bin.left.nd1.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 6))
rep_data6$high.bin.left.nd2.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 7))
rep_data6$high.bin.right.nd1.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 8))
rep_data6$high.bin.right.nd2.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 9))
rep_data6$high.bin.RL.nd1.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 10))
rep_data6$high.bin.RL.nd2.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 11))
rep_data6$high.bin.N2.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 12))
rep_data6$high.bin.P3.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 13))
rep_data6$high.bin.LP.rand_1 <- unlist(lapply(high.bin.ROIs.rand1.means, "[[", 14))
# 10.2.4. Random, session 2
rep_data6$high.bin.C1.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 1))
rep_data6$high.bin.P1.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 2))
rep_data6$high.bin.N1.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 3))
rep_data6$high.bin.occ.nd1.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 4))
rep_data6$high.bin.occ.nd2.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 5))
rep_data6$high.bin.left.nd1.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 6))
rep_data6$high.bin.left.nd2.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 7))
rep_data6$high.bin.right.nd1.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 8))
rep_data6$high.bin.right.nd2.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 9))
rep_data6$high.bin.RL.nd1.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 10))
rep_data6$high.bin.RL.nd2.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 11))
rep_data6$high.bin.N2.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 12))
rep_data6$high.bin.P3.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 13))
rep_data6$high.bin.LP.rand_2 <- unlist(lapply(high.bin.ROIs.rand2.means, "[[", 14))

# 10.3. Reshape by session
rep.data.bin <- reshape(rep_data6, varying = 4:ncol(rep_data6), sep = "_", 
                        direction = "long", 
                        new.row.names = NULL)
rep.data.bin[,ncol(rep.data.bin)]<- NULL
names(rep.data.bin)[names(rep.data.bin) == "time"] <- "session"

# 10.4. Tidyr by configuration
rep.data.bin2 <- rep.data.bin %>%
  unite(sqr,contains("sqr")) %>%
  unite(rand,contains("rand")) %>%
  gather(configuration,values,sqr:rand) %>%
  separate(values,c("low.bin.C1","low.bin.P1","low.bin.N1","low.bin.occ.nd1",
                    "low.bin.occ.nd2","low.bin.left.nd1","low.bin.left.nd2",
                    "low.bin.right.nd1","low.bin.right.nd2","low.bin.RL.nd1",
                    "low.bin.RL.nd2","low.bin.N2","low.bin.P3","low.bin.LP",
                    "high.bin.C1","high.bin.P1","high.bin.N1","high.bin.occ.nd1",
                    "high.bin.occ.nd2","high.bin.left.nd1","high.bin.left.nd2",
                    "high.bin.right.nd1","high.bin.right.nd2","high.bin.RL.nd1",
                    "high.bin.RL.nd2","high.bin.N2","high.bin.P3","high.bin.LP"),
           sep = "_",
           convert = TRUE)

# 10.5. Tidyr by intensity bin
rep.data.bin3 <- rep.data.bin2 %>%
  unite(low.bin,contains("low.bin")) %>%
  unite(high.bin,contains("high.bin")) %>%
  gather(intensity.bin,values,low.bin:high.bin) %>%
  separate(values,c("C1","P1","N1","occ.nd1","occ.nd2","left.nd1","left.nd2",
                    "right.nd1","right.nd2","RL.nd1","RL.nd2","N2","P3","LP"),
           sep = "_",
           convert = TRUE)

# 10.6. Coerce to factors
rep.data.bin3$group <- factor(rep.data.bin3$group)
rep.data.bin3$group.original <- factor(rep.data.bin3$group.original)
rep.data.bin3$session <- factor(rep.data.bin3$session)
rep.data.bin3$configuration <- factor(rep.data.bin3$configuration)
rep.data.bin3$intensity.bin <- factor(rep.data.bin3$intensity.bin)

#-------------------------Add threshold alpha to long data frame----------------------------
# 11. Add to long data frame
# 11.1. Raw alpha values
rep.data.bin3$alpha <- numeric(nrow(rep.data.bin3))
# Square, session 1
rep.data.bin3[rep.data.bin3$configuration == 'sqr' & 
                rep.data.bin3$session == 1 & 
                rep.data.bin3$intensity.bin == "low.bin",]$alpha <- low.bin.mean.alpha.sqr1
rep.data.bin3[rep.data.bin3$configuration == 'sqr' & 
                rep.data.bin3$session == 1 & 
                rep.data.bin3$intensity.bin == "high.bin",]$alpha <- high.bin.mean.alpha.sqr1
# Square, session 2
rep.data.bin3[rep.data.bin3$configuration == 'sqr' & 
                rep.data.bin3$session == 2 & 
                rep.data.bin3$intensity.bin == "low.bin",]$alpha <- low.bin.mean.alpha.sqr2
rep.data.bin3[rep.data.bin3$configuration == 'sqr' & 
                rep.data.bin3$session == 2 & 
                rep.data.bin3$intensity.bin == "high.bin",]$alpha <- high.bin.mean.alpha.sqr2
# Random, session 1
rep.data.bin3[rep.data.bin3$configuration == 'rand' & 
                rep.data.bin3$session == 1 & 
                rep.data.bin3$intensity.bin == "low.bin",]$alpha <- low.bin.mean.alpha.rand1
rep.data.bin3[rep.data.bin3$configuration == 'rand' & 
                rep.data.bin3$session == 1 & 
                rep.data.bin3$intensity.bin == "high.bin",]$alpha <- high.bin.mean.alpha.rand1
# Random, session 2
rep.data.bin3[rep.data.bin3$configuration == 'rand' & 
                rep.data.bin3$session == 2 & 
                rep.data.bin3$intensity.bin == "low.bin",]$alpha <- low.bin.mean.alpha.rand2
rep.data.bin3[rep.data.bin3$configuration == 'rand' & 
                rep.data.bin3$session == 2 & 
                rep.data.bin3$intensity.bin == "high.bin",]$alpha <- high.bin.mean.alpha.rand2
# 11.2. Log alpha values
rep.data.bin3$log.alpha <- log(rep.data.bin3$alpha)
