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
library(psyphy)
# Data report packages
library(knitr)

# 0. Save graphical defaults
defaults <- par()

# Contrasts
contrasts(rep.data.long2$configuration) <- c(-1, 1) # contrasts for config
contrasts(rep.data.long2$session) <- c(-1, 1) # contrasts for session
contrasts(rep.data.long2$group.original) <- c(-1, 1) # contrasts for group.original
contrasts(rep.data.long2$group) <- c(-1, 1) # contrasts for group

#---------------------------Psychophysical analysis-------------------------
# 1.RT
# 1.1. Descriptives
summary(questionnaire.ERPs$RT.mean.1)
summary(questionnaire.ERPs$RT.mean.2)
summary(questionnaire.ERPs$RT.mean.3)

# 1.2. Plots by session
# 1.2.1. Scatterplot of mean RT per session
layout(rbind(1,2), heights=c(5,1))
# mean RTs for each session
plot(questionnaire.ERPs$Subject, questionnaire.ERPs$RT.mean_1, main = "RT by session",
     ylim = range(c(RT.mean_2 - RT.sd_2, RT.mean_1 + RT.sd_1)),
     xlab = "Subject", ylab = "mean RT +/- SD", pch = 16, col = 5) #session 1
points(questionnaire.ERPs$Subject, questionnaire.ERPs$RT.mean_2,
     pch = 17, col = 6) #session 2
# SDs of RTs for each session
arrows(questionnaire.ERPs$Subject, RT.mean_1 - RT.sd_1, 
       questionnaire.ERPs$Subject, RT.mean_1 + RT.sd_1, 
       length=0.05, angle=90, code=3, col = 5) #session 1
arrows(questionnaire.ERPs$Subject, RT.mean_2 - RT.sd_2, 
       questionnaire.ERPs$Subject, RT.mean_2 + RT.sd_2, 
       length=0.05, angle=90, code=3, col = 6) #session 2
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = c("session 1", " session 2"), 
       col = c(5, 6),
       pch = 16)
par(defaults)
# 1.2.2. Histograms of mean RT per session
hist(questionnaire.ERPs$RT.mean.1, main = "RT in session 1",
     xlab = "RT", col = 7) #session 1
hist(questionnaire.ERPs$RT.mean.2, main = "RT in session 2",
     xlab = "RT", col = 12) #session 2

# 1.3. Plots by configuration
# 1.2.1. Scatterplot of mean RT per session
layout(rbind(1,2), heights=c(5,1))
# mean RTs for each session
plot(questionnaire.ERPs$Subject, questionnaire.ERPs$RT.mean.sqr_1, 
     main = "RT in session 1 by configuration",
     ylim = range(c(RT.mean.sqr_1 - RT.sd.sqr_1, RT.mean.sqr_1 + RT.sd.sqr_1)),
     xlab = "Subject", ylab = "mean RT +/- SD", pch = 16, col = 5) #square
points(questionnaire.ERPs$Subject, questionnaire.ERPs$RT.mean.rand_1,
       pch = 17, col = 6) #random
# SDs of RTs for each session
arrows(questionnaire.ERPs$Subject, RT.mean.sqr_1 - RT.sd.sqr_1, 
       questionnaire.ERPs$Subject, RT.mean.sqr_1 + RT.sd.sqr_1, 
       length=0.05, angle=90, code=3, col = 5) #square
arrows(questionnaire.ERPs$Subject, RT.mean.rand_1 - RT.sd.rand_1, 
       questionnaire.ERPs$Subject, RT.mean.rand_1 + RT.sd.rand_1, 
       length=0.05, angle=90, code=3, col = 6) #random
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = c("square", "random"), 
       col = c(5, 6),
       pch = 16)
par(defaults)

# 1.4. ANOVA - mean RT; session x configuration x group
RT.mean.baseline <- lme(RT.mean ~ 1, random = ~1|Subject/configuration/session, 
                   data = rep.data.long2, method = "ML") #baseline
RT.mean.config <- update(RT.mean.baseline, .~. + configuration)
RT.mean.session <- update(RT.mean.config, .~. + session)
RT.mean.group.original <- update(RT.mean.session, .~. + group.original)
RT.mean.config.session <- update(RT.mean.group.original, .~. + configuration:session)
RT.mean.session.group.original <- update(RT.mean.config.session, .~. + 
                                           session:group.original)
RT.mean.config.group.original <- update(RT.mean.session.group.original, .~. + 
                                          configuration:group.original)
RT.mean.lme <- update(RT.mean.config.group.original, .~. + 
                        configuration:session:group.original)
anova(RT.mean.baseline, RT.mean.config, RT.mean.session, RT.mean.group.original, 
      RT.mean.config.session, RT.mean.session.group.original, 
      RT.mean.config.group.original, 
      RT.mean.lme)

# # Function to bind values for each subject
# nested.columns <- function(adata, avector, aname) {
#   adata$column <- avector
# }
# nested.data <- function(nested.list) {
#   return(lapply(nested.columns, nested.list, cbind))
#   blocksnames <- paste("block.intensities.", seq_along(0:9), sep = "")
# }

# 1.5. Right difference by RT
layout(rbind(1,2), heights=c(5,1))
plot(lm(questionnaire.ERPs$left.diff.1 ~ questionnaire.ERPs$RT.mean_1))
cor.test(questionnaire.ERPs$RT.mean_1, questionnaire.ERPs$left.diff.1)

# 2. Intensities
# 2.1. Create data frame for intensities
intensities.data <- data.frame()
intensities.columsn <- data.frame()
intensities.data <- rbind(blockintensities.1[[1]], blockintensities.1[[2]], 
                          blockintensities.1[[3]], blockintensities.1[[4]], 
                          blockintensities.1[[5]], blockintensities.1[[6]], 
                          blockintensities.1[[7]], blockintensities.1[[8]], 
                          blockintensities.1[[9]], blockintensities.1[[10]])
colnames(intensities.data) <- c("block.intensities.0", "block.intensities.1", 
                                "block.intensities.2", "block.intensities.3", 
                                "block.intensities.4", "block.intensities.5", 
                                "block.intensities.6", "block.intensities.7",  
                                "block.intensities.8", "block.intensities.9")

# # Bind to dataset
# questionnaire.ERPs <- cbind(questionnaire.ERPs, intensities.data)
# 
# questionnaire.ERPs$group <- factor(questionnaire.ERPs$group)
# questionnaire.ERPs$group.original <- factor(questionnaire.ERPs$group.original)


# 2.2. Function to plot intensities
plot.intensities <- function(intlist, subject) {
  plot(seq(1:length(intlist[[subject]])), intlist[[subject]],
       xlab = "Trial", ylab = "Intensities", main = "Intensities by trial", type = 'l')
}

# 2.3. Plot intensities for all subjects
pdf('threshold estimation.pdf')
for (x in which(questionnaire.ERPs$group.original == "unaware")) {
  par(new = TRUE)
  plot(seq(1:length(intensities_1[[x]])), intensities_1[[x]],
       xlab = "Trial", ylab = "Intensities", main = "Intensities by trial", type = 'l',
       col = x)
}
#dev.off()
par(new = FALSE)

# 2.4. Separate plots
par(mfrow = c(3,3))
for (x in which(questionnaire.ERPs$group.original == "unaware")) {
  plot(seq(1:length(intensities_1[[x]])), intensities_1[[x]],
       xlab = "Trial", ylab = "Decrement values", main = paste("Subject ", x), type = 'l',
       col = x)
}
par(defaults)

length(questionnaire.ERPs[which(questionnaire.ERPs$group.original == "unaware"),]$Subject)

# 2.5. Plot block end intensities values by awareness group
layout(rbind(1,2), heights=c(5,1))
plot(x = questionnaire.ERPs$Subject, y = questionnaire.ERPs$threshold_1, 
     col = questionnaire.ERPs$group.original, pch = 16, main = "Main task thresholds",
     xlab = "Subject", ylab = "Threshold")
abline(h = mean(questionnaire.ERPs[questionnaire.ERPs$group.original == 
                                     'aware', ]$threshold_1))
abline(h = mean(questionnaire.ERPs[questionnaire.ERPs$group.original == 
                                     'unaware', ]$threshold_1), 
       col = 'red')
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = levels(questionnaire.ERPs$group.original), 
       col = c("black", "red"),
       pch = 16)
par(defaults)

# Line plot
threshold.lineplot <- ggplot(rep.data.long2, aes(x = group.original, 
                                                 y = threshold,
                                                 colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "Quest Threshold", x = "group", y = "Threshold", 
       colour = "configuration")
# 3.0. Histogram 

# 3. Median split analysis of ERPs
# 3.1. Compute medians
low.threshold.group <- questionnaire.ERPs[which(questionnaire.ERPs$threshold.1 < 
                                         median(questionnaire.ERPs$threshold.1)),]$Subject
high.threshold.group <- questionnaire.ERPs[which(questionnaire.ERPs$threshold.1 > 
                                          median(questionnaire.ERPs$threshold.1)),]$Subject

# 3.2. Create factor for median split group
questionnaire.ERPs$split.group <- numeric(nrow(questionnaire.ERPs))
questionnaire.ERPs[which(questionnaire.ERPs$Subject %in% 
                  low.threshold.group),]$split.group <- 'low.thresh'
questionnaire.ERPs[which(questionnaire.ERPs$Subject %in% 
                  high.threshold.group),]$split.group <- 'high.thresh'
questionnaire.ERPs$split.group <- factor(questionnaire.ERPs$split.group)

# 3.3. Plot - median split group x awareness group
table(questionnaire.ERPs$split.group, questionnaire.ERPs$group.original)
plot(questionnaire.ERPs$split.group, questionnaire.ERPs$group.original, 
     main = "Median split group x awareness",
     xlab = "median split group", ylab = "awareness group")

# 3.4. Chi-square - median split group x awareness group
CrossTable(questionnaire.ERPs$split.group, questionnaire.ERPs$group.original, fisher = TRUE,
           chisq = TRUE, expected = TRUE, sresid = TRUE, format = 'SPSS')

# 3.5. ERPs between median-split groups
rep.data.long2$split.group <- numeric(nrow(rep.data.long2))
rep.data.long2[which(rep.data.long2$Subject %in% 
                       low.threshold.group),]$split.group <- 'low.thresh'
rep.data.long2[which(rep.data.long2$Subject %in% 
                       high.threshold.group),]$split.group <- 'high.thresh'
rep.data.long2$split.group <- factor(rep.data.long2$split.group)
t.test(occ.nd1 ~ split.group, data = rep.data.long2) #nd1
t.test(left.nd2 ~ split.group, data = rep.data.long2) #nd2 left
t.test(left.nd2 ~ split.group, data = rep.data.long2) #nd2 right

# 3.6. Comparison Nd2 - session x configuration x median split group
# 3.6.1. Line plots
# 3.6.1.1. Left nd2
leftnd2.split.lineplot <- ggplot(rep_data_long2, aes(x = split.group, 
                                                     y = left.nd2,
                                                     colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "Left nd2 x median split group", x = "Median split group", 
       y = "Left nd2", 
       colour = "configuration")

# 3.6.1.1. Right nd2
rightnd2.split.lineplot <- ggplot(rep_data_long2, aes(x = split.group, 
                                                      y = right.nd2,
                                                      colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "right nd2 x median split group", x = "Median split group", 
       y = "right nd2", 
       colour = "configuration")

# 3.6.2. ANOVA
# 3.6.2.1. Left nd2
contrasts(rep_data_long2$configuration) <- c(-1, 1) # contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # contrasts for session
contrasts(rep_data_long2$split.group) <- c(-1, 1) # contrasts for split.group
left.nd2_baseline <- lme(left.nd2 ~ 1, random = ~1|Subject/configuration/session, 
                         data = rep_data_long2, method = "ML") #baseline
left.nd2_config <- update(left.nd2_baseline, .~. + configuration)
left.nd2_session <- update(left.nd2_config, .~. + session)
left.nd2_split.group <- update(left.nd2_session, .~. + split.group)
left.nd2_config_session <- update(left.nd2_split.group, .~. + configuration:session)
left.nd2_session_split.group <- update(left.nd2_config_session, .~. + 
                                         session:split.group)
left.nd2_config_split.group <- update(left.nd2_session_split.group, .~. + 
                                        configuration:split.group)
left.nd2_lme <- update(left.nd2_config_split.group, .~. + 
                         configuration:session:split.group)
anova(left.nd2_baseline, left.nd2_config, left.nd2_session, left.nd2_split.group, 
      left.nd2_config_session, left.nd2_session_split.group, 
      left.nd2_config_split.group, left.nd2_lme)

# 3.6.2.1. Right nd2
contrasts(rep_data_long2$configuration) <- c(-1, 1) # contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # contrasts for session
contrasts(rep_data_long2$split.group) <- c(-1, 1) # contrasts for split.group
right.nd2_baseline <- lme(right.nd2 ~ 1, random = ~1|Subject/configuration/session, 
                          data = rep_data_long2, method = "ML") #baseline
right.nd2_config <- update(right.nd2_baseline, .~. + configuration)
right.nd2_session <- update(right.nd2_config, .~. + session)
right.nd2_split.group <- update(right.nd2_session, .~. + split.group)
right.nd2_config_session <- update(right.nd2_split.group, .~. + configuration:session)
right.nd2_session_split.group <- update(right.nd2_config_session, .~. + 
                                          session:split.group)
right.nd2_config_split.group <- update(right.nd2_session_split.group, .~. + 
                                         configuration:split.group)
right.nd2_lme <- update(right.nd2_config_split.group, .~. + 
                          configuration:session:split.group)
anova(right.nd2_baseline, right.nd2_config, right.nd2_session, right.nd2_split.group, 
      right.nd2_config_session, right.nd2_session_split.group, 
      right.nd2_config_split.group, right.nd2_lme)

# 4. Psychometrical curve fitting
length.int <- lapply(intensities.1, length)
psychsubjects <- mapply(rep, seq_along(intensities.1), length.int)
psychData <- data.frame('subjects' =  unlist(psychsubjects), 'intensities' = unlist(intensities.1), 
                        'accuracy' = unlist(accuracies.1))
psychData$subjects <- factor(psychData$subjects)
psychometricFit <- quickpsy(psychData, x = intensities, k = accuracy,
                            grouping = .(subjects))

#---------------------------------Questionnaire data------------------------------
# 1. Descriptives
summary(questionnaire.ERPs[,c(25:30)])
lapply(questionnaire.ERPs[,c(25:30)], sd)

by(questionnaire.ERPs[,25:30], questionnaire.ERPs$group, 
   stat.desc, basic = FALSE)

# 2. Plots
# 2.1. Heat maps - questionnaire measures
# Session 1
cross.Freq <- count(questionnaire.ERPs, freq.4.ses1, conf.4.ses1)
kable(head(cross.Freq))
conf.x.freq <- ggplot(cross.Freq, aes(conf.4.ses1, freq.4.ses1)) +
  geom_tile(aes(fill = n), colour = "black") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Confidence x frequency session 1", x = "Confidence", y = "Frequency")

# Session 2
cross.Freq <- count(questionnaire.ERPs, freq.4.ses2, conf.4.ses2)
kable(head(cross.Freq))
conf.x.freq <- ggplot(cross.Freq, aes(conf.4.ses2, freq.4.ses2)) +
  geom_tile(aes(fill = n), colour = "black") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Confidence x frequency session 1", x = "Confidence", y = "Frequency")

# 2.2. Histograms - questionnaire measures
par(mfrow = c(1, 2))
# Session 1
hist(questionnaire.ERPs$conf.4.ses1, main = "Confidence rating square session 1",
     xlab = "Confidence rated", col = "red", breaks = c(0,1,2,3,4,5))
hist(questionnaire.ERPs$freq.4.ses1, main = "Frequency rating square session 1",
     xlab = "Frequency rated", col = "green", breaks = c(0,1,2,3,4,5))
# Session 2
hist(questionnaire.ERPs$conf.4.ses2, main = "Confidence ratings square session 2",
     xlab = "Confidence rated", col = "red", breaks = c(0,1,2,3,4,5))
hist(questionnaire.ERPs$freq.4.ses2, main = "Frequency ratings square session 1",
     xlab = "Frequency rated", col = "green", breaks = c(0,1,2,3,4,5))

# 3. Correlations between awareness ratings
# 3.1. Session 1
cor.test(questionnaire.ERPs$conf.4.ses1, questionnaire.ERPs$freq.4.ses1, 
         method = "pearson")
# 3.2. session 2
cor.test(questionnaire.ERPs$conf.4.ses2, questionnaire.ERPs$freq.4.ses2, 
         method = "pearson")

# 4. Comparison of awareness measures by session in unaware group
# 4.1. Confidence ratings
# 4.1.1. Compare means in confidence ratings
t.test(questionnaire.ERPs$conf.4.ses1, questionnaire.ERPs$conf.4.ses2, 
       paired = TRUE)
# 4.1.2. Compute effect sizes
cohen.d(questionnaire.ERPs$conf.4.ses1, questionnaire.ERPs$conf.4.ses2, 
        paired = TRUE)
# 4.2. Frequency ratings
# 4.1.1. Compare means in frequency ratings
t.test(questionnaire.ERPs$freq.4.ses1, questionnaire.ERPs$freq.4.ses2, 
       paired = TRUE)
# 4.2.2. Compute effect sizes
cohen.d(questionnaire.ERPs$freq.4.ses1, questionnaire.ERPs$freq.4.ses2, 
        paired = TRUE)

# 5. Composite awareness index
# 5.1. Plots
# 5.1.1. Scatterplot - aware index session 1
plot(x = questionnaire.ERPs$Subject, y = questionnaire.ERPs$aware.index.1, 
     col = questionnaire.ERPs$group.original, pch = 16, 
     main = "Awareness index session 1",
     xlab = "Subject", ylab = "Awareness index")
legend(2, 0, legend = levels(questionnaire.ERPs$group.original), 
       col = c("black", "red"),
       pch = 16)

# 5.1.2. Scatterplot - aware index session 2
plot(x = questionnaire.ERPs$Subject, y = questionnaire.ERPs$aware.index.2, 
     col = questionnaire.ERPs$group.original, pch = 16, 
     main = "Awareness index session 1",
     xlab = "Subject", ylab = "Awareness index")
legend(2, 0, legend = levels(questionnaire.ERPs$group.original), 
       col = c("black", "red"),
       pch = 16)

# 5.1.3. Line plots with error bars - Aware index session 1 per group
aware.index.1.lines <- ggplot(questionnaire.ERPs, aes(x = group.original, 
                                                      y = aware.index.1)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2)
labs(title = "Aware index session 1", x = "group", y = "aware index")

# 5.1.4. Line plots with error bars - Aware index session 2 per group
aware.index.2.lines <- ggplot(questionnaire.ERPs, aes(x = group.original, 
                                                      y = aware.index.2)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2)
labs(title = "Aware index session 2", x = "group", y = "aware index")


# 3. Correlations: ERP differences x awareness measures
# 3.1. ERP Differences x confidence ratings
# 3.1.1. Both groups
# 3.1.1.1. Session 1
cor.test(questionnaire.ERPs$occ.diff.1, questionnaire.ratings1$conf.4.ses1, 
         method = "pearson")
cor.test(questionnaire.ERPs$left.diff.1, questionnaire.ratings1$conf.4.ses1, 
         method = "pearson")
cor.test(questionnaire.ERPs$right.diff.1, questionnaire.ratings1$conf.4.ses1, 
         method = "pearson")

# 3.1.1.2. session 2
cor.test(questionnaire.ERPs$occ.diff.2, questionnaire.ratings1$conf.4.ses2, 
         method = "pearson")
cor.test(questionnaire.ERPs$left.diff.2, questionnaire.ratings1$conf.4.ses2, 
         method = "pearson")
cor.test(questionnaire.ERPs$right.diff.2, questionnaire.ratings1$conf.4.ses2, 
         method = "pearson")

# 3.1.2. By group
# 3.1.2.1. Session 1
cor.test(aware$conf.4.ses1, aware$occ.diff.1)
cor.test(aware$conf.4.ses1, aware$left.diff.1)
cor.test(aware$conf.4.ses1, aware$right.diff.1)
cor.test(unaware$conf.4.ses1, unaware$occ.diff.1)
cor.test(unaware$conf.4.ses1, unaware$left.diff.1)
cor.test(unaware$conf.4.ses1, unaware$right.diff.1)
# 3.1.2.1. Session 2
cor.test(aware$conf.4.ses2, aware$occ.diff.2)
cor.test(aware$conf.4.ses2, aware$left.diff.2)
cor.test(aware$conf.4.ses2, aware$right.diff.2)
cor.test(unaware$conf.4.ses2, unaware$occ.diff.2)
cor.test(unaware$conf.4.ses2, unaware$left.diff.2)
cor.test(unaware$conf.4.ses2, unaware$right.diff.2)

# 3.1. ERP Differences x frequency ratings
# 3.1.1. Both groups
# 3.1.1.1. Session 1
cor.test(questionnaire.ERPs$occ.diff.1, questionnaire.ERPs$freq.4.ses1, 
         method = "pearson")
cor.test(questionnaire.ERPs$left.diff.1, questionnaire.ERPs$freq.4.ses1, 
         method = "pearson")
cor.test(questionnaire.ERPs$right.diff.1, questionnaire.ERPs$freq.4.ses1, 
         method = "pearson")
# 3.1.1.2. session 2
cor.test(questionnaire.ERPs$occ.diff.2, questionnaire.ERPs$freq.4.ses2, 
         method = "pearson")
cor.test(questionnaire.ERPs$left.diff.2, questionnaire.ERPs$freq.4.ses2, 
         method = "pearson")
cor.test(questionnaire.ERPs$right.diff.2, questionnaire.ERPs$freq.4.ses2, 
         method = "pearson")

# 3.2.2. By group
# 3.2.2.1. Session 1
corr.test(data.frame(questionnaire.ERPs[group.original == "aware",]$occ.diff.1, 
                     questionnaire.ERPs[group.original == "aware",]$left.diff.1, 
                     questionnaire.ERPs[group.original == "aware",]$right.diff.1, 
                     questionnaire.ERPs[group.original == "aware",]$conf.4.ses1, 
                     questionnaire.ERPs[group.original == "aware",]$freq.4.ses1))
cor.test(aware$freq.4.ses1, aware$occ.diff.1)
cor.test(aware$freq.4.ses1, aware$left.diff.1)
cor.test(aware$freq.4.ses1, aware$right.diff.1)
cor.test(unaware$freq.4.ses1, unaware$occ.diff.1)
cor.test(unaware$freq.4.ses1, unaware$left.diff.1)
cor.test(unaware$freq.4.ses1, unaware$right.diff.1)
# 3.2.2.1. Session 2
cor.test(aware$freq.4.ses2, aware$occ.diff.2)
cor.test(aware$freq.4.ses2, aware$left.diff.2)
cor.test(aware$freq.4.ses2, aware$right.diff.2)
cor.test(unaware$freq.4.ses2, unaware$occ.diff.2)
cor.test(unaware$freq.4.ses2, unaware$left.diff.2)
cor.test(unaware$freq.4.ses2, unaware$right.diff.2)

# 3.2. ERP Differences x combined awareness index
cor.test(questionnaire.ERPs$occ.diff.1, questionnaire.ERPs$aware.index.1, 
         method = "pearson")
cor.test(questionnaire.ERPs$left.diff.1, questionnaire.ERPs$aware.index.1, 
         method = "pearson")
cor.test(questionnaire.ERPs$right.diff.1, questionnaire.ERPs$aware.index.1, 
         method = "pearson")

# 4. Behavioral data correlations matrix
ggpairs(questionnaire.ratings1, mapping = aes(color=Recall))

# 1.3. Correlational measures subset
# cor_data <- questionnaire.ERPs[,c("RT.mean.1", "threshold.1", "conf.4.ses1",
#                                   "freq.4.ses1", "aware.index.1", "ses.1.Pc",
#                                   "ses1.Ph", "ses1.Pm", "ses1.Pfa", "ses1.Pcr")]
# cor_data2 <- questionnaire.ERPs[,c("RT.mean.2", "threshold.2", "conf.4.ses2",
#                                   "freq.4.ses2", "aware.index.2", "ses.2.Pc",
#                                   "ses2.Ph", "ses2.Pm", "ses2.Pfa", "ses2.Pcr")]

cor_data <- questionnaire.ERPs[,c("RT.mean.1", "conf.4_1", "freq.4_1", 
                                  "occ.diff.1", "left.diff.1", "right.diff.1")]
colnames(cor_data) <- c("RT", "conf. rating", "freq. rating", "nd1 diff.",
                        "nd2 left diff.", "nd2 right diff.")
cor_data2 <- questionnaire.ERPs[,c("RT.mean.2", "conf.4_2", "freq.4_2", 
                                   "occ.diff.2", "left.diff.2", "right.diff.2")]
colnames(cor_data2) <- c("RT", "conf. rating", "freq. rating", "nd1 diff.",
                        "nd2 left diff.", "nd2 right diff.")


corrplot(cor(cor_data), method = "circle")
corrplot(cor(cor_data2), method = "circle")

# 2. Correlations: psychophysical x awareness measures
# 2.1. d' x Confidence ratings - session 1
cor.test(questionnaire.ERPs$ses.dprime_1, questionnaire.ERPs$conf.4.ses1, 
         method = "pearson")
cor.test(questionnaire.ERPs$RT.mean.1, questionnaire.ERPs$conf.4.ses1, 
         method = "pearson")

# 2.2. d' x Frequency ratings - session 1
cor.test(questionnaire.ERPs$ses.dprime_1, questionnaire.ERPs$freq.4.ses1, 
         method = "pearson")
cor.test(questionnaire.ERPs$RT.mean.1, questionnaire.ERPs$freq.4.ses1, 
         method = "pearson")

# 2.3. RT x confidence ratings by group
# 2.3.1. Session 1
cor.test(aware$RT.mean.1, aware$conf.4.ses1)
cor.test(unaware$RT.mean.1, unaware$conf.4.ses1)
# 2.3.1. Session 2
cor.test(aware$RT.mean.2, aware$conf.4.ses2)
cor.test(unaware$RT.mean.2, unaware$conf.4.ses2)

# 4. Correlations: ERP differences x psychophysical measures
# 4.1. Correlations: ERP Differences x reaction time
# 4.1.1. Both groups
# 4.1.1.1 Session 1
cor.test(questionnaire.ERPs$occ.diff.1, questionnaire.ERPs$RT.mean.1, 
         method = "pearson")
cor.test(questionnaire.ERPs$left.diff.1, questionnaire.ERPs$RT.mean.1, 
         method = "pearson")
cor.test(questionnaire.ERPs$right.diff.1, questionnaire.ERPs$RT.mean.1, 
         method = "pearson")
# 4.1.1.2 Session 2
cor.test(questionnaire.ERPs$occ.diff.2, questionnaire.ERPs$RT.mean.2, 
         method = "pearson")
cor.test(questionnaire.ERPs$left.diff.2, questionnaire.ERPs$RT.mean.2, 
         method = "pearson")
cor.test(questionnaire.ERPs$right.diff.2, questionnaire.ERPs$RT.mean.2, 
         method = "pearson")
# 4.1.1.3 Average
cor.test(questionnaire.ERPs$occ.diff.1, questionnaire.ERPs$RT.mean.1_2, 
         method = "pearson")
cor.test(questionnaire.ERPs$left.diff.1, questionnaire.ERPs$RT.mean.1_2, 
         method = "pearson")
cor.test(questionnaire.ERPs$right.diff.1, questionnaire.ERPs$RT.mean.1_2, 
         method = "pearson")

# 4.1.2. By group
# 4.1.2.1. Session 1
cor.test(aware$RT.mean.1, aware$occ.diff.1)
cor.test(aware$RT.mean.1, aware$left.diff.1)
cor.test(aware$RT.mean.1, aware$right.diff.1)
cor.test(unaware$RT.mean.1, unaware$occ.diff.1)
cor.test(unaware$RT.mean.1, unaware$left.diff.1)
cor.test(unaware$RT.mean.1, unaware$right.diff.1)
# 4.1.2.1. Session 2
cor.test(aware$RT.mean.2, aware$occ.diff.2)
cor.test(aware$RT.mean.2, aware$left.diff.2)
cor.test(aware$RT.mean.2, aware$right.diff.2)
cor.test(unaware$RT.mean.2, unaware$occ.diff.2)
cor.test(unaware$RT.mean.2, unaware$left.diff.2)
cor.test(unaware$RT.mean.2, unaware$right.diff.2)

# 4.2. ERP Differences x d'
# 4.2.1. Both groups
# 4.2.1.1. Session 1
cor.test(questionnaire.ERPs$occ.diff.1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")
cor.test(questionnaire.ERPs$left.diff.1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")
cor.test(questionnaire.ERPs$right.diff.1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")
# 4.2.1.2. Session 2
cor.test(questionnaire.ERPs$occ.diff.2, questionnaire.ERPs$ses.dprime_2, 
         method = "pearson")
cor.test(questionnaire.ERPs$left.diff.2, questionnaire.ERPs$ses.dprime_2, 
         method = "pearson")
cor.test(questionnaire.ERPs$right.diff.2, questionnaire.ERPs$ses.dprime_2, 
         method = "pearson")

# 4.2.2. By group
# 4.1.2.1. Session 1
cor.test(aware$ses.dprime_1, aware$occ.diff.1)
cor.test(aware$ses.dprime_1, aware$left.diff.1)
cor.test(aware$ses.dprime_1, aware$right.diff.1)
cor.test(unaware$ses.dprime_1, unaware$occ.diff.1)
cor.test(unaware$ses.dprime_1, unaware$left.diff.1)
cor.test(unaware$ses.dprime_1, unaware$right.diff.1)
# 4.1.2.1. Session 2
cor.test(aware$ses.dprime_2, aware$occ.diff.2)
cor.test(aware$ses.dprime_2, aware$left.diff.2)
cor.test(aware$ses.dprime_2, aware$right.diff.2)
cor.test(unaware$ses.dprime_2, unaware$occ.diff.2)
cor.test(unaware$ses.dprime_2, unaware$left.diff.2)
cor.test(unaware$ses.dprime_2, unaware$right.diff.2)

# 4.3. ERP Differences x % correct - FINISH!
cor.test(questionnaire.ERPs$occ.diff.1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")
cor.test(questionnaire.ERPs$left.diff.1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")
cor.test(questionnaire.ERPs$right.diff.1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")

# 4.3. ERP Differences x thresholds
# 4.3.1. Session 1
cor.test(questionnaire.ERPs$occ.diff.1, questionnaire.ERPs$threshold.1, 
         method = "pearson")
cor.test(questionnaire.ERPs$left.diff.1, questionnaire.ERPs$threshold.1, 
         method = "pearson")
cor.test(questionnaire.ERPs$right.diff.1, questionnaire.ERPs$threshold.1, 
         method = "pearson")

#--------------------------------Comparisons--------------------------------------
# 1. Compare d' across conditions
contrasts(rep_data_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long2$group.original) <- c(-1, 1) # setting contrasts for group.original
dprime_baseline <- lme(dprime ~ 1, random = ~1|Subject/configuration/session, 
                       data = rep_data_long2, method = "ML") #baseline
dprime_config <- update(dprime_baseline, .~. + configuration)
dprime_session <- update(dprime_config, .~. + session)
dprime_group.original <- update(dprime_session, .~. + group.original)
dprime_config_session <- update(dprime_group.original, .~. + configuration:session)
dprime_session_group.original <- update(dprime_config_session, .~. + session:group.original)
dprime_config_group.original <- update(dprime_session_group.original, .~. + configuration:group.original)
dprime_lme <- update(dprime_config_group.original, .~. + configuration:session:group.original)
anova(dprime_baseline, dprime_config, dprime_session, dprime_group.original, 
      dprime_config_session, dprime_session_group.original, dprime_config_group.original, 
      dprime_lme)

# 2. Compare RT across conditions
contrasts(rep_data_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long2$group.original) <- c(-1, 1) # setting contrasts for group.original
RT.mean.baseline <- lme(RT ~ 1, random = ~1|Subject/configuration/session, 
                   data = rep_data_long2, method = "ML") #baseline
RT.mean.config <- update(RT.mean.baseline, .~. + configuration)
RT.mean.session <- update(RT.mean.config, .~. + session)
RT.mean.group.original <- update(RT.mean.session, .~. + group.original)
RT.mean.config_session <- update(RT.mean.group.original, .~. + configuration:session)
RT.mean.session_group.original <- update(RT.mean.config_session, .~. + session:group.original)
RT.mean.config_group.original <- update(RT.mean.session_group.original, .~. + configuration:group.original)
RT.mean.lme <- update(RT.mean.config_group.original, .~. + configuration:session:group.original)
anova(RT.mean.baseline, RT.mean.config, RT.mean.session, RT.mean.group.original, 
      RT.mean.config_session, RT.mean.session_group.original, RT.mean.config_group.original, 
      RT.mean.lme)

# 3. Compare threshold across conditions
contrasts(rep_data_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long2$group.original) <- c(-1, 1) # setting contrasts for group.original
threshold_baseline <- lme(threshold ~ 1, random = ~1|Subject/configuration/session, 
                          data = rep_data_long2, method = "ML") #baseline
threshold_config <- update(threshold_baseline, .~. + configuration)
threshold_session <- update(threshold_config, .~. + session)
threshold_group.original <- update(threshold_session, .~. + group.original)
threshold_config_session <- update(threshold_group.original, .~. + configuration:session)
threshold_session_group.original <- update(threshold_config_session, .~. + session:group.original)
threshold_config_group.original <- update(threshold_session_group.original, .~. + configuration:group.original)
threshold_lme <- update(threshold_config_group.original, .~. + configuration:session:group.original)
anova(threshold_baseline, threshold_config, threshold_session, threshold_group.original, 
      threshold_config_session, threshold_session_group.original, threshold_config_group.original, 
      threshold_lme)


# 4. Thresholds.1 across conditions
t.test(threshold.1 ~ group.original, data = questionnaire.ERPs)
t.test(threshold.2 ~ group.original, data = questionnaire.ERPs)


# 5. Awareness ratings between session in unaware group
# 5.1. Compare means in awareness index
t.test(questionnaire.ERPs$aware.index.1, questionnaire.ERPs$aware.index.2, 
       paired = TRUE)
# 5.2. Compute effect sizes
cohen.d(questionnaire.ERPs$aware.index.1, questionnaire.ERPs$aware.index.2, 
        paired = TRUE)

# Logistic regression - alpha, ratings and intensity/threshold/accuracy/rt on group
questionnaire.ERPs$group.original.relevel <- 
  relevel(questionnaire.ERPs$group.original, "unaware")

# Confidence ratings
logistic.model.group1 <- glm(group.original.relevel ~ conf.4_1, 
                             data = questionnaire.ERPs,
                             family = binomial())

logistic.model.group2 <- glm(group.original.relevel ~ conf.4_1 + mean.alpha.1, 
                             data = questionnaire.ERPs,
                            family = binomial())

logistic.model.group3 <- glm(group.original.relevel ~ conf.4_1 + mean.alpha.1 + 
                               RT.mean_1, 
                             data = questionnaire.ERPs,
                             family = binomial())

logistic.model.group4 <- glm(group.original.relevel ~ conf.4_1 + mean.alpha.1 + 
                               RT.mean_1 + right.diff.1, 
                             data = questionnaire.ERPs,
                             family = binomial())

summary(logistic.model.group1)
summary(logistic.model.group2)
summary(logistic.model.group3)
summary(logistic.model.group4)

# Frequency ratings
log.model.freq.group1 <- glm(group.original.relevel ~ freq.4_1, 
                             data = questionnaire.ERPs,
                             family = binomial())

log.model.freq.group2 <- glm(group.original.relevel ~ freq.4_1 + mean.alpha.1, 
                             data = questionnaire.ERPs,
                             family = binomial())

log.model.freq.group3 <- glm(group.original.relevel ~ freq.4_1 + mean.alpha.1 + 
                               RT.mean_1, 
                             data = questionnaire.ERPs,
                             family = binomial())



summary(log.model.freq.group1)
summary(log.model.freq.group2)
summary(log.model.freq.group3)

