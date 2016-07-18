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

# 0. Save graphical defaults
defaults <- par()


#---------------------------Psychophysical analysis-------------------------
# 0. Subsets
aware <- subset(questionnaire.ERPs, group.original == 'aware')
unaware <- subset(questionnaire.ERPs, group.original == 'unaware')

# 1.RT
# 1.1. Descriptives
summary(rep_data4$RT_1)
summary(rep_data4$RT_2)

# 1.2. Plots
# 1.2.1. Scatterplot
plot(rep_data4$Subject, rep_data4$RT_1, main = "RT in session 1",
     xlab = "RT", pch = 16, col = 6)
# 1.2.2. Histograms
hist(rep_data4$RT.mean.1, main = "RT in session 1",
     xlab = "RT", col = 7)
hist(rep_data4$RT.mean.2, main = "RT in session 2",
     xlab = "RT", col = 12)

# 1.3. Compare RTs between square and random
t.test(unaware$RT.mean.sqr.1, unaware$RT.mean.rand.1, paired = T)
t.test(aware$RT.mean.sqr.1, aware$RT.mean.rand.1, paired = T)
t.test(questionnaire.ERPs$RT.mean.sqr.1, questionnaire.ERPs$RT.mean.rand.1, 
       paired = T)

# 1.4. Compare RT across conditions
contrasts(rep_data_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long2$group.original) <- c(-1, 1) # setting contrasts for group.original
RT_baseline <- lme(RT ~ 1, random = ~1|Subject/configuration/session, 
                   data = rep_data_long2, method = "ML") #baseline
RT_config <- update(RT_baseline, .~. + configuration)
RT_session <- update(RT_config, .~. + session)
RT_group.original <- update(RT_session, .~. + group.original)
RT_config_session <- update(RT_group.original, .~. + configuration:session)
RT_session_group.original <- update(RT_config_session, .~. + session:group.original)
RT_config_group.original <- update(RT_session_group.original, .~. + configuration:group.original)
RT_lme <- update(RT_config_group.original, .~. + configuration:session:group.original)
anova(RT_baseline, RT_config, RT_session, RT_group.original, 
      RT_config_session, RT_session_group.original, RT_config_group.original, 
      RT_lme)

# # Function to bind values for each subject
# nested.columns <- function(adata, avector, aname) {
#   adata$column <- avector
# }
# nested.data <- function(nested.list) {
#   return(lapply(nested.columns, nested.list, cbind))
#   blocksnames <- paste("block.intensities.", seq_along(0:9), sep = "")
# }

# Create data frame for intensities
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

# Bind to dataset
rep_data4 <- cbind(rep_data4, intensities.data)

rep_data4$group <- factor(rep_data4$group)
rep_data4$group.original <- factor(rep_data4$group.original)

# Intensities
# 1.1. Function to plot intensities
plot.intensities <- function(intlist, subject) {
  plot(seq(1:length(intlist[[subject]])), intlist[[subject]], ylim = c(-0.5, 0.5),
       xlab = "Trial", ylab = "Intensities", main = "Intensities by trial", type = 'l')
}

# 1.2. Plot intensities for all subjects
pdf('timeseries.pdf')
for (x in 1:32) {
  par(new = TRUE)
  plot(seq(1:length(intensities.1[[x]])), intensities.1[[x]], ylim = c(-0.5, 0.5),
       xlab = "Trial", ylab = "Intensities", main = "Intensities by trial", type = 'l',
       col = x)
}
dev.off()
par(new = FALSE)


# 2. Plot block end intensities values
plot(x = rep_data4$Subject, y = rep_data4$threshold.1, ylim = c(-0.5, 0.5), 
     col = rep_data4$group.original, pch = 16, main = "Main task thresholds",
     xlab = "Subject", ylab = "Threshold")
legend(0, -0.2, legend = levels(rep_data4$group.original), 
       col = c("black", "red"),
       pch = 16)
abline(h = mean(rep_data4[rep_data4$group.original == 'aware', ]$threshold.1))
abline(h = mean(rep_data4[rep_data4$group.original == 'unaware', ]$threshold.1), 
       col = 'red')

# 3. Median split analysis of ERPs
# Threshold
# Line plot
threshold.lineplot <- ggplot(rep_data_long2, aes(x = group.original, 
                                                 y = threshold,
                                                 colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "Quest Threshold", x = "group", y = "Threshold", 
       colour = "configuration")
# 3.0. Histogram 
# 3.1. Compute medians
low.threshold.group <- rep_data4[which(rep_data4$threshold.1 < 
                                         median(rep_data4$threshold.1)),]$Subject
high.threshold.group <- rep_data4[which(rep_data4$threshold.1 > 
                                          median(rep_data4$threshold.1)),]$Subject

# 3.2. Create factor for median split group
rep_data4$split.group <- numeric(32)
rep_data4[which(rep_data4$Subject %in% 
                  low.threshold.group),]$split.group <- 'low.thresh'
rep_data4[which(rep_data4$Subject %in% 
                  high.threshold.group),]$split.group <- 'high.thresh'
rep_data4$split.group <- factor(rep_data4$split.group)

# 3.3. Plot median split group x awareness group
table(rep_data4$split.group, rep_data4$group.original)
plot(rep_data4$split.group, rep_data4$group.original, main = "Median split group x awareness",
     xlab = "median split group", ylab = "awareness group")

# 3.4. Chi-square
CrossTable(rep_data4$split.group, rep_data4$group.original, fisher = TRUE,
           chisq = TRUE, expected = TRUE, sresid = TRUE, format = 'SPSS')

# 6. Nd2 ERPs between median-split groups
rep_data_long2$split.group <- numeric(nrow(rep_data_long2))
rep_data_long2[which(rep_data_long2$Subject %in% 
                       low.threshold.group),]$split.group <- 'low.thresh'
rep_data_long2[which(rep_data_long2$Subject %in% 
                       high.threshold.group),]$split.group <- 'high.thresh'
rep_data_long2$split.group <- factor(rep_data_long2$split.group)
t.test(RL.nd2 ~ split.group, data = rep_data_long2)

# 3.6. Compare nd2 across conditions
# 3.6.1. Line plot
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


# 1. Variances and means
summary(questionnaire.ERPs[,c(25:30)])
lapply(questionnaire.ERPs[,c(25:30)], sd)

by(questionnaire.ERPs[,25:30], questionnaire.ERPs$group, 
   stat.desc, basic = FALSE)


# Heat maps - questionnaire measures
# Session 1
cross.Freq <- count(questionnaire.ERPs, freq.4.ses1, conf.4.ses1)
kable(head(cross.Freq))
conf.x.freq <- ggplot(cross.Freq, aes(conf.4.ses1, freq.4.ses1)) +
  geom_tile(aes(fill = n), colour = "black") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Confidence x frequency session 1", x = "Confidence", y = "Frequency") 

# Histograms - questionnaire measures
par(mfrow = c(1, 2))
# session 1
hist(questionnaire.ERPs$conf.4.ses1, main = "Confidence rating square session 1",
     xlab = "Confidence rated", col = "red", breaks = c(0,1,2,3,4,5))
hist(questionnaire.ERPs$freq.4.ses1, main = "Frequency rating square session 1",
     xlab = "Frequency rated", col = "green", breaks = c(0,1,2,3,4,5))
# session 2
hist(questionnaire.ERPs$conf.4.ses2, main = "Confidence ratings square session 2",
     xlab = "Confidence rated", col = "red", breaks = c(0,1,2,3,4,5))
hist(questionnaire.ERPs$freq.4.ses2, main = "Frequency ratings square session 1",
     xlab = "Frequency rated", col = "green", breaks = c(0,1,2,3,4,5))

# 1.3. Test correlations between awareness ratings
# 1.3.1. Session 1
cor.test(questionnaire.ERPs$conf.4.ses1, questionnaire.ERPs$freq.4.ses1, 
         method = "pearson")
# 1.3.2. session 2
cor.test(questionnaire.ERPs$conf.4.ses2, questionnaire.ERPs$freq.4.ses2, 
         method = "pearson")


# 2. Compute Correlational measures
# 2.1. Combined measure of awareness: RT x confidence ratings
# 2.1.1. Session 1
questionnaire.ERPs$aware.index.1 <- questionnaire.ERPs$RT.mean.1 * 
  questionnaire.ERPs$conf.4.ses1
# 2.1.2. Session 2
questionnaire.ERPs$aware.index.2 <- questionnaire.ERPs$RT.mean.2 * 
  questionnaire.ERPs$conf.4.ses2

# 2.2. Combined measure of awareness: RT x threshold value
# 2.2.1. Session 1
questionnaire.ERPs$aware.threshold.1 <- questionnaire.ERPs$threshold.1 * 
  questionnaire.ERPs$conf.4.ses1
# 2.2.1. Session 2
questionnaire.ERPs$aware.threshold.1 <- questionnaire.ERPs$threshold.1 * 
  questionnaire.ERPs$conf.4.ses1

# 2.2 Compute RT means across sessions
questionnaire.ERPs$RT_1_2 <- rowMeans(questionnaire.ERPs[,19:20])

# 2.3. Compute differences
# session 1
questionnaire.ERPs$occ_diff_1 <- questionnaire.ERPs$occ.sqr.nd1_1 - 
  questionnaire.ERPs$occ.rand.nd1_1
questionnaire.ERPs$left_diff_1 <- questionnaire.ERPs$left.sqr.nd2_1 - 
  questionnaire.ERPs$left.rand.nd2_1
questionnaire.ERPs$right_diff_1 <- questionnaire.ERPs$right.sqr.nd2_1 - 
  questionnaire.ERPs$right.rand.nd2_1
# session 2
questionnaire.ERPs$occ_diff_2 <- questionnaire.ERPs$occ.sqr.nd1_2 - 
  questionnaire.ERPs$occ.rand.nd1_2
questionnaire.ERPs$left_diff_2 <- questionnaire.ERPs$left.sqr.nd2_2 - 
  questionnaire.ERPs$left.rand.nd2_2
questionnaire.ERPs$right_diff_2 <- questionnaire.ERPs$right.sqr.nd2_2 - 
  questionnaire.ERPs$right.rand.nd2_2

# 2.1.3. Scatterplot - aware index session 1
plot(x = questionnaire.ERPs$Subject, y = questionnaire.ERPs$aware.index.1, 
     col = questionnaire.ERPs$group.original, pch = 16, 
     main = "Awareness index session 1",
     xlab = "Subject", ylab = "Awareness index")
legend(2, 0, legend = levels(questionnaire.ERPs$group.original), 
       col = c("black", "red"),
       pch = 16)

# 2.1.3. Scatterplot - aware index session 2
plot(x = questionnaire.ERPs$Subject, y = questionnaire.ERPs$aware.index.2, 
     col = questionnaire.ERPs$group.original, pch = 16, 
     main = "Awareness index session 1",
     xlab = "Subject", ylab = "Awareness index")
legend(2, 0, legend = levels(questionnaire.ERPs$group.original), 
       col = c("black", "red"),
       pch = 16)

# 2.1.4. Line plots with error bars - Aware index session 1 per group
aware.index.1.lines <- ggplot(questionnaire.ERPs, aes(x = group.original, 
                                                      y = aware.index.1)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2)
labs(title = "Aware index session 1", x = "group", y = "aware index")

# 2.1.4. Line plots with error bars - Aware index session 2 per group
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
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ratings1$conf.4.ses1, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ratings1$conf.4.ses1, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ratings1$conf.4.ses1, 
         method = "pearson")

# 3.1.1.2. session 2
cor.test(questionnaire.ERPs$occ_diff_2, questionnaire.ratings1$conf.4.ses2, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_2, questionnaire.ratings1$conf.4.ses2, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_2, questionnaire.ratings1$conf.4.ses2, 
         method = "pearson")

# 3.1.2. By group
# 3.1.2.1. Session 1
cor.test(aware$conf.4.ses1, aware$occ_diff_1)
cor.test(aware$conf.4.ses1, aware$left_diff_1)
cor.test(aware$conf.4.ses1, aware$right_diff_1)
cor.test(unaware$conf.4.ses1, unaware$occ_diff_1)
cor.test(unaware$conf.4.ses1, unaware$left_diff_1)
cor.test(unaware$conf.4.ses1, unaware$right_diff_1)
# 3.1.2.1. Session 2
cor.test(aware$conf.4.ses2, aware$occ_diff_2)
cor.test(aware$conf.4.ses2, aware$left_diff_2)
cor.test(aware$conf.4.ses2, aware$right_diff_2)
cor.test(unaware$conf.4.ses2, unaware$occ_diff_2)
cor.test(unaware$conf.4.ses2, unaware$left_diff_2)
cor.test(unaware$conf.4.ses2, unaware$right_diff_2)

# 3.1. ERP Differences x frequency ratings
# 3.1.1. Both groups
# 3.1.1.1. Session 1
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ERPs$freq.4.ses1, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ERPs$freq.4.ses1, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ERPs$freq.4.ses1, 
         method = "pearson")
# 3.1.1.2. session 2
cor.test(questionnaire.ERPs$occ_diff_2, questionnaire.ERPs$freq.4.ses2, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_2, questionnaire.ERPs$freq.4.ses2, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_2, questionnaire.ERPs$freq.4.ses2, 
         method = "pearson")

# 3.2.2. By group
# 3.2.2.1. Session 1
cor.test(aware$freq.4.ses1, aware$occ_diff_1)
cor.test(aware$freq.4.ses1, aware$left_diff_1)
cor.test(aware$freq.4.ses1, aware$right_diff_1)
cor.test(unaware$freq.4.ses1, unaware$occ_diff_1)
cor.test(unaware$freq.4.ses1, unaware$left_diff_1)
cor.test(unaware$freq.4.ses1, unaware$right_diff_1)
# 3.2.2.1. Session 2
cor.test(aware$freq.4.ses2, aware$occ_diff_2)
cor.test(aware$freq.4.ses2, aware$left_diff_2)
cor.test(aware$freq.4.ses2, aware$right_diff_2)
cor.test(unaware$freq.4.ses2, unaware$occ_diff_2)
cor.test(unaware$freq.4.ses2, unaware$left_diff_2)
cor.test(unaware$freq.4.ses2, unaware$right_diff_2)

# 3.2. ERP Differences x combined awareness index
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ERPs$aware.index.1, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ERPs$aware.index.1, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ERPs$aware.index.1, 
         method = "pearson")

#------------------------------------Correlations----------------------------
# 1. Behavioral data correlations matrix
ggpairs(questionnaire.ratings1, mapping = aes(color=Recall))

# 1.3. Correlational measures subset
cor_data <- questionnaire.ERPs[,c("RT_1", "threshold.1", "conf.4.ses1",
                                  "freq.4.ses1", "aware.index.1", "ses.1.Pc",
                                  "ses1.Ph", "ses1.Pm", "ses1.Pfa", "ses1.Pcr")]
cor_data2 <- questionnaire.ERPs[,c("RT_2", "threshold.2", "conf.4.ses2",
                                  "freq.4.ses2", "aware.index.2", "ses.2.Pc",
                                  "ses2.Ph", "ses2.Pm", "ses2.Pfa", "ses2.Pcr")]
cor(cor_data)
cor(cor_data2)

# 2. Correlations: psychophysical x awareness measures
# 2.1. d' x Confidence ratings - session 1
cor.test(questionnaire.ERPs$ses.dprime_1, questionnaire.ERPs$conf.4.ses1, 
         method = "pearson")
cor.test(questionnaire.ERPs$RT_1, questionnaire.ERPs$conf.4.ses1, 
         method = "pearson")

# 2.2. d' x Frequency ratings - session 1
cor.test(questionnaire.ERPs$ses.dprime_1, questionnaire.ERPs$freq.4.ses1, 
         method = "pearson")
cor.test(questionnaire.ERPs$RT_1, questionnaire.ERPs$freq.4.ses1, 
         method = "pearson")

# 2.3. RT x confidence ratings by group
# 2.3.1. Session 1
cor.test(aware$RT_1, aware$conf.4.ses1)
cor.test(unaware$RT_1, unaware$conf.4.ses1)
# 2.3.1. Session 2
cor.test(aware$RT_2, aware$conf.4.ses2)
cor.test(unaware$RT_2, unaware$conf.4.ses2)

# 4. Correlations: ERP differences x psychophysical measures
# 4.1. Correlations: ERP Differences x reaction time
# 4.1.1. Both groups
# 4.1.1.1 Session 1
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ERPs$RT.mean.1, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ERPs$RT.mean.1, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ERPs$RT.mean.1, 
         method = "pearson")
# 4.1.1.2 Session 2
cor.test(questionnaire.ERPs$occ_diff_2, questionnaire.ERPs$RT.mean.2, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_2, questionnaire.ERPs$RT.mean.2, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_2, questionnaire.ERPs$RT.mean.2, 
         method = "pearson")
# 4.1.1.3 Average
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ERPs$RT_1_2, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ERPs$RT_1_2, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ERPs$RT_1_2, 
         method = "pearson")

# 4.1.2. By group
# 4.1.2.1. Session 1
cor.test(aware$RT_1, aware$occ_diff_1)
cor.test(aware$RT_1, aware$left_diff_1)
cor.test(aware$RT_1, aware$right_diff_1)
cor.test(unaware$RT_1, unaware$occ_diff_1)
cor.test(unaware$RT_1, unaware$left_diff_1)
cor.test(unaware$RT_1, unaware$right_diff_1)
# 4.1.2.1. Session 2
cor.test(aware$RT_2, aware$occ_diff_2)
cor.test(aware$RT_2, aware$left_diff_2)
cor.test(aware$RT_2, aware$right_diff_2)
cor.test(unaware$RT_2, unaware$occ_diff_2)
cor.test(unaware$RT_2, unaware$left_diff_2)
cor.test(unaware$RT_2, unaware$right_diff_2)

# 4.2. ERP Differences x d'
# 4.2.1. Both groups
# 4.2.1.1. Session 1
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")
# 4.2.1.2. Session 2
cor.test(questionnaire.ERPs$occ_diff_2, questionnaire.ERPs$ses.dprime_2, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_2, questionnaire.ERPs$ses.dprime_2, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_2, questionnaire.ERPs$ses.dprime_2, 
         method = "pearson")

# 4.2.2. By group
# 4.1.2.1. Session 1
cor.test(aware$ses.dprime_1, aware$occ_diff_1)
cor.test(aware$ses.dprime_1, aware$left_diff_1)
cor.test(aware$ses.dprime_1, aware$right_diff_1)
cor.test(unaware$ses.dprime_1, unaware$occ_diff_1)
cor.test(unaware$ses.dprime_1, unaware$left_diff_1)
cor.test(unaware$ses.dprime_1, unaware$right_diff_1)
# 4.1.2.1. Session 2
cor.test(aware$ses.dprime_2, aware$occ_diff_2)
cor.test(aware$ses.dprime_2, aware$left_diff_2)
cor.test(aware$ses.dprime_2, aware$right_diff_2)
cor.test(unaware$ses.dprime_2, unaware$occ_diff_2)
cor.test(unaware$ses.dprime_2, unaware$left_diff_2)
cor.test(unaware$ses.dprime_2, unaware$right_diff_2)

# 4.3. ERP Differences x % correct - FINISH!
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")

# 4.3. ERP Differences x thresholds
# 4.3.1. Session 1
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ERPs$threshold.1, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ERPs$threshold.1, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ERPs$threshold.1, 
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
RT_baseline <- lme(RT ~ 1, random = ~1|Subject/configuration/session, 
                   data = rep_data_long2, method = "ML") #baseline
RT_config <- update(RT_baseline, .~. + configuration)
RT_session <- update(RT_config, .~. + session)
RT_group.original <- update(RT_session, .~. + group.original)
RT_config_session <- update(RT_group.original, .~. + configuration:session)
RT_session_group.original <- update(RT_config_session, .~. + session:group.original)
RT_config_group.original <- update(RT_session_group.original, .~. + configuration:group.original)
RT_lme <- update(RT_config_group.original, .~. + configuration:session:group.original)
anova(RT_baseline, RT_config, RT_session, RT_group.original, 
      RT_config_session, RT_session_group.original, RT_config_group.original, 
      RT_lme)

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

# 5. Awareness ratings between session ins unaware group
# 5.1. Compare means in confidence ratings
t.test(questionnaire.ERPs$conf.4.ses1, questionnaire.ERPs$conf.4.ses2, 
       paired = TRUE)
# 5.2. Compute effect sizes
cohen.d(questionnaire.ERPs$conf.4.ses1, questionnaire.ERPs$conf.4.ses2, 
        paired = TRUE)

# 5.3. Compare means in frequency ratings
t.test(questionnaire.ERPs$freq.4.ses1, questionnaire.ERPs$freq.4.ses2, 
       paired = TRUE)
# 5.2. Compute effect sizes
cohen.d(questionnaire.ERPs$freq.4.ses1, questionnaire.ERPs$freq.4.ses2, 
        paired = TRUE)

# 5.1. Compare means in awareness index
t.test(questionnaire.ERPs$aware.index.1, questionnaire.ERPs$aware.index.2, 
       paired = TRUE)
# 5.2. Compute effect sizes
cohen.d(questionnaire.ERPs$aware.index.1, questionnaire.ERPs$aware.index.2, 
        paired = TRUE)


