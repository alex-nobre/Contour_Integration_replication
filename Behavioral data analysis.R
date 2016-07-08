library(MPDiR)
library(xlsx)
library(polycor)
library(ggm)
library(GGally)
library(dplyr)
library(lattice)
library(psyphy)
library(ggplot2)
library(gridExtra)
library(knitr)
library(quickpsy)

#-----------------------------------Prepare data----------------------------
# Get list of psychopy file names
behav_ses_1 <- list.files(getwd(), pattern = "Implicit segregation IB_.*\\_1")
behav_ses_2 <- list.files(getwd(), pattern = "Implicit segregation IB_.*\\_2")
behav_ses_3 <- list.files(getwd(), pattern = "Implicit segregation IB_.*\\_3")

#-------------------------Compute Psychophysical measures----------------------------
# 1. Compute accuracy and d'
# 1.1. Function to read files and compute proportion of hits & false alarms in both 
# configs
# 1.1.1. Sessions 1 & 2 (task: dim disc detection)
# 1.1.1.1. Square and random configurations
# P(Hits) square
compute.Ph.sqr <- function(behav.file) {
  subj.data <- read.table(behav.file, header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  target.trials.sqr <- subset(subj.data, target.presence == 1 & Configuration == 1)
  Ph.sqr <- mean(target.trials.sqr$correct)
}
# P(Hits) random
compute.Ph.rand <- function(behav.file) {
  subj.data <- read.table(behav.file, header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  target.trials.rand <- subset(subj.data, target.presence == 1 & Configuration == 0)
  Ph.rand <- mean(target.trials.rand$correct)
}
# P(False alarms) square
compute.Pfa.sqr <- function(behav.file) {
    subj.data <- read.table(behav.file, header = TRUE, sep = "\t", skip = 12, 
                            row.names = NULL)
    colnames(subj.data)[5] <- "target.presence"
    notarget.trials.sqr <- subset(subj.data, target.presence == 0 & Configuration == 1)
    Pfa.sqr <- mean(notarget.trials.sqr$Resp)
}
# P(False alarms) random
compute.Pfa.rand <- function(behav.file) {
  subj.data <- read.table(behav.file, header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  notarget.trials.rand <- subset(subj.data, target.presence == 0 & Configuration == 0)
  Pfa.rand <- mean(notarget.trials.rand$Resp)
}

# 1.1.1.2. Both
# P(Hits)
compute.Ph <- function(behav.file) {
  subj.data <- read.table(behav.file, header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  target.trials <- subset(subj.data, target.presence == 1)
  Ph <- mean(target.trials$correct)
}
# P(False alarms)
compute.Pfa <- function(behav.file) {
  subj.data <- read.table(behav.file, header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  notarget.trials <- subset(subj.data, target.presence == 0)
  Pfa <- mean(notarget.trials$Resp)
}

# 1.1.2. Session 3 (task: diamond detection)
# P(Hits)
compute.Ph.ses3 <- function(behav.file) {
  subj.data <- read.table(behav.file, header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  diamond.trials <- subset(subj.data, Configuration == 2)
  Ph <- mean(diamond.trials$correct)
}
# P(False alarms)
compute.Pfa.ses3 <- function(behav.file) {
  subj.data <- read.table(behav.file, header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  colnames(subj.data)[5] <- "target.presence"
  nodiamond.trials <- subset(subj.data, Configuration == 0 | Configuration == 1)
  Pfa <- mean(nodiamond.trials$Resp)
}


# Remove defective files from list
#behav_ses_3 <- behav_ses_3[-2]

# 1.2. Compute proportions of hits for each configurations
#square
ses1.Ph.sqr <- lapply(behav_ses_1, compute.Ph.sqr)
ses2.Ph.sqr <- lapply(behav_ses_2, compute.Ph.sqr)
#random
ses1.Ph.rand <- lapply(behav_ses_1, compute.Ph.rand)
ses2.Ph.rand <- lapply(behav_ses_2, compute.Ph.rand)
# Both
ses1.Ph <- lapply(behav_ses_1, compute.Ph)
ses2.Ph <- lapply(behav_ses_2, compute.Ph)
ses3.Ph <- lapply(behav_ses_3, compute.Ph.ses3)

# 1.3. Compute proportions of false alarms for both configurations
#square
ses1.Pfa.sqr <- lapply(behav_ses_1, compute.Pfa.sqr)
ses2.Pfa.sqr <- lapply(behav_ses_2, compute.Pfa.sqr)
#random
ses1.Pfa.rand <- lapply(behav_ses_1, compute.Pfa.rand)
ses2.Pfa.rand <- lapply(behav_ses_2, compute.Pfa.rand)
# Both
ses1.Pfa <- lapply(behav_ses_1, compute.Pfa)
ses2.Pfa <- lapply(behav_ses_2, compute.Pfa)
ses3.Pfa <- lapply(behav_ses_3, compute.Pfa.ses3)

# 1.4. Compute proportion correct rejections, misses, correct - UPDATE WITH CONFIGS!!!!
ses1.Pcr <- lapply(ses1.Pfa, function(x) 1-x)
ses1.Pm <- lapply(ses1.Ph, function(y) 1-y)
ses1.Pc <- mapply(function(a, b) (a + 9 * b)/10, ses1.Ph, ses1.Pcr)

# 1.5. Function to compute d'
compute.dprime <- function(hit, fa) {
  z.hit <- qnorm(hit)
  z.fa <- qnorm(fa)
  if (is.infinite(z.fa) == T) {
    z.fa = 0
  }
  dprime <- z.hit - z.fa
}
# 1.6. Compute d'
#square
ses.sqr.dprime_1 <- mapply(compute.dprime, ses1.Ph.sqr, ses1.Pfa.sqr)
ses.sqr.dprime_2 <- mapply(compute.dprime, ses2.Ph.sqr, ses2.Pfa.sqr)
#random
ses.rand.dprime_1 <- mapply(compute.dprime, ses1.Ph.rand, ses1.Pfa.rand)
ses.rand.dprime_2 <- mapply(compute.dprime, ses2.Ph.rand, ses2.Pfa.rand)
# Both
ses.dprime_1 <- mapply(compute.dprime, ses1.Ph, ses1.Pfa)
ses.dprime_2 <- mapply(compute.dprime, ses2.Ph, ses2.Pfa)
ses.dprime_3 <- mapply(compute.dprime, ses3.Ph, ses3.Pfa)
ses.dprime_3[28] <- 3.090232

# 2. Retrieve RT
# 2.1. Functions for retrieving RT
#square
square.RT <- function(behav.file) {
  subj.data <- read.table(behav.file, header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  square.trials <- subset(subj.data, Configuration == 1 & Resp == 1 & 
                            correct == 1)
  RT.sqr <- mean(square.trials$RT)
}
#random
random.RT <- function(behav.file) {
  subj.data <- read.table(behav.file, header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  random.trials <- subset(subj.data, Configuration == 0 & Resp == 1 & 
                            correct == 1)
  RT.rand <- mean(random.trials$RT)
}
#both
retrieve.RT <- function(behav.file) {
  subj.data <- read.table(behav.file, header = TRUE, sep = "\t", skip = 12, 
                          row.names = NULL)
  response.trials <- subset(subj.data, Resp == 1 & 
                              correct == 1)
  RT <- mean(response.trials$RT)
}
# 2.2. Compute RTs for each session
#square
RT.sqr_1 <- sapply(behav_ses_1, square.RT)
RT.sqr_2 <- sapply(behav_ses_2, square.RT)
#random
RT.rand_1 <- sapply(behav_ses_1, random.RT)
RT.rand_2 <- sapply(behav_ses_2, random.RT)
#both
RT_1 <- sapply(behav_ses_1, retrieve.RT)
RT_2 <- sapply(behav_ses_2, retrieve.RT)
RT_3 <- sapply(behav_ses_3, retrieve.RT)

# 3. Prepare data frame
# Bind dprime values to data frame
rep_data3 <- cbind(rep_data2, ses.sqr.dprime_1, ses.sqr.dprime_2, ses.rand.dprime_1, 
                   ses.rand.dprime_2)
# Bind RT values to data frame
rep_data4 <- cbind(rep_data3, RT.sqr_1, RT.sqr_2, RT.rand_1, RT.rand_2)

# Convert from wide to long
# Extracts session number to column
rep_data_long <- reshape(rep_data4, varying = c("occ.sqr_1", "occ.sqr_2",
                                                "occ.rand_1", "occ.rand_2",
                                                "left.sqr_1", "left.sqr_2",
                                                "left.rand_1", "left.rand_2",
                                                "right.sqr_1", "right.sqr_2",
                                                "right.rand_1", "right.rand_2",
                                                "ses.sqr.dprime_1", "ses.sqr.dprime_2",
                                                "ses.rand.dprime_1", "ses.rand.dprime_2",
                                                "RT.sqr_1", "RT.sqr_2",
                                                "RT.rand_1", "RT.rand_2"),  
                         direction = "long", idvar = "Subject", sep = "_")
# Rename session column
names(rep_data_long)[names(rep_data_long) == "time"] <- "session"

# Rename columns to separate configuration number using sep = "_"
names(rep_data_long)[names(rep_data_long) == "occ.sqr"] <- "occ_1"
names(rep_data_long)[names(rep_data_long) == "occ.rand"] <- "occ_2"
names(rep_data_long)[names(rep_data_long) == "left.sqr"] <- "left_1"
names(rep_data_long)[names(rep_data_long) == "left.rand"] <- "left_2"
names(rep_data_long)[names(rep_data_long) == "right.sqr"] <- "right_1"
names(rep_data_long)[names(rep_data_long) == "right.rand"] <- "right_2"
names(rep_data_long)[names(rep_data_long) == "ses.sqr.dprime"] <- "ses.dprime_1"
names(rep_data_long)[names(rep_data_long) == "ses.rand.dprime"] <- "ses.dprime_2"
names(rep_data_long)[names(rep_data_long) == "RT.sqr"] <- "RT_1"
names(rep_data_long)[names(rep_data_long) == "RT.rand"] <- "RT_2"
# Extracts configuration number to column
rep_data_long2 <- reshape(rep_data_long, varying = c("occ_1", "occ_2",
                                                     "left_1", "left_2",
                                                     "right_1", "right_2",
                                                     "ses.dprime_1", "ses.dprime_2",
                                                     "RT_1", "RT_2"),  
                          direction = "long", idvar = " Subject", sep = "_")
# Rename configuration column name
names(rep_data_long2)[names(rep_data_long2) == "time"] <- "configuration"
# Rename configuration levels
rep_data_long2$configuration[rep_data_long2$configuration == 1] <- "sqr"
rep_data_long2$configuration[rep_data_long2$configuration == 2] <- "rand"
# Rename d' column
names(rep_data_long2)[names(rep_data_long2) == "ses.dprime"] <- "dprime"

# Bind dprimes for sessions to rep_data2
rep_data4 <- cbind(rep_data2, ses.dprime_1, ses.dprime_2, ses.dprime_3, RT_1, 
                   RT_2, RT_3)

# Convert configuration, group and session to factors
rep_data_long2$group <- factor(rep_data_long2$group)
rep_data_long2$group.original <- factor(rep_data_long2$group.original)
rep_data_long2$session <- factor(rep_data_long2$session)
rep_data_long2$configuration <- factor(rep_data_long2$configuration)

#--------------------------------Comparisons--------------------------------------
# 1. Compare d' across conditions
contrasts(rep_data_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long2$group) <- c(-1, 1) # setting contrasts for group
dprime_baseline <- lme(dprime ~ 1, random = ~1|Subject/configuration/session, 
                    data = rep_data_long2, method = "ML") #baseline
dprime_config <- update(dprime_baseline, .~. + configuration)
dprime_session <- update(dprime_config, .~. + session)
dprime_group <- update(dprime_session, .~. + group)
dprime_config_session <- update(dprime_group, .~. + configuration:session)
dprime_session_group <- update(dprime_config_session, .~. + session:group)
dprime_config_group <- update(dprime_session_group, .~. + configuration:group)
dprime_lme <- update(dprime_config_group, .~. + configuration:session:group)
anova(dprime_baseline, dprime_config, dprime_session, dprime_group, 
      dprime_config_session, dprime_session_group, dprime_config_group, 
      dprime_lme)

# 2. Compare RT across conditions
contrasts(rep_data_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long2$group) <- c(-1, 1) # setting contrasts for group
RT_baseline <- lme(RT ~ 1, random = ~1|Subject/configuration/session, 
                       data = rep_data_long2, method = "ML") #baseline
RT_config <- update(RT_baseline, .~. + configuration)
RT_session <- update(RT_config, .~. + session)
RT_group <- update(RT_session, .~. + group)
RT_config_session <- update(RT_group, .~. + configuration:session)
RT_session_group <- update(RT_config_session, .~. + session:group)
RT_config_group <- update(RT_session_group, .~. + configuration:group)
RT_lme <- update(RT_config_group, .~. + configuration:session:group)
anova(RT_baseline, RT_config, RT_session, RT_group, 
      RT_config_session, RT_session_group, RT_config_group, 
      RT_lme)

#---------------------------Psychometric function fitting------------------------


#---------------------------Prepare questionnaire data---------------------------
# 1. Import questionnaire data and rename columns
questionnaire.ses1 <- read.xlsx("Questionnaire data ses1.xlsx", sheetName = "Questionnaire data", 
                           header = TRUE)
questionnaire.ses2 <- read.xlsx("Questionnaire data ses2.xlsx", sheetName = "Questionnaire data", 
                                header = TRUE)
questionnaire.ses1 <- questionnaire.ses1[-c(25,26),]
questionnaire.ses2 <- questionnaire.ses2[-c(25,26),]
questionnaire.ses1[,c(1,16,17)] <- NULL # exclude subject, group & annotations columns
questionnaire.ses2[,c(1,16,17)] <- NULL # exclude subject, group & annotations columns
colnames(questionnaire.ses1) <- c("Recall",	"Block", "4.1",	"4.2", "4.3",	
                             "4.4",	"4.5",	"4.6",	"5.1",	"5.2",	"5.3",	"5.4",	
                             "5.5",	"5.6")
colnames(questionnaire.ses2) <- c("Recall",	"Block", "4.1",	"4.2", "4.3",	
                                  "4.4",	"4.5",	"4.6",	"5.1",	"5.2",	"5.3",	"5.4",	
                                  "5.5",	"5.6")
questionnaire.ERPs <- cbind(rep_data4, questionnaire.ses1)
questionnaire.ERPs$group <- factor(questionnaire.ERPs$group)

# 2. Correlational measures
# 2.1. Combined measure of awareness
questionnaire.ERPs$aware.index <- questionnaire.ERPs$ses.dprime_1 * 
  questionnaire.ERPs$`4.4`
# 2.2 Compute RT means across sessions
questionnaire.ERPs$RT_1_2 <- rowMeans(questionnaire.ERPs[,19:20])
# 2.3. Compute differences
# session 1
questionnaire.ERPs$occ_diff_1 <- questionnaire.ERPs$occ.sqr_1 - 
  questionnaire.ERPs$occ.rand_1
questionnaire.ERPs$left_diff_1 <- questionnaire.ERPs$left.sqr_1 - 
  questionnaire.ERPs$left.rand_1
questionnaire.ERPs$right_diff_1 <- questionnaire.ERPs$right.sqr_1 - 
  questionnaire.ERPs$right.rand_1
# session 2
questionnaire.ERPs$occ_diff_2 <- questionnaire.ERPs$occ.sqr_2 - 
  questionnaire.ERPs$occ.rand_2
questionnaire.ERPs$left_diff_2 <- questionnaire.ERPs$left.sqr_2 - 
  questionnaire.ERPs$left.rand_2
questionnaire.ERPs$right_diff_2 <- questionnaire.ERPs$right.sqr_2 - 
  questionnaire.ERPs$right.rand_2

# 4. Plots
defaults <- par()
par(xpd = TRUE)
# Line plots with error bars - Confidence in square per group
aware.index.lines <- ggplot(rep_data2, aes(x = group, y = aware.index)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2)
  labs(title = "Occ average reference", x = "group", y = "Nd1 occ amplitude", 
       colour = "configuration")

# Histogram - questionnaire measures
hist(questionnaire.ERPs$`4.4`)
hist(questionnaire.ERPs$`5.4`)
boxplot(RT_1 ~ group, questionnaire.ERPs)
  
# Scatterplot - d' session 1
plot(questionnaire.ERPs$Subject, questionnaire.ERPs$ses.dprime_1, 
     col = questionnaire.ERPs$group, pch = 16, main = "d' session 1", 
     xlab = "Subjects", ylab  = "d'")
legend(2,1.1, legend = levels(questionnaire.ERPs$group), col = c("black", "red"),
       pch = 16)

# Scatterplot - aware index
plot(questionnaire.ERPs$Subject, questionnaire.ERPs$aware.index, 
     col = questionnaire.ERPs$group, pch = 16, main = "aware index")
legend(12, 15, legend = levels(questionnaire.ERPs$group), col = c("black", "red"),
       pch = 16)

# Heat maps/ - ordinal variables
cross.Freq <- count(questionnaire.ERPs, `5.4`, `4.4`)
kable(head(cross.Freq))
conf.x.freq <- ggplot(cross.Freq, aes(`4.4`,`5.4`)) +
  geom_tile(aes(fill = n), colour = "black") +
  scale_fill_gradient(low = "white", high = "steelblue")

plot(questionnaire.ERPs$`5.4`, questionnaire.ERPs$`4.4`)

# 3. Descriptives
summary(questionnaire.ERPs[,c(25:30)])
lapply(questionnaire.ERPs[,c(25:30)], sd)

by(questionnaire.ERPs[,25:30], questionnaire.ERPs$group, 
   stat.desc, basic = FALSE)

#------------------------------------Correlations----------------------------
# 1. Behavioral data correlations matrix
questionnaire.ratings1 <- questionnaire.ses1[,c(2,7,13)]
questionnaire.ratings2 <- questionnaire.ses2[,c(2,7,13)]
ggpairs(questionnaire.ratings1, mapping = aes(color=Recall))
# subsets
aware <- subset(questionnaire.ERPs, group.original == 'aware')
unaware <- subset(questionnaire.ERPs, group.original == 'unaware')
# Test correlations between measures
cor.test(questionnaire.ERPs$`5.4`, questionnaire.ERPs$`4.4`, method = "pearson")

# 2. Correlations: psychophysical x awareness measures
# 2.1. d' x Confidence ratings
cor.test(questionnaire.ERPs$ses.dprime_1, questionnaire.ERPs$`4.4`, 
         method = "pearson")
cor.test(questionnaire.ERPs$RT_1, questionnaire.ERPs$`4.4`, 
         method = "pearson")
# 2.2. d' x Frequency ratings
cor.test(questionnaire.ERPs$ses.dprime_1, questionnaire.ERPs$`5.4`, 
         method = "pearson")
cor.test(questionnaire.ERPs$RT_1, questionnaire.ERPs$`5.4`, 
         method = "pearson")
# 2.3. RT x confidence ratings by group
cor.test(aware$RT_1, aware$`4.4`)
cor.test(unaware$RT_1, unaware$`4.4`)
cor.test(aware$RT_2, aware$`4.4`)
cor.test(unaware$RT_2, unaware$`4.4`)

# 3. Correlations: ERPs x awareness measures
# 3.1. ERP Differences x confidence ratings
# 3.1.1. Both groups
# 3.1.1.1. Session 1
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ratings1$`4.4`, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ratings1$`4.4`, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ratings1$`4.4`, 
         method = "pearson")
# 3.1.1.2. session 2
cor.test(questionnaire.ERPs$occ_diff_2, questionnaire.ratings1$`4.4`, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_2, questionnaire.ratings1$`4.4`, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_2, questionnaire.ratings1$`4.4`, 
         method = "pearson")
# 3.1.2. By group
# 3.1.2.1. Session 1
cor.test(aware$`4.4`, aware$occ_diff_1)
cor.test(aware$`4.4`, aware$left_diff_1)
cor.test(aware$`4.4`, aware$right_diff_1)
cor.test(unaware$`4.4`, unaware$occ_diff_1)
cor.test(unaware$`4.4`, unaware$left_diff_1)
cor.test(unaware$`4.4`, unaware$right_diff_1)
# 3.1.2.1. Session 2
cor.test(aware$`4.4`, aware$occ_diff_2)
cor.test(aware$`4.4`, aware$left_diff_2)
cor.test(aware$`4.4`, aware$right_diff_2)
cor.test(unaware$`4.4`, unaware$occ_diff_2)
cor.test(unaware$`4.4`, unaware$left_diff_2)
cor.test(unaware$`4.4`, unaware$right_diff_2)

# 3.1. ERP Differences x frequency ratings
# 3.1.1. Both groups
# 3.1.1.1. Session 1
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ratings1$`5.4`, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ratings1$`5.4`, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ratings1$`5.4`, 
         method = "pearson")
# 3.1.1.2. session 2
cor.test(questionnaire.ERPs$occ_diff_2, questionnaire.ratings1$`5.4`, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_2, questionnaire.ratings1$`5.4`, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_2, questionnaire.ratings1$`5.4`, 
         method = "pearson")
# 3.2.2. By group
# 3.2.2.1. Session 1
cor.test(aware$`5.4`, aware$occ_diff_1)
cor.test(aware$`5.4`, aware$left_diff_1)
cor.test(aware$`5.4`, aware$right_diff_1)
cor.test(unaware$`5.4`, unaware$occ_diff_1)
cor.test(unaware$`5.4`, unaware$left_diff_1)
cor.test(unaware$`5.4`, unaware$right_diff_1)
# 3.2.2.1. Session 2
cor.test(aware$`5.4`, aware$occ_diff_2)
cor.test(aware$`5.4`, aware$left_diff_2)
cor.test(aware$`5.4`, aware$right_diff_2)
cor.test(unaware$`5.4`, unaware$occ_diff_2)
cor.test(unaware$`5.4`, unaware$left_diff_2)
cor.test(unaware$`5.4`, unaware$right_diff_2)

# 3.2. ERP Differences x combined awareness index
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ERPs$aware.index, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ERPs$aware.index, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ERPs$aware.index, 
         method = "pearson")

# 4. Correlations: ERP differences x behavioral measures
# 4.1. Correlations: ERP Differences x reaction time
# 4.1.1. Both groups
# 4.1.1.1 Session 1
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ERPs$RT_1, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ERPs$RT_1, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ERPs$RT_1, 
         method = "pearson")
# 4.1.1.2 Session 2
cor.test(questionnaire.ERPs$occ_diff_2, questionnaire.ERPs$RT_2, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_2, questionnaire.ERPs$RT_2, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_2, questionnaire.ERPs$RT_2, 
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

# 4.3. ERP Differences x % correct
cor.test(questionnaire.ERPs$occ_diff_1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")
cor.test(questionnaire.ERPs$left_diff_1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")
cor.test(questionnaire.ERPs$right_diff_1, questionnaire.ERPs$ses.dprime_1, 
         method = "pearson")
