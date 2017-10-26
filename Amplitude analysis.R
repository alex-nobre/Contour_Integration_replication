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
library(ez)
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

#--------------------------------Prepare data-----------------------------------
# 1. Subsets
# 1.1. Create variable for means of ROI conditions (wide format)
# rep_data2$occ.means <- rowMeans(rep_data2[,c(2,5,8,11)])
# rep_data2$left.means <- rowMeans(rep_data2[,c(3,6,9,12)])
# rep_data2$right.means <- rowMeans(rep_data2[,c(4,7,10,13)])
# # 1.2. Subset configuration x session
# sqr_config <- subset(rep_data_long2, configuration == "sqr")
# rand_config <- subset(rep_data_long2, configuration == "rand")
# session_1 <- subset(rep_data_long2, session == 1)
# session_2 <- subset(rep_data_long2, session == 2)

# 1.3. Log transform
# mapply(min, )

#===============================================================================#
#===========================Exploratory data analysis===========================#
#===============================================================================#

defaults <- par() #save graphical parameters defaults

#=======================2. Compute variances and means==========================#

by(rep.data.long2[,6:8], list(rep.data.long2$configuration, 
                              rep.data.long2$session, rep.data.long2$group.original), 
   stat.desc, basic = FALSE)

#=============================3. Normality tests================================#

# 3.1. Function to test normality of data
normality.test <- function(data, variable, color) {
  print(variable)
  print(shapiro.test(data))
  qqnorm(data, main = variable);qqline(data, col = color)
}

# 3.2. Compute normality and plot qqplot for all dependent variables
par(mfrow = c(3,3))
normal.tests <- mapply(normality.test, rep.data.long2[,c(6:9,12,14,16:18)], 
                       colnames(rep.data.long2[,c(6:9,12,14,16:18)]), 
                       1:ncol(rep.data.long2[,c(6:9,12,14,16:18)]))
par(mfrow = c(1,1))

# 3.3. Plot histograms
# 5 Histograms for normality checks
# 5.1. Nd1
hist(rep.data.long2$occ.nd1, col = 3, prob = T)
lines(density(rep.data.long2$occ.nd1), col = 2)
# 5.2. Nd2 left
par(mfrow = c(1,2))
hist(rep.data.long2$left.nd2, main = "left nd2 mean amplitude", ylim = c(0, 0.5),
     xlab = "amplitude", col = 2, prob = T)
lines(density(rep.data.long2$left.nd2), col = 4, lwd = 3)
# 5.3. Nd2 right
hist(rep.data.long2$right.nd2, main = "Right nd2 mean amplitude", ylim = c(0, 0.5),
     xlab = "amplitude", col = 3, prob = T)
lines(density(rep.data.long2$right.nd2), col = 9, lwd = 3)
par(mfrow = c(1,1))
# 5.4. Occ difference
# 5.5. Left difference
hist(questionnaire.ERPs$left.diff.1, main = "Difference in left nd2 session 1", 
     xlab = "amplitude", col = 16, prob = T)
# 5.6. Right difference
hist(questionnaire.ERPs$right.diff.1, main = "Difference in right nd2 session 1", 
     xlab = "amplitude", col = 3, prob = T)


#=============================================================================#
#============================Linear mixed models==============================#
#=============================================================================#

#==============================4.0. Set constrasts============================#

contrasts(rep.data.long2$configuration) <- c(-1, 1) # contrasts for config
contrasts(rep.data.long2$session) <- c(-1, 1) # contrasts for session
contrasts(rep.data.long2$group.original) <- c(-1, 1) # contrasts for group.original
contrasts(rep.data.long2$group) <- c(-1, 1) # contrasts for group

#================================== 4.1. C1===================================#

# 4.1.1. Scatterplot
plot(1:18, rep_data4[rep_data4$group.original == "aware",]$C1.sqr_1, 
     ylab = "mean C1 amplitude", xlab = "subjects", pch = 20, col = "red", 
     main = "C1 amplitudes")
points(1:14, rep_data4[rep_data4$group.original == "unaware",]$C1.sqr_1, 
       pch = 20, col = "blue")
abline(h=mean(rep_data4[rep_data4$group.original == "aware",]$C1.sqr_1), 
       col = "red")
abline(h=mean(rep_data4[rep_data4$group.original == "unaware",]$C1.sqr_1), 
       col = "blue")

# 4.1.2. ANOVA
C1.baseline <- lme(C1 ~ 1, random = ~1|Subject/configuration/session, 
                    data = rep.data.long2, method = "ML") #baseline
C1.config <- update(C1.baseline, .~. + configuration)
C1.session <- update(C1.config, .~. + session)
C1.group.original <- update(C1.session, .~. + group.original)
C1.config.session <- update(C1.group.original, .~. + configuration:session)
C1.session.group.original <- update(C1.config.session, .~. + session:group.original)
C1.config.group.original <- update(C1.session.group.original, .~. + 
                                     configuration:group.original)
C1.lme <- update(C1.config.group.original, .~. + configuration:session:group.original)
anova(C1.baseline, C1.config, C1.session, C1.group.original, C1.config.session,
      C1.session.group.original, C1.config.group.original, C1.lme)

# 4.1.3. Line plot
C1.line <- ggplot(rep.data.long2, aes(x = group.original, y = C1, 
                                      colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "C1 mean aplitude", x = "session", 
       y = "C1 mean amplitude", colour = "configuration")
C1.line

#==================================4.2. P1===================================#
# 4.2.1. Scatterplot
dot.colors <- ifelse(rep.data.long2[rep.data.long2$session == 1 & 
                                      group.original == "unaware",]$configuration == 
                       "rand", "red", "blue")
plot(rep.data.long2[rep.data.long2$session == 1 & 
                      group.original == "unaware",]$Subject, 
     rep.data.long2[rep.data.long2$session == 1 & 
                      group.original == "unaware",]$P1, 
     ylab = "P1 amplitude", 
     xlab = "subjects", pch = 20, col = dot.colors, 
     main = "P1 amplitudes by configuration")
abline(h=mean(rep.data.long2[rep.data.long2$session == 1 & 
                               rep.data.long2$configuration == "rand" &
                               group.original == "unaware",]$P1), 
       col = "red")
abline(h=mean(rep.data.long2[rep.data.long2$session == 1 & 
                               rep.data.long2$configuration == "sqr" &
                               group.original == "unaware",]$P1), 
       col = "blue")

# 4.2.2. ANOVA
P1.baseline <- lme(P1 ~ 1, random = ~1|Subject/configuration/session, 
                                 data = rep.data.long2, method = "ML") #baseline
P1.config <- update(P1.baseline, .~. + configuration)
P1.session <- update(P1.config, .~. + session)
P1.group.original <- update(P1.session, .~. + group.original)
P1.config.session <- update(P1.group.original, .~. + configuration:session)
P1.session.group.original <- update(P1.config.session, .~. + session:group.original)
P1.config.group.original <- update(P1.session.group.original, .~. + 
                                     configuration:group.original)
P1.lme <- update(P1.config.group.original, .~. + configuration:session:group.original)
anova(P1.baseline, P1.config, P1.session, P1.group.original, 
      P1.config.session, P1.session.group.original, P1.config.group.original, 
      P1.lme)

summary(P1.lme)

# 4.2.3. Line plot
P1.line <- ggplot(rep.data.long2, aes(x = group.original, y = P1, 
                                      colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "P1 mean amplitude", x = "session", 
       y = "P1 mean amplitude", colour = "configuration")
P1.line

#================================== 4.3. N1===================================#

# 4.3.1. Scatterplot
dot.colors <- ifelse(rep.data.long2[rep.data.long2$session == 2 & 
                                      group.original == "unaware",]$configuration == 
                       "rand", "red", "blue")
plot(rep.data.long2[rep.data.long2$session == 2 & 
                      group.original == "unaware",]$Subject, 
     rep.data.long2[rep.data.long2$session == 2 & 
                      group.original == "unaware",]$N1, 
     ylab = "N1 amplitude", 
     xlab = "subjects", pch = 20, col = dot.colors, 
     main = "N1 amplitudes by configuration")
abline(h=mean(rep.data.long2[rep.data.long2$session == 2 & 
                               rep.data.long2$configuration == "rand" &
                               group.original == "unaware",]$N1), 
       col = "red")
abline(h=mean(rep.data.long2[rep.data.long2$session == 2 & 
                               rep.data.long2$configuration == "sqr" &
                               group.original == "unaware",]$N1), 
       col = "blue")

# 4.3.2. ANOVA
N1.baseline <- lme(N1 ~ 1, random = ~1|Subject/configuration/session, 
                           data = rep.data.long2, method = "ML") #baseline
N1.config <- update(N1.baseline, .~. + configuration)
N1.session <- update(N1.config, .~. + session)
N1.group.original <- update(N1.session, .~. + group.original)
N1.config.session <- update(N1.group.original, .~. + configuration:session)
N1.session.group.original <- update(N1.config.session, .~. + session:group.original)
N1.config.group.original <- update(N1.session.group.original, .~. + 
                                     configuration:group.original)
N1.lme <- update(N1.config.group.original, .~. + configuration:session:group.original)
anova(N1.baseline, N1.config, N1.session, N1.group.original, 
      N1.config.session, N1.session.group.original, N1.config.group.original, 
      N1.lme)

summary(N1.lme)

# 4.3.3. Line plot
N1.line <- ggplot(rep.data.long2, aes(x = group.original, y = N1, 
                                      colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "N1 mean amplitude", x = " session", y = "N1 mean amplitude", 
       colour = "configuration")
N1.line

#================================== 4.4. Nd1===================================#

# 4.4.1. Scatterplot
dot.colors <- ifelse(rep.data.long2[rep.data.long2$session == 1 & 
                                      group.original == "aware",]$configuration == 
                       "rand", "red", "blue")
plot(rep.data.long2[rep.data.long2$session == 1 & 
                      group.original == "aware",]$Subject, 
     rep.data.long2[rep.data.long2$session == 1 & 
                      group.original == "aware",]$occ.nd1, 
     ylab = "occ nd1 amplitude", 
     xlab = "subjects", pch = 20, col = dot.colors, 
     main = "occ nd1 amplitudes by configuration")
abline(h=mean(rep.data.long2[rep.data.long2$session == 1 & 
                               rep.data.long2$configuration == "rand" &
                               group.original == "aware",]$occ.nd1), 
       col = "red")
abline(h=mean(rep.data.long2[rep.data.long2$session == 1 & 
                               rep.data.long2$configuration == "sqr" &
                               group.original == "aware",]$occ.nd1), 
       col = "blue")

# 4.4.2. ANOVA
nd1.baseline <- lme(occ.nd1 ~ 1, random = ~1|Subject/configuration/session, 
                    data = rep.data.long2, method = "ML") #baseline
nd1.config <- update(nd1.baseline, .~. + configuration)
nd1.session <- update(nd1.config, .~. + session)
nd1.group.original <- update(nd1.session, .~. + group.original)
nd1.config.session <- update(nd1.group.original, .~. + configuration:session)
nd1.session.group.original <- update(nd1.config.session, .~. + 
                                       session:group.original)
nd1.config.group.original <- update(nd1.session.group.original, .~. 
                                    + configuration:group.original)
nd1.lme <- update(nd1.config.group.original, .~. + 
                    configuration:session:group.original)
anova(nd1.baseline, nd1.config, nd1.session, nd1.group.original, nd1.config.session,
      nd1.session.group.original, nd1.config.group.original, nd1.lme)
####################################################################################
# nd1.baseline <- lme(occ.nd1 ~ 1, random = ~1|Subject/configuration/session, 
#                     data = rep.data.long2, method = "ML") #baseline
# nd1.group <- update(nd1.baseline, .~. + group.original)
# nd1.config <- update(nd1.group, .~. + configuration)
# nd1.session <- update(nd1.config, .~. + session)
# nd1.group.config <- update(nd1.session, .~. + group.original:configuration)
# nd1.group.session <- update(nd1.group.config, .~. + 
#                                        group:session)
# nd1.config.session <- update(nd1.group.session, .~. 
#                                     + configuration:session)
# nd1.lme <- update(nd1.config.session, .~. + 
#                     group.original:configuration:session)
# anova(nd1.baseline, nd1.config, nd1.session, nd1.group.original, nd1.config.session,
#       nd1.session.group.original, nd1.config.group.original, nd1.lme)
##############################################################################

summary(nd1.lme)

# 4.4.3. Line plot
#jpeg(file = "./Plots/Pitts ROIs/nd1line.jpeg")
nd1.line <- ggplot(rep.data.long2, 
                   aes(x = group.original, y = occ.nd1, 
                                       colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "Occ Nd1 mean amplitude by session", x = "group", 
       y = "Mean amplitude", 
       colour = "configuration")
nd1.line
#dev.off()

#===============================4.5. Nd2 (VAN) left==============================#

# 4.5.1. Scatterplot by configuration
dot.colors <- ifelse(rep.data.long2[rep.data.long2$session == 1 & 
                                      group.original == "unaware",]$configuration == 
                       "rand", "red", "blue")
plot(rep.data.long2[rep.data.long2$session == 1 & 
                      group.original == "unaware",]$Subject, 
     rep.data.long2[rep.data.long2$session == 1 & 
                      group.original == "unaware",]$left.nd2, 
     ylab = "left nd2 amplitude", 
     xlab = "subjects", pch = 20, col = dot.colors, 
     main = "left Nd2 amplitudes by configuration")
abline(h=mean(rep.data.long2[rep.data.long2$session == 1 & 
                               rep.data.long2$configuration == "rand" &
                               group.original == "unaware",]$left.nd2), 
       col = "red")
abline(h=mean(rep.data.long2[rep.data.long2$session == 1 & 
                               rep.data.long2$configuration == "sqr" &
                               group.original == "unaware",]$left.nd2), 
       col = "blue")

# 4.5.2. ANOVA
nd2.left.baseline <- lme(left.nd2 ~ 1, random = ~1|Subject/configuration/session, 
                         data = rep.data.long2, method = "ML") #baseline
nd2.left.config <- update(nd2.left.baseline, .~. + configuration)
nd2.left.session <- update(nd2.left.config, .~. + session)
nd2.left.group.original <- update(nd2.left.session, .~. + group.original)
nd2.left.config.session <- update(nd2.left.group.original, .~. + 
                                    configuration:session)
nd2.left.session.group.original <- update(nd2.left.config.session, .~. + 
                                            session:group.original)
nd2.left.config.group.original <- update(nd2.left.session.group.original, .~. + 
                                           configuration:group.original)
nd2.left.lme <- update(nd2.left.config.group.original, .~. + 
                         configuration:session:group.original)
anova(nd2.left.baseline, nd2.left.config, nd2.left.session, nd2.left.group.original, 
      nd2.left.config.session, nd2.left.session.group.original, nd2.left.config.group.original, 
      nd2.left.lme)

summary(nd2.left.lme)

# 4.5.3. Line plot
left.nd2.line <- ggplot(rep.data.long2,
                        aes(x = group.original, y = left.nd2, 
                                       colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "Left nd2 mean amplitude by session", x = "group", 
       y = "Mean amplitude", 
       colour = "configuration")
left.nd2.line

#==============================4.6. Nd2 (VAN) right=============================#

# 4.6.1. Scatterplot by configuration
dot.colors <- ifelse(rep.data.long2[rep.data.long2$session == 1 & 
                                      group.original == "aware",]$configuration == 
                       "rand", "red", "blue")
plot(rep.data.long2[rep.data.long2$session == 1 & 
                      group.original == "aware",]$Subject, 
     rep.data.long2[rep.data.long2$session == 1 & 
                      group.original == "aware",]$right.nd2, 
     ylab = "right nd2 amplitude", 
     xlab = "subjects", pch = 20, col = dot.colors, 
     main = "right Nd2 amplitudes by configuration")
abline(h=mean(rep.data.long2[rep.data.long2$session == 1 & 
                               rep.data.long2$configuration == "rand" &
                               group.original == "aware",]$right.nd2), 
       col = "red")
abline(h=mean(rep.data.long2[rep.data.long2$session == 1 & 
                               rep.data.long2$configuration == "sqr" &
                               group.original == "aware",]$right.nd2), 
       col = "blue")

# 4.6.2. ANOVA 
nd2.right.baseline <- lme(right.nd2 ~ 1, random = ~1|Subject/configuration/session, 
                          data = rep.data.long2, method = "ML") #baseline
nd2.right.config <- update(nd2.right.baseline, .~. + configuration)
nd2.right.session <- update(nd2.right.config, .~. + session)
nd2.right.group.original <- update(nd2.right.session, .~. + group.original)
nd2.right.config.session <- update(nd2.right.group.original, .~. + 
                                     configuration:session)
nd2.right.session.group.original <- update(nd2.right.config.session, .~. + 
                                             session:group.original)
nd2.right.config.group.original <- update(nd2.right.session.group.original, .~. + 
                                            configuration:group.original)
nd2.right.lme <- update(nd2.right.config.group.original, .~. + 
                          configuration:session:group.original)
anova(nd2.right.baseline, nd2.right.config, nd2.right.session, 
      nd2.right.group.original, 
      nd2.right.config.session, nd2.right.session.group.original, 
      nd2.right.config.group.original, 
      nd2.right.lme)

summary(nd2.right.lme)

# 4.6.3. Test normality of residuals
qqnorm(nd2.right.lme, grid = T, ~ranef(., level = 2))
hist((resid(nd2.right.lme) - mean(resid(nd2.right.lme))) / sd(resid(nd2.right.lme)), 
     freq = FALSE); curve(dnorm, add = TRUE)
plot.lme(nd2.right.lme)

# 4.6.4. Line plot
right.nd2.line <- ggplot(rep.data.long2,
                        aes(x = group.original, y = right.nd2, 
                            colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "right nd2 mean amplitude by group", x = "group", 
       y = "Mean amplitude", 
       colour = "configuration")
right.nd2.line

 
#===========================4.7. Nd2 (VAN) right-left==========================#

# 4.7.2. ANOVA
RL.nd2.baseline <- lme(RL.nd2 ~ 1, random = ~1|Subject/configuration/session, 
                          data = rep.data.long2, method = "ML") #baseline
RL.nd2.config <- update(RL.nd2.baseline, .~. + configuration)
RL.nd2.session <- update(RL.nd2.config, .~. + session)
RL.nd2.group.original <- update(RL.nd2.session, .~. + group.original)
RL.nd2.config.session <- update(RL.nd2.group.original, .~. + 
                                     configuration:session)
RL.nd2.session.group.original <- update(RL.nd2.config.session, .~. + 
                                             session:group.original)
RL.nd2.config.group.original <- update(RL.nd2.session.group.original, .~. + 
                                            configuration:group.original)
RL.nd2.lme <- update(RL.nd2.config.group.original, .~. + 
                          configuration:session:group.original)
anova(RL.nd2.baseline, RL.nd2.config, RL.nd2.session, 
      RL.nd2.group.original, 
      RL.nd2.config.session, RL.nd2.session.group.original, 
      RL.nd2.config.group.original, 
      RL.nd2.lme)

summary(nd2.right.lme)

#=====================================4.8. N2=====================================#

# 4.8.1 Scatterplot
dot.colors <- ifelse(rep.data.long2[rep.data.long2$session == 1 & 
                                      group.original == "aware",]$configuration == 
                       "rand", "red", "blue")
plot(rep.data.long2[rep.data.long2$session == 1 & 
                      group.original == "aware",]$Subject, 
     rep.data.long2[rep.data.long2$session == 1 & 
                      group.original == "aware",]$N2, 
     ylab = "N2 amplitude", 
     xlab = "subjects", pch = 20, col = dot.colors, 
     main = "N2 amplitudes by configuration")
abline(h=mean(rep.data.long2[rep.data.long2$session == 1 & 
                               rep.data.long2$configuration == "rand" &
                               group.original == "aware",]$N2), 
       col = "red")
abline(h=mean(rep.data.long2[rep.data.long2$session == 1 & 
                               rep.data.long2$configuration == "sqr" &
                               group.original == "aware",]$N2), 
       col = "blue")

# 4.8.2. ANOVA
N2.baseline <- lme(N2 ~ 1, random = ~1|Subject/configuration/session, 
                           data = rep.data.long2, method = "ML") #baseline
N2.config <- update(N2.baseline, .~. + configuration)
N2.session <- update(N2.config, .~. + session)
N2.group <- update(N2.session, .~. + group)
N2.config_session <- update(N2.group, .~. + configuration:session)
N2.session_group <- update(N2.config_session, .~. + session:group)
N2.config_group <- update(N2.session_group, .~. + configuration:group)
N2.lme <- update(N2.config_group, .~. + configuration:session:group)
anova(N2.baseline, N2.config, N2.session, N2.group, 
      N2.config_session, N2.session_group, N2.config_group, 
      N2.lme)
summary(N2.lme)

# 4.8.3. Line plot
N2.line <- ggplot(rep.data.long2[rep.data.long2$session == 1,], aes(x = group.original, y = N2, 
                                                        colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "N2 average reference", x = "group", y = "N2 amplitude", 
       colour = "configuration")
N2.line

#=====================================4.9. LP=====================================#

# 4.9.1. Scatterplot
dot.colors <- ifelse(rep.data.long2[rep.data.long2$session == 1 & 
                                      group.original == "aware",]$configuration == 
                       "rand", "red", "blue")
plot(rep.data.long2[rep.data.long2$session == 1 & 
                      group.original == "aware",]$Subject, 
     rep.data.long2[rep.data.long2$session == 1 & 
                      group.original == "aware",]$LP, 
     ylab = "LP amplitude", 
     xlab = "subjects", pch = 20, col = dot.colors, 
     main = "LP amplitudes by configuration")
abline(h=mean(rep.data.long2[rep.data.long2$session == 1 & 
                               rep.data.long2$configuration == "rand" &
                               group.original == "aware",]$LP), 
       col = "red")
abline(h=mean(rep.data.long2[rep.data.long2$session == 1 & 
                               rep.data.long2$configuration == "sqr" &
                               group.original == "aware",]$LP), 
       col = "blue")

# 4.9.2. ANOVA
LP.baseline <- lme(LP ~ 1, random = ~1|Subject/configuration/session, 
                               data = rep.data.long2, method = "ML") #baseline
LP.config <- update(LP.baseline, .~. + configuration)
LP.session <- update(LP.config, .~. + session)
LP.group.original <- update(LP.session, .~. + group.original)
LP.config.session <- update(LP.group.original, .~. + configuration:session)
LP.session.group.original <- update(LP.config.session, .~. + session:group.original)
LP.config.group.original <- update(LP.session.group.original, .~. + 
                                     configuration:group.original)
LP.lme <- update(LP.config.group.original, .~. + configuration:session:group.original)
anova(LP.baseline, LP.config, LP.session, LP.group.original, 
      LP.config.session, LP.session.group.original, LP.config.group.original, 
      LP.lme)

summary(LP_lme)

# 4.9.3. Line plot
LP.line <- ggplot(rep.data.long2, aes(x = group.original, y = LP, 
                                      colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "LP central", x = " group.original", y = "LP 
       amplitude", colour = "configuration")
LP.line

# Location test
nd2.lme <- lme(alpha ~ configuration * session * group.original * location, 
               random = ~1|Subject/configuration/location, 
               data = rep_data_long2, 
               method = "ML")
anova(nd2.lme)

#-----------------------------------Differences----------------------------------------
# occ diff
dot.colors <- ifelse(questionnaire.ERPs$group.original 
                     == "unaware", "purple", "orange")
layout(rbind(1,2), heights=c(5,1))
plot(x = questionnaire.ERPs$Subject,
     y = questionnaire.ERPs$occ.diff.1,
     col = dot.colors, pch = 16, main = "occ Nd1 sqr - rand",
     xlab = "Subject", ylab = "Mean amplitude")
# points(x = rep.data.long2[rep.data.long2$configuration == "sqr",]$log.alpha,
#        y = rep.data.long2[rep.data.long2$configuration == "sqr",]$occ.nd2,
#        col = " black", pch = 16)
# abline(lm(rep.data.long2[rep.data.long2$configuration == "rand",]$occ.nd2 ~
#             rep.data.long2[rep.data.long2$configuration == "rand",]$log.alpha),
#        col = "red")
# abline(lm(rep.data.long2[rep.data.long2$configuration == "sqr",]$occ.nd2 ~
#             rep.data.long2[rep.data.long2$configuration == "sqr",]$log.alpha),
#        col = 'black')
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = c("rand", "sqr"),
       col = c("red", "black"),
       pch = 16)
par(defaults)

# left diff
dot.colors <- ifelse(questionnaire.ERPs$group.original 
                     == "unaware", "purple", "orange")
layout(rbind(1,2), heights=c(5,1))
plot(x = questionnaire.ERPs$Subject,
     y = questionnaire.ERPs$left.diff.2,
     col = dot.colors, pch = 16, main = "left Nd2 sqr - rand",
     xlab = "Subject", ylab = "Mean amplitude")
# points(x = rep.data.long2[rep.data.long2$configuration == "sqr",]$log.alpha,
#        y = rep.data.long2[rep.data.long2$configuration == "sqr",]$left.nd2,
#        col = " black", pch = 16)
# abline(lm(rep.data.long2[rep.data.long2$configuration == "rand",]$left.nd2 ~
#             rep.data.long2[rep.data.long2$configuration == "rand",]$log.alpha),
#        col = "red")
# abline(lm(rep.data.long2[rep.data.long2$configuration == "sqr",]$left.nd2 ~
#             rep.data.long2[rep.data.long2$configuration == "sqr",]$log.alpha),
#        col = 'black')
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = c("rand", "sqr"),
       col = c("red", "black"),
       pch = 16)
par(defaults)

# Right diff
dot.colors <- ifelse(questionnaire.ERPs$group.original 
                     == "unaware", "purple", "orange")
layout(rbind(1,2), heights=c(5,1))
plot(x = questionnaire.ERPs$Subject,
     y = questionnaire.ERPs$right.diff.2,
     col = dot.colors, pch = 16, main = "Right Nd2 sqr - rand",
     xlab = "Subject", ylab = "Mean amplitude")
# points(x = rep.data.long2[rep.data.long2$configuration == "sqr",]$log.alpha,
#        y = rep.data.long2[rep.data.long2$configuration == "sqr",]$left.nd2,
#        col = " black", pch = 16)
# abline(lm(rep.data.long2[rep.data.long2$configuration == "rand",]$left.nd2 ~
#             rep.data.long2[rep.data.long2$configuration == "rand",]$log.alpha),
#        col = "red")
# abline(lm(rep.data.long2[rep.data.long2$configuration == "sqr",]$left.nd2 ~
#             rep.data.long2[rep.data.long2$configuration == "sqr",]$log.alpha),
#        col = 'black')
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = c("rand", "sqr"),
       col = c("red", "black"),
       pch = 16)
par(defaults)


#==============================================================================#
#===================================Ez ANOVA===================================#
#==============================================================================#

library(ez)

contrasts(rep.data.long2$configuration) <- c(-1, 1) # contrasts for config
contrasts(rep.data.long2$session) <- c(-1, 1) # contrasts for session
contrasts(rep.data.long2$group.original) <- c(-1, 1) # contrasts for group.original
contrasts(rep.data.long2$group) <- c(-1, 1) # contrasts for group

ez_Model_nd1 <- ezANOVA(data = rep.data.long2, dv = occ.nd1, wid = Subject, within = .(configuration, session), 
                    between = group.original, type = 3, detailed = TRUE)


ez_Model_nd2_left <- ezANOVA(data = rep.data.long2, dv = left.nd2, wid = Subject, within = .(configuration, session), 
                    between = group.original, type = 3, detailed = TRUE)

ez_Model_nd2_right <- ezANOVA(data = rep.data.long2, dv = right.nd2, wid = Subject, within = .(configuration, session), 
                             between = group.original, type = 3, detailed = TRUE)