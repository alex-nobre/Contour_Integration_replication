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

#--------------------------------Prepare data-----------------------------------
# 1. Subsets
# 1.1. Create variable for means of ROI conditions (wide format)
rep_data2$occ.means <- rowMeans(rep_data2[,c(2,5,8,11)])
rep_data2$left.means <- rowMeans(rep_data2[,c(3,6,9,12)])
rep_data2$right.means <- rowMeans(rep_data2[,c(4,7,10,13)])
# 1.2. Subset configuration x session
sqr_config <- subset(rep_data_long2, configuration == "sqr")
rand_config <- subset(rep_data_long2, configuration == "rand")
session_1 <- subset(rep_data_long2, session == 1)
session_2 <- subset(rep_data_long2, session == 2)

# 1.3. Log transform
# mapply(min, )

#---------------------------Exploratory data analysis---------------------------
defaults <- par() #save graphical parameters defaults

# 2. Compute variances and means
by(rep_data_long2[,6:8], list(rep_data_long2$configuration, 
                              rep_data_long2$session, rep_data_long2$group.original), 
   stat.desc, basic = FALSE)

# 3. Normality tests
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


#----------------------------------Linear model---------------------------------
contrasts(rep.data.long2$configuration) <- c(-1, 1) # contrasts for config
contrasts(rep.data.long2$session) <- c(-1, 1) # contrasts for session
contrasts(rep.data.long2$group.original) <- c(-1, 1) # contrasts for group.original
contrasts(rep.data.long2$group) <- c(-1, 1) # contrasts for group

# 4.1. C1
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

# 4.2. P1
# 4.2.1. Scatterplot
ROImap_sqr_dat$P1.means <- rowMeans(ROImap_sqr_dat[,c(3,12)])
ROImap_rand_dat$P1.means <- rowMeans(ROImap_rand_dat[,c(3,12)])
plot(ROImap_sqr_dat$subject, ROImap_sqr_dat$P1.means, ylab = "mean nd2 P1 amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "P1 amplitudes", ylim =c(-5.5, 3.0))
points(ROImap_rand_dat$subject, ROImap_rand_dat$P1.means, pch = 20, col = "blue")
abline(h=mean(ROImap_sqr_dat$P1.means), col = "red")
abline(h=mean(ROImap_rand_dat$P1.means), col = "blue")

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

# 4.3. N1
# 4.3.1. Scatterplot
ROImap_sqr_dat$N1.means <- rowMeans(ROImap_sqr_dat[,c(4,13)])
ROImap_rand_dat$N1.means <- rowMeans(ROImap_rand_dat[,c(4,13)])
plot(ROImap_sqr_dat$subject, ROImap_sqr_dat$N1.means, ylab = "mean nd2 N1 amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "N1 amplitudes")
points(ROImap_rand_dat$subject, ROImap_rand_dat$N1.means, pch = 20, col = "blue")
abline(h=mean(ROImap_sqr_dat$N1.means), col = "red")
abline(h=mean(ROImap_rand_dat$N1.means), col = "blue")

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

# 4.4. Nd1
# 4.4.1. Scatterplot
ROImap_sqr_dat$P2_right.means <- rowMeans(ROImap_sqr_dat[,c(5,14)])
ROImap_rand_dat$P2_right.means <- rowMeans(ROImap_rand_dat[,c(5,14)])
plot(ROImap_sqr_dat$subject, ROImap_sqr_dat$P2_right.means, ylab = "mean nd2 P2_right amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "P2_right amplitudes")
points(ROImap_rand_dat$subject, ROImap_rand_dat$P2_right.means, pch = 20, col = "blue")
abline(h=mean(ROImap_sqr_dat$P2_right.means), col = "red")
abline(h=mean(ROImap_rand_dat$P2_right.means), col = "blue")

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

summary(nd1.lme)

# 4.4.3. Line plot
#jpeg(file = "./Plots/Pitts ROIs/nd1line.jpeg")
nd1.line <- ggplot(rep_data_long2, aes(x = group.original, y = occ.nd1, 
                                       colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "Occ Nd1 mean amplitude", x = "group", y = "Occ Nd1 mean amplitude", 
       colour = "configuration")
nd1.line
#dev.off()

# 4.5. Nd2 (VAN) left
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
nd2_left_line_session <- ggplot(rep_data_long2, aes(x = group, y = left, 
                                                    colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "Left average reference", x = "group", y = "Nd2 left amplitude", 
       colour = "configuration")

# 4.6. Nd2 right
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
nd2_right_line_session <- ggplot(rep_data_long2, aes(x = group, y = right, 
                                                     colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "Right average reference", x = "group", y = "Nd2 right amplitude", 
       colour = "configuration")

 
# 4.7. Nd2 right-left
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

# 4.8. N2
# 4.8.1 Scatterplot
ROImap_sqr_dat$N2_occ.means <- rowMeans(ROImap_sqr_dat[,c(7,16)])
ROImap_rand_dat$N2_occ.means <- rowMeans(ROImap_rand_dat[,c(7,16)])
plot(ROImap_sqr_dat$subject, ROImap_sqr_dat$N2_occ.means, ylab = "mean nd2 N2_occ amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "N2_occ amplitudes")
points(ROImap_rand_dat$subject, ROImap_rand_dat$N2_occ.means, pch = 20, col = "blue")
abline(h=mean(ROImap_sqr_dat$N2_occ.means), col = "red")
abline(h=mean(ROImap_rand_dat$N2_occ.means), col = "blue")

# 4.8.2. ANOVA
N2_baseline <- lme(N2_occ ~ 1, random = ~1|subject/configuration/session, 
                           data = ROImap_data_long, method = "ML") #baseline
N2_config <- update(N2_baseline, .~. + configuration)
N2_session <- update(N2_config, .~. + session)
N2_group <- update(N2_session, .~. + group)
N2_config_session <- update(N2_group, .~. + configuration:session)
N2_session_group <- update(N2_config_session, .~. + session:group)
N2_config_group <- update(N2_session_group, .~. + configuration:group)
N2_lme <- update(N2_config_group, .~. + configuration:session:group)
anova(N2_baseline, N2_config, N2_session, N2_group, 
      N2_config_session, N2_session_group, N2_config_group, 
      N2_lme)
summary(N2_lme)

# 4.8.3. Line plot
nd2_N2_occ_line_session <- ggplot(ROImap_data_long, aes(x = session, y = N2_occ, 
                                                        colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~group.original) +
  labs(title = "N2 average reference", x = " session", y = "Nd2 N2 amplitude", 
       colour = "configuration")

# 4.9. LP
# 4.9.1. Scatterplot
ROImap_sqr_dat$LP_anterior.means <- rowMeans(ROImap_sqr_dat[,c(9,18)])
ROImap_rand_dat$LP_anterior.means <- rowMeans(ROImap_rand_dat[,c(9,18)])
plot(ROImap_sqr_dat$subject, ROImap_sqr_dat$LP_anterior.means, ylab = "mean nd2 LP_anterior amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "LP_anterior amplitudes")
points(ROImap_rand_dat$subject, ROImap_rand_dat$LP_anterior.means, pch = 20, col = "blue")
abline(h=mean(ROImap_sqr_dat$LP_anterior.means), col = "red")
abline(h=mean(ROImap_rand_dat$LP_anterior.means), col = "blue")

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

# # 4.9.3. Line plot
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

