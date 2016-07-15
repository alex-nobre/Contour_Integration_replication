library(pastecs)
library(ggplot2)
library(nlme)
library(lme4)
library(gridExtra)
library(outliers)
library(lattice)
library(car)
library(effsize)

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
normal.tests <- mapply(normality.test, rep_data_long2[,6:18], 
                       colnames(rep_data_long2[,6:18]), 1:ncol(rep_data_long2[,6:18]))

# 3.3. Plot histograms
# 5 Histograms for normality checks
# 5.1. Nd1
hist(rep_data_long2$occ.nd1, col = 3, prob = T)
lines(density(rep_data_long2$occ), col = 2)
# 5.2. Nd2 left
par(mfrow = c(1,2))
hist(rep_data_long2$left.nd2, main = "left nd2 mean amplitude", ylim = c(0, 0.5),
     xlab = "amplitude", col = 2, prob = T)
lines(density(rep_data_long2$left.nd2), col = 4, lwd = 3)
# 5.3. Nd2 right
hist(rep_data_long2$right.nd2, main = "Right nd2 mean amplitude", ylim = c(0, 0.5),
     xlab = "amplitude", col = 3, prob = T)
lines(density(rep_data_long2$right.nd2), col = 9, lwd = 3)
par(mfrow = c(1,1))

# 4. Plot variances and means


# 4.1.1 P1 means x configuration
ROImap_sqr_dat$P1.means <- rowMeans(ROImap_sqr_dat[,c(3,12)])
ROImap_rand_dat$P1.means <- rowMeans(ROImap_rand_dat[,c(3,12)])
plot(ROImap_sqr_dat$subject, ROImap_sqr_dat$P1.means, ylab = "mean nd2 P1 amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "P1 amplitudes", ylim =c(-5.5, 3.0))
points(ROImap_rand_dat$subject, ROImap_rand_dat$P1.means, pch = 20, col = "blue")
abline(h=mean(ROImap_sqr_dat$P1.means), col = "red")
abline(h=mean(ROImap_rand_dat$P1.means), col = "blue")

# 4.1.2 N1 means x configuration
ROImap_sqr_dat$N1.means <- rowMeans(ROImap_sqr_dat[,c(4,13)])
ROImap_rand_dat$N1.means <- rowMeans(ROImap_rand_dat[,c(4,13)])
plot(ROImap_sqr_dat$subject, ROImap_sqr_dat$N1.means, ylab = "mean nd2 N1 amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "N1 amplitudes")
points(ROImap_rand_dat$subject, ROImap_rand_dat$N1.means, pch = 20, col = "blue")
abline(h=mean(ROImap_sqr_dat$N1.means), col = "red")
abline(h=mean(ROImap_rand_dat$N1.means), col = "blue")

# 4.1.3 Occ_nd1 means x configuration
ROImap_sqr_dat$P2_right.means <- rowMeans(ROImap_sqr_dat[,c(5,14)])
ROImap_rand_dat$P2_right.means <- rowMeans(ROImap_rand_dat[,c(5,14)])
plot(ROImap_sqr_dat$subject, ROImap_sqr_dat$P2_right.means, ylab = "mean nd2 P2_right amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "P2_right amplitudes")
points(ROImap_rand_dat$subject, ROImap_rand_dat$P2_right.means, pch = 20, col = "blue")
abline(h=mean(ROImap_sqr_dat$P2_right.means), col = "red")
abline(h=mean(ROImap_rand_dat$P2_right.means), col = "blue")

# 4.1.4 Occ_nd2 means x configuration
ROImap_sqr_dat$P2_anterior.means <- rowMeans(ROImap_sqr_dat[,c(6,15)])
ROImap_rand_dat$P2_anterior.means <- rowMeans(ROImap_rand_dat[,c(6,15)])
plot(ROImap_sqr_dat$subject, ROImap_sqr_dat$P2_anterior.means, ylab = "mean nd2 P2_anterior amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "P2_anterior amplitudes")
points(ROImap_rand_dat$subject, ROImap_rand_dat$P2_anterior.means, pch = 20, col = "blue")
abline(h=mean(ROImap_sqr_dat$P2_anterior.means), col = "red")
abline(h=mean(ROImap_rand_dat$P2_anterior.means), col = "blue")

# 4.1.5 N2 means x configuration
ROImap_sqr_dat$N2_occ.means <- rowMeans(ROImap_sqr_dat[,c(7,16)])
ROImap_rand_dat$N2_occ.means <- rowMeans(ROImap_rand_dat[,c(7,16)])
plot(ROImap_sqr_dat$subject, ROImap_sqr_dat$N2_occ.means, ylab = "mean nd2 N2_occ amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "N2_occ amplitudes")
points(ROImap_rand_dat$subject, ROImap_rand_dat$N2_occ.means, pch = 20, col = "blue")
abline(h=mean(ROImap_sqr_dat$N2_occ.means), col = "red")
abline(h=mean(ROImap_rand_dat$N2_occ.means), col = "blue")

# 4.1.7 LP means x configuration
ROImap_sqr_dat$LP_anterior.means <- rowMeans(ROImap_sqr_dat[,c(9,18)])
ROImap_rand_dat$LP_anterior.means <- rowMeans(ROImap_rand_dat[,c(9,18)])
plot(ROImap_sqr_dat$subject, ROImap_sqr_dat$LP_anterior.means, ylab = "mean nd2 LP_anterior amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "LP_anterior amplitudes")
points(ROImap_rand_dat$subject, ROImap_rand_dat$LP_anterior.means, pch = 20, col = "blue")
abline(h=mean(ROImap_sqr_dat$LP_anterior.means), col = "red")
abline(h=mean(ROImap_rand_dat$LP_anterior.means), col = "blue")

# 4.2.Multipanel plots with lines
xyplot(N1_occ ~ subject | group * session, groups = configuration, col = c("red", "blue"), 
       data = ROImap_data_long, auto.key = T, type = "smooth")
# panel = function(...){
#   panel.xyplot(...)
#   panel.loess(lwd=1,groups = configuration,...)
# })



# 7.3 Nd2 left - session
nd2_left_line_session <- ggplot(rep_data_long2, aes(x = group, y = left, 
                                                    colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "Left average reference", x = "group", y = "Nd2 left amplitude", 
       colour = "configuration")

# 7.5 Nd2 right - session
nd2_right_line_session <- ggplot(rep_data_long2, aes(x = group, y = right, 
                                                     colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "Right average reference", x = "group", y = "Nd2 right amplitude", 
       colour = "configuration")

# 6.5. 280-360 - session
nd2_N2_occ_line_session <- ggplot(ROImap_data_long, aes(x = session, y = N2_occ, 
                                                        colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~group.original) +
  labs(title = "N2 average reference", x = " session", y = "Nd2 N2 amplitude", 
       colour = "configuration")

# 6.7 Nd2 LP_central - session
nd2_LP_central_line_session <- ggplot(ROImap_data_long, aes(x = session, y = LP_central, 
                                                            colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~group.original) +
  labs(title = "LP central average reference", x = " session", y = "LP 
       amplitude", colour = "configuration")

#----------------------------------Linear model---------------------------------
# 8. Mixed model
ses1_subset <- subset(rep_data_long2, session == 1)
ses2_subset <- subset(rep_data_long2, session == 2)

contrasts(ses2_subset$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(ses2_subset$group) <- c(-1, 1) # setting contrasts for group
nd1_baseline <- lme(occ ~ 1, random = ~1|Subject/configuration, 
                    data = ses2_subset, method = "ML") #baseline
nd1_config <- update(nd1_baseline, .~. + configuration)
anova(nd1_baseline, nd1_config)
summary(nd1_lme)

# 8.1. C1
# Scatterplot
plot(1:18, rep_data4[rep_data4$group.original == "aware",]$C1.sqr_1, 
     ylab = "mean C1 amplitude", xlab = "subjects", pch = 20, col = "red", 
     main = "C1 amplitudes")
points(1:14, rep_data4[rep_data4$group.original == "unaware",]$C1.sqr_1, 
       pch = 20, col = "blue")
abline(h=mean(rep_data4[rep_data4$group.original == "aware",]$C1.sqr_1), 
       col = "red")
abline(h=mean(rep_data4[rep_data4$group.original == "unaware",]$C1.sqr_1), 
       col = "blue")
# Line plot
C1.line <- ggplot(rep_data_long2, aes(x = group.original, y = C1, 
                                      colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "C1 mean aplitude", x = "session", 
       y = "C1 mean amplitude", colour = "configuration")

# ANOVA
contrasts(rep_data_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long2$group) <- c(-1, 1) # setting contrasts for group
C1_baseline <- lme(C1 ~ 1, random = ~1|Subject/configuration/session, 
                    data = rep_data_long2, method = "ML") #baseline
C1_config <- update(C1_baseline, .~. + configuration)
C1_session <- update(C1_config, .~. + session)
C1_group <- update(C1_session, .~. + group)
C1_config_session <- update(C1_group, .~. + configuration:session)
C1_session_group <- update(C1_config_session, .~. + session:group)
C1_config_group <- update(C1_session_group, .~. + configuration:group)
C1_lme <- update(C1_config_group, .~. + configuration:session:group)
anova(C1_baseline, C1_config, C1_session, C1_group, C1_config_session,
      C1_session_group, C1_config_group, C1_lme)

# 8.2. P1
# Line plot
P1.line <- ggplot(rep_data_long2, aes(x = group.original, y = P1, 
                                        colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "P1 mean amplitude", x = "session", 
       y = "P1 mean amplitude", colour = "configuration")

# ANOVA
contrasts(ROImap_data_long$configuration) <- c(-1, 1) # set contrasts for config
contrasts(ROImap_data_long$session) <- c(-1, 1) # set contrasts for session
contrasts(ROImap_data_long$group) <- c(-1, 1) # set contrasts for group
P1_baseline <- lme(P1 ~ 1, random = ~1|subject/configuration/session, 
                                 data = ROImap_data_long, method = "ML") #baseline
P1_config <- update(P1_baseline, .~. + configuration)
P1_session <- update(P1_config, .~. + session)
P1_group <- update(P1_session, .~. + group)
P1_config_session <- update(P1_group, .~. + configuration:session)
P1_session_group <- update(P1_config_session, .~. + session:group)
P1_config_group <- update(P1_session_group, .~. + configuration:group)
P1_lme <- update(P1_config_group, .~. + configuration:session:group)
anova(P1_baseline, P1_config, P1_session, P1_group, 
      P1_config_session, P1_session_group, P1_config_group, 
      P1_lme)
summary(P1_lme)

# 8.3. N1
# Line plot
N1.line <- ggplot(rep_data_long2, aes(x = group.original, y = N1, 
                                      colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "N1 mean amplitude", x = " session", y = "N1 mean amplitude", 
       colour = "configuration")
# ANOVA
contrasts(ROImap_data_long$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(ROImap_data_long$session) <- c(-1, 1) # setting contrasts for session
contrasts(ROImap_data_long$group) <- c(-1, 1) # setting contrasts for group
N1_baseline <- lme(N1_occ ~ 1, random = ~1|subject/configuration/session, 
                           data = ROImap_data_long, method = "ML") #baseline
N1_config <- update(N1_baseline, .~. + configuration)
N1_session <- update(N1_config, .~. + session)
N1_group <- update(N1_session, .~. + group)
N1_config_session <- update(N1_group, .~. + configuration:session)
N1_session_group <- update(N1_config_session, .~. + session:group)
N1_config_group <- update(N1_session_group, .~. + configuration:group)
N1_lme <- update(N1_config_group, .~. + configuration:session:group)
anova(N1_baseline, N1_config, N1_session, N1_group, 
      N1_config_session, N1_session_group, N1_config_group, 
      N1_lme)
summary(N1_lme)

# 8.4. Nd1 (P2)
# Line plot
nd1.line <- ggplot(rep_data_long2, aes(x = group.original, y = occ.nd1, 
                                               colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "Occ Nd1 mean amplitude", x = "group", y = "Occ Nd1 mean amplitude", 
       colour = "configuration")
# ANOVA
contrasts(rep_data_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long2$group) <- c(-1, 1) # setting contrasts for group
nd1_baseline <- lme(occ ~ 1, random = ~1|Subject/configuration/session, 
                    data = rep_data_long2, method = "ML") #baseline
nd1_config <- update(nd1_baseline, .~. + configuration)
nd1_session <- update(nd1_config, .~. + session)
nd1_group <- update(nd1_session, .~. + group)
nd1_config_session <- update(nd1_group, .~. + configuration:session)
nd1_session_group <- update(nd1_config_session, .~. + session:group)
nd1_config_group <- update(nd1_session_group, .~. + configuration:group)
nd1_lme <- update(nd1_config_group, .~. + configuration:session:group)
anova(nd1_baseline, nd1_config, nd1_session, nd1_group, nd1_config_session,
      nd1_session_group, nd1_config_group, nd1_lme)

summary(nd1_lme)

# 8.5. Nd2 (VAN) left
contrasts(rep_data_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long2$group.original) <- c(-1, 1) # setting contrasts for group.original
nd2_left_baseline <- lme(left ~ 1, random = ~1|Subject/configuration/session, 
                         data = rep_data_long2, method = "ML") #baseline
nd2_left_config <- update(nd2_left_baseline, .~. + configuration)
nd2_left_session <- update(nd2_left_config, .~. + session)
nd2_left_group.original <- update(nd2_left_session, .~. + group.original)
nd2_left_config_session <- update(nd2_left_group.original, .~. + configuration:session)
nd2_left_session_group.original <- update(nd2_left_config_session, .~. + session:group.original)
nd2_left_config_group.original <- update(nd2_left_session_group.original, .~. + configuration:group.original)
nd2_left_lme <- update(nd2_left_config_group.original, .~. + configuration:session:group.original)
anova(nd2_left_baseline, nd2_left_config, nd2_left_session, nd2_left_group.original, 
      nd2_left_config_session, nd2_left_session_group.original, nd2_left_config_group.original, 
      nd2_left_lme)
summary(nd2_left_lme)

# 8.6. Nd2 (VAN) right
contrasts(rep_data_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long2$group.original) <- c(-1, 1) # setting contrasts for group.original
nd2_right_baseline <- lme(right ~ 1, random = ~1|Subject/configuration/session, 
                          data = rep_data_long2, method = "ML") #baseline
nd2_right_config <- update(nd2_right_baseline, .~. + configuration)
nd2_right_session <- update(nd2_right_config, .~. + session)
nd2_right_group.original <- update(nd2_right_session, .~. + group.original)
nd2_right_config_session <- update(nd2_right_group.original, .~. + configuration:session)
nd2_right_session_group.original <- update(nd2_right_config_session, .~. + session:group.original)
nd2_right_config_group.original <- update(nd2_right_session_group.original, .~. + configuration:group.original)
nd2_right_lme <- update(nd2_right_config_group.original, .~. + configuration:session:group.original)
anova(nd2_right_baseline, nd2_right_config, nd2_right_session, nd2_right_group.original, 
      nd2_right_config_session, nd2_right_session_group.original, nd2_right_config_group.original, 
      nd2_right_lme)
summary(nd2_right_lme)
t.test(C1 ~ configuration, data = rep_data_long2, paired = TRUE)
 
# 8.6. Nd2 (VAN) right-left
contrasts(rep_data_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long2$group.original) <- c(-1, 1) # setting contrasts for group.original
nd2_right_baseline <- lme(right ~ 1, random = ~1|Subject/configuration/session, 
                          data = rep_data_long2, method = "ML") #baseline
nd2_right_config <- update(nd2_right_baseline, .~. + configuration)
nd2_right_session <- update(nd2_right_config, .~. + session)
nd2_right_group.original <- update(nd2_right_session, .~. + group.original)
nd2_right_config_session <- update(nd2_right_group.original, .~. + configuration:session)
nd2_right_session_group.original <- update(nd2_right_config_session, .~. + session:group.original)
nd2_right_config_group.original <- update(nd2_right_session_group.original, .~. + configuration:group.original)
nd2_right_lme <- update(nd2_right_config_group.original, .~. + configuration:session:group.original)
anova(nd2_right_baseline, nd2_right_config, nd2_right_session, nd2_right_group.original, 
      nd2_right_config_session, nd2_right_session_group.original, nd2_right_config_group.original, 
      nd2_right_lme)
summary(nd2_right_lme)

# 8.7. N2
contrasts(ROImap_data_long$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(ROImap_data_long$session) <- c(-1, 1) # setting contrasts for session
contrasts(ROImap_data_long$group) <- c(-1, 1) # setting contrasts for group
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

# 8.8. LP
contrasts(ROImap_data_long$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(ROImap_data_long$session) <- c(-1, 1) # setting contrasts for session
contrasts(ROImap_data_long$group) <- c(-1, 1) # setting contrasts for group
LP_baseline <- lme(LP_central ~ 1, random = ~1|subject/configuration/session, 
                               data = ROImap_data_long, method = "ML") #baseline
LP_config <- update(LP_baseline, .~. + configuration)
LP_session <- update(LP_config, .~. + session)
LP_group <- update(LP_session, .~. + group)
LP_config_session <- update(LP_group, .~. + configuration:session)
LP_session_group <- update(LP_config_session, .~. + session:group)
LP_config_group <- update(LP_session_group, .~. + configuration:group)
LP_lme <- update(LP_config_group, .~. + configuration:session:group)
anova(LP_baseline, LP_config, LP_session, LP_group, 
      LP_config_session, LP_session_group, LP_config_group, 
      LP_lme)
summary(LP_lme)

#---------------------------------------MANOVA------------------------------------------


# 10. Assumption checking
par(mfrow = c(2,2))
plot(nd1_config)
qqnorm(nd1_config)
par <- par(defaults)

# 9. Pairwise comparisons for each session and group separately
# Subset groups
aware_subset <- subset(rep_data2, group == "aware")
unaware_subset <- subset(rep_data2, group == "unaware")
# Occ (Nd1)
t.test(aware_subset$occ.sqr_1, aware_subset$occ.rand_1, paired = TRUE)
t.test(aware_subset$occ.sqr_2, aware_subset$occ.rand_2, paired = TRUE)
t.test(unaware_subset$occ.sqr_1, unaware_subset$occ.rand_1, paired = TRUE)
t.test(unaware_subset$occ.sqr_2, unaware_subset$occ.rand_2, paired = TRUE)
# left (Nd2)
t.test(aware_subset$left.sqr_1, aware_subset$left.rand_1, paired = TRUE)
t.test(aware_subset$left.sqr_2, aware_subset$left.rand_2, paired = TRUE)
t.test(unaware_subset$left.sqr_1, unaware_subset$left.rand_1, paired = TRUE)
t.test(unaware_subset$left.sqr_2, unaware_subset$left.rand_2, paired = TRUE)
# right (Nd2)
t.test(aware_subset$right.sqr_1, aware_subset$right.rand_1, paired = TRUE)
t.test(aware_subset$right.sqr_2, aware_subset$right.rand_2, paired = TRUE)
t.test(unaware_subset$right.sqr_1, unaware_subset$right.rand_1, paired = TRUE)
t.test(unaware_subset$right.sqr_2, unaware_subset$right.rand_2, paired = TRUE)

# 9.2. Differences in occ between groups for session 1
rep_data_long6 <- subset(rep_data_long2, configuration == "sqr")
rep_data_long7 <- subset(rep_data_long6, session == 1)
t.test(occ ~ group, data = rep_data_long7, paired = FALSE)

# 9.3 Differences in left & right between groups for session 1
t.test(left ~ group, data = rep_data_long7, paired = FALSE)
t.test(right ~ group, data = rep_data_long7, paired = FALSE)

# O.o
rep_data_long8 <- subset(rep_data_long2, configuration == "rand")
rep_data_long9 <- subset(rep_data_long8, group == "aware")
t.test(occ ~ group, data = rep_data_long7, paired = FALSE)
t.test(left ~ session, data = rep_data_long9, paired = TRUE)
t.test(right ~ session, data = rep_data_long9, paired = TRUE)
