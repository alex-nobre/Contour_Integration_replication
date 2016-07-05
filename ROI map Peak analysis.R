library(pastecs)
library(ggplot2)
library(nlme)
library(gridExtra)
library(outliers)
library(lattice)
library(car)
library(ez)
library(stat.desc)
library(effsize)


# 2. Subsets for testing (from long formatted data)
# 4.1. Create variable for means of ROI conditions (wide format)
# rep_data2$occ.means <- rowMeans(rep_data2[,c(2,5,8,11)])
# rep_data2$P1_occ_right.means <- rowMeans(rep_data2[,c(3,6,9,12)])
# rep_data2$right.means <- rowMeans(rep_data2[,c(4,7,10,13)])
# 4.2. Subset configuration x session
ROIpeak_sqr_config <- subset(ROIpeak_data_long, configuration == "sqr")
ROIpeak_rand_config <- subset(ROIpeak_data_long, configuration == "rand")
ROIpeak_session_1 <- subset(ROIpeak_data_long, session == 1)
ROIpeak_session_2 <- subset(ROIpeak_data_long, session == 2)

# 3. Compute variances and means)
by(ROIpeak_data_long[,6:12], list(ROIpeak_data_long$configuration,
                                 ROIpeak_data_long$session, ROIpeak_data_long$group.original),
   stat.desc, basic = FALSE)

# 4. Normality tests
# Create list of variables
nd2.sqr.list <- list(ROIpeak_sqr_config$P1_occ_right, ROIpeak_sqr_config$N1_occ, 
                     ROIpeak_sqr_config$P2_right, ROIpeak_sqr_config$P2_anterior,
                     ROIpeak_sqr_config$N2_occ, ROIpeak_sqr_config$LP_left, 
                     ROIpeak_sqr_config$LP_central)
nd2.rand.list <- list(ROIpeak_rand_config$P1_occ_right, ROIpeak_rand_config$N1_occ, 
                      ROIpeak_rand_config$P2_right, ROIpeak_rand_config$P2_anterior,
                      ROIpeak_rand_config$N2_occ, ROIpeak_rand_config$LP_left, 
                      ROIpeak_rand_config$LP_central)
nd2.list <- list(nd2.sqr.list, nd2.rand.list)

# shapiro-wilk tests
rapply(nd2.list, f = shapiro.test, how = "replace")

defaults <- par()
par(mfrow=c(2,4))
lapply(nd2.sqr.list, hist())
hist(ROIpeak_sqr_config$P1_occ_right)
hist(ROIpeak_sqr_config$N1_occ)
hist(ROIpeak_sqr_config$P2_anterior)
hist(ROIpeak_sqr_config$N2_occ)
hist(ROIpeak_sqr_config$LP_central)

par(defaults)
# 4.1. P1_occ_right
shapiro.test(ROIpeak_sqr_config$P1_occ_right)
shapiro.test(ROIpeak_rand_config$P1_occ_right)
qqnorm(ROIpeak_sqr_config$P1_occ_right, main = "Nd1 square configuration");qqline(
  ROIpeak_sqr_config$P1_occ_right, col = 4)
qqnorm(ROIpeak_rand_config$P1_occ_right, main = "Nd1 random configuration");qqline(
  ROIpeak_rand_config$P1_occ_right, col = 2)

# 4.2. Nd2 N1_occ
shapiro.test(ROIpeak_sqr_config$N1_occ)
shapiro.test(ROIpeak_rand_config$N1_occ)
qqnorm(ROIpeak_sqr_config$N1_occ, main = "Nd2 N1_occ square configuration");qqline(
  ROIpeak_sqr_config$N1_occ, col = 4)
qqnorm(ROIpeak_rand_config$N1_occ, main = "Nd2 N1_occ random configuration");qqline(
  ROIpeak_rand_config$N1_occ, col = 2)

# 4.3. Nd2 P2_right
shapiro.test(ROIpeak_sqr_config$P2_right)
shapiro.test(ROIpeak_rand_config$P2_right)
qqnorm(ROIpeak_sqr_config$P2_right, main = "Nd2 P2_right square configuration");qqline(
  ROIpeak_sqr_config$P2_right, col = 4)
qqnorm(ROIpeak_rand_config$P2_right, main = "Nd2 P2_right random configuration");qqline(
  ROIpeak_rand_config$P2_right, col = 2)

# 4.4. Nd2 P2_anterior
shapiro.test(ROIpeak_sqr_config$P2_anterior)
shapiro.test(ROIpeak_rand_config$P2_anterior)
qqnorm(ROIpeak_sqr_config$P2_anterior, main = "Nd2 P2_anterior square configuration");qqline(
  ROIpeak_sqr_config$P2_anterior, col = 4)
qqnorm(ROIpeak_rand_config$P2_anterior, main = "Nd2 P2_anterior random configuration");qqline(
  ROIpeak_rand_config$P2_anterior, col = 2)
describe(ROIpeak_data_long$P2_anterior)

# 4.5. Nd2 N2_occ
shapiro.test(ROIpeak_sqr_config$N2_occ)
shapiro.test(ROIpeak_rand_config$N2_occ)
qqnorm(ROIpeak_sqr_config$N2_occ, main = "Nd2 N2_occ square configuration");qqline(
  ROIpeak_sqr_config$N2_occ, col = 4)
qqnorm(ROIpeak_rand_config$N2_occ, main = "Nd2 N2_occ random configuration");qqline(
  ROIpeak_rand_config$N2_occ, col = 2)
describe(ROIpeak_data_long$N2_occ)

# 4.6. Nd2 LP_left
shapiro.test(ROIpeak_sqr_config$LP_left)
shapiro.test(ROIpeak_rand_config$LP_left)
qqnorm(ROIpeak_sqr_config$LP_left, main = "Nd2 LP_left square configuration");qqline(
  ROIpeak_sqr_config$LP_left, col = 4)
qqnorm(ROIpeak_rand_config$LP_left, main = "Nd2 LP_left random configuration");qqline(
  ROIpeak_rand_config$LP_left, col = 2)
describe(ROIpeak_data_long$LP_left)

# 4.7. Nd2 LP_central
shapiro.test(ROIpeak_sqr_config$LP_central)
shapiro.test(ROIpeak_rand_config$LP_central)
qqnorm(ROIpeak_sqr_config$LP_central, main = "Nd2 LP_central square configuration");qqline(
  ROIpeak_sqr_config$LP_central, col = 4)
qqnorm(ROIpeak_rand_config$LP_central, main = "Nd2 LP_central random configuration");qqline(
  ROIpeak_rand_config$LP_central, col = 2)
describe(ROIpeak_data_long$LP_central)

# 4. Plot variances and means
# 4.1. Scatterplot
# 4.1.1 P1_occ_right means x configuration
ROIpeak_sqr_dat$P1.means <- rowMeans(ROIpeak_sqr_dat[,c(3,12)])
ROIpeak_rand_dat$P1.means <- rowMeans(ROIpeak_rand_dat[,c(3,12)])
plot(ROIpeak_sqr_dat$subject, ROIpeak_sqr_dat$P1.means, ylab = "mean nd2 P1 amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "P1 amplitudes", ylim =c(-5.5, 3.0))
points(ROIpeak_rand_dat$subject, ROIpeak_rand_dat$P1.means, pch = 20, col = "blue")
abline(h=mean(ROIpeak_sqr_dat$P1.means), col = "red")
abline(h=mean(ROIpeak_rand_dat$P1.means), col = "blue")

# 4.1.2 N1_occ means x configuration
ROIpeak_sqr_dat$N1.means <- rowMeans(ROIpeak_sqr_dat[,c(4,13)])
ROIpeak_rand_dat$N1.means <- rowMeans(ROIpeak_rand_dat[,c(4,13)])
plot(ROIpeak_sqr_dat$subject, ROIpeak_sqr_dat$N1.means, ylab = "mean nd2 N1 amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "N1 amplitudes")
points(ROIpeak_rand_dat$subject, ROIpeak_rand_dat$N1.means, pch = 20, col = "blue")
abline(h=mean(ROIpeak_sqr_dat$N1.means), col = "red")
abline(h=mean(ROIpeak_rand_dat$N1.means), col = "blue")

# 4.1.3 P2_right means x configuration
ROIpeak_sqr_dat$P2_right.means <- rowMeans(ROIpeak_sqr_dat[,c(5,14)])
ROIpeak_rand_dat$P2_right.means <- rowMeans(ROIpeak_rand_dat[,c(5,14)])
plot(ROIpeak_sqr_dat$subject, ROIpeak_sqr_dat$P2_right.means, ylab = "mean nd2 P2_right amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "P2_right amplitudes")
points(ROIpeak_rand_dat$subject, ROIpeak_rand_dat$P2_right.means, pch = 20, col = "blue")
abline(h=mean(ROIpeak_sqr_dat$P2_right.means), col = "red")
abline(h=mean(ROIpeak_rand_dat$P2_right.means), col = "blue")

# 4.1.4 P2_anterior means x configuration
ROIpeak_sqr_dat$P2_anterior.means <- rowMeans(ROIpeak_sqr_dat[,c(6,15)])
ROIpeak_rand_dat$P2_anterior.means <- rowMeans(ROIpeak_rand_dat[,c(6,15)])
plot(ROIpeak_sqr_dat$subject, ROIpeak_sqr_dat$P2_anterior.means, ylab = "mean nd2 P2_anterior amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "P2_anterior amplitudes")
points(ROIpeak_rand_dat$subject, ROIpeak_rand_dat$P2_anterior.means, pch = 20, col = "blue")
abline(h=mean(ROIpeak_sqr_dat$P2_anterior.means), col = "red")
abline(h=mean(ROIpeak_rand_dat$P2_anterior.means), col = "blue")

# 4.1.5 N2_occ means x configuration
ROIpeak_sqr_dat$N2_occ.means <- rowMeans(ROIpeak_sqr_dat[,c(7,16)])
ROIpeak_rand_dat$N2_occ.means <- rowMeans(ROIpeak_rand_dat[,c(7,16)])
plot(ROIpeak_sqr_dat$subject, ROIpeak_sqr_dat$N2_occ.means, ylab = "mean nd2 N2_occ amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "N2_occ amplitudes")
points(ROIpeak_rand_dat$subject, ROIpeak_rand_dat$N2_occ.means, pch = 20, col = "blue")
abline(h=mean(ROIpeak_sqr_dat$N2_occ.means), col = "red")
abline(h=mean(ROIpeak_rand_dat$N2_occ.means), col = "blue")

# 4.1.6 LP_left means x configuration
ROIpeak_sqr_dat$LP_left.means <- rowMeans(ROIpeak_sqr_dat[,c(8,17)])
ROIpeak_rand_dat$LP_left.means <- rowMeans(ROIpeak_rand_dat[,c(8,17)])
plot(ROIpeak_sqr_dat$subject, ROIpeak_sqr_dat$LP_left.means, ylab = "mean nd2 LP_left amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "LP_left amplitudes")
points(ROIpeak_rand_dat$subject, ROIpeak_rand_dat$LP_left.means, pch = 20, col = "blue")
abline(h=mean(ROIpeak_sqr_dat$LP_left.means), col = "red")
abline(h=mean(ROIpeak_rand_dat$LP_left.means), col = "blue")

# 4.1.7 LP_anterior means x configuration
ROIpeak_sqr_dat$LP_anterior.means <- rowMeans(ROIpeak_sqr_dat[,c(9,18)])
ROIpeak_rand_dat$LP_anterior.means <- rowMeans(ROIpeak_rand_dat[,c(9,18)])
plot(ROIpeak_sqr_dat$subject, ROIpeak_sqr_dat$LP_anterior.means, ylab = "mean nd2 LP_anterior amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "LP_anterior amplitudes")
points(ROIpeak_rand_dat$subject, ROIpeak_rand_dat$LP_anterior.means, pch = 20, col = "blue")
abline(h=mean(ROIpeak_sqr_dat$LP_anterior.means), col = "red")
abline(h=mean(ROIpeak_rand_dat$LP_anterior.means), col = "blue")

# 4.2.Multipanel plots with lines
xyplot(N1_occ ~ subject | group * session, groups = configuration, col = c("red", "blue"), 
       data = ROIpeak_data_long, auto.key = T, type = "smooth")
# panel = function(...){
#   panel.xyplot(...)
#   panel.loess(lwd=1,groups = configuration,...)
# })

# 5. Outlier detection and replacement
# 5.1. Identify outliers
# Nd1
occ_box <- ggplot(ROIpeak_data_long, aes(x = configuration, y = occ, fill = configuration)) +
  geom_boxplot() + facet_grid(session~group)
Boxplot(occ ~ configuration * session * group, data = ROIpeak_data_long, 
        colour = group) #car boxplot
bwplot(occ ~ configuration|session, groups = group, data = ROIpeak_data_long, 
       auto.key = T) #lattice boxplot
boxplot(ROIpeak_data_long$occ ~ ROIpeak_data_long$configuration * 
          ROIpeak_data_long$group) #base boxplot
occ_out <- boxplot(ROIpeak_data_long$occ, plot = FALSE)$out
ROIpeak_data_long[ROIpeak_data_long$occ %in% occ_out,]
# Nd2 P1_occ_right
P1_occ_right_box <- ggplot(ROIpeak_data_long, aes(x = configuration, y = P1_occ_right,
                                                 fill = configuration)) +
  geom_boxplot() + 
  facet_grid(session~group)
boxplot(ROIpeak_data_long$P1_occ_right ~ ROIpeak_data_long$configuration * ROIpeak_data_long$group * 
          ROIpeak_data_long$session)
P1_occ_right_out <- boxplot(ROIpeak_data_long$P1_occ_right ~ ROIpeak_data_long$configuration * ROIpeak_data_long$group * 
                              ROIpeak_data_long$session, plot = FALSE)$out
ROIpeak_data_long[ROIpeak_data_long$P1_occ_right %in% P1_occ_right_out,]
# Nd2 right
boxplot(ROIpeak_data_long$right ~ ROIpeak_data_long$configuration * ROIpeak_data_long$group * 
          ROIpeak_data_long$session)
right_out <- boxplot(ROIpeak_data_long$right ~ ROIpeak_data_long$configuration * ROIpeak_data_long$group * 
                       ROIpeak_data_long$session, plot = FALSE)$out
ROIpeak_data_long[ROIpeak_data_long$right %in% right_out,]
# 5.2. Replace outliers by the mean
ROIpeak_data_long$occ <- rm.outlier(ROIpeak_data_long$occ, fill = TRUE)
ROIpeak_data_long$P1_occ_right <- rm.outlier(ROIpeak_data_long$P1_occ_right, fill = TRUE)
ROIpeak_data_long$right <- rm.outlier(ROIpeak_data_long$right, fill = TRUE)

# 6. Line graphs with error bars
# 6.1 Nd2 P1_occ_right - session
nd2_P1_occ_right_line_session <- ggplot(ROIpeak_data_long, aes(x = group.original, 
                                                              y = P1_occ_right, 
                                                              colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "P1 average reference", x = "session", 
       y = "P1 amplitude", colour = "configuration")

# 6.2 Nd2 N1_occ - session
nd2_N1_occ_line_session <- ggplot(ROIpeak_data_long, aes(x = session, y = N1_occ, 
                                                        colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~group.original) +
  labs(title = "N1 average reference", x = " session", y = "N1 amplitude", 
       colour = "configuration")

# # 6.3 Nd2 P2_right - session
# nd2_P2_right_line_session <- ggplot(ROIpeak_data_long, aes(x = session, y = P2_right, 
#                                                         colour = configuration)) + 
#   stat_summary(fun.y = mean, geom = "point") + 
#   stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
#   stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
#   facet_grid(.~group.original) +
#   labs(title = "P2 right average reference", x = " session", y = "Nd2 P2 amplitude", 
#        colour = "configuration")

# 6.4 220-280 - session
nd2_P2_anterior_line_session <- ggplot(ROIpeak_data_long, aes(x = session, y = P2_anterior, 
                                                             colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~group.original) +
  labs(title = "P2 anterior average reference", x = " session", y = "Nd2 P2 amplitude", 
       colour = "configuration")

# 6.5. 280-360 - session
nd2_N2_occ_line_session <- ggplot(ROIpeak_data_long, aes(x = session, y = N2_occ, 
                                                        colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~group.original) +
  labs(title = "N2 average reference", x = " session", y = "Nd2 N2 amplitude", 
       colour = "configuration")

# # 6.6 Nd2 LP_left - session
# nd2_LP_left_line_session <- ggplot(ROIpeak_data_long, aes(x = session, y = LP_left, 
#                                                           colour = configuration)) + 
#   stat_summary(fun.y = mean, geom = "point") + 
#   stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
#   stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
#   facet_grid(.~group.original) +
#   labs(title = "LP left average reference", x = " session", y = "Nd2 N1 amplitude", 
#        colour = "configuration")

# 6.7 Nd2 LP_central - session
nd2_LP_central_line_session <- ggplot(ROIpeak_data_long, aes(x = session, y = LP_central, 
                                                            colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~group.original) +
  labs(title = "LP central average reference", x = " session", y = "LP 
       amplitude", colour = "configuration")

#--------------------------Linear model----------------------------
# 6.1. P1_occ_right
contrasts(ROIpeak_data_long$configuration) <- c(-1, 1) # set contrasts for config
contrasts(ROIpeak_data_long$session) <- c(-1, 1) # set contrasts for session
contrasts(ROIpeak_data_long$group.original) <- c(-1, 1) # set contrasts for group.original
nd2_P1_occ_right_baseline <- lme(P1_occ_right ~ 1, random = ~1|subject/configuration/session, 
                                 data = ROIpeak_data_long, method = "ML") #baseline
nd2_P1_occ_right_config <- update(nd2_P1_occ_right_baseline, .~. + configuration)
nd2_P1_occ_right_session <- update(nd2_P1_occ_right_config, .~. + session)
nd2_P1_occ_right_group.original <- update(nd2_P1_occ_right_session, .~. + group.original)
nd2_P1_occ_right_config_session <- update(nd2_P1_occ_right_group.original, .~. + configuration:session)
nd2_P1_occ_right_session_group.original <- update(nd2_P1_occ_right_config_session, .~. + session:group.original)
nd2_P1_occ_right_config_group.original <- update(nd2_P1_occ_right_session_group.original, .~. + configuration:group.original)
nd2_P1_occ_right_lme <- update(nd2_P1_occ_right_config_group.original, .~. + configuration:session:group.original)
anova(nd2_P1_occ_right_baseline, nd2_P1_occ_right_config, nd2_P1_occ_right_session, nd2_P1_occ_right_group.original, 
      nd2_P1_occ_right_config_session, nd2_P1_occ_right_session_group.original, nd2_P1_occ_right_config_group.original, 
      nd2_P1_occ_right_lme)
summary(nd2_P1_occ_right_lme)

# 6.2. N1_occ
contrasts(ROIpeak_data_long$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(ROIpeak_data_long$session) <- c(-1, 1) # setting contrasts for session
contrasts(ROIpeak_data_long$group.original) <- c(-1, 1) # setting contrasts for group.original
nd2_N1_occ_baseline <- lme(N1_occ ~ 1, random = ~1|subject/configuration/session, 
                           data = ROIpeak_data_long, method = "ML") #baseline
nd2_N1_occ_config <- update(nd2_N1_occ_baseline, .~. + configuration)
nd2_N1_occ_session <- update(nd2_N1_occ_config, .~. + session)
nd2_N1_occ_group.original <- update(nd2_N1_occ_session, .~. + group.original)
nd2_N1_occ_config_session <- update(nd2_N1_occ_group.original, .~. + configuration:session)
nd2_N1_occ_session_group.original <- update(nd2_N1_occ_config_session, .~. + session:group.original)
nd2_N1_occ_config_group.original <- update(nd2_N1_occ_session_group.original, .~. + configuration:group.original)
nd2_N1_occ_lme <- update(nd2_N1_occ_config_group.original, .~. + configuration:session:group.original)
anova(nd2_N1_occ_baseline, nd2_N1_occ_config, nd2_N1_occ_session, nd2_N1_occ_group.original, 
      nd2_N1_occ_config_session, nd2_N1_occ_session_group.original, nd2_N1_occ_config_group.original, 
      nd2_N1_occ_lme)
summary(nd2_N1_occ_lme)

# # 6.3. P2_right
# contrasts(ROIpeak_data_long$configuration) <- c(-1, 1) # setting contrasts for config
# contrasts(ROIpeak_data_long$session) <- c(-1, 1) # setting contrasts for session
# contrasts(ROIpeak_data_long$group.original) <- c(-1, 1) # setting contrasts for group.original
# nd2_P2_right_baseline <- lme(P2_right ~ 1, random = ~1|subject/configuration/session, 
#                            data = ROIpeak_data_long, method = "ML") #baseline
# nd2_P2_right_config <- update(nd2_P2_right_baseline, .~. + configuration)
# nd2_P2_right_session <- update(nd2_P2_right_config, .~. + session)
# nd2_P2_right_group.original <- update(nd2_P2_right_session, .~. + group.original)
# nd2_P2_right_config_session <- update(nd2_P2_right_group.original, .~. + configuration:session)
# nd2_P2_right_session_group.original <- update(nd2_P2_right_config_session, .~. + session:group.original)
# nd2_P2_right_config_group.original <- update(nd2_P2_right_session_group.original, .~. + configuration:group.original)
# nd2_P2_right_lme <- update(nd2_P2_right_config_group.original, .~. + configuration:session:group.original)
# anova(nd2_P2_right_baseline, nd2_P2_right_config, nd2_P2_right_session, nd2_P2_right_group.original, 
#       nd2_P2_right_config_session, nd2_P2_right_session_group.original, nd2_P2_right_config_group.original, 
#       nd2_P2_right_lme)
# summary(nd2_P2_right_lme)

# 6.4. P2_anterior
contrasts(ROIpeak_data_long$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(ROIpeak_data_long$session) <- c(-1, 1) # setting contrasts for session
contrasts(ROIpeak_data_long$group.original) <- c(-1, 1) # setting contrasts for group.original
nd2_P2_anterior_baseline <- lme(P2_anterior ~ 1, random = ~1|subject/configuration/session, 
                                data = ROIpeak_data_long, method = "ML") #baseline
nd2_P2_anterior_config <- update(nd2_P2_anterior_baseline, .~. + configuration)
nd2_P2_anterior_session <- update(nd2_P2_anterior_config, .~. + session)
nd2_P2_anterior_group.original <- update(nd2_P2_anterior_session, .~. + group.original)
nd2_P2_anterior_config_session <- update(nd2_P2_anterior_group.original, .~. + configuration:session)
nd2_P2_anterior_session_group.original <- update(nd2_P2_anterior_config_session, .~. + session:group.original)
nd2_P2_anterior_config_group.original <- update(nd2_P2_anterior_session_group.original, .~. + configuration:group.original)
nd2_P2_anterior_lme <- update(nd2_P2_anterior_config_group.original, .~. + configuration:session:group.original)
anova(nd2_P2_anterior_baseline, nd2_P2_anterior_config, nd2_P2_anterior_session, nd2_P2_anterior_group.original, 
      nd2_P2_anterior_config_session, nd2_P2_anterior_session_group.original, nd2_P2_anterior_config_group.original, 
      nd2_P2_anterior_lme)
summary(nd2_P2_anterior_lme)

# 6.5. N2_occ
contrasts(ROIpeak_data_long$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(ROIpeak_data_long$session) <- c(-1, 1) # setting contrasts for session
contrasts(ROIpeak_data_long$group.original) <- c(-1, 1) # setting contrasts for group.original
nd2_N2_occ_baseline <- lme(N2_occ ~ 1, random = ~1|subject/configuration/session, 
                           data = ROIpeak_data_long, method = "ML") #baseline
nd2_N2_occ_config <- update(nd2_N2_occ_baseline, .~. + configuration)
nd2_N2_occ_session <- update(nd2_N2_occ_config, .~. + session)
nd2_N2_occ_group.original <- update(nd2_N2_occ_session, .~. + group.original)
nd2_N2_occ_config_session <- update(nd2_N2_occ_group.original, .~. + configuration:session)
nd2_N2_occ_session_group.original <- update(nd2_N2_occ_config_session, .~. + session:group.original)
nd2_N2_occ_config_group.original <- update(nd2_N2_occ_session_group.original, .~. + configuration:group.original)
nd2_N2_occ_lme <- update(nd2_N2_occ_config_group.original, .~. + configuration:session:group.original)
anova(nd2_N2_occ_baseline, nd2_N2_occ_config, nd2_N2_occ_session, nd2_N2_occ_group.original, 
      nd2_N2_occ_config_session, nd2_N2_occ_session_group.original, nd2_N2_occ_config_group.original, 
      nd2_N2_occ_lme)
summary(nd2_N2_occ_lme)

# # 6.6. Nd2 LP_left
# contrasts(ROIpeak_data_long$configuration) <- c(-1, 1) # setting contrasts for config
# contrasts(ROIpeak_data_long$session) <- c(-1, 1) # setting contrasts for session
# contrasts(ROIpeak_data_long$group.original) <- c(-1, 1) # setting contrasts for group.original
# nd2_LP_left_baseline <- lme(LP_left ~ 1, random = ~1|subject/configuration/session, 
#                            data = ROIpeak_data_long, method = "ML") #baseline
# nd2_LP_left_config <- update(nd2_LP_left_baseline, .~. + configuration)
# nd2_LP_left_session <- update(nd2_LP_left_config, .~. + session)
# nd2_LP_left_group.original <- update(nd2_LP_left_session, .~. + group.original)
# nd2_LP_left_config_session <- update(nd2_LP_left_group.original, .~. + configuration:session)
# nd2_LP_left_session_group.original <- update(nd2_LP_left_config_session, .~. + session:group.original)
# nd2_LP_left_config_group.original <- update(nd2_LP_left_session_group.original, .~. + configuration:group.original)
# nd2_LP_left_lme <- update(nd2_LP_left_config_group.original, .~. + configuration:session:group.original)
# anova(nd2_LP_left_baseline, nd2_LP_left_config, nd2_LP_left_session, nd2_LP_left_group.original, 
#       nd2_LP_left_config_session, nd2_LP_left_session_group.original, nd2_LP_left_config_group.original, 
#       nd2_LP_left_lme)
# summary(nd2_LP_left_lme)

# 6.7. Nd2 LP_central
contrasts(ROIpeak_data_long$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(ROIpeak_data_long$session) <- c(-1, 1) # setting contrasts for session
contrasts(ROIpeak_data_long$group.original) <- c(-1, 1) # setting contrasts for group.original
nd2_LP_central_baseline <- lme(LP_central ~ 1, random = ~1|subject/configuration/session, 
                               data = ROIpeak_data_long, method = "ML") #baseline
nd2_LP_central_config <- update(nd2_LP_central_baseline, .~. + configuration)
nd2_LP_central_session <- update(nd2_LP_central_config, .~. + session)
nd2_LP_central_group.original <- update(nd2_LP_central_session, .~. + group.original)
nd2_LP_central_config_session <- update(nd2_LP_central_group.original, .~. + configuration:session)
nd2_LP_central_session_group.original <- update(nd2_LP_central_config_session, .~. + session:group.original)
nd2_LP_central_config_group.original <- update(nd2_LP_central_session_group.original, .~. + configuration:group.original)
nd2_LP_central_lme <- update(nd2_LP_central_config_group.original, .~. + configuration:session:group.original)
anova(nd2_LP_central_baseline, nd2_LP_central_config, nd2_LP_central_session, nd2_LP_central_group.original, 
      nd2_LP_central_config_session, nd2_LP_central_session_group.original, nd2_LP_central_config_group.original, 
      nd2_LP_central_lme)
summary(nd2_LP_central_lme)

# 7. Pairwise comparisons for each session and group separately
# Subset groups
ROIpeak_aware_subset <- subset(ROIpeak_data_long, group.original == "aware")
ROIpeak_sqr_aware_subset <- subset(ROIpeak_aware_subset, configuration == "sqr")
ROIpeak_sqr1_aware_subset <- subset(ROIpeak_sqr_aware_subset, session == 1)
ROIpeak_sqr2_aware_subset <- subset(ROIpeak_sqr_aware_subset, session == 2)
ROIpeak_rand_aware_subset <- subset(ROIpeak_rand_aware_subset, configuration == "rand")
ROIpeak_rand1_aware_subset <- subset(ROIpeak_rand_aware_subset, session == 1)
ROIpeak_rand2_aware_subset <- subset(ROIpeak_rand_aware_subset, session == 2)
ROIpeak_unaware_subset <- subset(ROIpeak_data_long, group.original == "unaware")
ROIpeak_sqr_unaware_subset <- subset(ROIpeak_unaware_subset, configuration == "sqr")
ROIpeak_sqr1_unaware_subset <- subset(ROIpeak_sqr_unaware_subset, session == 1)
ROIpeak_sqr2_unaware_subset <- subset(ROIpeak_sqr_unaware_subset, session == 2)
ROIpeak_rand_unaware_subset <- subset(ROIpeak_rand_unaware_subset, configuration == "rand")
ROIpeak_rand1_unaware_subset <- subset(ROIpeak_rand_unaware_subset, session == 1)
ROIpeak_rand2_unaware_subset <- subset(ROIpeak_rand_unaware_subset, session == 2)

# P1
t.test(ROIpeak_sqr1_aware_subset$P1_occ_right, ROIpeak_rand1_aware_subset$P1_occ_right, paired = TRUE)
t.test(ROIpeak_sqr2_aware_subset$P1_occ_right, ROIpeak_rand2_aware_subset$P1_occ_right, paired = TRUE)
t.test(ROIpeak_sqr1_unaware_subset$P1_occ_right, ROIpeak_rand1_unaware_subset$P1_occ_right, paired = TRUE)
t.test(ROIpeak_sqr2_unaware_subset$P1_occ_right, ROIpeak_rand2_unaware_subset$P1_occ_right, paired = TRUE)
# N1
t.test(ROIpeak_sqr1_aware_subset$N1_occ, ROIpeak_rand1_aware_subset$N1_occ, paired = TRUE)
t.test(ROIpeak_sqr2_aware_subset$N1_occ, ROIpeak_rand2_aware_subset$N1_occ, paired = TRUE)
t.test(ROIpeak_sqr1_unaware_subset$N1_occ, ROIpeak_rand1_unaware_subset$N1_occ, paired = TRUE)
t.test(ROIpeak_sqr2_unaware_subset$N1_occ, ROIpeak_rand2_unaware_subset$N1_occ, paired = TRUE)
# 220-280
t.test(ROIpeak_sqr1_aware_subset$P2_anterior, ROIpeak_rand1_aware_subset$P2_anterior, paired = TRUE)
t.test(ROIpeak_sqr2_aware_subset$P2_anterior, ROIpeak_rand2_aware_subset$P2_anterior, paired = TRUE)
t.test(ROIpeak_sqr1_unaware_subset$P2_anterior, ROIpeak_rand1_unaware_subset$P2_anterior, paired = TRUE)
t.test(ROIpeak_sqr2_unaware_subset$P2_anterior, ROIpeak_rand2_unaware_subset$P2_anterior, paired = TRUE)
# 280-360
t.test(ROIpeak_sqr1_aware_subset$N2_occ, ROIpeak_rand1_aware_subset$N2_occ, paired = TRUE)
t.test(ROIpeak_sqr2_aware_subset$N2_occ, ROIpeak_rand2_aware_subset$N2_occ, paired = TRUE)
t.test(ROIpeak_sqr1_unaware_subset$N2_occ, ROIpeak_rand1_unaware_subset$N2_occ, paired = TRUE)
t.test(ROIpeak_sqr2_unaware_subset$N2_occ, ROIpeak_rand2_unaware_subset$N2_occ, paired = TRUE)
# LP
t.test(ROIpeak_sqr1_aware_subset$LP_central, ROIpeak_rand1_aware_subset$LP_central, paired = TRUE)
t.test(ROIpeak_sqr2_aware_subset$LP_central, ROIpeak_rand2_aware_subset$LP_central, paired = TRUE)
t.test(ROIpeak_sqr1_unaware_subset$LP_central, ROIpeak_rand1_unaware_subset$LP_central, paired = TRUE)
t.test(ROIpeak_sqr2_unaware_subset$LP_central, ROIpeak_rand2_unaware_subset$LP_central, paired = TRUE)

# 8. Differences in occ between groups for session 1
rep_data_long6 <- subset(ROIpeak_data_long, configuration == "sqr")
rep_data_long7 <- subset(rep_data_long6, session == 1)
t.test(occ ~ group, data = rep_data_long7, paired = FALSE)

# 9. Differences in P1_occ_right & right between groups for session 1
t.test(P1_occ_right ~ group, data = rep_data_long7, paired = FALSE)
t.test(right ~ group, data = rep_data_long7, paired = FALSE)
