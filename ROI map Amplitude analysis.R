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
ROImap_sqr_config <- subset(ROImap_data_long, configuration == "sqr")
ROImap_rand_config <- subset(ROImap_data_long, configuration == "rand")
ROImap_session_1 <- subset(ROImap_data_long, session == 1)
ROImap_session_2 <- subset(ROImap_data_long, session == 2)

# 3. Compute variances and means)
by(ROImap_data_long[,6:12], list(ROImap_data_long$configuration,
                             ROImap_data_long$session, ROImap_data_long$group.original),
   stat.desc, basic = FALSE)

# 4. Normality tests
# Create list of variables
nd2.sqr.list <- list(ROImap_sqr_config$P1_occ_right, ROImap_sqr_config$N1_occ, 
                  ROImap_sqr_config$P2_right, ROImap_sqr_config$P2_anterior,
                  ROImap_sqr_config$N2_occ, ROImap_sqr_config$LP_left, 
                  ROImap_sqr_config$LP_central)
nd2.rand.list <- list(ROImap_rand_config$P1_occ_right, ROImap_rand_config$N1_occ, 
                     ROImap_rand_config$P2_right, ROImap_rand_config$P2_anterior,
                     ROImap_rand_config$N2_occ, ROImap_rand_config$LP_left, 
                     ROImap_rand_config$LP_central)
nd2.list <- list(nd2.sqr.list, nd2.rand.list)

# shapiro-wilk tests
rapply(nd2.list, f = shapiro.test, how = "replace")

defaults <- par()
par(mfrow=c(2,4))
lapply(nd2.sqr.list, hist())
hist(ROImap_sqr_config$P1_occ_right)
hist(ROImap_sqr_config$N1_occ)
hist(ROImap_sqr_config$P2_anterior)
hist(ROImap_sqr_config$N2_occ)
hist(ROImap_sqr_config$LP_central)

par(defaults)
# 4.1. P1_occ_right
shapiro.test(ROImap_sqr_config$P1_occ_right)
shapiro.test(ROImap_rand_config$P1_occ_right)
qqnorm(ROImap_sqr_config$P1_occ_right, main = "Nd1 square configuration");qqline(
  ROImap_sqr_config$P1_occ_right, col = 4)
qqnorm(ROImap_rand_config$P1_occ_right, main = "Nd1 random configuration");qqline(
  ROImap_rand_config$P1_occ_right, col = 2)

# 4.2. Nd2 N1_occ
shapiro.test(ROImap_sqr_config$N1_occ)
shapiro.test(ROImap_rand_config$N1_occ)
qqnorm(ROImap_sqr_config$N1_occ, main = "Nd2 N1_occ square configuration");qqline(
  ROImap_sqr_config$N1_occ, col = 4)
qqnorm(ROImap_rand_config$N1_occ, main = "Nd2 N1_occ random configuration");qqline(
  ROImap_rand_config$N1_occ, col = 2)

# 4.3. Nd2 P2_right
shapiro.test(ROImap_sqr_config$P2_right)
shapiro.test(ROImap_rand_config$P2_right)
qqnorm(ROImap_sqr_config$P2_right, main = "Nd2 P2_right square configuration");qqline(
  ROImap_sqr_config$P2_right, col = 4)
qqnorm(ROImap_rand_config$P2_right, main = "Nd2 P2_right random configuration");qqline(
  ROImap_rand_config$P2_right, col = 2)

# 4.4. Nd2 P2_anterior
shapiro.test(ROImap_sqr_config$P2_anterior)
shapiro.test(ROImap_rand_config$P2_anterior)
qqnorm(ROImap_sqr_config$P2_anterior, main = "Nd2 P2_anterior square configuration");qqline(
  ROImap_sqr_config$P2_anterior, col = 4)
qqnorm(ROImap_rand_config$P2_anterior, main = "Nd2 P2_anterior random configuration");qqline(
  ROImap_rand_config$P2_anterior, col = 2)
describe(ROImap_data_long$P2_anterior)

# 4.5. Nd2 N2_occ
shapiro.test(ROImap_sqr_config$N2_occ)
shapiro.test(ROImap_rand_config$N2_occ)
qqnorm(ROImap_sqr_config$N2_occ, main = "Nd2 N2_occ square configuration");qqline(
  ROImap_sqr_config$N2_occ, col = 4)
qqnorm(ROImap_rand_config$N2_occ, main = "Nd2 N2_occ random configuration");qqline(
  ROImap_rand_config$N2_occ, col = 2)
describe(ROImap_data_long$N2_occ)

# 4.6. Nd2 LP_left
shapiro.test(ROImap_sqr_config$LP_left)
shapiro.test(ROImap_rand_config$LP_left)
qqnorm(ROImap_sqr_config$LP_left, main = "Nd2 LP_left square configuration");qqline(
  ROImap_sqr_config$LP_left, col = 4)
qqnorm(ROImap_rand_config$LP_left, main = "Nd2 LP_left random configuration");qqline(
  ROImap_rand_config$LP_left, col = 2)
describe(ROImap_data_long$LP_left)

# 4.7. Nd2 LP_central
shapiro.test(ROImap_sqr_config$LP_central)
shapiro.test(ROImap_rand_config$LP_central)
qqnorm(ROImap_sqr_config$LP_central, main = "Nd2 LP_central square configuration");qqline(
  ROImap_sqr_config$LP_central, col = 4)
qqnorm(ROImap_rand_config$LP_central, main = "Nd2 LP_central random configuration");qqline(
  ROImap_rand_config$LP_central, col = 2)
describe(ROImap_data_long$LP_central)

# 4. Plot variances and means
# 4.1. Scatterplot
# 4.1.1 P1_occ_right means x configuration
ROImap_sqr_dat$P1.means <- rowMeans(ROImap_sqr_dat[,c(3,12)])
ROImap_rand_dat$P1.means <- rowMeans(ROImap_rand_dat[,c(3,12)])
plot(ROImap_sqr_dat$subject, ROImap_sqr_dat$P1.means, ylab = "mean nd2 P1 amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "P1 amplitudes", ylim =c(-5.5, 3.0))
points(ROImap_rand_dat$subject, ROImap_rand_dat$P1.means, pch = 20, col = "blue")
abline(h=mean(ROImap_sqr_dat$P1.means), col = "red")
abline(h=mean(ROImap_rand_dat$P1.means), col = "blue")

# 4.1.2 N1_occ means x configuration
ROImap_sqr_dat$N1.means <- rowMeans(ROImap_sqr_dat[,c(4,13)])
ROImap_rand_dat$N1.means <- rowMeans(ROImap_rand_dat[,c(4,13)])
plot(ROImap_sqr_dat$subject, ROImap_sqr_dat$N1.means, ylab = "mean nd2 N1 amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "N1 amplitudes")
points(ROImap_rand_dat$subject, ROImap_rand_dat$N1.means, pch = 20, col = "blue")
abline(h=mean(ROImap_sqr_dat$N1.means), col = "red")
abline(h=mean(ROImap_rand_dat$N1.means), col = "blue")

# 4.1.3 P2_right means x configuration
ROImap_sqr_dat$P2_right.means <- rowMeans(ROImap_sqr_dat[,c(5,14)])
ROImap_rand_dat$P2_right.means <- rowMeans(ROImap_rand_dat[,c(5,14)])
plot(ROImap_sqr_dat$subject, ROImap_sqr_dat$P2_right.means, ylab = "mean nd2 P2_right amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "P2_right amplitudes")
points(ROImap_rand_dat$subject, ROImap_rand_dat$P2_right.means, pch = 20, col = "blue")
abline(h=mean(ROImap_sqr_dat$P2_right.means), col = "red")
abline(h=mean(ROImap_rand_dat$P2_right.means), col = "blue")

# 4.1.4 P2_anterior means x configuration
ROImap_sqr_dat$P2_anterior.means <- rowMeans(ROImap_sqr_dat[,c(6,15)])
ROImap_rand_dat$P2_anterior.means <- rowMeans(ROImap_rand_dat[,c(6,15)])
plot(ROImap_sqr_dat$subject, ROImap_sqr_dat$P2_anterior.means, ylab = "mean nd2 P2_anterior amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "P2_anterior amplitudes")
points(ROImap_rand_dat$subject, ROImap_rand_dat$P2_anterior.means, pch = 20, col = "blue")
abline(h=mean(ROImap_sqr_dat$P2_anterior.means), col = "red")
abline(h=mean(ROImap_rand_dat$P2_anterior.means), col = "blue")

# 4.1.5 N2_occ means x configuration
ROImap_sqr_dat$N2_occ.means <- rowMeans(ROImap_sqr_dat[,c(7,16)])
ROImap_rand_dat$N2_occ.means <- rowMeans(ROImap_rand_dat[,c(7,16)])
plot(ROImap_sqr_dat$subject, ROImap_sqr_dat$N2_occ.means, ylab = "mean nd2 N2_occ amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "N2_occ amplitudes")
points(ROImap_rand_dat$subject, ROImap_rand_dat$N2_occ.means, pch = 20, col = "blue")
abline(h=mean(ROImap_sqr_dat$N2_occ.means), col = "red")
abline(h=mean(ROImap_rand_dat$N2_occ.means), col = "blue")

# 4.1.6 LP_left means x configuration
ROImap_sqr_dat$LP_left.means <- rowMeans(ROImap_sqr_dat[,c(8,17)])
ROImap_rand_dat$LP_left.means <- rowMeans(ROImap_rand_dat[,c(8,17)])
plot(ROImap_sqr_dat$subject, ROImap_sqr_dat$LP_left.means, ylab = "mean nd2 LP_left amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "LP_left amplitudes")
points(ROImap_rand_dat$subject, ROImap_rand_dat$LP_left.means, pch = 20, col = "blue")
abline(h=mean(ROImap_sqr_dat$LP_left.means), col = "red")
abline(h=mean(ROImap_rand_dat$LP_left.means), col = "blue")

# 4.1.7 LP_anterior means x configuration
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

# 5. Outlier detection and replacement
# 5.1. Identify outliers
# Nd1
occ_box <- ggplot(ROImap_data_long, aes(x = configuration, y = occ, fill = configuration)) +
  geom_boxplot() + facet_grid(session~group)
Boxplot(occ ~ configuration * session * group, data = ROImap_data_long, 
        colour = group) #car boxplot
bwplot(occ ~ configuration|session, groups = group, data = ROImap_data_long, 
       auto.key = T) #lattice boxplot
boxplot(ROImap_data_long$occ ~ ROImap_data_long$configuration * 
          ROImap_data_long$group) #base boxplot
occ_out <- boxplot(ROImap_data_long$occ, plot = FALSE)$out
ROImap_data_long[ROImap_data_long$occ %in% occ_out,]
# Nd2 P1_occ_right
P1_occ_right_box <- ggplot(ROImap_data_long, aes(x = configuration, y = P1_occ_right,
                                       fill = configuration)) +
  geom_boxplot() + 
  facet_grid(session~group)
boxplot(ROImap_data_long$P1_occ_right ~ ROImap_data_long$configuration * ROImap_data_long$group * 
          ROImap_data_long$session)
P1_occ_right_out <- boxplot(ROImap_data_long$P1_occ_right ~ ROImap_data_long$configuration * ROImap_data_long$group * 
                      ROImap_data_long$session, plot = FALSE)$out
ROImap_data_long[ROImap_data_long$P1_occ_right %in% P1_occ_right_out,]
# Nd2 right
boxplot(ROImap_data_long$right ~ ROImap_data_long$configuration * ROImap_data_long$group * 
          ROImap_data_long$session)
right_out <- boxplot(ROImap_data_long$right ~ ROImap_data_long$configuration * ROImap_data_long$group * 
                       ROImap_data_long$session, plot = FALSE)$out
ROImap_data_long[ROImap_data_long$right %in% right_out,]
# 5.2. Replace outliers by the mean
ROImap_data_long$occ <- rm.outlier(ROImap_data_long$occ, fill = TRUE)
ROImap_data_long$P1_occ_right <- rm.outlier(ROImap_data_long$P1_occ_right, fill = TRUE)
ROImap_data_long$right <- rm.outlier(ROImap_data_long$right, fill = TRUE)

# 6. Line graphs with error bars
# 6.1 Nd2 P1_occ_right - session
nd2_P1_occ_right_line_session <- ggplot(ROImap_data_long, aes(x = group.original, 
                                                              y = P1_occ_right, 
                                                    colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "P1 average reference", x = "session", 
       y = "P1 amplitude", colour = "configuration")

# 6.2 Nd2 N1_occ - session
nd2_N1_occ_line_session <- ggplot(ROImap_data_long, aes(x = session, y = N1_occ, 
                                                     colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~group.original) +
  labs(title = "N1 average reference", x = " session", y = "N1 amplitude", 
       colour = "configuration")

# # 6.3 Nd2 P2_right - session
# nd2_P2_right_line_session <- ggplot(ROImap_data_long, aes(x = session, y = P2_right, 
#                                                         colour = configuration)) + 
#   stat_summary(fun.y = mean, geom = "point") + 
#   stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
#   stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
#   facet_grid(.~group.original) +
#   labs(title = "P2 right average reference", x = " session", y = "Nd2 P2 amplitude", 
#        colour = "configuration")

# 6.4 220-280 - session
nd2_P2_anterior_line_session <- ggplot(ROImap_data_long, aes(x = session, y = P2_anterior, 
                                                          colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~group.original) +
  labs(title = "P2 anterior average reference", x = " session", y = "Nd2 P2 amplitude", 
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

# # 6.6 Nd2 LP_left - session
# nd2_LP_left_line_session <- ggplot(ROImap_data_long, aes(x = session, y = LP_left, 
#                                                           colour = configuration)) + 
#   stat_summary(fun.y = mean, geom = "point") + 
#   stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
#   stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
#   facet_grid(.~group.original) +
#   labs(title = "LP left average reference", x = " session", y = "Nd2 N1 amplitude", 
#        colour = "configuration")

# 6.7 Nd2 LP_central - session
nd2_LP_central_line_session <- ggplot(ROImap_data_long, aes(x = session, y = LP_central, 
                                                          colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~group.original) +
  labs(title = "LP central average reference", x = " session", y = "LP 
       amplitude", colour = "configuration")

#--------------------------Linear model----------------------------
# 6.1. P1_occ_right
contrasts(ROImap_data_long$configuration) <- c(-1, 1) # set contrasts for config
contrasts(ROImap_data_long$session) <- c(-1, 1) # set contrasts for session
contrasts(ROImap_data_long$group) <- c(-1, 1) # set contrasts for group
nd2_P1_occ_right_baseline <- lme(P1_occ_right ~ 1, random = ~1|subject/configuration/session, 
                         data = ROImap_data_long, method = "ML") #baseline
nd2_P1_occ_right_config <- update(nd2_P1_occ_right_baseline, .~. + configuration)
nd2_P1_occ_right_session <- update(nd2_P1_occ_right_config, .~. + session)
nd2_P1_occ_right_group <- update(nd2_P1_occ_right_session, .~. + group)
nd2_P1_occ_right_config_session <- update(nd2_P1_occ_right_group, .~. + configuration:session)
nd2_P1_occ_right_session_group <- update(nd2_P1_occ_right_config_session, .~. + session:group)
nd2_P1_occ_right_config_group <- update(nd2_P1_occ_right_session_group, .~. + configuration:group)
nd2_P1_occ_right_lme <- update(nd2_P1_occ_right_config_group, .~. + configuration:session:group)
anova(nd2_P1_occ_right_baseline, nd2_P1_occ_right_config, nd2_P1_occ_right_session, nd2_P1_occ_right_group, 
      nd2_P1_occ_right_config_session, nd2_P1_occ_right_session_group, nd2_P1_occ_right_config_group, 
      nd2_P1_occ_right_lme)
summary(nd2_P1_occ_right_lme)

# 6.2. N1_occ
contrasts(ROImap_data_long$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(ROImap_data_long$session) <- c(-1, 1) # setting contrasts for session
contrasts(ROImap_data_long$group) <- c(-1, 1) # setting contrasts for group
nd2_N1_occ_baseline <- lme(N1_occ ~ 1, random = ~1|subject/configuration/session, 
                          data = ROImap_data_long, method = "ML") #baseline
nd2_N1_occ_config <- update(nd2_N1_occ_baseline, .~. + configuration)
nd2_N1_occ_session <- update(nd2_N1_occ_config, .~. + session)
nd2_N1_occ_group <- update(nd2_N1_occ_session, .~. + group)
nd2_N1_occ_config_session <- update(nd2_N1_occ_group, .~. + configuration:session)
nd2_N1_occ_session_group <- update(nd2_N1_occ_config_session, .~. + session:group)
nd2_N1_occ_config_group <- update(nd2_N1_occ_session_group, .~. + configuration:group)
nd2_N1_occ_lme <- update(nd2_N1_occ_config_group, .~. + configuration:session:group)
anova(nd2_N1_occ_baseline, nd2_N1_occ_config, nd2_N1_occ_session, nd2_N1_occ_group, 
      nd2_N1_occ_config_session, nd2_N1_occ_session_group, nd2_N1_occ_config_group, 
      nd2_N1_occ_lme)
summary(nd2_N1_occ_lme)

# # 6.3. P2_right
# contrasts(ROImap_data_long$configuration) <- c(-1, 1) # setting contrasts for config
# contrasts(ROImap_data_long$session) <- c(-1, 1) # setting contrasts for session
# contrasts(ROImap_data_long$group) <- c(-1, 1) # setting contrasts for group
# nd2_P2_right_baseline <- lme(P2_right ~ 1, random = ~1|subject/configuration/session, 
#                            data = ROImap_data_long, method = "ML") #baseline
# nd2_P2_right_config <- update(nd2_P2_right_baseline, .~. + configuration)
# nd2_P2_right_session <- update(nd2_P2_right_config, .~. + session)
# nd2_P2_right_group <- update(nd2_P2_right_session, .~. + group)
# nd2_P2_right_config_session <- update(nd2_P2_right_group, .~. + configuration:session)
# nd2_P2_right_session_group <- update(nd2_P2_right_config_session, .~. + session:group)
# nd2_P2_right_config_group <- update(nd2_P2_right_session_group, .~. + configuration:group)
# nd2_P2_right_lme <- update(nd2_P2_right_config_group, .~. + configuration:session:group)
# anova(nd2_P2_right_baseline, nd2_P2_right_config, nd2_P2_right_session, nd2_P2_right_group, 
#       nd2_P2_right_config_session, nd2_P2_right_session_group, nd2_P2_right_config_group, 
#       nd2_P2_right_lme)
# summary(nd2_P2_right_lme)

# 6.4. P2_anterior
contrasts(ROImap_data_long$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(ROImap_data_long$session) <- c(-1, 1) # setting contrasts for session
contrasts(ROImap_data_long$group) <- c(-1, 1) # setting contrasts for group
nd2_P2_anterior_baseline <- lme(P2_anterior ~ 1, random = ~1|subject/configuration/session, 
                             data = ROImap_data_long, method = "ML") #baseline
nd2_P2_anterior_config <- update(nd2_P2_anterior_baseline, .~. + configuration)
nd2_P2_anterior_session <- update(nd2_P2_anterior_config, .~. + session)
nd2_P2_anterior_group <- update(nd2_P2_anterior_session, .~. + group)
nd2_P2_anterior_config_session <- update(nd2_P2_anterior_group, .~. + configuration:session)
nd2_P2_anterior_session_group <- update(nd2_P2_anterior_config_session, .~. + session:group)
nd2_P2_anterior_config_group <- update(nd2_P2_anterior_session_group, .~. + configuration:group)
nd2_P2_anterior_lme <- update(nd2_P2_anterior_config_group, .~. + configuration:session:group)
anova(nd2_P2_anterior_baseline, nd2_P2_anterior_config, nd2_P2_anterior_session, nd2_P2_anterior_group, 
      nd2_P2_anterior_config_session, nd2_P2_anterior_session_group, nd2_P2_anterior_config_group, 
      nd2_P2_anterior_lme)
summary(nd2_P2_anterior_lme)

# 6.5. N2_occ
contrasts(ROImap_data_long$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(ROImap_data_long$session) <- c(-1, 1) # setting contrasts for session
contrasts(ROImap_data_long$group) <- c(-1, 1) # setting contrasts for group
nd2_N2_occ_baseline <- lme(N2_occ ~ 1, random = ~1|subject/configuration/session, 
                                data = ROImap_data_long, method = "ML") #baseline
nd2_N2_occ_config <- update(nd2_N2_occ_baseline, .~. + configuration)
nd2_N2_occ_session <- update(nd2_N2_occ_config, .~. + session)
nd2_N2_occ_group <- update(nd2_N2_occ_session, .~. + group)
nd2_N2_occ_config_session <- update(nd2_N2_occ_group, .~. + configuration:session)
nd2_N2_occ_session_group <- update(nd2_N2_occ_config_session, .~. + session:group)
nd2_N2_occ_config_group <- update(nd2_N2_occ_session_group, .~. + configuration:group)
nd2_N2_occ_lme <- update(nd2_N2_occ_config_group, .~. + configuration:session:group)
anova(nd2_N2_occ_baseline, nd2_N2_occ_config, nd2_N2_occ_session, nd2_N2_occ_group, 
      nd2_N2_occ_config_session, nd2_N2_occ_session_group, nd2_N2_occ_config_group, 
      nd2_N2_occ_lme)
summary(nd2_N2_occ_lme)

# # 6.6. Nd2 LP_left
# contrasts(ROImap_data_long$configuration) <- c(-1, 1) # setting contrasts for config
# contrasts(ROImap_data_long$session) <- c(-1, 1) # setting contrasts for session
# contrasts(ROImap_data_long$group) <- c(-1, 1) # setting contrasts for group
# nd2_LP_left_baseline <- lme(LP_left ~ 1, random = ~1|subject/configuration/session, 
#                            data = ROImap_data_long, method = "ML") #baseline
# nd2_LP_left_config <- update(nd2_LP_left_baseline, .~. + configuration)
# nd2_LP_left_session <- update(nd2_LP_left_config, .~. + session)
# nd2_LP_left_group <- update(nd2_LP_left_session, .~. + group)
# nd2_LP_left_config_session <- update(nd2_LP_left_group, .~. + configuration:session)
# nd2_LP_left_session_group <- update(nd2_LP_left_config_session, .~. + session:group)
# nd2_LP_left_config_group <- update(nd2_LP_left_session_group, .~. + configuration:group)
# nd2_LP_left_lme <- update(nd2_LP_left_config_group, .~. + configuration:session:group)
# anova(nd2_LP_left_baseline, nd2_LP_left_config, nd2_LP_left_session, nd2_LP_left_group, 
#       nd2_LP_left_config_session, nd2_LP_left_session_group, nd2_LP_left_config_group, 
#       nd2_LP_left_lme)
# summary(nd2_LP_left_lme)

# 6.7. Nd2 LP_central
contrasts(ROImap_data_long$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(ROImap_data_long$session) <- c(-1, 1) # setting contrasts for session
contrasts(ROImap_data_long$group) <- c(-1, 1) # setting contrasts for group
nd2_LP_central_baseline <- lme(LP_central ~ 1, random = ~1|subject/configuration/session, 
                            data = ROImap_data_long, method = "ML") #baseline
nd2_LP_central_config <- update(nd2_LP_central_baseline, .~. + configuration)
nd2_LP_central_session <- update(nd2_LP_central_config, .~. + session)
nd2_LP_central_group <- update(nd2_LP_central_session, .~. + group)
nd2_LP_central_config_session <- update(nd2_LP_central_group, .~. + configuration:session)
nd2_LP_central_session_group <- update(nd2_LP_central_config_session, .~. + session:group)
nd2_LP_central_config_group <- update(nd2_LP_central_session_group, .~. + configuration:group)
nd2_LP_central_lme <- update(nd2_LP_central_config_group, .~. + configuration:session:group)
anova(nd2_LP_central_baseline, nd2_LP_central_config, nd2_LP_central_session, nd2_LP_central_group, 
      nd2_LP_central_config_session, nd2_LP_central_session_group, nd2_LP_central_config_group, 
      nd2_LP_central_lme)
summary(nd2_LP_central_lme)

# 7. Pairwise comparisons for each session and group separately
# Subset groups
ROImap_aware_subset <- subset(ROImap_data_long, group.original == "aware")
ROImap_sqr_aware_subset <- subset(ROImap_aware_subset, configuration == "sqr")
ROImap_sqr1_aware_subset <- subset(ROImap_sqr_aware_subset, session == 1)
ROImap_sqr2_aware_subset <- subset(ROImap_sqr_aware_subset, session == 2)
ROImap_rand_aware_subset <- subset(ROImap_rand_aware_subset, configuration == "rand")
ROImap_rand1_aware_subset <- subset(ROImap_rand_aware_subset, session == 1)
ROImap_rand2_aware_subset <- subset(ROImap_rand_aware_subset, session == 2)
ROImap_unaware_subset <- subset(ROImap_data_long, group.original == "unaware")
ROImap_sqr_unaware_subset <- subset(ROImap_unaware_subset, configuration == "sqr")
ROImap_sqr1_unaware_subset <- subset(ROImap_sqr_unaware_subset, session == 1)
ROImap_sqr2_unaware_subset <- subset(ROImap_sqr_unaware_subset, session == 2)
ROImap_rand_unaware_subset <- subset(ROImap_rand_unaware_subset, configuration == "rand")
ROImap_rand1_unaware_subset <- subset(ROImap_rand_unaware_subset, session == 1)
ROImap_rand2_unaware_subset <- subset(ROImap_rand_unaware_subset, session == 2)

# P1
t.test(ROImap_sqr1_aware_subset$P1_occ_right, ROImap_rand1_aware_subset$P1_occ_right, paired = TRUE)
t.test(ROImap_sqr2_aware_subset$P1_occ_right, ROImap_rand2_aware_subset$P1_occ_right, paired = TRUE)
t.test(ROImap_sqr1_unaware_subset$P1_occ_right, ROImap_rand1_unaware_subset$P1_occ_right, paired = TRUE)
t.test(ROImap_sqr2_unaware_subset$P1_occ_right, ROImap_rand2_unaware_subset$P1_occ_right, paired = TRUE)
# N1
t.test(ROImap_sqr1_aware_subset$N1_occ, ROImap_rand1_aware_subset$N1_occ, paired = TRUE)
t.test(ROImap_sqr2_aware_subset$N1_occ, ROImap_rand2_aware_subset$N1_occ, paired = TRUE)
t.test(ROImap_sqr1_unaware_subset$N1_occ, ROImap_rand1_unaware_subset$N1_occ, paired = TRUE)
t.test(ROImap_sqr2_unaware_subset$N1_occ, ROImap_rand2_unaware_subset$N1_occ, paired = TRUE)
# 220-280
t.test(ROImap_sqr1_aware_subset$P2_anterior, ROImap_rand1_aware_subset$P2_anterior, paired = TRUE)
t.test(ROImap_sqr2_aware_subset$P2_anterior, ROImap_rand2_aware_subset$P2_anterior, paired = TRUE)
t.test(ROImap_sqr1_unaware_subset$P2_anterior, ROImap_rand1_unaware_subset$P2_anterior, paired = TRUE)
t.test(ROImap_sqr2_unaware_subset$P2_anterior, ROImap_rand2_unaware_subset$P2_anterior, paired = TRUE)
# 280-360
t.test(ROImap_sqr1_aware_subset$N2_occ, ROImap_rand1_aware_subset$N2_occ, paired = TRUE)
t.test(ROImap_sqr2_aware_subset$N2_occ, ROImap_rand2_aware_subset$N2_occ, paired = TRUE)
t.test(ROImap_sqr1_unaware_subset$N2_occ, ROImap_rand1_unaware_subset$N2_occ, paired = TRUE)
t.test(ROImap_sqr2_unaware_subset$N2_occ, ROImap_rand2_unaware_subset$N2_occ, paired = TRUE)
# LP
t.test(ROImap_sqr1_aware_subset$LP_central, ROImap_rand1_aware_subset$LP_central, paired = TRUE)
t.test(ROImap_sqr2_aware_subset$LP_central, ROImap_rand2_aware_subset$LP_central, paired = TRUE)
t.test(ROImap_sqr1_unaware_subset$LP_central, ROImap_rand1_unaware_subset$LP_central, paired = TRUE)
t.test(ROImap_sqr2_unaware_subset$LP_central, ROImap_rand2_unaware_subset$LP_central, paired = TRUE)

# 8. Differences in occ between groups for session 1
rep_data_long6 <- subset(ROImap_data_long, configuration == "sqr")
rep_data_long7 <- subset(rep_data_long6, session == 1)
t.test(occ ~ group, data = rep_data_long7, paired = FALSE)

# 9. Differences in P1_occ_right & right between groups for session 1
t.test(P1_occ_right ~ group, data = rep_data_long7, paired = FALSE)
t.test(right ~ group, data = rep_data_long7, paired = FALSE)
