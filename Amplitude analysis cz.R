library(pastecs)
library(ggplot2)
library(nlme)
library(gridExtra)
library(lattice)
library(car)
library(outliers)
library(ez)
library(stat.desc)

#------------------------Preliminary analysis-----------------------

# 1. Convert configuration and session number to factors
rep_data_cz_long2$group <- factor(rep_data_cz_long2$group)
rep_data_cz_long2$session <- factor(rep_data_cz_long2$session)
rep_data_cz_long2$configuration <- factor(rep_data_cz_long2$configuration)
rep_data_cz_long2$group.original <- factor(rep_data_cz_long2$group.original)

# 2. Subsets
sqr_config_cz <- subset(rep_data_cz_long2, configuration == "sqr")
rand_config_cz <- subset(rep_data_cz_long2, configuration == "rand")
session_1_cz <- subset(rep_data_cz_long2, session == 1)
session_2_cz <- subset(rep_data_cz_long2, session == 2)

# 3. Compute variances and means
by(rep_data_cz_long2[,6:8], list(rep_data_cz_long2$configuration, 
                            rep_data_cz_long2$session, rep_data_cz_long2$group), 
   stat.desc, basic = FALSE)

# 4. Normality tests
shapiro.test(sqr_config_cz$occ)
shapiro.test(rand_config_cz$occ)
qqnorm(sqr_config_cz$occ, main = "Square configuration");qqline(
  sqr_config_cz$occ, col = 4)
qqnorm(rand_config_cz$occ, main = "Random configuration");qqline(
  rand_config_cz$occ, col = 2)

#------------------------Plots and outliers-----------------------
# 5. Plot variances and means
# 5.1. Scatterplot
# 5.1.1. Occ
rep_data_cz2$occ.sqr.means <- rowMeans(rep_data_cz2[,c(2,5)])
rep_data_cz2$occ.rand.means <- rowMeans(rep_data_cz2[,c(8,11)])
plot(rep_data_cz2$Subject, rep_data_cz2$occ.sqr.means, ylab = "mean occ amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "Occ amplitudes")
points(rep_data_cz2$Subject, rep_data_cz2$occ.rand.means, pch = 20, col = "blue")
abline(h=mean(rep_data_cz2$occ.sqr.means), col = "red")
abline(h=mean(rep_data_cz2$occ.rand.means), col = "blue")

# 5.1.2. Left
rep_data_cz2$left.sqr.means <- rowMeans(rep_data_cz2[,c(3,6)])
rep_data_cz2$left.rand.means <- rowMeans(rep_data_cz2[,c(9,12)])
plot(rep_data_cz2$Subject, rep_data_cz2$left.sqr.means, ylab = "mean left amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "Left amplitudes")
points(rep_data_cz2$Subject, rep_data_cz2$left.rand.means, pch = 20, col = "blue")
abline(h=mean(rep_data_cz2$left.sqr.means), col = "red")
abline(h=mean(rep_data_cz2$left.rand.means), col = "blue")

# 5.1.3. Right
rep_data_cz2$right.sqr.means <- rowMeans(rep_data_cz2[,c(4,7)])
rep_data_cz2$right.rand.means <- rowMeans(rep_data_cz2[,c(10,13)])
plot(rep_data_cz2$Subject, rep_data_cz2$right.sqr.means, ylab = "mean right amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "right amplitudes")
points(rep_data_cz2$Subject, rep_data_cz2$right.rand.means, pch = 20, col = "blue")
abline(h=mean(rep_data_cz2$right.sqr.means), col = "red")
abline(h=mean(rep_data_cz2$right.rand.means), col = "blue")

# 6. Outlier detection and replacement
# 6.1. Identify outliers
# 6.1.1. Nd1
occ_box <- ggplot(rep_data_cz_long2, aes(x = configuration, y = occ, fill = configuration)) +
  geom_boxplot() + facet_grid(session~group)
Boxplot(occ ~ configuration * session * group, data = rep_data_cz_long2, 
        colour = group) #car boxplot
bwplot(occ ~ configuration|session, groups = group, data = rep_data_cz_long2, 
       auto.key = T) #lattice boxplot
boxplot(rep_data_cz_long2$occ ~ rep_data_cz_long2$configuration * 
          rep_data_cz_long2$group) #base boxplot
occ_out <- boxplot(rep_data_cz_long2$occ, plot = FALSE)$out
rep_data_cz_long2[rep_data_cz_long2$occ %in% occ_out,]
# 6.1.2. Nd2 left
left_box <- ggplot(rep_data_cz_long2, aes(x = configuration, y = left,
                                          fill = configuration)) +
  geom_boxplot() + 
  facet_grid(session~group) +
  labs(title = "Nd2 left Cz reference")
boxplot(rep_data_cz_long2$left ~ rep_data_cz_long2$configuration * rep_data_cz_long2$group * 
          rep_data_cz_long2$session)
left_out <- boxplot(rep_data_cz_long2$left ~ rep_data_cz_long2$configuration * rep_data_cz_long2$group * 
                      rep_data_cz_long2$session, plot = FALSE)$out
rep_data_cz_long2[rep_data_cz_long2$left %in% left_out,]

# 6.1.3. Nd2 right
right_box <- ggplot(rep_data_cz_long2, aes(x = configuration, y = right,
                                           fill = configuration)) +
  geom_boxplot() + 
  facet_grid(session~group) +
  labs(title = "Nd2 right Cz reference")
boxplot(rep_data_cz_long2$right ~ rep_data_cz_long2$configuration * rep_data_cz_long2$group * 
          rep_data_cz_long2$session)
right_out <- boxplot(rep_data_cz_long2$right ~ rep_data_cz_long2$configuration * rep_data_cz_long2$group * 
                       rep_data_cz_long2$session, plot = FALSE)$out
rep_data_cz_long2[rep_data_cz_long2$right %in% right_out,]
# 6.2. Replace outliers by the mean
rep_data_cz_long2$occ <- rm.outlier(rep_data_cz_long2$occ, fill = TRUE)
rep_data_cz_long2$left <- rm.outlier(rep_data_cz_long2$left, fill = TRUE)
rep_data_cz_long2$right <- rm.outlier(rep_data_cz_long2$right, fill = TRUE)

# 7. Line graphs with error bars
# 7.1 Nd1 - session
nd1_line_session <- ggplot(rep_data_cz_long2, aes(x = session, y = occ, 
                                                  colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_grid(.~group) +
  labs(title = "Occ Cz reference", x = " session", y = "Nd1 occ amplitude", 
       colour = "configuration")

# 7.3 Nd2 left - session
nd2_left_line_session <- ggplot(rep_data_cz_long2, aes(x = session, y = left, 
                                                       colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~group.original) +
  labs(title = "Left Cz reference", x = " session", y = "Nd2 left amplitude", 
       colour = "configuration")

# 7.5 Nd2 right - session
nd2_right_line_session <- ggplot(rep_data_cz_long2, aes(x = session, y = right, 
                                                       colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~group) +
  labs(title = "Right Cz reference", x = " session", y = "Nd2 right amplitude", 
       colour = "configuration")


#--------------------------Linear model----------------------------
# 8. Mixed model
# 8.1 Nd1
contrasts(rep_data_cz_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_cz_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_cz_long2$group) <- c(-1, 1) # setting contrasts for group
nd1_baseline <- lme(occ ~ 1, random = ~1|Subject/configuration/session, 
                    data = rep_data_cz_long2, method = "ML") #baseline
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

# 6.2 Nd2 left
contrasts(rep_data_cz_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_cz_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_cz_long2$group) <- c(-1, 1) # setting contrasts for group
nd2_left_baseline <- lme(left ~ 1, random = ~1|Subject/configuration/session, 
                    data = rep_data_cz_long2, method = "ML") #baseline
nd2_left_config <- update(nd2_left_baseline, .~. + configuration)
nd2_left_session <- update(nd2_left_config, .~. + session)
nd2_left_group <- update(nd2_left_session, .~. + group)
nd2_left_config_session <- update(nd2_left_group, .~. + configuration:session)
nd2_left_session_group <- update(nd2_left_config_session, .~. + session:group)
nd2_left_config_group <- update(nd2_left_session_group, .~. + configuration:group)
nd2_left_lme <- update(nd2_left_config_group, .~. + configuration:session:group)
anova(nd2_left_baseline, nd2_left_config, nd2_left_session, nd2_left_group, 
      nd2_left_config_session, nd2_left_session_group, nd2_left_config_group, 
      nd2_left_lme)
summary(nd2_left_lme)
# Plot configuration x group interaction
nd2_left_config_group_plot <- ggplot(rep_data_cz_long2, aes(x = group, y = left,
                                                              color = configuration)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Cz Left configuration x group interaction")
# Plot session x group interaction
nd2_left_session_group_plot <- ggplot(rep_data_cz_long2, aes(x = session, y = left,
                                                              color = group)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = group)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Cz Left session x group interaction")

# 6.3 Nd2 right
contrasts(rep_data_cz_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_cz_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_cz_long2$group) <- c(-1, 1) # setting contrasts for group
nd2_right_baseline <- lme(right ~ 1, random = ~1|Subject/configuration/session, 
                         data = rep_data_cz_long2, method = "ML") #baseline
nd2_right_config <- update(nd2_right_baseline, .~. + configuration)
nd2_right_session <- update(nd2_right_config, .~. + session)
nd2_right_group <- update(nd2_right_session, .~. + group)
nd2_right_config_session <- update(nd2_right_group, .~. + configuration:session)
nd2_right_session_group <- update(nd2_right_config_session, .~. + session:group)
nd2_right_config_group <- update(nd2_right_session_group, .~. + configuration:group)
nd2_right_lme <- update(nd2_right_config_group, .~. + configuration:session:group)
anova(nd2_right_baseline, nd2_right_config, nd2_right_session, nd2_right_group, 
      nd2_right_config_session, nd2_right_session_group, nd2_right_config_group, 
      nd2_right_lme)
summary(nd2_right_lme)
# rep_data_cz3 <- rep_data_cz2
# rep_data_cz3$occ_mean <- rowMeans(rep_data_cz3[,c(2,5,8,11)])
# plot(occ_mean ~ Subject, data = rep_data_cz3)
# abline(h = nd2_right_lme)
# Plot session x group interaction
nd2_right_session_group_plot <- ggplot(rep_data_cz_long2, aes(x = session, y = right,
                                                              color = group)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = group)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Cz Right session x group interaction")

# 7. Pairwise comparisons for each session and group separately
# Subset groups
aware_subset <- subset(rep_data_cz2, group == "aware")
unaware_subset <- subset(rep_data_cz2, group == "unaware")
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

# 8. Differences in occ between groups for session 1
rep_data_cz_long3 <- subset(rep_data_cz_long2, configuration == "sqr")
rep_data_cz_long4 <- subset(rep_data_cz_long3, session == 1)
t.test(occ ~ group, data = rep_data_cz_long4, paired = FALSE)

# 9. Differences in left & right between groups for session 1
t.test(left ~ group, data = rep_data_cz_long4, paired = FALSE)
t.test(right ~ group, data = rep_data_cz_long4, paired = FALSE)

#------------------------------Plots-------------------------------
# 8 Interaction plots
# 8.1 Nd1
interaction.plot(rep_data_cz_long2$session, rep_data_cz_long2$configuration,
                 rep_data_cz_long2$occ, col = c("blue", "red"), 
                 ylim = c(-0.8, 0.8), xlab = "Session", 
                 ylab = "mean amplitude", main = "Interaction plot")
interaction.plot(rep_data_cz_long2$group, rep_data_cz_long2$configuration,
                 rep_data_cz_long2$occ, col = c("blue", "red"),
                 ylim = c(-0.8, 0.8), xlab = "Group", 
                 ylab = "mean amplitude", main = "Interaction plot")
interaction.plot(sqr_config$group, sqr_config$session,
                 sqr_config$occ, col = c("cyan3", "deeppink"),
                 ylim = c(-0.8, 0.8), xlab = "Group", 
                 ylab = "mean amplitude", main = "Interaction plot sqr")
interaction.plot(rand_config$group, rand_config$session,
                 rand_config$occ, col = c("cyan3", "deeppink"),
                 ylim = c(-0.8, 0.8), xlab = "Group", 
                 ylab = "mean amplitude", main = "Interaction plot rand")


# 8.2 Nd2 left
interaction.plot(rep_data_cz_long2$session, rep_data_cz_long2$configuration,
                 rep_data_cz_long2$left, col = c("blue", "red"), 
                 ylim = c(-1.8, 0.8), xlab = "Session", 
                 ylab = "mean amplitude", main = "Interaction plot left")
interaction.plot(rep_data_cz_long2$group, rep_data_cz_long2$configuration,
                 rep_data_cz_long2$left, col = c("blue", "red"),
                 ylim = c(-1.8, 0.8), xlab = "Group", 
                 ylab = "mean amplitude", main = "Interaction plot left")
interaction.plot(sqr_config$group, sqr_config$session,
                 sqr_config$left, col = c("cyan3", "deeppink"),
                 ylim = c(-1.8, 0.8), xlab = "Group", 
                 ylab = "mean amplitude", main = "Interaction plot left sqr")
interaction.plot(rand_config$group, rand_config$session,
                 rand_config$left, col = c("cyan3", "deeppink"),
                 ylim = c(-1.8, 0.8), xlab = "Group", 
                 ylab = "mean amplitude", main = "Interaction plot left rand")

# 9. Boxplots
nd1_boxplots <- 