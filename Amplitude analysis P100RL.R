library(pastecs)
library(ggplot2)
library(nlme)
library(gridExtra)
library(outliers)
library(lattice)
library(car)
library(ez)
library(stat.desc)

# 2. Subsets
# 4.1. Create variable for means of ROI conditions (wide format)
rep_data2$occ.means <- rowMeans(rep_data2[,c(2,5,8,11)])
rep_data2$left.means <- rowMeans(rep_data2[,c(3,6,9,12)])
rep_data2$right.means <- rowMeans(rep_data2[,c(4,7,10,13)])
# 4.2. Subset configuration x session
sqr_config <- subset(P100RL_data_long, configuration == "sqr")
rand_config <- subset(P100RL_data_long, configuration == "rand")
session_1 <- subset(P100RL_data_long, session == 1)
session_2 <- subset(P100RL_data_long, session == 2)

# 3. Compute variances and means
by(P100RL_data_long[,6:8], list(P100RL_data_long$configuration, 
                              P100RL_data_long$session, P100RL_data_long$group.original), 
   stat.desc, basic = FALSE)


# 4. Normality tests
# 4.1. Nd1
shapiro.test(P100RL_data_long$occ)
qqnorm(P100RL_data_long$occ, main = "Nd1");qqline(P100RL_data_long$occ, col = 3)
shapiro.test(sqr_config$occ)
qqnorm(sqr_config$occ, main = "Nd1 square configuration");qqline(
  sqr_config$occ, col = 4)
shapiro.test(rand_config$occ)
qqnorm(rand_config$occ, main = "Nd1 random configuration");qqline(
  rand_config$occ, col = 2)

# 4.2. Nd2 left
shapiro.test(P100RL_data_long$left)
shapiro.test(sqr_config$left)
shapiro.test(rand_config$left)
qqnorm(sqr_config$left, main = "Nd2 left square configuration");qqline(
  sqr_config$left, col = 4)
qqnorm(rand_config$left, main = "Nd2 left random configuration");qqline(
  rand_config$left, col = 2)

# 4.3. Nd2 right
shapiro.test(sqr_config$right)
shapiro.test(rand_config$right)
qqnorm(sqr_config$right, main = "Nd2 right square configuration");qqline(
  sqr_config$occ, col = 4)
qqnorm(rand_config$right, main = "Nd2 right random configuration");qqline(
  rand_config$occ, col = 2)

# 4. Plot variances and means
# 4.1. Scatterplot
# 4.1.1 Group occ means
plot(1:14, rep_data2$occ.means[rep_data2$group == "aware"], ylab = "mean occ amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "Nd1 windows amplitudes")
points(1:18, rep_data2$occ.means[rep_data2$group == "unaware"], pch = 20, col = "blue")
abline(h=mean(rep_data2$occ.means[rep_data2$group == "aware"]), col = "red")
abline(h=mean(rep_data2$occ.means[rep_data2$group == "unaware"]), col = "blue")
occ_scat <- ggplot(P100RL_data_long, aes(x = Subject, y = occ, colour = group)) + 
  geom_point(aes(shape = configuration)) + 
  geom_smooth(method = "lm", se = F) #ggplot
# 4.1.2 Configuration x session means


# 5. Outlier detection and replacement
# 5.1. Identify outliers
# Nd1
occ_box <- ggplot(P100RL_data_long, aes(x = configuration, y = occ, fill = configuration)) +
  geom_boxplot() + facet_grid(session~group)
Boxplot(occ ~ configuration * session * group, data = P100RL_data_long, 
        colour = group) #car boxplot
bwplot(occ ~ configuration|session, groups = group, data = P100RL_data_long, 
       auto.key = T) #lattice boxplot
boxplot(P100RL_data_long$occ ~ P100RL_data_long$configuration * 
          P100RL_data_long$group) #base boxplot
occ_out <- boxplot(P100RL_data_long$occ, plot = FALSE)$out
P100RL_data_long[P100RL_data_long$occ %in% occ_out,]
# Nd2 left
left_box <- ggplot(P100RL_data_long, aes(x = configuration, y = left,
                                       fill = configuration)) +
  geom_boxplot() + 
  facet_grid(session~group)
boxplot(P100RL_data_long$left ~ P100RL_data_long$configuration * P100RL_data_long$group * 
          P100RL_data_long$session)
left_out <- boxplot(P100RL_data_long$left ~ P100RL_data_long$configuration * P100RL_data_long$group * 
                      P100RL_data_long$session, plot = FALSE)$out
P100RL_data_long[P100RL_data_long$left %in% left_out,]
# Nd2 right
boxplot(P100RL_data_long$right ~ P100RL_data_long$configuration * P100RL_data_long$group * 
          P100RL_data_long$session)
right_out <- boxplot(P100RL_data_long$right ~ P100RL_data_long$configuration * P100RL_data_long$group * 
                       P100RL_data_long$session, plot = FALSE)$out
P100RL_data_long[P100RL_data_long$right %in% right_out,]
# 5.2. Replace outliers by the mean
P100RL_data_long$occ <- rm.outlier(P100RL_data_long$occ, fill = TRUE)
P100RL_data_long$left <- rm.outlier(P100RL_data_long$left, fill = TRUE)
P100RL_data_long$right <- rm.outlier(P100RL_data_long$right, fill = TRUE)

# 7. Line graphs with error bars
# 7.1 Nd1 - session
P100RL_line_session <- ggplot(P100RL_data_long, aes(x = group, y = P100RL, 
                                               colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "P100RL average reference", x = "group", y = "Nd1 P100RL amplitude", 
       colour = "configuration")

# 7.3 Nd2 left - session
nd2_left_line_session <- ggplot(P100RL_data_long, aes(x = group, y = left, 
                                                    colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "Left average reference", x = "group", y = "Nd2 left amplitude", 
       colour = "configuration")

# 7.5 Nd2 right - session
nd2_right_line_session <- ggplot(P100RL_data_long, aes(x = group, y = right, 
                                                     colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "Right average reference", x = "group", y = "Nd2 right amplitude", 
       colour = "configuration")

#--------------------------Linear model----------------------------
# 6. Mixed model
ses1_subset <- subset(P100RL_data_long, session == 1)
ses2_subset <- subset(P100RL_data_long, session == 2)

contrasts(ses2_subset$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(ses2_subset$group) <- c(-1, 1) # setting contrasts for group
nd1_baseline <- lme(occ ~ 1, random = ~1|Subject/configuration, 
                    data = ses2_subset, method = "ML") #baseline
nd1_config <- update(nd1_baseline, .~. + configuration)
anova(nd1_baseline, nd1_config)
summary(nd1_lme)

# 6.1 Nd1
contrasts(P100RL_data_long$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(P100RL_data_long$session) <- c(-1, 1) # setting contrasts for session
contrasts(P100RL_data_long$group) <- c(-1, 1) # setting contrasts for group
P100RL_baseline <- lme(P100RL ~ 1, random = ~1|subject/configuration/session, 
                    data = P100RL_data_long, method = "ML") #baseline
P100RL_config <- update(P100RL_baseline, .~. + configuration)
P100RL_session <- update(P100RL_config, .~. + session)
P100RL_group <- update(P100RL_session, .~. + group)
P100RL_config_session <- update(P100RL_group, .~. + configuration:session)
P100RL_session_group <- update(P100RL_config_session, .~. + session:group)
P100RL_config_group <- update(P100RL_session_group, .~. + configuration:group)
P100RL_lme <- update(P100RL_config_group, .~. + configuration:session:group)
anova(P100RL_baseline, P100RL_config, P100RL_session, P100RL_group, P100RL_config_session,
      P100RL_session_group, P100RL_config_group, P100RL_lme)
summary(P100RL_lme)


# 7. Pairwise comparisons for each session and group separately
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

# 8. Differences in occ between groups for session 1
rep_data_long6 <- subset(P100RL_data_long, configuration == "sqr")
rep_data_long7 <- subset(rep_data_long6, session == 1)
t.test(occ ~ group, data = rep_data_long7, paired = FALSE)

# 9. Differences in left & right between groups for session 1
t.test(left ~ group, data = rep_data_long7, paired = FALSE)
t.test(right ~ group, data = rep_data_long7, paired = FALSE)

# hmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
rep_data_long8 <- subset(P100RL_data_long, configuration == "rand")
rep_data_long9 <- subset(rep_data_long8, group == "aware")
t.test(occ ~ group, data = rep_data_long7, paired = FALSE)
t.test(left ~ session, data = rep_data_long9, paired = TRUE)
t.test(right ~ session, data = rep_data_long9, paired = TRUE)

#------------------------------Plots-------------------------------
# 7.1 Nd1
interaction.plot(P100RL_data_long$session, P100RL_data_long$configuration,
                 P100RL_data_long$occ, col = c("blue", "red"), 
                 ylim = c(-0.8, 0.8), xlab = "Session", 
                 ylab = "mean amplitude", main = "Interaction plot")
interaction.plot(P100RL_data_long$group, P100RL_data_long$configuration,
                 P100RL_data_long$occ, col = c("blue", "red"),
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


# 7.2 Nd2 left
interaction.plot(P100RL_data_long$session, P100RL_data_long$configuration,
                 P100RL_data_long$left, col = c("blue", "red"), 
                 ylim = c(-1.8, 0.8), xlab = "Session", 
                 ylab = "mean amplitude", main = "Interaction plot left")
interaction.plot(P100RL_data_long$group, P100RL_data_long$configuration,
                 P100RL_data_long$left, col = c("blue", "red"),
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