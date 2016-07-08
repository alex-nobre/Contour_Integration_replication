library(pastecs)
library(ggplot2)
library(nlme)
library(outliers)
library(lattice)
library(car)

# 1. Convert configuration and session number to factors
rep_data_long_alt2$group <- factor(rep_data_long_alt2$group)
rep_data_long_alt2$session <- factor(rep_data_long_alt2$session)
rep_data_long_alt2$configuration <- factor(rep_data_long_alt2$configuration)

# 2. Subsets
sqr_config <- subset(rep_data_long_alt2, configuration == "sqr")
rand_config <- subset(rep_data_long_alt2, configuration == "rand")
session_1 <- subset(rep_data_long_alt2, session == 1)
session_2 <- subset(rep_data_long_alt2, session == 2)

# 3. Compute variances and means
# 3.1. Nd1
by(rep_data_long_alt2$occ, list(rep_data_long_alt2$configuration, 
                                rep_data_long_alt2$session, rep_data_long_alt2$group), 
   stat.desc, basic = FALSE)
# 3.2. Nd2 left
by(rep_data_long_alt2$left, list(rep_data_long_alt2$configuration, 
                                 rep_data_long_alt2$session, rep_data_long_alt2$group), 
   stat.desc, basic = FALSE)
# 3.3. Nd2 right
by(rep_data_long_alt2$right, list(rep_data_long_alt2$configuration, 
                                  rep_data_long_alt2$session, rep_data_long_alt2$group), 
   stat.desc, basic = FALSE)

# 4. Normality tests
shapiro.test(sqr_config$occ)
shapiro.test(rand_config$occ)
qqnorm(sqr_config$occ, main = "Square configuration");qqline(
  sqr_config$occ, col = 4)
qqnorm(rand_config$occ, main = "Random configuration");qqline(
  rand_config$occ, col = 2)

# 4. Plot variances and means
# 4.1. Scatterplot
# 4.1.1 Group occ means
plot(1:14, rep_data2$occ.means[rep_data2$group == "aware"], ylab = "mean occ amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "Nd1 windows amplitudes")
points(1:18, rep_data2$occ.means[rep_data2$group == "unaware"], pch = 20, col = "blue")
abline(h=mean(rep_data2$occ.means[rep_data2$group == "aware"]), col = "red")
abline(h=mean(rep_data2$occ.means[rep_data2$group == "unaware"]), col = "blue")
occ_scat <- ggplot(rep_data_long_alt2, aes(x = Subject, y = occ, colour = group)) + 
  geom_point(aes(shape = configuration)) + 
  geom_smooth(method = "lm", se = F) #ggplot


# 5. Outlier detection and replacement
# 5.1. Identify outliers
# Nd1
occ_alt_box <- ggplot(rep_data_long_alt2, aes(x = configuration, y = occ, fill = configuration)) +
  geom_boxplot() + facet_grid(session~group)
Boxplot(occ ~ configuration * session * group, data = rep_data_long_alt2, 
        colour = group) #car boxplot
bwplot(occ ~ configuration|session, groups = group, data = rep_data_long_alt2, 
       auto.key = T) #lattice boxplot
boxplot(rep_data_long_alt2$occ ~ rep_data_long_alt2$configuration * 
          rep_data_long_alt2$group * rep_data_long_alt2$session) #base boxplot
occ_out <- boxplot(rep_data_long_alt2$occ  ~ rep_data_long_alt2$configuration * 
                     rep_data_long_alt2$group * rep_data_long_alt2$session, plot = FALSE)$out
rep_data_long_alt2[rep_data_long_alt2$occ %in% occ_out,]
# Nd2 left
left_alt_box <- ggplot(rep_data_long_alt2, aes(x = configuration, y = left, 
                                               fill = configuration)) +
  geom_boxplot() + 
  facet_grid(session~group)
boxplot(rep_data_long_alt2$left ~ rep_data_long_alt2$configuration * rep_data_long_alt2$group * 
          rep_data_long_alt2$session)
left_out <- boxplot(rep_data_long_alt2$left ~ rep_data_long_alt2$configuration * rep_data_long_alt2$group * 
                      rep_data_long_alt2$session, plot = FALSE)$out
rep_data_long_alt2[rep_data_long_alt2$left %in% left_out,]
rep_data_long_alt2$left <- rm.outlier(rep_data_long_alt2$left, fill = TRUE)
# Nd2 right
right_alt_box <- ggplot(rep_data_long_alt2, aes(x = configuration, y = right, 
                                                fill = configuration)) +
  geom_boxplot() + 
  facet_grid(session~group)
boxplot(rep_data_long_alt2$right ~ rep_data_long_alt2$configuration * rep_data_long_alt2$group * 
          rep_data_long_alt2$session)
right_out <- boxplot(rep_data_long_alt2$right, plot = FALSE)$out
rep_data_long_alt2[rep_data_long_alt2$right %in% right_out,]
# 5.2. Replace outliers by the mean
rep_data_long_alt2$occ <- rm.outlier(rep_data_long_alt2$occ, fill = TRUE)
rep_data_long_alt2$left <- rm.outlier(rep_data_long_alt2$left, fill = TRUE)
rep_data_long_alt2$right <- rm.outlier(rep_data_long_alt2$right, fill = TRUE)

# 7. Line graphs with error bars
# 7.1 Nd1 - session
nd1_line_session <- ggplot(rep_data_long_alt2, aes(x = session, y = occ, 
                                                   colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_grid(.~group) +
  labs(title = "Occ average reference", x = " session", y = "Nd1 occ amplitude", 
       colour = "configuration")

# 7.3 Nd2 left - session
nd2_left_line_session <- ggplot(rep_data_long_alt2, aes(x = session, y = left, 
                                                    colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~group) +
  labs(title = "Left average reference new grouping", x = " session", y = "Nd2 left amplitude", 
       colour = "configuration")

# 7.5 Nd2 right - session
nd2_right_line_session <- ggplot(rep_data_long_alt2, aes(x = session, y = right, 
                                                     colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~group) +
  labs(title = "Right average reference", x = " session", y = "Nd2 right amplitude", 
       colour = "configuration")

#--------------------------Linear model----------------------------
# 6. Mixed model
# 6.1 Nd1
contrasts(rep_data_long_alt2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long_alt2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long_alt2$group) <- c(-1, 1) # setting contrasts for group
nd1_baseline <- lme(occ ~ 1, random = ~1|Subject/configuration/session, 
                    data = rep_data_long_alt2, method = "ML") #baseline
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
contrasts(rep_data_long_alt2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long_alt2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long_alt2$group) <- c(-1, 1) # setting contrasts for group
nd2_left_baseline <- lme(left ~ 1, random = ~1|Subject/configuration/session, 
                         data = rep_data_long_alt2, method = "ML") #baseline
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

# 6.3 Nd2 right
contrasts(rep_data_long_alt2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long_alt2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long_alt2$group) <- c(-1, 1) # setting contrasts for group
nd2_right_baseline <- lme(right ~ 1, random = ~1|Subject/configuration/session, 
                          data = rep_data_long_alt2, method = "ML") #baseline
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

# 7. Pairwise comparisons for each session and group separately
# Subset groups
aware_subset <- subset(rep_data5, group == "aware")
unaware_subset <- subset(rep_data5, group == "unaware")
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
rep_data_long10 <- subset(rep_data5, configuration == "sqr")
rep_data_long11 <- subset(rep_data_long10, session == 1)
t.test(occ ~ group, data = rep_data_long11, paired = FALSE)

# 9. Differences in left & right between groups for session 1
t.test(left ~ group, data = rep_data_long11, paired = FALSE)
t.test(right ~ group, data = rep_data_long11, paired = FALSE)

# hmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
rep_data_long12 <- subset(rep_data5, configuration == "rand")
rep_data_long13 <- subset(rep_data_long12, group == "aware")
t.test(occ ~ group, data = rep_data_long11, paired = FALSE)
t.test(left ~ session, data = rep_data_long13, paired = TRUE)
t.test(right ~ session, data = rep_data_long13, paired = TRUE)

#------------------------------Plots-------------------------------
# 7.1 Nd1
interaction.plot(rep_data_long_alt2$session, rep_data_long_alt2$configuration,
                 rep_data_long_alt2$occ, col = c("blue", "red"), 
                 ylim = c(-0.8, 0.8), xlab = "Session", 
                 ylab = "mean amplitude", main = "Interaction plot")
interaction.plot(rep_data_long_alt2$group, rep_data_long_alt2$configuration,
                 rep_data_long_alt2$occ, col = c("blue", "red"),
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
interaction.plot(rep_data_long_alt2$session, rep_data_long_alt2$configuration,
                 rep_data_long_alt2$left, col = c("blue", "red"), 
                 ylim = c(-1.8, 0.8), xlab = "Session", 
                 ylab = "mean amplitude", main = "Interaction plot left")
interaction.plot(rep_data_long_alt2$group, rep_data_long_alt2$configuration,
                 rep_data_long_alt2$left, col = c("blue", "red"),
                 ylim = c(-1.8, 0.8), xlab = "Group", 
                 ylab = "mean amplitude", main = "Interaction plot left")
interaction.plot(sqr_config$group, sqr_config$session,
                 sqr_config$left, col = c("cyan3", "deeppink"),
                 ylim = c(-1.8, 0.8), xlab = "Group", 
                 ylab = "mean amplitude", main = "Interaction plot left sqr")
interaction.plot(rand_config$group, rand_config$session,
                 rand_config$left, col = c("cyan3", "deeppink"),
                 ylim = c(-1.8, 0.8), xlab = "Group", 
                 ylab = "mean amplitude", main = "Interaction plot left rand") <- subset(rep_data_long6, session == 1)
t.test(occ ~ group, data = rep_data_long11, paired = FALSE)