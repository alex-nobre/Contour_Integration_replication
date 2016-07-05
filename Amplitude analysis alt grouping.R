library(pastecs)
library(ggplot2)
library(nlme)
library(gridExtra)

#------------------------Preliminary analysis-----------------------
# 0. Create new dataset and change awareness for subjects 16, 33 & 36
rep_data_long3 <- rep_data_long2
rep_data_long3[,8] <- NULL
rep_data_long3$group[rep_data_long3$Subject==16 | rep_data_long3$Subject==33 | 
                       rep_data_long3$Subject==36] <- "unaware"

# 1. Subsets
sqr_config <- subset(rep_data_long3, configuration == "sqr")
rand_config <- subset(rep_data_long3, configuration == "rand")
session_1 <- subset(rep_data_long3, session == 1)
session_2 <- subset(rep_data_long3, session == 2)

# 2. Compute variances and means
# Nd1
by(rep_data_long3$occ, list(rep_data_long3$configuration, 
                            rep_data_long3$session, rep_data_long3$group), 
   stat.desc, basic = FALSE)
# Nd2 left
by(rep_data_long3$left, list(rep_data_long3$configuration, 
                            rep_data_long3$session, rep_data_long3$group), 
   stat.desc, basic = FALSE)
# Nd2 right
by(rep_data_long3$right, list(rep_data_long3$configuration, 
                             rep_data_long3$session, rep_data_long3$group), 
   stat.desc, basic = FALSE)

# 3. Normality tests
shapiro.test(sqr_config$occ)
shapiro.test(rand_config$occ)
qqnorm(sqr_config$occ, main = "Square configuration");qqline(
  sqr_config$occ, col = 4)
qqnorm(rand_config$occ, main = "Random configuration");qqline(
  rand_config$occ, col = 2)

# 4. Plot variances and means
# Scatterplot
# plot(1:32, sqr_config$occ, ylab = "mean amplitude", xlab = "subjects",
#      pch = 21, col = "red", main = "Nd1 windows amplitudes")
# points(1:32, rand_config$occ, col = "blue")
# abline(h=mean(sqr_config$occ), col = "red")
# abline(h=mean(rand_config$occ_rand),col="blue")

# 5. Convert configuration and session number to factors
rep_data_long3$group <- factor(rep_data_long3$group)
rep_data_long3$session <- factor(rep_data_long3$session)
rep_data_long3$configuration <- factor(rep_data_long3$configuration)

#--------------------------Linear model----------------------------
# 6. Mixed model
# 6.1 Nd1
contrasts(rep_data_long3$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long3$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long3$group) <- c(-1, 1) # setting contrasts for group
nd1_baseline <- lme(occ ~ 1, random = ~1|Subject/configuration/session, 
                    data = rep_data_long3, method = "ML") #baseline
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
contrasts(rep_data_long3$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long3$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long3$group) <- c(-1, 1) # setting contrasts for group
nd2_left_baseline <- lme(left ~ 1, random = ~1|Subject/configuration/session, 
                    data = rep_data_long3, method = "ML") #baseline
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
contrasts(rep_data_long3$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long3$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long3$group) <- c(-1, 1) # setting contrasts for group
nd2_right_baseline <- lme(right ~ 1, random = ~1|Subject/configuration/session, 
                         data = rep_data_long3, method = "ML") #baseline
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

#------------------------------Plots-------------------------------
# 7. Line graphs
# 7.1 Nd1 - session
nd1_line_session <- ggplot(rep_data_long3, aes(session, occ, 
                                          colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.2) + 
  labs(x = " session", y = "Nd1 amplitude", colour = "configuration")
# 7.2 Nd1 - group
nd1_line_group <- ggplot(rep_data_long3, aes(group, occ, 
                                          colour = configuration)) + stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.2) + 
  labs(x = "group", y = "Nd1 amplitude", colour = "configuration")
# Both plots 
grid.arrange(nd1_line_session, nd1_line_group)

# 7.3 Nd2 left - session
nd2_left_line_session <- ggplot(rep_data_long3, aes(session, left, 
                                                  colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.2) + 
  labs(x = " session", y = "Nd2 left amplitude", colour = "configuration")
# 7.4 Nd2 left - group
nd2_left_line_group <- ggplot(rep_data_long3, aes(group, left, 
                                                colour = configuration)) + stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.2) + 
  labs(x = "group", y = "Nd2 left amplitude", colour = "configuration")
#Both plots
grid.arrange(nd2_left_line_session, nd2_left_line_group)

# 7.5 Nd2 right - session
nd2_right_line_session <- ggplot(rep_data_long3, aes(session, right, 
                                                       colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.2) + 
  labs(x = " session", y = "Nd2 right amplitude", colour = "configuration")
# 7.6 Nd2 right - group
nd2_right_line_group <- ggplot(rep_data_long3, 
                               aes(group, right, colour = configuration)) + stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.2) + 
  labs(x = "group", y = "Nd2 right amplitude", colour = "configuration")
# Both plots
grid.arrange(nd2_right_line_session, nd2_right_line_group)

# 8 Interaction plots
# 8.1 Nd1
interaction.plot(rep_data_long3$session, rep_data_long3$configuration,
                 rep_data_long3$occ, col = c("blue", "red"), 
                 ylim = c(-0.8, 0.8), xlab = "Session", 
                 ylab = "mean amplitude", main = "Interaction plot")
interaction.plot(rep_data_long3$group, rep_data_long3$configuration,
                 rep_data_long3$occ, col = c("blue", "red"),
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
interaction.plot(rep_data_long3$session, rep_data_long3$configuration,
                 rep_data_long3$left, col = c("blue", "red"), 
                 ylim = c(-1.8, 0.8), xlab = "Session", 
                 ylab = "mean amplitude", main = "Interaction plot left")
interaction.plot(rep_data_long3$group, rep_data_long3$configuration,
                 rep_data_long3$left, col = c("blue", "red"),
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