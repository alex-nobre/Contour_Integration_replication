library(pastecs)
library(ggplot2)
library(nlme)
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
mapply(min, c(rep_data_long2$occ, rep_data_long2$left, rep_data_long2$right))


#---------------------------Exploratory data analysis---------------------------
# 2. Compute variances and means
by(rep_data_long2[,6:8], list(rep_data_long2$configuration, 
                              rep_data_long2$session, rep_data_long2$group.original), 
   stat.desc, basic = FALSE)

# 3. Normality tests
# 3.1. Nd1
shapiro.test(rep_data_long2$occ)
qqnorm(rep_data_long2$occ, main = "Nd1");qqline(rep_data_long2$occ, col = 3)
shapiro.test(sqr_config$occ)
qqnorm(sqr_config$occ, main = "Nd1 square configuration");qqline(
  sqr_config$occ, col = 4)
shapiro.test(rand_config$occ)
qqnorm(rand_config$occ, main = "Nd1 random configuration");qqline(
  rand_config$occ, col = 2)
# 3.2. Nd2 left
shapiro.test(rep_data_long2$left)
shapiro.test(sqr_config$left)
shapiro.test(rand_config$left)
qqnorm(sqr_config$left, main = "Nd2 left square configuration");qqline(
  sqr_config$left, col = 4)
qqnorm(rand_config$left, main = "Nd2 left random configuration");qqline(
  rand_config$left, col = 2)
# 3.3. Nd2 right
shapiro.test(rep_data_long2$right)
shapiro.test(sqr_config$right)
shapiro.test(rand_config$right)
qqnorm(sqr_config$right, main = "Nd2 right square configuration");qqline(
  sqr_config$occ, col = 4)
qqnorm(rand_config$right, main = "Nd2 right random configuration");qqline(
  rand_config$occ, col = 2)
# 3.4 Vectorized operations


#----------------------------------Plots----------------------------------------
# 4. Scatterplot
# 4.1 Group occ means
plot(1:14, rep_data2$occ.means[rep_data2$group == "aware"], ylab = "mean occ amplitude", 
     xlab = "subjects", pch = 20, col = "red", main = "Nd1 windows amplitudes")
points(1:18, rep_data2$occ.means[rep_data2$group == "unaware"], pch = 20, col = "blue")
abline(h=mean(rep_data2$occ.means[rep_data2$group == "aware"]), col = "red")
abline(h=mean(rep_data2$occ.means[rep_data2$group == "unaware"]), col = "blue")
occ_scat <- ggplot(rep_data_long2, aes(x = Subject, y = occ, colour = group)) + 
  geom_point(aes(shape = configuration)) + 
  geom_smooth(method = "lm", se = F) #ggplot
# 4.2 Configuration x session means

# 5 Histograms for normality checks
# 5.1. Nd1
hist(rep_data_long2$occ, prob = T)
lines(density(rep_data_long2$occ), col = 2)
# 5.2. Nd2 left
hist(rep_data_long2$left, prob = T)
lines(density(rep_data_long2$left), col = 3)
# histogram(~x,
# 5.3. Nd2 right
hist(rep_data_long2$right, prob = T)
lines(density(rep_data_long2$right), col = 4)

#           panel = function(x,...){
#             panel.histogram(x,...)
#             panel.mathdensity(dmath = dnorm, col = "red",
#                               args = list(mean=mean(x),sd=sd(x)))
#           },
#           type="density")


# 6. Boxplots for outlier detection
# 6.1. Nd1
occ_box <- ggplot(rep_data_long2, aes(x = configuration, y = occ, fill = configuration)) +
  geom_boxplot() + facet_grid(session~group)
Boxplot(occ ~ configuration * session * group, data = rep_data_long2, 
        colour = group) #car boxplot
bwplot(occ ~ configuration|session, groups = group, data = rep_data_long2, 
       auto.key = T) #lattice boxplot
boxplot(rep_data_long2$occ ~ rep_data_long2$configuration * 
          rep_data_long2$group) #base boxplot
occ_out <- boxplot(rep_data_long2$occ, plot = FALSE)$out
rep_data_long2[rep_data_long2$occ %in% occ_out,]
# 6.2. Nd2 left
left_box <- ggplot(rep_data_long2, aes(x = configuration, y = left,
                                       fill = configuration)) +
  geom_boxplot() + 
  facet_grid(session~group)
boxplot(rep_data_long2$left ~ rep_data_long2$configuration * rep_data_long2$group * 
          rep_data_long2$session)
left_out <- boxplot(rep_data_long2$left ~ rep_data_long2$configuration * rep_data_long2$group * 
                      rep_data_long2$session, plot = FALSE)$out
rep_data_long2[rep_data_long2$left %in% left_out,]
# 6.3. Nd2 right
boxplot(rep_data_long2$right ~ rep_data_long2$configuration * rep_data_long2$group * 
          rep_data_long2$session)
right_out <- boxplot(rep_data_long2$right ~ rep_data_long2$configuration * rep_data_long2$group * 
                       rep_data_long2$session, plot = FALSE)$out
rep_data_long2[rep_data_long2$right %in% right_out,]
# 6.4. Replace outliers by the mean
# rep_data_long2$occ <- rm.outlier(rep_data_long2$occ, fill = TRUE)
# rep_data_long2$left <- rm.outlier(rep_data_long2$left, fill = TRUE)
# rep_data_long2$right <- rm.outlier(rep_data_long2$right, fill = TRUE)


# 7. Line graphs with error bars for comparison of means
# 7.1 Nd1 - session
nd1_line_session <- ggplot(rep_data_long2, aes(x = group, y = occ, 
                                               colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "Occ average reference", x = "group", y = "Nd1 occ amplitude", 
       colour = "configuration")

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

# hmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
rep_data_long8 <- subset(rep_data_long2, configuration == "rand")
rep_data_long9 <- subset(rep_data_long8, group == "aware")
t.test(occ ~ group, data = rep_data_long7, paired = FALSE)
t.test(left ~ session, data = rep_data_long9, paired = TRUE)
t.test(right ~ session, data = rep_data_long9, paired = TRUE)
