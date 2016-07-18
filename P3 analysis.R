
library(pastecs)
library(ggplot2)
library(nlme)
library(lme4)
library(gridExtra)
library(outliers)
library(lattice)
library(car)
library(effsize)

#--------------------------------------------------------------------------------
# 0. Descriptives
# 0.1. By group
by(rep_data_P3_long[,17], list(rep_data_P3_long$configuration, 
                                  rep_data_P3_long$group.original),
   stat.desc, basic = FALSE)

# 0.2. Differences
mean(rep_data_P3[rep_data_P3$group.original == "aware",]$P3_1 -
  rep_data_P3[rep_data_P3$group.original == "aware",]$P3_2)

mean(rep_data_P3[rep_data_P3$group.original == "unaware",]$P3_1 -
       rep_data_P3[rep_data_P3$group.original == "unaware",]$P3_2)

rep_data_P3_long

# 1. Line plots
P3.line <- ggplot(rep_data_P3_long, aes(x = group.original, y = P3,
                                        colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "P3", x = "group", y = "P3 mean amplitude",
       colour = "configuration")

P3.500.line <- ggplot(rep_data_P3_long, aes(x = group, y = P3.500,
                                        colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "P3.500", x = "group", y = "P3.500 mean amplitude",
       colour = "configuration")

# 2. ANOVA
contrasts(rep_data_P3_long$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_P3_long$group.original) <- c(-1, 1) # setting contrasts for group.original
P3_baseline <- lme(P3 ~ 1, random = ~1|Subject/configuration, 
                   data = rep_data_P3_long, method = "ML") #baseline
P3_config <- update(P3_baseline, .~. + configuration)
P3_group.original <- update(P3_config, .~. + group.original)
P3_lme <- update(P3_group.original, .~. + configuration:group.original)
anova(P3_baseline, P3_config, P3_group.original, P3_lme)

# 3. T-test
t.test(rep_data_P3$P3_1, rep_data_P3$P3_2, paired = TRUE)
# 4. Effect size
cohen.d(rep_data_P3$P3_1, rep_data_P3$P3_2, paired = TRUE)
