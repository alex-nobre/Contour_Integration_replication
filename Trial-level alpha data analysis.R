# Data import packages
library(xlsx)
# Data manipulation packages
library(outliers)
library(dplyr)
library(tidyr)
library(reshape2)
# Plotting packages
library(lattice)
library(Hmisc)
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
library(lsmeans)
# Psychophysics packages
library(quickpsy)
library(MPDiR)
library(psyphy)
# Data report packages
library(knitr)

#---------------------------------------------------------------------------------------

# 2. ANOVAs
# 2.1. Set contrasts
contrasts(rep_data_alpha3$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_alpha3$alpha.power) <- c(-1, 1) # setting contrasts for alpha.power
contrasts(rep_data_alpha3$session) <- c(-1, 1) # setting contrasts for session
rep_data_alpha3$Subject <- factor(rep_data_alpha3$Subject)

# 2.2. C1
alpha.C1.baseline <- lme(C1 ~ 1, random = ~1|Subject/configuration/alpha.power, 
                          data = rep_data_alpha3[rep_data_alpha3$session == 1,], method = "ML") #baseline
alpha.C1.config <- update(alpha.C1.baseline, .~. + configuration)
alpha.C1.alpha.power <- update(alpha.C1.config, .~. + alpha.power)
alpha.C1.lme <- update(alpha.C1.alpha.power, .~. + configuration:alpha.power)
anova(alpha.C1.baseline, alpha.C1.config, alpha.C1.alpha.power,
      alpha.C1.lme)

# 2.3. P1
alpha.P1.baseline <- lme(P1 ~ 1, random = ~1|Subject/configuration/alpha.power, 
                         data = rep_data_alpha3, method = "ML") #baseline
alpha.P1.config <- update(alpha.P1.baseline, .~. + configuration)
alpha.P1.alpha.power <- update(alpha.P1.config, .~. + alpha.power)
alpha.P1.lme <- update(alpha.P1.alpha.power, .~. + configuration:alpha.power)
anova(alpha.P1.baseline, alpha.P1.config, alpha.P1.alpha.power,
      alpha.P1.lme)

# 2.4. N1
alpha.N1.baseline <- lme(N1 ~ 1, random = ~1|Subject/configuration/alpha.power, 
                         data = rep_data_alpha3[rep_data_alpha3$session == 1,], method = "ML") #baseline
alpha.N1.config <- update(alpha.N1.baseline, .~. + configuration)
alpha.N1.alpha.power <- update(alpha.N1.config, .~. + alpha.power)
alpha.N1.lme <- update(alpha.N1.alpha.power, .~. + configuration:alpha.power)
anova(alpha.N1.baseline, alpha.N1.config, alpha.N1.alpha.power,
      alpha.N1.lme)

# 2.5. nd1
alpha.nd1.baseline <- lme(occ.nd1 ~ 1, random = ~1|Subject/configuration/alpha.power, 
                    data = rep_data_alpha3[rep_data_alpha3$session == 1,], 
                    method = "ML") #baseline
alpha.nd1.config <- update(alpha.nd1.baseline, .~. + configuration)
alpha.nd1.alpha.power <- update(alpha.nd1.config, .~. + alpha.power)
alpha.nd1.lme <- update(alpha.nd1.alpha.power, .~. + configuration:alpha.power)
anova(alpha.nd1.baseline, alpha.nd1.config, alpha.nd1.alpha.power,
      alpha.nd1.lme)

# 2.5.1. Post-hocs
alpha.nd1.lme <- lme(occ.nd1 ~ configuration * alpha.power, 
                     random = ~1|Subject/configuration/alpha.power, 
                     data = rep_data_alpha3[rep_data_alpha3$session == 1,], 
                     method = "ML")
anova(alpha.nd1.lme)
summary(alpha.nd1.lme)

# 2.5.2. using lsmeans
lsmeans(alpha.nd1.lme, pairwise ~ configuration | alpha.power)

# using multcomp
# 1.
# compute means for all combinations
tmp <- expand.grid(configuration = unique(rep_data_alpha3$configuration),
                   alpha.power = unique(rep_data_alpha3$alpha.power))
x <- model.matrix(~ configuration * alpha.power, data = tmp)
glht(alpha.nd1.lme, linfct = x)

# construct contrast matrix
Tukey <- contrMat(table(rep_data_alpha3$configuration), "Tukey")
K1 <- cbind(Tukey, matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)))
rownames(K1) <- paste(levels(rep_data_alpha3$alpha.power)[1], rownames(K1), 
                      sep = ":")
K2 <- cbind(matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)), Tukey)
rownames(K2) <- paste(levels(rep_data_alpha3$alpha.power)[2], rownames(K2), 
                      sep = ":")
K <- rbind(K1, K2)
colnames(K) <- c(colnames(Tukey), colnames(Tukey))
#test
summary(glht(alpha.nd1.lme, linfct = K %*% x))


# ANOVA TESTING
alpha.nd1.model <- lme(occ.nd1 ~ configuration * alpha.power, 
                       random = ~1|Subject/configuration/alpha.power, 
                          data = rep_data_alpha3, method = "ML")
anova(alpha.nd1.model)

tanova <- aov(occ.nd1 ~ (configuration * alpha.power) +
              Error(Subject/(configuration * alpha.power)), 
              data = rep_data_alpha3)
      
summary(alpha.nd1.baseline)

# Plots
alpha.nd1.line <- ggplot(rep_data_alpha3[rep_data_alpha3$session == 1,], 
                         aes(x = alpha.power, y = occ.nd1, 
                                              colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "Nd1 mean amplitude", x = "alpha power", y = "Nd1 mean amplitude", 
       colour = "configuration")
alpha.nd1.line


#2.6. left nd2
# 1.6.1. ANOVA
alpha.left.nd2.baseline <- lme(left.nd2 ~ 1, 
                          random = ~1|Subject/configuration/alpha.power, 
                          data = rep_data_alpha3[rep_data_alpha3$session == 1,], 
                          method = "ML") #baseline
alpha.left.nd2.config <- update(alpha.left.nd2.baseline, .~. + configuration)
alpha.left.nd2.alpha.power <- update(alpha.left.nd2.config, .~. + alpha.power)
alpha.left.nd2.lme <- update(alpha.left.nd2.alpha.power, .~. + 
                               configuration:alpha.power)
anova(alpha.left.nd2.baseline, alpha.left.nd2.config, alpha.left.nd2.alpha.power,
      alpha.left.nd2.lme)

# 1.6.2. Post-hocs
alpha.left.nd2.model <- lme(left.nd2 ~ configuration * alpha.power, 
                       random = ~1|Subject/configuration/alpha.power, 
                       data = rep_data_alpha3[rep_data_alpha3$session == 1,], 
                       method = "ML")
anova(alpha.left.nd2.lme)

lsmeans(alpha.left.nd2.lme, pairwise ~ configuration | alpha.power)

# 2.6.2. left Nd2
alpha.left.nd2.line <- ggplot(rep_data_alpha3[rep_data_alpha3$session == 1,], 
                              aes(x = alpha.power, y = left.nd2, 
                                                   colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "Left Nd2 mean amplitude", x = "alpha power", 
       y = "Left Nd2 mean amplitude", 
       colour = "configuration")
alpha.left.nd2.line

# 2.7. right nd2
alpha.right.nd2.baseline <- lme(right.nd2 ~ 1, 
                                random = ~1|Subject/configuration/alpha.power, 
                          data = rep_data_alpha3[rep_data_alpha3$session == 1,], 
                          method = "ML") #baseline
alpha.right.nd2.config <- update(alpha.right.nd2.baseline, .~. + configuration)
alpha.right.nd2.alpha.power <- update(alpha.right.nd2.config, .~. + alpha.power)
alpha.right.nd2.lme <- update(alpha.right.nd2.alpha.power, .~. + 
                                configuration:alpha.power)
anova(alpha.right.nd2.baseline, alpha.right.nd2.config, alpha.right.nd2.alpha.power,
      alpha.right.nd2.lme)

summary(alpha.right.nd2.lme)

# Post-hocs
alpha.right.nd2.model <- lme(right.nd2 ~ configuration * alpha.power, 
                            random = ~1|Subject/configuration/alpha.power, 
                            data = rep_data_alpha3[rep_data_alpha3$session == 1,], 
                            method = "ML")
anova(alpha.right.nd2.lme)

lsmeans(alpha.right.nd2.lme, pairwise ~ configuration | alpha.power)

# 2.7.2. Plot
alpha.right.nd2.line <- ggplot(rep_data_alpha3[rep_data_alpha3$session == 1,], 
                               aes(x = alpha.power, y = right.nd2, 
                                                    colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "Right Nd2 mean amplitude", x = "alpha power",
       y = "Left Nd2 mean amplitude", 
       colour = "configuration")
alpha.right.nd2.line

# 2.8. RL nd2
alpha.RL.nd2.baseline <- lme(RL.nd2 ~ 1, 
                             random = ~1|Subject/configuration/alpha.power, 
                          data = rep_data_alpha3[rep_data_alpha3$session == 1,], 
                          method = "ML") #baseline
alpha.RL.nd2.config <- update(alpha.RL.nd2.baseline, .~. + configuration)
alpha.RL.nd2.alpha.power <- update(alpha.RL.nd2.config, .~. + alpha.power)
alpha.RL.nd2.lme <- update(alpha.RL.nd2.alpha.power, .~. + 
                             configuration:alpha.power)
anova(alpha.RL.nd2.baseline, alpha.RL.nd2.config, alpha.RL.nd2.alpha.power,
      alpha.RL.nd2.lme)

# 2.8.2. Post-hocs
lsmeans(alpha.RL.nd2.lme, pairwise ~ configuration | alpha.power)

# 2.8.3. Plots
alpha.RL.nd2.line <- ggplot(rep_data_alpha3[rep_data_alpha3$session == 1,], 
                               aes(x = alpha.power, y = RL.nd2, 
                                   colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "RL Nd2 mean amplitude", x = "alpha power",
       y = "Left Nd2 mean amplitude", 
       colour = "configuration")
alpha.RL.nd2.line

# 2.9. N2
alpha.N2.baseline <- lme(N2 ~ 1, random = ~1|Subject/configuration/alpha.power, 
                             data = rep_data_alpha3, method = "ML") #baseline
alpha.N2.config <- update(alpha.N2.baseline, .~. + configuration)
alpha.N2.alpha.power <- update(alpha.N2.config, .~. + alpha.power)
alpha.N2.lme <- update(alpha.N2.alpha.power, .~. + configuration:alpha.power)
anova(alpha.N2.baseline, alpha.N2.config, alpha.N2.alpha.power,
      alpha.N2.lme)
# 2.10. LP
alpha.LP.baseline <- lme(LP ~ 1, random = ~1|Subject/configuration/alpha.power, 
                         data = rep_data_alpha3, method = "ML") #baseline
alpha.LP.config <- update(alpha.LP.baseline, .~. + configuration)
alpha.LP.alpha.power <- update(alpha.LP.config, .~. + alpha.power)
alpha.LP.lme <- update(alpha.LP.alpha.power, .~. + configuration:alpha.power)
anova(alpha.LP.baseline, alpha.LP.config, alpha.LP.alpha.power,
      alpha.LP.lme)

by(rep_data_alpha3$LP, rep_data_alpha3$configuration, 
   stat.desc, basic = FALSE)

# 3. ANOVAs With session as factor
contrasts(rep_data_alpha3$session) <- c(-1, 1) # setting contrasts for session
# 3.4. nd1
alpha.nd1.baseline <- lme(occ.nd1 ~ 1, 
                               random = ~1|Subject/configuration/session/alpha.power, 
                               data = rep_data_alpha3, method = "ML") #baseline
alpha.nd1.config <- update(alpha.nd1.baseline, .~. + configuration)
alpha.nd1.session <- update(alpha.nd1.config, .~. + session)
alpha.nd1.alpha.power <- update(alpha.nd1.session, .~. + alpha.power)
alpha.nd1.config.session <- update(alpha.nd1.alpha.power, .~. + 
                                          configuration:session)
alpha.nd1.session.alpha.power <- update(alpha.nd1.config.session, .~. + 
                                               session:alpha.power)
alpha.nd1.config.alpha.power <- update(alpha.nd1.session.alpha.power, .~. + 
                                              configuration:alpha.power)
alpha.nd1.lme <- update(alpha.nd1.config.alpha.power, .~. + 
                               configuration:session:alpha.power)
anova(alpha.nd1.baseline, alpha.nd1.config, alpha.nd1.session, 
      alpha.nd1.alpha.power, alpha.nd1.config.session, 
      alpha.nd1.session.alpha.power, alpha.nd1.config.alpha.power, 
      alpha.nd1.lme)

# 3.5. left nd2
alpha.left.nd2.baseline <- lme(left.nd2 ~ 1, 
                               random = ~1|Subject/configuration/session/alpha.power, 
                         data = rep_data_alpha3, method = "ML") #baseline
alpha.left.nd2.config <- update(alpha.left.nd2.baseline, .~. + configuration)
alpha.left.nd2.session <- update(alpha.left.nd2.config, .~. + session)
alpha.left.nd2.alpha.power <- update(alpha.left.nd2.session, .~. + alpha.power)
alpha.left.nd2.config.session <- update(alpha.left.nd2.alpha.power, .~. + 
                                          configuration:session)
alpha.left.nd2.session.alpha.power <- update(alpha.left.nd2.config.session, .~. + 
                                               session:alpha.power)
alpha.left.nd2.config.alpha.power <- update(alpha.left.nd2.session.alpha.power, .~. + 
                                              configuration:alpha.power)
alpha.left.nd2.lme <- update(alpha.left.nd2.config.alpha.power, .~. + 
                               configuration:session:alpha.power)
anova(alpha.left.nd2.baseline, alpha.left.nd2.config, alpha.left.nd2.session, 
      alpha.left.nd2.alpha.power, alpha.left.nd2.config.session, 
      alpha.left.nd2.session.alpha.power, alpha.left.nd2.config.alpha.power, 
      alpha.left.nd2.lme)

# 3.6. right nd2
alpha.right.nd2.baseline <- lme(right.nd2 ~ 1, 
                               random = ~1|Subject/configuration/session/alpha.power, 
                               data = rep_data_alpha3, method = "ML") #baseline
alpha.right.nd2.config <- update(alpha.right.nd2.baseline, .~. + configuration)
alpha.right.nd2.session <- update(alpha.right.nd2.config, .~. + session)
alpha.right.nd2.alpha.power <- update(alpha.right.nd2.session, .~. + alpha.power)
alpha.right.nd2.config.session <- update(alpha.right.nd2.alpha.power, .~. + 
                                          configuration:session)
alpha.right.nd2.session.alpha.power <- update(alpha.right.nd2.config.session, .~. + 
                                               session:alpha.power)
alpha.right.nd2.config.alpha.power <- update(alpha.right.nd2.session.alpha.power, .~. + 
                                              configuration:alpha.power)
alpha.right.nd2.lme <- update(alpha.right.nd2.config.alpha.power, .~. + 
                               configuration:session:alpha.power)
anova(alpha.right.nd2.baseline, alpha.right.nd2.config, alpha.right.nd2.session, 
      alpha.right.nd2.alpha.power, alpha.right.nd2.config.session, 
      alpha.right.nd2.session.alpha.power, alpha.right.nd2.config.alpha.power, 
      alpha.right.nd2.lme)

# 3. T-test
# nd1
t.test(rep_data2$low.alpha.occ.nd1.sqr_1, 
       rep_data2$low.alpha.occ.nd1.rand_1, 
       paired = TRUE)

cohen.d(rep_data2$low.alpha.occ.nd1.sqr_1, 
        rep_data2$low.alpha.occ.nd1.rand_1)

t.test(rep_data2$low.alpha.occ.nd1.sqr_2, 
       rep_data2$low.alpha.occ.nd1.rand_2, 
       paired = TRUE)

cohen.d(rep_data2$low.alpha.occ.nd1.sqr_2, 
        rep_data2$low.alpha.occ.nd1.rand_2)

t.test(rep_data2$high.alpha.occ.nd1.sqr_1, 
       rep_data2$high.alpha.occ.nd1.rand_1, 
       paired = TRUE)

cohen.d(rep_data2$high.alpha.occ.nd1.sqr_1, 
        rep_data2$high.alpha.occ.nd1.rand_1)


t.test(rep_data2$high.alpha.occ.nd1.sqr_2, 
       rep_data2$high.alpha.occ.nd1.rand_2, 
       paired = TRUE)

cohen.d(rep_data2$high.alpha.occ.nd1.sqr_2, 
        rep_data2$high.alpha.occ.nd1.rand_2)

# left nd2
t.test(rep_data2$low.alpha.left.nd2.sqr_1, 
       rep_data2$low.alpha.left.nd2.rand_1, 
       paired = TRUE)

cohen.d(rep_data2$low.alpha.left.nd2.sqr_1, 
        rep_data2$low.alpha.left.nd2.rand_1)

t.test(rep_data2$low.alpha.left.nd2.sqr_2, 
       rep_data2$low.alpha.left.nd2.rand_2, 
       paired = TRUE)

cohen.d(rep_data2$low.alpha.left.nd2.sqr_2, 
        rep_data2$low.alpha.left.nd2.rand_2)

t.test(rep_data2$high.alpha.left.nd2.sqr_1, 
       rep_data2$high.alpha.left.nd2.rand_1, 
       paired = TRUE)

cohen.d(rep_data2$high.alpha.left.nd2.sqr_1, 
        rep_data2$high.alpha.left.nd2.rand_1)


t.test(rep_data2$high.alpha.left.nd2.sqr_2, 
       rep_data2$high.alpha.left.nd2.rand_2, 
       paired = TRUE)

cohen.d(rep_data2$high.alpha.left.nd2.sqr_2, 
        rep_data2$high.alpha.left.nd2.rand_2)


# right nd2
t.test(rep_data2$low.alpha.right.nd2.sqr_1, 
       rep_data2$low.alpha.right.nd2.rand_1, 
       paired = TRUE)

cohen.d(rep_data2$low.alpha.right.nd2.sqr_1, 
        rep_data2$low.alpha.right.nd2.rand_1)

t.test(rep_data2$low.alpha.right.nd2.sqr_2, 
       rep_data2$low.alpha.right.nd2.rand_2, 
       paired = TRUE)

cohen.d(rep_data2$low.alpha.right.nd2.sqr_2, 
        rep_data2$low.alpha.right.nd2.rand_2)

t.test(rep_data2$high.alpha.right.nd2.sqr_1, 
       rep_data2$high.alpha.right.nd2.rand_1, 
       paired = TRUE)

cohen.d(rep_data2$high.alpha.right.nd2.sqr_1, 
        rep_data2$high.alpha.right.nd2.rand_1)


t.test(rep_data2$high.alpha.right.nd2.sqr_2, 
       rep_data2$high.alpha.right.nd2.rand_2, 
       paired = TRUE)

cohen.d(rep_data2$high.alpha.right.nd2.sqr_2, 
        rep_data2$high.alpha.right.nd2.rand_2)

# Group comparison
t.test(rep_data2$low.alpha.occ.nd1.sqr_1, 
       rep_data2$high.alpha.occ.nd1.sqr_1, 
       paired = TRUE)


t.test(rep_data2$low.alpha.occ.nd1.sqr_2, 
       rep_data2$high.alpha.occ.nd1.sqr_2, 
       paired = TRUE)

t.test(rep_data2$low.alpha.occ.nd1.rand_1, 
       rep_data2$high.alpha.occ.nd1.rand_1, 
       paired = TRUE)

t.test(rep_data2$low.alpha.occ.nd1.rand_2, 
       rep_data2$high.alpha.occ.nd1.rand_2, 
       paired = TRUE)

facet_grid(.~session) +
  
# Multiplot
rep_data_alpha3[rep_data_alpha3$session == 1,] %>%
   gather(occ.nd1, left.nd2, right.nd2, RL.nd2, 
          key = "var", value = "value") %>% 
  ggplot(aes(x = alpha.power, y = value, color = configuration)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()