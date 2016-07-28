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
# Psychophysics packages
library(quickpsy)
library(MPDiR)
library(psyphy)
# Data report packages
library(knitr)

#---------------------------------------------------------------------------------------
# 1. Linear models
# 1.4. Nd1 (P2)
# Line plot
jpeg(file = "./Plots/Pitts ROIs/nd1line.jpeg")
low.alpha.nd1.line <- ggplot(rep_data_long3, aes(x = group.original, y = low.alpha.occ.nd1, 
                                       colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "Low alpha Nd1 mean amplitude", x = "group", y = "Nd1 mean amplitude", 
       colour = "configuration")
nd1.line
dev.off()

# ANOVA
nd1_baseline <- lme(low.alpha.occ.nd1 ~ 1, random = ~1|Subject/configuration, 
                    data = rep_data_long2, method = "ML") #baseline
nd1_config <- update(nd1_baseline, .~. + configuration)
nd1_group.original <- update(nd1_session, .~. + group.original)
nd1_config_session <- update(nd1_group.original, .~. + configuration:session)
nd1_session_group.original <- update(nd1_config_session, .~. + session:group.original)
nd1_config_group.original <- update(nd1_session_group.original, .~. + configuration:group.original)
nd1_lme <- update(nd1_config_group.original, .~. + configuration:session:group.original)
anova(nd1_baseline, nd1_config, nd1_session, nd1_group.original, nd1_config_session,
      nd1_session_group.original, nd1_config_group.original, nd1_lme)
summary(nd1_lme)



# 1. T-test
t.test(rep_data2$low.alpha.sqr1.right.nd2, rep_data2$low.alpha.rand1.right.nd2,
       paired = TRUE)

t.test(rep_data2$high.alpha.sqr1.right.nd2, rep_data2$high.alpha.rand1.right.nd2,
       paired = TRUE)

t.test(rep_data2$low.alpha.sqr1.right.nd2, rep_data2$high.alpha.sqr1.right.nd2,
       paired = FALSE)

t.test(rep_data2$low.alpha.sqr1.occ.nd1, rep_data2$low.alpha.rand1.occ.nd1, paired = TRUE)

t.test(rep_data2$low.alpha.sqr1.occ.nd1, rep_data2$low.alpha.rand1.occ.nd1, paired = TRUE)

t.test(rep_data2$high.alpha.sqr1.occ.nd1, rep_data2$high.alpha.rand1.occ.nd1, paired = TRUE)

cohen.d(rep_data2$high.alpha.sqr1.occ.nd1, 
        rep_data2$high.alpha.rand1.occ.nd1)

cohen.d(rep_data2$low.alpha.sqr1.occ.nd1, rep_data2$low.alpha.rand1.occ.nd1)