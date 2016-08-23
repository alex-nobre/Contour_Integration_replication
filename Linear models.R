# Data import packages
library(xlsx)
# Data manipulation packages
library(outliers)
library(dplyr)
library(tidyr)
library(reshape2)
# Plotting packages
library(lattice)
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

#-------------------------------------------------------------------------------------
# 0. Contrasts
contrasts(rep.data.long2$configuration) <- contr.treatment(2, base = 1)
contrasts(rep.data.long2$group.original) <- contr.treatment(2, base = 2)

# 1. Model 1
occ.model1 <- lm(occ.nd1 ~ configuration + group.original + log.alpha, 
                 data = rep.data.long2)
plot(occ.model1)

# 2. Model 2
occ.model2 <- lm(occ.nd1 ~ configuration + group + log.alpha, 
                 data = rep.data.long2)
plot(occ.model1)

anova(occ.model1)

# Assess assumptions - multicollinearity, normality of residuals, independence

# cross-validation

