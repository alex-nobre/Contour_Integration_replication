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

# Occ
# 1. Model occ 1
occ.model1 <- lm(occ.nd1 ~ configuration + group.original + log.alpha, 
                 data = rep.data.long2)
plot(occ.model1)

# 2. Model occ 2
occ.model2 <- lm(occ.nd1 ~ configuration + group + log.alpha, 
                 data = rep.data.long2)
plot(occ.model1)

anova(occ.model1)

# 2. Left nd2
left.nd2.model1 <- lm(left.nd2 ~ configuration + log.alpha, 
                      data = rep.data.long2[rep.data.long2$session == 1,])
dot.colors <- ifelse(rep.data.long2[rep.data.long2$session == 1,]$configuration == 
                       "rand", "red", "blue")
layout(rbind(1,2), heights=c(5,1))
plot(x = rep.data.long2[rep.data.long2$session == "1",]$log.alpha,
     y = rep.data.long2[rep.data.long2$session == "1",]$left.nd2,
     col = dot.colors, pch = 16, main = "log alpha x left Nd2 amplitude",
     xlab = "log alpha", ylab = "Mean amplitude")
curve(cbind(1,0,x) %*% coef(left.nd2.model1), add = TRUE, col = "red")
curve(cbind(1,1,x) %*% coef(left.nd2.model1), add = TRUE, col = "blue")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = c("rand", "sqr"),
       col = c("red", "blue"),
       pch = 16)
par(defaults)

# layout(rbind(1,2), heights=c(5,1))
# plot(x = questionnaire.ERPs$log.alpha.sqr.1,
#      y = questionnaire.ERPs$left.sqr.nd2_1,
#      col = "blue", pch = 16, main = "log alpha x left Nd2 amplitude",
#      xlab = "log alpha", ylab = "Mean amplitude")
# points(x = questionnaire.ERPs$log.alpha.rand.1,
#        y = questionnaire.ERPs$left.rand.nd2_1,
#        col = "red", pch = 16)
# curve(cbind(1,x) %*% coef(lm(left.sqr.nd2_1 ~ log.alpha.sqr.1, 
#                              data = questionnaire.ERPs)), 
#       add = TRUE, col = "blue")
# curve(cbind(1,x) %*% coef(lm(left.rand.nd2_1 ~ log.alpha.rand.1, 
#                              data = questionnaire.ERPs)),
#       add = TRUE, col = "red")
# par(mar=c(0, 0, 0, 0))
# plot.new()
# legend("center", legend = c("sqr", "rand"),
#        col = c("blue", "red"),
#        pch = 16)
# par(defaults)

# 2.1. Model 1
left.nd2.model1 <- lm(left.nd2 ~ configuration + log.alpha, 
                       data = rep.data.long2)
dot.colors <- ifelse(rep.data.long2$configuration == "rand", "red", "blue")
plot(rep.data.long2$log.alpha, rep.data.long2$left.nd2, 
     col = dot.colors, pch = 20)
curve(cbind(1,1,x) %*% coef(left.nd2.model1), add = TRUE, col = "red")
curve(cbind(1,2,x) %*% coef(left.nd2.model1), add = TRUE, col = "blue")

anova(left.nd2.model1)

# 2.2. Model 2
left.nd2.model2 <- lm(left.nd2 ~ configuration + group.original + log.alpha + threshold, 
                      data = rep.data.long2)
plot(left.nd2.model2)

anova(left.nd2.model2)

# 3. Right nd2
# Correct positive skew with square root transform on inverse values
rep.data.long2$transf.right.nd2 <- sqrt(max(rep.data.long2$right.nd2) - 
                                          rep.data.long2$right.nd2)

questionnaire.ERPs$transf.right.sqr.nd2_1 <- sqrt(max(questionnaire.ERPs$right.sqr.nd2_1) - 
                                          questionnaire.ERPs$right.sqr.nd2_1)


# 3.1. Model 1
right.nd2.model1 <- lm(transf.right.nd2 ~ configuration + log.alpha + 
                         configuration * log.alpha, 
                      data = rep.data.long2)
dot.colors <- ifelse(rep.data.long2$configuration == "rand", "red", "blue")
plot(rep.data.long2$log.alpha, rep.data.long2$transf.right.nd2, 
     col = dot.colors, pch = 20)
curve(cbind(1,1,x, 1*x) %*% coef(right.nd2.model1), add = TRUE, col = "red")
curve(cbind(1,2,x, 2*x) %*% coef(right.nd2.model1), add = TRUE, col = "blue")

right.nd2.model1 <- lm(transf.right.sqr.nd2_1 ~ freq.4_1, 
                       data = questionnaire.ERPs)

right.diff.model1 <- lm(right.diff ~ mean.log.alpha, 
                      data = rep.data.long2[rep.data.long2$session == 1,])

anova(right.diff.model)

# 4. Confidence ratings

plot(right.nd2.model1)

anova(right.nd2.model1)

resid(right.nd2.model1)

plot(right.nd2.model1$residuals)

plot(rep.data.long2$log.alpha, rep.data.long2$right.nd2)
abline(lm(rep.data.long2$right.nd2 ~ rep.data.long2$log.alpha))


anova(right.nd2.model1)

# 3.2. Model 2
hist(sqrt(max(rep.data.long2$right.nd2) - rep.data.long2$right.nd2))
rep.data.long2$log.right.nd2 < log()
right.nd2.model2 <- lm(log(right.nd2) ~ configuration + group.original + 
                         log.alpha * threshold, 
                       data = rep.data.long2)

plot(right.nd2.model2)
anova(right.nd2.model2)

plot(rep.data.long2$log.alpha, rep.data.long2$right.nd2)
abline(lm(rep.data.long2$right.nd2 ~ rep.data.long2$log.alpha))

plot(rep.data.long2$threshold, rep.data.long2$right.nd2)
abline(lm(rep.data.long2$right.nd2 ~ rep.data.long2$threshold))

# 3.3. Model 3
right.nd2.model3 <- lm(right.nd2 ~ configuration + group.original + 
                         log.alpha * RT.mean, 
                       data = rep.data.long2)

plot(right.nd2.model3)
anova(right.nd2.model3)

plot(rep.data.long2$log.alpha, rep.data.long2$right.nd2)
abline(lm(rep.data.long2$right.nd2 ~ rep.data.long2$log.alpha))

plot(rep.data.long2$RT.mean, rep.data.long2$right.nd2)
abline(lm(rep.data.long2$right.nd2 ~ rep.data.long2$RT.mean))

# 3.4. Model 4
right.nd2.model4 <- lm(right.nd2 ~ configuration + group.original + 
                         log.alpha + Pc, 
                       data = rep.data.long2)

anova(right.nd2.model4)


#----------------------------------Alpha power-----------------------------------
alpha.model1 <- lm(rep.data.long2$log.alpha ~ rep.data.long2$RT)

anova(alpha.model1)

alpha.model2 <- lm(rep.data.long2$Pc ~ rep.data.long2$log.alpha)

anova(alpha.model2)

plot(rep.data.long2$log.alpha, rep.data.long2$Pc)
abline(lm(rep.data.long2$Pc ~ rep.data.long2$log.alpha))

# Assess assumptions - multicollinearity, normality of residuals, independence

# cross-validation

