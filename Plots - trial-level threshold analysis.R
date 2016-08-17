
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

#-----------------------------------------------------------------------------------

#1. Parameters for plots
defaults <- par()
labels <- c("sqr low bin", "sqr high bin", "rand low bin", 
            "rand high bin")
x.points <- seq(-100, 596, by = 4)


# 2. bin power x configuration
# 2.1. C1
# 2.1.1. Session 1
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.bin.grand.averages.sqr1[[1]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.5),
     main = "C1 bin x configuration session 1")
lines(x.points, high.bin.grand.averages.sqr1[[1]], type = "l", col = "blue")
lines(x.points, low.bin.grand.averages.rand1[[1]], type = "l", col = "green")
lines(x.points, high.bin.grand.averages.rand1[[1]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)
# 2.1.2. Session 2
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.bin.grand.averages.sqr2[[1]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.5),
     main = "C1 bin x configuration session 2")
lines(x.points, high.bin.grand.averages.sqr2[[1]], type = "l", col = "blue")
lines(x.points, low.bin.grand.averages.rand2[[1]], type = "l", col = "green")
lines(x.points, high.bin.grand.averages.rand2[[1]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)

# P1
# 2.2.1. Session 1
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.bin.grand.averages.sqr1[[2]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.5),
     main = "P1 bin x configuration session 1")
lines(x.points, high.bin.grand.averages.sqr1[[2]], type = "l", col = "blue")
lines(x.points, low.bin.grand.averages.rand1[[2]], type = "l", col = "green")
lines(x.points, high.bin.grand.averages.rand1[[2]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)
# 2.2.2. Session 2
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.bin.grand.averages.sqr2[[2]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.5),
     main = "P1 bin x configuration session 2")
lines(x.points, high.bin.grand.averages.sqr2[[2]], type = "l", col = "blue")
lines(x.points, low.bin.grand.averages.rand2[[2]], type = "l", col = "green")
lines(x.points, high.bin.grand.averages.rand2[[2]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)

# 2.3. N1
# 2.3.1. Session 1
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.bin.grand.averages.sqr1[[3]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.5),
     main = "N1 bin x configuration session 1")
lines(x.points, high.bin.grand.averages.sqr1[[3]], type = "l", col = "blue")
lines(x.points, low.bin.grand.averages.rand1[[3]], type = "l", col = "green")
lines(x.points, high.bin.grand.averages.rand1[[3]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)
# 2.3.2. Session 2
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.bin.grand.averages.sqr2[[3]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.5),
     main = "N1 bin x configuration session 2")
lines(x.points, high.bin.grand.averages.sqr2[[3]], type = "l", col = "blue")
lines(x.points, low.bin.grand.averages.rand2[[3]], type = "l", col = "green")
lines(x.points, high.bin.grand.averages.rand2[[3]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)

# 2.4. Occ
# 2.4.1. Session 1
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.bin.grand.averages.sqr1[[4]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.5),
     main = "Nd1 bin x configuration session 1")
lines(x.points, high.bin.grand.averages.sqr1[[4]], type = "l", col = "blue")
lines(x.points, low.bin.grand.averages.rand1[[4]], type = "l", col = "green")
lines(x.points, high.bin.grand.averages.rand1[[4]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)
# 2.4.2. Session 2
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.bin.grand.averages.sqr2[[4]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.5),
     main = "Nd1 bin x configuration session 2")
lines(x.points, high.bin.grand.averages.sqr2[[4]], type = "l", col = "blue")
lines(x.points, low.bin.grand.averages.rand2[[4]], type = "l", col = "green")
lines(x.points, high.bin.grand.averages.rand2[[4]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)

# 2.5. Left
# 2.5.1. Session 1
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.bin.grand.averages.sqr1[[5]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.5),
     main = "Left nd2 bin x configuration session 1")
lines(x.points, high.bin.grand.averages.sqr1[[5]], type = "l", col = "blue")
lines(x.points, low.bin.grand.averages.rand1[[5]], type = "l", col = "green")
lines(x.points, high.bin.grand.averages.rand1[[5]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)
# 2.5.2. Session 2
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.bin.grand.averages.sqr2[[5]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.5),
     main = "Left nd2 bin x configuration session 2")
lines(x.points, high.bin.grand.averages.sqr2[[5]], type = "l", col = "blue")
lines(x.points, low.bin.grand.averages.rand2[[5]], type = "l", col = "green")
lines(x.points, high.bin.grand.averages.rand2[[5]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)

# 2.6. Right
# 2.6.1. Session 1
pdf("Right nd2 bin x config ses 1.pdf")
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.bin.grand.averages.sqr1[[6]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.5),
     main = "Right nd2 bin x configuration session 1")
lines(x.points, high.bin.grand.averages.sqr1[[6]], type = "l", col = "blue")
lines(x.points, low.bin.grand.averages.rand1[[6]], type = "l", col = "green")
lines(x.points, high.bin.grand.averages.rand1[[6]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)
dev.off()
# 2.6.2. Session 2
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.bin.grand.averages.sqr2[[6]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.5),
     main = "Right nd2 bin x configuration session 2")
lines(x.points, high.bin.grand.averages.sqr2[[6]], type = "l", col = "blue")
lines(x.points, low.bin.grand.averages.rand2[[6]], type = "l", col = "green")
lines(x.points, high.bin.grand.averages.rand2[[6]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)


# 2.7. Right-left
# 2.7.1. Session 1
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.bin.grand.averages.sqr1[[7]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.5),
     main = "RL nd2 bin x configuration session 1")
lines(x.points, high.bin.grand.averages.sqr1[[7]], type = "l", col = "blue")
lines(x.points, low.bin.grand.averages.rand1[[7]], type = "l", col = "green")
lines(x.points, high.bin.grand.averages.rand1[[7]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)
# 2.7.2. Session 2
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.bin.grand.averages.sqr2[[7]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.5),
     main = "RL nd2 bin x configuration session 2")
lines(x.points, high.bin.grand.averages.sqr2[[7]], type = "l", col = "blue")
lines(x.points, low.bin.grand.averages.rand2[[7]], type = "l", col = "green")
lines(x.points, high.bin.grand.averages.rand2[[7]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)


# 2.8. N2
# 2.8.1. Session 1
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.bin.grand.averages.sqr1[[8]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.5),
     main = "N2 bin x configuration session 1")
lines(x.points, high.bin.grand.averages.sqr1[[8]], type = "l", col = "blue")
lines(x.points, low.bin.grand.averages.rand1[[8]], type = "l", col = "green")
lines(x.points, high.bin.grand.averages.rand1[[8]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)
# 2.8.2. Session 2
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.bin.grand.averages.sqr2[[8]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.5),
     main = "N2 bin x configuration session 2")
lines(x.points, high.bin.grand.averages.sqr2[[8]], type = "l", col = "blue")
lines(x.points, low.bin.grand.averages.rand2[[8]], type = "l", col = "green")
lines(x.points, high.bin.grand.averages.rand2[[8]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)

# 2.10. LP
# 2.10.1. Session 1
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.bin.grand.averages.sqr1[[10]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.5),
     main = "LP bin x configuration session 1")
lines(x.points, high.bin.grand.averages.sqr1[[10]], type = "l", col = "blue")
lines(x.points, low.bin.grand.averages.rand1[[10]], type = "l", col = "green")
lines(x.points, high.bin.grand.averages.rand1[[10]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)
# 2.10.2. Session 2
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.bin.grand.averages.sqr2[[10]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-2.5, 1.5),
     main = "LP bin x configuration session 2")
lines(x.points, high.bin.grand.averages.sqr2[[10]], type = "l", col = "blue")
lines(x.points, low.bin.grand.averages.rand2[[10]], type = "l", col = "green")
lines(x.points, high.bin.grand.averages.rand2[[10]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)

# 3 By group
# 3.1. Get indices for list of grand averages
low.bin.list.indices <- which(rep_data2$Subject %in% low.bin.group)
high.bin.list.indices <- which(rep_data2$Subject %in% high.bin.group)

# 3.2. Create groups for plotting by condition
# Low bin trial-level, sqr1 
low.group.low.bin.sqr1 <- lapply(lapply(split.ROIs(low.bin.segments.sqr1.corrected[low.bin.list.indices]),
                                        function(l)do.call(rbind, l)), colMeans)
high.group.low.bin.sqr1 <- lapply(lapply(split.ROIs(low.bin.segments.sqr1.corrected[high.bin.list.indices]),
                                         function(l)do.call(rbind, l)), colMeans)
# high bin trial-level, sqr1
low.group.high.bin.sqr1 <- lapply(lapply(split.ROIs(high.bin.segments.sqr1.corrected[low.bin.list.indices]),
                                         function(l)do.call(rbind, l)), colMeans)
high.group.high.bin.sqr1 <- lapply(lapply(split.ROIs(high.bin.segments.sqr1.corrected[high.bin.list.indices]),
                                          function(l)do.call(rbind, l)), colMeans)
# Low bin trial-level, sqr2 
low.group.low.bin.sqr2 <- lapply(lapply(split.ROIs(low.bin.segments.sqr2.corrected[low.bin.list.indices]),
                                        function(l)do.call(rbind, l)), colMeans)
high.group.low.bin.sqr2 <- lapply(lapply(split.ROIs(low.bin.segments.sqr2.corrected[high.bin.list.indices]),
                                         function(l)do.call(rbind, l)), colMeans)
# high bin trial-level, sqr2
low.group.high.bin.sqr2 <- lapply(lapply(split.ROIs(high.bin.segments.sqr2.corrected[low.bin.list.indices]),
                                         function(l)do.call(rbind, l)), colMeans)
high.group.high.bin.sqr2 <- lapply(lapply(split.ROIs(high.bin.segments.sqr2.corrected[high.bin.list.indices]),
                                          function(l)do.call(rbind, l)), colMeans)
# Low bin trial-level, rand1 
low.group.low.bin.rand1 <- lapply(lapply(split.ROIs(low.bin.segments.rand1.corrected[low.bin.list.indices]),
                                         function(l)do.call(rbind, l)), colMeans)
high.group.low.bin.rand1 <- lapply(lapply(split.ROIs(low.bin.segments.rand1.corrected[high.bin.list.indices]),
                                          function(l)do.call(rbind, l)), colMeans)
# high bin trial-level, rand1
low.group.high.bin.rand1 <- lapply(lapply(split.ROIs(high.bin.segments.rand1.corrected[low.bin.list.indices]),
                                          function(l)do.call(rbind, l)), colMeans)
high.group.high.bin.rand1 <- lapply(lapply(split.ROIs(high.bin.segments.rand1.corrected[high.bin.list.indices]),
                                           function(l)do.call(rbind, l)), colMeans)
# Low bin trial-level, rand2 
low.group.low.bin.rand2 <- lapply(lapply(split.ROIs(low.bin.segments.rand2.corrected[low.bin.list.indices]),
                                         function(l)do.call(rbind, l)), colMeans)
high.group.low.bin.rand2 <- lapply(lapply(split.ROIs(low.bin.segments.rand2.corrected[high.bin.list.indices]),
                                          function(l)do.call(rbind, l)), colMeans)
# high bin trial-level, rand2
low.group.high.bin.rand2 <- lapply(lapply(split.ROIs(high.bin.segments.rand2.corrected[low.bin.list.indices]),
                                          function(l)do.call(rbind, l)), colMeans)
high.group.high.bin.rand2 <- lapply(lapply(split.ROIs(high.bin.segments.rand2.corrected[high.bin.list.indices]),
                                           function(l)do.call(rbind, l)), colMeans)



# 3.3. Occ
# 3.3.1. Session 1
# 3.3.1.1. Low-bin group
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.group.low.bin.sqr1[[4]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-3, 1.5),
     main = "Nd1 bin x config session 1 low bin group")
lines(x.points, low.group.high.bin.sqr1[[4]], type = "l", col = "blue")
lines(x.points, low.group.low.bin.rand1[[4]], type = "l", col = "green")
lines(x.points, low.group.high.bin.rand1[[4]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)
# # 3.3.1.2. High-bin group
layout(rbind(1,2), heights=c(5,1))
plot(x.points, high.group.low.bin.sqr1[[4]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-3, 1.5),
     main = "Nd1 bin x config session 1 high bin group")
lines(x.points, high.group.high.bin.sqr1[[4]], type = "l", col = "blue")
lines(x.points, high.group.low.bin.rand1[[4]], type = "l", col = "green")
lines(x.points, high.group.high.bin.rand1[[4]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)
# 3.3.2. Session 2
# 3.3.2.1. Low-bin group
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.group.low.bin.sqr2[[4]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-3, 1.5),
     main = "Nd1 bin x config session 2 low bin group")
lines(x.points, low.group.high.bin.sqr2[[4]], type = "l", col = "blue")
lines(x.points, low.group.low.bin.rand2[[4]], type = "l", col = "green")
lines(x.points, low.group.high.bin.rand2[[4]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)
# # 3.3.2.2. High-bin group
layout(rbind(1,2), heights=c(5,1))
plot(x.points, high.group.low.bin.sqr2[[4]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-3, 1.5),
     main = "Nd1 bin x config session 2 high bin group")
lines(x.points, high.group.high.bin.sqr2[[4]], type = "l", col = "blue")
lines(x.points, high.group.low.bin.rand2[[4]], type = "l", col = "green")
lines(x.points, high.group.high.bin.rand2[[4]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)

# 3.4. Left
# 3.4.1. Session 1
# 3.4.1.1. Low-bin group
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.group.low.bin.sqr1[[5]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-3, 1.5),
     main = "Nd2 left bin x config session 1 low bin group")
lines(x.points, low.group.high.bin.sqr1[[5]], type = "l", col = "blue")
lines(x.points, low.group.low.bin.rand1[[5]], type = "l", col = "green")
lines(x.points, low.group.high.bin.rand1[[5]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)
# # 3.4.1.2. High-bin group
layout(rbind(1,2), heights=c(5,1))
plot(x.points, high.group.low.bin.sqr1[[5]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-3, 1.5),
     main = "Nd2 left bin x config session 1 high bin group")
lines(x.points, high.group.high.bin.sqr1[[5]], type = "l", col = "blue")
lines(x.points, high.group.low.bin.rand1[[5]], type = "l", col = "green")
lines(x.points, high.group.high.bin.rand1[[5]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)
# 3.4.2. Session 2
# 3.4.2.1. Low-bin group
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.group.low.bin.sqr2[[5]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-3, 1.5),
     main = "Nd2 left bin x config session 2 low bin group")
lines(x.points, low.group.high.bin.sqr2[[5]], type = "l", col = "blue")
lines(x.points, low.group.low.bin.rand2[[5]], type = "l", col = "green")
lines(x.points, low.group.high.bin.rand2[[5]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)
# # 3.3.2.2. High-bin group
layout(rbind(1,2), heights=c(5,1))
plot(x.points, high.group.low.bin.sqr2[[5]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-3, 1.5),
     main = "Nd2 left bin x config session 2 high bin group")
lines(x.points, high.group.high.bin.sqr2[[5]], type = "l", col = "blue")
lines(x.points, high.group.low.bin.rand2[[5]], type = "l", col = "green")
lines(x.points, high.group.high.bin.rand2[[5]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)

# 3.5. Right
# 3.5.1. Session 1
# 3.5.1.1. Low-bin group
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.group.low.bin.sqr1[[6]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-3, 1.5),
     main = "Nd2 right bin x config session 1 low bin group")
lines(x.points, low.group.high.bin.sqr1[[6]], type = "l", col = "blue")
lines(x.points, low.group.low.bin.rand1[[6]], type = "l", col = "green")
lines(x.points, low.group.high.bin.rand1[[6]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)
# # 3.5.1.2. High-bin group
layout(rbind(1,2), heights=c(5,1))
plot(x.points, high.group.low.bin.sqr1[[6]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-3, 1.5),
     main = "Nd2 right bin x config session 1 high bin group")
lines(x.points, high.group.high.bin.sqr1[[6]], type = "l", col = "blue")
lines(x.points, high.group.low.bin.rand1[[6]], type = "l", col = "green")
lines(x.points, high.group.high.bin.rand1[[6]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)
# 3.5.2. Session 2
# 3.5.2.1. Low-bin group
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.group.low.bin.sqr2[[6]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-3, 1.5),
     main = "Nd2 right bin x config session 2 low bin group")
lines(x.points, low.group.high.bin.sqr2[[6]], type = "l", col = "blue")
lines(x.points, low.group.low.bin.rand2[[6]], type = "l", col = "green")
lines(x.points, low.group.high.bin.rand2[[6]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)
# # 3.5.2.2. High-bin group
layout(rbind(1,2), heights=c(5,1))
plot(x.points, high.group.low.bin.sqr2[[6]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-3, 1.5),
     main = "Nd2 right bin x config session 2 high bin group")
lines(x.points, high.group.high.bin.sqr2[[6]], type = "l", col = "blue")
lines(x.points, high.group.low.bin.rand2[[6]], type = "l", col = "green")
lines(x.points, high.group.high.bin.rand2[[6]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)


# 3.6. Right-left
# 3.6.1. Session 1
# 3.6.1.1. Low-bin group
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.group.low.bin.sqr1[[7]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-3, 1.5),
     main = "Nd2 RL bin x config session 1 low bin group")
lines(x.points, low.group.high.bin.sqr1[[7]], type = "l", col = "blue")
lines(x.points, low.group.low.bin.rand1[[7]], type = "l", col = "green")
lines(x.points, low.group.high.bin.rand1[[7]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)
# # 3.6.1.2. High-bin group
layout(rbind(1,2), heights=c(5,1))
plot(x.points, high.group.low.bin.sqr1[[7]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-3, 1.5),
     main = "Nd2 RL bin x config session 1 high bin group")
lines(x.points, high.group.high.bin.sqr1[[7]], type = "l", col = "blue")
lines(x.points, high.group.low.bin.rand1[[7]], type = "l", col = "green")
lines(x.points, high.group.high.bin.rand1[[7]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)
# 3.5.2. Session 2
# 3.6.2.1. Low-bin group
layout(rbind(1,2), heights=c(5,1))
plot(x.points, low.group.low.bin.sqr2[[7]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-3, 1.5),
     main = "Nd2 RL bin x config session 2 low bin group")
lines(x.points, low.group.high.bin.sqr2[[7]], type = "l", col = "blue")
lines(x.points, low.group.low.bin.rand2[[7]], type = "l", col = "green")
lines(x.points, low.group.high.bin.rand2[[7]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)
# # 3.6.2.2. High-bin group
layout(rbind(1,2), heights=c(5,1))
plot(x.points, high.group.low.bin.sqr2[[7]], type = "l", xlab = "Time points", 
     ylab = "Mean amplitude", col = "red", ylim = c(-3, 1.5),
     main = "Nd2 RL bin x config session 2 high bin group")
lines(x.points, high.group.high.bin.sqr2[[7]], type = "l", col = "blue")
lines(x.points, high.group.low.bin.rand2[[7]], type = "l", col = "green")
lines(x.points, high.group.high.bin.rand2[[7]], type = "l", col = "purple")
abline(h = 0, lty = 2, col = "black")
abline(v = 0, lty = 1, col = "black")
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = labels, col = c("red", "blue", "green", "purple"), 
       lwd=c(2,2), cex = 0.8)
par(defaults)