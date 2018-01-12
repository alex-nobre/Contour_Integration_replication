
library(dplyr)
library(knitr)
library(xtable)
library(lazyeval)

#========================================Table 1========================================#
mean.RT.1 <- questionnaire.ERPs %>% 
  group_by(group) %>%
  summarize(RT.mean = mean(RT.mean_1))

mean.RT.2 <- questionnaire.ERPs %>% 
  group_by(group) %>%
  summarize(RT.mean = mean(RT.mean_2))

mean.RT.3 <- questionnaire.ERPs %>% 
  group_by(group) %>%
  summarize(RT.mean = mean(RT.mean_3))

mean.threshold.1 <- questionnaire.ERPs %>%
  group_by(group) %>%
  summarize(threshold.mean = mean(threshold_1))

mean.threshold.2 <- questionnaire.ERPs %>%
  group_by(group) %>%
  summarize(threshold.mean = mean(threshold_2))

mean.threshold.2 <- questionnaire.ERPs %>%
  group_by(group) %>%
  summarize(threshold.mean = mean(threshold_3))

mean.threshold.3 <- as_tibble(data.frame(matrix(c("aware", "unaware", "-", "-"), 
                                        nrow = 2, ncol = 2, byrow = FALSE, dimnames = 
                                       list(c("1","2"), c("group", "threshold.mean")))))

mean.threshold.3 <- as_tibble(data.frame(c("aware", "unaware", "-", "-"), 
                                     nrow = 2, ncol = 2,
                                     colnames = c("group", "threshold.mean")))


# Bind all variables and observations in data frame
task.performance <- mean.threshold.1 %>%
  rbind(mean.threshold.2) %>% 
  rbind(mean.threshold.3) %>%
  cbind(rbind(mean.RT.1, mean.RT.2, mean.RT.3))
task.performance <- task.performance[,-3]  
  
# generate LaTex code
print(xtable(task.performance), comment = FALSE, include.rownames = FALSE)

#===========================================Table 2=======================================#

# Function to generate tibbles
mean_data <- function(dataframe, name, original.score) {
  grouped_data <- group_by(dataframe, group)
  summarized_data <- summarize_(grouped_data, 
            name = interp(~ mean(var_name), var_name = as.name(original.score)))
  colnames(summarized_data)[2] <- as.character(substitute(name))
  summarized_data
}


sd_data <- function(dataframe, name, original.score) {
  grouped_data <- group_by(dataframe, group)
  summarized_data <- summarize_(grouped_data, 
                                name = interp(~ sd(var_name), var_name = as.name(original.score)))
  colnames(summarized_data)[2] <- as.character(substitute(name))
  summarized_data
}

test_conf <- mean_data(questionnaire.ERPs, "Centripetal Radial", "conf.6_1")
test_conf_sd <- sd_data(questionnaire.ERPs, "Centripetal radial", "conf.6_1")

#==========================================Tests===============================================#

test_conf <- mean.data(questionnaire.ERPs, "conf.6_1")


test.conf2 <- questionnaire.ERPs %>%
  group_by(group) %>%
  summarize("Centripetal radial" = mean(conf.6_1))



conf.radcen.1 <- questionnaire.ERPs %>%
  group_by(group) %>%
  summarize("Centripetal Radial" = mean(conf.6_1))

