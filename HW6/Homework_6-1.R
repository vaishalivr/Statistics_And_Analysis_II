library(ggplot2)
library(dplyr)

data <- gcookbook::heightweight

subset_male <- subset(data, data$sex == 'm')
subset_male

subset_female <- subset(data, data$sex == 'f')
subset_female

set.seed(100)
random_sample_female <- subset_female[sample(nrow(subset_female), 15, replace = FALSE), ]
random_sample_female

random_sample_male <- subset_male[sample(nrow(subset_male), 15, replace = FALSE), ]
random_sample_male
# t test
# H0 - there is no significant difference between the mean of two groups
# H1 - there is a significant difference between mean of two groups
t_test_result <- t.test(random_sample_female$heightIn, random_sample_male$heightIn, alpha = 0.05)
# pvalue > alpha - ftr null hypothesis
print(t_test_result)

#combining sample dataframes
combined_data <- bind_rows(random_sample_female, random_sample_male)
combined_data

#visualizaing error - std.dev.
std_dev <- sd(combined_data$heightIn)
std_dev

mean_female <- mean(random_sample_female$heightIn)
mean_female

mean_male <- mean(random_sample_male$heightIn)
mean_male

std_dev_random_male <- sd(random_sample_male$heightIn)
std_dev_random_male

std_dev_random_female <- sd(random_sample_female$heightIn)
std_dev_random_female
# Create a jittered points chart
plot <-  ggplot(combined_data, aes(x = sex, y = heightIn)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) 
plot <- plot + geom_errorbar(data = random_sample_male, aes(x = sex, y = heightIn, ymin = mean_male - std_dev_random_male, ymax = mean_male + std_dev_random_male),
                width = 0.2, color = "red") 
plot <-  plot + geom_errorbar(data = random_sample_female, aes(x = sex, y = heightIn, ymin = mean_female - std_dev_random_female, ymax = mean_female + std_dev_random_female),
                       width = 0.2, color = 'red')
plot


#answer 2
data$grade<-as.factor(round(data$ageYear) -6)
data

#t tests
t_test_grade6 <- t.test(subset(data, data$grade == 6 & data$sex == 'm')$heightIn, subset(data, data$grade == 6 & data$sex == 'f')$heightIn, alpha = 0.05)
t_test_grade7 <- t.test(subset(data, data$grade == 7 & data$sex == 'm')$heightIn, subset(data, data$grade == 7 & data$sex == 'f')$heightIn)
t_test_grade8 <- t.test(subset(data, data$grade == 8 & data$sex == 'm')$heightIn, subset(data, data$grade == 8 & data$sex == 'f')$heightIn)
t_test_grade9 <- t.test(subset(data, data$grade == 9 & data$sex == 'm')$heightIn, subset(data, data$grade == 9 & data$sex == 'f')$heightIn)
t_test_grade10 <- t.test(subset(data, data$grade == 10 & data$sex == 'm')$heightIn, subset(data, data$grade == 10 & data$sex == 'f')$heightIn)

grade <- c("Grade_6", "Grade_7", "Grade_8", "Grade_9", "Grade_10")
alpha <- rep(0.05, 5)
p_value <- c(t_test_grade6$p.value, t_test_grade7$p.value, t_test_grade8$p.value, t_test_grade9$p.value, t_test_grade10$p.value)
p_values
result <- c()
for (i in 1:length(p_value)) {
  if (p_value[i] > alpha[i]) {
    result <- c(result, "fail to reject")
  } else {
    result <- c(result, "reject")
  }
}

test_table <- data.frame(Name = grade, Alpha = alpha, pValue = p_value, Result=result)
test_table

#optional
ggplot(data, aes(x = sex, y = heightIn)) +
  geom_point(width = 0.2, height = 0, alpha = 0.3) +
  stat_summary(
    fun = mean, 
    geom = "errorbar", 
    position = position_dodge(width = 0.2),
    width = 0.2,
    fun.data = mean_cl_normal,
    na.rm = TRUE,
    color = "red"
  ) +
  facet_wrap(~ grade, nrow = 2)
