install.packages("gcookbook")
library(gcookbook)
library(ggplot2)
# q1.1
population <- gcookbook::heightweight

weightLb_count <- length(population$weightLb)
weightLb_count

weightLb_mean <- mean(population$weightLb)
weightLb_mean

weightLb_sd <- sd(population$weightLb)
weightLb_sd

#q1.2 with bins = 30
hist <- ggplot(data = population, aes(x = population$weightLb)) + 
  geom_histogram(fill = 'white', color = 'black') + 
  xlab("Population Weight") + 
  ylab('Population Count') +
  theme_bw() +
  theme(panel.border = element_blank()) +
  geom_vline(xintercept = mean(population$weightLb), linetype = 'dashed', color = 'black', size = 0.6) 
hist <-  hist + geom_text(x = weightLb_mean + 2, 
                          y = 33, 
                          label = 'Population Mean', 
                          size = 3.5, 
                          angle = 90,
                          fontface = 'italic', 
                          family = 'Helvetica'
                          )
hist

#q2.1
sampled_data <- population[sample(nrow(population), size = 15, replace = FALSE), ]
sampled_data

mean_sampled_data <- mean(sampled_data$weightLb)
mean_sampled_data

sd_sampled_data <- sd(sampled_data$weightLb)
sd_sampled_data

result_90 <- t.test(sampled_data$weightLb, conf.level = 0.90)
result_90 <- result_90$conf.int
result_90

result_99 <- t.test(sampled_data$weightLb, conf.level = 0.99)
result_99 <- result_99$conf.int
result_99

sampled_hist <- hist + 
  geom_histogram(data = sampled_data, aes(x = sampled_data$weightLb), fill = 'red', color = 'black') +
  geom_vline(xintercept = mean(sampled_data$weightLb), linetype = 'dashed', color = 'red', size = 0.6) 
sampled_hist <- sampled_hist + 
  geom_text(x = mean_sampled_data - 2, 
            y = 33.5, 
            label = 'Sampled Mean', 
            size = 3.5, 
            angle = 90, 
            fontface = 'italic', 
            family = 'Helvetica', 
            color = 'red')
sampled_hist <- sampled_hist + 
  geom_vline(xintercept = result_99[1], color = 'blue', size =0.6) +
  geom_vline(xintercept = result_99[2], color = 'blue', size = 0.6)

sampled_hist <- sampled_hist +
  geom_vline(xintercept = result_90[1], color = 'green', size=0.6) +
  geom_vline(xintercept = result_90[2], color = 'green', size = 0.6) +
sampled_hist
