library(dplyr)

data = read.csv("/Users/vaishaliverma/Downloads/hw_wk1/TitanicPassengerList.csv")
data

#count and mean ages of all passengers
all_passangers_count <- nrow(data)
all_passangers_count

female_count <- nrow(data[data$sex == "female", ])
female_count

male_count <- nrow(data[data$sex == "male", ])
male_count

#mean all passenger age
all_passenger_mean_age <- mean(na.omit(data$age))
all_passenger_mean_age

# mean female age
data_female <- data[data$sex == "female", ]
data_female <- data_female[!is.na(data_female$age), ]
female_mean_age <- mean(na.omit(data_female$age))
female_mean_age

# mean male age
data_male <- data[data$sex == "male", ]
data_male <- data_male[!is.na(data_male$age), ]
mean_male_age <- mean(data_male$age)
mean_male_age

#dataset with no NA in age
all_age_dataset <- data[!is.na(data$age), ]
all_age_dataset
#histogram of all ages of passengers
ggplot(data = all_age_dataset, aes(x = age)) + geom_histogram(bins=40)

#check NA in survived
na_in_survived <- sum(is.na(data$survived))
na_in_survived #0

#total number, departed, survived
barPlot_df <- data.frame(
  name = c("Total", "Departed", "Survived"),
  value = c(nrow(data), sum(data$survived == 0), sum(data$survived == 1))
)
barPlot_df <- barPlot_df[order(barPlot_df$value),]
barPlot_df
ggplot(barPlot_df, aes(x = reorder(name, +value), y = value)) + geom_bar(stat = "identity")

#check NA in cabin
na_in_cabin <- data[!(is.na(data$cabin) | data$cabin == ""), ]

#find how many cabins of each type
#find all unique cabin alphabets
na_in_cabin$cabin
cabin_alphabets <- c()
for (alphabet in na_in_cabin$cabin) {
  if (!(substring(alphabet,1,1) %in% cabin_alphabets)) {
    cabin_alphabets <- append(cabin_alphabets, substring(alphabet,1,1))
  }
}
cabin_alphabets
class_A_cabin <- c()
class_B_cabin <- c()
class_C_cabin <- c()
class_D_cabin <- c()
class_E_cabin <- c()
class_F_cabin <- c()
class_G_cabin <- c()
class_T <- c()
# put all cabins in their vector
#convert to switch case 
 
for (cabin in scan(text = na_in_cabin$cabin, what = "")) {
  value = substring(cabin,1,1)
  if (value == "A") {
    #class_A_cabin <- c(class_A_cabin, cabin)
    class_A_cabin <- append(class_A_cabin, cabin)
  } else if (value == "B") {
    #class_B_cabin <- c(class_B_cabin, cabin)
    class_B_cabin <- append(class_B_cabin, cabin)
  } else if (value == "C") {
    class_C_cabin <- append(class_C_cabin, cabin)
  } else if (value == "D") {
    class_D_cabin <- append(class_D_cabin, cabin)
  } else if (value == "E") {
    class_E_cabin <- append(class_E_cabin, cabin)
  } else if (value == "F") {
    class_F_cabin <- append(class_F_cabin, cabin)
  } else if (value == "G") {
    class_G_cabin <- append(class_G_cabin, cabin)
  } else {
    class_T <- c(class_T, cabin)
  }
}

sorting_function <- function (cabin_class, char) {
  placeholder_array = c()
  for (temp in cabin_class) {
    sub_temp = as.numeric(substring(temp,2,3))
    placeholder_array <- append(placeholder_array, sub_temp)
  }
  placeholder_array <- sort(placeholder_array)
  placeholder_array <- as.character(placeholder_array)
  placeholder_array <- paste(char, placeholder_array)
  placeholder_array <- gsub(" ", "", placeholder_array)
  return(placeholder_array)
}
class_A_cabin <- sorting_function(class_A_cabin, "A")
class_B_cabin <- sorting_function(class_B_cabin, "B")
class_C_cabin <- sorting_function(class_C_cabin, "C")
class_D_cabin <- sorting_function(class_D_cabin, "D")
class_E_cabin <- sorting_function(class_E_cabin, "E")
class_F_cabin <- sorting_function(class_F_cabin, "F")
class_G_cabin <- sorting_function(class_G_cabin, "G")

print(class_A_cabin)
print(class_B_cabin)

#see number of documented cabins
all_cabins <- list(class_A = class_A_cabin,
                   class_B = class_B_cabin,
                   class_C = class_C_cabin,
                   class_D = class_D_cabin,
                   class_E = class_E_cabin,
                   class_F = class_F_cabin,
                   class_G = class_G_cabin)
all_cabins

