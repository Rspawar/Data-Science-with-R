<<<<<<< HEAD
library(tidyverse)
library(ggplot2)
library("RColorBrewer")
library(readr)
library(dplyr)


file_path<- "Input Dataset/Cleaned Dataset/Supermarket_Data_Prediction.csv"
supermarket_data_predict <- read_csv(file_path)
View(supermarket_data_predict)



p<-ggplot(data=supermarket_data_predict, aes(x=supermarket_data_predict$most_pref_shop, y=supermarket_data_predict$distance_avg, group=1)) +
  geom_point()

q<-ggplot(data=supermarket_data_predict, aes(x=supermarket_data_predict$most_pref_shop, y=supermarket_data_predict$amount_purchased_avg, group=2)) +
  geom_point(color="red")

p+scale_x_discrete(name="Shops")+scale_y_continuous(name="Likelihood based on avg distance")
q+scale_x_discrete(name="Shops")+scale_y_continuous(name="Likelihood based on avg amount")
=======
library(tidyverse)
library(ggplot2)
library("RColorBrewer")
library(readr)
library(dplyr)


file_path<- "Input Dataset/Cleaned Dataset/Supermarket_Data_Prediction.csv"
supermarket_data_predict <- read_csv(file_path)
View(supermarket_data_predict)



p<-ggplot(data=supermarket_data_predict, aes(x=supermarket_data_predict$most_pref_shop, y=supermarket_data_predict$distance_avg, group=1)) +
  geom_point()

q<-ggplot(data=supermarket_data_predict, aes(x=supermarket_data_predict$most_pref_shop, y=supermarket_data_predict$amount_purchased_avg, group=2)) +
  geom_point(color="red")

p+scale_x_discrete(name="Shops")+scale_y_continuous(name="Likelihood based on avg distance")
q+scale_x_discrete(name="Shops")+scale_y_continuous(name="Likelihood based on avg amount")
>>>>>>> 6dc4b2206d136c71aba67e4d2b6099eebabcd1ad
