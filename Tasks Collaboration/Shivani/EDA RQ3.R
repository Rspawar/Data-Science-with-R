library(tidyverse)
library(ggplot2)
library("RColorBrewer")
library(readr)
library(dplyr)


file_path<- "Input Dataset/Cleaned Dataset/Supermarket_Data_Prediction.csv"
supermarket_data_predict <- read_csv(file_path)
View(supermarket_data_predict)

p<-ggplot(data=supermarket_data_predict, aes(x=supermarket_data_predict$distance_avg, y=supermarket_data_predict$product_price_avg, group=1)) +
  geom_point()
  
p+scale_x_continuous(name="Average distance")+scale_y_continuous(name="Average product price")
