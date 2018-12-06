library(tidyverse)
library(ggplot2)
library("RColorBrewer")
library(readr)
library(dplyr)


file_path<- "Input Dataset/Cleaned Dataset/Supermarket_DataCleaned.csv"
supermarket_data_clean <- read_csv(file_path)
View(supermarket_data_clean)

#select related columns

slice1<-select(supermarket_data_clean,1,3,4,5,6,7,8,9,29,30,31,32,33,34)
View(slice1)

slice2<-select(supermarket_data_clean,1,3,4,5,6,7,29,30,31,32,33,34,9)
View(slice2)

slice3<-select(supermarket_data_clean,1,29,30,31,32,33,34)
View(slice3)

#likelihood based on price

likelihood_ratio_price_shop1<-slice3$amount_purchased_shop_1/slice3$amount_purchased_total
likelihood_ratio_price_shop2<-slice3$amount_purchased_shop_2/slice3$amount_purchased_total
likelihood_ratio_price_shop3<-slice3$amount_purchased_shop_3/slice3$amount_purchased_total
likelihood_ratio_price_shop4<-slice3$amount_purchased_shop_4/slice3$amount_purchased_total
likelihood_ratio_price_shop5<-slice3$amount_purchased_shop_5/slice3$amount_purchased_total

likelihood_ratio_price<-data.frame(slice3$customer_id,likelihood_ratio_price_shop1,likelihood_ratio_price_shop2,likelihood_ratio_price_shop3,likelihood_ratio_price_shop4,likelihood_ratio_price_shop5)
View(likelihood_ratio_price)

#likelihood based on distance and amount

likelihood_shop1<-(slice2$distance_shop_1*slice2$amount_purchased_shop_1)/(slice2$distance_shop_1*slice2$amount_purchased_shop_1+slice2$distance_shop_2*slice2$amount_purchased_shop_2+slice2$distance_shop_3*slice2$amount_purchased_shop_3+slice2$distance_shop_4*slice2$amount_purchased_shop_4+slice2$distance_shop_5*slice2$amount_purchased_shop_5)
likelihood_shop2<-(slice2$distance_shop_2*slice2$amount_purchased_shop_2)/(slice2$distance_shop_1*slice2$amount_purchased_shop_1+slice2$distance_shop_2*slice2$amount_purchased_shop_2+slice2$distance_shop_3*slice2$amount_purchased_shop_3+slice2$distance_shop_4*slice2$amount_purchased_shop_4+slice2$distance_shop_5*slice2$amount_purchased_shop_5)
likelihood_shop3<-(slice2$distance_shop_3*slice2$amount_purchased_shop_3)/(slice2$distance_shop_1*slice2$amount_purchased_shop_1+slice2$distance_shop_2*slice2$amount_purchased_shop_2+slice2$distance_shop_3*slice2$amount_purchased_shop_3+slice2$distance_shop_4*slice2$amount_purchased_shop_4+slice2$distance_shop_5*slice2$amount_purchased_shop_5)
likelihood_shop4<-(slice2$distance_shop_4*slice2$amount_purchased_shop_4)/(slice2$distance_shop_1*slice2$amount_purchased_shop_1+slice2$distance_shop_2*slice2$amount_purchased_shop_2+slice2$distance_shop_3*slice2$amount_purchased_shop_3+slice2$distance_shop_4*slice2$amount_purchased_shop_4+slice2$distance_shop_5*slice2$amount_purchased_shop_5)
likelihood_shop5<-(slice2$distance_shop_5*slice2$amount_purchased_shop_5)/(slice2$distance_shop_1*slice2$amount_purchased_shop_1+slice2$distance_shop_2*slice2$amount_purchased_shop_2+slice2$distance_shop_3*slice2$amount_purchased_shop_3+slice2$distance_shop_4*slice2$amount_purchased_shop_4+slice2$distance_shop_5*slice2$amount_purchased_shop_5)

likelihood<-data.frame(slice2$amount_purchased_total,likelihood_shop1,likelihood_shop2,likelihood_shop3,likelihood_shop4,likelihood_shop5)
View(likelihood)

#plot graph for likelihood vs amount

graph1<-data.frame(likelihood$likelihood_shop1,likelihood$slice2.amount_purchased_total)
plot(graph1,col="blue",xlab("likelihoodShop1"),ylab("total_amount_customer_can_spent"))
graph2<-data.frame(likelihood$likelihood_shop2,likelihood$slice2.amount_purchased_total)
plot(graph2,col="blue",xlab("likelihoodShop2"),ylab("total_amount_customer_can_spent"))
graph3<-data.frame(likelihood$likelihood_shop3,likelihood$slice2.amount_purchased_total)
plot(graph3,col="blue",xlab("likelihoodShop3"),ylab("total_amount_customer_can_spent"))
graph4<-data.frame(likelihood$likelihood_shop4,likelihood$slice2.amount_purchased_total)
plot(graph4,col="blue",xlab("likelihoodShop4"),ylab("total_amount_customer_can_spent"))
graph5<-data.frame(likelihood$likelihood_shop5,likelihood$slice2.amount_purchased_total)
plot(graph5,col="blue",xlab("likelihoodShop5"),ylab("total_amount_customer_can_spent"))


#plot graph for likelihood vs distance

graph1<-data.frame(likelihood$likelihood_shop1,slice2$max_dist_to_custSel_shops)
plot(graph1,col="pink",xlab("likelihoodShop1"),ylab("maximum_distance_customer_Can_travel"))
graph2<-data.frame(likelihood$likelihood_shop2,slice2$max_dist_to_custSel_shops)
plot(graph2,col="pink",xlab("likelihoodShop2"),ylab("maximum_distance_customer_Can_travel"))
graph3<-data.frame(likelihood$likelihood_shop3,slice2$max_dist_to_custSel_shops)
plot(graph3,col="pink",xlab("likelihoodShop3"),ylab("maximum_distance_customer_Can_travel"))
graph4<-data.frame(likelihood$likelihood_shop4,slice2$max_dist_to_custSel_shops)
plot(graph4,col="pink",xlab("likelihoodShop4"),ylab("maximum_distance_customer_Can_travel"))
graph5<-data.frame(likelihood$likelihood_shop5,slice2$max_dist_to_custSel_shops)
plot(graph5,col="pink",xlab("likelihoodShop5"),ylab("maximum_distance_customer_Can_travel"))


