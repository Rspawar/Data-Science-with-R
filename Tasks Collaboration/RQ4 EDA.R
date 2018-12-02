library(tidyverse)
library(ggplot2)
library("RColorBrewer")

file_path<- "Input Dataset/Cleaned Dataset/Supermarket_DataCleaned.csv"
supermarket_data_clean <- read_csv(file_path)

View(supermarket_data_clean)

## Determine highest revenue generating shops, highest number of products sold 
## by the shops, highest number of unique products sold 
## by the shops and the relation between the three (as a plot)

# Revenue generation by Shops 1-5

# select columns related to amount purchased in shops 1-5

slice1<-select(supermarket_data_clean, 29,30,31,32,33)
View(slice1)

amountS1<-sum(slice1$amount_purchased_shop_1)
amountS2<-sum(slice1$amount_purchased_shop_2)
amountS3<-sum(slice1$amount_purchased_shop_3)
amountS4<-sum(slice1$amount_purchased_shop_4)
amountS5<-sum(slice1$amount_purchased_shop_5)

# create data frame
Shop<- c(1,2,3,4,5)
Revenue_Generated<- c(amountS1,amountS2,amountS3,amountS4,amountS5)
Revenue<- data.frame(Shop, Revenue_Generated)
rownames(Revenue) <- NULL
View(Revenue)

# Products Sold by Shops 1-5

slice2<-select(supermarket_data_clean, 11,12,13,14,15)
View(slice2)

productsS1<-sum(slice2$products_purchased_shop_1)
productsS2<-sum(slice2$products_purchased_shop_2)
productsS3<-sum(slice2$products_purchased_shop_3)
productsS4<-sum(slice2$products_purchased_shop_4)
productsS5<-sum(slice2$products_purchased_shop_5)

# create data frame
Products_Purchased<- c(productsS1,productsS2,productsS3,productsS4,productsS5)
ProductsSold<- data.frame(Shop, Products_Purchased)
rownames(ProductsSold) <- NULL
View(ProductsSold)

# Unique products Sold by Shops 1-5

slice3<-select(supermarket_data_clean, 17,18,19,20,21)
View(slice3)

uproductsS1<-sum(slice3$unique_products_purchased_shop_1)
uproductsS2<-sum(slice3$unique_products_purchased_shop_2)
uproductsS3<-sum(slice3$unique_products_purchased_shop_3)
uproductsS4<-sum(slice3$unique_products_purchased_shop_4)
uproductsS5<-sum(slice3$unique_products_purchased_shop_5)

# create data frame
UProducts_Purchased<- c(uproductsS1,uproductsS2,uproductsS3,uproductsS4,uproductsS5)
UProductsSold<- data.frame(Shop, UProducts_Purchased)
rownames(UProductsSold) <- NULL
View(UProductsSold)

# Total Customer Base for Shops 1-5

C1<-slice2$products_purchased_shop_1
custS1<-length(which(C1 !=0))

C2<-slice2$products_purchased_shop_2
custS2<-length(which(C2 !=0))

C3<-slice2$products_purchased_shop_3
custS3<-length(which(C3 !=0))

C4<-slice2$products_purchased_shop_4
custS4<-length(which(C4 !=0))

C5<-slice2$products_purchased_shop_5
custS5<-length(which(C5 !=0))


# create data frame
Customers<- c(custS1,custS2,custS3,custS4,custS5)
TotalCustomers<- data.frame(Shop, Customers)
rownames(TotalCustomers) <- NULL
View(TotalCustomers)

# Plot a Bar graph
Legends <-c(rep("Revenue Generated", 5), rep("Products Sold", 5), rep("Unique Products Sold", 5))
values <-c(Revenue_Generated, Products_Purchased, UProducts_Purchased)
mydata <-data.frame(Shop, values)

p <-ggplot(mydata, aes(Shop, values))
p +geom_bar(stat = "identity", aes(fill = Legends), position = "dodge") +
  xlab("Shop") + ylab("Total") +
  ggtitle("Relation between Revenue Generated, Products Sold, Unique Products Sold across Shops") +
  theme_bw()

# Customer Base
values <-c(Customers)
mydata <-data.frame(Shop, values)

p <-ggplot(mydata, aes(Shop, values))
p +geom_bar(stat = "identity", fill = brewer.pal(n = 5, name = "Set1"), position = "dodge") +
  xlab("Shop") + ylab("Total") +
  ggtitle("Customer Base") + 
  theme_bw()


