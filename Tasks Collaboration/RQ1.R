library(caret)
library(randomForest)
library(e1071)
library(ggplot2)
library(dplyr)
library(tidyverse)

cleared_supermarket_data<-read_csv("Input Dataset/Cleaned Dataset/Supermarket_DataCleaned.csv")
cleared_supermarked_tbl <- tbl_df(cleared_supermarket_data)

shop_ordered_slice <- select(cleared_supermarked_tbl, 3,23,4,24,5,25,6,26,7,27) %>% 
  bind_cols(cleared_supermarked_tbl[,8:10], cleared_supermarked_tbl[,28])

distance_separator <- mean(shop_ordered_slice[["avg_distance_to_all_shops"]])

set.seed(101)
row_count <- nrow(shop_ordered_slice)
train_index <- sample(row_count, round(0.7 * row_count))

rq1_dataset <- shop_ordered_slice %>%
  select(11,12,13,14)%>%
  mutate(label=factor(ifelse(max_dist_to_custSel_shops > distance_separator, "y", "n"))) %>%
  rename(average_distance = avg_distance_to_all_shops, 
         average_price = avg_purchased_product_price_allShops) %>%
  select(average_distance, average_price, label) %>%
  mutate(in_train = if_else(row_number() %in% train_index, TRUE, FALSE))


# splitting data to test and training
train_ds <- rq1_dataset %>% 
  filter(in_train) %>% 
  select(average_distance, average_price)
y_train <- rq1_dataset %>% 
  filter(in_train) %>%
  pull(label)
y_test <- rq1_dataset %>% 
  filter(!in_train) %>% 
  pull(label)

test_ds <- rq1_dataset %>% 
  filter(!in_train) %>% 
  select(average_distance, average_price)


create_knn_fit <- function(k){
  fit <- knn3(train_ds, y_train, k = k)
  return(fit)
}

create_svm_fit <- function(kernel, cost, scale){
  fit <- svm(y_train ~ ., data = cbind(train_ds,y_train), kernel = kernel, cost = cost, 
             scale=scale, cachesize=95)
  return(fit)
}

create_random_forest_fit <- function(ntree){
  fit <- randomForest(y_train ~ ., cbind(train_ds,y_train), ntree=ntree)
  return(fit)
}

draw_plot_for_classes <- function(data, predicted){
  plot_data <- data %>% mutate(label = as.factor(predicted))
  ggplot(plot_data, aes(average_distance, average_price, color = label)) +
    geom_point(alpha = .75) +
    #geom_smooth(method=lm) +
    coord_trans(x ="sqrt", y="log10") +
    theme_bw()
}

classify_with_fit <- function(fit){
  train_predicted <- predict(fit, train_ds, type = "class")
  print("Evaluation for the training")
  get_evaluation(y_train, train_predicted, "y")
  # draw_plot_for_classes(train_ds, train_predicted)
  predicted <- predict(fit, test_ds, type = "class")
  print("Evaluation for the tests")
  get_evaluation(y_test, predicted, "y")
  draw_plot_for_classes(test_ds, predicted)
}

#plot(test_ds, col = c("red","blue")[as.numeric(yclass_test)], pch = 20, cex = 1.3)

# knn classification

classify_with_fit(create_knn_fit(300))
classify_with_fit(create_knn_fit(5))
classify_with_fit(create_knn_fit(20))

# SVM

classify_with_fit(create_svm_fit("radial", 1, FALSE))
classify_with_fit(create_svm_fit("radial", 10, FALSE))
classify_with_fit(create_svm_fit("radial", 30, TRUE))

#plot(svmfit, cbind(train_ds,y_train))

# Random Forest

classify_with_fit(create_random_forest_fit(500))
classify_with_fit(create_random_forest_fit(3))
classify_with_fit(create_random_forest_fit(250))
