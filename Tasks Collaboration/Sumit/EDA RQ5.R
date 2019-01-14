library(tidyverse)
library(ggplot2)
library("RColorBrewer")
library(plotly)

file_path<- "Input Dataset/Cleaned Dataset/Supermarket_DataCleaned.csv"
supermarket_data_clean <- read_csv(file_path)


## Ratio of products purchased from each shop (product purchased by a customer at a shop over total produccts purchased by that customer)
prod_purch_ratio_shop_1 <- with(supermarket_data_clean, products_purchased_shop_1/products_purchased_total)
prod_purch_ratio_shop_2 <- with(supermarket_data_clean, products_purchased_shop_2/products_purchased_total)
prod_purch_ratio_shop_3 <- with(supermarket_data_clean, products_purchased_shop_3/products_purchased_total)
prod_purch_ratio_shop_4 <- with(supermarket_data_clean, products_purchased_shop_4/products_purchased_total)
prod_purch_ratio_shop_5 <- with(supermarket_data_clean, products_purchased_shop_5/products_purchased_total)

## Ratio of amount spent at each shop (amount spent by a customer at a shop over total amount spent by that customer)
amt_purch_ratio_shop_1 <- with(supermarket_data_clean, amount_purchased_shop_1/amount_purchased_total)
amt_purch_ratio_shop_2 <- with(supermarket_data_clean, amount_purchased_shop_2/amount_purchased_total)
amt_purch_ratio_shop_3 <- with(supermarket_data_clean, amount_purchased_shop_3/amount_purchased_total)
amt_purch_ratio_shop_4 <- with(supermarket_data_clean, amount_purchased_shop_4/amount_purchased_total)
amt_purch_ratio_shop_5 <- with(supermarket_data_clean, amount_purchased_shop_5/amount_purchased_total)

## Loyality of customer to a shop (Ratio of products purchased from each shop times Ratio of amount spent at each shop)
loyality_shop_1 <- prod_purch_ratio_shop_1 * amt_purch_ratio_shop_1
loyality_shop_2 <- prod_purch_ratio_shop_2 * amt_purch_ratio_shop_2
loyality_shop_3 <- prod_purch_ratio_shop_3 * amt_purch_ratio_shop_3
loyality_shop_4 <- prod_purch_ratio_shop_4 * amt_purch_ratio_shop_4
loyality_shop_5 <- prod_purch_ratio_shop_5 * amt_purch_ratio_shop_5


## Data frames based on calculated values
prod_purch_ratio_shop <- data.frame(supermarket_data_clean$customer_id, prod_purch_ratio_shop_1, prod_purch_ratio_shop_2, prod_purch_ratio_shop_3, prod_purch_ratio_shop_4, prod_purch_ratio_shop_5)
amt_purch_ratio_shop <- data.frame(supermarket_data_clean$customer_id, amt_purch_ratio_shop_1, amt_purch_ratio_shop_2, amt_purch_ratio_shop_3, amt_purch_ratio_shop_4, amt_purch_ratio_shop_5)
loyality <- data.frame(supermarket_data_clean$customer_id, loyality_shop_1, loyality_shop_2, loyality_shop_3, loyality_shop_4, loyality_shop_5)


## Data frames as per shop nos
shop_1 <- data.frame(supermarket_data_clean$customer_id, prod_purch_ratio_shop_1, amt_purch_ratio_shop_1, loyality_shop_1)
shop_2 <- data.frame(supermarket_data_clean$customer_id, prod_purch_ratio_shop_2, amt_purch_ratio_shop_2, loyality_shop_2)
shop_3 <- data.frame(supermarket_data_clean$customer_id, prod_purch_ratio_shop_3, amt_purch_ratio_shop_3, loyality_shop_3)
shop_4 <- data.frame(supermarket_data_clean$customer_id, prod_purch_ratio_shop_4, amt_purch_ratio_shop_4, loyality_shop_4)
shop_5 <- data.frame(supermarket_data_clean$customer_id, prod_purch_ratio_shop_5, amt_purch_ratio_shop_5, loyality_shop_5)


## Plot product purchased ratio vs amount purchased ratio based on customer segments for shop 1 (all)
shop_1$segment <- ifelse(loyality_shop_1 > 0.9, 'high', 
                         ifelse(loyality_shop_1 > 0.50 & loyality_shop_1 <= 0.9, 'med',
                                ifelse(loyality_shop_1 > 0 & loyality_shop_1 <= 0.5, 'low', 'na')))

shop_1 <- shop_1[order(shop_1$prod_purch_ratio_shop_1, shop_1$amt_purch_ratio_shop_1),]
shop_1$size <- ((shop_1$loyality_shop_1 + 0.1) * 10)
colors <- c('#FFFF00', '#0000FF', '#808000', '#FFFFFF')

plot_shop_1 <- plot_ly(shop_1, x = ~prod_purch_ratio_shop_1, y = ~amt_purch_ratio_shop_1, color = ~segment,
              size = ~size, colors = colors, type = 'scatter', mode = 'markers', 
              sizes = c(min(shop_1$size), max(shop_1$size)),
              marker = list(symbol = 'circle', sizemode = 'diameter', 
                            line = list(width = 0.5, color = '#800000')), 
              text = ~paste('Customer ID:', supermarket_data_clean.customer_id, '<br>Loyality:',
                            round(loyality_shop_1 * 100, digits = 2), '%')) %>% 
  layout(title = 'Product vs. Amount Purchased Ratio for Shop 1',
         xaxis = list(title = 'Product Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0,1), zerolinewidth = 1, ticklen = 5, gridwidth = 2),
         yaxis = list(title = 'Amount Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0, 1), zerolinewidth = 1, ticklen = 5, gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)', plot_bgcolor = 'rgb(243, 243, 243)')

## Plot product purchased ratio vs amount purchased ratio based on customer segments for shop 1 (high)
shop_1_high <- shop_1[which(shop_1$segment == 'high'),]

plot_shop_1_high <- plot_ly(shop_1_high, x = ~prod_purch_ratio_shop_1, y = ~amt_purch_ratio_shop_1, color = ~segment,
                     size = ~size, colors = colors, type = 'scatter', mode = 'markers', 
                     sizes = c(min(shop_1$size), max(shop_1$size)),
                     marker = list(symbol = 'circle', sizemode = 'diameter', 
                                   line = list(width = 0.5, color = '#800000')), 
                     text = ~paste('Customer ID:', supermarket_data_clean.customer_id, '<br>Loyality:',
                                   round(loyality_shop_1 * 100, digits = 2), '%')) %>% 
  layout(title = 'Product vs. Amount Purchased Ratio for Shop 1 (Segment - High)',
         xaxis = list(title = 'Product Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.9, 1.01), zerolinewidth = 1, ticklen = 5, gridwidth = 2),
         yaxis = list(title = 'Amount Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.9, 1.01), zerolinewidth = 1, ticklen = 5, gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)', plot_bgcolor = 'rgb(243, 243, 243)')

## Plot product purchased ratio vs amount purchased ratio based on customer segments for shop 1 (med)
shop_1_med <- shop_1[which(shop_1$segment == 'med'),]

plot_shop_1_med <- plot_ly(shop_1_med, x = ~prod_purch_ratio_shop_1, y = ~amt_purch_ratio_shop_1, color = ~segment,
                     size = ~size, colors = colors, type = 'scatter', mode = 'markers', 
                     sizes = c(min(shop_1$size), max(shop_1$size)),
                     marker = list(symbol = 'circle', sizemode = 'diameter', 
                                   line = list(width = 0.5, color = '#800000')), 
                     text = ~paste('Customer ID:', supermarket_data_clean.customer_id, '<br>Loyality:',
                                   round(loyality_shop_1 * 100, digits = 2), '%')) %>% 
  layout(title = 'Product vs. Amount Purchased Ratio for Shop 1 (Segment - Med)',
         xaxis = list(title = 'Product Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.5, 0.91), zerolinewidth = 1, ticklen = 5, gridwidth = 2),
         yaxis = list(title = 'Amount Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.5, 0.91), zerolinewidth = 1, ticklen = 5, gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)', plot_bgcolor = 'rgb(243, 243, 243)')

## Plot product purchased ratio vs amount purchased ratio based on customer segments for shop 1 (low)
shop_1_low <- shop_1[which(shop_1$segment == 'low'),]

plot_shop_1_low <- plot_ly(shop_1_low, x = ~prod_purch_ratio_shop_1, y = ~amt_purch_ratio_shop_1, color = ~segment,
                     size = ~size, colors = colors, type = 'scatter', mode = 'markers', 
                     sizes = c(min(shop_1$size), max(shop_1$size)),
                     marker = list(symbol = 'circle', sizemode = 'diameter', 
                                   line = list(width = 0.5, color = '#800000')), 
                     text = ~paste('Customer ID:', supermarket_data_clean.customer_id, '<br>Loyality:',
                                   round(loyality_shop_1 * 100, digits = 2), '%')) %>% 
  layout(title = 'Product vs. Amount Purchased Ratio for Shop 1 (Segment - High)',
         xaxis = list(title = 'Product Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.0, 0.51), zerolinewidth = 1, ticklen = 5, gridwidth = 2),
         yaxis = list(title = 'Amount Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.0, 0.51), zerolinewidth = 1, ticklen = 5, gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)', plot_bgcolor = 'rgb(243, 243, 243)')

## Pie chart based on customer segments for Shop 1 
colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)')

pie_shop_1 <- plot_ly(shop_1_customer_segment_count, labels = ~customer_segments, values = ~counts, type = 'pie',
             textposition = 'inside', textinfo = 'label+percent', 
             insidetextfont = list(color = '#FFFFFF'), hoverinfo = 'text',
             text = ~paste(customer_segments, '=', counts),
             marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)),
             ## 'pull' attribute can also be used to create space between the sectors
             showlegend = FALSE) %>%
  layout(title = 'Customer Segment for Shop 1',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


## Plot product purchased ratio vs amount purchased ratio based on customer segments for shop 2 (all)
shop_2$segment <- ifelse(loyality_shop_2 > 0.9, 'high', 
                         ifelse(loyality_shop_2 > 0.50 & loyality_shop_2 <= 0.9, 'med',
                                ifelse(loyality_shop_2 > 0 & loyality_shop_2 <= 0.5, 'low', 'na')))

shop_2 <- shop_2[order(shop_2$prod_purch_ratio_shop_2, shop_2$amt_purch_ratio_shop_2),]
shop_2$size <- ((shop_2$loyality_shop_2 + 0.1) * 10)
colors <- c('#FFFF00', '#0000FF', '#808000', '#FFFFFF')

plot_shop_2 <- plot_ly(shop_2, x = ~prod_purch_ratio_shop_2, y = ~amt_purch_ratio_shop_2, color = ~segment,
                       size = ~size, colors = colors, type = 'scatter', mode = 'markers', 
                       sizes = c(min(shop_2$size), max(shop_2$size)),
                       marker = list(symbol = 'circle', sizemode = 'diameter', 
                                     line = list(width = 0.5, color = '#800000')), 
                       text = ~paste('Customer ID:', supermarket_data_clean.customer_id, '<br>Loyality:',
                                     round(loyality_shop_2 * 100, digits = 2), '%')) %>% 
  layout(title = 'Product vs. Amount Purchased Ratio for Shop 2',
         xaxis = list(title = 'Product Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0,1), zerolinewidth = 1, ticklen = 5, gridwidth = 2),
         yaxis = list(title = 'Amount Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0, 1), zerolinewidth = 1, ticklen = 5, gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)', plot_bgcolor = 'rgb(243, 243, 243)')

## Plot product purchased ratio vs amount purchased ratio based on customer segments for shop 2 (high)
shop_2_high <- shop_2[which(shop_2$segment == 'high'),]

plot_shop_2_high <- plot_ly(shop_2_high, x = ~prod_purch_ratio_shop_2, y = ~amt_purch_ratio_shop_2, color = ~segment,
                            size = ~size, colors = colors, type = 'scatter', mode = 'markers', 
                            sizes = c(min(shop_2$size), max(shop_2$size)),
                            marker = list(symbol = 'circle', sizemode = 'diameter', 
                                          line = list(width = 0.5, color = '#800000')), 
                            text = ~paste('Customer ID:', supermarket_data_clean.customer_id, '<br>Loyality:',
                                          round(loyality_shop_2 * 100, digits = 2), '%')) %>% 
  layout(title = 'Product vs. Amount Purchased Ratio for Shop 2 (Segment - High)',
         xaxis = list(title = 'Product Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.9, 1.01), zerolinewidth = 1, ticklen = 5, gridwidth = 2),
         yaxis = list(title = 'Amount Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.9, 1.01), zerolinewidth = 1, ticklen = 5, gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)', plot_bgcolor = 'rgb(243, 243, 243)')

## Plot product purchased ratio vs amount purchased ratio based on customer segments for shop 2 (med)
shop_2_med <- shop_2[which(shop_2$segment == 'med'),]

plot_shop_2_med <- plot_ly(shop_2_med, x = ~prod_purch_ratio_shop_2, y = ~amt_purch_ratio_shop_2, color = ~segment,
                           size = ~size, colors = colors, type = 'scatter', mode = 'markers', 
                           sizes = c(min(shop_2$size), max(shop_2$size)),
                           marker = list(symbol = 'circle', sizemode = 'diameter', 
                                         line = list(width = 0.5, color = '#800000')), 
                           text = ~paste('Customer ID:', supermarket_data_clean.customer_id, '<br>Loyality:',
                                         round(loyality_shop_2 * 100, digits = 2), '%')) %>% 
  layout(title = 'Product vs. Amount Purchased Ratio for Shop 2 (Segment - Med)',
         xaxis = list(title = 'Product Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.5, 0.91), zerolinewidth = 1, ticklen = 5, gridwidth = 2),
         yaxis = list(title = 'Amount Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.5, 0.91), zerolinewidth = 1, ticklen = 5, gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)', plot_bgcolor = 'rgb(243, 243, 243)')

## Plot product purchased ratio vs amount purchased ratio based on customer segments for shop 2 (low)
shop_2_low <- shop_2[which(shop_2$segment == 'low'),]

plot_shop_2_low <- plot_ly(shop_2_low, x = ~prod_purch_ratio_shop_2, y = ~amt_purch_ratio_shop_2, color = ~segment,
                           size = ~size, colors = colors, type = 'scatter', mode = 'markers', 
                           sizes = c(min(shop_2$size), max(shop_2$size)),
                           marker = list(symbol = 'circle', sizemode = 'diameter', 
                                         line = list(width = 0.5, color = '#800000')), 
                           text = ~paste('Customer ID:', supermarket_data_clean.customer_id, '<br>Loyality:',
                                         round(loyality_shop_2 * 100, digits = 2), '%')) %>% 
  layout(title = 'Product vs. Amount Purchased Ratio for Shop 2 (Segment - High)',
         xaxis = list(title = 'Product Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.0, 0.51), zerolinewidth = 1, ticklen = 5, gridwidth = 2),
         yaxis = list(title = 'Amount Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.0, 0.51), zerolinewidth = 1, ticklen = 5, gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)', plot_bgcolor = 'rgb(243, 243, 243)')

## Pie chart based on customer segments for Shop 2 
colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)')

pie_shop_2 <- plot_ly(shop_2_customer_segment_count, labels = ~customer_segments, values = ~counts, type = 'pie',
                      textposition = 'inside', textinfo = 'label+percent', 
                      insidetextfont = list(color = '#FFFFFF'), hoverinfo = 'text',
                      text = ~paste(customer_segments, '=', counts),
                      marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)),
                      ## 'pull' attribute can also be used to create space between the sectors
                      showlegend = FALSE) %>%
  layout(title = 'Customer Segment for Shop 2',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


## Plot product purchased ratio vs amount purchased ratio based on customer segments for shop 3 (all)
shop_3$segment <- ifelse(loyality_shop_3 > 0.9, 'high', 
                         ifelse(loyality_shop_3 > 0.50 & loyality_shop_3 <= 0.9, 'med',
                                ifelse(loyality_shop_3 > 0 & loyality_shop_3 <= 0.5, 'low', 'na')))

shop_3 <- shop_3[order(shop_3$prod_purch_ratio_shop_3, shop_3$amt_purch_ratio_shop_3),]
shop_3$size <- ((shop_3$loyality_shop_3 + 0.1) * 10)
colors <- c('#FFFF00', '#0000FF', '#808000', '#FFFFFF')

plot_shop_3 <- plot_ly(shop_3, x = ~prod_purch_ratio_shop_3, y = ~amt_purch_ratio_shop_3, color = ~segment,
                       size = ~size, colors = colors, type = 'scatter', mode = 'markers', 
                       sizes = c(min(shop_3$size), max(shop_3$size)),
                       marker = list(symbol = 'circle', sizemode = 'diameter', 
                                     line = list(width = 0.5, color = '#800000')), 
                       text = ~paste('Customer ID:', supermarket_data_clean.customer_id, '<br>Loyality:',
                                     round(loyality_shop_3 * 100, digits = 2), '%')) %>% 
  layout(title = 'Product vs. Amount Purchased Ratio for Shop 2',
         xaxis = list(title = 'Product Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0,1), zerolinewidth = 1, ticklen = 5, gridwidth = 2),
         yaxis = list(title = 'Amount Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0, 1), zerolinewidth = 1, ticklen = 5, gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)', plot_bgcolor = 'rgb(243, 243, 243)')

## Plot product purchased ratio vs amount purchased ratio based on customer segments for shop 3 (high)
shop_3_high <- shop_3[which(shop_3$segment == 'high'),]

plot_shop_3_high <- plot_ly(shop_3_high, x = ~prod_purch_ratio_shop_3, y = ~amt_purch_ratio_shop_3, color = ~segment,
                            size = ~size, colors = colors, type = 'scatter', mode = 'markers', 
                            sizes = c(min(shop_3$size), max(shop_3$size)),
                            marker = list(symbol = 'circle', sizemode = 'diameter', 
                                          line = list(width = 0.5, color = '#800000')), 
                            text = ~paste('Customer ID:', supermarket_data_clean.customer_id, '<br>Loyality:',
                                          round(loyality_shop_3 * 100, digits = 2), '%')) %>% 
  layout(title = 'Product vs. Amount Purchased Ratio for Shop 3 (Segment - High)',
         xaxis = list(title = 'Product Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.9, 1.01), zerolinewidth = 1, ticklen = 5, gridwidth = 2),
         yaxis = list(title = 'Amount Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.9, 1.01), zerolinewidth = 1, ticklen = 5, gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)', plot_bgcolor = 'rgb(243, 243, 243)')

## Plot product purchased ratio vs amount purchased ratio based on customer segments for shop 3 (med)
shop_3_med <- shop_3[which(shop_3$segment == 'med'),]

plot_shop_3_med <- plot_ly(shop_3_med, x = ~prod_purch_ratio_shop_3, y = ~amt_purch_ratio_shop_3, color = ~segment,
                           size = ~size, colors = colors, type = 'scatter', mode = 'markers', 
                           sizes = c(min(shop_3$size), max(shop_3$size)),
                           marker = list(symbol = 'circle', sizemode = 'diameter', 
                                         line = list(width = 0.5, color = '#800000')), 
                           text = ~paste('Customer ID:', supermarket_data_clean.customer_id, '<br>Loyality:',
                                         round(loyality_shop_3 * 100, digits = 2), '%')) %>% 
  layout(title = 'Product vs. Amount Purchased Ratio for Shop 3 (Segment - Med)',
         xaxis = list(title = 'Product Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.5, 0.91), zerolinewidth = 1, ticklen = 5, gridwidth = 2),
         yaxis = list(title = 'Amount Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.5, 0.91), zerolinewidth = 1, ticklen = 5, gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)', plot_bgcolor = 'rgb(243, 243, 243)')

## Plot product purchased ratio vs amount purchased ratio based on customer segments for shop 3 (low)
shop_3_low <- shop_3[which(shop_3$segment == 'low'),]

plot_shop_3_low <- plot_ly(shop_3_low, x = ~prod_purch_ratio_shop_3, y = ~amt_purch_ratio_shop_3, color = ~segment,
                           size = ~size, colors = colors, type = 'scatter', mode = 'markers', 
                           sizes = c(min(shop_3$size), max(shop_3$size)),
                           marker = list(symbol = 'circle', sizemode = 'diameter', 
                                         line = list(width = 0.5, color = '#800000')), 
                           text = ~paste('Customer ID:', supermarket_data_clean.customer_id, '<br>Loyality:',
                                         round(loyality_shop_3 * 100, digits = 2), '%')) %>% 
  layout(title = 'Product vs. Amount Purchased Ratio for Shop 3 (Segment - High)',
         xaxis = list(title = 'Product Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.0, 0.51), zerolinewidth = 1, ticklen = 5, gridwidth = 2),
         yaxis = list(title = 'Amount Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.0, 0.51), zerolinewidth = 1, ticklen = 5, gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)', plot_bgcolor = 'rgb(243, 243, 243)')

## Pie chart based on customer segments for Shop 3 
colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)')

pie_shop_3 <- plot_ly(shop_3_customer_segment_count, labels = ~customer_segments, values = ~counts, type = 'pie',
                      textposition = 'inside', textinfo = 'label+percent', 
                      insidetextfont = list(color = '#FFFFFF'), hoverinfo = 'text',
                      text = ~paste(customer_segments, '=', counts),
                      marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)),
                      ## 'pull' attribute can also be used to create space between the sectors
                      showlegend = FALSE) %>%
  layout(title = 'Customer Segment for Shop 3',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


## Plot product purchased ratio vs amount purchased ratio based on customer segments for shop 4 (all)
shop_4$segment <- ifelse(loyality_shop_4 > 0.9, 'high', 
                         ifelse(loyality_shop_4 > 0.50 & loyality_shop_4 <= 0.9, 'med',
                                ifelse(loyality_shop_4 > 0 & loyality_shop_4 <= 0.5, 'low', 'na')))

shop_4 <- shop_4[order(shop_4$prod_purch_ratio_shop_4, shop_4$amt_purch_ratio_shop_4),]
shop_4$size <- ((shop_4$loyality_shop_4 + 0.1) * 10)
colors <- c('#FFFF00', '#0000FF', '#808000', '#FFFFFF')

plot_shop_4 <- plot_ly(shop_4, x = ~prod_purch_ratio_shop_4, y = ~amt_purch_ratio_shop_4, color = ~segment,
                       size = ~size, colors = colors, type = 'scatter', mode = 'markers', 
                       sizes = c(min(shop_4$size), max(shop_4$size)),
                       marker = list(symbol = 'circle', sizemode = 'diameter', 
                                     line = list(width = 0.5, color = '#800000')), 
                       text = ~paste('Customer ID:', supermarket_data_clean.customer_id, '<br>Loyality:',
                                     round(loyality_shop_4 * 100, digits = 2), '%')) %>% 
  layout(title = 'Product vs. Amount Purchased Ratio for Shop 2',
         xaxis = list(title = 'Product Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0,1), zerolinewidth = 1, ticklen = 5, gridwidth = 2),
         yaxis = list(title = 'Amount Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0, 1), zerolinewidth = 1, ticklen = 5, gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)', plot_bgcolor = 'rgb(243, 243, 243)')

## Plot product purchased ratio vs amount purchased ratio based on customer segments for shop 4 (high)
shop_4_high <- shop_4[which(shop_4$segment == 'high'),]

plot_shop_4_high <- plot_ly(shop_4_high, x = ~prod_purch_ratio_shop_4, y = ~amt_purch_ratio_shop_4, color = ~segment,
                            size = ~size, colors = colors, type = 'scatter', mode = 'markers', 
                            sizes = c(min(shop_4$size), max(shop_4$size)),
                            marker = list(symbol = 'circle', sizemode = 'diameter', 
                                          line = list(width = 0.5, color = '#800000')), 
                            text = ~paste('Customer ID:', supermarket_data_clean.customer_id, '<br>Loyality:',
                                          round(loyality_shop_4 * 100, digits = 2), '%')) %>% 
  layout(title = 'Product vs. Amount Purchased Ratio for shop 4 (Segment - High)',
         xaxis = list(title = 'Product Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.9, 1.01), zerolinewidth = 1, ticklen = 5, gridwidth = 2),
         yaxis = list(title = 'Amount Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.9, 1.01), zerolinewidth = 1, ticklen = 5, gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)', plot_bgcolor = 'rgb(243, 243, 243)')

## Plot product purchased ratio vs amount purchased ratio based on customer segments for shop 4 (med)
shop_4_med <- shop_4[which(shop_4$segment == 'med'),]

plot_shop_4_med <- plot_ly(shop_4_med, x = ~prod_purch_ratio_shop_4, y = ~amt_purch_ratio_shop_4, color = ~segment,
                           size = ~size, colors = colors, type = 'scatter', mode = 'markers', 
                           sizes = c(min(shop_4$size), max(shop_4$size)),
                           marker = list(symbol = 'circle', sizemode = 'diameter', 
                                         line = list(width = 0.5, color = '#800000')), 
                           text = ~paste('Customer ID:', supermarket_data_clean.customer_id, '<br>Loyality:',
                                         round(loyality_shop_4 * 100, digits = 2), '%')) %>% 
  layout(title = 'Product vs. Amount Purchased Ratio for shop 4 (Segment - Med)',
         xaxis = list(title = 'Product Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.5, 0.91), zerolinewidth = 1, ticklen = 5, gridwidth = 2),
         yaxis = list(title = 'Amount Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.5, 0.91), zerolinewidth = 1, ticklen = 5, gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)', plot_bgcolor = 'rgb(243, 243, 243)')

## Plot product purchased ratio vs amount purchased ratio based on customer segments for shop 4 (low)
shop_4_low <- shop_4[which(shop_4$segment == 'low'),]

plot_shop_4_low <- plot_ly(shop_4_low, x = ~prod_purch_ratio_shop_4, y = ~amt_purch_ratio_shop_4, color = ~segment,
                           size = ~size, colors = colors, type = 'scatter', mode = 'markers', 
                           sizes = c(min(shop_4$size), max(shop_4$size)),
                           marker = list(symbol = 'circle', sizemode = 'diameter', 
                                         line = list(width = 0.5, color = '#800000')), 
                           text = ~paste('Customer ID:', supermarket_data_clean.customer_id, '<br>Loyality:',
                                         round(loyality_shop_4 * 100, digits = 2), '%')) %>% 
  layout(title = 'Product vs. Amount Purchased Ratio for shop 4 (Segment - High)',
         xaxis = list(title = 'Product Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.0, 0.51), zerolinewidth = 1, ticklen = 5, gridwidth = 2),
         yaxis = list(title = 'Amount Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.0, 0.51), zerolinewidth = 1, ticklen = 5, gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)', plot_bgcolor = 'rgb(243, 243, 243)')

## Pie chart based on customer segments for shop 4 
colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)')

pie_shop_4 <- plot_ly(shop_4_customer_segment_count, labels = ~customer_segments, values = ~counts, type = 'pie',
                      textposition = 'inside', textinfo = 'label+percent', 
                      insidetextfont = list(color = '#FFFFFF'), hoverinfo = 'text',
                      text = ~paste(customer_segments, '=', counts),
                      marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)),
                      ## 'pull' attribute can also be used to create space between the sectors
                      showlegend = FALSE) %>%
  layout(title = 'Customer Segment for shop 4',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


## Plot product purchased ratio vs amount purchased ratio based on customer segments for shop 5 (all)
shop_5$segment <- ifelse(loyality_shop_5 > 0.9, 'high', 
                         ifelse(loyality_shop_5 > 0.50 & loyality_shop_5 <= 0.9, 'med',
                                ifelse(loyality_shop_5 > 0 & loyality_shop_5 <= 0.5, 'low', 'na')))

shop_5 <- shop_5[order(shop_5$prod_purch_ratio_shop_5, shop_5$amt_purch_ratio_shop_5),]
shop_5$size <- ((shop_5$loyality_shop_5 + 0.1) * 10)
colors <- c('#FFFF00', '#0000FF', '#808000', '#FFFFFF')

plot_shop_5 <- plot_ly(shop_5, x = ~prod_purch_ratio_shop_5, y = ~amt_purch_ratio_shop_5, color = ~segment,
                       size = ~size, colors = colors, type = 'scatter', mode = 'markers', 
                       sizes = c(min(shop_5$size), max(shop_5$size)),
                       marker = list(symbol = 'circle', sizemode = 'diameter', 
                                     line = list(width = 0.5, color = '#800000')), 
                       text = ~paste('Customer ID:', supermarket_data_clean.customer_id, '<br>Loyality:',
                                     round(loyality_shop_5 * 100, digits = 2), '%')) %>% 
  layout(title = 'Product vs. Amount Purchased Ratio for Shop 2',
         xaxis = list(title = 'Product Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0,1), zerolinewidth = 1, ticklen = 5, gridwidth = 2),
         yaxis = list(title = 'Amount Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0, 1), zerolinewidth = 1, ticklen = 5, gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)', plot_bgcolor = 'rgb(243, 243, 243)')

## Plot product purchased ratio vs amount purchased ratio based on customer segments for shop 5 (high)
shop_5_high <- shop_5[which(shop_5$segment == 'high'),]

plot_shop_5_high <- plot_ly(shop_5_high, x = ~prod_purch_ratio_shop_5, y = ~amt_purch_ratio_shop_5, color = ~segment,
                            size = ~size, colors = colors, type = 'scatter', mode = 'markers', 
                            sizes = c(min(shop_5$size), max(shop_5$size)),
                            marker = list(symbol = 'circle', sizemode = 'diameter', 
                                          line = list(width = 0.5, color = '#800000')), 
                            text = ~paste('Customer ID:', supermarket_data_clean.customer_id, '<br>Loyality:',
                                          round(loyality_shop_5 * 100, digits = 2), '%')) %>% 
  layout(title = 'Product vs. Amount Purchased Ratio for shop 5 (Segment - High)',
         xaxis = list(title = 'Product Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.9, 1.01), zerolinewidth = 1, ticklen = 5, gridwidth = 2),
         yaxis = list(title = 'Amount Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.9, 1.01), zerolinewidth = 1, ticklen = 5, gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)', plot_bgcolor = 'rgb(243, 243, 243)')

## Plot product purchased ratio vs amount purchased ratio based on customer segments for shop 5 (med)
shop_5_med <- shop_5[which(shop_5$segment == 'med'),]

plot_shop_5_med <- plot_ly(shop_5_med, x = ~prod_purch_ratio_shop_5, y = ~amt_purch_ratio_shop_5, color = ~segment,
                           size = ~size, colors = colors, type = 'scatter', mode = 'markers', 
                           sizes = c(min(shop_5$size), max(shop_5$size)),
                           marker = list(symbol = 'circle', sizemode = 'diameter', 
                                         line = list(width = 0.5, color = '#800000')), 
                           text = ~paste('Customer ID:', supermarket_data_clean.customer_id, '<br>Loyality:',
                                         round(loyality_shop_5 * 100, digits = 2), '%')) %>% 
  layout(title = 'Product vs. Amount Purchased Ratio for shop 5 (Segment - Med)',
         xaxis = list(title = 'Product Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.5, 0.91), zerolinewidth = 1, ticklen = 5, gridwidth = 2),
         yaxis = list(title = 'Amount Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.5, 0.91), zerolinewidth = 1, ticklen = 5, gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)', plot_bgcolor = 'rgb(243, 243, 243)')

## Plot product purchased ratio vs amount purchased ratio based on customer segments for shop 5 (low)
shop_5_low <- shop_5[which(shop_5$segment == 'low'),]

plot_shop_5_low <- plot_ly(shop_5_low, x = ~prod_purch_ratio_shop_5, y = ~amt_purch_ratio_shop_5, color = ~segment,
                           size = ~size, colors = colors, type = 'scatter', mode = 'markers', 
                           sizes = c(min(shop_5$size), max(shop_5$size)),
                           marker = list(symbol = 'circle', sizemode = 'diameter', 
                                         line = list(width = 0.5, color = '#800000')), 
                           text = ~paste('Customer ID:', supermarket_data_clean.customer_id, '<br>Loyality:',
                                         round(loyality_shop_5 * 100, digits = 2), '%')) %>% 
  layout(title = 'Product vs. Amount Purchased Ratio for shop 5 (Segment - High)',
         xaxis = list(title = 'Product Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.0, 0.51), zerolinewidth = 1, ticklen = 5, gridwidth = 2),
         yaxis = list(title = 'Amount Purchased Ratio', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0.0, 0.51), zerolinewidth = 1, ticklen = 5, gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)', plot_bgcolor = 'rgb(243, 243, 243)')

## Pie chart based on customer segments for shop 5 
colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)')

pie_shop_5 <- plot_ly(shop_5_customer_segment_count, labels = ~customer_segments, values = ~counts, type = 'pie',
                      textposition = 'inside', textinfo = 'label+percent', 
                      insidetextfont = list(color = '#FFFFFF'), hoverinfo = 'text',
                      text = ~paste(customer_segments, '=', counts),
                      marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)),
                      ## 'pull' attribute can also be used to create space between the sectors
                      showlegend = FALSE) %>%
  layout(title = 'Customer Segment for shop 5',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


## Customer segments for Shop 1 based on Loyality Score
high_end_shop_1 <- shop_1[which(shop_1$loyality_shop_1 > 0.9), ]
mid_range_shop_1 <- shop_1[which(shop_1$loyality_shop_1 > 0.50 & shop_1$loyality_shop_1 <= 0.9), ]
low_end_shop_1 <- shop_1[which(shop_1$loyality_shop_1 > 0 & shop_1$loyality_shop_1 <= 0.5), ]
na_shop_1 <- shop_1[which(shop_1$loyality_shop_1 == 0.0), ]

## Customer segments for Shop 2 based on Loyality Score
high_end_shop_2 <- shop_2[which(shop_2$loyality_shop_2 > 0.9), ]
mid_range_shop_2 <- shop_2[which(shop_2$loyality_shop_2 > 0.50 & shop_2$loyality_shop_2 <= 0.9), ]
low_end_shop_2 <- shop_2[which(shop_2$loyality_shop_2 > 0 & shop_2$loyality_shop_2 <= 0.5), ]
na_shop_2 <- shop_2[which(shop_2$loyality_shop_2 == 0.0), ]

## Customer segments for Shop 3 based on Loyality Score
high_end_shop_3 <- shop_3[which(shop_3$loyality_shop_3 > 0.9), ]
mid_range_shop_3 <- shop_3[which(shop_3$loyality_shop_3 > 0.50 & shop_3$loyality_shop_3 <= 0.9), ]
low_end_shop_3 <- shop_3[which(shop_3$loyality_shop_3 > 0 & shop_3$loyality_shop_3 <= 0.5), ]
na_shop_3 <- shop_3[which(shop_3$loyality_shop_3 == 0.0), ]

## Customer segments for Shop 4 based on Loyality Score
high_end_shop_4 <- shop_4[which(shop_4$loyality_shop_4 > 0.9), ]
mid_range_shop_4 <- shop_4[which(shop_4$loyality_shop_4 > 0.50 & shop_4$loyality_shop_4 <= 0.9), ]
low_end_shop_4 <- shop_4[which(shop_4$loyality_shop_4 > 0 & shop_4$loyality_shop_4 <= 0.5), ]
na_shop_4 <- shop_4[which(shop_4$loyality_shop_4 == 0.0), ]

## Customer segments for Shop 5 based on Loyality Score
high_end_shop_5 <- shop_5[which(shop_5$loyality_shop_5 > 0.9), ]
mid_range_shop_5 <- shop_5[which(shop_5$loyality_shop_5 > 0.50 & shop_5$loyality_shop_5 <= 0.9), ]
low_end_shop_5 <- shop_5[which(shop_5$loyality_shop_5 > 0 & shop_5$loyality_shop_5 <= 0.5), ]
na_shop_5 <- shop_5[which(shop_5$loyality_shop_5 == 0.0), ]

## Number of customers in each segment for Shop 1
count_high_end_shop_1 <- nrow(high_end_shop_1)
count_mid_range_shop_1 <- nrow(mid_range_shop_1)
count_low_end_shop_1 <- nrow(low_end_shop_1)
count_na_shop_1 <- nrow(na_shop_1)

## Number of customers in each segment for Shop 2
count_high_end_shop_2 <- nrow(high_end_shop_2)
count_mid_range_shop_2 <- nrow(mid_range_shop_2)
count_low_end_shop_2 <- nrow(low_end_shop_2)
count_na_shop_2 <- nrow(na_shop_2)

## Number of customers in each segment for Shop 3
count_high_end_shop_3 <- nrow(high_end_shop_3)
count_mid_range_shop_3 <- nrow(mid_range_shop_3)
count_low_end_shop_3 <- nrow(low_end_shop_3)
count_na_shop_3 <- nrow(na_shop_3)

## Number of customers in each segment for Shop 4
count_high_end_shop_4 <- nrow(high_end_shop_4)
count_mid_range_shop_4 <- nrow(mid_range_shop_4)
count_low_end_shop_4 <- nrow(low_end_shop_4)
count_na_shop_4 <- nrow(na_shop_4)

## Number of customers in each segment for Shop 5
count_high_end_shop_5 <- nrow(high_end_shop_5)
count_mid_range_shop_5 <- nrow(mid_range_shop_5)
count_low_end_shop_5 <- nrow(low_end_shop_5)
count_na_shop_5 <- nrow(na_shop_5)

## Setting up values
shops <- c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5)

customer_segments <- c("high", "med", "low", "na", "high", "med", "low", "na", "high", "med", "low", "na", "high", "med", "low", "na", "high", "med", "low", "na")

count_shop_1 <- c(count_high_end_shop_1, count_mid_range_shop_1, count_low_end_shop_1, count_na_shop_1)
count_shop_2 <- c(count_high_end_shop_2, count_mid_range_shop_2, count_low_end_shop_2, count_na_shop_2)
count_shop_3 <- c(count_high_end_shop_3, count_mid_range_shop_3, count_low_end_shop_3, count_na_shop_3)
count_shop_4 <- c(count_high_end_shop_4, count_mid_range_shop_4, count_low_end_shop_4, count_na_shop_4)
count_shop_5 <- c(count_high_end_shop_5, count_mid_range_shop_5, count_low_end_shop_5, count_na_shop_5)

counts <- c(count_shop_1, count_shop_2, count_shop_3, count_shop_4, count_shop_5)


## Count of customers based on the three customer segments (high, mid, low)
shop_customer_segment_count <-data.frame(shops, customer_segments, counts)

## Change datatype of columns to numerical
shop_customer_segment_count <- shop_customer_segment_count %>% mutate_if(is.integer, as.numeric) %>% mutate_if(is.factor, as.numeric)

## Plot the count of customers for each shop based on customer segments (high - 1, mid - 3, low - 2)
shop_customer_segment_count$size <- (shop_customer_segment_count$counts) ^ (1/3)

plot_shop_customer_segment_count <- plot_ly(shop_customer_segment_count, x = ~shops, y = ~counts, color = ~customer_segments,
             size = ~size, colors = colors, type = 'scatter', mode = 'markers', 
             sizes = c(min(shop_customer_segment_count$size), max(shop_customer_segment_count$size)),
             marker = list(symbol = 'circle', sizemode = 'diameter', 
                           line = list(width = 0.5, color = '#800000')), 
             text = ~paste('Shop:', shops, '<br>Segment:',
                           customer_segments, '<br>Count:', counts)) %>% 
  layout(title = 'Shops vs. Count of Customers per Segment',
         xaxis = list(title = 'Shops', gridcolor = 'rgb(255, 255, 255)',
                      range = c(0, 5.9), zerolinewidth = 1, ticklen = 5, gridwidth = 2),
         yaxis = list(title = 'Count of Customers', gridcolor = 'rgb(255, 255, 255)',
                      range = c(500, 52000), zerolinewidth = 1, ticklen = 5, gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)', plot_bgcolor = 'rgb(243, 243, 243)')
