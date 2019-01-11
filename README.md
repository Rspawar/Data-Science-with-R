# Customer Behavioural Analytics in the Retail sector
# Motivation
A customer is a key-centric factor for any business to be successful. Conventional wisdom tells us that the cost of retaining an existing customer is far less than acquiring a new one. In order that a business has a sustainable growth, the retention of its old customer base and expansion of the new customer base is very critical. This demands from a business to understand the behaviour of its customers in relation to the business. Therefore obtaining a 360&deg; view of its customers is crucial for a business looking for a competitive edge in the market. In such a scenario, Customer Behavioural Analytics plays an important role in leveraging data analytics to find meaningful behavioural patterns in the customer-specific business data.

# Synopsis
This project aims to understand the purchase behaviour of customers in the retail sector specifically of an Italian retail distribution company <i>Coop</i> in a single Italian city. Decoding the consumer behaviour is based on understanding how consumers make purchase decisions and what factors influence those decisions. This project also discovers existence of dependencies between customers, products and shops to highlight further insights about their behaviour. These meaningful insights will further help a business to implement strategies leading to an increased revenue through customer satisfaction. 

# Overview of the Input Dataset
The dataset used is Supermarket aggr.Customer [1]. The dataset contains the retail market data of one of the largest Italian retail distribution company called <i>Coop</i> for a single Italian city.<br />
The Supermarket aggr.Customer dataset used for the analysis contains data aggregated from customer and information from shops [2] [3] and pivoted to new columns. The dataset thus contains 40 features with 60,366 instances and is approximately 14.0 MB in size.

# Installation and Run

1. Install the following packages in Rstudio

```
   install.packages("pacman")    
```
```
   pacman::p_load(char = c(
   "tidyverse",
   "DataExplorer",
   "corrplot",
   "corrr",
   "deldir",
   "e1071",
   ))
```

2. Open and Run the Customer_Behavioural_Analytics_in_the_Retail_sector.rmd file in Rstudio

3. Select Knit to html_document to generate the detailed Analysis Report Customer_Behavioural_Analytics_in_the_Retail_sector.html

# Output
The output is a detailed Analysis Report rendered as a HTML file Customer_Behavioural_Analytics_in_the_Retail_sector.html

<i>[Click here](https://cdn.staticaly.com/gh/Rspawar/Data-Science-with-R/2d40603b/Customer_Behavioural_Analytics_in_the_Retail_sector.html) to view the HTML file directly in the browser</i>

# Links
Project Website:

Project Screencast: 

Project Presentation:

# Contributors
Rutuja Shivraj Pawar, Nadiia Honcharenko, Shivani Jadhav, Sumit Kundu

# License
This project is licensed under the terms of the MIT license.

# References
[1] https://bigml.com/user/czuriaga/gallery/dataset/5559c2c6200d5a6570000084

[2] Pennacchioli, D., Coscia, M., Rinzivillo, S., Pedreschi, D. and Giannotti, F., 2013, October. Explaining the product range effect in purchase data. In Big Data, 2013 IEEE International Conference on (pp. 648-656). IEEE.

[3] http://www.michelecoscia.com/?page_id=379


