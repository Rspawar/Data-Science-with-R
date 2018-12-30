#### Boruta ####
## Install the package
##install.packages("Boruta")

## Call the package
library(Boruta)

## Set Pseudo Random Number Generator
set.seed(1234)

## Implement Boruta
cus_boruta <- Boruta(class ~ ., data = supermarket_data_clean, doTrace = 1)

## check the performance of Boruta
print(cus_boruta)