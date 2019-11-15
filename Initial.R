library(readr)
library(tidyverse)
library(skimr)

#####reading files
customer <- read.csv('data/olist_customers_dataset.csv')
order_item <- read.csv('data/olist_order_items_dataset.csv')
order_item <- read.csv('data/olist_order_items_dataset.csv')
order_payment <- read.csv('data/olist_order_payments_dataset.csv')
order_reviews <- read.csv('data/olist_order_reviews_dataset.csv')
order <- read.csv('data/olist_orders_dataset.csv')
order_product <- read.csv('data/olist_products_dataset.csv')
product <- read.csv('data/olist_products_dataset.csv')
location <- read.csv('data/olist_geolocation_dataset.csv')



#######
p<- left_join(left_join(left_join(customer, order, by = 'customer_id'),order_item,by = 'order_id'),order_product, by = 'product_id')
transaction <- left_join(p,order_payment, by = "order_id")
View(p)
head(p)



## Assoction Rule


## PCA
# Keep only numerica columns
p2 = p %>% select(-customer_id, -customer_unique_id, -customer_city, -customer_city, 
                 -customer_state,-order_id:-order_estimated_delivery_date, 
                 -product_id:-shipping_limit_date, -product_category_name)
glimpse(p)
skimr::skim(p)

# Clean missing values
p = p %>% replace_na(list(order_item_id=0))

# Correlation Matrix
p_cor = cor(p)
p_cor
corrplot(p_cor, type="upper")
corrplot(p_cor, method="number")

# Fit PCA Model
p_pca = prcomp(p, center=T, scale = FALSE)
class(p_pca)
summary(p_pca)
is.list(p_pca)
names(p_pca)
p_pca$rotation

## EFA





