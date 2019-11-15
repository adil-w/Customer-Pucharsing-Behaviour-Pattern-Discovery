
# install the packages
library(readr)
library(tidyverse)

<<<<<<< HEAD
# load the datasets
customer <- read.csv('data/olist_customers_dataset.csv')
order_item <- read.csv('data/olist_order_items_dataset.csv')
=======
#####reading files
customer <- read.csv('data/olist_customers_dataset.csv')
order_item <- read.csv('data/olist_order_items_dataset.csv')
order_item <- read.csv('data/olist_order_items_dataset.csv')
>>>>>>> 5ff1c7396338bf607bfb426ff31dc1122be25c03
order_payment <- read.csv('data/olist_order_payments_dataset.csv')
order_reviews <- read.csv('data/olist_order_reviews_dataset.csv')
order <- read.csv('data/olist_orders_dataset.csv')
order_product <- read.csv('data/olist_products_dataset.csv')
<<<<<<< HEAD

=======
product <- read.csv('data/olist_products_dataset.csv')
location <- read.csv('data/olist_geolocation_dataset.csv')


#######
p<- left_join(left_join(left_join(customer, order, by = 'customer_id'),order_item,by = 'order_id'),order_product, by = 'product_id')
transaction <- left_join(p,order_payment, by = "order_id")
>>>>>>> 5ff1c7396338bf607bfb426ff31dc1122be25c03
