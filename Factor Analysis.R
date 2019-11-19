library(tidyverse)
library(arules)
library(arulesViz)
library(factoextra)
library(skimr)
library(psych)
library(GPArotation)

# load the dataset
customer <- read.csv('data/olist_customers_dataset.csv')
order_item <- read.csv('data/olist_order_items_dataset.csv')
order_payment <- read.csv('data/olist_order_payments_dataset.csv')
order_reviews <- read.csv('data/olist_order_reviews_dataset.csv')
order <- read.csv('data/olist_orders_dataset.csv')
order_product <- read.csv('data/olist_products_dataset.csv')
product <- read.csv('data/olist_products_dataset.csv')
location <- read.csv('data/olist_geolocation_dataset.csv')
p<- left_join(left_join(left_join(customer, order, by = 'customer_id'),order_item,by = 'order_id'),order_product, by = 'product_id')
transaction <- left_join(p,order_payment, by = "order_id")

# clean data 
skimr::skim(transaction)
transaction = na.omit(transaction)
transaction = transaction %>%  select(-customer_id, -customer_unique_id, -customer_city, -customer_city, 
       -customer_state,-order_id:-order_estimated_delivery_date, 
       -product_id:-shipping_limit_date, -product_category_name, - payment_type, - order_item_id )


# Data Assessment 
transaction_cor = cor(transaction)
cortest.bartlett(transaction_cor, nrow(transaction))
KMO(transaction_cor)

# Number of factors 

pca_eval = prcomp(transaction, center=T, scale=T)
fviz_screeplot(pca_eval, addlabels=T)


# Parallel plot 
fa_plot = fa.parallel(transaction, fm="ml", fa="fa")
fa_plot

sum(fa_plot$fa.values > .7)


#Apply the model 
fa1 = fa(transaction, nfactors=2, rotate = "oblimin", fm = "ml")

print(fa1, cut=.3)


#Assess Fit 
mf1 = transaction %>% select(price,  product_weight_g, product_height_cm, product_length_cm,
                         payment_value)
psych::alpha(mf1)





