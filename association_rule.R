# This file intends to use association rules
# to explore the recognize the transactional pattern 
# to future determine the co-occurence between items.
<<<<<<< HEAD
# The application of the association rules is to 
# 
=======

library(tidyverse)
library(arules)
library(arulesViz)

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

# choose the variables
tr1 = transaction %>% 
  select(order_id,product_id,product_category_name) %>% 
  na.omit(tr1$product_category_name) %>% 
  na.omit(tr1$product_id)
readr::write_csv(tr1,"data/tran1.csv")
tr2 = transaction %>% 
  select(order_id,product_id) %>% 
  na.omit(tr2$product_id)
readr::write_csv(tr2,"data/tran2.csv")
tr3 = transaction %>% 
  select(order_id,product_category_name) %>% 
  na.omit(tr3$product_category_name)
readr::write_csv(tr3,"data/tran3.csv")
dim(tr3)
length(unique(tr3$order_id))

# get the data into the transaction format
t2 = read.transactions("data/tran2.csv",
                       format = "single",
                       header = T,
                       sep = ",",
                       cols=c("order_id","product_id"),
                       rm.duplicates = T
                       
)
summary(t2)
t3 = read.transactions("data/tran3.csv",
                       format = "single",
                       header = T,
                       sep = ",",
                       cols=c("order_id","product_category_name"),
                       rm.duplicates = T
                       
)
summary(t3)
hist(size(t3))
itemFrequency(t3,type = "absolute")

# establish the rules
rules = apriori(t3,
                parameter = list(supp = .001,
                                 conf = .01,
                                 minlen = 2,
                                 target = "rules"))
summary(rules)






>>>>>>> cea335b1c2207b460a28c58d74442c818da65ad9
