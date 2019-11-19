# This file intends to use association rules
# to explore the recognize the transactional pattern 
# to future determine the co-occurence between items.

# The application of the association rules is to 
# 


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
View(transaction)
length(unique(transaction$customer_id))
length(unique(transaction$customer_unique_id))

# choose the variables
# tr1 = transaction %>% 
#   select(order_id,product_id,product_category_name) %>% 
#   na.omit(tr1$product_category_name) %>% 
#   na.omit(tr1$product_id)
# readr::write_csv(tr1,"data/tran1.csv")
# tr2 = transaction %>% 
#   select(order_id,product_id) %>% 
#   na.omit(tr2$product_id)
# readr::write_csv(tr2,"data/tran2.csv")
# tr3 = transaction %>% 
#   select(order_id,product_category_name) %>% 
#   na.omit(tr3$product_category_name)
# readr::write_csv(tr3,"data/tran3.csv")
# dim(tr3)
# length(unique(tr3$order_id))
# length(unique(tr3$order_id)) / nrow(tr3)
# now we try the customer_id and product_category_name
tr4 = transaction %>% 
  select(customer_unique_id,product_category_name) %>% 
  na.omit(tr4$product_category_name)
readr::write_csv(tr4,"data/tran4.csv")

# get the tr4 into the transaction format
t4 = read.transactions("data/tran4.csv",
                       format = "single",
                       header = T,
                       sep = ",",
                       cols=c("customer_unique_id","product_category_name"),
                       rm.duplicates = T

)
summary(t4)

# establish the rules with 0.1% support and confidence level
rules2 = apriori(t4,
                 parameter = list(supp = .001,
                                  conf = .001,
                                  minlen = 2,
                                  target = "rules"))
summary(rules2)
inspect(rules2)
# {moveis_decoracao} => {cama_mesa_banho}
# furniture_decoration -> bed table bath
# The only two rules are the relationship between furnitures and bed bath table
# The total transaction we use for this rule is around 95.


# If we want to establish more rules with lower support .01%
rules1 = apriori(t4,
                 parameter = list(supp = .0001,
                                  conf = .001,
                                  minlen = 2,
                                  target = "rules"))
summary(rules1)
#### Comments:
# we have a set of 146 rules when we have 0.01% support and 0.01% confidence
## sort the rules decreasing by lift - print out the first 5
inspect(head(sort(rules1,decreasing = T, by = "lift"),5))
##  We can add other interest measures
##  we do this by calculating the measure, and then cbinding it
## to the QUALITY of our rules
## ?interestMeasure
## chisquare - test of independence between LHS and RHS, p < .05 is depdenence
rule_chisq = interestMeasure(rules1,
                             measure="chiSquared",
                             transactions = t4,
                             significance = T)
quality(rules1) = cbind(quality(rules1),rule_chisq)
inspect(head(rules1,5))










# get the data into the transaction format
# t2 = read.transactions("data/tran2.csv",
#                        format = "single",
#                        header = T,
#                        sep = ",",
#                        cols=c("order_id","product_id"),
#                        rm.duplicates = T
#                        
# )
# summary(t2)
# t3 = read.transactions("data/tran3.csv",
#                        format = "single",
#                        header = T,
#                        sep = ",",
#                        cols=c("order_id","product_category_name"),
#                        rm.duplicates = T
#                        
# )
# summary(t3)
# hist(size(t3))
# itemFrequency(t3,type = "absolute")
# 
# # establish the rules
# rules = apriori(t3,
#                 parameter = list(supp = .001,
#                                  conf = .001,
#                                  minlen = 2,
#                                  target = "rules"))
# summary(rules)









