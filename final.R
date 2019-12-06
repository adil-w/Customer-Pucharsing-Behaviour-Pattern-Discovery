install.packages("tm")
# install the packages
library(readr)
library(tidyverse)
library(skimr)
library(dplyr)
library(purrr)
library(cluster)
library(factoextra)
library(corrplot)
library(arules)
library(arulesViz)
library(psych)
library(GPArotation)
library(leaflet)
library(tm)

# load the datasets
customer <- read.csv('data/olist_customers_dataset.csv')
order_item <- read.csv('data/olist_order_items_dataset.csv')
customer <- read.csv('data/olist_customers_dataset.csv')
order_item <- read.csv('data/olist_order_items_dataset.csv')
order_payment <- read.csv('data/olist_order_payments_dataset.csv')
order_reviews <- read.csv('data/olist_order_reviews_dataset.csv')
order <- read.csv('data/olist_orders_dataset.csv')
order_product <- read.csv('data/olist_products_dataset.csv')
product <- read.csv('data/olist_products_dataset.csv')
location <- read.csv('data/olist_geolocation_dataset.csv')
sellers <- read.csv('data/olist_sellers_dataset.csv')
location <- unique(location)


location1 = location %>% group_by(geolocation_zip_code_prefix) %>% 
  summarize(mean_lat = mean(geolocation_lat),
            mean_long = mean(geolocation_lng))

#######
p<- left_join(left_join(left_join(customer, order, by = 'customer_id'),order_item,by = 'order_id'),order_product, by = 'product_id')
transaction <- left_join(p,order_payment, by = "order_id")
transaction2 <- left_join(transaction,location1,
                          by = c("customer_zip_code_prefix"="geolocation_zip_code_prefix"))
transaction2 <- left_join(transaction2,sellers)



clean_transaction <-na.omit(transaction)
clean_tra= clean_transaction %>% select(-customer_zip_code_prefix, -order_item_id,-customer_id, -customer_unique_id, -customer_city, -customer_city, 
                                      -customer_state,-order_id:-order_estimated_delivery_date, 
                                      -product_id:-shipping_limit_date, -product_category_name, -payment_type, -payment_installments)


## Assoction Rule

##tr4 = transaction %>% 
##select(customer_unique_id,product_category_name)
##readr::write_csv(tr4,"data/tran4.csv")

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
rules = apriori(t4,
                 parameter = list(supp = .001,
                                  conf = .001,
                                  minlen = 2,
                                  target = "rules"))
summary(rules)
inspect(rules)
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
inspect(rules1)

#### Comments:
# we have a set of 146 rules when we have 0.01% support and 0.01% confidence
## sort the rules decreasing by lift - print out the first 5

inspect(head(sort(rules1,decreasing = T, by = "lift"),5))
#{cama_mesa_banho}& {casa_conforto}
#{casa_conforto}&{cama_mesa_banho}


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

## if we wanna visulize the categories purchased at high frequency

## see location information about

select <- transaction2 %>% filter(product_category_name %in% c('casa_conforto','cama_mesa_banho','moveis_decoracao'))
select <- select %>% select(-customer_id,-customer_unique_id)


hist(select$customer_zip_code_prefix)
ggplot(select, aes(x  = customer_city))+
  geom_bar()

select %>% group_by(customer_city) %>%
  count(sort = T) %>%
  print(15)

select %>% group_by(customer_zip_code_prefix) %>%
  count(sort = T) %>% 
  print(15)

###try map
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(select$mean_long,
                   select$mean_lat,
                   color = select$product_category_name,
                   radius = 0.5,
                   fill = T,
                   fillOpacity = 0.2,
                   opacity = 0.6,
                   popup = paste(select$product_category_name,select$mean_lat,select$mean_long,sep = "")) %>%
  addLegend("topright",
            colors = c("#a9a9a9","red", "blue"),
            labels = c('casa_conforto','cama_mesa_banho','moveis_decoracao'),
            opacity = 2.0)
            
### R will nearly crush by this function, BE CAREFUL

## Can we figure out in which city dilivery time is faster
delivery <- transaction2 %>% select(customer_zip_code_prefix:customer_state, order_purchase_timestamp:order_estimated_delivery_date,
                                    product_category_name, mean_lat, mean_long)
delivery$order_estimated_delivery_date <- as.POSIXct(delivery$order_estimated_delivery_date, format="%Y-%m-%d %H:%M:%S")
delivery$order_approved_at <- as.POSIXct(delivery$order_approved_at, format="%Y-%m-%d %H:%M:%S")
delivery$order_delivered_customer_date <- as.POSIXct(delivery$order_delivered_customer_date, format="%Y-%m-%d %H:%M:%S")

delivery$deliverd_difftime <- as.numeric(difftime(delivery$order_delivered_customer_date ,delivery$order_estimated_delivery_date)/3600/24)
class(delivery$deliverd_difftime)
hist(delivery$deliverd_difftime)

delivery_late <- delivery %>% filter(deliverd_difftime > 0)
##is there certain goods/ city have higher possibility to late 

zip_late <- delivery_late %>% group_by(customer_zip_code_prefix) %>% 
  count(sort = T) %>% 

zip_total <- transaction2 %>% group_by(customer_zip_code_prefix) %>% 
  count(sort= T)

zip <- left_join(zip_late, zip_total,by = ("customer_zip_code_prefix"))
zip <- left_join(zip, location1, by = c("customer_zip_code_prefix"='geolocation_zip_code_prefix'))
zip <- zip %>% mutate(late_rate = n.x/n.y) %>% arrange(late_rate)
zip %>% filter(late_rate >= 0.3 & n.y >= 10) 


leaflet() %>%
  addTiles() %>%
  addCircleMarkers(zip$mean_long,
                   zip$mean_lat,
                   color = zip$late_rate,
                   radius = 0.5,
                   fill = T,
                   fillOpacity = 0.2,
                   opacity = 0.6)

### is there any pattern on product catogory? 
ggplot(delivery_late,aes(x =product_category_name))+
  geom_bar()

late_catogories <- delivery_late %>% group_by(product_category_name) %>% 
  summarise(mean = mean(deliverd_difftime),
            n = n()) %>% 
  arrange(desc(mean))

## Text Analysis
View(order_reviews)
names(order_reviews)
order_reviews = na.omit(order_reviews)
order_reviews$review_comment_message = tolower(order_reviews$review_comment_message)
order_reviews1 <- order_reviews %>%
  unnest_tokens(token, review_comment_message) 
stopwords::stopwords_getsources() 
stopwords("portuguese")
head(stopwords("portuguese"))
#order_reviews <- tm_map(order_reviews, removeWords, as.data.frame(stopwords("portuguese")))
stopwords::stopwords_getlanguages("misc") 
stopwords::stopwords_getlanguages("snowball") 
stopwords::stopwords_getlanguages("stopwords-iso") 
stopwords::stopwords_getlanguages("smart") 
order_reviews2 <- order_reviews1 %>%
  anti_join(get_stopwords(), by = c('token' = 'word')) 
order_reviews_sum <- order_reviews2 %>%
  group_by(token) %>%
  count(sort = T)

## Word Cloud Plot
wordcloud(words = order_reviews_sum$token,
          freq = order_reviews_sum$n, min.freq = 10, max.words = 50)

## LDA model
order_reviews_corpus <- corpus(order_reviews$review_comment_message) 
order_reviews_corpus1 <- tm_map(order_reviews_corpus, removeWords, c("de", "o", "que", "e"))

summary(order_reviews_corpus, n = 20, showmeta = T) 
order_reviews_dfm <- dfm(order_reviews_corpus,remove_punct= T,remove = stopwords(), remove_numbers= T, remove_symbols= T) %>%
  dfm_trim(min_termfreq = 2, max_docfreq = .5,
           docfreq_type = "prop") 
order_reviews_dtm <- convert(order_reviews_dfm, 'topicmodels')
order_reviews_lda <- LDA(order_reviews_dtm, k = 2, control = list(seed = 729))
terms(order_reviews_lda, 10)

