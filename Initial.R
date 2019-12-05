
# install the packages
library(readr)
library(tidyverse)
library(skimr)
install.packages("googleLanguageR")
install.packages("translateR")
library(googleLanguageR)
library(translateR)

# load the datasets
customer <- read.csv('data/olist_customers_dataset.csv')
order_item <- read.csv('data/olist_order_items_dataset.csv')
customer <- read.csv('data/olist_customers_dataset.csv')
order_item <- read.csv('data/olist_order_items_dataset.csv')
order_item <- read.csv('data/olist_order_items_dataset.csv')
order_payment <- read.csv('data/olist_order_payments_dataset.csv')
order_reviews <- read.csv('data/olist_order_reviews_dataset.csv')
order <- read.csv('data/olist_orders_dataset.csv')
order_product <- read.csv('data/olist_products_dataset.csv')



product <- read.csv('data/olist_products_dataset.csv')
location <- read.csv('data/olist_geolocation_dataset.csv')



<<<<<<< HEAD
#######
p<- left_join(left_join(left_join(customer, order, by = 'customer_id'),order_item,by = 'order_id'),order_product, by = 'product_id')
transaction <- left_join(p,order_payment, by = "order_id")
View(p)
head(p)
dim(p)
skimr::skim(p)

## Clean data



## Assoction Rule


## PCA
# Keep only numerica columns
transaction = na.omit(transaction)
View(transaction)
transaction2 = transaction %>% select(-customer_zip_code_prefix, -order_item_id,-customer_id, 
                                      -customer_unique_id, -customer_city, -customer_city, 
                                      -customer_state,-order_id:-order_estimated_delivery_date, 
                                      -product_id:-shipping_limit_date, -product_category_name, 
                                      -payment_type, -payment_installments)
View(transaction2)
names(transaction2)
glimpse(transaction2)
skimr::skim(transaction2)
dim(transaction2)
class(transaction2)
is.list(transaction2)

# Correlation Matrix
transaction2 = as.data.frame(sapply(transaction2, as.numeric))
transaction2_cor = cor(transaction2)
transaction2_cor
corrplot(transaction2_cor, type="upper")
corrplot(transaction2_cor, method="number")
corrplot(transaction2_cor, 
         method = "color", 
         type="upper", 
         diag=F, 
         addCoef.col = "black")

# Fit PCA Model
transaction2_pca = prcomp(transaction2, center=T, scale = FALSE)
class(transaction2_pca)
summary(transaction2_pca)
is.list(transaction2_pca)
names(transaction2_pca)
transaction2_pca$rotation

transaction2_scale = scale(transaction2)
transaction2_pca2 = prcomp(transaction2_scale, center=FALSE, scale=FALSE)
transaction2_pca2$center

fviz_screeplot(transaction2_pca, addlabels=T, ylim =c(0,100))
get_eigenvalue(traxnsaction2_pca)
fviz_pca_contrib(transaction2_pca, choice="var")
skimr::skim(transaction2)


## EFA




=======
p<- left_join(left_join(left_join(customer, order, by = 'customer_id'),order_item,by = 'order_id'),order_product, by = 'product_id')
transaction <- left_join(p,order_payment, by = "order_id")
>>>>>>> 64d3cc9a9ebc26c4284c0d608486a8e5e123714a

## Text Analysis
View(order_reviews)
names(order_reviews)
order_reviews = na.omit(order_reviews)
order_reviews$review_comment_message = tolower(order_reviews$review_comment_message)
order_reviews1 <- order_reviews %>%
  unnest_tokens(token, review_comment_message) 
stopwords::stopwords_getsources() 
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

## Topic Model
order_reviews_corpus <- corpus(order_reviews$review_comment_message) 
summary(order_reviews_corpus, n = 20, showmeta = T) 
order_reviews_dfm <- dfm(order_reviews_corpus,remove_punct= T,remove = stopwords(), remove_numbers= T, remove_symbols= T) %>%
  dfm_trim(min_termfreq = 2, max_docfreq = .5,
           docfreq_type = "prop") 
order_reviews_dtm <- convert(order_reviews_dfm, 'topicmodels')
order_reviews_lda <- LDA(order_reviews_dtm, k = 2, control = list(seed = 729))
terms(order_reviews_lda, 10)




