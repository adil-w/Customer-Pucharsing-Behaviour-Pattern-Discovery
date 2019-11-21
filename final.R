
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
location <- unique(location)

#######
p<- left_join(left_join(left_join(customer, order, by = 'customer_id'),order_item,by = 'order_id'),order_product, by = 'product_id')
transaction <- left_join(p,order_payment, by = "order_id")
transaction2 <- left_join(transaction,location,
                          by = c("customer_zip_code_prefix"="geolocation_zip_code_prefix"))


clean_transaction <-na.omit(transaction)
clean_tra= clean_transaction %>% select(-customer_zip_code_prefix, -order_item_id,-customer_id, -customer_unique_id, -customer_city, -customer_city, 
                                      -customer_state,-order_id:-order_estimated_delivery_date, 
                                      -product_id:-shipping_limit_date, -product_category_name, -payment_type, -payment_installments)


## PCA
# Keep only numerica columns
# Clean missing values
##p = p %>% replace_na(list(order_item_id=0))

# Correlation Matrix
transaction2 = as.data.frame(sapply(clean_tra, as.numeric))
transaction2_cor = cor(transaction2)
transaction2_cor
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
get_eigenvalue(transaction2_pca)
fviz_pca_contrib(transaction2_pca, choice="var")



## EFA
# Data Assessment 
transaction2_cor
cortest.bartlett(transaction2_cor, nrow(transaction2))
KMO(transaction2_cor)


## Clustering

##  choose numerical value
t_k = transaction2 %>% select_if(Negate(is.factor))

### scale the data
j = scale(t_k)

### WSS analysis
k_wss = function(k) {
  km = kmeans(j, k, nstart=25, iter=25)
  kwss = km$tot.withinss
  return(kwss)
}

x = 1:15
wss_vals = map_dbl(x, k_wss)

plot(x, wss_vals,
     type="b", pch = 19, frame = FALSE,
     main= "Transactions Select K - WSS",
     xlab= "Number of clusters K",
     ylab= "Total within-clusters sum of squares")

plot(x, wss_vals, type="b", main = "Transactions Select K - WSS")

### cluster plot 
k = kmeans(j, centers=10, iter.max=25, nstart=25)
fviz_cluster(k, data=j)

# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(j, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(j))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

plot(silhouette(k$cluster, dist=dist(j)), col=1:3)

names(k)
k$cluster
k$centers
table(k$cluster)
k$size

table(k$cluster)
k$centers


fviz_nbclust(j,
             kmeans,
             method="silhouette",
             k.max=20)

########################################################################
t_dist = dist(j)
h1 = hclust(t_dist)
plot(h1)


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












