options(stringsAsFactors = FALSE)

library(dplyr)
library(purrr)
library(cluster)
library(factoextra)
library(tidyverse)
library(skimr)

skim(transaction)

t_k = transaction %>% select_if(Negate(is.factor))
t_k1 = t_k %>% select(-customer_zip_code_prefix)
t_k1[is.na(t_k1)] <- 0
sapply(t_k1, as.numeric)
t_k1 = t_k1 %>% select(-order_id)

j = scale(t_k1)

k = kmeans(j, centers=3, iter.max=25, nstart=25)
fviz_cluster(k, data=j)

names(k)
k$cluster
k$centers
table(k$cluster)
k$size

table(k$cluster)
k$centers

k_wss = function(k) {
  km = kmeans(j, k, nstart=25, iter=25)
  kwss = km$tot.withinss
  return(kwss)
}

x = 1:15
wss_vals = map_dbl(x, k_wss)

plot(x, wss_vals, type="b", main = "Transactions Select K - WSS")

fviz_nbclust(j,
             kmeans,
             method="silhouette",
             k.max=20)
