options(stringsAsFactors = FALSE)

library(dplyr)
library(purrr)
library(cluster)
library(factoextra)
library(tidyverse)
library(skimr)

set.seed(123)

t_k = transaction %>% select_if(Negate(is.factor))
t_k1 = t_k %>% select(-customer_zip_code_prefix)
t_k1[is.na(t_k1)] <- 0
sapply(t_k1, as.numeric)
t_k1 = t_k1 %>% select(-order_id)

### scale the data
j = scale(t_k1)

### WSS analysis
k_wss = function(k) {
  km = kmeans(j, k, nstart=25, iter=25)
  kwss = km$tot.withinss
  return(kwss)
}

x = 1:15
wss_vals = map_dbl(x, k_wss)

plot(x, wss_values,
     type="b", pch = 19, frame = FALSE,
     main= "Transactions Select K - WSS",
     xlab= "Number of clusters K",
     ylab= "Total within-clusters sum of squares")

plot(x, wss_vals, type="b", main = "Transactions Select K - WSS")

### cluster plot 
k = kmeans(j, centers=2, iter.max=25, nstart=25)
fviz_cluster(k, data=j)

k2 = kmeans(j, centers=10, iter.max=25, nstart=25)
fviz_cluster(k2, data=j)

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
