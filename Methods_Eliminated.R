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
