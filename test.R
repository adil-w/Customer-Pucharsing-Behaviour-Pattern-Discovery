# this file is a test file on cluster

##  choose numerical value
transac = transaction %>% select("payment_installments","payment_sequential",
                                 "product_weight_g","freight_value","payment_value","deliverd_difftime") %>% 
  na.omit()
t_k = transac

##state/ big small city
##k = 27/2
### scale the data
j = scale(t_k)

### cluster plot 
k = kmeans(j, centers=4, iter.max=25, nstart=25)
fviz_cluster(k, data=j)

merge <- cbind(transac, cluster = k$cluster)
merge %>% group_by(cluster) %>% 
  summarise(weight=mean(product_weight_g),
            payment_value = mean(payment_value),
            timedif = mean(deliverd_difftime),
            payment_installments = mean(payment_installments))
     
               
####clustering 

trans = transaction %>% select( "product_weight_g","deliverd_difftime") %>% 
  na.omit()
t = transac
s = scale(t)

k_wss = function(k) {
  km = kmeans(s, k, nstart=25, iter=25)
  kwss = km$tot.withinss
  return(kwss)
}

x = 1:10
wss_vals = map_dbl(x, k_wss)

plot(x, wss_vals,
     type="b", pch = 19, frame = FALSE,
     main= "Transactions Select K - WSS",
     xlab= "Number of clusters K",
     ylab= "Total within-clusters sum of squares")

plot(x, wss_vals, type="b", main = "Transactions Select K - WSS")

k = kmeans(s, centers=2, iter.max=25, nstart=25)
fviz_cluster(k, data=s)
