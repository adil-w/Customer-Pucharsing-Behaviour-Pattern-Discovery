# this file is a test file on cluster

##  choose numerical value
transac = transaction %>% select("payment_installments","payment_sequential",
                                 "product_weight_g","freight_value","payment_value","deliverd_difftime")
t_k = transac

### scale the data
j = scale(t_k)

### cluster plot 
k = kmeans(j, centers=4, iter.max=25, nstart=25)
fviz_cluster(k, data=j)

merge <- cbind(transac, cluster = k$cluster, major = transaction$major_state, state = transaction$customer_state)
con <- merge %>% group_by(cluster) %>% 
  summarise(weight=mean(product_weight_g),
            payment_value = mean(payment_value),
            timedif = mean(deliverd_difftime),
            payment_installments = mean(payment_installments),
            major_state = mean(major))

cluster1 <- merge %>% filter(cluster == 1)
cluster2 <- merge %>% filter(cluster == 2)            
cluster3 <- merge %>% filter(cluster == 3) 
cluster4 <- merge %>% filter(cluster == 4)

state1 <- as.data.frame(table(cluster1$state))
state2 <- as.data.frame(table(cluster2$state))
state3 <- as.data.frame(table(cluster3$state))
state4 <- as.data.frame(table(cluster4$state))

top5 <- state4[state4$Freq %in% tail(sort(state4$Freq),5),] 

ggplot(top5, aes(x=reorder(Var1,Freq), y=Freq, fill=Var1))+
  geom_bar(position = 'stack', stat = 'identity')+
  scale_fill_discrete(name = "City", labels = c('Minas Gerais','Paraná','Rio de Janeiro','Rio Grande do Sul','São Paulo'))+
  labs(title = "City Buy Most Valuable Products", 
       x = "City")+
  scale_fill_manual(values=c("#fec44f", "#56B1F7","#E69F00","#addd8e","#fc9272"))

