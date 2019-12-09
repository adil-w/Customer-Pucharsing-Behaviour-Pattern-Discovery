# Customer Pucharsing Behaviour Pattern Discovery through Unsurpervised Machine Learning
The repository is created for a cluster/text analysis project focusing on Brazil e-commerce data to evaluate customer/supplier behaviour pattern and give a business recommendation.

# Project Overview
Our team used R as the major tool. PCA, EFA  are the main dimension reduction method and Clustering (k-means),  Text analysis (Sentiment analysis and Topic modelling)as well as the association rules are main algorithm  tools to looking pattern. 

# Analysis Description and Conclusion
The original dataset contains 113,098 transaction records. Each record includes buyers, seller, product and transaction information.  For example, buyer’s information includes buyer id, zip code. 
First, combine all the information in one data frame by primary and foreign keys and do some data cleaning.  We try association rules since it’s a intuitional decision on transaction data. However, we got too low support (0.001) to be meaningful in business. 
Then we shift to other method and  apply PCA and EFA to reduce the dimension. Unfortunately, we don’t have much numerical data and PCA/EFA results seems not ideal so we decided to use original data to avoid missing much information. 
Based on total sum of square plot, we build our clusters of and further dig some geometric patterns on consumer behaviors.  We found sao Paulo state people consumer most of the expensive products.  
Finally, we use text analysis to customers’ review. We found customers do care about delivery.  Therefore, we shift to focus on late delivery and found some geometric and categorial pattern on late delivery. Sentiment analysis also be applied to filter the most/least welcomed products. 
