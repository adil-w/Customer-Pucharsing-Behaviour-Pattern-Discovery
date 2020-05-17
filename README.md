# Customer Pucharsing Behaviour Pattern Discovery through Unsupervised Machine Learning
The repository is created for a cluster/text analysis project focusing on Brazil's e-commerce data to evaluate customer/supplier behavior pattern and give a business recommendation.

# Project Overview
Our team used R as the major tool. PCA and EFA are the main dimensionality reduction methods. Clustering (k-means), Text analysis (Sentiment analysis and Topic modeling), and association rules are the main algorithm tools for pattern-detection. 

# Analysis Description and Conclusion
The original dataset contains 113,098 transaction records. Each record includes buyers, sellers, products, and transaction information. For example, the buyer’s information includes buyer id, zip code. 

First, combine all the information in one data frame by primary and foreign keys and do some data cleaning. We try association rules since it’s an intuitional decision on transaction data. However, we got too low support (0.001) to be meaningful in business. 

Then we shift to other methods and apply PCA and EFA to reduce the dimension. Unfortunately, we don’t have many numerical data and PCA/EFA results seem not ideal so we decided to use original data to avoid missing much information.

Based on the total sum of the square plot, we build our clusters and further dig some geometric patterns on consumer behaviors. We found Sao Paulo state people buy most of the expensive products. 

Finally, we use text analysis for customer reviews. We found customers primarily care about delivery. Therefore, we shift to focus on late delivery and found some geometric and categorial pattern on late delivery. Sentiment analysis also is applied to filter the most/least welcomed products. 
