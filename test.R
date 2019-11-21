# this file is a test file on cluster

library(dummies)

###product vs city
### product vs seasonality

pro <- clean_transaction %>% select(customer_zip_code_prefix:customer_state,order_purchase_timestamp,
                                    product_category_name:product_width_cm, payment_type)
procity <- pro %>% select(customer_city, product_category_name)
dummy <- dummy.data.frame(procity)
saveRDS(dummy, "dummy.rds")

####toonhuge to load