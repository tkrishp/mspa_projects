# import libraries
library(dplyr)
library(tidyr)

# Method for splitting original dataset into train and test datasets
# test data has truncated quote history
split_data = function(allstate_data_tbl) {
  allstate_data_tbl$tmp_shopping_pt = as.numeric(allstate_data_tbl$shopping_pt)
  customer_list = allstate_data_tbl %>% 
                  select(customer_ID, tmp_shopping_pt) %>% 
                  group_by(customer_ID) %>% 
                  summarise(quotes = max(tmp_shopping_pt)-1)
  
  for (i in 1:nrow(customer_list)) {
    if ((i %% 3) == 0) {
      customer_list[i, 'split'] = 'test'
    }
    else {
      customer_list[i, 'split'] = 'train'
    }
  }
  
  # create tain data frame
  customers_in_train = customer_list %>% filter(split=="train")
  allstate_train_tbl = inner_join(allstate_data_tbl, 
                                  customers_in_train %>% select(customer_ID, split), 
                                  by = "customer_ID")
  allstate_train_tbl$split = NULL
  allstate_train_tbl$tmp_shopping_pt = NULL
  
  # determine new quote history for each customer
  # new quote history should follow poisson distribution
  customers_in_test = customer_list %>% filter(split=="test")
  set.seed(124)
  customers_in_test$pois_quote = rpois(nrow(customers_in_test), lambda = 3)
  customers_in_test$pois_quote = ifelse(customers_in_test$pois_quote==1, 5, customers_in_test$pois_quote)
  customers_in_test$norm_quote = rnorm(nrow(customers_in_test), mean=6)
  customers_in_test$new_quote = ifelse((customers_in_test$pois_quote > 5), 
                              ceiling((customers_in_test$pois_quote + customers_in_test$norm_quote)/2),
                              customers_in_test$pois_quote)
  customers_in_test$new_quote = ifelse(customers_in_test$new_quote==0, customers_in_test$quotes, customers_in_test$new_quote)
  # create test data frame
  allstate_test_tbl = inner_join(allstate_data_tbl, 
                                 customers_in_test %>% select(customer_ID, split, new_quote),
                                 by = "customer_ID")
  
  # mark rows to be deleted as 'delete'
  allstate_test_tbl$delete = ifelse(allstate_test_tbl$record_type == 1, 'KEEP',
                                    ifelse(allstate_test_tbl$tmp_shopping_pt > allstate_test_tbl$new_quote, 'DELETE', 'KEEP'))
  allstate_test_tbl = allstate_test_tbl %>% filter(delete == 'KEEP')
  allstate_test_tbl$split = NULL
  allstate_test_tbl$delete = NULL
  allstate_test_tbl$pois_quote = NULL
  allstate_test_tbl$norm_quote = NULL
  allstate_test_tbl$new_quote = NULL
  allstate_test_tbl$tmp_shopping_pt = NULL
  return (list(allstate_train_tbl, allstate_test_tbl))
}

# transform train and test data for modeling purpose
transform_data = function(train, test) {
  quotes = train %>% filter(record_type==0)
  purchases = train %>% filter(record_type==1)
  # rename columns
  purchases = purchases %>% rename(purchased_A=A, 
                                   purchased_B=B, 
                                   purchased_C=C,
                                   purchased_D=D,
                                   purchased_E=E,
                                   purchased_F=F,
                                   purchased_G=G,
                                   purchased_cost=cost)
  
  train = inner_join(
    quotes,
    purchases %>% select(customer_ID, 
                         purchased_A, 
                         purchased_B, 
                         purchased_C, 
                         purchased_D, 
                         purchased_E, 
                         purchased_F, 
                         purchased_G,
                         purchased_cost),
    by="customer_ID")
  
  quotes = test %>% filter(record_type==0)
  purchases = test %>% filter(record_type==1)
  # rename columns
  purchases = purchases %>% rename(purchased_A=A, 
                                   purchased_B=B, 
                                   purchased_C=C,
                                   purchased_D=D,
                                   purchased_E=E,
                                   purchased_F=F,
                                   purchased_G=G,
                                   purchased_cost=cost)
  
  test = inner_join(
    quotes,
    purchases %>% select(customer_ID, 
                         purchased_A, 
                         purchased_B, 
                         purchased_C, 
                         purchased_D, 
                         purchased_E, 
                         purchased_F, 
                         purchased_G,
                         purchased_cost),
    by="customer_ID")
  
  # save training data to csv
  write.csv(train, 
            file = 'data/allstate_train.csv', 
            row.names=FALSE,
            quote = FALSE, 
            eol = "\n"
  )
  
  # save test data to csv
  write.csv(test, 
            file = 'data/allstate_test.csv', 
            row.names=FALSE,
            quote = FALSE, 
            eol = "\n"
  )
  return (list(train, test))
}