# import libraries
library(dplyr)
library(tidyr)

# load allstate data
load_data = function() {
  allstate_data = read.csv("data/allstate_data.csv")
  allstate_data_tbl = tbl_df(allstate_data)
  
  # derive hour from time
  allstate_data_tbl$hour = as.numeric(substr(allstate_data_tbl$time,1,2))
  
  # bin the hour
  allstate_data_tbl$hod = 
    ifelse(((allstate_data_tbl$hour >= 0) & (allstate_data_tbl$hour < 3)), "12am - 3am", 
    ifelse(((allstate_data_tbl$hour >= 3) & (allstate_data_tbl$hour < 6)), "3am - 6am",
    ifelse(((allstate_data_tbl$hour >= 6) & (allstate_data_tbl$hour < 9)), "6am - 9am",
    ifelse(((allstate_data_tbl$hour >= 9) & (allstate_data_tbl$hour < 12)), "9am - 12pm",
    ifelse(((allstate_data_tbl$hour >= 12) & (allstate_data_tbl$hour < 15)), "12pm - 3pm",
    ifelse(((allstate_data_tbl$hour >= 15) & (allstate_data_tbl$hour < 18)), "3pm - 6pm",
    ifelse(((allstate_data_tbl$hour >= 18) & (allstate_data_tbl$hour < 21)), "6pm - 9pm",
    ifelse(((allstate_data_tbl$hour >= 21)), "9pm - 12am",  "unknown"
    ))))))))
  
  allstate_data_tbl$productstr <- paste(allstate_data_tbl$A, allstate_data_tbl$B, allstate_data_tbl$C,allstate_data_tbl$D, allstate_data_tbl$E, allstate_data_tbl$F, allstate_data_tbl$G, sep="")
  allstate_data_tbl$productnum <- as.numeric(allstate_data_tbl$productstr)
  allstate_data_tbl$productfactor <- as.factor(allstate_data_tbl$productstr)
  
  # factorize variables
  allstate_data_tbl$shopping_pt = factor(allstate_data_tbl$shopping_pt)
  allstate_data_tbl$day = factor(allstate_data_tbl$day)
  allstate_data_tbl$state = factor(allstate_data_tbl$state)
  allstate_data_tbl$location = factor(allstate_data_tbl$location)
  allstate_data_tbl$group_size = factor(allstate_data_tbl$group_size)
  allstate_data_tbl$homeowner = factor(allstate_data_tbl$homeowner)
  allstate_data_tbl$risk_factor = factor(allstate_data_tbl$risk_factor)
  allstate_data_tbl$married_couple = factor(allstate_data_tbl$married_couple)
  allstate_data_tbl$C_previous = factor(allstate_data_tbl$C_previous)
  allstate_data_tbl$A = factor(allstate_data_tbl$A)
  allstate_data_tbl$B = factor(allstate_data_tbl$B)
  allstate_data_tbl$C = factor(allstate_data_tbl$C)
  allstate_data_tbl$D = factor(allstate_data_tbl$D)
  allstate_data_tbl$E = factor(allstate_data_tbl$E)
  allstate_data_tbl$F = factor(allstate_data_tbl$F)
  allstate_data_tbl$G = factor(allstate_data_tbl$G)
  allstate_data_tbl$hour = factor(allstate_data_tbl$hour)
  allstate_data_tbl$hod = factor(allstate_data_tbl$hod)
  
  return (allstate_data_tbl)
}