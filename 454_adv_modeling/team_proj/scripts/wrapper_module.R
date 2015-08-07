setwd("~/Google Drive/MSPA/454 (Advanced Modeling Techniques)/team project/allstate purchase prediction")

initialize = function() {
  # load allstate data from csv into dataframe
  source("scripts/load_data.R")
  allstate_data_tbl = load_data()
  
  allstate_data_tbl$FixedRiskFactor = ifelse(is.na(allstate_data_tbl$risk_factor),0,allstate_data_tbl$risk_factor) #NAs to 0
  allstate_data_tbl$FixedCPrevious = ifelse(is.na(allstate_data_tbl$C_previous),0,allstate_data_tbl$C_previous) #NAs to 0
  allstate_data_tbl$FixedDurationPrevious = ifelse(is.na(allstate_data_tbl$duration_previous),6,allstate_data_tbl$duration_previous) 
  
  # source("scripts/draw_plots.R")
  # draw_plots(allstate_data_tbl)
  
  # split original dataset into train and test datasets
  # before running models, data is transformed such that
  # [A..G] from purchased records are coverted to dependent
  # variables on quotes
  source("scripts/method_with_multiple_returns.R")
  source("scripts/data_transformations.R")
  c(train, test) := split_data(allstate_data_tbl)
  c(train, test) := transform_data(train, test)
}

load_train_test = function() {
  train = read.csv("data/allstate_train.csv")
  test = read.csv("data/allstate_test.csv")
  
  # factorize variables
  
  return (list(train, test))
}

library(dplyr)
library(tidyr)
source("scripts/method_with_multiple_returns.R")
select = dplyr :: select

c(train, test) := load_train_test()
train$data = 'train'
test$data = 'test'

# merge train & test for some data manipulations
levels(test$time) = levels(train$time)
merge = union(train, test)
merge = merge %>% select(shopping_pt, day, state, location, group_size, homeowner, car_age, car_value, age_oldest, age_youngest, married_couple, A, B, C, D, E, F, G, cost, hour, hod, FixedRiskFactor, FixedCPrevious, FixedDurationPrevious, purchased_A, purchased_B, purchased_C, purchased_D, purchased_E, purchased_F, purchased_G, data)

# factorize variables
merge$shopping_pt = factor(merge$shopping_pt)
merge$day = factor(merge$day)
merge$location = factor(merge$location)
merge$group_size = factor(merge$group_size)
merge$homeowner = factor(merge$homeowner)
merge$married_couple = factor(merge$married_couple)
merge$A = factor(merge$A)
merge$B = factor(merge$B)
merge$C = factor(merge$C)
merge$D = factor(merge$D)
merge$E = factor(merge$E)
merge$F = factor(merge$F)
merge$G = factor(merge$G)
merge$FixedRiskFactor = factor(merge$FixedRiskFactor)
merge$FixedCPrevious = factor(merge$FixedCPrevious)
merge$purchased_A = factor(merge$purchased_A)
merge$purchased_B = factor(merge$purchased_B)
merge$purchased_C = factor(merge$purchased_C)
merge$purchased_D = factor(merge$purchased_D)
merge$purchased_E = factor(merge$purchased_E)
merge$purchased_F = factor(merge$purchased_F)
merge$purchased_G = factor(merge$purchased_G)

# separate train and test
train = merge %>% filter(data == 'train')
test = merge %>% filter(data == 'test')

# neural network
library(neuralnet)
select = neuralnet :: select
compute = neuralnet :: compute

# neural network doesnt create dummy variables for qualitative vars
# use model.matrix to do that
train.matrix = model.matrix(~ shopping_pt + day + state + group_size + homeowner + 
                              car_age + car_value + age_oldest + age_youngest + married_couple + 
                              A + B + C + D + E + F + G + 
                              cost + FixedRiskFactor + FixedCPrevious + FixedDurationPrevious + 
                              purchased_A + purchased_B + purchased_C + purchased_D + purchased_E + purchased_F + purchased_G,
                            data = train)
formula = {purchased_A1 + purchased_A2 ~ shopping_pt2 + shopping_pt3 + shopping_pt4 + shopping_pt5 + shopping_pt6 + shopping_pt7 + shopping_pt8 + shopping_pt9 + shopping_pt10 + shopping_pt11 + shopping_pt12 + 
             day1 + day2 + day3 + day4 + day5 + day6 + 
             stateAR + stateCO + stateCT + stateDC + stateDE + stateFL + stateGA + stateIA + stateID + stateIN + stateKS + stateKY + stateMD + stateME + stateMO + stateMS + stateMT + stateND + stateNE + stateNH + stateNM + stateNV + stateNY + stateOH + stateOK + stateOR + statePA + stateRI + stateSD + stateTN + stateUT + stateWA + stateWI + stateWV + stateWY + 
             group_size2 + group_size3 + group_size4 + 
             homeowner1 + 
             car_age + 
             car_valuea + car_valueb + car_valuec + car_valued + car_valuee + car_valuef + car_valueg + car_valueh + car_valuei + 
             age_oldest + 
             age_youngest + 
             married_couple1 + 
             A1 + A2 + 
             B1 + 
             C2 + C3 + C4 + 
             D2 + D3 + 
             E1 + 
             F1 + F2 + F3 + 
             G2 + G3 + G4 + 
             cost + 
             FixedRiskFactor1 + FixedRiskFactor2 + FixedRiskFactor3 + FixedRiskFactor4 + 
             FixedCPrevious1 + FixedCPrevious2 + FixedCPrevious3 + FixedCPrevious4 + 
             FixedDurationPrevious}


nn.fit.A = neuralnet(formula = formula, 
                   data = train.matrix,
                   hidden = c(3))


