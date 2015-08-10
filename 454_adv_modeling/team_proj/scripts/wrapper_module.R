# setwd("~/Google Drive/MSPA/454 (Advanced Modeling Techniques)/team project/allstate purchase prediction")
setwd("~/github/mspa_projects/454_adv_modeling/team_proj")

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

# level out time for merge to work
levels(test$time) = levels(train$time)

# merge datasets for now
merge = union(train, test)
merge = merge %>% dplyr :: select(shopping_pt, day, state, location, group_size, homeowner, car_age, car_value, age_oldest, age_youngest, married_couple, A, B, C, D, E, F, G, cost, hour, hod, FixedRiskFactor, FixedCPrevious, FixedDurationPrevious, purchased_A, purchased_B, purchased_C, purchased_D, purchased_E, purchased_F, purchased_G, data)

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

# merge train & test for some data manipulations
# ensure both train and test have same levels for each factor
levels(test$shopping_pt) = levels(train$shopping_pt)
levels(test$day) = levels(train$day)
levels(test$time) = levels(train$time)
levels(test$state) = levels(train$state)
levels(test$location) = levels(train$location)
levels(test$group_size) = levels(train$group_size)
levels(test$homeowner) = levels(train$homeowner)
levels(test$car_value) = levels(train$car_value)
levels(test$married_couple) = levels(train$married_couple)
levels(test$FixedCPrevious) = levels(train$FixedCPrevious)
levels(test$FixedRiskFactor) = levels(train$FixedRiskFactor)
levels(test$A) = levels(train$A)
levels(test$B) = levels(train$B)
levels(test$C) = levels(train$C)
levels(test$D) = levels(train$D)
levels(test$E) = levels(train$E)
levels(test$F) = levels(train$F)
levels(test$G) = levels(train$G)
levels(test$hod) = levels(train$hod)
levels(test$purchased_A) = levels(train$purchased_A)
levels(test$purchased_B) = levels(train$purchased_B)
levels(test$purchased_C) = levels(train$purchased_C)
levels(test$purchased_D) = levels(train$purchased_D)
levels(test$purchased_E) = levels(train$purchased_E)
levels(test$purchased_F) = levels(train$purchased_F)
levels(test$purchased_G) = levels(train$purchased_G)

# normalize continous variables in 0-1 range
train$car_age_norm = (train$car_age - min(train$car_age))/(max(train$car_age) - min(train$car_age))
train$age_oldest_norm = (train$age_oldest - min(train$age_oldest))/(max(train$age_oldest) - min(train$age_oldest))
train$age_youngest_norm = (train$age_youngest - min(train$age_youngest))/(max(train$age_youngest) - min(train$age_youngest))
train$cost_norm = (train$cost - min(train$cost))/(max(train$cost) - min(train$cost))
train$FixedDurationPrevious_norm = (train$FixedDurationPrevious - min(train$FixedDurationPrevious))/(max(train$FixedDurationPrevious) - min(train$car_age))

test$car_age_norm = (test$car_age - min(test$car_age))/(max(test$car_age) - min(test$car_age))
test$age_oldest_norm = (test$age_oldest - min(test$age_oldest))/(max(test$age_oldest) - min(test$age_oldest))
test$age_youngest_norm = (test$age_youngest - min(test$age_youngest))/(max(test$age_youngest) - min(test$age_youngest))
test$cost_norm = (test$cost - min(test$cost))/(max(test$cost) - min(test$cost))
test$FixedDurationPrevious_norm = (test$FixedDurationPrevious - min(test$FixedDurationPrevious))/(max(test$FixedDurationPrevious) - min(test$car_age))

# neural network
library(neuralnet)
compute = neuralnet :: compute

# neural network doesnt create dummy variables for qualitative vars
# use model.matrix to do that
train.matrix = model.matrix(~ shopping_pt + day + state + group_size + homeowner + 
                              car_age + car_value + age_oldest + age_youngest + married_couple + 
                              FixedCPrevious + FixedRiskFactor + FixedDurationPrevious + 
                              A + B + C + D + E + F + G + cost + 
                              purchased_A + purchased_B + purchased_C + purchased_D + purchased_E + purchased_F + purchased_G,
                            data = train)

test.matrix = model.matrix(~ shopping_pt + day + state + group_size + homeowner + 
                             car_age + car_value + age_oldest + age_youngest + married_couple + 
                             FixedCPrevious + FixedRiskFactor + FixedDurationPrevious + 
                             A + B + C + D + E + F + G + cost + 
                             purchased_A + purchased_B + purchased_C + purchased_D + purchased_E + purchased_F + purchased_G,
                           data = test)

traindata = subset(train.matrix, select = -c(purchased_A2,
                                            purchased_B1,
                                            purchased_C2,purchased_C3,purchased_C4,
                                            purchased_D2,purchased_D3,
                                            purchased_E1,
                                            purchased_F1,purchased_F2,purchased_F3,
                                            purchased_G2,purchased_G3,purchased_G4))
testdata = subset(test.matrix, select = -c(purchased_A1,purchased_A2,
                                           purchased_B1,
                                           purchased_C2,purchased_C3,purchased_C4,
                                           purchased_D2,purchased_D3,
                                           purchased_E1,
                                           purchased_F1,purchased_F2,purchased_F3,
                                           purchased_G2,purchased_G3,purchased_G4))

formula = {
            purchased_A1 ~ shopping_pt2 + shopping_pt3 + shopping_pt4 + shopping_pt5 + shopping_pt6 + shopping_pt7 + shopping_pt8 + shopping_pt9 + shopping_pt10 + shopping_pt11 + shopping_pt12 + 
            day1 + day2 + day3 + day4 + day5 + day6 + 
            stateAR + stateCO + stateCT + stateDC + stateDE + stateFL + stateGA + stateIA + stateID + stateIN + stateKS + stateKY + stateMD + stateME + stateMO + stateMS + stateMT + stateND + stateNE + stateNH + stateNM + stateNV + stateNY + stateOH + stateOK + stateOR + statePA + stateRI + stateSD + stateTN + stateUT + stateWA + stateWI + stateWV + stateWY + 
            group_size2 + group_size3 + group_size4 + 
            homeowner1 + 
            car_age + 
            car_valuea + car_valueb + car_valuec + car_valued + car_valuee + car_valuef + car_valueg + car_valueh + car_valuei + 
            age_oldest + 
            age_youngest + 
            married_couple1 + 
            FixedCPrevious1 + FixedCPrevious2 + FixedCPrevious3 + FixedCPrevious4 + 
            FixedRiskFactor1 + FixedRiskFactor2 + FixedRiskFactor3 + FixedRiskFactor4 + 
            FixedDurationPrevious +
            A1 + A2 + 
            B1 + 
            C2 + C3 + C4 + 
            D2 + D3 + 
            E1 + 
            F1 + F2 + F3 + 
            G2 + G3 + G4 + 
            cost
          }

# fit a neural network
nn.fit.A = neuralnet(formula = formula, 
                   data = traindata,
                   hidden = c(3))

# predictions for A
nn.predict.A = compute(nn.fit.A, testdata)


library(nnet)

# define formula
feature.list = c("shopping_pt","day","state","group_size","homeowner","car_age_norm","car_value", "age_oldest_norm",
                 "age_youngest_norm", "married_couple", "A", "B", "C", "D", "E", "F", "G",
                 "cost_norm", "hod", "FixedRiskFactor", "FixedCPrevious", "FixedDurationPrevious_norm"
                 )
A.formula = as.formula(paste("purchased_A ~ ", paste(feature.list, collapse = "+")))

# multinomial log-linear neural net
nn.fit.A = multinom(formula = A.formula,
                    data = train[c(feature.list, "purchased_A")],
                    model = as.logical("true")
                    )
nn.predict.A = predict(nn.fit.A, newdata = test[c(feature.list, "purchased_A")], type = "class")
test$predict_A = nn.predict.A


