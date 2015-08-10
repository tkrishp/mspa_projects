setwd("~/github/mspa_projects/454_adv_modeling/ind_asg4")

library(psych)
library(tidyr)
library(dplyr)

wine = read.csv('wine.data')
wine$X = NULL

# dependent variable is categorical
wine$origin = factor(wine$origin)

# check for missing values
apply(is.na(wine), 2, any)

library(psych)
# get summary statistics group by spam
describeBy(wine, group=wine$origin)

library(tidyr)
library(dplyr)
# check % of origin
wine %>% group_by(origin) %>% summarise(n=n(), pct=(n/nrow(wine)))

library(lattice)
# scatter-plot matrix
pairs(origin ~ alcohol + malic_acid + ash + alcalinity_of_ash + magnesium + 
        total_phenols + flavanoids + nonflavanoid_phenols + 
        proanthocyanins + color_intensity + hue + diluted_wines + proline,
      data = wine)

library(PerformanceAnalytics)
# scatter-plot matrix with correlation & histogram
chart.Correlation(wine[,-c(1)])

# check for outliers
# use back-arrow in RStudio to iterate over all the plots
# boxplots for each combination of two factors 
# alcohol
wine.columns = colnames(wine)
for (i in 2 : ncol(wine)) { 
  plot = bwplot(wine[,c(i)] ~ wine[,c(1)],
           data = wine,
           ylab="origin", 
           xlab=wine.columns[i], 
           main=paste(wine.columns[i], "vs origin"), 
           layout=(c(1,1)))
  print(plot)
}

# heatplots to check for correlation
wine.origin.1 = wine %>% filter(origin == 1)
wine.origin.2 = wine %>% filter(origin == 2)
wine.origin.3 = wine %>% filter(origin == 3)

wine.cor.1 = cor(wine.origin.1[,-c(1)])
levelplot(wine.cor.1,
          main = "Correlation Matrix for Spam ",
          ylab = "Variable Name"
)

wine.cor.2 = cor(wine.origin.2[,-c(1)])
levelplot(wine.cor.2,
          main = "Correlation Matrix for Spam ",
          ylab = "Variable Name"
)

wine.cor.3 = cor(wine.origin.3[,-c(1)])
levelplot(wine.cor.3,
          main = "Correlation Matrix for Spam ",
          ylab = "Variable Name"
)

############################################################################
# Remove outliers
############################################################################
source("removeOutliers.R")

# remove outliers from class 1
wine.origin.1 = wine %>% filter(origin == 1)
tmp = removeOutliers(wine.origin.1[, -c(1)])
tmp$origin = 1
wine.origin.1 = tmp

# remove outliers from class 2
wine.origin.2 = wine %>% filter(origin == 2)
tmp = removeOutliers(wine.origin.2[, -c(1)])
tmp$origin = 2
wine.origin.2 = tmp

# remove outliers from class 3
wine.origin.3 = wine %>% filter(origin == 3)
tmp = removeOutliers(wine.origin.3[, -c(1)])
tmp$origin = 3
wine.origin.3 = tmp

wine = bind_rows(bind_rows(wine.origin.1, wine.origin.2), wine.origin.3)
wine$origin = factor(wine$origin)

# check for outliers now
# alcohol
plot = bwplot(wine$alcohol ~ factor(wine$origin),
              data = wine,
              ylab = "alcohol", 
              xlab = "origin",
              main=paste("alcohol", "vs origin"), 
              layout=(c(1,1)))
print(plot)

# malic_acid
plot = bwplot(wine$malic_acid ~ factor(wine$origin),
              data = wine,
              ylab = "malic_acid", 
              xlab = "origin",
              main=paste("malic_acid", "vs origin"), 
              layout=(c(1,1)))
print(plot)

# ash
plot = bwplot(wine$ash ~ factor(wine$origin),
              data = wine,
              ylab = "ash", 
              xlab = "origin",
              main=paste("ash", "vs origin"), 
              layout=(c(1,1)))
print(plot)

# alcalinity_of_ash
plot = bwplot(wine$alcalinity_of_ash ~ factor(wine$origin),
              data = wine,
              ylab = "alcolhol", 
              xlab = "origin",
              main=paste("alcalinity_of_ash", "vs origin"), 
              layout=(c(1,1)))
print(plot)

# magnesium
plot = bwplot(wine$magnesium ~ factor(wine$origin),
              data = wine,
              ylab = "magnesium", 
              xlab = "origin",
              main=paste("magnesium", "vs origin"), 
              layout=(c(1,1)))
print(plot)

# total_phenols
plot = bwplot(wine$total_phenols ~ factor(wine$origin),
              data = wine,
              ylab = "total_phenols", 
              xlab = "origin",
              main=paste("total_phenols", "vs origin"), 
              layout=(c(1,1)))
print(plot)

# flavanoids
plot = bwplot(wine$flavanoids ~ factor(wine$origin),
              data = wine,
              ylab = "flavanoids", 
              xlab = "origin",
              main=paste("flavanoids", "vs origin"), 
              layout=(c(1,1)))
print(plot)

# nonflavanoid_phenols
plot = bwplot(wine$nonflavanoid_phenols ~ factor(wine$origin),
              data = wine,
              ylab = "nonflavanoid_phenols", 
              xlab = "origin",
              main=paste("nonflavanoid_phenols", "vs origin"), 
              layout=(c(1,1)))
print(plot)

# proanthocyanins
plot = bwplot(wine$proanthocyanins ~ factor(wine$origin),
              data = wine,
              ylab = "proanthocyanins", 
              xlab = "origin",
              main=paste("proanthocyanins", "vs origin"), 
              layout=(c(1,1)))
print(plot)

# color_intensity
plot = bwplot(wine$color_intensity ~ factor(wine$origin),
              data = wine,
              ylab = "color_intensity", 
              xlab = "origin",
              main=paste("color_intensity", "vs origin"), 
              layout=(c(1,1)))
print(plot)

# hue
plot = bwplot(wine$hue ~ factor(wine$origin),
              data = wine,
              ylab = "hue", 
              xlab = "origin",
              main=paste("hue", "vs origin"), 
              layout=(c(1,1)))
print(plot)

# diluted_wines
plot = bwplot(wine$diluted_wines ~ factor(wine$origin),
              data = wine,
              ylab = "diluted_wines", 
              xlab = "origin",
              main=paste("diluted_wines", "vs origin"), 
              layout=(c(1,1)))
print(plot)

# proline
plot = bwplot(wine$proline ~ factor(wine$origin),
              data = wine,
              ylab = "proline", 
              xlab = "origin",
              main=paste("proline", "vs origin"), 
              layout=(c(1,1)))
print(plot)

# normalize data
source("normalize.R")
wine.origin = wine$origin
wine = normalize(subset(wine, select = -origin))
wine$origin = wine.origin

############################################################################
# Neural Network
############################################################################
library(neuralnet)
compute = neuralnet :: compute
wine.matrix = model.matrix(~ ., data = wine)
formula = {origin2 + origin3 ~ alcohol + malic_acid + ash + alcalinity_of_ash + 
             magnesium + total_phenols + flavanoids + nonflavanoid_phenols + 
             proanthocyanins + color_intensity + hue + diluted_wines + proline}
nn.fit = neuralnet(formula, 
                   data = wine.matrix,
                   hidden = c(1),
                   rep = 3,
                   act.fct = 'logistic',
                   linear.output = TRUE)
print (nn.fit)
nn.predict = compute(nn.fit, wine.matrix[,2:14])
nn.result = nn.predict$net.result
nn.predict.origin = matrix(,nrow(wine),4)
colnames(nn.predict.origin) = c("origin1", "origin2", "origin3", "predict_origin")
nn.predict.origin[,2] = round(nn.result[,1], 0)
nn.predict.origin[,3] = round(nn.result[,2], 0)
nn.predict.origin[,1] = ifelse(nn.predict.origin[,2] <= 0.33,
                               ifelse(nn.predict.origin[,3] <= 0.33, 1, 0),
                               0)
nn.predict.origin[,4] = ifelse(nn.predict.origin[,1] == 1, 1,
                               ifelse(nn.predict.origin[,2] == 1, 2,
                                      ifelse(nn.predict.origin[,3] == 1, 3, 0)
                               ))
wine$pred_origin_nn = nn.predict.origin[,4]
table(wine$origin, wine$pred_origin_nn)

############################################################################
# SVM
############################################################################
require('e1071') || install.packages('e1071')
formula = {origin ~ alcohol + malic_acid + ash + alcalinity_of_ash + 
             magnesium + total_phenols + flavanoids + nonflavanoid_phenols + 
             proanthocyanins + color_intensity + hue + diluted_wines + proline}
svm.fit = svm(formula,
              data = wine,
              kernel = 'sigmoid',
              gamma = 0.1,
              cross = 5,
              probability = as.logical('true')
           )
svm.predict = predict(svm.fit, 
                      newdata = subset(wine, select = -c(origin, pred_origin_nn))
              )
wine$pred_origin_svm = svm.predict
table(wine$origin, wine$pred_origin_svm)

############################################################################
# Random Forest
############################################################################
require('randomForest') || install.packages('randomForest')
formula = {origin ~ alcohol + malic_acid + ash + alcalinity_of_ash + 
             magnesium + total_phenols + flavanoids + nonflavanoid_phenols + 
             proanthocyanins + color_intensity + hue + diluted_wines + proline}
rf.fit = randomForest(formula = formula,
                      data = wine,
                      ntree = 500,
                      mtree = 3,
                      importance = TRUE
         )
summary(rf.fit)
plot(rf.fit)
varImpPlot(rf.fit)
rf.predict = predict(rf.fit,
                     newdata = subset(wine, select = -c(origin, pred_origin_nn, pred_origin_svm))
                     )
wine$pred_origin_rf = rf.predict
table(wine$origin, wine$pred_origin_rf)

############################################################################
# ROC and AUC
############################################################################
require('ROCR') || install.packages('ROCR')
prediction = ROCR :: prediction
wine.matrix = model.matrix(~ ., data = subset(wine, 
                                              select = -c(pred_origin_rf,
                                                          pred_origin_nn,
                                                          pred_origin_svm)))

wine.matrix = data.frame(wine.matrix)
wine.matrix$origin1 = ifelse(wine.matrix$origin2 == 0,
                             ifelse(wine.matrix$origin3 == 0, 1, 0), 0)
wine.matrix$X.Intercept. = NULL
wine.matrix$origin1 = factor(wine.matrix$origin1)
wine.matrix$origin2 = factor(wine.matrix$origin2)
wine.matrix$origin3 = factor(wine.matrix$origin3)

# random forest
# origin = 1
y_test = wine.matrix$origin1
formula = {origin1 ~ alcohol + malic_acid + ash + alcalinity_of_ash + 
             magnesium + total_phenols + flavanoids + nonflavanoid_phenols + 
             proanthocyanins + color_intensity + hue + diluted_wines + proline}
rf.fit.origin1 = randomForest(formula = formula,
                      data = wine.matrix,
                      ntree = 500,
                      mtree = 3,
                      importance = TRUE)
rf.pred.origin1 = predict(rf.fit.origin1, 
                    newdata = wine.matrix, 
                    type = "prob")
y_pred = prediction(rf.pred.origin1[,2], y_test)
rf.perf.origin1 = performance(y_pred, "tpr", "fpr")
rf.auc.origin1 = performance(y_pred, "auc")

# origin = 2
y_test = wine.matrix$origin2
formula = {origin2 ~ alcohol + malic_acid + ash + alcalinity_of_ash + 
             magnesium + total_phenols + flavanoids + nonflavanoid_phenols + 
             proanthocyanins + color_intensity + hue + diluted_wines + proline}
rf.fit.origin2 = randomForest(formula = formula,
                              data = wine.matrix,
                              ntree = 100,
                              mtree = 6,
                              importance = TRUE)
rf.pred.origin2 = predict(rf.fit.origin2, 
                          newdata = wine.matrix, 
                          type = "prob")
y_pred = prediction(rf.pred.origin2[,2], y_test)
rf.perf.origin2 = performance(y_pred, "tpr", "fpr")
rf.auc.origin2 = performance(y_pred, "auc")

# origin = 3
y_test = wine.matrix$origin3
formula = {origin3 ~ alcohol + malic_acid + ash + alcalinity_of_ash + 
             magnesium + total_phenols + flavanoids + nonflavanoid_phenols + 
             proanthocyanins + color_intensity + hue + diluted_wines + proline}
rf.fit.origin3 = randomForest(formula = formula,
                              data = wine.matrix,
                              ntree = 100,
                              mtree = 6,
                              importance = TRUE)
rf.pred.origin3 = predict(rf.fit.origin3, 
                          newdata = wine.matrix, 
                          type = "prob")
y_pred = prediction(rf.pred.origin3[,2], y_test)
rf.perf.origin3 = performance(y_pred, "tpr", "fpr")
rf.auc.origin3 = performance(y_pred, "auc")

# plot it
dev.new()
plot(rf.perf.origin1, col=1, main="Random Forest ROC curves using Wine Dataset")
# Draw a legend.
legend(0.6, 0.6, c('origin=1', 'origin=2', 'origin=3'), 1:4)
# add=TRUE draws on the existing chart
plot(rf.perf.origin2, col=2, add=TRUE)
plot(rf.perf.origin3, col=3, add=TRUE)

# predictions for svm
library(e1071)
svm.train.fit = svm(spam ~ ., data = spam.train, gamma=0.0625, probability = TRUE)
svm.pred = predict(svm.train.fit, newdata = spam.test, probability = TRUE, type = "prob")
y_pred = prediction(attr(svm.pred, "probabilities")[,2], y_test)
svm.perf = performance(y_pred, "tpr", "fpr")
svm.auc = performance(y_pred, "auc")

# plot it
dev.new()
plot(logistic.perf, col=1, main="ROC curves using Spam Dataset")
# Draw a legend.
legend(0.6, 0.6, c('logisitc', 'tree', 'random forest','svm'), 1:4)
# add=TRUE draws on the existing chart
plot(tree.perf, col=2, add=TRUE)
plot(rf.perf, col=3, add=TRUE)
plot(svm.perf, col=4, add=TRUE)


# svm
# origin = 1
y_test = wine.matrix$origin1
formula = {origin1 ~ alcohol + malic_acid + ash + alcalinity_of_ash + 
             magnesium + total_phenols + flavanoids + nonflavanoid_phenols + 
             proanthocyanins + color_intensity + hue + diluted_wines + proline}
rf.fit.origin1 = randomForest(formula = formula,
                              data = wine.matrix,
                              ntree = 100,
                              mtree = 6,
                              importance = TRUE)
rf.pred.origin1 = predict(rf.fit.origin1, 
                          newdata = wine.matrix, 
                          type = "prob")
y_pred = prediction(rf.pred.origin1[,2], y_test)
rf.perf.origin1 = performance(y_pred, "tpr", "fpr")
rf.auc.origin1 = performance(y_pred, "auc")

# origin = 2
y_test = wine.matrix$origin2
formula = {origin2 ~ alcohol + malic_acid + ash + alcalinity_of_ash + 
             magnesium + total_phenols + flavanoids + nonflavanoid_phenols + 
             proanthocyanins + color_intensity + hue + diluted_wines + proline}
rf.fit.origin2 = randomForest(formula = formula,
                              data = wine.matrix,
                              ntree = 100,
                              mtree = 6,
                              importance = TRUE)
rf.pred.origin2 = predict(rf.fit.origin2, 
                          newdata = wine.matrix, 
                          type = "prob")
y_pred = prediction(rf.pred.origin2[,2], y_test)
rf.perf.origin2 = performance(y_pred, "tpr", "fpr")
rf.auc.origin2 = performance(y_pred, "auc")

# origin = 3
y_test = wine.matrix$origin3
formula = {origin3 ~ alcohol + malic_acid + ash + alcalinity_of_ash + 
             magnesium + total_phenols + flavanoids + nonflavanoid_phenols + 
             proanthocyanins + color_intensity + hue + diluted_wines + proline}
rf.fit.origin3 = randomForest(formula = formula,
                              data = wine.matrix,
                              ntree = 100,
                              mtree = 6,
                              importance = TRUE)
rf.pred.origin3 = predict(rf.fit.origin3, 
                          newdata = wine.matrix, 
                          type = "prob")
y_pred = prediction(rf.pred.origin3[,2], y_test)
rf.perf.origin3 = performance(y_pred, "tpr", "fpr")
rf.auc.origin3 = performance(y_pred, "auc")

# plot it
dev.new()
plot(rf.perf.origin1, col=1, main="Random Forest ROC curves using Wine Dataset")
# Draw a legend.
legend(0.6, 0.6, c('origin=1', 'origin=2', 'origin=3'), 1:4)
# add=TRUE draws on the existing chart
plot(rf.perf.origin2, col=2, add=TRUE)
plot(rf.perf.origin3, col=3, add=TRUE)

# predictions for svm
library(e1071)
svm.train.fit = svm(spam ~ ., data = spam.train, gamma=0.0625, probability = TRUE)
svm.pred = predict(svm.train.fit, newdata = spam.test, probability = TRUE, type = "prob")
y_pred = prediction(attr(svm.pred, "probabilities")[,2], y_test)
svm.perf = performance(y_pred, "tpr", "fpr")
svm.auc = performance(y_pred, "auc")

# plot it
dev.new()
plot(logistic.perf, col=1, main="ROC curves using Spam Dataset")
# Draw a legend.
legend(0.6, 0.6, c('logisitc', 'tree', 'random forest','svm'), 1:4)
# add=TRUE draws on the existing chart
plot(tree.perf, col=2, add=TRUE)
plot(rf.perf, col=3, add=TRUE)
plot(svm.perf, col=4, add=TRUE)

