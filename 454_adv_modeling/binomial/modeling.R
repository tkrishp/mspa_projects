# import libraries
library(tidyr)
library(dplyr)

library(psych)
library(tree)
library(ROCR)

library(lattice)
library(ggplot2)
library(GGally)
library(RColorBrewer)

############################################################################
# function definitions
############################################################################
# reference: http://stackoverflow.com/questions/15271103/how-to-modify-this-correlation-matrix-plot
panel.cor <- function(x, y, digits=2, cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  test <- cor.test(x,y)
  Signif <- ifelse(round(test$p.value,3)<0.001,"p<0.001",paste("p=",round(test$p.value,3)))  
  text(0.5, 0.25, paste("r=",txt))
  text(.5, .75, Signif)
}

panel.smooth<-function (x, y, col = "blue", bg = NA, pch = 18, 
                        cex = 0.8, col.smooth = "red", span = 2/3, iter = 3, ...) 
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
          col = col.smooth, ...)
}

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

get.auc = function(y_pred) {
  # Recall-Precision curve             
  RP.perf = performance(y_pred, "prec", "rec");
  # plot (RP.perf);
  
  # ROC curve
  ROC.perf = performance(y_pred, "tpr", "fpr");
  # plot (ROC.perf);
  
  # ROC area under the curve
  auc.tmp = performance(y_pred,"auc");
  auc = as.numeric(auc.tmp@y.values)
  
  return (auc)
}

# Function: evaluation metrics
## True positives (TP) - Correctly idd as success
## True negatives (TN) - Correctly idd as failure
## False positives (FP) - success incorrectly idd as failure
## False negatives (FN) - failure incorrectly idd as success
## Precision - P = TP/(TP+FP) how many idd actually success/failure
## Recall - R = TP/(TP+FN) how many of the successes correctly idd
## F-score - F = (2 * P * R)/(P + R) harm mean of precision and recall
prf <- function(predAct){
  ## predAct is two col dataframe of pred,act
  preds = predAct[,1]
  trues = predAct[,2]
  xTab <- table(preds, trues)
  clss <- as.character(sort(unique(preds)))
  r <- matrix(NA, ncol = 7, nrow = 1, 
              dimnames = list(c(),c('Acc',
                                    paste("P",clss[1],sep='_'), 
                                    paste("R",clss[1],sep='_'), 
                                    paste("F",clss[1],sep='_'), 
                                    paste("P",clss[2],sep='_'), 
                                    paste("R",clss[2],sep='_'), 
                                    paste("F",clss[2],sep='_'))))
  r[1,1] <- sum(xTab[1,1],xTab[2,2])/sum(xTab) # Accuracy
  r[1,2] <- xTab[1,1]/sum(xTab[,1]) # Miss Precision
  r[1,3] <- xTab[1,1]/sum(xTab[1,]) # Miss Recall
  r[1,4] <- (2*r[1,2]*r[1,3])/sum(r[1,2],r[1,3]) # Miss F
  r[1,5] <- xTab[2,2]/sum(xTab[,2]) # Hit Precision
  r[1,6] <- xTab[2,2]/sum(xTab[2,]) # Hit Recall
  r[1,7] <- (2*r[1,5]*r[1,6])/sum(r[1,5],r[1,6]) # Hit F
  r
}

confusion.glm <- function(data, model) {
  prediction <- ifelse(predict(model, data, type='response') > 0.5, TRUE, FALSE)
  confusion  <- table(prediction, as.logical(model$y))
  confusion  <- cbind(confusion, c(1 - confusion[1,1]/(confusion[1,1]+confusion[2,1]), 1 - confusion[2,2]/(confusion[2,2]+confusion[1,2])))
  confusion  <- as.data.frame(confusion)
  names(confusion) <- c('FALSE', 'TRUE', 'class.error')
  confusion
}

############################################################################
# Data extraction
############################################################################

# load data
spambase = tbl_df(read.csv("spambase/spambase.data"))
spambase$spam = factor(spambase$spam)

# check for missing values
apply(is.na(spambase), 2, any)

# get summary statistics group by spam
describeBy(spambase, group=spambase$spam)

# check % of spam vs non-spam
spambase %>% group_by(spam) %>% summarise(n=n(), pct=(n/nrow(data)))

############################################################################
# Remove outliers
############################################################################
source("removeOutliers.R")
spambase = removeOutliers(spambase)

############################################################################
# plots
############################################################################
# correlation plot matrix
spam = spambase %>% filter(spam==1)
notspam = spambase %>% filter(spam==0)
cor.spam = cor(spam[,-c(58)])
levelplot(cor.spam,
          main = "Correlation Matrix for Spam ",
          ylab = "Variable Name"
          )

cor.notspam = cor(notspam[,-c(58)])
levelplot(cor.notspam,
          main = "Correlation Matrix Non-spam",
          ylab = "Variable Name"
)

# another mechanism with different colors
# rgb.palette <- colorRampPalette(c("blue", "yellow"), space = "rgb")
# levelplot(z, 
#           main="correlation matrix", 
#           xlab="", ylab="", 
#           col.regions=rgb.palette(120), 
#           cuts=100, 
#           at=seq(0,1,0.01)
# )


# boxplots for each combination of two factors 
# char_freq_dollar
bwplot(char_freq_dollar ~ spam,
       data = spambase,
       ylab="Spam", xlab="Char Frequency (Make)", 
       main="Char Freqency", 
       layout=(c(1,1))
)

# char_freq_!
bwplot(char_freq_excl ~ spam,
       data = spambase,
       ylab="Spam", xlab="Char Frequency (Make)", 
       main="Char Freqency", 
       layout=(c(1,1))
)

# word_freq_remove
bwplot(word_freq_remove ~ spam,
       data = spambase,
       ylab="Spam", xlab="Char Frequency (Make)", 
       main="Char Freqency", 
       layout=(c(1,1))
)

# word_freq_hp
bwplot(word_freq_hp ~ spam,
       data = spambase,
       ylab="Spam", xlab="Char Frequency (Make)", 
       main="Char Freqency", 
       layout=(c(1,1))
)

# word_freq_free
bwplot(word_freq_free ~ spam,
       data = spambase,
       ylab="Spam", xlab="Char Frequency (Make)", 
       main="Char Freqency", 
       layout=(c(1,1))
)

# word_freq_george
bwplot(word_freq_george ~ spam,
       data = spambase,
       ylab="Spam", xlab="Char Frequency (Make)", 
       main="Char Freqency", 
       layout=(c(1,1))
)

# word_freq_edu
bwplot(word_freq_edu ~ spam,
       data = spambase,
       ylab="Spam", xlab="Char Frequency (Make)", 
       main="Char Freqency", 
       layout=(c(1,1))
)

# capital_run_length_longest
bwplot(capital_run_length_longest ~ spam,
       data = spambase,
       ylab="Spam", xlab="Char Frequency (Make)", 
       main="Char Freqency", 
       layout=(c(1,1))
)

# capital_run_length_average
bwplot(capital_run_length_average ~ spam,
       data = spambase %>% filter(capital_run_length_average > 0),
       ylab="Spam", 
       xlab="Capital Letter Run Length", 
       main="capital_run_length_average", 
       layout=(c(1,1))
)

# capital_run_length_average
bwplot(word_freq_make ~ spam,
       data = spambase %>% filter(word_freq_make > 0),
       ylab="make", 
       xlab="Spam", 
       main="Word Freqency", 
       layout=(c(1,1))
)

# histograms
histogram(~ word_freq_address | factor(spam), data = spambase, 
          ref = TRUE,
          nint = 20,
          main = "Word Frequency",
          xlab = "make")

histogram(~ word_freq_make, 
          data = spambase, 
          ref = TRUE,
          nint = 20,
          main = "Word Frequency",
          xlab = "make")

histogram(~ word_freq_make, data = spambase %>% filter(word_freq_make > 0), 
          ref = TRUE,
          nint = 20,
          main = "Word Frequency",
          xlab = "make")

############################################################################
# Run this code to prevent the error:
# Error in plot.new() : figure margins too large
par(mfcol=c(12,12), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))

for(m in 1:141 ){
  x <- rnorm(100)
  hist(x[x != 0],30, xlab=NA, ylab=NA, main=paste('data: ',m), 
       cex.axis=0.5, font.main=1, cex.main=0.8)
}
############################################################################

tmp = spambase[,c("char_freq_dollar",
                  "char_freq_excl",
                  "word_freq_remove",
                  "word_freq_hp",
                  "word_freq_our",
                  "word_freq_free",
                  "word_freq_george",
                  "word_freq_edu",
                  "capital_run_length_longest",
                  "capital_run_length_average",
                  "spam")
               ]

# matrix plots
pairs(~char_freq_dollar + char_freq_excl + word_freq_remove + 
        word_freq_hp + word_freq_our + word_freq_free + 
        word_freq_george + word_freq_edu + capital_run_length_longest + 
        capital_run_length_average | spam,
      data = tmp,
      groups = spam,
      upper.panel=panel.cor,
      diag.panel=panel.hist,
      panel=panel.smooth)

splom(tmp[c(1:10)], groups=spam, data=tmp,
      panel=panel.superpose, 
      key=list(title="Three Cylinder Options",
               columns=3,
               points=list(pch=super.sym$pch[1:3],
                           col=super.sym$col[1:3]),
               text=list(c("4 Cylinder","6 Cylinder","8 Cylinder"))))

library(car)
scatterplot.matrix(~char_freq_dollar + char_freq_excl + word_freq_remove + 
                     word_freq_hp + word_freq_our + word_freq_free + 
                     word_freq_george + word_freq_edu + capital_run_length_longest + 
                     capital_run_length_average | spam, 
                   data=tmp,
                   main="Three Cylinder Options")

############################################################################
# Variable select
############################################################################
# full model
logistic.fullmodel = glm(spam ~ .,
                         data = spambase,
                         family = 'binomial')
summary(logistic.fullmodel)

# backwards selection
logistic.backwards = step(logistic.fullmodel, 
                          trace = 0 # suppres step-by-step output
                          )
summary(logistic.backwards)

# nothing model
logistic.nothing = glm(spam ~ 1,
                    data = spambase,
                    family=binomial)

# forward selection
logistic.forward = step(logistic.nothing,
                      scope=list(lower=formula(logistic.nothing),
                                 upper=formula(logistic.fullmodel)
                                 ), 
                      direction="forward",
                      trace = 0)
summary(logistic.forward)

# bothways
logistic.bothways = step(logistic.nothing, 
                    list(lower=formula(logistic.nothing),
                         upper=formula(logistic.fullmodel)
                         ), 
                    direction="both",
                    trace=0)
summary(logistic.bothways)

logistic.fit = glm(spam ~ word_freq_3d + word_freq_our + word_freq_over + word_freq_remove + 
                    word_freq_internet + word_freq_order + word_freq_will + word_freq_report + 
                    word_freq_addresses + word_freq_free + word_freq_business + 
                    word_freq_you + word_freq_credit + word_freq_your + word_freq_font + 
                    word_freq_000 + word_freq_money + word_freq_hp + word_freq_hpl + 
                    word_freq_george + word_freq_650 + word_freq_lab + word_freq_data + 
                    word_freq_85 + word_freq_technology + word_freq_1999 + word_freq_pm + 
                    word_freq_cs + word_freq_meeting + word_freq_original + word_freq_project + 
                    word_freq_re + word_freq_edu + word_freq_conference + char_freq_excl + 
                    char_freq_dollar + capital_run_length_average + capital_run_length_total,
                  data = spambase,
                  family = binomial)

# goodness of fit tests
# Hosmer-Lemeshow gof test
library(ResourceSelection)
hltest = hoslem.test(spambase$spam, fitted(logistic.backwards))
hltest$p.value

# diagnostic plots
library(boot)
glm.diag.plots(logistic.fit, glmdiag = glm.diag(logistic.fit))

############################################################################
# Modeling
############################################################################

# logistic regression (full model)
logistic.full.fit = glm(spam ~ ., 
                      data = spam.train, 
                      family = binomial)
summary(logistic.full.fit)

# build a basic tree model for EDA
library(tree)
tree.model = tree(spam ~ ., data = spambase)
plot(tree.model)
text(tree.model)

# support vector machine
library(e1071)
svm.fit = svm(spam ~ ., 
              data = spambase,
              gamma = 0.1)

# random forest
library(randomForest)
rf.fit = randomForest(spam ~ ., 
                      data = spambase,
                      importance = TRUE)
plot(rf.fit)
summary(rf.fit)
varImpPlot(rf.fit)

############################################################################
# Predictions
############################################################################
# split the data into train-test datasets
# 70% of the sample size
smp_size <- floor(0.70 * nrow(spambase))
set.seed(900100)
split.index <- sample(seq_len(nrow(spambase)), size = smp_size)
spam.train <- spambase[split.index, ]
spam.test <- spambase[-split.index, ]

X_train = spam.train[,-c(58)]
y_train = spam.train$spam

X_test = spam.test[,-c(58)]
y_test = spam.test$spam

############################################################################
# In-sample
############################################################################
# predictions for logistic
y = spambase$spam

logistic.fullmodel = glm(spam ~ .,
                               data = spambase,
                               family = 'binomial')
logistic.backwards = step(logistic.fullmodel, 
                                trace = 0 # suppres step-by-step output
)
logistic.pred = predict(logistic.backwards, newdata = spambase, type=c("response"))
y_pred = prediction(logistic.pred, y)
logistic.auc = get.auc(y_pred)
logistic.auc
# confusion matrix
confusion.glm(spambase, logistic.backwards)

# predictions for decision tree
library(rpart)
tree.fit = rpart(spam ~ ., data = spambase)
tree.pred = predict(tree.fit, newdata = spambase, type = "prob")
y_pred = prediction(tree.pred[,2], y)
tree.auc = performance(y_pred, "auc")
tree.auc
# confusion matrix
tree.pred.class = predict(tree.fit, newdata = spambase, type = "class")
table(tree.pred.class, spambase$spam)


# predictions for random forest
library(randomForest)
rf.fit = randomForest(spam ~ ., data = spambase, mtry = 6)
rf.pred = predict(rf.fit, newdata = spambase, type = "prob")
y_pred = prediction(rf.pred[,2], y)
performance(y_pred, "auc")
performance(y_pred, "f")
performance(y_pred, "tpr")
#confusion matrix
rf.fit$confusion


# predictions for svm
library(e1071)
svm.fit = svm(spam ~ ., data = spambase, gamma=0.0625, probability = TRUE)
svm.pred = predict(svm.fit, newdata = spambase[,-c(58)], probability = TRUE, type = "prob")
y_pred = prediction(attr(svm.pred, "probabilities")[,2], y)
performance(y_pred, "auc")
performance(y_pred, "lift")


############################################################################
# Out-of-sample
############################################################################
# predictions for logistic
logistic.train.fullmodel = glm(spam ~ .,
                            data = spam.train,
                            family = 'binomial')
summary(logistic.train.fullmodel)

logistic.train.backwards = step(logistic.train.fullmodel, 
                            trace = 0 # suppres step-by-step output
)
summary(logistic.train.backwards)

logistic.pred = predict(logistic.train.backwards, newdata = spam.test, type=c("response"))
y_pred = prediction(logistic.pred, y_test)
logistic.auc = get.auc(y_pred)
logistic.perf = performance(y_pred, "tpr", "fpr")

# predictions for decision tree
library(rpart)
tree.train.fit = rpart(spam ~ ., data = spam.train)
tree.pred = predict(tree.train.fit, newdata = spam.test, type = "prob")
y_pred = prediction(tree.pred[,2], y_test)
tree.perf = performance(y_pred, "tpr", "fpr")
tree.auc = performance(y_pred, "auc")

# predictions for random forest
library(randomForest)
rf.train.fit = randomForest(spam ~ ., data = spam.train, mtry = 6)
rf.pred = predict(rf.train.fit, newdata = spam.test, type = "prob")
y_pred = prediction(rf.pred[,2], y_test)
rf.perf = performance(y_pred, "tpr", "fpr")
rf.auc = performance(y_pred, "auc")

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

