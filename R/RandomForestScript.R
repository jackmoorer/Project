#Title: Random Forrest Script
#Discription: Run code for Random Forest part of project

#load packages
library(randomForest)
library(ROCR)

#read in clean data
train <- read.csv("../data/clean_train.csv", header = TRUE)

#remove numeric response column
train_forest <- train[, -ncol(train)]

#seperate predictors and response for cross validation
train_predictors <- train_forest[, -ncol(train_forest)]
train_response <- train_forest$Over50k

#set seed and run random forest cross validation with 10 folds
set.seed(100)
rf_cv <- rfcv(train_predictors, train_response, cv.fold = 5)

#get plot of cross validation error rate with number of variables used in random forest
pdf(file = "../training_plots/random-forest-cv-plot.pdf")
with(rf_cv, plot(n.var, error.cv, log="x", type="o", lwd=2, main = "CV Variables vs Error Rate", xlab = "Variables Used", ylab = "CV Error Rate"))
dev.off()

#get best number of variables to use
num_var <- as.numeric(names(which.min(rf_cv$error.cv)))
#report number of variables
sink("../training_results/random-forest-variables-used.txt")
print("Hyper-parameter for Random Forest: number of variables used")
print(num_var)
sink()

#model random forest with selected number of variables to use
set.seed(100)
random_forest <- randomForest(Over50k ~., data = train_forest, mtry = num_var,
                              importance = TRUE)

#report random forest result
sink("../training_results/random-forest-model.txt")
print(random_forest)
sink()

#report variable importance
sink("../training_results/random-forest-variable-importance.txt")
print(importance(random_forest))
sink()

#make variable importance plot
pdf("../training_plots/random-forest-variable-importance-plot.pdf")
varImpPlot(random_forest)
dev.off()

#get predictions of training data
rf_pred <- predict(random_forest, train_forest[, -ncol(train_forest)])
#find error rate
real_train <- train_forest$Over50k
err_rate <- mean(rf_pred != real_train)

#report error rate
sink("../training_results/random-forest-training-error-rate.txt")
print("Training error rate for Random Forest")
print(err_rate)
sink()

#get "yes" posterior props for ROC curve
probs <- as.vector(random_forest$votes[,2])
#get perdiction off of yes posterior probs
prediction <- prediction(probs, real_train)
#get performance
performance <- performance(prediction, measure = "tpr", x.measure = "fpr")

#plot ROC curve
pdf("../training_plots/random-forest-roc.pdf")
plot(performance, main="ROC Random Forest")
abline(a=0, b=1, lty = 2)
dev.off()

#get auc
auc <- performance(prediction, measure="auc")@y.values[[1]]

#report auc
sink("../training_results/random-forest-auc.txt")
print("AUC of training set for Random Forest")
print(auc)
sink()

#report training confusion matrix
sink("../training_results/random-forest-confusion-matrix.txt")
print(random_forest$confusion)
sink()

#This part of the script deals with the test performance of the Random Forest

#prepare test set
test_forest <- test[, -ncol(test)]

#seperate predictors and responses
test_forest_preds <- test_forest[, -ncol(test_forest)]
real_test <- test_forest$Over50k

#get test error rate
rf_test_preds <- predict(random_forest, test_forest_preds)
test_err_rate <- mean(rf_test_preds != real_test)

#report test error rate
sink("../test_results/random-forest-test-error-rate.txt")
print("Random Forest Test Error Rate")
print(test_err_rate)
sink()

#get confusion matrix
confusionMatrix <- table(rf_test_preds, real_test)

#report confusion matrix
sink("../test_results/random-forest-confusion-matirx.txt")
print("Random Forest Confusion Matrix")
print(confusionMatrix)
sink()

#get sensitivity
sensitivity <- confusionMatrix[2, 2]/(confusionMatrix[2, 2] + confusionMatrix[1, 2])

#get specificity
specificity <- confusionMatrix[1, 1]/(confusionMatrix[1, 1] + confusionMatrix[2, 1])

#report sensitivity and specificity
sink("../test_results/random-forest-sensitivity-specificity.txt")
print("Random Forest Sensitivity:")
print(sensitivity)
print()
print("Rnaomd Forest Specificity:")
print(specificity)
sink()

#prepare ROC plot
test_probs <- predict(random_forest, test_forest_preds, type = "prob")
test_prediction <- prediction(test_probs[,2], real_test)
test_performance <- performance(test_prediction,  measure = "tpr", x.measure = "fpr")

#plot test ROC
pdf("../test_plots/random-forest-test-ROC.pdf")
plot(test_performance, main="Test ROC Random Forest")
abline(a=0, b=1, lty=2)
dev.off()

#get auc of test
test_auc <- performance(test_prediction, measure="auc")@y.values[[1]]

#report auc
sink("../test_results/random-forest-test-auc.txt")
print("Random Forest Test AUC")
print(test_auc)
sink()

























