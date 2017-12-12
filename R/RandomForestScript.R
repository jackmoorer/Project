#Title: Random Forrest Script
#Discription: Run code for Random Forest part of project

#load packages
library(randomForest)
library(ROCR)
library(caret)
library(stringr)

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
pdf(file = "../images/training_plots/random-forest-cv-plot.pdf")
with(rf_cv, plot(n.var, error.cv, log="x", type="o", lwd=2, main = "CV Variables vs Error Rate", xlab = "Variables Used", ylab = "CV Error Rate"))
dev.off()


#get best number of variables to use
num_var <- as.numeric(names(which.min(rf_cv$error.cv)))

#issue with above is it will give full number of variables, which is what bagging is. 
#We are going to try a different appraoch where number of trees and number of variables are tuned together
#create cv list
set.seed(200)
ntree <- c(50, 100, 500, 1000, 1500)
cv_list <- as.list(rep(0, 5))
names(cv_list) <- ntree
i <- 1
for (tree in ntree) {
  tune_param <- tuneRF(train_predictors, train_response, ntreeTry = tree, trace = FALSE, plot = FALSE)
  cv_list[[i]] <- tune_param
  i <- i + 1
}

#create cv data frame
matrix <- matrix(rep(0, 3*5*3), nrow = 15, ncol = 3)
index <- 0
for (i in 1:5) {
  cur_cv <- cv_list[i]
  errs <- cv_list[[i]][,2]
  for (j in 1:length(errs)){
    cur_err <- cv_list[[i]][,2][[j]]
    val <- str_sub(names(cv_list[[i]][,2][j]), 1, 1)
    val <- as.numeric(val)
    matrix[index + j, ] <- c(ntree[i], val, cur_err)
  }
  index <- index + j
}
cv_df <- data.frame(matrix)
names(cv_df) <- c("ntree", "num_var", "OOB")

#report cv data frame
sink("../output/training_results/random-forest-hyper-parameter-results.txt")
cv_df
sink()

#add colummn for plotting
cv_df$number_of_trees <- factor(cv_df$ntree)

#plot param tuning
pdf("../images/training_plots/random-forest-hyper-parameter-plot.pdf")
ggplot(data = cv_df, aes(x = num_var, y = OOB, color = number_of_trees)) + geom_line() + ggtitle("Random Forest Hyper Parameter Tuning")
dev.off()

#get hyper parameters
oobs <- cv_df$OOB
row_num <- as.numeric(which.min(oobs))
num_tree <- cv_df[row_num, 1]
num_var <- cv_df[row_num, 2]

#report hyper parameters
sink("../output/training_results/random-forest-best-hyper-parameters.txt")
print("Number of Trees")
print(num_tree)
print("")
print("Number of Variables in Random Selection")
print(num_var)


#model random forest with selected number of variables to use
set.seed(100)
random_forest <- randomForest(Over50k ~., data = train_forest,
                              ntree = num_tree, mtry = num_var,
                              importance = TRUE)

#report random forest result
sink("../output/training_results/random-forest-model.txt")
print(random_forest)
sink()

#report variable importance
sink("../output/training_results/random-forest-variable-importance.txt")
print(importance(random_forest))
sink()

#make variable importance plot
pdf("../images/training_plots/random-forest-variable-importance-plot.pdf")
varImpPlot(random_forest)
dev.off()

#get predictions of training data
rf_pred <- predict(random_forest, train_forest[, -ncol(train_forest)])
#find error rate
real_train <- train_forest$Over50k
err_rate <- mean(rf_pred != real_train)

#report error rate
sink("../output/training_results/random-forest-training-error-rate.txt")
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
pdf("../images/training_plots/random-forest-roc.pdf")
plot(performance, main="ROC Random Forest")
abline(a=0, b=1, lty = 2)
dev.off()

#get auc
auc <- performance(prediction, measure="auc")@y.values[[1]]

#report auc
sink("../output/training_results/random-forest-auc.txt")
print("AUC of training set for Random Forest")
print(auc)
sink()

#report training confusion matrix
sink("../output/training_results/random-forest-confusion-matrix.txt")
print(random_forest$confusion)
sink()

#This part of the script deals with the test performance of the Random Forest

#read in data
test <- read.csv("../data/clean_test.csv", header = TRUE)

#prepare test set
test_forest <- test[, -ncol(test)]

#seperate predictors and responses
test_forest_preds <- test_forest[, -ncol(test_forest)]
real_test <- test_forest$Over50k

#get test error rate
rf_test_preds <- predict(random_forest, test_forest_preds)
test_err_rate <- mean(rf_test_preds != real_test)

#report test error rate
sink("../output/test_results/random-forest-test-error-rate.txt")
print("Random Forest Test Error Rate")
print(test_err_rate)
sink()

#get confusion matrix
confusionMatrix <- table(rf_test_preds, real_test)

#report confusion matrix
sink("../output/test_results/random-forest-confusion-matirx.txt")
print("Random Forest Confusion Matrix")
print(confusionMatrix)
sink()

#get sensitivity
sensitivity <- confusionMatrix[2, 2]/(confusionMatrix[2, 2] + confusionMatrix[1, 2])

#get specificity
specificity <- confusionMatrix[1, 1]/(confusionMatrix[1, 1] + confusionMatrix[2, 1])

#report sensitivity and specificity
sink("../output/test_results/random-forest-sensitivity-specificity.txt")
print("Random Forest Sensitivity:")
print(sensitivity)
print("")
print("Rnaomd Forest Specificity:")
print(specificity)
sink()

#prepare ROC plot
test_probs <- predict(random_forest, test_forest_preds, type = "prob")
test_prediction <- prediction(test_probs[,2], real_test)
test_performance <- performance(test_prediction,  measure = "tpr", x.measure = "fpr")

#plot test ROC
pdf("../images/test_plots/random-forest-test-ROC.pdf")
plot(test_performance, main="Test ROC Random Forest")
abline(a=0, b=1, lty=2)
dev.off()

#get auc of test
test_auc <- performance(test_prediction, measure="auc")@y.values[[1]]

#report auc
sink("../output/test_results/random-forest-test-auc.txt")
print("Random Forest Test AUC")
print(test_auc)
sink()

























