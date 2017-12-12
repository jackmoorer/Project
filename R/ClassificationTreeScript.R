#Title: Classification Tree Script
#Discription: Run code for Classification Tree part of project

#load packages
library(ggplot2)
library(tree)
library(ROCR)

#read in clean data
train <- read.csv("../data/clean_train.csv", header = TRUE)

#remove numeric predictor
train_tree <- train[,-ncol(train)]

#intialize basic classification tree
classification_tree <- tree(Over50k ~., data = train_tree)

#cross validation based on prune.misclass
set.seed(100)
classification_tree_cv <- cv.tree(classification_tree, FUN = prune.misclass)

#report cross validation for prune.misclass
sink("../output/training_results/cv-prune-misclass-classification-tree.txt")
print(classification_tree_cv)
sink()

#prepare cv plots
Size <- classification_tree_cv$size
K <- classification_tree_cv$k
Dev <- classification_tree_cv$dev
Misclass <- data.frame(Size, K, Dev)

#report cv plot for size
pdf("../images/training_plots/cv-prine-misclass-size-vs-error.pdf")
ggplot(data = Misclass, aes(x = Size, y = Dev)) + geom_point() + geom_line() + ggtitle("Size of Tree vs Error for CV Misclass")
dev.off()

#report cv plot for cost complexicity
pdf("../images/training_plots/cv-prine-misclass-k-vs-error.pdf")
ggplot(data = Misclass, aes(x = K, y = Dev)) + geom_point() + geom_line() + ggtitle("Cost-Complexity vs Error for CV Misclass")
dev.off()

#use default method for cv
set.seed(200)
classification_tree_cv_default <- cv.tree(classification_tree, FUN = prune.tree)

#report cross validation for prune.misclass
sink("../output/training_results/cv-prune-tree-classification-tree.txt")
print(classification_tree_cv_default)
sink()


#prepare cv plots
Size <- classification_tree_cv_default$size
K <- classification_tree_cv_default$k
Dev <- classification_tree_cv_default$dev
default <- data.frame(Size, K, Dev)

#report cv plot for size
pdf("../images/training_plots/cv-prine-tree-size-vs-error.pdf")
ggplot(data = default, aes(x = Size, y = Dev)) + geom_point() + geom_line() + ggtitle("Size of Tree vs Error for CV Default Method")
dev.off()

#report cv plot for cost complexicity
pdf("../images/training_plots/cv-prine-tree-k-vs-error.pdf")
ggplot(data = default, aes(x = K, y = Dev)) + geom_point() + geom_line() + ggtitle("Cost-Complexity vs Error for Default Method")
dev.off()

#get hyper-parameter size
names <- classification_tree_cv_default$size
values <- classification_tree_cv_default$dev
names(values) <- names
size <- as.numeric(names(which.min(values)))

#build tree
set.seed(4)
prune_classification_tree <- prune.misclass(classification_tree, best = size)

#report results of tree
sink("../output/training_results/pruned-classification-tree.txt")
print(prune_classification_tree)
print(" ")
print(summary(prune_classification_tree))
sink()

#show classification tree plot
pdf("../images/training_plots/classification-tree-plot.pdf")
plot(prune_classification_tree)
text(prune_classification_tree, pretty = 0)
dev.off()

#build confusion matrix
real_train <- train_tree$Over50k
train_preds <- predict(prune_classification_tree, train_tree, type = "class")

#report confusion matrix
sink("../output/training_results/classification-tree-train-confusion-matrix.txt")
print(table(train_preds, real_train))
sink()

#get error rate
err_rate <- mean(train_preds != real_train)

#report error rate
sink("../output/training_results/train-error-rate-classification-tree.txt")
print("Train Error Rate for Classification Tree")
print(err_rate)
sink()

#prepare roc
train_probs <- predict(prune_classification_tree, train_tree)
train_prediction <- prediction(train_probs[,2], real_train)
train_performance <- performance(train_prediction, measure = "tpr", x.measure = "fpr")

pdf("../images/training_plots/train-ROC-classificaiotn-tree.pdf")
plot(train_performance, main = "Train ROC Curve for Classification Tree")
abline(a=0, b=1, lty=2)
dev.off()

#get auc
auc <- performance(train_prediction, measure="auc")@y.values[[1]]

#report auc
sink("../output/training_results/classification-tree-train-auc.txt")
print("Train AUC for Classifcation Tree")
print(auc)
sink()

#This part of the script deals with Test Performance

#read in data
test <- read.csv("../data/clean_test.csv", header = TRUE)

#prepare data
test_tree <- test[, -ncol(test)]
test_tree_preds <- test_tree[, -ncol(test_tree)]
real_test <- test_tree$Over50k

#get test error rate
ct_test_preds <- predict(prune_classification_tree, test_tree_preds, type = "class")
test_err_rate <- mean(ct_test_preds != real_test)

#report test error rate
sink("../output/test_results/classificaton-tree-test-error-rate.txt")
print("Classification tree test error rate")
print(test_err_rate)
sink()

#create test confusion matrix
confusionMatrix <- table(ct_test_preds, real_test)

#report test confusion matrix
sink("../output/test_results/classification-tree-test-confusion-matrix.txt")
print("Classification Tree Confusion Matrix")
print(confusionMatrix )
sink()

#get sensitivity and specificity
sensitivity <- confusionMatrix[2, 2]/(confusionMatrix[2, 2] + confusionMatrix[1, 2])
specificity <- confusionMatrix[1, 1]/(confusionMatrix[1, 1] + confusionMatrix[2, 1])


#report sensitivity and specificity
sink("../output/test_results/classification-tree-sensitivity-specificity.txt")
print("Classification Tree Sensitivity:")
print(sensitivity)
print("Classification Tree Specificity:")
print(specificity)
sink()

#prepare roc cruve
ct_test_probs <- predict(prune_classification_tree, test_tree_preds)
ct_test_prediction <- prediction(ct_test_probs[,2], real_test)
ct_test_performance <- performance(ct_test_prediction,  measure = "tpr", x.measure = "fpr")

#plot roc
pdf("../images/test_plots/classification-tree-test-roc-curve.pdf")
plot(ct_test_performance, main="Test ROC Classification Tree")
abline(a=0, b=1, lty=2)
dev.off()

#get test auc
test_auc <- performance(ct_test_prediction, measure="auc")@y.values[[1]]

#report test auc
sink("../output/classification-tree-test-auc.txt")
print("Classification Test AUC")
print(test_auc)
sink()

















