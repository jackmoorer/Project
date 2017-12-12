require(ggplot2)
require(caret)
require(ROCR)
require(randomForest)


#Read cleaned data in
train = read.csv('../data/clean_train.csv', header = TRUE)
test = read.csv("../data/clean_test.csv", header = TRUE)

train_bag = train[, -ncol(train)]
test_bag = test[, -ncol(train)]



#Vectorize response column for later comparison
train_bag_response = train_bag$Over50k
test_bag_response = test_bag$Over50k



############################## 5-fold Cross Validation #################################
ntree = seq(100, 1000, by = 200)
CV_mat = matrix(ncol = 5, nrow = 5)
rownames(CV_mat) = ntree
colnames(CV_mat) = paste0("Fold", 1:5)
set.seed(200)
folds = createFolds(train_bag_response, k = 5)
for (j in 1:length(ntree)) {
  err_rate = numeric(5)
  for (i in 1:length(folds)) {
    fold = folds[[i]]
    train_set = train_bag[-fold, ]
    test_set = train_bag[fold, ]
    bag_fit = randomForest(Over50k ~., data = train_set, ntree = ntree[j],
                           mtry = ncol(train_bag) - 1, importance = TRUE)
    bag_pred = predict(bag_fit, test_set)
    err_rate[i] = 1 - mean(test_set$Over50k == bag_pred)
  }
  CV_mat[j,] = err_rate
}
CV_mat = cbind(CV_mat, rowMeans(CV_mat))
colnames(CV_mat) = c(paste0("Fold", 1:5), "Average")





#Cross-Validation On Train Set Results
sink("../output/training_results/cross_val_bagged_trees_plot.txt")
CV_mat
sink()


#Plot comparing average missclassification error vs. number of trees
pdf("../images/training_plots/cross_val_bagged_trees_plot.pdf")
plot(as.numeric(rownames(CV_mat)), CV_mat[,6], type = "p", pch = 19,
     xlab = "Number of Trees", ylab = "Misclassif. Error", 
     main = "Five-Fold Cross Validation: Number of Trees")
lines(as.numeric(rownames(CV_mat)), CV_mat[,6])
dev.off()


#Going to use 2 most accurate fits
sort(CV_mat[,6])[1:2]







#create vectors for comparison
AUC_vec = numeric(2)
AUC_vec1 = numeric(2)
train_misclass_vec = numeric(2)
test_misclass_vec = numeric(2)
test_sensitivity = numeric(2)
test_specificity = numeric(2)


#################################### ntree = 700 #######################################
set.seed(100)
bag_fit = randomForest(Over50k ~., data = train_bag, ntree = 700,
                       mtry = ncol(train_bag) - 1, importance = TRUE)

#Training Set Classification Error
bag_pred = predict(bag_fit, newdata = train_bag)
train_misclass_vec[1] = mean(bag_pred != train_bag_response)
train_probabilities = predict(bag_fit, newdata = train_bag, type = "prob")
prediction_train = prediction(train_probabilities[,2], train_bag_response)
performance_train = performance(prediction_train, measure = "tpr", x.measure = "fpr")
AUC_vec1[1] = performance(prediction_train, measure="auc")@y.values[[1]]


#Test Set Classification Error
bag_pred = predict(bag_fit, newdata = test_bag)
test_misclass_vec[1] = mean(bag_pred != test_bag_response)


confusion_mat = table(bag_pred, test_bag_response)
test_sensitivity[1] = confusion_mat[2, 2]/(confusion_mat[2, 2] + confusion_mat[1, 2])
test_specificity[1] = confusion_mat[1, 1]/(confusion_mat[1, 1] + confusion_mat[2, 1])


test_probabilities = predict(bag_fit, newdata = test_bag, type = "prob")
prediction_test = prediction(test_probabilities[,2], test_bag_response)
performance_test = performance(prediction_test, measure = "tpr", x.measure = "fpr")
AUC_vec[1] = performance(prediction_test, measure="auc")@y.values[[1]]




#################################### ntree = 900 #######################################
set.seed(100)
bag_fit = randomForest(Over50k ~., data = train_bag, ntree = 900,
                       mtry = ncol(train_bag) - 1, importance = TRUE)



#Training Set Classification Error
bag_pred = predict(bag_fit, newdata = train_bag)
train_misclass_vec[2] = mean(bag_pred != train_bag_response)
train_probabilities = predict(bag_fit, newdata = train_bag, type = "prob")
prediction_train = prediction(train_probabilities[,2], train_bag_response)
performance_train = performance(prediction_train, measure = "tpr", x.measure = "fpr")
AUC_vec1[2] = performance(prediction_train, measure="auc")@y.values[[1]]


#Test Set Classification Error
bag_pred = predict(bag_fit, newdata = test_bag)
test_misclass_vec[2] = mean(bag_pred != test_bag_response)

#Confusion Matrix
confusion_mat = table(bag_pred, test_bag_response)
test_sensitivity[2] = confusion_mat[2, 2]/(confusion_mat[2, 2] + confusion_mat[1, 2])
test_specificity[2] = confusion_mat[1, 1]/(confusion_mat[1, 1] + confusion_mat[2, 1])

#Find AUC values
test_probabilities = predict(bag_fit, newdata = test_bag, type = "prob")
prediction_test = prediction(test_probabilities[,2], test_bag_response)
performance_test = performance(prediction_test, measure = "tpr", x.measure = "fpr")
AUC_vec[2] = performance(prediction_test, measure="auc")@y.values[[1]]




############################# Bagged Tree Selection ###################################
comparison_mat = cbind(train_misclass_vec, test_misclass_vec, test_sensitivity, test_specificity, AUC_vec, AUC_vec1)
rownames(comparison_mat) = c(700, 900)
colnames(comparison_mat) = c("train misclass.", "test misclass.", "test sensitivity",
                             "test specificity", "test AUC", "train AUC")

sink("../output/test_results/bagged_comparison_matrix.txt")
comparison_mat
sink()



#According to our results, running a bagged tree with ntree = 900 provided the most 
#reasonable results and predictions. From the two most accurate fits via 5-fold cross 
#validation, the aforementioned model yielded that best train misclassification rate, 
#test misclassification rate, test sensitivity, test specificity, test AUC, and train 
#AUC. We considered choosing ntree = 700 in order to decrease our chances of overfitting; 
#however, this fit did not perform nearly as well as the other.


set.seed(100)
bag_fit = randomForest(Over50k ~., data = train_bag, ntree = 900,
                       mtry = ncol(train_bag) - 1, importance = TRUE)
bag_fit

bag_pred = predict(bag_fit, newdata = test_bag)

#Confusion Matrix
confusion_mat = table(bag_pred, test_bag_response)
sink("../output/test_results/final_bagged_confusion_mat.txt")
confusion_mat
sink()


#ROC Curve for Train Set
train_probabilities = predict(bag_fit, newdata = train_bag, type = "prob")
prediction_train = prediction(train_probabilities[,2], train_bag_response)
performance_train = performance(prediction_train, measure = "tpr", x.measure = "fpr")

pdf("../images/training_plots/final_bagged_train_ROC.pdf")
plot(performance_train, main = "ROC: Bagged Tree for Train, ntree = 900")
abline(a = 0, b = 1, lty = 2)
dev.off()


#ROC Curve for Test Set
test_probabilities = predict(bag_fit, newdata = test_bag, type = "prob")
prediction_test = prediction(test_probabilities[,2], test_bag_response)
performance_test = performance(prediction_test, measure = "tpr", x.measure = "fpr")

pdf("../images/test_plots/final_bagged_test_ROC.pdf")
plot(performance_test, main = "ROC: Bagged Tree for Test, ntree = 900")
abline(a = 0, b = 1, lty = 2)
dev.off()



#Variable Importance
imp_data = as.data.frame(importance(bag_fit))
sink("../output/test_results/final_bagged_var_importance.txt")
imp_data[order(imp_data$MeanDecreaseGini, decreasing = TRUE)[1:6],]
sink()


#Variable Importance Plot
pdf("../images/test_plots/final_bagged_var_plot.pdf")
varImpPlot(bag_fit)
dev.off()
