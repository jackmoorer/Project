#Title: cleandata.R
#Discription: R script for cleaning data

#read in training data
train <- read.csv("../data/adult.csv", header = FALSE, strip.white=TRUE,
                  stringsAsFactors = FALSE)

#read in test data
test <- read.csv("../data/test.csv", header = FALSE, strip.white=TRUE,
                 stringsAsFactors = FALSE)

#create vector of column names
names <- c("age", "workclass", "fnlwgt", "education", "education_num",
           "marital_status", "occupation", "relationship", "race", "sex",
           "capital_gain", "capital_loss", "hours_per_week", "native_country",
           "Over50k")

#add column names to train
names(train) <- names

#add column names to test
names(test) <- names

#convert age to a numeric value instead of factor
train$age <- as.numeric(train$age)
test$age <- as.numeric(test$age)

#convert "?" to NA
train[train == "?"] <- NA
test[test == "?"] <- NA

#create numeric response variable for train
over50k_numeric_train <- rep(0, nrow(train))
over50k_numeric_train[train$Over50k == ">50K"] <- 1
train$over50k_numeric <- over50k_numeric_train

#create numeric response variable for test
over50k_numeric_test <- rep(0, nrow(test))
over50k_numeric_test[test$Over50k == ">50K."] <- 1
test$over50k_numeric <- over50k_numeric_test

#change train response column Over50k to yes or no factor
train_response <- train$over50k_numeric
train_response[train$over50k_numeric == 1] <- "Yes"
train_response[train$over50k_numeric == 0] <- "No"
train$Over50k <- as.factor(train_response)

#change test response column Over50k to yes or no factor
test_response <- test$over50k_numeric
test_response[test$over50k_numeric == 1] <- "Yes"
test_response[test$over50k_numeric == 0] <- "No"
test$Over50k <- as.factor(test_response)

#convert string columns to factor columns for train
train$workclass <- as.factor(train$workclass)
train$education <- as.factor(train$education)
train$marital_status <- as.factor(train$marital_status)
train$occupation <- as.factor(train$occupation)
train$relationship <- as.factor(train$relationship)
train$race <- as.factor(train$race)
train$sex <- as.factor(train$sex)
train$native_country <- as.factor(train$native_country)

#convert string columns to factor columns for test
test$workclass <- as.factor(test$workclass)
test$education <- as.factor(test$education)
test$marital_status <- as.factor(test$marital_status)
test$occupation <- as.factor(test$occupation)
test$relationship <- as.factor(test$relationship)
test$race <- as.factor(test$race)
test$sex <- as.factor(test$sex)
test$native_country <- as.factor(test$native_country)

#remove na values from train
train_clean <- na.omit(train)

#remove na values from test
test_clean <- na.omit(test)

#write clean train csv file
write.csv(train_clean, file = "../data/clean_test.csv")

#write clean train csv file
write.csv(test_clean, file = "../data/clean_train.csv")












