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

#convert string columns to factor columns for train
train$workclass <- as.factor(train$workclass)
train$education <- as.factor(train$education)
train$marital_status <- as.factor(train$marital_status)
train$occupation <- as.factor(train$occupation)
train$relationship <- as.factor(train$relationship)
train$race <- as.factor(train$race)
train$sex <- as.factor(train$sex)


#convert string columns to factor columns for test
test$workclass <- as.factor(test$workclass)
test$education <- as.factor(test$education)
test$marital_status <- as.factor(test$marital_status)
test$occupation <- as.factor(test$occupation)
test$relationship <- as.factor(test$relationship)
test$race <- as.factor(test$race)
test$sex <- as.factor(test$sex)


#remove na values from train
train <- na.omit(train)

#remove na values from test
test <- na.omit(test)

#We decided to omit native country for several reasons. 
#One is that in general, when we kept native country, it was one of the least important variables
#The other is it was hard to deal with since tree could not accept a factor with more than 32 levels
#Also the test set had one country not in the training set
#Instead we are going to make a variable "US_Citizen" that indicates whether an individual is a US citizen or not
#We did this because the vast majority of people are US Citizens
library(ggplot2)
native_country <- data.frame(table(train$native_country))

#report results from the native country table
sink("../output/EDA_results/freq-table-train-data.txt")
table(train$native_country)
sink()

#report plot of frequencies showing how overwhelming US majority is
pdf("../images/EDA_plots/tain-data-country-freq-plot.pdf")
ggplot(data = native_country, aes(x = Var1, y = Freq)) + geom_bar(stat = "identity") + ggtitle("Frequency Plot of Native Countries for Training Data")
dev.off()

#create dumby variable for training data
us_citizen <- rep(0, nrow(train))
us_citizen[train$native_country == "United-States"] <- "Yes"
us_citizen[train$native_country != "United-States"] <- "No"

#add dumby variable
train$US_Citizen <- as.factor(us_citizen)

#create dumby varaible for test data
us_citizen_test <- rep(0, nrow(test))
us_citizen_test[test$native_country == "United-States"] <- "Yes"
us_citizen_test[test$native_country != "United-States"] <- "No"

#add dumby variable
test$US_Citizen <- as.factor(us_citizen_test)

#remove original native_countries
train <- train[, -14]
test <- test[, -14]

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

#lets look that the correlation of the varibles using hetcor() from the polycor package
library(polycor)

#report correlation
sink("../output/EDA_results/training-correlation.txt")
hetcor(train)
sink()

#reorder the data frames and remove education_num
train <- train[c("age", "workclass", "fnlwgt", "education",
           "marital_status", "occupation", "relationship", "race", "sex",
           "capital_gain", "capital_loss", "hours_per_week", "US_Citizen",
           "Over50k", "over50k_numeric")]

test <- test[c("age", "workclass", "fnlwgt", "education",
                 "marital_status", "occupation", "relationship", "race", "sex",
                 "capital_gain", "capital_loss", "hours_per_week", "US_Citizen",
                 "Over50k", "over50k_numeric")]

#write clean train csv file
write.csv(train, file = "../data/clean_test.csv", row.names = FALSE)

#write clean train csv file
write.csv(test, file = "../data/clean_train.csv", row.names = FALSE)














