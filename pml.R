rm(list = ls())
gc()

library(dplyr)

pml_train <- read.csv("pml-training.csv")

# remove descriptive features
pml_train <- pml_train %>%
        select(-c(X, user_name, raw_timestamp_part_1, raw_timestamp_part_2,
                  cvtd_timestamp, new_window, num_window))

pml_train$classe <- factor(pml_train$classe)

# remove features with >95% NAs or empty values
keep <- sapply(pml_train, function(x) mean(!is.na(x))) > .95
pml_train <- pml_train[, keep]
keep <- sapply(pml_train, function(x) mean(x != "")) > .95
pml_train <- pml_train[, keep]

library(caret)

#ensure reproducibility
set.seed(42)

# partition data
inTrain <- createDataPartition(pml_train$classe, p = .7, list = F)
trainDS <- pml_train[inTrain,]
testDS <- pml_train[-inTrain,]

#train model Random Forest using built-in cross validation control
ctrlRF <- trainControl(method = "cv", number = 3, verboseIter = F)
model <- train(classe ~., data = trainDS, method = "rf", trControl = ctrlRF)
model$finalModel

# predictions and accuracy measures
predictions <- predict(model, newdata = testDS)
conf <- confusionMatrix(predictions, testDS$classe)

# Test data
pml_test <- read.csv("pml-testing.csv")
predict(model, newdata = pml_test)
