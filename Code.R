library(readr)
library(caret)
library(parallel)
library(doParallel)
library(ggplot2)
library(printr)
set.seed(1984)    #random seed set for reproducibility

train_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

pml_training <- read.csv(train_url, na.strings = c("NA", "#DIV/0!", ""))
pml_testing <- read.csv(test_url, na.strings = c("NA", "#DIV/0!", ""))
# pml_testing = read_csv("pml-testing.csv", na = c("NA", "#DIV/0!", ""))
# pml_training = read_csv("pml-training.csv", na = c("NA", "#DIV/0!", ""))

nzv = nearZeroVar(pml_training)
pml_training = pml_training[, -nzv]

# Remove index column @col=1 (no title)
pml_training = pml_training[,-1]

# Remove Timestamp columns
pml_training2 = pml_training[, !grepl("timestamp", names(pml_training))]

# Check and remove columns with more than 95% <NA>.
na_rate = colMeans(is.na(pml_training2))
high_na_cols = which(na_rate >= .95)
pml_training3 = pml_training2[, -high_na_cols]

# Separate into train and validate sets
in_train = createDataPartition(pml_training3$classe, p=0.8, list=FALSE)
validate = pml_training3[-in_train, ]
train = pml_training3[in_train, ]

cv_control = trainControl(method="cv", number=5, allowParallel = TRUE)
set.seed(1984)
cluster = makePSOCKcluster(8) # 8 cores
registerDoParallel(cluster)

# Random Forest
# print(Sys.time())
model_rf = train(classe ~ ., data=train, method="rf", trControl=cv_control)
# print(Sys.time())
print(model_rf$results)
print(model_rf)

# gbm
# print(Sys.time())
# gbm (or caret) doesn't automatically handle NA'S, SO USE knnImpute
model_gbm = train(classe ~ ., data=train, method="gbm", trControl=cv_control, preProcess=c("knnImpute"))
# print(Sys.time())
print(model_gbm$results)
print(model_gbm)

# Clean up after parallel processing
stopCluster(cluster)
registerDoSEQ()

val_rf = predict(model_rf, validate)
confusionMatrix(factor(validate$classe), val_rf)
val_gbm = predict(model_gbm, validate)
confusionMatrix(factor(validate$classe), val_gbm)


test = pml_testing[, colnames(pml_testing) %in% colnames(train)]
predictions_rf = predict(model_rf, test)
predictions_gbm = predict(model_rf, test)
prob_id = as.character(pml_testing$problem_id)
predictions_table = t(data.frame(predictions_rf, predictions_gbm))
colnames(predictions_table) = prob_id
row.names(predictions_table) = c("Random Forest", "GBM")
predictions_table
