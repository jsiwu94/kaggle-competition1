
# To clear the console
rm(list=ls(all=T))

############################# STANDARD GBM MODEL #########################################

library(caret)
library(caTools)
library(sqldf)
library(gbm)
library(dismo)
library(DMwR)

data <- read.csv("Documents/data_final.csv",header=TRUE)
data$WnvPresent <- factor(data$WnvPresent)

x=nrow(data)
y=(.70)*x
y=round(y, digits=0)
sample_numbers <- sample(1:x, y, FALSE)

validation <- data[-sample_numbers,]
data <- data[sample_numbers,]

data$WnvPresent <- as.numeric(as.character(data$WnvPresent))
sum(data$WnvPresent)
length(data$WnvPresent)
sum(data$WnvPresent)/length(data$WnvPresent)

sum(validation$WnvPresent)
length(validation$WnvPresent)
sum(validation$WnvPresent)/length(validation$WnvPresent)

## Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification
balanced.data <- SMOTE(WnvPresent ~., data, perc.over = 500, k = 5, perc.under = 500)

as.data.frame(table(balanced.data$WnvPresent))
data <- balanced.data


data = data[,c(10:45,47:93)]
validation = validation[,c(10:45,47:93)]


labels=data['WnvPresent']
labels <- data.matrix(labels)
data = data[-grep('WnvPresent', colnames(data))]

labels_val=validation['WnvPresent']
validation = validation[-grep('WnvPresent', colnames(validation))]

data_1 <- data.matrix(data)
validation_1 <- data.matrix(validation)

xgb <- xgboost(data = data_1, 
               label = labels, 
               eta = 0.3,
               max_depth = 15, 
               nround=50, 
               subsample = 0.6,
               colsample_bytree = 0.5,
               lambda=1,
               alpha=0,
               seed = 1,
               eval_metric = "merror",
               objective = "multi:softmax",
               num_class = 12,
               nthread = 3
)


train_pred <- as.data.frame(cbind(labels, predict(xgb, data_1)))
#train_pred$output <- ifelse(train_pred[,2]>0.3,1,0)
train_pred$output <- train_pred$V2

test_pred <- as.data.frame(cbind(labels_val, predict(xgb, validation_1)))
#test_pred$output <- ifelse(test_pred[,2]>0.3,1,0)
test_pred$output <- test_pred[,2]

train_metric <- matrix(nrow = 2, ncol = 2)
val_metric <- matrix(nrow = 2, ncol = 2)
train_metric <- data.frame(train_metric)
val_metric <- data.frame(val_metric)

colnames(train_metric) <- c("actual-0", "actual-1")
rownames(train_metric) <- c("predicted-0", "predicted-1")
colnames(val_metric) <- c("actual-0", "actual-1")
rownames(val_metric) <- c("predicted-0", "predicted-1")

train_metric[1,1] <- sqldf("select count(V2) from train_pred where WnvPresent=0 and output=0")#*100/nrow(train_pred)
train_metric[2,1] <- sqldf("select count(V2) from train_pred where WnvPresent=0 and output=1")#*100/nrow(train_pred)
train_metric[1,2] <- sqldf("select count(V2) from train_pred where WnvPresent=1 and output=0")#*100/nrow(train_pred)
train_metric[2,2] <- sqldf("select count(V2) from train_pred where WnvPresent=1 and output=1")#*100/nrow(train_pred)


val_metric[1,1] <- sqldf("select count(WnvPresent) from test_pred where WnvPresent=0 and output=0")#*100/nrow(test_pred)
val_metric[2,1] <- sqldf("select count(WnvPresent) from test_pred where WnvPresent=0 and output=1")#*100/nrow(test_pred)
val_metric[1,2] <- sqldf("select count(WnvPresent) from test_pred where WnvPresent=1 and output=0")#*100/nrow(test_pred)
val_metric[2,2] <- sqldf("select count(WnvPresent) from test_pred where WnvPresent=1 and output=1")#*100/nrow(test_pred)
