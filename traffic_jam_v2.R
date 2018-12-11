

# install.packages(c('caret','skimr','RANN','randomForest','fastAdaboost','gbm','xgboost','caretEnsemble','C50','earth'))

#Importing required libraries

library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(caret)
library(dplyr)
library(randomForest)
library(caret)

# Reading the data

tr <- read.csv("C:/Users/dmunene/OneDrive - Dalberg Global Development Advisors/RESSOL/Personal/Data Analysis/Traffic Jam/train_revised.csv")
tt <- read.csv("C:/Users/dmunene/OneDrive - Dalberg Global Development Advisors/RESSOL/Personal/Data Analysis/Traffic Jam/test_questions.csv")

tr <- as_factor(tr)
tt <- as_factor(tt)

# Previewing the top data points

head(tr[,])
head(tt[,])

# Visualizing


ggplot(tr, aes(x = car_type)) +
  geom_bar()

ggplot(tr, aes(x = travel_date ,y = travel_from)) +
  geom_point(aes(color = car_type))

ggplot(tr, aes(x = travel_date ,y = travel_from)) +
  geom_hex(aes(color = car_type))

ggplot(tr, aes(x = travel_date ,y = travel_from)) +
  geom_point(aes(color = max_capacity))

ggplot(tr, aes(x = travel_time ,y = travel_from)) +
  geom_point(aes(color = car_type))

ggplot(tr, aes(x = travel_time ,y = travel_from)) +
  geom_point(aes(color = car_type, size = max_capacity))

ggplot(tr, aes(x = travel_time ,y = travel_from)) +
  geom_hex(aes(color = max_capacity))


# Processing travel date to day of week

t <- lubridate::dmy(tr$travel_date)
weekdays(t)

t1 <- lubridate::ymd(tt$travel_date)
weekdays(t1)

tr$travel_date <- t
tt$travel_date <- t1

# Processing travel time to mins
#train
tv <- tr$travel_time

tv1 <- hm(tv)

tv2 <- hour(tv1)*60 + minute(tv1)

tr$travel_time <- tv2

#test
tvv <- tt$travel_time

tvv1 <- hm(tvv)

tvv2 <- hour(tvv1)*60 + minute(tvv1)

tt$travel_time <- tvv2

# Grouping data on number of tickets and merging with main dataset

tr1 <- tr %>% group_by(ride_id,travel_date) %>% summarise(number_of_tickets=n())

tr2 <- merge(tr1,tr)



tt1 <- tt %>% group_by(ride_id,travel_date) %>% summarise(number_of_tickets=n())

tt2 <- merge(tt1,tt)

# make col of interest blank
tt2$number_of_tickets <- NA


# selecting variables for analysis

tr3 <- tr2 %>%
  select(travel_date,travel_time,travel_from,car_type,max_capacity,number_of_tickets)

# tt3 <- tt2 %>%
#   select(ride_id,travel_date,travel_time,travel_from,car_type,max_capacity,number_of_tickets)

anyNA(tr3)
# anyNA(tt3)

# Performing analysis


set.seed(1003)



#divide the data into train and test


# Using sample
# train <- sample(nrow(tr3), 0.7 *nrow(tr3), replace = F)
# md_train <- tr3[train,]
# md_test <- tr3[-train,]

# # Using createDataPartition
trainRowNumbers <- createDataPartition(tr3$number_of_tickets, p = 0.7, list = F)

train_dt <- tr3[trainRowNumbers,]
test_dt <- tr3[-trainRowNumbers,]
# 
# # ##
# test_dt2 <- tr4[-trainRowNumbers,]
# 
# test_dt2 <- data.frame(test_dt2)
# 
# train_dt2 <- tr4[trainRowNumbers,]
# 
# train_dt2 <- data.frame(train_dt2)

# Store X and Y for later use.
x <- train_dt[,1:5]
y <- train_dt$number_of_tickets

# One-Hot Encoding
# Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.

dummies_model <- dummyVars(number_of_tickets~., data = train_dt)

# Create the dummy variables using predict. The Y variable (number_of_tickets) will not be present in train_dt2.
train_dt <- predict(dummies_model,newdata = train_dt)

# # Convert to dataframe
train_dt <- data.frame(train_dt)

# # See the structure of the new dataset
str(train_dt)


# Controliing for the range to between 0 and 1
preProcess_range_model <- preProcess(train_dt, method = 'range')
train_dt <- predict(preProcess_range_model, newdata = train_dt)

# Append the Y variable
train_dt$number_of_tickets <- y

apply(train_dt[,1:10],2,FUN = function(x){c('min' = min(x),'max' = max(x))})


# How to visualize the importance of variables using featurePlot
# Box plots

# featurePlot(x = train_dt[,1:22],
#             y = train_dt$number_of_tickets,
#             plot = "box",
#             strip = strip.custom(par.strip.text = list(cex = .7)),
#             scales = list(x= list(relation = "free"),
#                           y= list(relation = "free")))


# How to do feature selection using recursive feature elimination (rfe)

set.seed(121)
options(warn = -1)

# subsets <- c(1:5,10,15,22)
# 
# ctrl <- rfeControl(functions = rfFuncs,
#                    method = "repeatedcv",
#                    repeats = 5,
#                    verbose = F)
# 
# 
# lmProfile <- rfe(x = train_dt[,1:22],y = train_dt$number_of_tickets,
#                  sizes = subsets,
#                  rfeControl = ctrl)
# 
# lmProfile


# Set the seed for reproducibility
set.seed(134)

# Train the model using randomForest and predict on the training data itself.
model_mars = train(number_of_tickets ~., data = train_dt, method = 'earth')
fitted <- predict(model_mars)

model_mars

plot(model_mars, main = "Model Accuracies with MARS")

varimp_mars <- varImp(model_mars)
plot(varimp_mars, main = "Variable Importance with MARS")

# Step 2: Create one-hot encodings (dummy variables) on test data
tr_test <- predict(dummies_model,test_dt)

tr_test <- data.frame(tr_test)

preProcess_range_model2 <- preProcess(tr_test, method = 'range')

tr_test <- predict(preProcess_range_model2,newdata =tr_test)

head(tr_test[,1:7])

# Predict on testData

predicts <- predict(model_mars,tr_test)

prr <- round(predicts)

head(prr)

prr <- data.frame(prr)


# Step 2: Create one-hot encodings (dummy variables) on test data
tt3 <- predict(dummies_model, tt2)

tt3 <- data.frame(tt3)

# Step 3: Transform the features to range between 0 and 1
tt4 <- predict(preProcess_range_model2, tt3)

# View
head(tt4[,1:7])

# Predict on testData
predicted <- predict(model_mars, tt4)

pr <- round(predicted)

head(pr)
pr <- data.frame(pr)

# out <- append(y,pr,after = length(y))
# out <- data.frame(out)
# 
# xy <- cbind(out,tr2)
# 
# xy2 <- xy %>%
#   group_by(ride_id,travel_date) %>% summarise(number_of_ticket = n())

# Train the model using SVM
model_svm <- train(number_of_tickets~., data = tr3, method = 'svmRadial')




# Train the model using xgboost Dart
set.seed(122)
model_xgbDART <- train(number_of_tickets ~., data = train_dt, method = 'xgbDART') 
model_xgbDART


predicted2 <- predict(model_xgbDART,train_dt)
predicted2 <- round(predicted2)
predicted2 <- data.frame(predicted2)



# Preparing test data
test_dt3 <- predict(dummies_model,test_dt)
test_dt3 <- data.frame(test_dt3)

test_dt4 <- predict(preProcess_range_model, test_dt3)

head(test_dt4[,1:7])


# Running prediction on test data
predicted <- predict(model_xgbDART, test_dt4)

predicted1 <- round(predicted)

# predicted1 <- data.frame(predicted1)

head(predicted1)

# Preparing ouput


out2 <- append(y,predicted1,after = length(y))
out2 <- data.frame(out2)

xy2 <- cbind(out2,tr2)

xy3 <- xy2 %>%
  group_by(ride_id,travel_date) %>% summarise(number_of_ticket = n())

xy4 <- xy3 %>%
  select(ride_id,number_of_ticket)




# grouping for output


df <- data.frame(tt2$ride_id,tt2$number_of_tickets,pr)
df_f <- df %>%
  select(ride_id = tt2.ride_id,number_of_ticket=y)

dt <- data.frame(test_dt2$ride_id,test_dt$number_of_tickets,predicted1)
dt_f <- dt %>%
  select(ride_id = test_dt2.ride_id,number_of_ticket=predicted1)

dt_fin <- merge(df_f,dt_f)
 
# Returns Root Mean Squared Error
number_of_tickets <- data.frame(md_test$number_of_tickets)
number_of_tickets <- as.numeric(number_of_tickets)

caret:: RMSE(predicted1,test_dt$number_of_tickets)

MAE(predicted1,test_dt$number_of_tickets)
MAE(xy4$number_of_ticket, tr1$number_of_tickets)

MAE(prr,test_dt$number_of_tickets)


write.csv(y,"C:/R/dmunene/trafficjam/NeshSubmission.csv", row.names = F)

write.csv(df_f,"C:/Users/dmunene/OneDrive - Dalberg Global Development Advisors/RESSOL/Personal/Data Analysis/Traffic Jam/Nesh_Submission.csv", row.names = F)

# Create a Random Forest model with default parameters

model1 <- randomForest(number_of_tickets ~ ., data = train_dt, importance = T)

model1

# Predict on testData

predictss <- predict(model1,tr_test)

prrr <- round(predictss)

head(prrr)


MAE(prrr,test_dt$number_of_tickets)

# running on main test dataset

prediction2 <- predict(model1,tt4)

p_forest <- round(prediction2)

head(p_forest)

p_forest <- data.frame(p_forest)

# embedding on main test data


df <- data.frame(tt2$ride_id,p_forest)
df_f <- df %>%
  select(ride_id = tt2.ride_id,number_of_tickets=p_forest)

write.csv(df_f,"C:/Users/dmunene/OneDrive - Dalberg Global Development Advisors/RESSOL/Personal/Data Analysis/Traffic Jam/Nesh_Submission3.csv", row.names = F)





# Fine tuning parameters of Random Forest model

model2 <- randomForest(number_of_tickets ~ ., data = train_dt, ntree = 500, mtry = 5, importance = T)

model2

model3 <- randomForest(number_of_tickets ~ ., data = train_dt, ntree = 500, mtry = 12, importance = T)

model3

model4 <- randomForest(number_of_tickets ~ ., data = train_dt, ntree = 500, mtry = 14, importance = T)

model4


model_rf4 <- train(number_of_tickets ~ ., data = train_dt,method = 'rf')



# Running traditional linear model

model_lm <- train(number_of_tickets ~ ., data = train_dt,method = 'lm')

model_lm

# setting up the trainControl()
# fitControl <- trainControl(
#   method = 'cv', # k-fold cross validation
#   number = 5,# number of folds
#   savePredictions = 'final', # saves predictions for optimal tuning parameter
#   classProbs = F, # should class probabilities be returned
#   summaryFunction = multiClassSummary # results summary function
# )

# hyper parameter tuning using tuneLength

# Step 1: Tune hyper parameters by setting tuneLength
set.seed(100)


model_rfest <- train(number_of_tickets~., data = train_dt, method = 'rf',tuneLength = 5, trControl = fitControl)




# Predict on testData

predict_frst6 <- predict(model6,tr_test)

p_forest6 <- round(predict_frst6)

head(p_forest6)


MAE(p_forest6,test_dt$number_of_tickets)


# running on main test dataset

prediction6 <- predict(model6,tt4)

p_forest6 <- round(prediction6)

head(p_forest6)

p_forest6 <- data.frame(p_forest6)

# embedding on main test data


df6 <- data.frame(tt2$ride_id,p_forest6)
df_f6 <- df6 %>%
  select(ride_id = tt2.ride_id,number_of_tickets=p_forest6)

write.csv(df_f6,"C:/Users/dmunene/OneDrive - Dalberg Global Development Advisors/RESSOL/Personal/Data Analysis/Traffic Jam/Nesh_Submission3b.csv", row.names = F)






# Predicting on train set

predtrain <- predict(model2, train_dt,type = "response")

predtrain2 <- predict(model_xgbDART, train_dt, type = "raw")

# Checking classification accuracy

table(predtrain, train_dt$number_of_tickets)
table(predtrain2, train_dt$number_of_tickets)


# To check important variables
importance(model1)
importance(model2)
importance(model3)



predtrain <- data.frame(predtrain)
predtrain_out <- round(predtrain)


predtrain2_out <- data.frame(predtrain2)

predtrain2_out2 <- round(predtrain2_out)
m <- c(predtrain2_out2,predtrain_out,train_dt$number_of_tickets)
m <- as.data.frame(m)

write.csv(m,"C:/R/dmunene/trafficjam/train_out.csv", sep = ",")
