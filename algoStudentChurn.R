#################################
###Load Data#####################
#################################

library(tidymodels)
library(janitor)
library(dplyr)
library(magrittr)
library(caret)

datOne <- read.csv("Binary_DF.csv") %>% 
  clean_names() %>% tibble()

attach(datOne)

print(datOne)

#Frequency table for showing number of instances that belong to each class and rep of whole df
y <- datOne$retention
cbind(freq=table(y), percentage=prop.table(table(y))*100)

#Check our correlations
correlations <- cor(datOne[,1:10])
correlations

#Vis the correlations 
corrplot(correlations, method="circle")

#Check data for NAs
missmap(datOne, col=c("black", "grey"), legend=FALSE)

colSums(is.na(datOne))

#Plot matrix
pairs(retention~ dalc, data=datOne, col=datOne$retention)

# box and whisker plots for each attribute by class value
datOne$x <- NULL

boxplot(datOne)

#Change to factor

datOne <- datOne %>%
  mutate_if(is.integer,as.factor)

levels(train$retention) <- c("No", "Yes")


#datOne <- datOne[sample(1:nrow(datOne), size = 500), ]

table(datOne$retention)

#Plot feature distribution 
# Pivot data to a long format
datOneLong <- datOne %>% 
  pivot_longer(!retention, names_to = "features", values_to = "values")


# Make a box plot for each predictor feature
correlations <- cor(datOne)
correlations

corrplot(correlations
         , method = 'color'
         , order = 'hclust' #cluster orders the variables so that the ones similar are placed next to each other
         , addCoef.col = 'black'
         , number.cex = .6 #lower values decrease the size of the cell
)

#Plot distribution
# Create separate boxplots for each attribute
par(mfrow=c(1,4))

ggplot(data= datOne, aes(x=retention, group = traveltime, fill = traveltime)) +
  geom_density()

ggplot(data= datOne, aes(x=retention, group = age, fill = age)) +
  geom_density() +
  facet_wrap(~ age)


par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(datOne[,i], main=names(datOne)[i])
}
#############################################
###############Pre process##################
############################################

preprocParam <- preProcess(datOne, method=c("scale"))
preprocParam <- preProcess(datOne, method=c("center"))
preprocParam <- preProcess(datOne, method = c("ica"), n.comp=15)
preprocParam <- preProcess(datOne, method=c("pca"))



datTransformed <- predict(preprocParam, datOne)

summary(datTransformed)

##########################################
#Train model
##########################################

#Partition data
set.seed(222)
ind <- sample(2, nrow(datOne), replace = TRUE, prob = c(0.80, 0.20))
train <- datOne [ind==1,]
testd <- datOne[ind==2,]

train$retention <- as.factor(train$retention)


#Fit model
naivModel <- NaiveBayes(as.factor(retention) ~ ., data = train)

# make predictions
predictions <- predict(naivModel, testd)





# summarize results in confusion matrix
results <- predict(naivModel, testd, type = "prob")

confusionMatrix(results$class, as.factor(testd$retention))

datOne$retention <- as.factor(datOne$retention)

#######################################
######Resample to Test Model Accuracy##
######################################



#Bootstrap for variance testing
trainControl <- trainControl(method="boot", number=25)
# evalaute the model
fit <- train(retention ~., data=datOne, trControl=trainControl, method = "nb")

fit

#K -fold cross validation
trainControl <- trainControl(method="cv", number=10)
# evaluate the model
fit <- train(retention ~., data=datOne, trControl=trainControl, method = "nb")
# display the results
print(fit)

#Repeated k-fold cross validation
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
# evaluate the model
fit <- train(retention ~., data=datOne, trControl=trainControl, method = "nb")
# display the results
print(fit)


#Leave One Out Cross Validation

trainControl <- trainControl(method="LOOCV")
# evaluate the model
fit <- train(retention ~., data=datOne, trControl=trainControl, method = "nb")
# display the results
print(fit)


########################################
############Model Metrics for Eval######
########################################

#Kappa
trainControl <- trainControl(method="cv", number=5)
set.seed(7)
fit <- train(retention ~., data=train, method="glm", metric="Accuracy",
             trControl=trainControl)
# display results
print(fit)

#R Means Squared Error and R2 for Regression Model
trainControl <- trainControl(method="cv", number=5)
set.seed(7)
fit <- train(retention ~., data=train, method="lm", metric="RMSE", trControl=trainControl)


#Calculate AUC
trainControl <- trainControl(method="cv", number=5, classProbs=TRUE,
                             summaryFunction=twoClassSummary)
set.seed(7)

fit <- train(retention ~ ., data = train, method="glm", metric="ROC",
             trControl=trainControl)

print(fit)

#Log loss
trainControl <- trainControl(method="cv", number=5, classProbs=TRUE,
                             summaryFunction=mnLogLoss)

set.seed(7)
fit <- train(retention ~., data=train, method="rpart", metric="logLoss", trControl=trainControl)

print(fit)

#Plot ROC
roc(retention, results$class)


rocPlot <- pROC::roc(testd$retention, results[,2])
control = negative
case = positive
controls < cases

plot(rocPlot)



roc(testd$retention, resultsRF$.pred_No)

resultsRF <- predict(rfModelFit, testd, type = "prob")

rocPlot <- pROC::roc(testd$retention, resultsRF$.pred_No)
control = negative
case = positive
controls < cases

plot(rocPlot, col = "red")

ggroc(rocPlot, col = "red")

ggroc(rocPlot, aes(linetype = "dashed"),
      legacy.axes = FALSE) 

ggroc(rocPlot, alpha = 0.5, colour = "red", linetype = "solid", size = 2)

#Use this ROC 
resultsRF %>%  roc_auc(as.factor(testd$retention),.pred_1)

results %>% roc_auc(as.factor(testd$retention), posterior.1)

resultsRF %>% 
  roc_curve(as.factor(testd$retention), .pred_1) %>% 
  autoplot()

results %>% 
  roc_curve(as.factor(testd$retention), posterior.1) %>% 
  autoplot()

#Visualize confusion Matrix
resultsRF %>% 
  conf_mat(testd$retention, .pred_Yes) %>% 
  autoplot()


#Plot relevant variables

varImp(fit)


plot(varImp(fit))






#View factor levels by frequency so that we can determine if we want to remove
datOne %>%
  ggplot(data = ., aes(x = g3)) +
  geom_bar(stat = "count", fill = "white", color = "black")




#Set Model

randForest <- rand_forest(trees = 1000, min_n = 5) %>% 
   set_engine("randomForest") %>% 
   set_mode("classification")

 rfModelFit <- randForest %>% fit(retention ~ ., train)


# multiRegr <- multinom_reg(penalty = 1) %>% 
#   set_engine("nnet") %>% 
#   set_mode("classification")

# Train a multinomial regression model without any reprocessing
# set.seed(2056)
# multiFit <- multiRegr%>% 
#   fit(retention ~ ., data = train)

# Print the model
glimpse(rfModelFit)

#Predictions
retentionResults <- testd %>% select(retention) %>% 
bind_cols(rfModelFit%>% 
            predict(new_data = testd))
# Print predictions
retentionResults %>% 
  slice_head(n = 5)



# Confusion matrix
retentionResults %>% 
  conf_mat(retention, .pred_class)


# Make predictions then bind them to the test set
results <- testd %>% select(retention) %>% 
  bind_cols(rfModelFit %>% predict(new_data = testd))

#if needed : results$retention <- as.factor(results$retention)

head(results)

# Calculate accuracy: proportion of data predicted correctly
accuracy(data = results, retention, estimate = .pred_claa)


#Set factors the same level 
# p1$.pred_class <- factor(p1$.pred_class, levels = levels(train$house_type))
# p2 <- factor(p2, levels = levels(testd$house_type))

#Test model accuracy

# Compute and bind probability results
results <- results %>% 
  bind_cols(rfModelFit %>% 
              predict(new_data = testd, type = "prob"))



results

# Visualize confusion matrix
results <- as.data.frame(results)

results %>% 
  conf_mat(retention, results$posterior.0) %>% 
  autoplot(type = "heatmap")


# results %>% 
#   roc_curve(class, score)
# 
# results %>% 
#   roc_auc(class, score) %>% 
#   autoplot()


#ROC
predicted <- predict(fit, testd, type="prob")


rocobj <- roc(testd$retention, predictor = as.numeric(predicted$.pred_class))
rocobj
control = 0
case = 1

plot(rocobj, col="red", lwd=3, main="ROC curve")



roc_qda <- roc(response = testd$retention, predictor = as.numeric(predicted$.pred_class))
plot(roc_qda, col="red", lwd=3, main="ROC curve QDA")
auc(roc_qda)

