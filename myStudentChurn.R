#################################
###Load Data#####################
#################################

library(tidymodels)
library(janitor)
library(dplyr)
library(magrittr)
library(caret)

datOne <- read.csv("Binary_DF.csv") %>% 
  clean_names()

attach(datOne)

datOne <- datOne[sample(1:nrow(datOne), size = 500), ]

table(datOne$retention)

#Plot feature distribution 
# Pivot data to a long format
datOneLong <- datOne %>% 
  pivot_longer(!retention, names_to = "features", values_to = "values")


# Make a box plot for each predictor feature
cor(retention, age)

cor_map_dat <- cor(select_if(datOne, is.integer))

cor_map_dat <- round(x = cor(cor_map_dat), digits = 2)

corrplot(cor_map_dat
         , method = 'color'
         , order = 'hclust' #cluster orders the variables so that the ones similar are placed next to each other
         , addCoef.col = 'black'
         , number.cex = .6 #lower values decrease the size of the cell
)


#Convert to factor
datOne %<>% mutate_if(is.integer,as.factor) 




set.seed(222)
ind <- sample(2, nrow(datOne), replace = TRUE, prob = c(0.80, 0.20))
train <- datOne [ind==1,]
testd <- datOne[ind==2,]


#Set Model

randForest <- rand_forest(trees = 1000, min_n = 5) %>% 
   set_engine("randomForest") %>% 
   set_mode("classification")
 rfModelFit <- randForest %>% fit(retention ~ sex_m, train)


# multiRegr <- multinom_reg(penalty = 1) %>% 
#   set_engine("nnet") %>% 
#   set_mode("classification")

# Train a multinomial regression model without any preprocessing
# set.seed(2056)
# multiFit <- multiRegr%>% 
#   fit(retention ~ ., data = train)

# Print the model
multiFit

#Predictions
retentionResults <- testd %>% select(retention) %>% 
bind_cols(rfModelFit%>% 
            predict(new_data = testd))
# Print predictions
retentionResults %>% 
  slice_head(n = 5)

retentionResults$retention <- as.factor(retentionResults$retention)

# Confusion matrix
retentionResults %>% 
  conf_mat(retention, .pred_class)


# Make predictions then bind them to the test set
results <- testd %>% select(retention) %>% 
  bind_cols(rfModelFit %>% predict(new_data = testd))

#if needed : results$retention <- as.factor(results$retention)

head(results)

# Calculate accuracy: proportion of data predicted correctly
accuracy(data = results, retention, estimate = .pred_class)


#Set factors the same level 
# p1$.pred_class <- factor(p1$.pred_class, levels = levels(train$house_type))
# p2 <- factor(p2, levels = levels(testd$house_type))

# Make a roc_chart
# Predict class probabilities and bind them to results
resultsRoc <- results %>% 
  bind_cols(rfModelFit %>% 
              predict(new_data = testd))

update_geom_defaults(geom = "tile", new = list(color = "black", alpha = 0.7))
# Visualize confusion matrix
results %>% 
  conf_mat(retention, .pred_class) %>% 
  autoplot(type = "heatmap")


