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


datOne$house_type <- as.factor(datOne$house_type)
datOne$state <- as.factor(datOne$state)

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

rfModelFit <- randForest %>% fit(retention ~ sex_m + age + mjob_health, train)


#Confusion Matrix
p1 <- predict(rfModelFit, train)
confusionMatrix(p1$.pred_class, train$ retention)

p2 <- predict(rfModelFit, testd)
confusionMatrix(p2$.pred_class, testd$ retention)

#Set factors the same level 
# p1$.pred_class <- factor(p1$.pred_class, levels = levels(train$house_type))
# p2 <- factor(p2, levels = levels(testd$house_type))

#

