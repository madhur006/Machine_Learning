# Decision tree 
library(tree)
library(tidyverse)
library(rsample)
library(ISLR2)
attach(Carseats)

glimpse(Carseats)

# convert sales to categorical variable 
Carseats$high <- factor(ifelse(Sales <= 8, "No", "Yes"))

summary(Carseats)

# fit decision tree 

tree.carseats <- tree(high ~ . -Sales, data = Carseats)

summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty = 0)

# The most important indicator of Sales appears to be shelving location,
# since the first branch differentiates Good locations from Bad and Medium locations.

tree.carseats

# test error, spit data 
set.seed(2)

split <- initial_split(Carseats, prop = 0.7)  # 70% training, 30% testing
train <- training(split)
test <- testing(split)
train$high
test$high


tree.carseats <- tree(train$high ~ . - Sales, data = train)

tree.pred <- predict(tree.carseats, test,
                     type = "class")

table(tree.pred, test$high)

(53 + 36)/(53+36+22+9)


# Pruning 
set.seed(7)

cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass())








