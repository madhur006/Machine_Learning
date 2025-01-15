library(pROC)
library(randomForest)

set.seed(420)

num.sample <- 100

obese <- weight <- sort(rnorm(num.sample, mean = 172, sd = 29))

obese <- ifelse(test = (runif(n=num.sample) < (rank(weight)/num.sample)),
       yes = 1, no =0)

obese

plot(obese)
## plot the data
plot(x=weight, y=obese)

## fit a logistic regression to the data...
glm.fit=glm(obese ~ weight, family=binomial)
lines(weight, glm.fit$fitted.values)
glm.fit$fitted.values


roc(obese, glm.fit$fitted.values, plot = TRUE)

par(pty = "s")
roc(obese, glm.fit$fitted.values, plot = TRUE)

roc(obese, glm.fit$fitted.values, plot = TRUE, legacy.axes = TRUE)

roc(obese, glm.fit$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab = "False Positive Percent", ylab = "True Positive Percent", col = "blue", 
    lwd = 4, print.auc = TRUE)

roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, 
    percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", 
    col="#377eb8", lwd=4, print.auc=TRUE, 
    print.auc.x=45, 
    partial.auc=c(100, 90),  # specificity from 100 to 90 but therefore 1-specificity : 0 to 10 
    auc.polygon = TRUE, auc.polygon.col = "#377eb822")


roc.info <- 
  roc(obese, glm.fit$fitted.values, plot = TRUE, legacy.axes = TRUE)

roc.df <- data.frame(
  tpp = roc.info$sensitivities*100, 
  fpp = (1-roc.info$specificities)*100,
  thresholds = roc.info$thresholds
)

head(roc.df)
tail(roc.df)

## thresholds between TPP 60% and 80%...
roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]

roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]


# random forest 

rf.model <- randomForest(factor(obese) ~ weight)
roc(obese, rf.model$votes[,1], plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#4daf4a", lwd=4, print.auc=TRUE)

roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)

# both the graphs together 

roc(obese, glm.fit$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=3, print.auc=TRUE)
plot.roc(obese, rf.model$votes[,1], percent = TRUE, col = "#4daf4a", lwd=4, print.auc=TRUE, 
         add= TRUE, print.auc.y = 40)
legend("bottomright", legend = c("Logistic regression", "Random forest"), col = c("#377eb8", "#4daf4a"), lwd = 4)



