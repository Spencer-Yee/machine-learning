#K-fold Cross Validation for Model Strength
***ORIGINAL DATA IS NOT INCLUDED***

#Splits data into 10 folds (900 obs.) using random seed
set.seed(324632)
data$fold <- sample(c(rep(0, 90), rep(1, 90), rep(2, 90),
                      rep(3, 90), rep(4, 90), rep(5, 90),
                      rep(6, 90), rep(7, 90), rep(8, 90), rep(9, 90)),
                    nrow(data),
                    replace = FALSE)

#Produces cross-validated metrics
#Compares model performance (metrics) of cross-validated data vs. original dataset
#Seeking minimal difference, but not values of 0 (indicative of overfitting)
set.seed(324632)
F <- matrix(NA, nrow = 10, ncol = 5) # for storing our metrics for each fold
dimnames(F)[[2]] <- c("fold", "accuracy", "precision", "sensitivity", "specificity")
i <- 1
for(fld in 0:9){
  fit <- glm(BAD ~ agesq + ap.inc,
             data = data,
             subset = fold != fld, # do not use one fold
             family = binomial(link = "logit"))
  
  p <- predict(fit,
               newdata = subset(data, subset = fold == fld), # on the fold not used
               type = "response")
  PC <- ifelse(p > 0.35, 1, 0)
  TC <- data$BAD[data$fold == fld] # True condition on the fold predicted
  cm <- table(factor(PC, levels = 1:0),
              factor(TC, levels = 1:0))
  F[i,] <- c(fld, cm.metrics(cm))
  i <- i + 1
}
(fld.means <- apply(F, 2, mean)[2:5]) # calculate means for each column


whole.sample <- M[6,]
tbl <- rbind("Whole Sample" = whole.sample,
             "Cross-Validated" = fld.means,
             "Difference" = whole.sample - fld.means)
dimnames(tbl)[[2]] <- c("Accuracy", "Precision", "Sensitivity", "Specificity")
round(tbl,4)