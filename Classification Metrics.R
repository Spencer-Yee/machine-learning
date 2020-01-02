#Producing Classification Metrics Code

#Function to calculate metrics based on a confusion matrix (2x2)
cm.metrics <- function(cm) {
  acc <- sum(diag(cm))/sum(cm)
  pre <- cm[1,1]/sum(cm[1,])
  sen <- cm[1,1]/sum(cm[,1])
  spe <- cm[2,2]/sum(cm[,2])
  
  ans <- c("Accuracy" = acc,
           "Precision" = pre,
           "Sensitivity" = sen,
           "Specificity" = spe)
  return(ans)
}

#Produces confusion matrix for binomial logistic model(s)
#Contingent on 14 models (m0-m13) but is adjustable
#Based on probability threshhold of 0.35 (adjustable based on scale of significant prediction and data)
M <- matrix(NA, nrow = 14, ncol = 4)
l <- list(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13)
i <- 1
for(f in l){
  p <- predict(f, type = "response")
  PC <- ifelse(p > 0.35, 1, 0)
  TC <- data$BAD
  cm <- table(factor(PC, levels = 1:0),
              factor(TC, levels = 1:0))
  M[i,] <- cm.metrics(cm)
  i <- i + 1
}
#Displays results
dimnames(M) <- list(paste("m", 0:13, sep = ""),
                    c("Accuracy", "Precision", "Sensitivity", "Specificity"))
round(M * 100, 2)
