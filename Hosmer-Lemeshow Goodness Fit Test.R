#Hosmer-Lemeshow Test of Goodness Fit
***ORIGINAL DATA IS NOT INCLUDED*** 

#HL Test function calculating/comparing difference in predicted probabilities to actual observed values
#Splits data into 10 groups based on ascending predicition probabilities
#Outputs HL test stat and p-value
HL <- function(a, e, g = 10) {
  y <- a
  yhat <- e
  qq <- quantile(yhat, probs = seq(0, 1, 1/g))
  cutyhat <- cut(yhat, breaks = qq, include.lowest = TRUE)
  observed <- xtabs(cbind(y0 = 1 - y, y1 = y) ~ cutyhat)
  expected <- xtabs(cbind(yhat0 = 1 - yhat, yhat1 = yhat) ~ cutyhat)
  C.hat <- sum((observed - expected)^2/expected)
  p.val <- 1 - pchisq(C.hat, g - 2)
  ans <- c("HL Stat." = C.hat,
           "P-Value" = p.val)
  return(ans)
}

#Displays test results of 14 models (m0-m13)
#Lower test stat = good; high p-value = good

rbind("m0" = HL(db$BAD, predict(m0, type = "response")),
      "m1" = HL(db$BAD, predict(m1, type = "response")),
      "m2" = HL(db$BAD, predict(m2, type = "response")),
      "m3" = HL(db$BAD, predict(m3, type = "response")),
      "m4" = HL(db$BAD, predict(m4, type = "response")),
      "m5" = HL(db$BAD, predict(m5, type = "response")),
      "m6" = HL(db$BAD, predict(m6, type = "response")),
      "m7" = HL(db$BAD, predict(m7, type = "response")),
      "m8" = HL(db$BAD, predict(m8, type = "response")),
      "m9" = HL(db$BAD, predict(m9, type = "response")),
      "m10" = HL(db$BAD, predict(m10, type = "response")),
      "m11" = HL(db$BAD, predict(m11, type = "response")),
      "m12" = HL(db$BAD, predict(m12, type = "response")),
      "m13" = HL(db$BAD, predict(m13, type = "response")))