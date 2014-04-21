# Example 2:  bivariate beta with repeated measures
library(zoib)

data("BiRepeated", package = "zoib")
eg2 <- zoib(y1|y2 ~ x|1|x, data= BiRepeated,
            random=1, EUID= BiRepeated$id,
            zero.inflation = FALSE, one.inflation = FALSE,				
            prior.Sigma = "UN.unif", n.iter=10, n.thin=2, n.burn=1)
eg2$coeff
eg2$ypred
eg2$Xb
eg2$Xd
eg2$Xb0
eg2$Xb1
