# Example 2:  bivariate beta with repeated measures
library(zoib)

data("BiRepeated", package = "zoib")
eg2 <- zoib(y1|y2 ~ x|1|x, data= BiRepeated, n.response=2,
            random=1, EUID= BiRepeated$id,
            zero.inflation = FALSE, one.inflation = FALSE,				
            prior.Sigma = "VC.unif", n.iter=5, n.thin=1, n.burn=1)
coeff<- eg2$coeff
ypred<- eg2$ypred
Xb<- eg2$Xb
Xd<- eg2$Xd
Xb0<- eg2$Xb0

