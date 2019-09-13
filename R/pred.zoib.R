pred.zoib<- function(object, xnew, summary=TRUE)
{
  model<-  object$model
  formula  <- as.Formula(model)
  nterm <- length(formula)
  
  data <- xnew
  n<- nrow(xnew)
  nchain<- length(object$coeff)
 
  zero.inflation <- !is.null(object$Xb0)
  one.inflation  <- !is.null(object$Xb1)

  xmu <- as.matrix(model.matrix(formula,data=xnew,rhs=1))
  xsum <- as.matrix(model.matrix(formula,data=xnew,rhs=2))
  
  if(zero.inflation & one.inflation){
    x0 <- as.matrix(model.matrix(formula,data=xnew,rhs=3))
    x1 <- as.matrix(model.matrix(formula,data=xnew,rhs=4))
    } else if(zero.inflation & !one.inflation){
    x0 <- as.matrix(model.matrix(formula,data=xnew,rhs=3))
    } else if(!zero.inflation & one.inflation){
    x1 <- as.matrix(model.matrix(formula,data=xnew,rhs=4))
    }
  
  
  ############## original data design matrix #################
  
  #ypred <- list(newdata, newdata)
  ypred <- NULL
  
  xmu.1 <-  object$Xb;  p.xmu <- ncol(xmu.1) 
  xsum.1 <- object$Xd;  p.xsum<- ncol(xsum.1)
  x0.1 <- object$Xb0
  x1.1 <- object$Xb1
  nsample <- n
  
  for(k in 1:nchain){
    b <- t(object$coeff[[k]][,1:p.xmu]) 
    ypred[[k]]<- matrix(NA,n, ncol(b))
    if(is.null(x1.1)& is.null(x0.1)){
      for(i in 1:n) 
        ypred[[k]][i,]<- exp(xmu[i,]%*%b)/(1+exp(xmu[i,]%*%b))
    }
    if(!is.null(x0.1) & is.null(x1.1)){
      p.x0 <- ncol(x0.1)   
      b0 <- t(object$coeff[[k]][,1:p.x0+p.xmu])
      for(i in 1:n)  
        ypred[[k]][i,]<- exp(xmu[i,]%*%b)/(1+exp(xmu[i,]%*%b))/(1+exp(x0[i,]%*%b0))
    }
    if(!is.null(x1.1)& is.null(x0.1)){
      p.x1 <-ncol(x1.1)     
      b1 <- object$coeff[[k]][,1:p.x1+p.xmu] 
      for(i in 1:n)  
        ypred[[k]][i,]<- (exp(xmu[i,]%*%b)/(1+exp(xmu[i,]%*%b))+exp(x1[i,]%*%b1))/(1+exp(x1[i,]%*%b1))
    }
    if(!is.null(x0.1) & !is.null(x1.1)){
      p.x0 <- ncol(x0.1)   
      b0 <- t(object$coeff[[k]][,1:p.x0+p.xmu])
      p.x1 <-ncol(x1.1)     
      b1 <- t(object$coeff[[k]][,1:p.x1+p.xmu+p.x0])
      for(i in 1:n) 
        ypred[[k]][i,]<- (exp(xmu[i,]%*%b)/(1+exp(xmu[i,]%*%b))+ exp(x1[i,]%*%b1) )/((1+exp(x0[i,]%*%b0))*(1+exp(x1[i,]%*%b1)))
    }
  }
  if(summary){
    summ<- matrix(0,n,8)
    colnames(summ)<- c("n",'mean','SD','min','max','med','2.5%','97.5%')
    for(i in 1:n){
      ypredi <- ypred[[1]][i,]
      if(nchain>1) for(k in 2:nchain)  ypredi<- c(ypredi,ypred[[1]][i,])
      summ[i,]<- c(length(ypredi),mean(ypredi),sd(ypredi),min(ypredi),max(ypredi),
                median(ypredi),quantile(ypredi,0.025),quantile(ypredi,0.975))
    }
  }
  return(list(pred=ypred, summary=summ))
}




