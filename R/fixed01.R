fixed01 <-
function(y, n, xmu.1,p.xmu,xsum.1,p.xsum, x0.1,p.x0, x1.1, p.x1,prior1,
                    prec.int, prec.DN,  lambda.L1, lambda.L2,lambda.ARD, link,n.chain)
{ 
  dataIn <- vector("list",14)  
  dataIn.name <- c("y","xmu.1","p.xmu","xsum.1","p.xsum", "x0.1","p.x0",
                   "x1.1","p.x1","n","zero","prior1","hyper","link")
  names(dataIn)<- dataIn.name  
  dataIn[[1]] <- y
  dataIn[[2]] <- as.matrix(xmu.1)
  dataIn[[3]] <- p.xmu
  dataIn[[4]] <- as.matrix(xsum.1)
  dataIn[[5]] <- p.xsum      
  dataIn[[6]] <- as.matrix(x0.1)
  dataIn[[7]] <- p.x0
  dataIn[[8]] <- as.matrix(x1.1)
  dataIn[[9]] <- p.x1       
  dataIn[[10]]<- n
  dataIn[[11]]<- rep(0,n) 
  dataIn[[12]]<- prior1
  dataIn[[13]]<- c(prec.int,prec.DN,lambda.L1,lambda.L2,lambda.ARD)
  dataIn[[14]]<- link
  
  init <- function( ){
    list("b.tmp" =  matrix(rnorm((p.xmu-1)*4,0,0.1),ncol=4),
         "d.tmp" =  matrix(rnorm((p.xsum-1)*4,0,0.1),ncol=4),
         "b1.tmp" = matrix(rnorm((p.x1-1)*4,0,0.1),ncol=4),
         "b0.tmp" = matrix(rnorm((p.x0-1)*4,0,0.1),ncol=4),
         "sigmab.L1" =  runif((p.xmu-1),0,2), 
         "sigmad.L1" =  runif((p.xsum-1),0,2), 
         "sigmab1.L1" = runif((p.x1-1),0,2), 
         "sigmab0.L1" = runif((p.x0-1),0,2), 
         "taub.ARD" =  runif((p.xmu-1),0,2), 
         "taud.ARD" =  runif((p.xsum-1),0,2), 
         "taub1.ARD" = runif((p.x1-1),0,2), 
         "taub0.ARD" = runif((p.x0-1),0,2), 
         "taub.L2" =  runif(1,0,2), 
         "taud.L2" =  runif(1,0,2),
         "taub0.L2" = runif(1,0,2),
         "taub1.L2" = runif(1,0,2))}    
  inits <- list(init());
  if(n.chain>=2){ for(j in 2:n.chain) inits <- c(inits,list(init( ))) } 
  op<- system.file("bugs", "fixed01.bug",package="zoib") 
  model<- jags.model(op,data=dataIn,n.adapt=0,inits=inits,n.chains=n.chain)  
  return(model)
}
