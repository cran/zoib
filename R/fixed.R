fixed <-
function(y, n, xmu.1,p.xmu,xsum.1,p.xsum, prior1, prec.int, 
         prec.DN, lambda.L1, lambda.L2, lambda.ARD, link,n.chain)
{
  dataIn <- vector("list",10)  
  dataIn.name <- c("y","xmu.1","p.xmu","xsum.1","p.xsum",
                   "n","zero","prior1","hyper","link")
  names(dataIn)<- dataIn.name  
  dataIn[[1]] <- y
  dataIn[[2]] <- as.matrix(xmu.1)
  dataIn[[3]] <- p.xmu
  dataIn[[4]] <- as.matrix(xsum.1)
  dataIn[[5]] <- p.xsum      
  dataIn[[6]] <- n
  dataIn[[7]] <- rep(0,n)   
  dataIn[[8]] <- prior1
  dataIn[[9]] <- c(prec.int,prec.DN,lambda.L1,lambda.L2,lambda.ARD)
  dataIn[[10]] <- link
  
  init <- function( ){
    list("b.tmp" = matrix(rnorm((p.xmu-1)*4,0,0.1),ncol=4),
         "d.tmp" = matrix(rnorm((p.xsum-1)*4,0,0.1),ncol=4),
         "sigmab.L1" = runif((p.xmu-1),0,1), 
         "sigmad.L1" = runif((p.xsum-1),0,1), 
         "taub.ARD" = runif((p.xmu-1),0,1), 
         "taud.ARD" = runif((p.xsum-1),0,1), 
         "taub.L2" = runif(1,0,1), 
         "taud.L2" = runif(1,0,1))}     
  inits <- list(init( ));
  if(n.chain >= 2) {for(j in 2:n.chain)  inits <- c(inits,list(init( ))) } 
  
  op<- system.file("bugs","fixed.bug",package="zoib") 
  model <- jags.model(op,data=dataIn,n.adapt=0, inits=inits,n.chains=n.chain)
  return(model)
}
