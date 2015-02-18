joint.1z01 <-
function(y, n, q, xmu.1, p.xmu, xsum.1, p.xsum, x1.1, p.x1, x0.1, p.x0,  
                       inflate0, inflate1, rid, EUID, nEU, prior1, prior2, prior.beta, 
                       prior.Sigma, prec.int, prec.DN, lambda.L1, lambda.L2, lambda.ARD,
                       scale.unif, scale.halft, link, n.chain) 
{ 
  dataIn <- vector("list",21)
  dataIn.name <- c("n","y","q","xmu.1","p.xmu","xsum.1","p.xsum","x0.1","p.x0",
                   "x1.1","p.x1","inflate0","inflate1","link","hyper",
                   "prior1","prior2","rid","EUID", "nEU","zero")
  names(dataIn)<- dataIn.name  
  dataIn[[1]] <- n      
  dataIn[[2]] <- as.matrix(y)
  dataIn[[3]] <- q
  dataIn[[4]] <- as.matrix(xmu.1)
  dataIn[[5]] <- p.xmu
  dataIn[[6]] <- as.matrix(xsum.1) 
  dataIn[[7]] <- p.xsum      
  dataIn[[8]] <- as.matrix(x0.1)
  dataIn[[9]] <- p.x0      
  dataIn[[10]]<- as.matrix(x1.1)
  dataIn[[11]]<- p.x1
  dataIn[[12]]<- inflate0
  dataIn[[13]]<- inflate1
  dataIn[[14]]<- link
  dataIn[[15]]<- c(prec.int,prec.DN,lambda.L1,lambda.L2,lambda.ARD)   
  if(grepl("unif",prior.Sigma))  dataIn[[15]] <- c(dataIn[[15]],scale.unif)
  if(grepl("halft",prior.Sigma)) dataIn[[15]] <- c(dataIn[[15]],scale.halft)   
  dataIn[[16]] <- prior1
  dataIn[[17]] <- prior2    
  dataIn[[18]] <- rid 
  dataIn[[19]] <- EUID
  dataIn[[20]] <- nEU
  dataIn[[21]] <- matrix(0,n,q)    
  
  init <- function( ){
    list("b.tmp"  = array(rnorm((p.xmu-1)*4*q,0,0.1), c((p.xmu-1),q,4)),
         "d.tmp"  = array(rnorm((p.xsum-1)*4*q,0,0.1),c((p.xsum-1),q,4)),
         "b1.tmp" = array(rnorm((p.x1-1)*4*q,0,0.1), c((p.x1-1),q,4)),
         "b0.tmp" = array(rnorm((p.x0-1)*4*q,0,0.1), c((p.x0-1),q,4)),
         
         "sigmab.L1" =  matrix(runif((p.xmu-1)*q,0,2), (p.xmu-1),q), 
         "sigmad.L1" =  matrix(runif((p.xsum-1)*q,0,2),(p.xmu-1),q),  
         "sigmab1.L1" = matrix(runif((p.x1-1)*q,0,2),(p.xmu-1),q),  
         "sigmab0.L1" = matrix(runif((p.x0-1)*q,0,2),(p.xmu-1),q),  
         
         "taub.ARD" =  matrix(runif((p.xmu-1)*q,0,2), (p.xmu-1),q), 
         "taud.ARD" =  matrix(runif((p.xsum-1)*q,0,2),(p.xmu-1),q),  
         "taub1.ARD" = matrix(runif((p.x1-1)*q,0,2), (p.xmu-1),q), 
         "taub0.ARD" = matrix(runif((p.x0-1)*q,0 ,2),(p.xmu-1),q),  
         
         "taub.L2" =  runif(q,0,2), 
         "taud.L2" =  runif(q,0,2),
         "taub0.L2" = runif(q,0,2),
         "taub1.L2" = runif(q,0,2),
         
         "sigma1" = runif(1,0.25,1),
         "xi" = runif(1,0.25,1),
         "eta" = runif(1,0.25,1))}    
  inits <- list(init());
  if(n.chain>=2) {for(j in 2:n.chain) inits <- c(inits,list(init( )))}      
  op<- system.file("bugs", "joint_1z01.bug", package="zoib") 
  model <- jags.model(op,data=dataIn,n.adapt=0,inits=inits, n.chains=n.chain)    
  return(model)
}
