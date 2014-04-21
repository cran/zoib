joint.1z1 <-
function(y, n, q, xmu.1, p.xmu, xsum.1, p.xsum, x1.1, p.x1,inflate1,
                      rid, EUID, nEU, prior1, prior2, prior.beta, prior.Sigma, 
                      prec.int, prec.DN, lambda.L1, lambda.L2,lambda.ARD,
                      scale.unif, scale.halft, link, n.chain) 
{ 
  dataIn <- vector("list",18)
  dataIn.name <- c("n","y","q","xmu.1","p.xmu","xsum.1","p.xsum","x1.1","p.x1",                 
                   "inflate1","link","hyper","prior1","prior2","rid","EUID","nEU", "zero")
  names(dataIn)<- dataIn.name 
  dataIn[[1]] <- n      
  dataIn[[2]] <- as.matrix(y)
  dataIn[[3]] <- q
  dataIn[[4]] <- as.matrix(xmu.1)
  dataIn[[5]] <- p.xmu
  dataIn[[6]] <- as.matrix(xsum.1) 
  dataIn[[7]] <- p.xsum      
  dataIn[[8]] <- as.matrix(x1.1)
  dataIn[[9]] <- p.x1  
  dataIn[[10]]<- inflate1 
  dataIn[[11]]<- matrix(0,n,q)  
  dataIn[[12]]<- link 
  dataIn[[13]]<- c(prec.int,prec.DN,lambda.L1,lambda.L2,lambda.ARD)        
  if(grepl("unif",prior.Sigma))  dataIn[[13]] <- c(dataIn[[13]],scale.unif)
  if(grepl("halft",prior.Sigma)) dataIn[[13]] <- c(dataIn[[13]],scale.halft)   
  dataIn[[14]] <- prior1
  dataIn[[15]] <- prior2    
  dataIn[[16]] <- rid 
  dataIn[[17]] <- EUID
  dataIn[[18]] <- nEU
  
  init <- function( ){
    list("b.tmp" = array(rnorm((p.xmu-1)*4*q,0,0.1),  c((p.xmu-1),q,4)),
         "d.tmp" = array(rnorm((p.xsum-1)*4*q,0,0.1),c((p.xsum-1),q,4)),
         "b1.tmp"= array(rnorm((p.x1-1)*4*q,0,0.1), c((p.x1-1),q,4)),
         
         "sigmab.L1" =  matrix(runif((p.xmu-1)*q,0,2),(p.xmu-1),q), 
         "sigmad.L1" =  matrix(runif((p.xsum-1)*q,0,2),(p.xmu-1),q),  
         "sigmab1.L1" = matrix(runif((p.x1-1)*q,0,2),(p.xmu-1),q),  
         
         "taub.ARD" =  matrix(runif((p.xmu-1)*q,0,2), (p.xmu-1),q), 
         "taud.ARD" =  matrix(runif((p.xsum-1)*q,0,2),(p.xmu-1),q),  
         "taub1.ARD" = matrix(runif((p.x1-1)*q,0,2), (p.xmu-1),q), 
         
         "taub.L2" =  runif(q,0,2), 
         "taud.L2" =  runif(q,0,2),
         "taub1.L2" = runif(q,0,2),
         
         "sigma1" = runif(1,0,1),
         "xi" = runif(1,0,1),
         "eta" = runif(1,0,1))}    
  inits <- list(init());
  if(n.chain>=2) {for(j in 2:n.chain) inits <- c(inits,list(init( )))}        
  op<- system.file("bugs", "joint_1z1.bug", package="zoib") 
  model <- jags.model(op,n.adapt=0, data=dataIn,inits=inits, n.chains=n.chain)   
  return(model)
}
