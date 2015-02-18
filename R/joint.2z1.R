joint.2z1 <-
function(y, n, q, xmu.1, p.xmu, xsum.1, p.xsum, x1.1, p.x1,
                      inflate1, zdummy, qz,nz0, m, rid, EUID, nEU,
                      prior1, prior2, prior.beta, prior.Sigma, 
                      prec.int, prec.DN, lambda.L1, lambda.L2,lambda.ARD,
                      scale.unif, scale.halft, link, n.chain) 
{ 
  dataIn <- vector("list",23)
  dataIn.name <- c("y","n","q","xmu.1","p.xmu", "xsum.1","p.xsum","x1.1","p.x1",
                   "inflate1", "z","nz0","qz","m","zero","cumm","link","hyper",
                   "prior1","prior2","rid","EUID","nEU")
  names(dataIn)<- dataIn.name  
  dataIn[[1]] <- as.matrix(y)
  dataIn[[2]] <- n      
  dataIn[[3]] <- q
  dataIn[[4]] <- as.matrix(xmu.1)
  dataIn[[5]] <- p.xmu
  dataIn[[6]] <- as.matrix(xsum.1) 
  dataIn[[7]] <- p.xsum      
  dataIn[[8]] <- as.matrix(x1.1)
  dataIn[[9]] <- p.x1      
  dataIn[[10]]<- inflate1
  dataIn[[11]]<- zdummy
  dataIn[[12]]<- nz0
  dataIn[[13]]<- qz
  dataIn[[14]]<- m
  dataIn[[15]]<- c(0,cumsum(m[-nz0])) 
  dataIn[[16]]<- matrix(0,n,q)   
  dataIn[[17]]<- link  
  dataIn[[18]]<- c(prec.int,prec.DN,lambda.L1,lambda.L2,lambda.ARD)         
  if(grepl("unif",prior.Sigma))  dataIn[[18]] <- c(dataIn[[18]],scale.unif)
  if(grepl("halft",prior.Sigma)) dataIn[[18]] <- c(dataIn[[18]],scale.halft)              
  dataIn[[19]] <- prior1
  dataIn[[20]] <- prior2    
  dataIn[[21]] <- rid
  dataIn[[22]] <- EUID 
  dataIn[[23]] <- nEU    
    
  init <- function( ){
    list("b.tmp" = array(rnorm((p.xmu-1)*4*q,0,0.1), c((p.xmu-1),q,4)),
         "d.tmp" = array(rnorm((p.xsum-1)*4*q,0,0.1),c((p.xsum-1),q,4)),
         "b1.tmp"= array(rnorm((p.x1-1)*4*q,0,0.1),  c((p.x1-1),q,4)),
         
         "sigmab.L1" =  matrix(runif((p.xmu-1)*q,0,2), (p.xmu-1),q), 
         "sigmad.L1" =  matrix(runif((p.xsum-1)*q,0,2),(p.xsum-1),q),  
         "sigmab1.L1"=  matrix(runif((p.x1-1)*q,0,2),  (p.x1-1),q),  
         
         "taub.ARD" =  matrix(runif((p.xmu-1)*q,0,2), (p.xmu-1),q), 
         "taud.ARD" =  matrix(runif((p.xsum-1)*q,0,2),(p.xsum-1),q),  
         "taub1.ARD" = matrix(runif((p.x1-1)*q,0,2), (p.x1-1),q), 
         
         "taub.L2" =  runif(q,0,2), 
         "taud.L2" =  runif(q,0,2),
         "taub1.L2" = runif(q,0,2),
         
         "sigma.VC1" = runif(nz0,0.25,2),
         "t" = runif(nz0,0.25,1),
         "scale1" = runif(qz,0.25,2),
         "scale2" = runif(qz,0.25,2),
         "rho1" = dunif(-0.5,0.5),
         "rho2" = dunif(-0.5,0.5))}    
  inits <- list(init());
  if(n.chain>=2) {for(j in 2:n.chain) inits <- c(inits,list(init( )))}
  op<- system.file("bugs", "joint_2z1.bug", package="zoib") 
  model <- jags.model(op, data = dataIn,n.adapt=0, inits = inits,n.chains=n.chain)   
  return(model)
}
