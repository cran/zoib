joint.2z01 <-
function(y, n, q, xmu.1, p.xmu, xsum.1, p.xsum, x1.1, p.x1, x0.1, p.x0,
                       inflate0, inflate1, zdummy, qz,nz0, m, rid, EUID, nEU,
                       prior1, prior2, prior.beta, prior.Sigma, 
                       prec.int, prec.DN, lambda.L1, lambda.L2,lambda.ARD,
                       scale.unif, scale.halft, link, n.chain) 
{
  dataIn <- vector("list",26)
  names(dataIn) <- c("y","n","q","xmu.1","p.xmu","xsum.1","p.xsum","x0.1","p.x0",
                     "x1.1","p.x1","inflate0","inflate1","z","nz0","qz","m","cumm",
                     "zero","link","hyper","prior1","prior2", "rid","EUID","nEU")
  dataIn[[1]] <- y
  dataIn[[2]] <- n      
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
  dataIn[[14]]<- zdummy
  dataIn[[15]]<- nz0
  dataIn[[16]]<- qz
  dataIn[[17]]<- m
  dataIn[[18]]<- c(0,cumsum(m[-nz0]))   
  dataIn[[19]]<- rep(0,n)
  dataIn[[20]]<- link
  dataIn[[21]]<- c(prec.int,prec.DN,lambda.L1,lambda.L2,lambda.ARD)       
  if(grepl("unif",prior.Sigma))  dataIn[[21]] <- c(dataIn[[21]],scale.unif)
  if(grepl("halft",prior.Sigma)) dataIn[[21]] <- c(dataIn[[21]],scale.halft)               
  dataIn[[22]] <- prior1
  dataIn[[23]] <- prior2 
  dataIn[[24]] <- rid
  dataIn[[25]] <- EUID
  dataIn[[26]] <- nEU
  
  init <- function( ){
    list("b.tmp"  = array(rnorm((p.xmu-1)*4*q,0,0.1), c((p.xmu-1),q,4)),
         "d.tmp"  = array(rnorm((p.xsum-1)*4*q,0,0.1),c((p.xsum-1),q,4)),
         "b1.tmp" = array(rnorm((p.x1-1)*4*q,0,0.1),  c((p.x1-1),q,4)),
         "b0.tmp" = array(rnorm((p.x0-1)*4*q,0,0.1),  c((p.x0-1),q,4)),
         
         "sigmab.L1" =  matrix(runif((p.xmu-1)*q,0,2),(p.xmu-1),q),
         "sigmad.L1" =  matrix(runif((p.xsum-1)*q,0,2),(p.xsum-1),q),  
         "sigmab1.L1" = matrix(runif((p.x1-1)*q,0,2),(p.x1-1),q),  
         "sigmab0.L1" = matrix(runif((p.x0-1)*q,0,2),(p.x0-1),q),  
         
         "taub.ARD" =  matrix(runif((p.xmu-1)*q,0,2), (p.xmu-1),q), 
         "taud.ARD" =  matrix(runif((p.xsum-1)*q,0,2),(p.xsum-1),q),  
         "taub1.ARD" = matrix(runif((p.x1-1)*q,0,2), (p.x1-1),q), 
         "taub0.ARD" = matrix(runif((p.x0-1)*q,0 ,2),(p.x0-1),q),  
         
         "taub.L2" =  runif(q,0,2), 
         "taud.L2" =  runif(q,0,2),
         "taub0.L2" = runif(q,0,2),
         "taub1.L2" = runif(q,0,2),
         
         "sigma.VC1" = runif(nz0,0,2),
         "t" = runif(nz0,0,1),
         "scale1" = runif(qz,0,2),
         "scale2" = runif(qz,0,2),
         "rho1" = dunif(-0.5,0.5),
         "rho2" = dunif(-0.5,0.5))}    
  inits <- list(init());
  if(n.chain>=2) {for(j in 2:n.chain) inits <- c(inits,list(init( )))}       
  op<- system.file("bugs", "joint_2z01.bug", package="zoib") 
  model <- jags.model(op,data=dataIn,n.adapt=0, inits=inits, n.chains=n.chain)   
  return(model)
}
