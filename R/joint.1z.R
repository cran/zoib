joint.1z <-
function(y, n, q, xmu.1, p.xmu, xsum.1, p.xsum,  
                     rid, EUID, nEU, prior1, prior2, prior.beta, prior.Sigma, 
                     prec.int, prec.DN, lambda.L1, lambda.L2, lambda.ARD,
                     scale.unif, scale.halft, link, n.chain) 
{ 
  dataIn <- vector("list",15)
  names(dataIn)<- c("n","y","q","xmu.1","p.xmu","xsum.1","p.xsum",
                    "zero","link", "hyper", "prior1","prior2","rid","EUID","nEU")
  dataIn[[1]] <- n      
  dataIn[[2]] <- as.matrix(y)
  dataIn[[3]] <- q
  dataIn[[4]] <- as.matrix(xmu.1, nrow=n)
  dataIn[[5]] <- p.xmu
  dataIn[[6]] <- as.matrix(xsum.1, nrow=n) 
  dataIn[[7]] <- p.xsum  
  dataIn[[8]] <- matrix(0,n,q)  
  dataIn[[9]] <- link 
  dataIn[[10]]<- c(prec.int,prec.DN,lambda.L1,lambda.L2,lambda.ARD)  
  if(grepl("unif", prior.Sigma)) dataIn[[10]] <- c(dataIn[[10]],scale.unif)
  if(grepl("halft",prior.Sigma)) dataIn[[10]] <- c(dataIn[[10]],scale.halft)                    
  dataIn[[11]] <- prior1
  dataIn[[12]] <- prior2    
  dataIn[[13]] <- rid 
  dataIn[[14]] <- EUID
  dataIn[[15]] <- nEU

  init <- function( ){
    list("b.tmp" = array(rnorm((p.xmu-1)*4*q,0,0.1),  c((p.xmu-1),q,4)),
         "d.tmp" = array(rnorm((p.xsum-1)*4*q,0,0.1), c((p.xsum-1),q,4)),
         
         "sigmab.L1" =  matrix(runif((p.xmu-1)*q,0,2), (p.xmu-1),q), 
         "sigmad.L1" =  matrix(runif((p.xsum-1)*q,0,2),(p.xmu-1),q),  
         
         "taub.ARD" =  matrix(runif((p.xmu-1)*q,0,2), (p.xmu-1),q), 
         "taud.ARD" =  matrix(runif((p.xsum-1)*q,0,2),(p.xmu-1),q),  
         
         "taub.L2" =  runif(q,0,2), 
         "taud.L2" =  runif(q,0,2),
         
         "sigma1" = runif(1,0.25,1),
         "xi" = runif(1,0.25,1),
         "eta" = runif(1,0.25,1))}    
  inits <- list(init());
  if(n.chain>=2) {for(j in 2:n.chain) inits <- c(inits,list(init( )))} 
  op<- system.file("bugs", "joint_1z.bug", package="zoib") 
  model <- jags.model(op,data=dataIn,n.adapt=0, inits=inits, n.chains=n.chain)   
  return(model)
}
