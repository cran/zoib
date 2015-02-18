sep.1z <-
function(y, n, xmu.1, p.xmu, xsum.1, p.xsum, 
                   rid, EUID, nEU, prior1, prior2, prior.beta, prior.Sigma, 
                   prec.int, prec.DN, lambda.L1, lambda.L2,lambda.ARD,
                   scale.unif, scale.halft, link, n.chain) 
{ 
  dataIn <- vector("list",14)
  names(dataIn) <- c("n","y","xmu.1","p.xmu","xsum.1", "p.xsum",
                     "zero","link","hyper","prior1","prior2","rid","EUID","nEU")

  dataIn[[1]] <- n      
  dataIn[[2]] <- y
  dataIn[[3]] <- as.matrix(xmu.1)
  dataIn[[4]] <- p.xmu
  dataIn[[5]] <- as.matrix(xsum.1)
  dataIn[[6]] <- p.xsum      
  dataIn[[7]] <- rep(0,n)
  dataIn[[8]] <- link
  dataIn[[9]] <- c(prec.int,prec.DN,lambda.L1,lambda.L2,lambda.ARD)      
  if(grepl("unif",prior.Sigma))  dataIn[[9]] <- c(dataIn[[9]],scale.unif)
  if(grepl("halft",prior.Sigma)) dataIn[[9]] <- c(dataIn[[9]],scale.halft)   
  dataIn[[10]] <- prior1
  dataIn[[11]] <- prior2    
  dataIn[[12]] <- rid 
  dataIn[[13]] <- EUID
  dataIn[[14]] <- nEU
    
  init <- function( ){
    list("b.tmp" = matrix(rnorm((p.xmu-1)*4,0,0.1),ncol=4),
         "d.tmp" = matrix(rnorm((p.xsum-1)*4,0,0.1),ncol=4),
         
         "sigmab.L1" = runif((p.xmu-1),0,2), 
         "sigmad.L1" = runif((p.xsum-1),0,2), 
         
         "taud.ARD" = runif((p.xsum-1),0,2), 
         
         "taub.L2" = runif(1,0,2), 
         "taud.L2" = runif(1,0,2),
         
         "sigma1" = runif(1,0.25,1),
         "xi" = runif(1,0.25,1),
         "eta" = runif(1,0.25,1))}    
  inits <- list(init());
  if(n.chain>=2) {for(j in 2:n.chain) inits <- c(inits,list(init( )))}
  op<- system.file("bugs", "sep_1z.bug",package="zoib") 
  model <- jags.model(op, data=dataIn,n.adapt=0,inits=inits,n.chains=n.chain)  
  return(model)
}
