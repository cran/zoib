sep.1z01 <-
function(y, n, xmu.1, p.xmu, xsum.1, p.xsum, x1.1, p.x1, x0.1, p.x0,  
                     rid, EUID, nEU, prior1, prior2, prior.beta, prior.Sigma, 
                     prec.int, prec.DN, lambda.L1, lambda.L2, lambda.ARD,
                     scale.unif, scale.halft, link, n.chain) 
{ 
  dataIn <- vector("list",18)
  names(dataIn) <- c("n","y","xmu.1","p.xmu","xsum.1","p.xsum","x0.1","p.x0","x1.1","p.x1",
                     "zero","link","hyper","prior1","prior2","rid","EUID", "nEU")
  dataIn[[1]] <- n      
  dataIn[[2]] <- y
  dataIn[[3]] <- as.matrix(xmu.1)
  dataIn[[4]] <- p.xmu
  dataIn[[5]] <- as.matrix(xsum.1)
  dataIn[[6]] <- p.xsum      
  dataIn[[7]] <- as.matrix(x0.1)
  dataIn[[8]] <- p.x0
  dataIn[[9]] <- as.matrix(x1.1)
  dataIn[[10]]<- p.x1       
  dataIn[[11]]<- rep(0,n)    
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
    list("b.tmp"  = matrix(rnorm((p.xmu-1)*4,0,0.1),ncol=4),
         "d.tmp"  = matrix(rnorm((p.xsum-1)*4,0,0.1),ncol=4),
         "b1.tmp" = matrix(rnorm((p.x1-1)*4,0,0.1),ncol=4),
         "b0.tmp" = matrix(rnorm((p.x0-1)*4,0,0.1),ncol=4),
         "sigmab.L1"  = runif((p.xmu-1),0,2), 
         "sigmad.L1"  = runif((p.xsum-1),0,2), 
         "sigmab1.L1" = runif((p.x1-1),0,2), 
         "sigmab0.L1" = runif((p.x0-1),0,2), 
         "taub.ARD"  = runif((p.xmu-1),0,2), 
         "taud.ARD"  = runif((p.xsum-1),0,2), 
         "taub1.ARD" = runif((p.x1-1),0,2), 
         "taub0.ARD" = runif((p.x0-1),0,2), 
         "taub.L2" = runif(1,0,2), 
         "taud.L2" = runif(1,0,2),
         "taub0.L2" = runif(1,0,2),
         "sigma1" = runif(1,0.25,1),
         "xi" = runif(1,0.25,1),
         "eta" = runif(1,0.25,1))}    
  inits <- list(init());
  if(n.chain>=2) {for(j in 2:n.chain) inits <- c(inits,list(init( )))}
  op<- system.file("bugs", "sep_1z01.bug",package="zoib") 
  model <- jags.model(op,data=dataIn,n.adapt=0, inits=inits,n.chains=n.chain)
  return(model)
}
