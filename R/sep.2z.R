sep.2z <-
function(y, n, xmu.1, p.xmu, xsum.1, p.xsum, 
                   zdummy, qz,nz0, m, rid, EUID, nEU,
                   prior1, prior2, prior.beta, prior.Sigma, 
                   prec.int, prec.DN, lambda.L1, lambda.L2,lambda.ARD,
                   scale.unif, scale.halft, link, n.chain) 
{
  dataIn <- vector("list",19)
  names(dataIn) <- c("n","y","xmu.1","p.xmu","xsum.1","p.xsum",
                     "z","nz0","qz","m","cumm", "zero","link",
                     "prior1","prior2","hyper","rid","EUID","nEU")
  dataIn[[1]] <- n      
  dataIn[[2]] <- y
  dataIn[[3]] <- as.matrix(xmu.1)
  dataIn[[4]] <- p.xmu
  dataIn[[5]] <- as.matrix(xsum.1)
  dataIn[[6]] <- p.xsum      
  dataIn[[7]] <- zdummy
  dataIn[[8]] <- nz0
  dataIn[[9]] <- qz
  dataIn[[10]]<- m
  dataIn[[11]]<- c(0,cumsum(m[-nz0]))   
  dataIn[[12]]<- rep(0,n) 
  dataIn[[13]]<- link
  dataIn[[14]] <- prior1
  dataIn[[15]] <- prior2 
  dataIn[[16]] <- c(prec.int,prec.DN,lambda.L1,lambda.L2,lambda.ARD)          
  if(grepl("unif",prior.Sigma))  dataIn[[16]] <- c(dataIn[[16]],scale.unif)
  if(grepl("halft",prior.Sigma)) dataIn[[16]] <- c(dataIn[[16]],scale.halft)   
  dataIn[[17]] <- rid
  dataIn[[18]] <- EUID 
  dataIn[[19]] <- nEU
  
  init <- function( ){
    list("b.tmp" = matrix(rnorm((p.xmu-1)*4,0,0.1),ncol=4),
         "d.tmp" = matrix(rnorm((p.xsum-1)*4,0,0.1),ncol=4),
         
         "sigmab.L1" = runif((p.xmu-1),0,2), 
         "sigmad.L1" = runif((p.xsum-1),0,2), 
         
         "taub.ARD" = runif((p.xmu-1),0,2), 
         "taud.ARD" = runif((p.xsum-1),0,2), 
         
         "taub.L2" = runif(1,0,2), 
         "taud.L2" = runif(1,0,2),
         
         "sigma.VC1" = runif(nz0,0,2),
         "xi1" = runif(nz0,0,1),
         "eta1" = runif(nz0,0,1),
         
         "scale1" = runif(qz,0,2),
         "scale2" = runif(qz,0,2))}    
  inits <- list(init());
  if(n.chain>=2) { for(j in 2:n.chain) inits <- c(inits,list(init( )))}
  
  op<- system.file("bugs", "sep_2z.bug", package="zoib") 
  model <- jags.model(op, data=dataIn,n.adapt=0, inits=inits, n.chains=n.chain)  
  return(model)
}
