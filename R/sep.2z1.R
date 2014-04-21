sep.2z1 <-
function(y, n, xmu.1, p.xmu, xsum.1, p.xsum, x1.1, p.x1, 
                    zdummy, qz,nz0, m, rid, EUID, nEU,
                    prior1, prior2, prior.beta, prior.Sigma, 
                    prec.int, prec.DN, lambda.L1, lambda.L2,lambda.ARD,
                    scale.unif, scale.halft, link, n.chain) 
{
  dataIn <- vector("list",21)
  names(dataIn) <- c("n","y","xmu.1","p.xmu","xsum.1","p.xsum","x1.1",
                     "p.x1","z","nz0","qz","m","cumm","zero","link",
                     "hyper","prior1","prior2","rid","EUID","nEU")
  dataIn[[1]] <- n      
  dataIn[[2]] <- y
  dataIn[[3]] <- as.matrix(xmu.1)
  dataIn[[4]] <- p.xmu
  dataIn[[5]] <- as.matrix(xsum.1)
  dataIn[[6]] <- p.xsum      
  dataIn[[7]] <- as.matrix(x1.1)
  dataIn[[8]] <- p.x1
  dataIn[[9]] <- zdummy
  dataIn[[10]]<- nz0
  dataIn[[11]]<- qz
  dataIn[[12]]<- m
  dataIn[[13]]<- c(0,cumsum(m[-nz0]))   
  dataIn[[14]]<- rep(0,n)
  dataIn[[15]]<- link
  dataIn[[16]]<- c(prec.int,prec.DN,lambda.L1,lambda.L2,lambda.ARD)   
  if(grepl("unif",prior.Sigma))  dataIn[[16]] <- c(dataIn[[16]],scale.unif)
  if(grepl("halft",prior.Sigma)) dataIn[[16]] <- c(dataIn[[16]],scale.halft)           
  dataIn[[17]] <- prior1
  dataIn[[18]] <- prior2    
  dataIn[[19]] <- rid 
  dataIn[[20]] <- EUID
  dataIn[[21]] <- nEU
  
  init <- function( ){
    list("b.tmp"  = matrix(rnorm((p.xmu-1)*4,0,0.1),ncol=4),
         "d.tmp"  = matrix(rnorm((p.xsum-1)*4,0,0.1),ncol=4),
         "b1.tmp" = matrix(rnorm((p.x1-1)*4,0,0.1), ncol=4),
         
         "sigmab.L1" = runif((p.xmu-1),0,2), 
         "sigmad.L1" = runif((p.xsum-1),0,2), 
         "sigmab1.L1" = runif((p.x1-1),0,2), 
         
         "taub.ARD" = runif((p.xmu-1),0,2), 
         "taud.ARD" = runif((p.xsum-1),0,2), 
         "taub1.ARD" = runif((p.x1-1),0,2), 
         
         "taub.L2" = runif(1,0,2), 
         "taud.L2" = runif(1,0,2),
         "taub1.L2" = runif(1,0,2),
         
         "sigma.VC1" = runif(nz0,0,2),
         "xi1" = runif(nz0,0,1),
         "eta1" = runif(nz0,0,1),
         
         "scale1" = runif(qz,0,2),
         "scale2" = runif(qz,0,2))}            
  inits=list(init( ));
  if(n.chain>=2) {for(j in 2:n.chain) inits <- c(inits,list(init( )))}  
  op<- system.file("bugs", "sep_2z1.bug",package="zoib") 
  model<- jags.model(op,data=dataIn,n.adapt=0, inits=inits, n.chains=n.chain)   
  return(model)
}