joint.2z <-
function(y, n, q, xmu.1, p.xmu, xsum.1, p.xsum, 
                     zdummy, qz,nz0, m, rid, EUID, nEU,
                     prior1, prior2, prior.beta, prior.Sigma, 
                     prec.int, prec.DN, lambda.L1, lambda.L2,lambda.ARD,
                     scale.unif, scale.halft, link, n.chain) 
{ 
  dataIn <- vector("list",20)
  names(dataIn) <- c("y","n","q","xmu.1","p.xmu","xsum.1","p.xsum",
                     "z","nz0","qz","m","cumm","zero","link",
                     "hyper","prior1","prior2","rid","EUID","nEU")
  dataIn[[1]] <- as.matrix(y)
  dataIn[[2]] <- n      
  dataIn[[3]] <- q
  dataIn[[4]] <- as.matrix(xmu.1)
  dataIn[[5]] <- p.xmu
  dataIn[[6]] <- as.matrix(xsum.1) 
  dataIn[[7]] <- p.xsum       
  dataIn[[8]] <- zdummy
  dataIn[[9]] <- nz0
  dataIn[[10]]<- qz
  dataIn[[11]]<- m
  dataIn[[12]]<- c(0,cumsum(m[-nz0])) 
  dataIn[[13]]<- matrix(0,n,q)   
  dataIn[[14]]<- link  
  dataIn[[15]]<- c(prec.int,prec.DN,lambda.L1,lambda.L2,lambda.ARD)          
  if(grepl("unif",prior.Sigma))   dataIn[[15]] <- c(dataIn[[15]],scale.unif)
  if(grepl("halft",prior.Sigma))  dataIn[[15]] <- c(dataIn[[15]],scale.halft)      
  dataIn[[16]] <- prior1
  dataIn[[17]] <- prior2    
  dataIn[[18]] <- rid
  dataIn[[19]] <- EUID 
  dataIn[[20]] <- nEU
 
  init <- function( ){
    rho1 <- runif(1,0,0.999)
    rho2 <- runif(1,0,0.999) 
    rho3 <- runif(1, rho1*rho2 - sqrt((1-rho1^2)*(1-rho2^2)), 
                  rho1*rho2 + sqrt((1-rho1^2)*(1-rho2^2)))
    return(
      list("b.tmp" = array(rnorm((p.xmu-1)*4*q,0,0.1), c((p.xmu-1),q,4)),
           "d.tmp" = array(rnorm((p.xsum-1)*4*q,0,0.1),c((p.xsum-1),q,4)),
         
         "sigmab.L1" =  matrix(runif((p.xmu-1)*q,0,2), (p.xmu-1),q), 
         "sigmad.L1" =  matrix(runif((p.xsum-1)*q,0,2),(p.xsum-1),q),  
         
         "taub.ARD" =  matrix(runif((p.xmu-1)*q,0,2), (p.xmu-1),q), 
         "taud.ARD" =  matrix(runif((p.xsum-1)*q,0,2),(p.xsum-1),q), 
         
         "taub.L2" =  runif(q,0,2), 
         "taud.L2" =  runif(q,0,2),
         
         "rho1" = rho1,
         "rho2" = rho2,
         "rho3" = rho3,
         
         "sigma.VC1" = runif(nz0,0.25,2),
         "t" = runif(nz0,0.25,1),
         "scale1" = runif(qz,0.25,2),
         "scale2" = runif(qz,0.25,2)))}      
  inits <- list(init());
  if(n.chain>=2) { for(j in 2:n.chain) inits <- c(inits,list(init( )))}
  op<- system.file("bugs", "joint_2z.bug", package="zoib") 
  model <- jags.model(op,data = dataIn, n.adapt=0, inits = inits,n.chains=n.chain)  
  return(model)
}
