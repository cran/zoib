sep.1z0 <-
function(y, n, xmu.1, p.xmu, xsum.1, p.xsum, x0.1, p.x0, 
                    rid, EUID, nEU, prior1, prior2, prior.beta, prior.Sigma, 
                    prec.int, prec.DN, lambda.L1, lambda.L2, lambda.ARD,
                    scale.unif, scale.halft, link, n.chain, inits, seed) 
{ 
  dataIn <- vector("list",17)
  names(dataIn) <- c("n","y","xmu.1","p.xmu","xsum.1","p.xsum","x0.1","p.x0",
                     "zero","link","hyper","prior1","prior2","rid","EUID", 
                     "nEU","hyper2")
  dataIn[[1]] <- n      
  dataIn[[2]] <- y
  dataIn[[3]] <- as.matrix(xmu.1)
  dataIn[[4]] <- p.xmu
  dataIn[[5]] <- as.matrix(xsum.1)
  dataIn[[6]] <- p.xsum      
  dataIn[[7]] <- as.matrix(x0.1)
  dataIn[[8]] <- p.x0
  dataIn[[9]]<- rep(0,n)
  dataIn[[10]]<- link
  dataIn[[11]]<- as.matrix(cbind(prec.int,prec.DN,lambda.L1,lambda.L2,lambda.ARD))      
  if(grepl("unif",prior.Sigma))  dataIn[[17]] <- scale.unif
  if(grepl("halfcauchy",prior.Sigma)) dataIn[[17]] <- scale.halft     
  dataIn[[12]] <- prior1
  dataIn[[13]] <- prior2    
  dataIn[[14]] <- rid 
  dataIn[[15]] <- EUID
  dataIn[[16]] <- nEU
  
  if(is.null(seed)){
    init <- function( rngname, rngseed){
      list("tmp1" = rnorm(1,0,0.1),
           "tmp2" = rnorm(1,0,0.1),
           "tmp3" = rnorm(1,0,0.1),
           
           "b.tmp"  = matrix(rnorm((p.xmu-1)*4,0,0.1),ncol=4),
           "d.tmp"  = matrix(rnorm((p.xsum-1)*4,0,0.1),ncol=4),
           "b0.tmp" = matrix(rnorm((p.x0-1)*4,0,0.1),ncol=4),
           
           "sigmab.L1"  = runif((p.xmu-1),0,2), 
           "sigmad.L1"  = runif((p.xsum-1),0,2), 
           "sigmab0.L1" = runif((p.x0-1),0,2), 
           
           "taub.ARD"  = runif((p.xmu-1),0,2), 
           "taud.ARD"  = runif((p.xsum-1),0,2), 
           "taub0.ARD" = runif((p.x0-1),0,2), 
           
           "taub.L2"  = runif(1,0,2), 
           "taud.L2"  = runif(1,0,2),
           "taub0.L2" = runif(1,0,2),
           
           "sigma1" = runif(1,0.25,1),
           "scale2" = runif(1,0.25,1))}
    
      inits.internal <- list(init( ));
      if(n.chain >= 2) {
        for(j in 2:n.chain) inits.internal <- c(inits.internal,list(init()))} 
    } else{
      init <- function( rngname, rngseed){
      list("tmp1" = rnorm(1,0,0.1),
         "tmp2" = rnorm(1,0,0.1),
         "tmp3" = rnorm(1,0,0.1),
         
         "b.tmp"  = matrix(rnorm((p.xmu-1)*4,0,0.1),ncol=4),
         "d.tmp"  = matrix(rnorm((p.xsum-1)*4,0,0.1),ncol=4),
         "b0.tmp" = matrix(rnorm((p.x0-1)*4,0,0.1),ncol=4),
         
         "sigmab.L1"  = runif((p.xmu-1),0,2), 
         "sigmad.L1"  = runif((p.xsum-1),0,2), 
         "sigmab0.L1" = runif((p.x0-1),0,2), 
         
         "taub.ARD"  = runif((p.xmu-1),0,2), 
         "taud.ARD"  = runif((p.xsum-1),0,2), 
         "taub0.ARD" = runif((p.x0-1),0,2), 
         
         "taub.L2"  = runif(1,0,2), 
         "taud.L2"  = runif(1,0,2),
         "taub0.L2" = runif(1,0,2),
         
         "sigma1" = runif(1,0.25,1),
         "scale2" = runif(1,0.25,1),
         
         .RNG.name = rngname, 
         .RNG.seed = rngseed)}    
  
  set.seed(seed[1]); inits.internal <- list(init("base::Super-Duper", seed[1]));
  if(n.chain >= 2) {
    for(j in 2:n.chain){ 
      set.seed(seed[j]); 
      inits.internal <- c(inits.internal,list(init("base::Wichmann-Hill",seed[j])))}}  
}

  if(!is.null(inits)){
    
  for(i in 1:n.chain){
    
    if(!is.null(inits[[i]]$b)) {
      inits.internal[[i]][[1]] <- inits[[i]]$b[1]
      if(p.xmu>=2) inits.internal[[i]][[4]] <- matrix(rep(inits[[i]]$b[2:p.xmu],4), 
                                         ncol=4, byrow=FALSE)}
    if(!is.null(inits[[i]]$d)) {
      inits.internal[[i]][[2]] <- inits[[i]]$d[1]
      if(p.xsum>=2) inits.internal[[i]][[5]] <- matrix(rep(inits[[i]]$d[2:p.xsum],4), 
                                         ncol=4, byrow=FALSE)}
    if(!is.null(inits[[i]]$b0)) {
      inits.internal[[i]][[3]] <- inits[[i]]$b0[1]
      if(p.x0>=2) inits.internal[[i]][[6]] <- matrix(rep(inits[[i]]$b0[2:p.x0],4), 
                                         ncol=4, byrow=FALSE)}

    if(!is.null(inits[[i]]$sigma)) {
      inits.internal[[i]][[16]]<- inits[[i]]$sigma
      inits.internal[[i]][[17]]<- inits[[i]]$sigma
    }
  }}
  op<- system.file("bugs", "sep_1z0.bug",package="zoib") 
  model <- jags.model(op,data=dataIn,n.adapt=0,inits=inits.internal,n.chains=n.chain)  
  return(model)
}
