sep.1z <-
function(y, n, xmu.1, p.xmu, xsum.1, p.xsum, 
                   rid, EUID, nEU, prior1, prior2, prior.beta, prior.Sigma, 
                   prec.int, prec.DN, lambda.L1, lambda.L2,lambda.ARD,
                   scale.unif, scale.halft, link, n.chain, inits, seed) 
{ 
  dataIn <- vector("list",15)
  names(dataIn) <- c("n","y","xmu.1","p.xmu","xsum.1", "p.xsum",
                     "zero","link","hyper","prior1","prior2","rid",
                     "EUID","nEU","hyper2")

  dataIn[[1]] <- n      
  dataIn[[2]] <- y
  dataIn[[3]] <- as.matrix(xmu.1)
  dataIn[[4]] <- p.xmu
  dataIn[[5]] <- as.matrix(xsum.1)
  dataIn[[6]] <- p.xsum      
  dataIn[[7]] <- rep(0,n)
  dataIn[[8]] <- link
  dataIn[[9]] <- as.matrix(cbind(prec.int,prec.DN,lambda.L1,lambda.L2,lambda.ARD))    
  if(grepl("unif",prior.Sigma))  dataIn[[15]] <- scale.unif
  if(grepl("halfcauchy",prior.Sigma)) dataIn[[15]] <- scale.halft   
  dataIn[[10]] <- prior1
  dataIn[[11]] <- prior2    
  dataIn[[12]] <- rid 
  dataIn[[13]] <- EUID
  dataIn[[14]] <- nEU
 
  
  if(is.null(seed)){
    init <- function( rngname, rngseed){
      list("tmp1" = rnorm(1,0,0.1),
           "tmp2" = rnorm(1,0,0.1),
           
           "b.tmp" = matrix(rnorm((p.xmu-1)*4,0,0.1),ncol=4),
           "d.tmp" = matrix(rnorm((p.xsum-1)*4,0,0.1),ncol=4),
           
           "sigmab.L1" = runif((p.xmu-1),0,2), 
           "sigmad.L1" = runif((p.xsum-1),0,2), 
           
           "taub.ARD" = runif((p.xmu-1),0,2), 
           "taud.ARD" = runif((p.xsum-1),0,2), 
           
           "taub.L2" = runif(1,0,2), 
           "taud.L2" = runif(1,0,2),
           
           "sigma1" = runif(1,0.25,1),
           "scale2" = runif(1,0.25,1))}
      inits.internal <- list(init( ));
      if(n.chain >= 2) {
        for(j in 2:n.chain) inits.internal <- c(inits.internal,list(init()))} 
    } else{
      init <- function(rngname, rngseed ){
      list("tmp1" = rnorm(1,0,0.1),
         "tmp2" = rnorm(1,0,0.1),
         
         "b.tmp" = matrix(rnorm((p.xmu-1)*4,0,0.1),ncol=4),
         "d.tmp" = matrix(rnorm((p.xsum-1)*4,0,0.1),ncol=4),
         
         "sigmab.L1" = runif((p.xmu-1),0,2), 
         "sigmad.L1" = runif((p.xsum-1),0,2), 
 
         "taub.ARD" = runif((p.xmu-1),0,2), 
         "taud.ARD" = runif((p.xsum-1),0,2), 
         
         "taub.L2" = runif(1,0,2), 
         "taud.L2" = runif(1,0,2),
         
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
      if(p.xmu>=2) inits.internal[[i]][[3]] <- 
        matrix(rep(inits[[i]]$b[2:p.xmu],4),ncol=4, byrow=FALSE)}
    if(!is.null(inits[[i]]$d)) {
      inits.internal[[i]][[2]] <- inits[[i]]$d[1]
      if(p.xsum>=2) inits.internal[[i]][[4]] <- 
        matrix(rep(inits[[i]]$d[2:p.xsum],4), ncol=4, byrow=FALSE)}    
    if(!is.null(inits[[i]]$sigma)) {
      inits.internal[[i]][[11]]<- inits[[i]]$sigma
      inits.internal[[i]][[12]]<- inits[[i]]$sigma}
  }}
  op<- system.file("bugs", "sep_1z.bug",package="zoib") 
   model <- jags.model(op, data=dataIn,n.adapt=0,inits=inits.internal,n.chains=n.chain)  
  return(model)
}
