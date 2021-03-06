fixed1 <-
function(y, n, xmu.1,p.xmu, xsum.1,p.xsum, x1.1,p.x1,prior1, prec.int,
         prec.DN, lambda.L1, lambda.L2, lambda.ARD, link, n.chain, inits, seed)
{  
  dataIn <- vector("list",12)  
  dataIn.name <- c("y","xmu.1","p.xmu","xsum.1","p.xsum","x1.1",
                   "p.x1","n","zero","prior1","hyper","link")
  names(dataIn)<- dataIn.name  
  dataIn[[1]]  <- y
  dataIn[[2]]  <- as.matrix(xmu.1,nrow=n,byrow=T)
  dataIn[[3]]  <- p.xmu
  dataIn[[4]]  <- as.matrix(xsum.1,nrow=n,byrow=T)
  dataIn[[5]]  <- p.xsum      
  dataIn[[6]]  <- as.matrix(x1.1,nrow=n,byrow=T)
  dataIn[[7]]  <- p.x1
  dataIn[[8]]  <- n
  dataIn[[9]]  <- rep(0,n)     
  dataIn[[10]] <- prior1
  dataIn[[11]] <- as.matrix(cbind(prec.int,prec.DN,lambda.L1,lambda.L2,lambda.ARD))
  dataIn[[12]] <- link

  if(is.null(seed)){
    init <- function(rngname, rngseed ){
      list("tmp1" = rnorm(1,0,0.1),
           "tmp2" = rnorm(1,0,0.1),
           "tmp3" = rnorm(1,0,0.1),
           
           "b.tmp" = matrix(rnorm((p.xmu-1)*4,0,0.1),ncol=4),
           "d.tmp" = matrix(rnorm((p.xsum-1)*4,0,0.1),ncol=4),
           "b1.tmp"= matrix(rnorm((p.x1-1)*4,0,0.1),ncol=4),
           
           "sigmab.L1" = runif((p.xmu-1),0,2), 
           "sigmad.L1" = runif((p.xsum-1),0,2), 
           "sigmab1.L1"= runif((p.x1-1),0,2), 
           
           "taub.ARD" = runif((p.xmu-1),0,2), 
           "taud.ARD" = runif((p.xsum-1),0,2), 
           "taub1.ARD"= runif((p.x1-1),0,2), 
           "taub.L2"  = runif(1,0,2), 
           "taud.L2"  = runif(1,0,2),
           "taub1.L2" = runif(1,0,2))}
    
      inits.internal <- list(init( ));
      if(n.chain >= 2) {
        for(j in 2:n.chain) inits.internal <- c(inits.internal,list(init()))} 
    } else{
  init <- function(rngname, rngseed){
    list("tmp1" = rnorm(1,0,0.1),
         "tmp2" = rnorm(1,0,0.1),
         "tmp3" = rnorm(1,0,0.1),
         
         "b.tmp" = matrix(rnorm((p.xmu-1)*4,0,0.1),ncol=4),
         "d.tmp" = matrix(rnorm((p.xsum-1)*4,0,0.1),ncol=4),
         "b1.tmp"= matrix(rnorm((p.x1-1)*4,0,0.1),ncol=4),
         
         "sigmab.L1" = runif((p.xmu-1),0,2), 
         "sigmad.L1" = runif((p.xsum-1),0,2), 
         "sigmab1.L1"= runif((p.x1-1),0,2), 
         
         "taub.ARD" = runif((p.xmu-1),0,2), 
         "taud.ARD" = runif((p.xsum-1),0,2), 
         "taub1.ARD"= runif((p.x1-1),0,2), 
         "taub.L2"  = runif(1,0,2), 
         "taud.L2"  = runif(1,0,2),
         "taub1.L2" = runif(1,0,2),
         
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

    if(!is.null(inits[[i]]$b1)) {
      inits.internal[[i]][[3]] <- inits[[i]]$b1[1]
      if(p.x1>=2) inits.internal[[i]][[6]] <- matrix(rep(inits[[i]]$b1[2:p.x1],4), 
                                         ncol=4, byrow=FALSE)}
  }  }
  op<- system.file("bugs", "fixed1.bug", package="zoib") 
  model <- jags.model(op, data=dataIn, n.adapt=0, inits=inits.internal,n.chains=n.chain)        
  return(model)
}
