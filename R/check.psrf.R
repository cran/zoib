my.mcmc <-
  function (data = NA, start = 1, end = numeric(0), thin = 1, 
            nom.start=start, nom.end=end) 
  {
    if (is.matrix(data)) {
      niter <- nrow(data)
      nvar <- ncol(data)
    }
    else if (is.data.frame(data)) {
      if (!all(sapply(data, is.numeric))) {
        stop("Data frame contains non-numeric values")
      }
      data <- as.matrix(data)
      niter <- nrow(data)
      nvar <- ncol(data)
    }
    else {
      niter <- length(data)
      nvar <- 1
    }
    thin <- round(thin)
    if (length(start) > 1) 
      stop("Invalid start")
    if (length(end) > 1) 
      stop("Invalid end")
    if (length(thin) != 1) 
      stop("Invalid thin")
    if (missing(end)) 
      end <- start + (niter - 1) * thin
    else if (missing(start)) 
      start <- end - (niter - 1) * thin
    nobs <- floor((end - start)/thin + 1)
    if (niter < nobs) 
      stop("Start, end and thin incompatible with data")
    else {
      end <- start + thin * (nobs - 1)
      if (nobs < niter) 
        data <- data[1:nobs, , drop = FALSE]
    }
    attr(data, "mcpar") <- c(nom.start, nom.end, thin)
    attr(data, "class") <- "mcmc"
    data
  }


check.psrf <-
function(post1=NULL, post2=NULL, post3=NULL, post4=NULL, post5=NULL)
{
  if(is.list(post1)){
    MCMC.list <- post1
  }
  else{
    tmp <-  list(post1,post2,post3,post4,post5)
    count <- 5
    if(is.null(post5)) count <- count-1
    if(is.null(post4)) count <- count-1
    if(is.null(post3)) count <- count-1  
    if(is.null(post2)) 
      stop("at last two Markov Chains are needed to compute psrf")
    
    draw <- vector("list", count)
    for(i in 1:length(tmp))
    {
      if(!is.null(tmp[[i]])) draw[[i]] <- mcmc(tmp[[i]])
      else break
    }
    MCMC.list <- mcmc.list(draw)
  }
  Nchain <- nchain(MCMC.list)
  Niter <- niter(MCMC.list)
  XXX <- as.list(1:Nchain)
  for(k in 1:Nchain){
    tmp <- MCMC.list[[k]]
    XXX[[k]]<- my.mcmc(tmp, thin=1, nom.start=1, nom.end=Niter)}
  x<- mcmc.list(XXX) 
 
  Nvar <- nvar(x)
  xnames <- varnames(x)
  x <- lapply(x, as.matrix)
  S2 <- array(sapply(x, var, simplify = TRUE), dim = c(Nvar, Nvar, Nchain))
  W <- apply(S2, c(1, 2), mean)
  PD <- is.positive.definite(W)    
  
  if(!PD)
  { 
    psrf.s <- gelman.diag(XXX, multivariate=FALSE)$psrf
    psrf.m <- NULL
    print("the covariance matrix of the posterior samples is not")  
    print("positive definite, and the multivarite psrf cannot be")
    print("computed") 
  }
  else{
    gelman.plot(XXX)
    psrf.s <- gelman.diag(XXX)[[1]]
    psrf.m <- gelman.diag(XXX)[[2]]
  }
  par(mfrow=c(1,2),mar=c(2,2,1,1)) 
  boxplot(psrf.s[,1]); mtext("psrf",1,cex=1.2)  
  boxplot(psrf.s[,2]); mtext("upper bound of 95% CI",1,cex=1.2)
 
  print(psrf.s); 
  print(psrf.m)
  return(list(psrf.s=psrf.s, psrf.m=psrf.m,
              psrf.s.summ = apply(psrf.s,2,summary))) 
}
