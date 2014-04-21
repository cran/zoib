zoib <-
function( 
  model, 
  data,
  zero.inflation = TRUE,
  one.inflation = TRUE,

  joint = TRUE, 
  random = 0,
  EUID,
  
  link.mu="logit", 
  link.x0="logit", 
  link.x1="logit",    

  prior.beta = rep("DN",4),  
  prec.int   = 1e-3,
  prec.DN    = 1e-3,
  lambda.L2  = 1e-3,
  lambda.L1  = 1e-3,
  lambda.ARD = 1e-3,
    
  prior.Sigma = "VC.halft", 
  scale.unif = 20, 
  scale.halft = 20,
  
  n.chain = 2,
  n.iter = 5000,
  n.thin = 2
)
                    
{ 
  cl <- match.call()
  if(missing(data)) data <- environment(model)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("model", "data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  
  formula  <- as.Formula(model)
  nterm <- length(formula)
  mf$formula <- formula
  mod <- model.frame(formula,data=data)  
  
  y <- NULL
  for(i in 1:nterm[1L]){
    y <- cbind(y, as.matrix(model.part(formula,data=mod,lhs=i)) )
  } 
  
  x1 <- NULL; x0 <- NULL;
  if(nterm[2L]==5L){
    xmu <- as.matrix(model.matrix(formula,data=mod,rhs=1))
    xsum <- as.matrix(model.matrix(formula,data=mod,rhs=2))
    x0 <- as.matrix(model.matrix(formula,data=mod,rhs=3))
    x1 <- as.matrix(model.matrix(formula,data=mod,rhs=4))
    
    z <- as.matrix(model.matrix(formula,data=mod,rhs=5)) 
    zname <- c("int",colnames(z))
    Fc <- as.character(formula)
    chai <- strsplit(Fc[3], " ")
    bar.pos <- which(chai[[1]]=="|")
    rand.part <- chai[[1]][(bar.pos[4]+1):length(chai[[1]])]
    zname <- rand.part[which(rand.part!="+")]
    if(all(zname=='1')) zname='int'
    else zname <- c("int",zname)            
  }
  else if(nterm[2L]==4L){
    xmu <- as.matrix(model.matrix(formula,data=mod,rhs=1))
    xsum <- as.matrix(model.matrix(formula,data=mod,rhs=2))
    if(random==0){
      x0 <- as.matrix(model.matrix(formula,data=mod,rhs=3))
      x1 <- as.matrix(model.matrix(formula,data=mod,rhs=4))
    }
    else{
      if(any(one.inflation) & all(!zero.inflation))
        x1 <- as.matrix(model.matrix(formula,data=mod,rhs=3))
      else if(any(zero.inflation) & !all(one.inflation))
        x0 <- as.matrix(model.matrix(formula,data=mod,rhs=3))
                        
      z <- as.matrix(model.matrix(formula,data=mod,rhs=4))
      zname <- c("int",colnames(z))
      Fc <- as.character(formula)
      chai <- strsplit(Fc[3], " ")
      bar.pos <- which(chai[[1]]=="|")
      rand.part <- chai[[1]][(bar.pos[3]+1):length(chai[[1]])]
      zname <- rand.part[which(rand.part!="+")]
      if(all(zname=='1')) zname='int'
      else zname <- c("int",zname)                       
    }
  }
  else if(nterm[2L]==3L){    
    if(random!=0){
      xmu <- as.matrix(model.matrix(formula,data=mod,rhs=1))
      xsum <- as.matrix(model.matrix(formula,data=mod,rhs=2))
      
      Fc <- as.character(formula)
      chai <- strsplit(Fc[3], " ")
      bar.pos <- which(chai[[1]]=="|")
      rand.part <- chai[[1]][(bar.pos[2]+1):length(chai[[1]])]
      zname <- rand.part[which(rand.part!="+")]
      if(all(zname=='1')) zname='int'
      else zname <- c("int",zname)
    }
    else{
      if(any(one.inflation) & all(!zero.inflation)){
        xmu <- as.matrix(model.matrix(formula,data=mod,rhs=1))
        xsum <- as.matrix(model.matrix(formula,data=mod,rhs=2))
        x1 <- as.matrix(model.matrix(formula,data=mod,rhs=3))
      }
      else if(any(zero.inflation) & all(!one.inflation)){
        xmu <- as.matrix(model.matrix(formula,data=mod,rhs=1))
        xsum <- as.matrix(model.matrix(formula,data=mod,rhs=2))
        x0 <- as.matrix(model.matrix(formula,data=mod,rhs=3))
      }   
    }
  }
  else if(nterm[2L]==2L){    
    xmu <- as.matrix(model.matrix(formula,data=mod,rhs=1))
    xsum <- as.matrix(model.matrix(formula,data=mod,rhs=2))
  }
  else if(nterm[2L]<2L){    
    warning("RHS should have at least two parts; intercept only model is used to")
    warning("model the sum of the two shape parameters of the beta distribution") 
      xmu <- as.matrix(model.matrix(formula,data=mod,rhs=1))
      xsum <- as.matrix(rep(1,nrow(xsum)))
  }

  # link choice
  link <- matrix(0,3,3)
  
  if(link.mu=="logit") link[1,1]<- 1
  else if(link.mu=="cloglog") link[1,2]<- 1
  else if(link.mu=="probit") link[1,3]<- 1
  
  if(link.x0=="logit") link[2,1]<- 1
  else if(link.x0=="cloglog") link[2,2]<- 1
  else if(link.x0=="probit") link[2,3]<- 1
  
  if(link.x1=="logit") link[3,1]<- 1
  else if(link.x1=="cloglog") link[3,2]<- 1
  else if(link.x1=="probit") link[3,3]<- 1

  # prior.beta choice
  prior1 <- matrix(0,4,4)
  for(i in 1:4){ 
    if(prior.beta[i]=="DN" | prior.beta[i]=="D")     prior1[i,1]<-1
    else if(prior.beta[i]=="L1")                     prior1[i,2]<-1
    else if(prior.beta[i]=="L2")                     prior1[i,3]<-1
    else if(prior.beta[i]=="ARD"|prior.beta[i]=="A") prior1[i,4]<-1
  }
  # prior.Sigma choice 
  prior2 <- matrix(0,2,2)
  if(prior.Sigma==c("VC.unif"))       prior2[1,1]<-1
  else if(prior.Sigma==c("VC.halft")) prior2[1,2]<-1
  else if(prior.Sigma==c("UN.unif"))  prior2[2,1]<-1
  else if(prior.Sigma==c("UN.halft")) prior2[2,2]<-1
  
  
  # random effect choice
  rid=rep(0,4)
  if(any(random==c(1,12,13,14,123,124,134,1234))) rid[1]<-1
  if(any(random==c(2,12,23,24,123,124,234,1234))) rid[2]<-1
  if(any(random==c(3,13,23,34,123,134,234,1234))) rid[3]<-1
  if(any(random==c(4,14,24,34,124,134,234,1234))) rid[4]<-1  
   
  if(is.null(ncol(y))) y<-as.matrix(y,ncol=1)
  n <- nrow(y)
  q <- ncol(y)
  
 
  # x-mu
  xmu.1 <- xmu;  p.xmu <- ncol(xmu.1)  
  if(p.xmu >1){
    mean.mu <- apply(as.data.frame(xmu.1[,2:p.xmu]),2,mean)
    sd.mu   <- apply(as.data.frame(xmu.1[,2:p.xmu]),2,sd)
    for(i in 2:p.xmu) xmu.1[,i] <- (xmu.1[,i]-mean.mu[i-1])/sd.mu[i-1]
  }
    
  # x-sum
  xsum.1 <- xsum;  p.xsum <- ncol(xsum)
  if(p.xsum>1) {
    mean.sum <- apply(as.data.frame(xsum.1[,2:p.xsum]),2,mean)
    sd.sum   <- apply(as.data.frame(xsum.1[,2:p.xsum]),2,sd)
    for(i in 2:p.xsum) xsum.1[,i] <- (xsum.1[,i]-mean.sum[i-1])/sd.sum[i-1]
  }
  
  # x0
  if(!is.null(x0)){
    x0.1 <- x0;  p.x0 <- ncol(x0.1)   
    if(p.x0>1) {
      mean0<- apply(as.data.frame(x0.1[,2:p.x0]),2,mean)
      sd0  <- apply(as.data.frame(x0.1[,2:p.x0]),2,sd)
      for(i in 2:p.x0) x0.1[,i] <- (x0.1[,i]-mean0[i-1])/sd0[i-1]
    }
  }
  
  # x1
  if(!is.null(x1)){
    x1.1 <- x1;  p.x1 <-ncol(x1.1)      
    if(p.x1>1) { 
      mean1<- apply(as.data.frame(x1.1[,2:p.x1]),2,mean)
      sd1  <- apply(as.data.frame(x1.1[,2:p.x1]),2,sd)
      for(i in 2:p.x1) x1.1[,i] <- (x1.1[,i]-mean1[i-1])/sd1[i-1]
    }
  }
  
  print("***************************************************************************") 
  print("* List of parameter for which the posterior samples are generated         *")
  print("* b: regression coeff in the linear predictor for the mean of beta dist'n *")
  print("* d: regression coeff in the linear predictor for the sum of the two shape*")
  print("*    parameters in the beta distribution                                  *")
  print("* b0: regression coeff in the linear predictor for Prob(y=0)              *")
  print("* b1: regression coeff in the linear predictor for Prob(y=1)              *")   
  print("***************************************************************************")
  
  ################################################################
  #  1- 4: fixed  effects model
  ################################################################
  if(random==0) 
  {
    model <- vector("list", q)  
    post.samples <- vector("list", q)  
    for(i in 1:q)
    {
      if(one.inflation[i] & zero.inflation[i] ){
        model[[i]]<- fixed01(y[,i], n, xmu.1, p.xmu, xsum.1, p.xsum, x0.1, p.x0, x1.1, p.x1,
                             prior1, prec.int, prec.DN,lambda.L1,lambda.L2,lambda.ARD,link,n.chain)                           
        para.list <- c("b","d","b0","b1")}
      else if(zero.inflation[i]  & !one.inflation[i] ){      
        model[[i]]<- fixed0(y[,i],n, xmu.1, p.xmu, xsum.1, p.xsum, x0.1, p.x0, prior1, 
                            prec.int, prec.DN,lambda.L1,lambda.L2,lambda.ARD,link, n.chain)   
        para.list <- c("b","d","b0")}
      else if(one.inflation[i] & !zero.inflation[i] ){ 
        model[[i]]<- fixed1(y[,i],n, xmu.1, p.xmu, xsum.1, p.xsum, x1.1, p.x1,prior1,
                            prec.int, prec.DN,lambda.L1,lambda.L2,lambda.ARD, link, n.chain) 
        para.list <- c("b","d","b1")}
      else if(!one.inflation[i] & !zero.inflation[i] ){ 
        model[[i]]<-  fixed(y[,i],n, xmu.1, p.xmu, xsum.1, p.xsum, prior1,prec.int, 
                            prec.DN,lambda.L1,lambda.L2,lambda.ARD, link, n.chain)   
        para.list <- c("b","d")}

      print(para.list)
      post.samples[[i]]<- coda.samples(model[[i]], para.list, n.iter=n.iter, thin=n.thin) 

      dim.para  <- dim(post.samples[[i]][[1]]) 
      name.para <- colnames(post.samples[[i]][[1]])
      
      post.samples.raw <- post.samples
      
      for(k in 1:dim.para[2]){       
        if(grepl("b0",name.para[k])){
          if(p.x0 > 1){ 
            MEAN <- matrix(rep(mean0,dim.para[1]), nrow=dim.para[1], byrow=T)
            SD <- matrix(rep(sd0,dim.para[1]), nrow=dim.para[1], byrow=T)
            for(j in 1:n.chain) { 
              tmp <- post.samples[[i]][[j]]
              post.samples.raw[[i]][[j]][,k] <- tmp[,k]-apply(tmp[,(k+1):(k+p.x0-1)]*MEAN/SD,1,sum)
              post.samples.raw[[i]][[j]][,(k+1):(k+p.x0-1)] <- tmp[,(k+1):(k+p.x0-1)]/SD}}
        break}}
      for(k in 1:dim.para[2]){   
        if(grepl("b1",name.para[k])){
          if(p.x1 > 1){ 
            MEAN <- matrix(rep(mean1,dim.para[1]), nrow=dim.para[1], byrow=T)
            SD <- matrix(rep(sd1,dim.para[1]), nrow=dim.para[1], byrow=T)
            for(j in 1:n.chain) { 
              tmp <- post.samples[[i]][[j]]
              post.samples.raw[[i]][[j]][,k] <- tmp[,k]-apply(tmp[,(k+1):(k+p.x1-1)]*MEAN/SD,1,sum)
              post.samples.raw[[i]][[j]][,(k+1):(k+p.x1-1)] <- tmp[,(k+1):(k+p.x1-1)]/SD}}
        break}}
      for(k in 1:dim.para[2]){   
        if(grepl("b",name.para[k])){
          if(p.xmu > 1){ 
            MEAN <- matrix(rep(mean.mu,dim.para[1]), nrow=dim.para[1], byrow=T)
            SD <- matrix(rep(sd.mu,dim.para[1]), nrow=dim.para[1], byrow=T)
            for(j in 1:n.chain) { 
              tmp <- post.samples[[i]][[j]]
              post.samples.raw[[i]][[j]][,k] <- tmp[,k]-apply(tmp[,(k+1):(k+p.xmu-1)]*MEAN/SD,1,sum)
              post.samples.raw[[i]][[j]][,(k+1):(k+p.xmu-1)] <- tmp[,(k+1):(k+p.xmu-1)]/SD}}
        break}}  
      for(k in 1:dim.para[2]){   
        if(grepl("d",name.para[k])){
          if(p.xsum > 1){ 
            MEAN <- matrix(rep(mean.sum,dim.para[1]), nrow=dim.para[1], byrow=T)
            SD   <- matrix(rep(sd.sum,dim.para[1]),   nrow=dim.para[1], byrow=T)
            for(j in 1:n.chain) { 
              tmp <- post.samples[[i]][[j]]
              post.samples.raw[[i]][[j]][,k] <- tmp[,k]-apply(tmp[,(k+1):(k+p.xsum-1)]*MEAN/SD,1,sum)
              post.samples.raw[[i]][[j]][,(k+1):(k+p.xsum-1)] <- tmp[,(k+1):(k+p.xsum-1)]/SD}}
        break}}
    }
    if(q==1) {
      model<- model[[1]]; 
      post.samples<- post.samples[[1]]; 
      post.samples.raw<- post.samples.raw[[1]]}
    return(list(model=model, oripara=post.samples.raw))
  }  
  
  ########## random effect models  ############################
  # nz0: # of raw random variables 
  # m: a vector/list with nz0 element that contains the number 
  #    of levels for each random effect. sum(m) would give the 
  #    dimension of the random effects
  # qz: the number of the column of zdummy
  ############################################################# 
  else
  {
    # -----------------------------------------------
      
    EUID.m <- unique(EUID)     
    nEU <- length(EUID.m)
    EUID1 <- rep(0,n)
    for(i in 1:n){
      for(j in 1:nEU){
        if(EUID[i]==EUID.m[j]) {EUID1[i]<- j; break}
      }
    }  
   
    # ------------------------------------------------
    if(all(zname=='int')) nz0<- 1
    else 
    {
      nz0 <- length(zname)
      nms<- colnames(data)
      z<- rep(1,n)
      for(k1 in 1:nz0){ 
        for(k2 in 1:length(nms)) {
          if(zname[k1]== nms[k2]) { 
            dk2 <- data[,k2]
            names(dk2) <-nms[k2] 
            z <- data.frame(z, dk2); break }
        }
      }
  
      zuniq <- vector("list",nz0)
      m <- rep(1,nz0)
      for(i in 1:nz0){
        if(is.factor(z[,i])|is.character(z[,i])){ 
          zuniq[[i]] <- unique(z[,i]); 
          m[i] <- length(zuniq[[i]])
        }
        else{
          zuniq[[i]] <- z[,i]
        }
      }
      qz <- sum(m)
      zdummy <- matrix(0,n,qz)
      
      id<-  0      
      for(j in 2:nz0)
      {  
        id <- id+m[j-1] 
        if(is.factor(z[,j])|is.character(z[,j])){    
          for(i in 1:nrow(z)){
            for(k in 1:m[j])
            {
              if(z[i,j]==zuniq[[j]][k])  {zdummy[i,id+k]<- 1; break}
            } }}
        else{ 
          zdummy[,id+1] <- (z[,j]-mean(z[,j]))/sd(z[,j])}
      }

      meanz<- apply(as.data.frame(zdummy[,2:qz]),2,mean)
      sdz  <- apply(as.data.frame(zdummy[,2:qz]),2,sd) 
      }
  }    
   
    ########################################################
    # 5-12: random, separate modeling
    ########################################################
    # q: # of y variables
    # z: random variables, before dummy coding
    # EUID: ID of the experimental units of all rows in the data sets
    # nEU: # of independent experimental units.
    # nz0: # of raw random variables 
    # qz:  # of zdummy variable
    # rid: a vector of 4, if random effects in mu, then the 1st element in rid =1; if sum, then 2nd =1
    # so on so forth.
    
    if(!joint)
    {    
      model <- vector("list", q) 
      post.samples <- vector("list", q) 
      for(i in 1:q)
      {
      
        if(nz0>1){ 
          if(one.inflation[i] & zero.inflation[i]){ 
            model[[i]]<- sep.2z01(y[,i],n, xmu.1, p.xmu, xsum.1,p.xsum, x0.1,p.x0, x1.1,p.x1, 
                                  zdummy,qz,nz0,m, rid,  nEU, prior1, prior2, prior.beta, 
                                  prior.Sigma, prec.int, prior1, prec.DN, lambda.L1, lambda.L2, 
                                  lambda.ARD, scale.unif, scale.halft,link, n.chain) 
            para.list <- c("b","d","b0","b1","Sigma")}
          else if(!one.inflation[i] & zero.inflation[i]){ 
            model[[i]]<- sep.2z0(y[,i],n, xmu.1, p.xmu, xsum.1, p.xsum, x0.1, p.x0, zdummy, 
                                 qz,nz0, m, rid,  nEU, prior1, prior2, prior.beta, prior.Sigma,
                                 prec.int, prior1, prec.DN, lambda.L1, lambda.L2, lambda.ARD,
                                 scale.unif, scale.halft,link, n.chain)
            para.list <- c("b","d", "b0","Sigma")}
          else  if(one.inflation[i] & !zero.inflation[i]){ 
            model[[i]]<- sep.2z1(y[,i], n, xmu.1, p.xmu, xsum.1, p.xsum, x1.1, p.x1, zdummy, 
                                 qz,nz0, m, rid, EUID1, nEU, prior1, prior2, prior.beta, 
                                 prior.Sigma, prec.int, prec.DN, lambda.L1, lambda.L2, 
                                 lambda.ARD, scale.unif, scale.halft,link, n.chain)
            para.list <- c("b","d", "b1","Sigma")}
          else if(!one.inflation[i] & !zero.inflation[i]){ 
            model[[i]]<- sep.2z(y[,i], n, xmu.1, p.xmu, xsum.1, p.xsum, zdummy, qz,nz0, m,
                                rid, EUID1, nEU, prior1, prior2, prior.beta, prior.Sigma, 
                                prec.int, prec.DN, lambda.L1, lambda.L2, lambda.ARD,
                                scale.unif, scale.halft,link, n.chain) 
            para.list <- c("b","d", "Sigma")}
          post.samples[[i]]<- coda.samples(model[[i]], para.list, thin=n.thin, n.iter=n.iter)
        }
        else   
        { 
          if(one.inflation[i] & zero.inflation[i]){ 
            model[[i]]<- sep.1z01(y[,i], n, xmu.1, p.xmu, xsum.1, p.xsum, x0.1, p.x0, x1.1, p.x1, 
                                  rid, EUID1, nEU, prior1, prior2, prior.beta, prior.Sigma,
                                  prec.int, prec.DN, lambda.L1, lambda.L2, lambda.ARD,
                                  scale.unif, scale.halft, link, n.chain) 
            para.list <- c("b","d", "b0","b1","sigma") }
          else if(one.inflation[i] & !zero.inflation[i]){ 
            model[[i]]<- sep.1z1(y[,i], n, xmu.1, p.xmu, xsum.1, p.xsum, x1.1, p.x1,
                                 rid, EUID1, nEU, prior1, prior2, prior.beta, prior.Sigma,
                                 prec.int, prec.DN, lambda.L1, lambda.L2, lambda.ARD,
                                 scale.unif, scale.halft, link, n.chain) 
            para.list <- c("b","d", "b1","sigma")  }
          else if(!one.inflation[i] & zero.inflation[i]){  
            model[[i]]<- sep.1z0(y[,i], n, xmu.1, p.xmu, xsum.1, p.xsum, x0.1, p.x0,
                                 rid, EUID1, nEU, prior1, prior2, prior.beta, prior.Sigma,
                                 prec.int, prec.DN, lambda.L1, lambda.L2, lambda.ARD,
                                 scale.unif, scale.halft, link, n.chain) 
            para.list <- c("b","d", "b0","sigma")}
          else if(!one.inflation[i] & !zero.inflation[i]) { 
            model[[i]]<- sep.1z(y[,i], n, xmu.1, p.xmu, xsum.1, p.xsum, rid,EUID1,
                                nEU, prior1, prior2, prior.beta, prior.Sigma,
                                prec.int, prec.DN, lambda.L1, lambda.L2, lambda.ARD,  
                                scale.unif, scale.halft, link, n.chain)
            para.list <- c("b","d", "sigma")}
          post.samples[[i]]<- coda.samples(model[[i]], para.list, thin=n.thin, n.iter=n.iter)
        }
        
        print(para.list)
        dim.para  <- dim(post.samples[[i]][[1]]) 
        name.para <- colnames(post.samples[[i]][[1]])
        post.samples.raw <- post.samples
        
        for(k in 1:dim.para[2]){       
          if(grepl("b0",name.para[k])){
            if(p.x0 > 1){ 
              MEAN <-  matrix(rep(mean0,dim.para[1]), nrow=dim.para[1], byrow=T)
              SD <-  matrix(rep(sd0,dim.para[1]), nrow=dim.para[1], byrow=T)
              for(j in 1:n.chain) { 
                tmp <- post.samples[[i]][[j]]
                post.samples.raw[[i]][[j]][,k] <- tmp[,k]-apply(tmp[,(k+1):(k+p.x0-1)]*MEAN/SD,1,sum)
                post.samples.raw[[i]][[j]][,(k+1):(k+p.x0-1)] <- tmp[,(k+1):(k+p.x0-1)]/SD}}
          break}}
        for(k in 1:dim.para[2]){   
          if(grepl("b1",name.para[k])){
            if(p.x1 > 1){ 
              MEAN <-  matrix(rep(mean1,dim.para[1]), nrow=dim.para[1], byrow=T)
              SD <-  matrix(rep(sd1,dim.para[1]), nrow=dim.para[1], byrow=T)
              for(j in 1:n.chain) { 
                tmp <- post.samples[[i]][[j]]
                post.samples.raw[[i]][[j]][,k] <- tmp[,k]-apply(tmp[,(k+1):(k+p.x1-1)]*MEAN/SD,1,sum)
                post.samples.raw[[i]][[j]][,(k+1):(k+p.x1-1)] <- tmp[,(k+1):(k+p.x1-1)]/SD}}
          break}}
        for(k in 1:dim.para[2]){   
          if(grepl("b",name.para[k])){
            if(p.xmu > 1){ 
              MEAN <-  matrix(rep(mean.mu,dim.para[1]), nrow=dim.para[1], byrow=T)
              SD <-  matrix(rep(sd.mu,dim.para[1]), nrow=dim.para[1], byrow=T)
              for(j in 1:n.chain) { 
                tmp <- post.samples[[i]][[j]]
                post.samples.raw[[i]][[j]][,k] <- tmp[,k]-apply(tmp[,(k+1):(k+p.xmu-1)]*MEAN/SD,1,sum)
                post.samples.raw[[i]][[j]][,(k+1):(k+p.xmu-1)] <- tmp[,(k+1):(k+p.xmu-1)]/SD}}
          break}}    
        for(k in 1:dim.para[2]){   
          if(grepl("d",name.para[k])){
            if(p.xsum > 1){ 
              MEAN <-  matrix(rep(mean.sum,dim.para[1]), nrow=dim.para[1], byrow=T)
              SD <-  matrix(rep(sd.sum,dim.para[1]),   nrow=dim.para[1], byrow=T)
              for(j in 1:n.chain) { 
                tmp <- post.samples[[i]][[j]]
                post.samples.raw[[i]][[j]][,k] <- tmp[,k]-apply(tmp[,(k+1):(k+p.xsum-1)]*MEAN/SD,1,sum)
                post.samples.raw[[i]][[j]][,(k+1):(k+p.xsum-1)] <- tmp[,(k+1):(k+p.xsum-1)]/SD}}
          break}}
      }
      if(q==1) {
        model<- model[[1]]; 
        post.samples<- post.samples[[1]]; 
        post.samples.raw<- post.samples.raw[[1]]}
      return(list(model=model,  oripara=post.samples.raw))
    }
    ########################################################
    # 13-20: random, joint modeling
    ######################################################## 
    else
    {  
      if(nz0>1) 
      {     
        if(any(one.inflation) & any(zero.inflation)){ 
          inflate1 <- rep(0,q)
          inflate0 <- rep(0,q)
          for(j in 1:q){ 
            if(one.inflation[j]) inflate1[j]<- 1
            if(zero.inflation[j]) inflate0[j]<- 1}
          model<-  joint.2z01(y,n,q, xmu.1, p.xmu, xsum.1, p.xsum, x0.1, p.x0, x1.1, p.x1,
                              inflate0, inflate1, zdummy, qz,nz0,m, rid, EUID1, nEU, 
                              prior1, prior2, prior.beta, prior.Sigma, prec.int, prec.DN, 
                              lambda.L1, lambda.L2, lambda.ARD, scale.unif, scale.halft,
                              link, n.chain)
          para.list <- c("b","d", "b0","b1","Sigma")}
        else if(all(!one.inflation) & any(zero.inflation)){
          inflate0 <- rep(1,q)
          for(j in 1:q){ 
            if(zero.inflation[j]) inflate0[j]<- 1}
          model<- joint.2z0(y,n,q, xmu.1, p.xmu, xsum.1, p.xsum, x0.1, p.x0,inflate0,
                            zdummy, qz,nz0,m,rid, EUID1, nEU, 
                            prior1, prior2, prior.beta, prior.Sigma,
                            prec.int, prec.DN, lambda.L1, lambda.L2, lambda.ARD,
                            scale.unif, scale.halft,link, n.chain) 
          para.list <- c("b","d", "b0","Sigma")}
        else  if(any(one.inflation) & all(!zero.inflation)) { 
          inflate1 <- rep(0,q)
          for(j in 1:q){ 
            if(one.inflation[j]) inflate1[j]<- 1}
          model<- joint.2z1(y,n,q, xmu.1, p.xmu, xsum.1, p.xsum, x1.1, p.x1,inflate1,
                            zdummy,qz,nz0,m,rid, EUID1, nEU,
                            prior1, prior2, prior.beta, prior.Sigma,
                            prec.int, prec.DN, lambda.L1, lambda.L2, lambda.ARD,
                            scale.unif, scale.halft,link, n.chain) 
          para.list <- c("b","d","b1","Sigma")}
        else if(all(!one.inflation) & all(!zero.inflation)){ 
          model<- joint.2z(y,n,q, xmu.1, p.xmu, xsum.1, p.xsum,zdummy,qz,nz0,m,
                            rid, EUID1, nEU, prior1, prior2, prior.beta, prior.Sigma,
                            prec.int, prec.DN, lambda.L1, lambda.L2, lambda.ARD,
                            scale.unif, scale.halft,link, n.chain)
          para.list <- c("b","d","Sigma")}
      }
      else 
      {
        if(any(one.inflation) & any(zero.inflation)) { 
          inflate1 <- rep(0,q)
          inflate0 <- rep(0,q)
          for(j in 1:q){ 
            if(one.inflation[j]) inflate1[j]<- 1
            if(zero.inflation[j]) inflate0[j]<- 1}
          
          model<- joint.1z01(y,n,q, xmu.1, p.xmu, xsum.1, p.xsum, x0.1, p.x0, x1.1, p.x1, 
                             inflate0, inflate1, rid, EUID1, nEU, prior1, prior2, 
                             prior.beta, prior.Sigma,prec.int, prec.DN, lambda.L1,lambda.L2, 
                             lambda.ARD, scale.unif, scale.halft,link, n.chain)
          para.list <- c("b","d", "b0","b1","sigma")}
        else if(all(!one.inflation) & any(zero.inflation)) { 
          inflate0 <- rep(1,q)
          for(j in 1:q){ 
            if(zero.inflation[j]) inflate0[j]<- 1}
          
          model<- joint.1z0(y,n,q, xmu.1, p.xmu, xsum.1, p.xsum, x0.1, p.x0, inflate0,
                            rid, EUID1, nEU, prior1, prior2, prior.beta, prior.Sigma,
                            prec.int,prec.DN, lambda.L1, lambda.L2, lambda.ARD,
                            scale.unif, scale.halft,link, n.chain) 
          para.list <- c("b","d", "b0","sigma")}
        else  if(any(one.inflation) & all(!zero.inflation)){ 
          inflate1 <- rep(0,q)
          for(j in 1:q){ 
            if(one.inflation[j]) inflate1[j]<- 1}
          
          model<- joint.1z1(y,n,q, xmu.1, p.xmu, xsum.1, p.xsum,x1.1, p.x1,inflate1,
                            rid, EUID1, nEU, prior1, prior2, prior.beta, prior.Sigma,
                            prec.int, prec.DN,lambda.L1, lambda.L2, lambda.ARD,
                            scale.unif, scale.halft,link, n.chain) 
          para.list <- c("b","d","b1","sigma")}
        else if(all(!one.inflation) & all(!zero.inflation)) { 
          model<- joint.1z(y,n,q, xmu.1, p.xmu, xsum.1, p.xsum, 
                            rid, EUID1, nEU, prior1, prior2, prior.beta, prior.Sigma,
                            prec.int, prec.DN,lambda.L1, lambda.L2, lambda.ARD, 
                            scale.unif, scale.halft,link, n.chain) 
          para.list <- c("b","d", "sigma")}
      }
      post.samples <- coda.samples(model, para.list, thin=n.thin, n.iter=n.iter)
      print(para.list)
      
      dim.para  <- dim(post.samples[[1]]) 
      name.para <- colnames(post.samples[[1]])
      post.samples.raw <- post.samples
      for(k in 1:dim.para[2]){       
        if(grepl("b0",name.para[k])){
          if(p.x0 > 1){ 
            MEAN <-  matrix(rep(mean0,dim.para[1]), nrow=dim.para[1], byrow=T)
            SD <-  matrix(rep(sd0,dim.para[1]), nrow=dim.para[1], byrow=T)
            for(j in 1:n.chain) { 
              tmp <- post.samples[[j]]
              post.samples.raw[[j]][,k] <- tmp[,k]-apply(tmp[,(k+1):(k+p.x0-1)]*MEAN/SD,1,sum)
              post.samples.raw[[j]][,(k+1):(k+p.x0-1)] <- tmp[,(k+1):(k+p.x0-1)]/SD}}
        break}}
      for(k in 1:dim.para[2]){   
        if(grepl("b1",name.para[k])){
          if(p.x1 > 1){ 
            MEAN <-  matrix(rep(mean1,dim.para[1]), nrow=dim.para[1], byrow=T)
            SD <- matrix(rep(sd1,dim.para[1]), nrow=dim.para[1], byrow=T)
            for(j in 1:n.chain) { 
              tmp <- post.samples[[j]]
              post.samples.raw[[j]][,k] <- tmp[,k]-apply(tmp[,(k+1):(k+p.x1-1)]*MEAN/SD,1,sum)
              post.samples.raw[[j]][,(k+1):(k+p.x1-1)] <- tmp[,(k+1):(k+p.x1-1)]/SD}}
        break}}
      for(k in 1:dim.para[2]){  
        if(grepl("b",name.para[k])){    
          if(p.xmu > 1){ 
            MEAN <-  matrix(rep(mean.mu,dim.para[1]), nrow=dim.para[1], byrow=T)
            SD <-  matrix(rep(sd.mu,dim.para[1]), nrow=dim.para[1], byrow=T)
            for(j in 1:n.chain) { 
              tmp <- post.samples[[j]]
              for(m in 1:q){
                s <- k+p.xmu*(m-1)
                post.samples.raw[[j]][,s] <- tmp[,s]-apply(tmp[,(s+1):(s+p.xmu-1)]*MEAN/SD,1,sum)
                post.samples.raw[[j]][,(s+1):(s+p.xmu-1)] <- tmp[,(s+1):(s+p.xmu-1)]/SD}}}
          break}}
      for(k in 1:dim.para[2]){   
        if(grepl("d",name.para[k])){
          if(p.xsum > 1){ 
            MEAN <-  matrix(rep(mean.sum,dim.para[1]), nrow=dim.para[1], byrow=T)
            SD  <-  matrix(rep(sd.sum,dim.para[1]),   nrow=dim.para[1], byrow=T)
            for(j in 1:n.chain) { 
              tmp <- post.samples[[j]]
              post.samples.raw[[j]][,k] <- tmp[,k]-apply(tmp[,(k+1):(k+p.xsum-1)]*MEAN/SD,1,sum)
              post.samples.raw[[j]][,(k+1):(k+p.xsum-1)] <- tmp[,(k+1):(k+p.xsum-1)]/SD}}
        break}}
      
      for(k in 1:dim.para[2]){ 
        if(grepl("Sigma",name.para[k])){ 
          L <- matrix(0,qz,qz)
          L[1,1] <- 1 
          for(m1 in 2:qz){
            L[1,m1] <- -meanz[m1-1]/sdz[m1-1]
            L[m1,m1] <- 1/sdz[m1-1]}
          for(j in 1:n.chain) { 
            tmp <- post.samples[[j]]
            for(m2 in 1:dim.para[1]){ 
              Sigma<- matrix(tmp[m2,k:(k+qz^2-1)],qz,qz)
              post.samples.raw[[j]][m2,k:(k+qz^2-1)]<- c(L%*%Sigma%*%t(L))}}
          break}}
      return(list(model=model, oripara=post.samples.raw)) 
    }

}