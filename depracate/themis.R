############ I need to generate some data to play around and then import them in Matlabl 
############ to check code

###### 
set.seed(1);

#T=5000;
#p=2;
#n=2;
#k=1;
#A1=matrix(c(0.2,0.5,0,0.2),2,2);
#A2=matrix(c(0.1,-.3,0.2,0.1),2,2);
#Y=matrix(0,nrow=T,ncol=2);
#B=matrix(c(-0.592,0.592,0.806,0.806),2,2);
#epsilon=cbind(e1=rnorm(T),e2=rnorm(T));
#for(i in (p+1):T){
#  Y[i,]=A1%*%Y[i-1,]+A2%*%Y[i-2,]+B%*%epsilon[i,];
#}
#m=5*epsilon[,1]+rnorm(T);
#colnames(Y)=c("Y1","Y2");
#write.table(cbind(Y,m),file="artificial.txt",sep="\t",row.names=F);


##########################################
# Estimate reduced form VAR

doProxySVAR_single=function(Y,m,p,irhor){
  n=ncol(Y);
  T=nrow(Y);
  VARY=embed(Y,dimension = p+1);
  Ahat=solve(crossprod(cbind(1,VARY[,-c(1:n)]))) %*% crossprod(cbind(1,VARY[,-c(1:n)]), VARY[,c(1:n)]); 
  #A1 top block; A2 bottom... Equivalently, first column coefs for first regression, second...
  #- OK working! check against library(vars), VAR(Y,p=2,"none")
  VARRES=VARY[,1:n]-cbind(1,VARY[,-c(1:n)])%*%Ahat;
  Sigmau <-  crossprod(VARRES) / (T - p - n * p - 1);
  k=NCOL(m);
  
  # Identification
  Phib=solve(crossprod(cbind(1,m[-c(1:p)]))) %*% crossprod(cbind(1,m[-c(1:p)]), VARRES); 
  Phib=Phib[-1,,drop=F];
  Phib11  = Phib[1:k,1:k,drop=F];
  Phib21  = Phib[1:k,(k+1):n,drop=F];
  b21ib11=t(solve(Phib11) %*% Phib21); # (Sigma_mu1^-1*Sigma_mu2)'
  Sig11   = Sigmau[1:k,1:k,drop=F];
  Sig21   = Sigmau[(k+1):n,1:k,drop=F];
  Sig22   = Sigmau[(k+1):n,(k+1):n,drop=F];
  ZZp     = b21ib11%*%Sig11%*%t(b21ib11)-(Sig21%*%t(b21ib11)+b21ib11%*%t(Sig21))+Sig22;
  
  b12b12p = t(Sig21- b21ib11%*%Sig11)%*%solve(ZZp)%*%(Sig21- b21ib11%*%Sig11);
  b11b11p = Sig11-b12b12p;
  b11 = sqrt(b11b11p);
  b1 = as.matrix(rbind(b11,b21ib11%*%b11)); ## OK Identification Working!!
  
  ### Skip Reliability for Now!!
  
  ######### Impulse Responses
  irs=matrix(0,irhor+p,n);
  irs[p+1,] = b1[,1]/b1[1,1]; # so on impact Y1 increase by -1, like in Evi's paper
  for(jj in 2:irhor){
    lvars=as.vector(t(irs[(p+jj-1):jj,]));
    irs[p+jj,]=lvars%*%Ahat[2:(p*n+1),,drop=F];
  }
  return(VAR=list(b1=b1,Sigmau=Sigmau,p=p,varres=VARRES,irs=irs[-c(1:p),],Ahat=Ahat));
}

# input the output of doProxySVAR_single()
doProxySVARbootstrap_single=function(psvar.output,irhor,Y,m,nboot,clevel){
  mres=apply(psvar.output$varres, 2, function(y) y - mean(y)); # demean by column - wb not needed but MR have it
  T=nrow(mres);
  n=ncol(mres);
  p=psvar.output$p;
  wboot.irs=array(0,dim=c(irhor,n,nboot));
  varsb=matrix(0,p+T,n);
  varsb[1:p,]=Y[1:p,]; # initial values
  for(jj in 1:nboot){
    print(jj);
    wb=sample(size=T,c(-1,1),replace=T);
    resb=mres*wb;
    for(j in (p+1):(p+T)){
      lvars=as.vector(t(varsb[(j-1):(j-p),]));
      varsb[j,] = lvars%*%psvar.output$Ahat[2:(p*n+1),]+psvar.output$Ahat[1,]+resb[(j-p),];     
    }
    # Frow here onwards needs to be adjusted for k>1.
    mprox=c(m[1:p],m[-c(1:p)]*wb); 
    
    VARBS=doProxySVAR_single(Y=varsb,m=mprox,p=p,irhor=irhor);
    
    wboot.irs[,,jj]=VARBS$irs;
    
  }
  return(list(wboot.irs,varsb));
}


#par(mfrow=c(1,2))
#plot.ts(irs[-c(1:2),2])
#plot.ts(irs[-c(1:2),1])