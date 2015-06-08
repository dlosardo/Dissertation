#Simulation Outcome Functions
#Mean
Mean = function(x){
  mean(x,na.rm=TRUE)
}
#SD
SD = function(x){
  sd(x,na.rm=TRUE)
}
#Bias function
Bias = function(x,truex){
  Mean(x) - truex
}
#Relative Bias function
relBias = function(x,truex){
  (Mean(x) - truex)/truex
}
#CI coverage
lcl = function(x,xse){
  x-(1.96*xse)
}
ucl = function(x,xse){
  x+(1.96*xse)
}
cov = function(lcl,ucl,truex){
  as.integer(ucl>truex & truex>lcl)
}
cov1 = function(lcl,ucl,truex){
  lcl=as.matrix(lcl)
  ucl=as.matrix(ucl)
  output=matrix(NA,nrow(lcl),1)
  for (p in 1:nrow(lcl)){
    if (ucl[p,]>truex && truex>lcl[p,]) {output[p,]=1} 	else {output[p,]=0} 
  }
  return(output)
}
#Power
pow = function(x,xse){
  as.integer(abs(x/xse) > 1.96)
}
pow1 = function(x,xse){
  x=as.matrix(x)
  xse=as.matrix(xse)
  output=matrix(NA,nrow(x),1)
  for (p in 1:nrow(x)){
    if (abs(x[p,]/xse[p,]) > 1.96) {output[p,]=1} else {output[p,]=0}  
  }
  return(output)
}
#RMSE
RMSE = function(x,truex){
  sqrt(Mean((x-truex)^2))
}
aRSDE = function(SEhat,SE){
  (SEhat-SE)/SE
}
CV = function(x,SE){
  SE/x
}
FML = function(ll,N){
  (N-1)*ll
}
#BIC= function(FML,nparm,N){
#	(2*FML)+nparm*log(N)
#}
BIC = function(FML,nparm,N){
  (-2*FML)+nparm*log(N)
}
#AIC= function(FML,nparm,N){
#	(2*FML)+2*nparm
#}
AIC = function(FML,nparm){
  (-2*FML)+2*nparm
}
RMSEA = function(ll,nparm,N,ny,nt,S,nd){
  k1=(ny*nt-nd)
  k=((k1*(k1+1))/2)+(ny*nt-nd)
  ll=-ll
  df=k-nparm
  K=log(2*pi)*(ny*nt-nd)
  neg2LL=2*ll
  Ffiml=neg2LL/N
  Fml1=Ffiml-K-log(det(cov(S)))-(ny*nt-nd)
  chi=Fml1*(N-1)
  out=sqrt((chi-df)/(df*(N-1)))
  return(out)
}
