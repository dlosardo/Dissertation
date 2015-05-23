#checking the modulus of a VARMA process with two factors only
StatCheck = function(Trans){
	N2=-Trans[1,1]+-Trans[2,2]
	N3=-Trans[1,1]*-Trans[2,2]-(Trans[1,2]*Trans[2,1])
	roots = Mod(polyroot(c(1,N2,N3)))
	return(roots)
}

#writing function for 'vec' operator (stacking columns of matrices)
vecop = function(M){
	sM = NULL
	for (i in 1:ncol(M)){
		Mtemp = M[,i]
		sM = c(sM,Mtemp)
		}
	return(sM)
}

#writing function for oppposite of 'vec' operator (making a matrix out of a stack of columns)
vec2mat = function(V,n){
sV = NULL
for (i in 1:(length(V)/n)){
	Vtemp = V[(1+(i-1)*n):(i*n)]
	sV = rbind(sV,Vtemp)
	}
return(sV)
}

#function for plots - number of time points, 1st i value, last i value, name where data is located, column where data is located,
#standardized?, y-axis label, x-axis label, title, lower limit for y-axis, upper limit for y-axis
plotsF = function(nt,snp,enp,varName,ncolN,stand=FALSE,yname=NULL,xname=NULL,title=NULL,lowerY=NULL,upperY=NULL){
	if (is.matrix(varName))plot(1:nt,varName[1:nt,ncolN],type="n",ylab=yname,xlab=xname,main=title,ylim=c(lowerY,upperY))
	if (is.vector(varName))plot(1:nt,varName[1:nt],type="n",ylab=yname,xlab=xname,main=title,ylim=c(lowerY,upperY))
	for (i in snp:enp){
		if (is.matrix(varName))ytemp = (varName[(1+(i-1)*nt):(i*nt),ncolN])
		if (is.vector(varName))ytemp = (varName[(1+(i-1)*nt)])
		if (stand) ytemp = (ytemp - mean(ytemp,na.rm=T))/sd(ytemp,na.rm=T)
		lines(1:nt,ytemp,lwd=1)
	}
}
#Functions
#Bias function
Bias = function(x,truex){
	mean(x, na.rm=T) - truex
	}
#Relative Bias function
relBias = function(x,truex){
	(mean(x, na.rm=T) - truex)/truex
	}
#CI coverage
lcl = function(x,xse){
	x-(1.96*xse)
	}
ucl = function(x,xse){
	x+(1.96*xse)
	}
cov = function(lcl,ucl,truex){
	if (ucl>truex && truex>lcl) 1
	else 0
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
	if (abs(x/xse) > 1.96) 1
	else 0 
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
#Mean
Mean = function(x){
	mean(x,na.rm=TRUE)
	}
#SD
SD = function(x){
	sd(x,na.rm=TRUE)
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
BIC= function(FML,nparm,N){
	(-2*FML)+nparm*log(N)
}
#AIC= function(FML,nparm,N){
#	(2*FML)+2*nparm
#}
AIC= function(FML,nparm){
	(-2*FML)+2*nparm
}
RMSEA=function(ll,nparm,N,ny,nt,S,nd){
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

outDET=function(var1){
	var1=as.numeric(var1)
	quant25=quantile(var1)[2]
	quant75=quantile(var1)[4]
	ll=quant25 - 1.5*IQR(var1)
	ul=quant75 + 1.5*IQR(var1)
	newVar=var1[var1<ul&var1>ll]
}

outDET1=function(var1){
	var1 = as.numeric(var1)
	quant25 = quantile(var1, na.rm = TRUE)[2]
	quant75 = quantile(var1, na.rm = TRUE)[4]
	ll=quant25 - 7*IQR(var1, na.rm = TRUE)
	ul=quant75 + 7*IQR(var1, na.rm = TRUE)
	which(var1 > ul | var1 < ll)
}


batchOX <-
function(oxdir,input,output,batchfile){
cat(paste(c("PATH ",oxdir,"\n
oxl \"",input,"\" > \"",output,"\"
")), 
file=batchfile, append=FALSE, sep="")
}