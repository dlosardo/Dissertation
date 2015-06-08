# functions
outlier_check <- function(x){
  x = as.numeric(x)
  quant25 = quantile(x, na.rm = TRUE)[2]
  quant75 = quantile(x, na.rm = TRUE)[4]
  ll=quant25 - 7*IQR(x, na.rm = TRUE)
  ul=quant75 + 7*IQR(x, na.rm = TRUE)
  return(x > ul | x < ll)
}
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