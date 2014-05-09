if(!AR1){

#nt=theTs[ss] 	# number of time points
#ne=2   	# number of states (LVs)
#ny=6	# number of observed 
nx=0    # number of fixed regressors
#np=sampleSizes[ss]	# number of subjects

if(!INIT){
npad=50 # start up
ist=npad+1   
ntt=nt+npad
}
if(INIT){
ntt=nt
ist=1
}

if (model==1|model==2){
# Z
Z=matrix(c(
1,0,
1.2,0,
.8,0,
0,1,
0,.9,
0,1.1
),ny,ne,byrow=T)
# V
V=matrix(c(
popValues[5],popValues[6],
popValues[6],popValues[7]
),ne,ne,byrow=T)
# T
T=matrix(c(
popValues[8],popValues[9],
popValues[10],popValues[11]),ne,ne,byrow=T)
# U
U=diag(c(popValues[12],popValues[13],popValues[14],
	popValues[15],popValues[16],popValues[17]))
# c
c=matrix(c(0,0),ne,1,byrow=T)
# d
#d=matrix(c(3.4,2.5,4.4,5.2,3,4),ny,1,byrow=T)
d=matrix(c(0,0,0,0,0,0),ny,1,byrow=T)
# states a t=1
if (trueI==1){
	I = diag(1,4)
	theta1 = solve(I-(T%x%T))%*%vecop(V)
	P0 = matrix(c(theta1[1],theta1[2],theta1[3],theta1[4]),ne,ne,byrow=T)
	a0=matrix(c(0,0),ne,1,byrow=T)	
	if (det(V)>0) {Vs=chol(V)} else {Vs=matrix(0,ne,ne)}
	if (det(U)>0) {Us=chol(U)} else {Us=matrix(0,ny,ny)}
	if (det(P0)>0) {P0s=chol(P0)} else {P0s=matrix(0,ne,ne)}
		}
if (trueI==3){
a0=matrix(c(0,0),ne,1,byrow=T)
P0=matrix(c(0,0,
			0,0),
			ne,ne,byrow=T)
# cholesky of V & U 
if (det(V)>0) {Vs=chol(V)} else {Vs=matrix(0,ne,ne)}
if (det(U)>0) {Us=chol(U)} else {Us=matrix(0,ny,ny)}
if (det(P0)>0) {P0s=chol(P0)} else {P0s=matrix(0,ne,ne)}
	}
if (trueI==2){
a0=matrix(c(1,.5),ne,1,byrow=T)
P0=matrix(c(1.2,.3,
			.3,.7),
			ne,ne,byrow=T)
# cholesky of V & U 
if (det(V)>0) {Vs=chol(V)} else {Vs=matrix(0,ne,ne)}
if (det(U)>0) {Us=chol(U)} else {Us=matrix(0,ny,ny)}
if (det(P0)>0) {P0s=chol(P0)} else {P0s=matrix(0,ne,ne)}
}
if (trueI==4){
	if (det(V)>0) {Vs=chol(V)} else {Vs=matrix(0,ne,ne)}
	if (det(U)>0) {Us=chol(U)} else {Us=matrix(0,ny,ny)}
	#if (det(P0)>0) {P0s=chol(P0)} else {P0s=matrix(0,ne,ne)}
	nt=50
	INIT=FALSE
	npad=0 # start up
	ist=npad+1   
	ntt=nt
	sdys=FALSE
	source(paste(source,"/Simulate.R",sep=""))
	#sdYall=NULL
	#for (i in 1:np){
	#	tempY1=yntonly[(1+(nt*(i-1))):(i*nt),1]
	#	tempY2=yntonly[(1+(nt*(i-1))):(i*nt),2]
	#	tempY3=yntonly[(1+(nt*(i-1))):(i*nt),3]
	#	tempY4=yntonly[(1+(nt*(i-1))):(i*nt),4]
	#	tempY5=yntonly[(1+(nt*(i-1))):(i*nt),5]
	#	tempY6=yntonly[(1+(nt*(i-1))):(i*nt),6]
		
	#	sdY1=(tempY1[50]-mean(tempY1))/sd(tempY1)
	#	sdY2=(tempY2[50]-mean(tempY2))/sd(tempY2)
	#	sdY3=(tempY3[50]-mean(tempY3))/sd(tempY3)
	#	sdY4=(tempY4[50]-mean(tempY4))/sd(tempY4)
	#	sdY5=(tempY5[50]-mean(tempY5))/sd(tempY5)
	#	sdY6=(tempY6[50]-mean(tempY6))/sd(tempY6)
		
	#	sdY=c(sdY1,sdY2,sdY3,sdY4,sdY5,sdY6)
	#	sdYall=rbind(sdYall,sdY)
	#	}	
	##tempX1=all[(1+(nt*(i-1))):(i*nt),1]
	##tempX2=all[(1+(nt*(i-1))):(i*nt),2]
	
	##sdX1=(tempX1[50]-mean(tempX1))/sd(tempX1)
	##sdX2=(tempX2[50]-mean(tempX2))/sd(tempX2)
	sdXall=NULL
	for (i in 1:np){
		tempX1=all[(1+(nt*(i-1))):(i*nt),1]
		tempX2=all[(1+(nt*(i-1))):(i*nt),2]

		sdX1=(tempX1[50]-mean(tempX1))/sd(tempX1)
		sdX2=(tempX2[50]-mean(tempX2))/sd(tempX2)
		
		sdX=c(sdX1,sdX2)
		sdXall=rbind(sdXall,sdX)
	}	
	
	nt=theTs[ss]
	ntt=nt
	sdys=TRUE
	}
#corr=.03/(sqrt(.1)*sqrt(.1))
if (trueI==6){
a0=matrix(c(0,0),ne,1,byrow=T)
P0=matrix(c(1000,0,
			0,1000),
			ne,ne,byrow=T)
}
}
}
if (model==3){
	nx=0
	Z=matrix(c(
	1,1,0,
	1.2,1,0,
	.8,1,0
	),ny,ne,byrow=T)
	# V
	V=matrix(c(
	1,0,0,
	0,.8,0,
	0,0,.4
	),ne,ne,byrow=T)
	# T
	T=matrix(c(
	.5,0,0,
	0,1,1,
	0,0,1),ne,ne,byrow=T)
	# R
	U=diag(c(.8,.6,2))
	# c
	c=matrix(c(0,0,0),ne,1,byrow=T)
	# d
	d=matrix(c(0,0,0),ny,1,byrow=T)
	a0 = matrix(0,ne,1)
	P0 = matrix(0,ne,ne)
	if (trueIstat==1){
		I = diag(1,1)
		theta1 = solve(I-(as.matrix(T[1,1]%x%T[1,1])))%*%V[1,1]
		P0[1,1]=theta1
		#if (det(V)>0) {Vs=chol(V)} else {Vs=matrix(0,ne,ne)}
		#if (det(U)>0) {Us=chol(U)} else {Us=matrix(0,ny,ny)}
		#if (det(P0)>0) {P0s=chol(P0)} else {P0s=matrix(0,ne,ne)}
	}
	if (trueIstat==2){
		P0[1,1]=1.2
		a0[1,1]=.1
		}
	
	if (trueI==4){
		if (det(U)>0) {Us=chol(U)} else {Us=matrix(0,ny,ny)}
		P0sStat=chol(P0[1,1])
		P0s=matrix(0,ne,ne)
		P0s[1,1]=P0sStat
		nt=50
		npad=0 # start up
		ist=npad+1   
		ntt=nt
		sdys=FALSE
		INIT=FALSE
		ne=2
		
		Z=matrix(c(
		1,0,
		1,0,
		1,0
		),ny,ne,byrow=T)
		# V
		V=matrix(c(
		.8,0,
		0,.4
		),ne,ne,byrow=T)
		# T
		T=matrix(c(
		1,1,
		0,1),ne,ne,byrow=T)
		# c
		c=matrix(c(0,0),ne,1,byrow=T)
		if (det(V)>0) {Vs=chol(V)} else {Vs=matrix(0,ne,ne)}
		source(paste(source,"/Simulate.R",sep=""))
		sdXall=NULL
		for (i in 1:np){
			tempX1=all[(1+(nt*(i-1))):(i*nt),1]
			tempX2=all[(1+(nt*(i-1))):(i*nt),2]
	
			sdX1=(tempX1[50]-mean(tempX1))/sd(tempX1)
			sdX2=(tempX2[50]-mean(tempX2))/sd(tempX2)
			
			sdX=c(sdX1,sdX2)
			sdXall=rbind(sdXall,sdX)
		}	
		ne=3
		Z=matrix(c(
		1,1,0,
		1.2,1,0,
		.8,1,0
		),ny,ne,byrow=T)
		# V
		V=matrix(c(
		1,0,0,
		0,.2,0,
		0,0,.4
		),ne,ne,byrow=T)
		# T
		T=matrix(c(
		.5,0,0,
		0,1,1,
		0,0,1),ne,ne,byrow=T)
		# R
		U=diag(c(.8,.6,2))
		# c
		c=matrix(c(0,0,0),ne,1,byrow=T)
		nt=theTs[ss]
		ntt=nt
		sdys=TRUE
		if (det(V)>0) {Vs=chol(V)} else {Vs=matrix(0,ne,ne)}
		INIT=TRUE
	}
	#corr=.03/(sqrt(.1)*sqrt(.1))
	if (trueI==6){
	a0=matrix(c(0,0,0),ne,1,byrow=T)
	P0=matrix(c(1000,0,0,
				0,1000,0,
				0,0,1000),
				ne,ne,byrow=T)
	}
}

#for AR(1)
if(AR1){
nt=20 	# number of time points
ne=1   	# number of states
ny=1	# number of observed 
nx=0    # number of fixed regressors
np=100	# number of subjects

npad=0 # start up
ist=npad+1   
ntt=nt+npad
INIT=FALSE
# Z
Z=matrix(c(
1,0),
ny,ne,byrow=T)
# V
V=matrix(c(
1),ne,ne,byrow=T)
# T
T=matrix(c(
1),ne,ne,byrow=T)
# U
U=matrix(c(0),ny,ny,byrow=T)
# c
c=matrix(c(.8),ne,1,byrow=T)
# d
d=matrix(c(0),ny,1,byrow=T)
# states a t=0
a0=matrix(c(0,0),ne,1,byrow=T)
P0=matrix(c(1000,0,0,
			0,1000,0,
			0,0,1000),
			ne,ne,byrow=T)
# cholesky of Q & R 
Vs = chol(V)
Us = ifelse(det(U)>0,chol(U),0)
}
	