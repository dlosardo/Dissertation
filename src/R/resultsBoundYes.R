rm(list=ls(all=TRUE))
library('xtable')
library('outliers')
smallpop=FALSE #if true then use smaller population values
small=FALSE #if true import data with smaller pop values
states=FALSE #if true import data with diffuse in states

if(small)small1="_small" else small1=NULL
if(states)states1="_states" else states1=NULL

source="C:/Documents and Settings/Diane Losardo/Desktop/dissertation/Dissertation/Dissertation" #where source files are located
source(paste(source,"/functions.R",sep=""))
outputDiri="C:/Documents and Settings/Diane Losardo/Desktop/dissertation/results/BoundsYes" #where SAS outputs results and I keep results
noMC = 500 #number of Monte Carlo runs
sampleSizes = c(200)#sample sizes
theTs = c(5)		#number of time points
ny=6

#For next two variables::
# 1=Model Implied, 2=Free Parameter, 3=Null Initial Condition,
# 4=deJong DKF, 5=Koopman EKF, 6=large kappa approx
trueI=4		#True Initial Condition
fittedI=c(2,3,4,5,6)	#Fittedn Initial Condition
model=2		#1=PFA Stationary, 2=PFA non-stationary, 3=PFA with local linear trend
allICspecTRUE=c("ModelImplied","FreeParm","Null","deJong","EKF","largeK")
ICspecTRUE=allICspecTRUE[trueI]
popValuesi=list(c(1.2,.8,.9,1.1,1,.4,1,.5,-.3,-.1,.6,.8,.6,2,1,1.5,.4),
			c(1.2,.8,.9,1.1,
				1,.4,1,
				1.2,.6,-.4,1,
				.8,.6,2,1,1.5,.4),
			c(1.2,.8,1,.8,.4,.5,.8,.6,2))
		
#which(outputs[outputs$V3=="EKF",41]!=outputs[outputs$V3=="deJong",41])
#ekf=outputs[outputs[,3]=="EKF",]
#dj=outputs[outputs[,3]=="deJong",]
#fp=outputs[outputs[,3]=="FreeParm",]
#mi=outputs[outputs[,3]=="ModelImplied",]
#lk=outputs[outputs[,3]=="largeK",]
#nu=outputs[outputs[,3]=="Null",]

#rbind(ekf[diffs,43],dj[diffs,43])
#rbind(mi[diffs[17],],fp[diffs[17],],nu[diffs[17],],dj[diffs[17],],ekf[diffs[17],],lk[diffs[17],])

#dj[diffs[17],]

#dlik=outputs[outputs$V3=="deJong",41]
#klik=outputs[outputs$V3=="EKF",41]

#diffs=which(abs(dlik-klik)>.1)
if (smallpop){popValuesi=list(c(1.2,.8,.9,1.1,1,.4,1,.5,-.3,-.1,.6,.8,.6,2,1,1.5,.4),
			c(1.2,.8,.9,1.1,
				.05,.02,.05,
				1.2,.6,-.4,.7,
				.8,.6,2,1,1.5,.4),
			c(1.2,.8,1,.8,.4,.5,.8,.6,2))}
popValues=popValuesi[[model]]
modelNamei=c("PFAstat","PFAnons","PFALLT")
modelName=modelNamei[model]

PFAcolnames = c("modelName","ICspecTRUE","ICfitted", "RMSE", "N","T","Z21","Z31","Z52","Z62",
"V11","V21","V22","T11","T21","T12","T22","U11","U22","U33","U44","U55","U66",
"seZ21","seZ31","seZ52","seZ62","seV11","seV21","seV22","seT11","seT21","seT12",
"seT22","seU11","seU22","seU33","seU44","seU55","seU66","LL","conv","invertSym","invertDec","time")
PFAcolnamesFP=c("modelName","ICspecTRUE","ICfitted", "RMSE","N","T","Z21","Z31","Z52","Z62",
"V11","V21","V22","T11","T21","T12","T22","U11","U22","U33","U44","U55","U66",
"X01","X02","P011","P012","P022",
"seZ21","seZ31","seZ52","seZ62","seV11","seV21","seV22","seT11","seT21","seT12",
"seT22","seU11","seU22","seU33","seU44","seU55","seU66",
"seX01","seX02","seP011","seP012","seP022",
"LL","conv","invertSym","invertDec","time")
#file = paste(outputDiri,"/M",modelName,"ICT_",ICspecTRUE,"ICF_All_N",sampleSizes,"_T",theTs,".dat",sep="")
#if (small1) file = paste(outputDiri,"/M",modelName,"ICT_",ICspecTRUE,"ICF_All_N",sampleSizes,"_T",theTs,small1,states1,".dat",sep="")
parNames=c("Z21","Z31","Z52","Z62","V11","V21","V22","T11","T21","T12","T22","U11","U22","U33","U44","U55","U66",
	"X01","X02","P011","P012","P022")

file = paste(outputDiri,"/M",modelName,"ICT_",ICspecTRUE,"ICF_All_N",sampleSizes,"_T",theTs,small1,states1,".dat",sep="")
outputs = read.table(file,stringsAsFactors=FALSE)

ekf=outputs[outputs[,3]=="EKF",]
dj=outputs[outputs[,3]=="deJong",]

#toP=which(dj$V44=="failedDecomp")

#sedv11=as.numeric(dj$V28)

#which(sedv11[toP]>1)
#minus=which(dv11[toP]>10)
#plot(sedv11[toP[-(128)]])
#dv11=as.numeric(dj$V11)
#plot(dv11[toP[-(minus)]])
#length(which(dv11[toP]>10))
#plot(sedv11[toP],ylim=c(-.1,.1))
#plot(dv11[toP],ylim=c(0,10))

#dv11[toP[minus]]

#dee=as.matrix(rbind(dv11[toP],sedv11[toP]))
#write(t(dee),file=paste(outputDiri,"/v11s.dat",sep=""),ncolumn=length(dv11))

dlik=outputs[outputs$V3=="deJong",41]
klik=outputs[outputs$V3=="EKF",41]

if(any(dlik=="."))dlik[which(dlik==".")]=99
if(any(klik=="."))klik[which(klik==".")]=99
dlik=as.numeric(dlik)
klik=as.numeric(klik)
diffs=which(abs(dlik-klik)>.1)

numDiff=length(diffs)
kcon=ekf[diffs,c(42,43,44)]
djcon=dj[diffs,c(42,43,44)]

kNstrong=length(which(kcon[,1]=="Strong"))
kNweak=length(which(kcon[,1]=="Weak"))
kNno=length(which(kcon[,1]=="No"))
dNstrong=length(which(djcon[,1]=="Strong"))
dNweak=length(which(djcon[,1]=="Weak"))
dNno=length(which(djcon[,1]=="No"))

kNinv=length(which(kcon[,2]=="failedInvertSym"))
kNdec=length(which(kcon[,3]=="failedDecomp"))

dNinv=length(which(djcon[,2]=="failedInvertSym"))
dNdec=length(which(djcon[,3]=="failedDecomp"))

ss=1
	np=sampleSizes[ss]
	nt=theTs[ss]
deR=c("deJong",allICspecTRUE[trueI],np,nt,numDiff,dNstrong,dNweak,dNno,dNinv,dNdec)
kR=c("Koopman",allICspecTRUE[trueI],np,nt,numDiff,kNstrong,kNweak,kNno,kNinv,kNdec)
both=rbind(deR,kR)
namesDK=c("FittedIC","TrueIC","np","nt","numDiff","nStrong","nWeak","nNo","nINV","nDEC")
colnames(both)=namesDK
dkDir=paste(outputDiri,"/DKcomp_M",modelName,"TIC",ICspecTRUE,"N",sampleSizes,"T",theTs,small1,states1,".txt",sep="")
write.table(both,file=dkDir,append=FALSE,col.names = TRUE)


#file=paste(outputDiri,"/Nullwritten.dat",sep="")
#outputs = read.table(file1)

#dee=scan(file,what="")
#nt=2500
#allz=matrix(NA,2500,55)
#toAdd=rep(c(55,45,45,45,45),500)
#start=0
#allAdd=NULL
#for (i in 1:length(toAdd)){
#	jj=toAdd[i]
#	start=start+jj
#	allAdd=c(allAdd,start)
#}
#begin=c(55,diff(allAdd))
#beginT=0
#for (i in 1:length(allAdd)){
#	jj=allAdd[i]
#	temp=dee[(1+beginT):(jj)]
#	dd=begin[i]
#	beginT=beginT+dd
	
#	allz[i,1:length(temp)]=temp
#}
#outputs=as.data.frame(allz)
#file1=paste(outputDiri,"/Nullwritten.dat",sep="")
#write(t(outputs),file1,ncolumn=ncol(outputs))


for (i in 1:length(fittedI)){
	m=fittedI[i]
	label=allICspecTRUE[m]
	nam=paste("output",label,sep="")
	assign(nam, outputs[outputs[,3]==label,])
	if (m==2){
		tmp=get(nam)
		colnames(tmp)=PFAcolnamesFP
		assign(nam,tmp)
	}else {tmp=get(nam)
		colnames(tmp)=PFAcolnames
		assign(nam,tmp)
		}
}

outputdeJong[is.na(outputdeJong$invertSym),43]="no"
outputEKF[is.na(outputEKF$invertSym),43]="no"
outputlargeK[is.na(outputlargeK$invertSym),43]="no"
outputFreeParm[is.na(outputFreeParm$invertSym),53]="no"
outputNull[is.na(outputNull$invertSym),43]="no"
if (model==1)outputModelImplied[is.na(outputModelImplied$invertSym),43]="no"

convStats=NULL
for (i in 1:length(fittedI)){
	m=fittedI[i]
	label=allICspecTRUE[m]
	nam=paste("output",label,sep="")
	if (m==2){
		NANtemp=length(which(is.nan(get(nam)[,4]),))+length(which(get(nam)[,4]=="Inf"))
		convTempStrong=length(which(get(nam)[,52]=="Strong"))
		convTempNo=length(which(get(nam)[,52]=="No"))
		convTempWeak=length(which(get(nam)[,52]=="Weak"))
		invertSymTemp=length(which(get(nam)[,53]=="failedInvertSym"))
		invertDecTemp=length(which(get(nam)[,54]=="failedDecomp"))
		noSE=length(which(get(nam)$seZ2=="."))
		temp=c(modelName,ICspecTRUE,label,sampleSizes,theTs,NANtemp,convTempStrong,convTempWeak,convTempNo,invertSymTemp,invertDecTemp,noSE)
		convStats=rbind(convStats,temp)
		} else {
		NANtemp=length(which(is.nan(get(nam)[,4]),))+length(which(get(nam)[,4]=="Inf"))
		convTempStrong=length(which(get(nam)[,42]=="Strong"))
		convTempNo=length(which(get(nam)[,42]=="No"))
		convTempWeak=length(which(get(nam)[,42]=="Weak"))
		invertSymTemp=length(which(get(nam)[,43]=="failedInvertSym"))
		invertDecTemp=length(which(get(nam)[,44]=="failedDecomp"))
		noSE=length(which(get(nam)$seZ2=="."))
		temp=c(modelName,ICspecTRUE,label,sampleSizes,theTs,NANtemp,convTempStrong,convTempWeak,convTempNo,invertSymTemp,invertDecTemp,noSE)
		convStats=rbind(convStats,temp)
		
		}
}

for (i in 1:length(fittedI)){
	m=fittedI[i]
	label=allICspecTRUE[m]
	nam=paste("output",label,sep="")
	nam1=paste("outputC",label,sep="")
	tmp1=get(nam)
	if (m==2){
		assign(nam1,get(nam)[(get(nam)[,4]!="Inf")&(!is.nan(get(nam)[,4]))&(get(nam)[,52]=="Strong"&get(nam)$seZ2!=".")   ,])#&is.na(get(nam)[,53]),])
		}else {
	assign(nam1,get(nam)[(get(nam)[,4]!="Inf")&(!is.nan(get(nam)[,4]))&(get(nam)[,42]=="Strong"&get(nam)$seZ2!=".")     ,])#&is.na(get(nam)[,43]),])
}
}

#for (i in 1:length(fittedI)){
#	m=fittedI[i]
#	label=allICspecTRUE[m]
#	nam=paste("output",label,sep="")
#	nam1=paste("outputC",label,sep="")
#	tmp1=get(nam)
#	if (m==2){
#		assign(nam1,get(nam)[(get(nam)[,4]!="Inf")&(!is.nan(get(nam)[,4]))&(get(nam)[,52]=="Strong"&get(nam)$seZ2!=".")&(get(nam)[,53]!="failedInvertSym")   ,])#&is.na(get(nam)[,53]),])
#		}else {
#	assign(nam1,get(nam)[(get(nam)[,4]!="Inf")&(!is.nan(get(nam)[,4]))&(get(nam)[,42]=="Strong"&get(nam)$seZ2!=".")&(get(nam)$invertSym!="failedInvertSym")      ,])#&is.na(get(nam)[,43]),])
#}
#}

#length(which(tmp1[,4]!="Inf"&!is.nan(tmp1[,4])&tmp1[,42]=="Strong"&tmp1$seZ2!="."))
#dim(tmp1[tmp1[,4]!="Inf"&!is.nan(tmp1[,4])&tmp1[,42]=="Strong"&tmp1$seZ2!=".",])
#tmp1[tmp1[,42]=="Strong",]

#length(which(tmp1[,42]=="Strong"))


ss=1
	np=sampleSizes[ss]
	nt=theTs[ss]
outs=NULL
for (m in 1:length(fittedI)){
	mm=fittedI[m]
	label=allICspecTRUE[mm]
	nam=paste("outputC",label,sep="")
	popValues=popValuesi[[model]]
	if (mm==2){
		popValues=c(popValues,1,.5,1.2,.3,.7)
	}
	noPars=length(popValues)	
	tmp=get(nam)
	if (nrow(tmp)==0) next
	numbsTemp=lapply(tmp[,c(7:(noPars*2+1+6),4)], as.numeric)
	numbsTemp=as.data.frame(numbsTemp)
	if(any(rowSums(is.na(numbsTemp))>=ncol(numbsTemp)-1))numbsTemp=numbsTemp[-which(rowSums(is.na(numbsTemp))>=(ncol(numbsTemp)-1)),]
	
	#outsALL=NULL
	#for (oo in 1:noPars){
	#	outsT=outDET1(numbsTemp[,oo])
	#	outsALL=c(outsALL,outsT)
	#}
	#outU=unique(outsALL)
	#outN=length(outU)
	#outs=rbind(outs,c(allICspecTRUE[trueI],allICspecTRUE[mm],outN))
	if (mm==2){
	outsT=length(unique(c(which(numbsTemp[,1]<(-20)|numbsTemp[,3]<(-40)|abs(numbsTemp$T12)>5|abs(numbsTemp$T21)>5|
	abs(numbsTemp$T22)>2.5|abs(numbsTemp$V21)>4|abs(numbsTemp$V11)>4|abs(numbsTemp$V22)>4|abs(numbsTemp$Z21)>3
		|abs(numbsTemp$Z31)>3|abs(numbsTemp$Z52)>3|abs(numbsTemp$Z62)>3
		|abs(numbsTemp$U11)>10|abs(numbsTemp$U22)>10|abs(numbsTemp$U33)>10
		|abs(numbsTemp$U44)>10|abs(numbsTemp$U55)>10|abs(numbsTemp$U66)>10
		|abs(numbsTemp$T11)>5))))
		}else{
	outsT=length(which(numbsTemp[,1]<(-20)|numbsTemp[,3]<(-40)|abs(numbsTemp$T12)>5|abs(numbsTemp$T21)>5|
	abs(numbsTemp$T22)>2.5|abs(numbsTemp$V21)>4|abs(numbsTemp$V11)>4|abs(numbsTemp$V22)>4|abs(numbsTemp$Z21)>3
		|abs(numbsTemp$Z31)>3|abs(numbsTemp$Z52)>3|abs(numbsTemp$Z62)>3
		|abs(numbsTemp$U11)>10|abs(numbsTemp$U22)>10|abs(numbsTemp$U33)>10
		|abs(numbsTemp$U44)>10|abs(numbsTemp$U55)>10|abs(numbsTemp$U66)>10|abs(numbsTemp$T11)>5))
		}
		outs=rbind(outs,c(allICspecTRUE[trueI],allICspecTRUE[mm],outsT))	
	
}
#plot(outputCFreeParm$V22)
#length(which(as.numeric(outputCFreeParm$V22)>.4))
#dee=outputCFreeParm$V22
#ones=which(as.numeric(dee)<.4)
#outputCFreeParm$invertSym[ones]

#outputCFreeParm[outU[11:15],7:28]
convStats=cbind(convStats,outs[,3])	
colnames(convStats)=c("Model","TrueIC","ICFit","np","nt","NAN","StrongConv","WeakConv","NoConv","failedInvertSym","failedDecomp","noSE","outlier")

convDir=paste(outputDiri,"/ConvStats_M",modelName,"TIC",ICspecTRUE,"N",sampleSizes,"T",theTs,small1,states1,".txt",sep="")
write.table(convStats,file=convDir,append=FALSE,col.names = TRUE)

#length(outputFreeParm[outputFreeParm[,52]=="Strong",])
#outputFreeParm[outputFreeParm[,52]=="Strong",52]
#length(which(is.nan(outputFreeParm[,4])))

#if(any(rowSums(is.na(outputs))>=ncol(outputs)-1)) nas1=length(which(rowSums(is.na(outputs))>=(ncol(outputs)-1))) else nas1=0
#if(any(rowSums(is.na(outputModelImplied))>=ncol(outputModelImplied)-1)) nas1=length(which(rowSums(is.na(outputModelImplied))>=(ncol(outputModelImplied)-1))) else nas1=0
#if(any(rowSums(is.na(outputCModelImplied))>=ncol(outputCModelImplied)-1)) nas1=length(which(rowSums(is.na(outputCModelImplied))>=(ncol(outputCModelImplied)-1))) else nas1=0
	

sumstatsALL=NULL
ostatsALL=NULL
ss=1
	np=sampleSizes[ss]
	nt=theTs[ss]
forANOVA=NULL
AICbest=matrix(NA,noMC,length(fittedI))
BICbest=matrix(NA,noMC,length(fittedI))

for (m in 1:length(fittedI)){
#for (m in 4:4){
	sumstats=NULL
	otherstats=NULL
	mm=fittedI[m]
	label=allICspecTRUE[mm]
	nam=paste("outputC",label,sep="")
	namf=paste("final",label,sep="")
	popValues=popValuesi[[model]]
	if (mm==2){
		popValues=c(popValues,1,.5,1.2,.3,.7)
	}
	noPars=length(popValues)
	tmp=get(nam)
	if (nrow(tmp)==0) next
	numbsTemp=lapply(tmp[,c(7:(noPars*2+1+6),4)], as.numeric)
	numbsTemp=as.data.frame(numbsTemp)
	if(any(rowSums(is.na(numbsTemp))>=ncol(numbsTemp)-1))numbsTemp=numbsTemp[-which(rowSums(is.na(numbsTemp))>=(ncol(numbsTemp)-1)),]
	
	#outsALL=NULL
	#for (oo in 1:noPars){
	#	outs=outDET1(numbsTemp[,oo])
	#	outsALL=c(outsALL,outs)
	#}
	#outU=unique(outsALL)
	#if(length(outU)>0)numbsTemp=numbsTemp[-(outU),]
	#if(!small){
	#if(any(numbsTemp$V11<.001)){
	#	numbsTemp=numbsTemp[-which((numbsTemp$V11)<.001),]
	#}
	#if(any(numbsTemp$V22<.001)){
	#	numbsTemp=numbsTemp[-which((numbsTemp$V22)<.001),]
	#	}
	#}
	#outsALL=NULL
	#for (oo in 1:noPars){
	#	outs=outDET1(numbsTemp[,oo])
	#	outsALL=c(outsALL,outs)
	#}
	#outU=unique(outsALL)
	#if(length(outU)>0)numbsTemp=numbsTemp[-(outU),]
	
		if(any(abs(numbsTemp$T11)>5)){ 
		numbsTemp=numbsTemp[-which(abs(numbsTemp$T11)>5),]
		}
	if(any(abs(numbsTemp$T12)>5)){ 
		numbsTemp=numbsTemp[-which(abs(numbsTemp$T12)>5),]
		}
	
	if(any(abs(numbsTemp$T21)>5)){
		numbsTemp=numbsTemp[-which(abs(numbsTemp$T21)>5),]
		}
	if(any(abs(numbsTemp$T22)>5)){
		numbsTemp=numbsTemp[-which(abs(numbsTemp$T22)>2.5),]
	}
	if (mm==3){
		if(any(abs(numbsTemp$V11)>10)){
			numbsTemp=numbsTemp[-which(abs(numbsTemp$V11)>10),]
			}
		if(any(abs(numbsTemp$V21)>9)){
 			numbsTemp=numbsTemp[-which(abs(numbsTemp$V21)>9),]
			}
		}else{
			if(any(abs(numbsTemp$V21)>4)){
 			numbsTemp=numbsTemp[-which(abs(numbsTemp$V21)>4),]
			}
			if(any(abs(numbsTemp$V11)>4)){
			numbsTemp=numbsTemp[-which(abs(numbsTemp$V11)>4),]
			}
		if(any(abs(numbsTemp$V22)>4)){
 		numbsTemp=numbsTemp[-which(abs(numbsTemp$V22)>4),]
		}
		}
	if(mm==2){
		if(any(abs(numbsTemp$P012)>100)){
 		numbsTemp=numbsTemp[-which(abs(numbsTemp$P012)>100),]
		} 
		if(any(abs(numbsTemp$X02)>100)){
 		numbsTemp=numbsTemp[-which(abs(numbsTemp$X02)>100),]
			} 
		}
	if(any(abs(numbsTemp$Z21)>3)){
 		numbsTemp=numbsTemp[-which(abs(numbsTemp$Z21)>3),]
		} 
	if(any(abs(numbsTemp$Z31)>3)){
 		numbsTemp=numbsTemp[-which(abs(numbsTemp$Z31)>3),]
	}
	if(any(numbsTemp$Z21<(-1))){
		numbsTemp=numbsTemp[-which(numbsTemp$Z21<(-1)),]
		}
	if(any(abs(numbsTemp$Z52)>3)){
 		numbsTemp=numbsTemp[-which(abs(numbsTemp$Z52)>3),]
		}
	if(any(abs(numbsTemp$Z62)>3)){
 		numbsTemp=numbsTemp[-which(abs(numbsTemp$Z62)>3),]
		}
	
	
	if(any(abs(numbsTemp$U11)>10)){
 		numbsTemp=numbsTemp[-which(abs(numbsTemp$U11)>10),]
		}
	if(any(abs(numbsTemp$U22)>10)){
 		numbsTemp=numbsTemp[-which(abs(numbsTemp$U22)>10),]
		}
	if(any(abs(numbsTemp$U33)>10)){
 		numbsTemp=numbsTemp[-which(abs(numbsTemp$U33)>10),]
		}
	if(any(abs(numbsTemp$U44)>10)){
 		numbsTemp=numbsTemp[-which(abs(numbsTemp$U44)>10),]
		}
	if(any(abs(numbsTemp$U55)>10)){
 		numbsTemp=numbsTemp[-which(abs(numbsTemp$U55)>10),]
		}
	if(any(abs(numbsTemp$U66)>10)){
 		numbsTemp=numbsTemp[-which(abs(numbsTemp$U66)>10),]
		}

	
	assign(namf,numbsTemp)
	if (nrow(numbsTemp)==0) next
	#if (mm==2){
	#	if(any(abs(numbsTemp$X02)>5))numbsTemp=numbsTemp[-which(abs(numbsTemp$X02)>5),]
	#	if(any(abs(numbsTemp$P012)>5))numbsTemp=numbsTemp[-which(abs(numbsTemp$P012)>5),]
	#	if(any(abs(numbsTemp$P022)>5))numbsTemp=numbsTemp[-which(abs(numbsTemp$P022)>5),]
	#	}
	#theResultsd1=theResultsd[rowSums(is.na(theResultsd[,c(4,18)]))!=2,]
	#numbsTemp=numbsTemp[-which(numbsTemp[,7]<(-20)),]
	for (p in 1:(noPars)){
		meantemp = Mean(numbsTemp[,p])
		sdtemp = SD(numbsTemp[,p])
		meanSE = Mean(numbsTemp[,p+noPars])
		tempbias = Bias(numbsTemp[,p],popValues[(p)])
		temprelbias = relBias(numbsTemp[,p],popValues[(p)])
		tempRMSE = RMSE(numbsTemp[,p],popValues[(p)])
		
		cover = matrix(NA,nrow(numbsTemp),1)
		power = matrix(NA,nrow(numbsTemp),1)
		#LLi = matrix(NA,nrow(numbsTemp),1)
		AICi= matrix(NA,nrow(numbsTemp),1)
		BICi = matrix(NA,nrow(numbsTemp),1)
		#RMSEAi = matrix(NA,nrow(numbsTemp),1)

			for (i in 1:nrow(numbsTemp)){
				if (is.na(numbsTemp[i,1])) next
				if (is.na(numbsTemp[i,(p+noPars)])) next
				templcl = lcl(numbsTemp[i,p],numbsTemp[i,(p+noPars)])
				tempucl = ucl(numbsTemp[i,p],numbsTemp[i,(p+noPars)])
				cover[i,1] = cov(templcl,tempucl,popValues[(p)])
				if (!(numbsTemp[i,p]==0&numbsTemp[i,(p+noPars)]==0)){
				power[i,1] = pow(numbsTemp[i,p],numbsTemp[i,(p+noPars)])} else power[i,1]=NA
				#LLi[i,1]=FML(numbsTemp$LL[i],np)
				AICi[i,1]=AIC(numbsTemp$LL[i],noPars)
				BICi[i,1]=BIC(numbsTemp$LL[i],noPars,np)
				#RMSEAi[i,1]=RMSEA(numbsTemp$LL[i],noPars,np,ny,nt)
				} #end i loop

		percovtemp = Mean(cover[,1])
		perpowtemp = Mean(power[,1])
		timetemp= Mean(as.numeric(tmp$time))		
		sumstats = rbind(sumstats,c(allICspecTRUE[trueI],allICspecTRUE[mm],popValues[(p)],meantemp,sdtemp,meanSE,tempbias,
			temprelbias,tempRMSE,percovtemp,perpowtemp))
	}#end p loop
		tempLL= Mean(numbsTemp$LL)
		tempAIC=Mean(AICi)
		tempBIC=Mean(BICi)
		finalN=nrow(numbsTemp)
		lvscores=Mean(numbsTemp$RMSE)
		#tempRMSEA = Mean(RMSEAi)
		otherstats=rbind(otherstats,c(allICspecTRUE[trueI],allICspecTRUE[mm],tempLL,tempAIC,tempBIC,timetemp,finalN,lvscores))
	sumstatsALL=rbind(sumstatsALL,sumstats)
	ostatsALL=rbind(ostatsALL,otherstats)
	AICbest[as.numeric(rownames(numbsTemp)),m]=AICi
	BICbest[as.numeric(rownames(numbsTemp)),m]=BICi
	
	#stacking parms
	stackParms=NULL	
	for (st in c(1:noPars)){
		stemp = cbind(numbsTemp[,st],(st),allICspecTRUE[trueI],allICspecTRUE[mm],np)
		dimnames(stemp)[[2]][[1]]="parm"
		dimnames(stemp)[[2]][[2]]="parmN"
		stackParms=rbind(stackParms,stemp)
		}
	#Stacking SE results
	stackSEs=NULL
	for (st in (noPars+1):(noPars*2)){
		stemp = as.matrix(numbsTemp[,st])
		stackSEs=rbind(stackSEs,stemp)
		}
	stackAll=cbind(stackParms,stackSEs)
	forANOVA=rbind(forANOVA,stackAll)
	
	#retaining outliers
#	if (mm==2){
#	maxO=max(c(length(outValsZ21),length(outValsZ31),length(outValsZ52),length(outValsZ62),
#		length(outValsV11),length(outValsV21),length(outValsV22),length(outValsT11),
#		length(outValsT21),length(outValsT12),length(outValsT22),length(outValsU11),
#		length(outValsU22),length(outValsU33),length(outValsU44),length(outValsU55),
#		length(outValsU66),(length(o1)+1),(length(o2)+1),(length(o3)+1),(length(o4)+1),(length(o5)+1)))
#	}else {maxO=max(c(length(outValsZ21),length(outValsZ31),length(outValsZ52),length(outValsZ62),
#		length(outValsV11),length(outValsV21),length(outValsV22),length(outValsT11),
#		length(outValsT21),length(outValsT12),length(outValsT22),length(outValsU11),
#		length(outValsU22),length(outValsU33),length(outValsU44),length(outValsU55),
#		length(outValsU66)))
#		}
#	outVals=matrix(NA,noPars,maxO)
#	for (oo in 1:noPars){
#		onam=paste("outVals",parNames[oo],sep="")
#		otemp=get(onam)
#		if (length(otemp)==0) next
#		outVals[oo,1:length(otemp)]=otemp
#	}
#	if(any(rowSums(is.na(outVals))==maxO))outVals=outVals[-which(rowSums(is.na(outVals))==maxO),]
#	outVals=outU
#	if (length(outVals)==0) next 
#	fileos=paste(outputDiri,"/OUTLIERS",modelName,allICspecTRUE[trueI],label,theTs,small1,states1,".dat",sep="")
#	write(t(outVals),file=fileos,ncolumns=length(outVals))
	}	
fileANOVA=paste(outputDiri,"/forANOVA",modelName,allICspecTRUE[trueI],theTs,small1,states1,".dat",sep="")
write(t(forANOVA),file=fileANOVA,ncolumns=ncol(forANOVA))

AICbest1=NULL
BICbest1=NULL
for(i in 1:noMC){
	baic=min(AICbest[i,],na.rm=T)
	num=which(AICbest[i,]==baic)
	num1=fittedI[num]
	AICbest1=cbind(AICbest1,num)
	
	bbic=min(BICbest[i,],na.rm=T)
	num=which(BICbest[i,]==bbic)
	BICbest1=cbind(BICbest1,num)
}
tableAICbest=NULL
tableBICbest=NULL
for (m in 1:length(fittedI)){
	mm=fittedI[m]
	label=allICspecTRUE[mm]
	numA=length(which(AICbest1==m))
	temp=c(modelName,ICspecTRUE,label,numA)
	numB=length(which(BICbest1==m))
	temp=c(modelName,ICspecTRUE,label,numB)
	tableAICbest=rbind(tableAICbest,temp)
	tableBICbest=rbind(tableBICbest,temp)
}
fileAIC=paste(outputDiri,"/AICbest",modelName,allICspecTRUE[trueI],theTs,small1,states1,".dat",sep="")
write(t(tableAICbest),file=fileAIC,ncolumns=ncol(tableAICbest))

fileBIC=paste(outputDiri,"/BICbest",modelName,allICspecTRUE[trueI],theTs,small1,states1,".dat",sep="")
write(t(tableBICbest),file=fileBIC,ncolumns=ncol(tableBICbest))


#plot(outputCEKF$V21)
#plot(outputClargeK$T22)
#which(abs(as.numeric(outputCEKF$T12))>5)
#outputCEKF[52,]
#plot(outputCEKF$RMSE)
#
#which(as.numeric(outputCdeJong$U44)>10)

#outputCdeJong[43,]

#plot(outputCdeJong$U44)
#windows()
#plot(finaldeJong$T11)


sumstatsALL[,c(4:(ncol(sumstatsALL)))]=round(as.numeric(sumstatsALL[,c(4:(ncol(sumstatsALL)))]),3)
ostatsALL[,3:8]=round(as.numeric(ostatsALL[,3:8]),3)
cnames1=c("TrueIC","FittedIC","LL","AIC","BIC","Time","finalN","rmseLVscores")
colnames(ostatsALL)=cnames1
a=as.data.frame(ostatsALL)
c=as.data.frame(convStats)
sumTable=cbind(as.matrix(a$TrueIC),as.matrix(a$FittedIC),as.matrix(a$finalN),as.matrix(c$StrongConv),
    as.matrix(c$WeakConv),as.matrix(c$NoConv),(as.numeric(as.matrix(c$outlier))+as.numeric(as.matrix(c$NAN)))
	,as.matrix(c$noSE),as.matrix(tableAICbest[,4]),as.matrix(tableBICbest[,4])
	,as.matrix(a$rmseLVscores),as.matrix(a$Time))
cnames2=c("TrueIC","FittedIC","Final # Reps","Strong Conv", "Weak Conv","No Conv","Outliers","No SEs",
	"AIC","BIC","RMSE lvs","Time")
sumTable=rbind(cnames2,sumTable)

#newVec=c("deJong","Null",NA,NA,NA,NA,0,NA)
#a1=rbind(a[1,],newVec,a[2:nrow(a),])

fileT2=paste(outputDiri,"/table3M",modelName,"ICT_",ICspecTRUE,"ICF_ALL_T",nt,small1,states1,".dat",sep="")
write(print(xtable(sumTable,include.rownames=FALSE,
	caption="Simulation Results: Comparison Across Initial Condition Specifications",digits=c(0, 0,0,2,2, 2, 2, 2, 2, 2, 2, 2,2)),
	type="latex",include.colnames=FALSE,include.rownames=FALSE,sanitize.text.function=function(x){x}),file=fileT2)


fileO=paste(outputDiri,"/ostats_",modelName,"ICT_",ICspecTRUE,"ICF_ALL_T",nt,small1,states1,".dat",sep="")
write.table(ostatsALL,file=fileO)


rnames = c("Z21","Z31","Z52","Z62","V11","V21","V22","T11","T21","T12","T22","U11","U22","U33","U44","U55","U66",
	"X01","X02","P011","P012","P022",
	rep(c(
	"Z21","Z31","Z52","Z62","V11","V21","V22","T11","T21","T12","T22","U11","U22","U33","U44","U55","U66"),
4))

cnames = c("$\\theta$","True IC","Fitted IC","True Value","$\\hat{\\theta}$","$SE_\\theta$","$\\hat{SE}$","Bias","RB","RMSE","cov","pow")

call = cbind(rnames,sumstatsALL)
rall = rbind(cnames,call)

file1=paste(outputDiri,"/table1M",modelName,"ICT_",ICspecTRUE,"ICF_ALL_T",nt,small1,states1,".dat",sep="")
file2=paste(outputDiri,"/table2M",modelName,"ICT_",ICspecTRUE,"ICF_ALL_T",nt,small1,states1,".dat",sep="")

write(print(xtable(rall,include.rownames=FALSE,
	caption="Simulation Results",digits=c(0, 0,0,3, 3, 3, 3, 3, 3, 3, 3,3,3)),
	type="latex",include.colnames=FALSE,include.rownames=FALSE,sanitize.text.function=function(x){x}),file=file1)

write(print(xtable(rall[,4:ncol(rall)],include.rownames=FALSE,
	caption="Simulation Results",digits=c(0,3, 3, 3, 3, 3, 3, 3, 3,3)),
	type="latex",include.colnames=FALSE,include.rownames=FALSE,sanitize.text.function=function(x){x}),file=file2)

file2=paste(outputDiri,"/summaryM",modelName,"ICT_",ICspecTRUE,"ICF_ALL_T",nt,small1,states1,".dat",sep="")
file3=paste(outputDiri,"/summary1M",modelName,"ICT_",ICspecTRUE,"ICF_ALL_T",nt,small1,states1,".dat",sep="")
write(t(sumstatsALL),file=file2,ncolumns=ncol(sumstatsALL))
write(t(rall),file=file3,ncolumns=ncol(rall))



#pdf(file="C:/Documents and Settings/Diane Losardo/Desktop/dissertation/latex/outlier.pdf")
#plot(dee$Z52,xlab="Replication",ylab="Z52 Parameter Estimates",main="Free-Parameter Specification Fit to Moderately Nonstationary Model")

#dev.off()

#outputCFreeParm$Z52

#dee=outputCFreeParm[-47,]

#which(abs(as.numeric(dee$Z52))>4)

#pdf(file="C:/Documents and Settings/Diane Losardo/Desktop/dissertation/latex/boundary.pdf")
#plot(outputCFreeParm$V22,xlab="Replication",ylab="V22 Parameter Estimates",main="Free-Parameter Specification Fit to Moderately Nonstationary Model")

#dev.off()

#windows()
#plot(outputCFreeParm$V22)

#plot(as.numeric(outputdeJong$V11))

#rem=which(as.numeric(outputdeJong$U11)>5|as.numeric(outputdeJong$U22)>5|as.numeric(outputdeJong$U33)>5|
	#as.numeric(outputdeJong$U44)>5|as.numeric(outputdeJong$U55)>5|as.numeric(outputdeJong$U66)>5)

#dee=outputdeJong[-c(rem),]
#plot(as.numeric(dee$U11))
#plot(as.numeric(dee$U22))
#plot(as.numeric(dee$U33))
#plot(as.numeric(dee$U44))
#plot(as.numeric(dee$U55))
#plot(as.numeric(dee$U66))

#plot(as.numeric(dee$Z62))
plot(outputCNull$V11)



