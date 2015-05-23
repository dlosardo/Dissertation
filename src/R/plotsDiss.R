rm(list=ls(all=TRUE))
library('car')
#Order of summary stats:
#1. Parameter
#2. nt
#3.	TrueIC
#4. FittedIC
#5. True Value
#6. Mean(theta)
#7. Abs Bias
#8. AbsRB
#9. LCL
#10. UCL
#11. Absolute(Coverage Rates - .95)
#12. Pow
#13. MSE
#14. SD-SE
#15. CI Width
	
source="C:/Documents and Settings/Diane Losardo/Desktop/dissertation/Dissertation/Dissertation" #where source files are located
source(paste(source,"/functions.R",sep=""))
outputDiri="C:/Documents and Settings/Diane Losardo/Desktop/dissertation/results/noSym" #where SAS outputs results and I keep results
noMC = 500 #number of Monte Carlo runs
sampleSizes = c(200)#sample sizes
theTs = c(5)		#number of time points

smallpop=FALSE #if true then use smaller population values
small=FALSE #if true import data with smaller pop values
states=FALSE #if true import data with diffuse in states

if(small)small1="_small" else small1=NULL
if(states)states1="_states" else states1=NULL

#For next two variables::
# 1=Model Implied, 2=Free Parameter, 3=Null Initial Condition,
# 4=deJong DKF, 5=Koopman EKF, 6=large kappa approx
trueI=c(2,3,4)		#True Initial Condition
fittedI=c(2,3,4,5,6)	#Fittedn Initial Condition
model=2	#1=PFA Stationary, 2=PFA non-stationary, 3=PFA with local linear trend

allICspecTRUE=c("ModelImplied","FreeParm","Null","deJong","EKF","largeK")
ICspecTRUE=allICspecTRUE[trueI]
modelNamei=c("PFAstat","PFAnons","PFALLT")
modelName=modelNamei[model]

#file=paste(outputDiri,"/summaryM",modelName,"ICT_",ICspecTRUE,"ICF_ALL_T",nt,".dat",sep="")
cnames = c("True IC","Fitted IC","True Value","$\\hat{\\theta}$",
	"$SE_\\theta$","$\\hat{SE}$","Bias","RB","RMSE","cov","pow")
parm = c("Z21","Z31","Z52","Z62","V11","V21","V22","T11","T21","T12","T22","U11","U22","U33","U44","U55","U66",
	"X01","X02","P011","P012","P022",
	rep(c(
	"Z21","Z31","Z52","Z62","V11","V21","V22","T11","T21","T12","T22","U11","U22","U33","U44","U55","U66"),
4))
parNames=c("Z21","Z31","Z52","Z62","V11","V21","V22","T11","T21","T12","T22","U11","U22","U33","U44","U55","U66",
	"X01","X02","P011","P012","P022")
#newResults=NULL
#for (s in 1:length(sampleSizes)){
#	for (ic in 1:length(trueI)){
#		nt=theTs[s]
#		icT=trueI[ic]
#		temp=read.table(paste(c(outputDiri,"/summaryM",modelName,"ICT_",allICspecTRUE[icT],"ICF_ALL_T",
#			nt,".dat"),sep="",collapse=""), header=FALSE ,col.names=cnames)
#		temp=cbind(parm,nt,temp)
#		newResults=rbind(newResults,temp)
#	}
#}

cnames1=c("Est","Parm","TrueIC","FittedIC","T","SE")
newResults=NULL
for (s in 1:length(sampleSizes)){
	nt=theTs[s]
	for (ic in 1:length(trueI)){
		icT=trueI[ic]
		#if (icT!=4)states1=NULL else states1="_states"
		temp=read.table(paste(c(paste(outputDiri,"/forANOVA",modelName,allICspecTRUE[icT],
			nt,small1,states1,".dat",sep="")),sep="",collapse=""), header=FALSE ,col.names=cnames1)
		newResults=rbind(newResults,temp)
	}
}

if(states)states1="_states" else states1=NULL
popValuesi=list(c(1.2,.8,.9,1.1,1,.4,1,.5,-.3,-.1,.6,.8,.6,2,1,1.5,.4),
			c(1.2,.8,.9,1.1,
				1,.4,1,
				1.2,.6,-.4,1,
				.8,.6,2,1,1.5,.4),
			c(1.2,.8,1,.8,.4,.5,.8,.6,2))
if (smallpop){popValuesi=list(c(1.2,.8,.9,1.1,1,.4,1,.5,-.3,-.1,.6,.8,.6,2,1,1.5,.4),
			c(1.2,.8,.9,1.1,
				.05,.02,.05,
				1.2,.6,-.4,.7,
				.8,.6,2,1,1.5,.4),
		c(1.2,.8,1,.8,.4,.5,.8,.6,2))}
popValues=popValuesi[[model]]
popValues=c(popValues,1,.5,1.2,.3,.7)	
nparms=length(popValues)

#Bias
for (i in 1:nparms){
	newResults[newResults[,2]==i,7]=(newResults[newResults[,2]==i,1]-popValues[i])
}
fun1=function(x){abs(mean(x))}
#Mean(newResults[newResults$TrueIC=="deJong"&newResults$FittedIC=="deJong"&newResults$byParm==3,7])
#bs=newResults[newResults$TrueIC=="deJong"&newResults$FittedIC=="deJong"&newResults$byParm==3,7]
#bs1=newResults[newResults$TrueIC=="deJong"&newResults$FittedIC=="deJong"&newResults$byParm==3,]
#Mean(bs1[bs1$Parm==8,7])
#Bias(bs1[bs1$Parm==8,1],popValues[8])
#fun1(bs1[bs1$Parm==8,7])
#length(bs)
#which(newResults[newResults$TrueIC=="deJong"&newResults$FittedIC=="deJong"&newResults$byParm==2,7]>1)
#dee=newResults[newResults$TrueIC=="deJong"&newResults$FittedIC=="deJong"&newResults$byParm==2,]
#dee[1603,]
#Mean(newResults[newResults$TrueIC=="deJong"&newResults$FittedIC=="FreeParm"&newResults$byParm==2,7])
#Bias
#for (i in 1:nparms){
#	newResults[newResults[,2]==i,7]=(newResults[newResults[,2]==i,1]-popValues[i])
#}
#Relative Bias
for (i in 1:nparms){
	newResults[newResults[,2]==i,8]=abs((newResults[newResults[,2]==i,1]-popValues[i])/popValues[i])
}
#Coverage rates
#LCL
newResults[,9]=lcl(newResults[,1],newResults[,6])
#UCL
newResults[,10]=ucl(newResults[,1],newResults[,6])

#Coverage Rates
for (i in 1:nparms){
	newResults[newResults[,2]==i,11]=cov1(newResults[newResults[,2]==i,9],
		newResults[newResults[,2]==i,10],popValues[i])
}
#Power
newResults[,12]=pow1(newResults[,1],newResults[,6])
#MSE 
for (i in 1:nparms){
	newResults[newResults[,2]==i,13]=(newResults[newResults[,2]==i,1]-popValues[i])^2
}
#SD-SE
newResults[,14]=abs(SD(newResults[,1])-newResults[,6])
#CI width
newResults[,15]=newResults[,10]-newResults[9]

dimnames(newResults)[[2]]=c("Est","Parm","TrueIC","FittedIC","T","SE","Absolute Bias",
	"AbsRB","LCL","UCL","Absolute(Coverage Rates - .95)","Pow","MSE","SD-SE","CI Width")


for (s in 1:length(sampleSizes)){
	ss=sampleSizes[s]
	for (ic in 1:length(trueI)){
		icT=trueI[ic]
		icTn=allICspecTRUE[icT]
			for (fic in 1:length(fittedI)){
				icF=fittedI[fic]
				icFn=allICspecTRUE[icF]
				for (p in 1:nparms){
					#pp=parNames[p]
					vtemp=(abs(Mean(newResults[newResults[,5]==ss&newResults[,3]==icTn&newResults[,4]==icFn&newResults[,2]==p,11]-.95))*length(newResults[newResults[,5]==ss&newResults[,3]==icTn&newResults[,4]==icFn&newResults[,2]==p,11]))/sum(newResults[newResults[,5]==ss&newResults[,3]==icTn&newResults[,4]==icFn&newResults[,2]==p,11])
					if (vtemp=="Inf"){
						newResults[newResults[,11]==0&newResults[,5]==ss&newResults[,3]==icTn&newResults[,4]==icFn&newResults[,2]==p,11]=.95
						}else{
					newResults[newResults[,11]==1&newResults[,5]==ss&newResults[,3]==icTn&newResults[,4]==icFn&newResults[,2]==p,11]=vtemp}
				}
			}
		}
	}

for (s in 1:length(sampleSizes)){
	ss=sampleSizes[s]
	for (ic in 1:length(trueI)){
		icT=trueI[ic]
		icTn=allICspecTRUE[icT]
			for (fic in 1:length(fittedI)){
				icF=fittedI[fic]
				icFn=allICspecTRUE[icF]
				for (p in 1:nparms){
					vtemp=abs(SD(newResults[newResults[,5]==ss&newResults[,3]==icTn&newResults[,4]==icFn&newResults[,2]==p,1])-(newResults[newResults[,5]==ss&newResults[,3]==icTn&newResults[,4]==icFn&newResults[,2]==p,6]))
					newResults[newResults[,5]==ss&newResults[,3]==icTn&newResults[,4]==icFn&newResults[,2]==p,14]=vtemp
				}
			}
		}
	}
##############################################
##############################################
#Rsquares Tables##############################
##############################################
##############################################
rsquares=NULL
rsquaresT=NULL
rsALL=NULL
enames=c("FittedIC","NT","FICxNT")
for (ic in 1:length(trueI)){
	icT=trueI[ic]
	resTemp=newResults[newResults$TrueIC==allICspecTRUE[icT],]
	rsquaresT=NULL
	for (i in c(7:8,11:15)){	
		temp=summary(aov(resTemp[,i]~resTemp$FittedIC*resTemp$T, data=resTemp))
		untemp=unlist(temp)
		for (e in 5:7){
			tot=untemp[5]+untemp[6]+untemp[7]+untemp[8]
			rstemp=untemp[e]/tot
			rsquaresT=rbind(rsquaresT,cbind(allICspecTRUE[icT],dimnames(newResults)[[2]][i],enames[e-4],rstemp))
		}
	}
	#rsquares=cbind(rsquares,allICspecTRUE[icT],rsquaresT)
	rsALL=rbind(rsALL,rsquaresT)
}
#resTemp=newResults[newResults$TrueIC==allICspecTRUE[icT]&newResults$T==200,]
#aov(resTemp[,7]~resTemp$FittedIC,data=resTemp)
#120/(120+85578)
#Anova(aov(resTemp[,i]~resTemp$FittedIC),type=c("III"),singular.ok=TRUE)
#Anova(aov(resTemp[,i]~resTemp$FittedIC*resTemp$T),type=c("III"),singular.ok=TRUE)
cnamesA=c("TrueIC","outcome","factor","R2")
colnames(rsALL)=cnamesA
dirANOVA = paste(outputDiri,"/anovaResultsTable",modelName,small1,states1,".dat",sep="")
write.table(rsALL,file=dirANOVA)


###BY PARAMETER
rsquares=NULL
rsquaresT=NULL
rsALL=NULL
enames=c("FittedIC","NT","FICxNT")
for (ic in 1:length(trueI)){
	icT=trueI[ic]
	resTemp=newResults[newResults$TrueIC==allICspecTRUE[icT],]
	noPar=length(unique(resTemp[,2]))
	Pars=unique(resTemp$parm)
	rsquaresT=NULL
	for (p in 1:17){
		parp=parNames[p]
		resTempP=resTemp[resTemp$Parm==p,]
		for (i in c(7:8,11:15)){	
			temp=summary(aov(resTempP[,i]~resTempP$FittedIC*resTempP$T, data=resTempP))
			untemp=unlist(temp)
			for (e in 5:7){
				tot=untemp[5]+untemp[6]+untemp[7]+untemp[8]
				rstemp=untemp[e]/tot
				rsquaresT=rbind(rsquaresT,cbind(parp,allICspecTRUE[icT],dimnames(newResults)[[2]][i],enames[e-4],rstemp))
				}
			}
		rsALL=rbind(rsALL,rsquaresT)
		}
		#rsquares=cbind(allICspecTRUE[icT],rsquaresT)
	}
cnamesA=c("Parm","TrueIC","outcome","factor","R2")
colnames(rsALL)=cnamesA
dirANOVA = paste(outputDiri,"/anovaResultsTableBYPARM",modelName,small1,states1,".dat",sep="")
write.table(rsALL,file=dirANOVA)


iclabelsT=c("MI","Free","Null","DKF",
	"EKF", "LargeK")

newResults=cbind(newResults,1)
dimnames(newResults)[[2]][[(ncol(newResults))]]="ICF"
for (j in 1:length(fittedI)){
	jj=fittedI[j]
	newResults[newResults$FittedIC==allICspecTRUE[jj],ncol(newResults)]=jj
}
newResults=cbind(newResults,1)
dimnames(newResults)[[2]][[(ncol(newResults))]]="ICFlab"
for (j in 1:length(fittedI)){
	jj=fittedI[j]
	newResults[newResults$FittedIC==allICspecTRUE[jj],ncol(newResults)]=iclabelsT[jj]
}

byParm=c(1,1,1,1,2,2,2,3,3,3,3,1,1,1,1,1,1)
newResults=cbind(newResults,NA)
dimnames(newResults)[[2]][[(ncol(newResults))]]="byParm"
for (j in 1:length(byParm)){
	newResults[newResults$Parm==j,ncol(newResults)]=byParm[j]
}

#newResults=newResults[,-(ncol(newResults))]
###BY PARAMETER TYPE
rsquares=NULL
rsquaresT=NULL
rsALL=NULL
enames=c("FittedIC","NT","FICxNT")
parType=c("Meas","PN","AR")
for (ic in 1:length(trueI)){
	icT=trueI[ic]
	resTemp=newResults[newResults$TrueIC==allICspecTRUE[icT],]
	noPar=length(unique(resTemp[,2]))
	Pars=unique(resTemp$parm)
	rsquaresT=NULL
	for (p in 1:3){
		parp=parType[p]
		resTempP=resTemp[resTemp$byParm==p,]
		for (i in c(7:8,11:15)){	
			temp=summary(aov(resTempP[,i]~resTempP$FittedIC*resTempP$T, data=resTempP))
			untemp=unlist(temp)
			for (e in 5:7){
				tot=untemp[5]+untemp[6]+untemp[7]+untemp[8]
				rstemp=untemp[e]/tot
				rsquaresT=rbind(rsquaresT,cbind(parp,allICspecTRUE[icT],dimnames(newResults)[[2]][i],enames[e-4],rstemp))
				}
			}
		rsALL=rbind(rsALL,rsquaresT)
		}
		#rsquares=cbind(allICspecTRUE[icT],rsquaresT)
	}
cnamesA=c("Parm","TrueIC","outcome","factor","R2")
colnames(rsALL)=cnamesA
dirANOVA = paste(outputDiri,"/anovaResultsTableBYPARMTYPE",modelName,small1,states1,".dat",sep="")
write.table(rsALL,file=dirANOVA)


###BY PARAMETER TYPE
if(small==FALSE&model==2){
rsquares=NULL
rsquaresT=NULL
rsALL=NULL
enames=c("FittedIC")
parType=c("Meas","PN","AR")
for (ic in 1:length(trueI)){
	icT=trueI[ic]
	resTemp=newResults[newResults$TrueIC==allICspecTRUE[icT],]
	noPar=length(unique(resTemp[,2]))
	Pars=unique(resTemp$parm)
	rsquaresT=NULL
	for (p in 1:3){
		parp=parType[p]
		resTempP=resTemp[resTemp$byParm==p,]
		for (i in c(7:8,11:15)){	
			temp=summary(aov(resTempP[,i]~resTempP$FittedIC, data=resTempP))
			untemp=unlist(temp)
			
				tot=untemp[3]+untemp[4]
				rstemp=untemp[3]/tot
				rsquaresT=rbind(rsquaresT,cbind(parp,allICspecTRUE[icT],dimnames(newResults)[[2]][i],enames,rstemp))
				
			}
		rsALL=rbind(rsALL,rsquaresT)
		}
		#rsquares=cbind(allICspecTRUE[icT],rsquaresT)
	}
cnamesA=c("Parm","TrueIC","outcome","factor","R2")
colnames(rsALL)=cnamesA
dirANOVA = paste(outputDiri,"/anovaResultsTableBYPARMTYPE",modelName,small1,states1,".dat",sep="")
write.table(rsALL,file=dirANOVA)
}
i=7 #Abs Bias
i=8 #AbsRB
i=9 #LCL
i=10 #UCL
i=11 #Absolute(Coverage Rates - .95)
i=12 #Pow
i=13 #MSE
i=14 #SD-SE
i=15 #CI Width

p=1 #Z21
p=2 #Z31
p=3 #Z52
p=4 #Z62
p=5 #V11
p=6 #V21
p=7 #V22
p=8 #T11
p=9 #T21
p=10 #T12
p=11 #T22
p=12 #U11
p=13 #U22
p=14 #U33
p=15 #U44
p=16 #U55
p=17 #U66
p=18 #X01
p=19 #X02
p=20 #P011
p=21 #P012
p=22 #P022

icT=1 #Model Implied
icT=2 #FreeParm
icT=3 #Null
icT=4 #deJong
icT=5 #EKF
icT=6 #largeK

outcomes=c("Absolute Bias", "Absolute Relative Bias", "LCL", "UCL", "Absolute(Coverage Rates - .95)",
	"Power", "MSE", "SD-SE", "CI Width")


#icT=4
#resTemp=newResults[newResults$TrueIC==allICspecTRUE[icT],]
#pdf (file = paste(outputDiri,"/plots/plot_,",modelName,"_",allICspecTRUE[icT],"byparm",small1,states1,".pdf",sep="",collapse=""))
#par(cex=1.3)
#par(mfrow=c(2,2))
#for (p in 1:17){
#	resTempP=resTemp[resTemp$Parm==p,]
#	for (i in c(7:8,11:15)){
#		interaction.plot((resTempP$T),(resTempP$ICF),resTempP[,i],legend=T,   lty=c(1:6),lwd=2, col=c(1:4,6,7),#  ylim=c(.0,.23),
#		trace.label = "FittedIC",   #deparse(substitute(trace.factor)),
#		fixed=TRUE,
#		type="b",
#		ylab=outcomes[i-6],
#		xlab="Sample Size",
#		main=paste(modelName," ",allICspecTRUE[icT],"_",parNames[p],sep="")
#		)
#	}
#}
#dev.off()

#icT=4
#resTemp=newResults[newResults$TrueIC==allICspecTRUE[icT],]
#pdf (file = paste(outputDiri,"/plots/plot",modelName,"_",allICspecTRUE[icT],"byparmREV",small1,states1,".pdf",sep="",collapse=""))
#par(cex=1.3)
#par(mfrow=c(2,2))
#for (p in 1:17){
#	resTempP=resTemp[resTemp$Parm==p,]
#	resTempP=resTempP[sort.list(resTempP$ICF),]
#	for (i in c(7:8,11:15)){
#		interaction.plot((resTempP$FittedIC),(resTempP$T),resTempP[,i],legend=T,   lty=c(1:2),lwd=2, col=c(1:2),#  ylim=c(.0,.23),
#		trace.label = "FittedIC",   #deparse(substitute(trace.factor)),
#		fixed=TRUE,
#		type="b",
#		ylab=outcomes[i-6],
#		xlab="Fitted IC",
#		main=paste(modelName," ",allICspecTRUE[icT],"_",parNames[p],sep="")
#		)
#	}
#}
#dev.off()

#1: Meausrement Parameters
#2: Process Noise Parameters
#3: Autoregression Parameters
titleLab=c("Model Implied","Free Parameter","Null","Diffuse") #,"Koopman EKF",expression(paste("Large ",kappa,sep="")))
parType=c("Meas","PN","AR")


#Mean(resTemp[resTemp$FittedIC=="FreeParm",i])
#Mean(resTemp[resTemp$ICF==5,i])
#which(resTemp[resTemp$FittedIC=="FreeParm",i]==0)
#resTemp[which(resTemp[,i]==0),]
#resTemp[resTemp[resTemp$FittedIC=="FreeParm",i]==0,]
	
if(small|model==1){
for (ti in 1:length(trueI)){
	icT=trueI[ti]
	for (ptype in 1:length(parType)){
		resTemp=newResults[newResults$TrueIC==allICspecTRUE[icT],]
		resTemp=resTemp[resTemp$byParm==ptype,]
		#resTemp=resTemp[resTemp$FittedIC!="Null",]
		#i=7 8 11 12 13 14 15
		for (i in c(7:8,11:15)){
			pdf (file = paste(outputDiri,"/plots/plot",modelName,"_",allICspecTRUE[icT],"Type_",parType[ptype],"Outcome_",outcomes[i-6],small1,states1,".pdf",sep="",collapse=""))
			par(cex=1.3)
			interaction.plot((resTemp$ICFlab),(resTemp$T),resTemp[,i],
				#fun=Mean,
				if(i==7){
				fun=fun1
					} else{
					fun=Mean
					}
 				
				,legend=F,   lty=c(1:2),lwd=2, col=c(2,4),#  ylim=c(.0,.23),
			fixed=TRUE,
			type="b",
			ylab=outcomes[i-6],
			xlab="Fitted IC",
			#main=paste(allICspecTRUE[icT],"_",parType[ptype],sep="")
			#main=expression(paste("Large ",kappa,sep=""))
			main=paste("True Initial Condition: ",titleLab[icT],sep="")
			)
			dev.off()
		}#end i loop
	}#end ptype loop
}#end ti loop
}
if(!small&model==2){
newResults=newResults[(newResults$T==200),]
for (ti in 1:length(trueI)){
	icT=trueI[ti]
	for (ptype in 1:length(parType)){
		resTemp=newResults[newResults$TrueIC==allICspecTRUE[icT],]
		resTemp=resTemp[resTemp$byParm==ptype,]
		#resTemp=resTemp[resTemp$FittedIC!="Null",]
		#i=7 8 11 12 13 14 15
		for (i in c(7:8,11:15)){
			pdf (file = paste(outputDiri,"/plots/plot",modelName,"_",allICspecTRUE[icT],"Type_",parType[ptype],"Outcome_",outcomes[i-6],small1,states1,".pdf",sep="",collapse=""))
			par(cex=1.3)
			interaction.plot((resTemp$ICFlab),resTemp$T,resTemp[,i],
				if(i==7){
				fun=fun1
					} else{
					fun=Mean
					}
				,legend=F,   pch=("2"),lty=c(2),lwd=2, col=c(4),#  ylim=c(.0,.23),
			fixed=TRUE,
			type="b",
			ylab=outcomes[i-6],
			xlab="Fitted IC",
			#main=paste(allICspecTRUE[icT],"_",parType[ptype],sep="")
			#main=expression(paste("Large ",kappa,sep=""))
			main=paste("True Initial Condition: ",titleLab[icT],sep="")
			)
			dev.off()
		}#end i loop
	}#end ptype loop
}#end ti loop
}

#which(is.na(newResults[newResults$TrueIC=="deJong",7]))
#mean(as.numeric((newResults[newResults$TrueIC=="deJong"&newResults$FittedIC=="deJong"&newResults$byParm==3,7])))
#Mean(resTemp[resTemp$FittedIC=="deJong",7])

#mean(as.numeric((newResults[newResults$TrueIC=="deJong"&newResults$FittedIC=="deJong"&newResults$Parm==1,7])))

#mean(as.numeric((newResults[newResults$TrueIC=="deJong"&newResults$FittedIC=="FreeParm"&newResults$Parm==5,7])))

#Bias(newResults[newResults$TrueIC=="deJong"&newResults$FittedIC=="deJong"&newResults$Parm==9,1],popValues[9])

#fun1=function(x){abs(mean(x))}
#windows()

#resTemp=newResults[newResults$TrueIC==allICspecTRUE[icT],]
#		resTemp=resTemp[resTemp$byParm==ptype,]
#		resTemp=resTemp[resTemp$FittedIC!="Null",]
		
#interaction.plot(resTemp$FittedIC,resTemp$T,resTemp[,7],fun=fun1)

			
			
		
#newResults[newResults$Parm==18,]

#Mean(resTemp[resTemp$T==200&resTemp$TrueIC=="deJong"&resTemp$FittedIC=="deJong"&resTemp$Parm==5,1])

#length(resTemp[resTemp$T==200&resTemp$TrueIC=="deJong"&resTemp$FittedIC=="FreeParm"&resTemp$Parm==5,1])

#plot((resTemp[resTemp$T==200&resTemp$TrueIC=="deJong"&resTemp$FittedIC=="deJong"&resTemp$Parm==5,1]))

#which(newResults[newResults$TrueIC=="deJong"&newResults$FittedIC=="deJong"&newResults$byParm==2,7]>1)
#dee=newResults[newResults$TrueIC=="deJong"&newResults$FittedIC=="deJong"&newResults$byParm==2,]
#dee[1603,]
#Mean(newResults[newResults$TrueIC=="deJong"&newResults$FittedIC=="FreeParm"&newResults$byParm==2,7])


#check=newResults[newResults$TrueIC=="deJong"&newResults$FittedIC=="FreeParm"&newResults$T==200,]
#length(which(newResults$T==200))
#check1=resTemp[resTemp$TrueIC=="deJong"&resTemp$FittedIC=="FreeParm",]
#check1[check1$T==200,]

#resTempP[resTempP$FittedIC=="FreeParm",11]
#Mean(resTempP[resTempP$T==200&resTempP$FittedIC=="FreeParm",7])
#Mean(resTempP[resTempP$T==200&resTempP$ICF==2,7])
#Mean(resTempP[resTempP$T==20&resTempP$ICF==6,7])

#plot(newResults[,1],newResults[,2],type="n", axes=F,ann=F)
#legend("center",cex=1.5,c("Free Parameter","Null","de Jong DKF"),pch=c("1","2","3"),lty=1:3,bty="n",lwd=2)
#legend("center",cex=1.5,c("Koopman EKF","Large Kappa"),pch=c("4","5"),lty=4:5,bty="n",lwd=2)

#pdf (file = paste(outputDiri,"/plots/legend.pdf",sep="",collapse=""))
#plot(newResults[,1],newResults[,2],type="n", axes=F,ann=F)
#legend("center",cex=1.4,c("Intensive Repeated Measures Data: T=50, N=20","Panel Data: T=5, N=200"),pch=c("1","2"),lty=1:2,bty="n",col=c(2,4),lwd=2)
#dev.off()
