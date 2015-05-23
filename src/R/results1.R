rm(list=ls(all=TRUE))
library('xtable')
source="C:/Documents and Settings/Diane Losardo/Desktop/dissertation/Dissertation/Dissertation" #where source files are located
source(paste(source,"/functions.R",sep=""))
outputDiri="C:/Documents and Settings/Diane Losardo/Desktop/dissertation/results/simResults" #where SAS outputs results and I keep results

sampleSizes = c(200)#sample sizes
theTs = c(5)		#number of time points

#For next two variables::
# 1=Model Implied, 2=Free Parameter, 3=Null Initial Condition,
# 4=deJong DKF, 5=Koopman EKF, 6=large kappa approx
trueI=2		#True Initial Condition
fittedI=c(1,2,3,4,5,6)	#Fittedn Initial Condition
model=1		#1=PFA Stationary, 2=PFA non-stationary, 3=PFA with local linear trend
allICspecTRUE=c("ModelImplied","FreeParm","Null","deJong","EKF","largeK")
ICspecTRUE=allICspecTRUE[trueI]
popValuesi=list(c(1.2,.8,.9,1.1,1,.4,1,.5,-.3,-.1,.6,.8,.6,2,1,1.5,.4),
			c(1.2,.8,.9,1.1,1,.4,1,1.2,.6,-.4,1,.8,.6,2,1,1.5,.4),
			c(1.2,.8,1,.8,.4,.5,.8,.6,2))
popValues=popValuesi[[model]]
modelNamei=c("PFAstat","PFAnons","PFALLT")
modelName=modelNamei[model]

file = paste(outputDiri,"/M",modelName,"ICT_",ICspecTRUE,"ICF_All.dat",sep="")
outputs = read.table(file)
noPars=17
outputsnm=outputs[(!is.nan(outputs[,4])),]
outputsnm=outputsnm[(!is.na(outputsnm[,4])),]
NC=length(which(is.nan(outputs[,4]),))
length(which(is.na(outputsnm[,4]),))

sumstatsALL=NULL
ostatsALL=NULL
ss=1
	np=sampleSizes[ss]
	nt=theTs[ss]
for (m in 1:length(fittedI)){
	sumstats=NULL
	otherstats=NULL
	mm=fittedI[m]
	outputsT=outputsnm[outputsnm[,3]==allICspecTRUE[mm],]
	for (p in 7:(noPars+6)){
		meantemp = Mean(outputsT[,p])
		sdtemp = SD(outputsT[,p])
		meanSE = Mean(outputsT[,p+noPars])
		tempbias = Bias(outputsT[,p],popValues[(p-6)])
		temprelbias = relBias(outputsT[,p],popValues[(p-6)])
		tempRMSE = RMSE(outputsT[,p],popValues[(p-6)])
		
		cover = matrix(NA,nrow(outputsT),1)
		power = matrix(NA,nrow(outputsT),1)
		LLi = matrix(NA,nrow(outputsT),1)
		AICi= matrix(NA,nrow(outputsT),1)
		BICi = matrix(NA,nrow(outputsT),1)

			for (i in 1:nrow(outputsT)){
				templcl = lcl(outputsT[i,p],outputsT[i,(p+noPars)])
				tempucl = ucl(outputsT[i,p],outputsT[i,(p+noPars)])
				cover[i,1] = cov(templcl,tempucl,popValues[(p-6)])
				power[i,1] = pow(outputsT[i,p],outputsT[i,(p+noPars)])
				LLi[i,1]=FML(outputsT[i,(ncol(outputsT)-1)],np)
				AICi[i,1]=AIC(LLi[i,],noPars)
				BICi[i,1]=BIC(LLi[i,],noPars,np)
				} #end i loop

		percovtemp = Mean(cover[,1])
		perpowtemp = Mean(power[,1])
		timetemp= Mean(outputsT[,(ncol(outputsT))])
		sumstats = rbind(sumstats,c(allICspecTRUE[trueI],allICspecTRUE[mm],popValues[(p-6)],meantemp,sdtemp,meanSE,tempbias,
			temprelbias,tempRMSE,percovtemp,perpowtemp,timetemp))
	}#end p loop
		tempLL= Mean(LLi)
		tempAIC=Mean(AICi)
		tempBIC=Mean(BICi)
		otherstats=rbind(otherstats,c(tempLL,tempAIC,tempBIC))
	sumstatsALL=rbind(sumstatsALL,sumstats)
	ostatsALL=rbind(ostatsALL,otherstats)
}# end m loop


sumstatsALL[35:51,]