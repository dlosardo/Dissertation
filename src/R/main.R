rm(list=ls(all=TRUE))
#sourcing functions
source("C:/Documents and Settings/Diane Losardo/Desktop/dissertation/Dissertation/Dissertation/functions.R")

AR1=FALSE
noMC = 1 #number of Monte Carlo runs
sampleSizes = c(200,20)#sample sizes
theTs = c(5,50)		#number of time points
sampleSizes = c(20)#sample sizes
theTs = c(50)		#number of time points

#For next two variables::
# 1=Model Implied, 2=Free Parameter, 3=Null Initial Condition,
# 4=diffuse deJong DKF, 5=Koopman EKF, 6=large kappa approx
trueI=3		#True Initial Condition
fittedI=c(1)	#Fitted Initial Condition
INIT=TRUE	#If true, then specify an exact initial condition
CONS=TRUE #If true, then apply exponential constraints
model=1		#1=PFA Stationary, 2=PFA non-stationary, 3=PFA with local linear trend
source="C:/Documents and Settings/Diane Losardo/Desktop/dissertation/Dissertation/Dissertation" #where source files are located
wd="C:/Documents and Settings/Diane Losardo/Desktop/dissertation/datafiles" #where the batch files are located, outputting data
outputDiri="C:/Documents and Settings/Diane Losardo/Desktop/dissertation/results" #where SAS outputs results and I keep results
#popValuesi=list(c(1.2,.8,.9,1.1,1,.4,1,.5,-.1,-.3,.6,.8,.6,2,1,1.5,.4),
#			c(1.2,.8,.9,1.1,1,.4,1,1.2,-.4,.6,1,.8,.6,2,1,1.5,.4),
#			c(1.2,.8,1,.8,.4,.5,.8,.6,2))
popValuesi=list(c(1.2,.8,.9,1.1,1,.4,1,.5,-.1,-.3,.6,.8,.6,2,1,1.5,.4),
			c(1.2,.8,.9,1.1,
				.05,.02,.05,
				1.2,-.4,.6,.6,.8,.6,2,1,1.5,.4),
			c(1.2,.8,1,.8,.4,.5,.8,.6,2))
popValues=popValuesi[[model]]
#corr=.02/(sqrt(.05)*sqrt(.05))
#		source(paste(source,"/controlVars.R",sep=""))
#		#sourcing actual data simulation
#		source(paste(source,"/Simulate.R",sep=""))
#allEV=NULL
#for (i in 1:ny){
#	temp=popValues[5]/var(yntonly[1:50,i])
#	allEV=c(allEV,temp)
#}
#allEV
#var(all[1:50,1])/var(yntonly[1:50,1])

allICspecTRUE=c("ModelImplied","FreeParm","Null","deJong","EKF","largeK")
ICspecTRUE=allICspecTRUE[trueI]

modelNamei=c("PFAstat","PFAnons","PFALLT")
modelName=modelNamei[model]
modelBatchi=c("PFA","PFA","LLT")
modelBatch=modelBatchi[model]

#setting up MCfile
#MClabs = paste("MCfile",1:length(fittedI),sep="")
#for (j in 1:length(fittedI)){
#	temp = NULL
#	temp = paste(MClabs[j],sep="")
#	assign(MClabs[j],temp)
#	}
#allMCs = NULL
#for (i in 1:length(fittedI)){
#	temp = get(MClabs[i])
#	allMCs = c(allMCs,temp)
#}


maxParms=22
#assign(allMCs[m],matrix(NA,noMC*length(sampleSizes)*length(fittedI),(nparms*2)+8))
MCfile=matrix(NA,noMC*length(sampleSizes)*length(fittedI),(maxParms*2)+8)
for (ss in 1:length(sampleSizes)){
	np=sampleSizes[ss]
	nt=theTs[ss]
	for (run in 1:noMC){
		nparms=length(popValues)
		#sourcing control variables for simulation
		source(paste(source,"/controlVars.R",sep=""))
		#sourcing actual data simulation
		source(paste(source,"/Simulate.R",sep=""))

		#reading out data
		filedata=paste(wd,"/PFAdata.dat",sep="")
		write(t(Wide1),file=filedata,ncolumn=(nt*ny),append=FALSE)

		#fileTRUE=paste(outputDiri,"/true/M",modelName,"ICT_",ICspecTRUE,"ICF_",ICfitted,"N",np,"T",nt,".dat",sep="")
		#write(t(All1),file=fileTRUE,ncolumn=(nt*ne),append=TRUE)
		
		#generating random values to add to starting values
		thetaSV=round(rnorm(nparms),2)
		thetaSV[c(5,7,12:17)]=abs(thetaSV[c(5,7,12:17)])
		SVs=thetaSV+popValues

		if(any(fittedI %in% 2)){
			addedSVs=c(1,1.5,1.2,.3,.7)
			thetaAdd=rnorm(length(addedSVs))
			thetaAdd[c(3,5)]=abs(thetaAdd[c(3,5)])
			SVs1=thetaAdd+addedSVs
			}
				
		for (m in 1:length(fittedI)){
			nparms=length(popValues)
			ICfitted=allICspecTRUE[fittedI[m]]
			print(paste(c(" iter ", run," model ", ICfitted)))
			popValuesM=popValues
			if (fittedI[m]==2){
				popValuesM=c(popValuesM,1,1.5,1.2,.3,.7)
				}
			nparms=length(popValuesM)
 						
			#Running model in SAS
			fileCODE=paste(wd,"/simTemp.sas",sep="")
			source(paste(source,"/compileSAScode.R",sep=""))
### NEED TO GET ORIGINAL duToitCodeDFAFreeParmN20T50PFAstat.sas
			#toOpen=paste("/",ICfitted,modelBatch,"N",np,"T",nt,modelName,".bat",sep="")
			#batchSAS = shQuote(paste(wd,toOpen,sep=""))
			batchSAS = shQuote(paste(wd,"/openSAS.bat",sep=""))
			system(batchSAS,wait = TRUE)
		
			#Reading in latent variable scores
			outputLVDir=paste(outputDiri,"/smoothN",np,"T",nt,".dat",sep="")
			lvs=matrix(scan(outputLVDir),nt*np,ne,byrow=TRUE)
			#fileLV=paste(outputDiri,"/lv/M",modelName,"ICT_",ICspecTRUE,"ICF_",ICfitted,"N",np,"T",nt,".dat",sep="")
			if (length(lvs)==0){
 				rmseLVs="did not converge"
				#write(t(LVAll),file=fileLV,append=TRUE)
				}
			else{
				#Transforming to wide format for complete data set
				LVAll=NULL
				LVtemp=NULL
				for (i in 1:np){
					tempX1=lvs[(1+(nt*(i-1))):(i*nt),1]
					tempX2=lvs[(1+(nt*(i-1))):(i*nt),2]
					LVtemp=c(tempX1,tempX2)
					LVAll=rbind(LVAll,LVtemp)
					}
				#write(t(LVAll),file=fileLV,ncolumn=(nt*ne),append=TRUE)
				rmseLVs=RMSE(LVAll,All1)
				}
			#Reading in data
			outputDir=paste(outputDiri,"/estimatesN",np,"T",nt,".dat",sep="")
			results=as.vector(scan(outputDir))
			
			MCfile[(run+length(fittedI)*(noMC*(ss-1))+(noMC*(m-1))),1:4]=c(modelName,ICspecTRUE,ICfitted,rmseLVs)
			MCfile[(run+length(fittedI)*(noMC*(ss-1))+(noMC*(m-1))),5:(length(results)+4)]=results
			#MC=get(allMCs[m])
			#MC[(run+(noMC*(ss-1))),1:4]=c(modelName,ICspecTRUE,ICfitted,rmseLVs)
			#MC[(run+(noMC*(ss-1))),5:(length(results)+4)]=results
			#assign((allMCs[m]),MC)
		} #end m loop
	} #end run loop
} #end ss loop

#writing out results
#fileSimResults=paste(outputDiri,"/simResults/M",modelName,"ICT_",ICspecTRUE,"ICF_ALL.dat",sep="")
#write(t(MCfile),file=fileSimResults,ncolumn=(ncol(MCfile)),append=FALSE)

#plotsF(nt,1,np,yntonly,6,stand=FALSE,yname="Y1",xname="Time",title="Test",lowerY=min(yntonly[,1]),upperY=max(yntonly[,1]))

