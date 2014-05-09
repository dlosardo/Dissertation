rm(list=ls(all=TRUE))
#sourcing functions
source("C:/Documents and Settings/Diane Losardo/Desktop/dissertation/Dissertation/Dissertation/functions.R")
source="C:/Documents and Settings/Diane Losardo/Desktop/dissertation/Dissertation/Dissertation" #where source files are located
wd="C:/Documents and Settings/Diane Losardo/Desktop/dissertation/datafiles" #where the batch files are located, outputting data
outputDiri="C:/Documents and Settings/Diane Losardo/Desktop/dissertation/results" #where SAS outputs results and I keep results
oxdir = "c:/Program Files/OxMetrics5/Ox/bin"
opname = "outputs"

AR1=FALSE
noMC = 50 #number of Monte Carlo runs
convALL=matrix(NA,noMC,15)
#sampleSizes = c(200,20)#sample sizes
#theTs = c(5,50)		#number of time points
sampleSizes = c(200)#sample sizes
theTs = c(5)		#number of time points

#For next two variables::
# 1=Model Implied, 2=Free Parameter, 3=Null Initial Condition,
# 4=diffuse deJong DKF, 5=Koopman EKF, 6=large kappa approx
trueI=4		#True Initial Condition
fittedI=c(2)	#Fitted Initial Condition
INIT=TRUE	#If true, then specify an exact initial condition
model=2		#1=PFA Stationary, 2=PFA non-stationary, 3=PFA with local linear trend
#popValuesi=list(c(1.2,.8,.9,1.1,1,.4,1,.5,-.1,-.3,.6,.8,.6,2,1,1.5,.4),
#			c(1.2,.8,.9,1.1,1,.4,1,1.2,-.4,.6,1,.8,.6,2,1,1.5,.4),
#			c(1.2,.8,1,.8,.4,.5,.8,.6,2))
popValuesi=list(c(1.2,.8,.9,1.1,1,.4,1,.5,-.1,-.3,.6,.8,.6,2,1,1.5,.4),
			c(1.2,.8,.9,1.1,
				1,.4,1,
				#.05,.02,.05,
			#.005,.002,005,
				1.2,-.4,.6,1,
			#1.2,-.4,.6,.7,
			#.1,.1,.1,.1,.1,.1),	
				.8,.6,2,1,1.5,.4),
			c(1.2,.8,1,.8,.4,.5,.8,.6,2))
popValues=popValuesi[[model]]
#corr=-.04/(sqrt(.83)*sqrt(.83))
#nparms=length(popValues)
#source(paste(source,"/controlVars.R",sep=""))
#StatCheck(T)
#source(paste(source,"/Simulate.R",sep=""))
#plotsF(nt,1,np,yntonly,2,stand=FALSE,yname="Y1",xname="Time",title="Test",lowerY=min(yntonly[,1]),upperY=max(yntonly[,1]))

allICspecTRUE=c("ModelImplied","FreeParm","Null","deJong","EKF","largeK")
ICspecTRUE=allICspecTRUE[trueI]

modelNamei=c("PFAstat","PFAnons","PFALLT")
modelName=modelNamei[model]
modelBatchi=c("PFA","PFA","LLT")
modelBatch=modelBatchi[model]

if (model==1|model==2){
 	maxParms=22
 	ne=2
	ny=6} else{
 	maxParms=14
	ne=3
	ny=3
	trueIstat=1
	}
MCfile=matrix(NA,noMC*length(sampleSizes)*length(fittedI),(maxParms*2)+12)
#for (ss in 1:length(sampleSizes)){
ss=1
	np=sampleSizes[ss]
	nt=theTs[ss]
	#for (run in 1:noMC){
		nparms=length(popValues)
		#sourcing control variables for simulation
		source(paste(source,"/controlVars.R",sep=""))
		#sourcing actual data simulation
		source(paste(source,"/Simulate.R",sep=""))

		#reading out data
		filedata=paste(wd,"/PFAdataDIFFsmall1.dat",sep="")
		write(t(Wide1),file=filedata,ncolumn=(nt*ny),append=FALSE)

		#generating random values to add to starting values
		#thetaSV=round(runif(nparms),2)
		thetaSV=round(rnorm(nparms),2)
		if (model==3){
			thetaSV[c(3:5)]=abs(thetaSV[c(3:5)])
			}else{
		thetaSV[c(5,7,12:17)]=abs(thetaSV[c(5,7,12:17)])
		}
		#thetaSV[c(5,7,12:17)]=round(log(thetaSV[c(5,7,12:17)]),2)
		SVs=thetaSV+popValues

		if(any(fittedI %in% 2)){
			if (model==3){
				addedSVs=c(.1,1.2)
				thetaAdd=round(rnorm(length(addedSVs)),2)
				thetaAdd[c(2)]=abs(thetaAdd[c(2)])
				SVs1=thetaAdd+addedSVs
				}else{
			addedSVs=c(1,1.5,1.2,.3,.7)
			thetaAdd=round(rnorm(length(addedSVs)),2)
			thetaAdd[c(3,5)]=abs(thetaAdd[c(3,5)])
			SVs1=thetaAdd+addedSVs
				}
			}
				
		for (m in 1:length(fittedI)){
			nparms=length(popValues)
			ICfitted=allICspecTRUE[fittedI[m]]
			print(paste(c(" iter ", run," model ", ICfitted)))
			popValuesM=popValues
			if (fittedI[m]==2){
				if (model==3){
					popValuesM=c(popValuesM,.1,1.2)
					}else{
					popValuesM=c(popValuesM,1,1.5,1.2,.3,.7)
					}
				}
			nparms=length(popValuesM)
 						
			fileOX = paste(wd,"/fileOX.ox",sep="")
			if (model==3){source(paste(source,"/compileOXcodeLLT.R",sep=""))
				}else{
				source(paste(source,"/compileOXcode.R",sep=""))
				}
			batchOx = shQuote(paste(wd,"/oxopen.bat",sep=""))
			#batchfile = paste(wd,"/oxopen.bat",sep="")
			#input = paste(wd,"/fileOX.ox",sep="")
			#output = paste(wd,"/ox.out",sep="")
			#input = gsub("/", "\\\\", input)
			#output = gsub("/", "\\\\", output)
			#oxdir = gsub("/","\\\\",oxdir)
			#batchOX(oxdir,input,output,batchfile)
			time=system.time(system(batchOx,wait = TRUE))
			
			conv = scan(paste(wd,"/conv.txt",sep=""),what="") 
			#if (!is.na(pmatch("no",conv)) | length(conv)==0){stop("Error: Model did not converge in Ox")}
			#if (!is.na(pmatch("weak",conv))){print("Warning: Weak Convergence")}
			warnings = scan(paste(wd,"/ox.out",sep=""),what="")
			#For:Warning: invertgen: invertsym failed, proceeding with generalized p.s.d. inverse
			invertSym=NA
			warnings1=pmatch(c("invertsym", "failed"),warnings)
			if (all(!is.na(warnings1))){ 
			invertSym="failedInvertSym"}
			#For: Warning: invert: decomposition failed
			invertDec=NA
			warnings2=pmatch(c("decomposition", "failed"),warnings)
			if (all(!is.na(warnings2))){ 
			invertDec="failedDecomp"}
			
			warnings3=pmatch(c("error","memory"),warnings)
			if (all(!is.na(warnings2))){ 
			next}
			
			resParms=scan(paste(wd,"/results.mat",sep=""),what="")
			resParms=resParms[-c(1,2)]
			
			smoothed = scan(paste(wd,"/smoothed.mat",sep=""),what="")
			smoothed = smoothed[-c(1,2)]
			smoothedN=as.numeric(smoothed)
			smoothed1 = matrix(smoothedN,ncol=nt,byrow=TRUE)
	
		
			#Transforming to wide format for complete data set
			LVAll=NULL
			LVtemp=NULL
			addS=0
			for (i in 1:np){
				tempX1=smoothed1[1+addS,]
				tempX2=smoothed1[2+addS,]
				if (model==3){
					tempX3=smoothed1[3+addS,]
					}
				LVtemp=c(tempX1,tempX2)
				if (model==3){
					LVtemp=c(LVtemp,tempX3)
					}
				addS=addS+2
				if (model==3){
					addS=addS+1
					}
				LVAll=rbind(LVAll,LVtemp)
				}
			rmseLVs=RMSE(LVAll,All1)
			
			MCfile[(run+length(fittedI)*(noMC*(ss-1))+(noMC*(m-1))),1:6]=c(modelName,ICspecTRUE,ICfitted,rmseLVs,nt,np)
			MCfile[(run+length(fittedI)*(noMC*(ss-1))+(noMC*(m-1))),7:(length(resParms)+6)]=resParms
			MCfile[(run+length(fittedI)*(noMC*(ss-1))+(noMC*(m-1))),(length(resParms)+7):(length(resParms)+10)]=c(conv[1],invertSym,invertDec,time[3])
			tempRes=c(modelName,ICspecTRUE,ICfitted,rmseLVs,nt,np,resParms,conv[1],invertSym,invertDec,time[3])
			fileTempResults=paste(outputDiri,"/simResults/tempresults.dat",sep="")
			write(tempRes,file=fileTempResults,ncolumn=(length(tempRes)),append=TRUE)
			convALL[run,1:length(conv)]=conv
		} #end m loop
	} #end run loop
} #end ss loop

#writing out results
#fileSimResults=paste(outputDiri,"/simResults/M",modelName,"ICT_",ICspecTRUE,"ICF_ALL.dat",sep="")
#write(t(MCfile),file=fileSimResults,ncolumn=(ncol(MCfile)),append=FALSE)

plotsF(nt,1,np,yntonly,3,stand=FALSE,yname="Y1",xname="Time",title="Test",lowerY=min(yntonly[,3]),upperY=max(yntonly[,3]))
plotsF(nt,1,np,all,1,stand=FALSE,yname="Y1",xname="Time",title="Test",lowerY=min(all[,1]),upperY=max(all[,1]))

which(outputs[MCfile[,3]=="deJong",41]!=outputs[outputs$V3=="deJong",41])

ekf=MCfile[MCfile[,3]=="EKF",]
dj=MCfile[MCfile[,3]=="deJong",]
rbind(ekf[8,],dj[8,])
rbind(ekf[diffs[3],],dj[diffs[3],])
dlik=outputs[outputs$V3=="deJong",41]
klik=outputs[outputs$V3=="EKF",41]

diffs=which(abs(dlik-klik)>.1)