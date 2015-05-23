#####################################
# Compile results from dissertation #
# Author: Diane Losardo             #
#####################################

# Installing and including libraries
# install.packages("xtable")
# install.packages("outliers")
library('xtable')
library('outliers')

################################################################
#### CHANGE THESE APPROPRIATELY FOR DIFFERENT SIMULATION RUNS ##
NO_MC = 500 # Number of Monte Carlo runs
SAMPLE_SIZES = c(200, 20) # Sample sizes
THE_TS = c(5, 50) # Number of time points

MODEL = 2  	# 1 = PFA Stationary, 2 = PFA non-stationary, 3 = PFA with local linear trend
# For next two variables:
# 1=Model Implied, 2=Free Parameter, 3=Null Initial Condition,
# 4=deJong DKF, 5=Koopman EKF, 6=large kappa approx
TRUE_I = 4    # True Initial Condition
FITTED_I = c(2,3,4,5,6)	# Fitted Initial Condition
SMALL_POP = FALSE # FALSE if using mild-mod nonstatinary condition, TRUE for very mild nonstationary condition
MOD_POP = FALSE # TRUE if using moderately nonstationary condition
DIFF_ONLY = FALSE #TRUE if only looking at the diffuse approaches that all converged
################################################################
################################################################

PROJ_DIR = "/Users/dlosardo/Documents/workspace/Dissertation" # where files for project are located
RESULTS_DIR = "/Users/dlosardo/Documents/workspace/Dissertation/simResults/PC" # where results are stored
NY = 6 # number of y variables

# Sourcing functions
source(paste(PROJ_DIR, "/functions.R", sep = "" ))

# Variables corresponding to a given simulation run 
noMC = NO_MC # Number of Monte Carlo runs
sampleSizes = SAMPLE_SIZES # Sample sizes
theTs = THE_TS    # Number of time points
ny= NY # number of y variables

model = MODEL  	# 1 = PFA Stationary, 2 = PFA non-stationary, 3 = PFA with local linear trend
# For next two variables:
# 1=Model Implied, 2=Free Parameter, 3=Null Initial Condition,
# 4=deJong DKF, 5=Koopman EKF, 6=large kappa approx
trueI = TRUE_I  	# True Initial Condition
fittedI = FITTED_I	# Fitted Initial Condition
smallpop = SMALL_POP
small = NULL
if (smallpop){
  small = "small"
}
modpop = MOD_POP
mod = NULL
if (modpop){
  mod = "MOD"
}
diffOnly <- DIFF_ONLY
jDiff = NULL
if (diffOnly){
  jDiff = "DIFFONLY"
}
allICspecTRUE = c("ModelImplied","FreeParm","Null","deJong","EKF","largeK")
modelNamei = c("PFAstat","PFAnons","PFALLT")
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
parNames=c("Z21","Z31","Z52","Z62","V11","V21","V22","T11","T21","T12","T22","U11","U22","U33","U44","U55","U66",
           "X01","X02","P011","P012","P022")

currentFitted = allICspecTRUE[fittedI]
ICspecTRUE = allICspecTRUE[trueI]
popValuesi=list(c(1.2,.8,.9,1.1,1,.4,1,.5,-.3,-.1,.6,.8,.6,2,1,1.5,.4),
                c(1.2,.8,.9,1.1,
                  .5,.1,.5,
                  1.2,-.4,.6,.7,
                  .8,.6,2,1,1.5,.4),
                c(1.2,.8,1,.8,.4,.5,.8,.6,2))

if (smallpop){popValuesi=list(c(1.2,.8,.9,1.1,1,.4,1,.5,-.3,-.1,.6,.8,.6,2,1,1.5,.4),
                              c(1.2,.8,.9,1.1,
                                .05,.02,.05,
                                1.2,-.4,.6, .7,
                                .8,.6,2,1,1.5,.4),
                              c(1.2,.8,1,.8,.4,.5,.8,.6,2))}
if (modpop){popValuesi=list(c(1.2,.8,.9,1.1,1,.4,1,.5,-.3,-.1,.6,.8,.6,2,1,1.5,.4),
                              c(1.2,.8,.9,1.1,
                                1,.4,1,
                                1.2,-.4,.6, .7,
                                .8,.6,2,1,1.5,.4),
                              c(1.2,.8,1,.8,.4,.5,.8,.6,2))}
popValues = popValuesi[[model]]
modelName = modelNamei[model]

file = paste(RESULTS_DIR,"/M",modelName,"ICT_",ICspecTRUE,"ICF_All", mod, ".dat",sep="")

nCol <- max(count.fields(file, sep = " "))
outputs <- read.table(file,stringsAsFactors=FALSE,fill=TRUE,col.names=1:nCol)

#o <- matrix(readLines(file))
#strsplit(o[1], " ")
#outputs = read.table(file,stringsAsFactors=FALSE)
#outputs = outputs[, -ncol(outputs)]
# check to see if any rows of all NAs
any(rowSums(is.na(outputs)) >= ncol(outputs)-1)

convStats=NULL
for (dp in 1:length(sampleSizes)){
  tempDP = outputs[outputs[, 5] == theTs[dp], ]
  for (i in 1:length(fittedI)){
    m = currentFitted[i]
    tempModel = tempDP[tempDP[, 3] == m, ]
    tempModel = tempModel[1:noMC, ]
    if (m == "FreeParm"){
      NANtemp = length(which(is.nan(tempModel[, 4]),)) + length(which(tempModel[, 4] == "Inf"))
      convTempStrong = length(which(tempModel[, 52] == "Strong"))
      convTempNo = length(which(tempModel[, 52] == "No"))
      convTempWeak = length(which(tempModel[, 52] == "Weak"))
      invertSymTemp = length(which(tempModel[, 53] == "failedInvertSym"))
      invertDecTemp = length(which(tempModel[, 54] == "failedDecomp"))
      noSE = length(which(tempModel$X35 == "."))
      temp = c(modelName, ICspecTRUE, m, sampleSizes[dp], theTs[dp], NANtemp, convTempStrong,
               convTempWeak, convTempNo, invertSymTemp, invertDecTemp, noSE)
      convStats = rbind(convStats,temp)
    } else {
      NANtemp = length(which(is.nan(tempModel[, 4]),)) + length(which(tempModel[, 4] == "Inf"))
      convTempStrong = length(which(tempModel[, 42] == "Strong"))
      convTempNo = length(which(tempModel[, 42] == "No"))
      convTempWeak = length(which(tempModel[, 42] == "Weak"))
      invertSymTemp = length(which(tempModel[, 43] == "failedInvertSym"))
      invertDecTemp = length(which(tempModel[, 44] == "failedDecomp"))
      noSE = length(which(tempModel$X25 == "."))
      temp = c(modelName, ICspecTRUE, m, sampleSizes[dp], theTs[dp], NANtemp, convTempStrong,
               convTempWeak, convTempNo, invertSymTemp, invertDecTemp, noSE)
      convStats = rbind(convStats, temp)
    }
  } # end fitted model loop
} # end data points loop

toKeep = NULL
for (i in 1:length(fittedI)){
  m = currentFitted[i]
  tempModel = outputs[outputs[, 3] == m, ]
  tempModel = tempModel[1:(noMC*2), ]
  tempModel$rep = c(1:noMC, 1:noMC)
  if (m == "FreeParm"){
    toKeep = rbind(toKeep, tempModel[which((tempModel[, 4] != "Inf") & (!is.nan(tempModel[, 4])) & 
                                             (tempModel[, 52] == "Strong") & (tempModel$X35 != ".") & !is.na(tempModel[, 3]) &
                                             is.na(tempModel[, 53])), ])
  } else {
    toKeep = rbind(toKeep, tempModel[which((tempModel[, 4] != "Inf") & (!is.nan(tempModel[, 4])) &
                            (tempModel[, 42]=="Strong") & (tempModel$X25 != ".") &                                                
                                             !is.na(tempModel[, 3]) &
                                             is.na(tempModel[, 43])), ])
  }
}
if (diffOnly){
  dKeep = NULL
  for (i in 1:length(fittedI)){
    m = currentFitted[i]
    tempModel = toKeep[toKeep[, 3] == m, ]
    if (m == "FreeParm"){
      dKeep = rbind(dKeep, tempModel[which((tempModel[, 4] != "Inf") & (!is.nan(tempModel[, 4])) & 
                                               (tempModel[, 52] == "Strong") & (tempModel$X35 != ".") & !is.na(tempModel[, 3]) &
                                               is.na(tempModel[, 53])), ])
    } else {
      dKeep = rbind(dKeep, tempModel[which((tempModel[, 4] != "Inf") & (!is.nan(tempModel[, 4])) &
                                               (tempModel[, 42]=="Strong") & (tempModel$X25 != ".") &
                                               !is.na(tempModel[, 3]) &
                                               is.na(tempModel[, 43])), ])
    }
  }
  toKeep <- dKeep
  allConv = NULL
  for (dp in 1:length(sampleSizes)){
    tempDP = toKeep[toKeep[, 5] == theTs[dp], ]
    tempDiff = tempDP[tempDP[, 3] == "deJong" | tempDP[, 3] == "largeK" | tempDP[, 3] == "EKF", ]
    allRep <- tempDiff$rep
    toRemove <- as.numeric(names(which(table(allRep) < 3)))
    allConvTemp <- tempDiff[-(which(tempDiff$rep %in% toRemove)), ]
    allConv = rbind(allConv, allConvTemp)
  }
  fittedI <- c(4, 5, 6)
  currentFitted = allICspecTRUE[fittedI]
  toKeep <- allConv
} 
# check to see if any rows of all NAs
#any(rowSums(is.na(toKeep)) >= ncol(toKeep) - 1)
#any(rowSums(is.na(tempModel)) >= ncol(tempModel) - 1)
#which(rowSums(is.na(toKeep)) >= ncol(toKeep) - 1)

#dim(tempModel[(tempModel[, 4] != "Inf") & (!is.nan(tempModel[, 4])) &
#                (tempModel[, 42]=="Strong") & (tempModel$V25 != ".") & !is.na(tempModel[, 3]), ])

#which((tempModel[, 4] != "Inf") & (!is.nan(tempModel[, 4])) &
#        (tempModel[, 42]=="Strong") & (tempModel$V25 != ".") & !is.na(tempModel[, 3]))

outputs <- toKeep
outs=NULL
for (dp in 1:length(sampleSizes)){
  tempDP = outputs[outputs[, 5] == theTs[dp], ]
  for (i in 1:length(fittedI)){
    m = currentFitted[i]
    tempModel = tempDP[tempDP[, 3] == m, ]
    if (m == "FreeParm"){
      popValues = c(popValuesi[[model]],1,.5,1.2,.3,.7)
    } else {
      popValues = popValuesi[[model]]
    }
    noPars = length(popValues)	
    if (nrow(tempModel) == 0) next
    numbsTemp = lapply(tempModel[, c(7:(noPars * 2 + 1 + 6), 4)], as.numeric)
    numbsTemp = as.data.frame(numbsTemp)
    if(any(rowSums(is.na(numbsTemp)) >= ncol(numbsTemp)-1)){
      numbsTemp = numbsTemp[-which(rowSums(is.na(numbsTemp)) >= (ncol(numbsTemp)-1)),]
    }
    outsALL=NULL
    for (oo in 1:noPars){
      outsT = outDET1(numbsTemp[, oo])
      outsALL = c(outsALL, outsT)
    }
    outU = unique(outsALL)
    
    outN = length(outU)
    outs = rbind(outs, c(modelName, ICspecTRUE, m, sampleSizes[dp], theTs[dp], outN))
  }
}

convStats=cbind(convStats, outs[, 6])	
colnames(convStats) = c("Model", "TrueIC", "ICFit", "np", "nt", "NAN", "StrongConv", 
                        "WeakConv", "NoConv", "failedInvertSym", "failedDecomp", 
                        "noSE", "outlier")

convDir = paste(RESULTS_DIR, "/ConvStats_M", modelName, "TIC", ICspecTRUE, small, mod, jDiff,".txt",sep="")
write.table(convStats, file=convDir, append=FALSE, col.names = TRUE)

sumstatsALL = NULL
ostatsALL = NULL
forANOVA = NULL
AICbest = matrix(NA, noMC*length(sampleSizes), length(fittedI))
BICbest = matrix(NA, noMC*length(sampleSizes), length(fittedI))

for (dp in 1:length(sampleSizes)){
  tempDP = outputs[outputs[, 5] == theTs[dp], ]
  for (i in 1:length(fittedI)){
    m = currentFitted[i]
    tempModel = tempDP[tempDP[, 3] == m, ]
    sumstats = NULL
    otherstats = NULL
    if (m == "FreeParm"){
      popValues = c(popValuesi[[model]],1,.5,1.2,.3,.7)
      if (model == 2){
        colnames(tempModel) = c(PFAcolnamesFP, "rep")
      } else if (model == 1){
        colnames(tempModel) = c(PFAcolnamesFP, "", "rep")
      }
    } else {
      popValues = popValuesi[[model]]
      colnames(tempModel) = c(PFAcolnames)
      colnames(tempModel)[ncol(tempModel)] = "rep"
    }
    noPars = length(popValues)
    if (nrow(tempModel) == 0) next
    numbsTemp = lapply(tempModel[, c(7:(noPars * 2 + 1 + 6), 4, which(names(tempModel) == "rep"),
                                     which(names(tempModel) == "LL"),
                                     which(names(tempModel) == "time"))], as.numeric)
    numbsTemp = as.data.frame(numbsTemp)
    if(any(rowSums(is.na(numbsTemp)) >= ncol(numbsTemp)-1)){
      numbsTemp = numbsTemp[-which(rowSums(is.na(numbsTemp)) >= (ncol(numbsTemp)-1)),]
    }
    AICbest[numbsTemp$rep + (dp - 1)*noMC, i] = AIC(numbsTemp$LL, noPars)
    BICbest[numbsTemp$rep + (dp - 1)*noMC, i] = BIC(numbsTemp$LL, noPars, sampleSizes[dp])
    
    #if (!smallpop){
    #  if (any(numbsTemp$V11 < .001)){
    #    numbsTemp = numbsTemp[-which((numbsTemp$V11) < .001), ]
    #  }
    #  if(any(numbsTemp$V22 < .001)){
    #    numbsTemp = numbsTemp[-which((numbsTemp$V22)<.001), ]
    #  }
    #}
    outsALL=NULL
    for (oo in 1:noPars){
      outs = outDET1(numbsTemp[, oo])
      outsALL = c(outsALL, outs)
    }
    outU = unique(outsALL)
    if (length(outU) > 0){
      numbsTemp = numbsTemp[-(outU), ]
    }
    if (nrow(numbsTemp) == 0) next
    for (p in 1:(noPars)){
      meantemp = Mean(numbsTemp[, p])
      sdtemp = SD(numbsTemp[, p])
      meanSE = Mean(numbsTemp[, p + noPars])
      tempbias = Bias(numbsTemp[, p], popValues[(p)])
      temprelbias = relBias(numbsTemp[, p], popValues[(p)])
      tempRMSE = RMSE(numbsTemp[, p], popValues[(p)])
      
      cover = matrix(NA,nrow(numbsTemp),1)
      power = matrix(NA,nrow(numbsTemp),1)
      #LLi = matrix(NA,nrow(numbsTemp),1)
      #RMSEAi = matrix(NA,nrow(numbsTemp),1)
      
      for (r in 1:nrow(numbsTemp)){
        if (is.na(numbsTemp[r, 1])) next
        if (is.na(numbsTemp[r, (p + noPars)])) next
        templcl = lcl(numbsTemp[r,p],numbsTemp[r, (p + noPars)])
        tempucl = ucl(numbsTemp[r,p],numbsTemp[r, (p + noPars)])
        cover[r,1] = cov(templcl, tempucl, popValues[(p)])
        if (!(numbsTemp[r,p] == 0 & numbsTemp[r, (p + noPars)] == 0)){
          power[r, 1] = pow(numbsTemp[r, p], numbsTemp[r, (p + noPars)])}
        else{
          power[r,1] = NA
        }
      } #end r loop
      
    percovtemp = Mean(cover[,1])
    perpowtemp = Mean(power[,1])
    timetemp= Mean(as.numeric(numbsTemp$time))		
    sumstats = rbind(sumstats, c(allICspecTRUE[trueI], m, sampleSizes[dp], theTs[dp], 
                                 popValues[(p)], meantemp, sdtemp, meanSE, tempbias,
                                temprelbias, tempRMSE, percovtemp, perpowtemp))
    }#end p loop
  tempLL = Mean(numbsTemp$LL)
  tempAIC = Mean(AIC(numbsTemp$LL, noPars))
  tempBIC = Mean(BIC(numbsTemp$LL, noPars, sampleSizes[dp]))
  finalN = nrow(numbsTemp)
  lvscores = Mean(numbsTemp$RMSE)
  #tempRMSEA = Mean(RMSEAi)
  otherstats = rbind(otherstats, c(allICspecTRUE[trueI], m, sampleSizes[dp], theTs[dp], 
                                   tempLL, tempAIC, tempBIC,
                                   timetemp, finalN, lvscores))
  sumstatsALL = rbind(sumstatsALL, sumstats)
  ostatsALL = rbind(ostatsALL, otherstats)
  }
}


allAIC = NULL
allBIC = NULL
for (dp in 1:length(sampleSizes)){
  AICbest1=NULL
  BICbest1=NULL
  tempAIC = AICbest[(1 + ((dp-1)*noMC)):(noMC*dp), ] 
  tempBIC = BICbest[(1 + ((dp-1)*noMC)):(noMC*dp), ] 
  for(i in 1:noMC){
    baic = min(tempAIC[i,], na.rm=T)
    num = which(tempAIC[i,] == baic)
    #num1=fittedI[num]
    AICbest1 = cbind(AICbest1, num)
    
    bbic = min(tempBIC[i,], na.rm=T)
    num = which(tempBIC[i,] == bbic)
    BICbest1 = cbind(BICbest1, num)
  }
  tableAICbest=NULL
  tableBICbest=NULL
  for (m in 1:length(fittedI)){
    mm=fittedI[m]
    label=allICspecTRUE[mm]
    numA=length(which(AICbest1==m))
    tempA=c(modelName,ICspecTRUE,label,numA)
    numB=length(which(BICbest1==m))
    tempB=c(modelName,ICspecTRUE,label,numB)
    tableAICbest=rbind(tableAICbest,tempA)
    tableBICbest=rbind(tableBICbest,tempB)
  }
  allAIC = rbind(allAIC, tableAICbest)
  allBIC = rbind(allBIC, tableBICbest)
}


fileAIC=paste(RESULTS_DIR,"/AICbest",modelName,allICspecTRUE[trueI],small,mod,jDiff,".dat",sep="")
write(t(allAIC),file=fileAIC,ncolumns=ncol(allAIC))

fileBIC=paste(RESULTS_DIR,"/BICbest",modelName,allICspecTRUE[trueI],small,mod,jDiff,".dat",sep="")
write(t(allBIC),file=fileBIC,ncolumns=ncol(allBIC))

# plots

library(ggplot2)
library(gridExtra)

data <- as.data.frame(sumstatsALL, stringsAsFactors = FALSE)
data[, c(6:13)] = apply(data[, c(6:13)], 2, as.numeric)

data$V2[data$V2 == "ModelImplied"] <- "MI"
data$V2[data$V2 == "FreeParm"] <- "FP"
data$V2[data$V2 == "deJong"] <- "DKF"
data$V2[data$V2 == "largeK"] <- "LK"

data$V1 = as.factor(data$V1)
data$V2 = as.factor(data$V2)
data$V3 = as.factor(data$V3)
data$V4 = as.factor(data$V4)
data$V5 = as.factor(data$V5)


#cnames = c("True IC","Fitted IC","N", "T", "True Value","$\\hat{\\theta}$","$SE_\\theta$","$\\hat{SE}$","Bias","RB","RMSE","cov","pow")
cnames = c("TrueIC","FittedIC","N", "T", "TrueValue","meanTheta","sdTheta","meanSE","Bias","RB","RMSE","cov","pow", "parType")

parType <- c(rep(1, 4), rep(2, 3), rep(3, 4), rep(1, 6))
parTypeFP <- c(rep(1, 4), rep(2, 3), rep(3, 4), rep(1, 6), rep(4, 5))
# for nonstationary:
if (model == 2 & !diffOnly){
  parTypeAll <- rep(c(parTypeFP, rep(parType, 4)), length(sampleSizes))
}
# for stationary
if (model == 1 & !diffOnly){
  parTypeAll <- rep(c(parType, parTypeFP, rep(parType, 4)), length(sampleSizes))
}
if (diffOnly){
  parTypeAll <- rep(rep(parType, 3), length(sampleSizes)) 
}

data <- cbind(data, parTypeAll)
names(data) <- cnames

toPlot <- data[data$parType != 4, ]

biasMeans <- aggregate(toPlot$Bias, by = list(toPlot$parType, toPlot$FittedIC, toPlot$N), mean)
names(biasMeans) <- c("parType", "FittedIC", "N", "Bias")
seMeans <- aggregate((toPlot$sdTheta - toPlot$meanSE), by = list(toPlot$parType, toPlot$FittedIC, toPlot$N), mean)
names(seMeans) <- c("parType", "FittedIC", "N", "SEDiff")
coverageMeans <- aggregate(toPlot$cov, by = list(toPlot$parType, toPlot$FittedIC, toPlot$N), mean)
names(coverageMeans) <- c("parType", "FittedIC", "N", "CovRates")
allAgg <- cbind(biasMeans, seMeans[,ncol(seMeans)], coverageMeans[,ncol(coverageMeans)])
names(allAgg) <- c("parType", "FittedIC", "N", "Bias", "SEDiff", "CovRates")
#mean(data$Bias[data$parType==3&data$FittedIC=="ModelImplied"], na.rm = T)
#pars <- rep(1:17, 12)
#toPlot <- cbind(toPlot, pars)

#parType_labeller <- function(var, value){
#  value <- as.character(value)
#  if (var == "parType"){
#    value[value == 1] <- "Measurement"
#    value[value == 2] <- "Time Series"
#    value[value == 3] <- "Process Noise"
#  }
#  return(value)
#}
#toPlot$parType <- factor(toPlot$parType,
#                    levels = c(1,2,3),
#                    labels = c("Measurement", "Time Series", "Process Noise"))
#biasMeans$parType <- factor(biasMeans$parType,
#                            levels = c(1,2,3),
#                            labels = c("Measurement", "Time Series", "Process Noise"))
allAgg$parType <- factor(allAgg$parType,
                            levels = c(1,2,3),
                            labels = c("Measurement", "Process Noise", "Time Series"))

noNull <- allAgg[allAgg$FittedIC != 'Null', ]
biasPlotAgg <- ggplot(allAgg, aes(FittedIC, Bias)) + 
  geom_point(aes(colour = N, size = 3)) +
  facet_wrap(~ parType, ncol = 1, scales = "free") +
  labs(title = "Bias") +
  theme(plot.background = element_rect(fill = "grey80", colour = "green"), 
        text = element_text(size = 16), plot.title = element_text(size = 16),
        strip.text.x = element_text(size=11), legend.position = "none", axis.title.y = element_blank())

ciPlotAgg <- ggplot(allAgg, aes(FittedIC, CovRates)) + 
  geom_point(aes(colour = N, size = 3)) +
  facet_wrap(~ parType, ncol = 1, scales = "free") +
  labs(title = "Coverage Rates") +
  theme(plot.background = element_rect(fill = "grey80", colour = "green"), 
        text = element_text(size = 16), plot.title = element_text(size = 16),
        strip.text.x = element_text(size=11), legend.position = "none", axis.title.y = element_blank())

sePlotAgg <- ggplot(allAgg, aes(FittedIC, SEDiff)) + 
  geom_point(aes(colour = N, size = 3)) +
  facet_wrap(~ parType, ncol = 1, scales = "free") +
  labs(title = "SD - SE") +
  theme(plot.background = element_rect(fill = "grey80", colour = "green"), 
        text = element_text(size = 16), plot.title = element_text(size = 16),
        strip.text.x = element_text(size=11), legend.position = "none", axis.title.y = element_blank())

models <- c("STAT", "NONstat")

pdf(paste(RESULTS_DIR, "/Plot_M", models[model], "_TI", allICspecTRUE[trueI], mod, jDiff,".pdf", sep = ""),
    width=10, height=8)
grid.arrange(biasPlotAgg, ciPlotAgg, sePlotAgg, ncol = 3)
dev.off()

pdf(paste(RESULTS_DIR, "/Plot_NoSE_M", models[model], "_TI", allICspecTRUE[trueI], mod, jDiff,".pdf", sep = ""),
    width=7, height=8)
grid.arrange(biasPlotAgg, ciPlotAgg, ncol = 2)
dev.off()

biasPlotAgg <- ggplot(noNull, aes(FittedIC, Bias)) + 
  geom_point(aes(colour = N)) +
  facet_wrap(~ parType, ncol = 1, scales = "free") +
  labs(title = "Bias") +
  theme(plot.background = element_rect(fill = "grey80", colour = "green"), 
        text = element_text(size = 16), plot.title = element_text(size = 16),
        strip.text.x = element_text(size=11), legend.position = "none", axis.title.y = element_blank())

ciPlotAgg <- ggplot(noNull, aes(FittedIC, CovRates)) + 
  geom_point(aes(colour = N)) +
  facet_wrap(~ parType, ncol = 1, scales = "free") +
  labs(title = "Coverage Rates") +
  theme(plot.background = element_rect(fill = "grey80", colour = "green"), 
        text = element_text(size = 16), plot.title = element_text(size = 16),
        strip.text.x = element_text(size=11), legend.position = "none", axis.title.y = element_blank())

sePlotAgg <- ggplot(noNull, aes(FittedIC, SEDiff)) + 
  geom_point(aes(colour = N)) +
  facet_wrap(~ parType, ncol = 1, scales = "free") +
  labs(title = "SD - SE") +
  theme(plot.background = element_rect(fill = "grey80", colour = "green"), 
        text = element_text(size = 16), plot.title = element_text(size = 16),
        strip.text.x = element_text(size=11), legend.position = "none", axis.title.y = element_blank())

pdf(paste(RESULTS_DIR, "/Plot_NONULL_M", models[model], "_TI", allICspecTRUE[trueI], mod, jDiff,".pdf", sep = ""),
    width=10, height=8)
grid.arrange(biasPlotAgg, ciPlotAgg, sePlotAgg, ncol = 3)
dev.off()



sumstatsALL[,c(5:(ncol(sumstatsALL)))]=round(as.numeric(sumstatsALL[,c(5:(ncol(sumstatsALL)))]),3)
ostatsALL[,5:10]=round(as.numeric(ostatsALL[,5:10]),3)
cnames1=c("TrueIC","FittedIC", "n", "t", "LL","AIC","BIC","Time","finalN","rmseLVscores")
colnames(ostatsALL)=cnames1
a = as.data.frame(ostatsALL)
c = as.data.frame(convStats, row.names = 1:nrow(convStats))
sumTable=cbind(as.matrix(a$TrueIC),as.matrix(a$FittedIC),as.matrix(a$finalN),as.matrix(c$StrongConv),
               as.matrix(c$WeakConv),as.matrix(c$NoConv),(as.numeric(as.matrix(c$outlier))+as.numeric(as.matrix(c$NAN)))
               ,as.matrix(c$noSE),as.matrix(allAIC[,4]),as.matrix(allBIC[,4])
               ,as.matrix(a$rmseLVscores),as.matrix(a$Time))
cnames2=c("TrueIC","FittedIC","Final # Reps","Strong Conv", "Weak Conv","No Conv","Outliers","No SEs",
          "AIC","BIC","RMSE lvs","Time")
sumTable=rbind(cnames2,sumTable)

fileT2 = paste(RESULTS_DIR,"/table3M",modelName,"ICT_",ICspecTRUE,"ICF_ALL",small,mod,jDiff,".dat",sep="")
write(print(xtable(sumTable,include.rownames=FALSE,
                   caption="Simulation Results: Comparison Across Initial Condition Specifications",digits=c(0, 0,0,2,2, 2, 2, 2, 2, 2, 2, 2,2)),
            type="latex",include.colnames=FALSE,include.rownames=FALSE,sanitize.text.function=function(x){x}),file=fileT2)


fileO=paste(RESULTS_DIR,"/ostats_",modelName,"ICT_",ICspecTRUE,"ICF_ALL",small,mod,jDiff,".dat",sep="")
write.table(ostatsALL,file=fileO)

# nonstationary
rnames = c("Z21","Z31","Z52","Z62","V11","V21","V22","T11","T21","T12","T22","U11","U22","U33","U44","U55","U66",
           "X01","X02","P011","P012","P022",
           rep(c(
             "Z21","Z31","Z52","Z62","V11","V21","V22","T11","T21","T12","T22","U11","U22","U33","U44","U55","U66"),
               4))
# stationary
rnames = rep(c("Z21","Z31","Z52","Z62","V11","V21","V22","T11","T21","T12","T22","U11","U22","U33","U44","U55","U66",
               "Z21","Z31","Z52","Z62","V11","V21","V22","T11","T21","T12","T22","U11","U22","U33","U44","U55","U66",
               "X01","X02","P011","P012","P022",
               rep(c(
                 "Z21","Z31","Z52","Z62","V11","V21","V22","T11","T21","T12","T22","U11","U22","U33","U44","U55","U66"),
                   4)), 2)

cnames = c("$\\theta$","True IC","Fitted IC","N", "T", "True Value","$\\hat{\\theta}$","$SE_\\theta$","$\\hat{SE}$","Bias","RB","RMSE","cov","pow")

call = cbind(rnames,sumstatsALL)
rall = rbind(cnames,call)



file1=paste(RESULTS_DIR,"/table1M",modelName,"ICT_",ICspecTRUE,"ICF_ALL",small,mod,jDiff,".dat",sep="")
file2=paste(RESULTS_DIR,"/table2M",modelName,"ICT_",ICspecTRUE,"ICF_ALL",small,mod,jDiff,".dat",sep="")

write(print(xtable(rall,include.rownames=FALSE,
                   caption="Simulation Results",digits=c(0, 0,0,3, 3, 3, 3, 3, 3, 3, 3,3,3, 3, 3)),
            type="latex",include.colnames=FALSE,include.rownames=FALSE,sanitize.text.function=function(x){x}),file=file1)

write(print(xtable(rall[,4:ncol(rall)],include.rownames=FALSE,
                   caption="Simulation Results",digits=c(0,3, 3, 3, 3, 3, 3, 3, 3,3, 3, 3)),
            type="latex",include.colnames=FALSE,include.rownames=FALSE,sanitize.text.function=function(x){x}),file=file2)

file2=paste(RESULTS_DIR,"/summaryM",modelName,"ICT_",ICspecTRUE,"ICF_ALL",small,mod,jDiff,".dat",sep="")
file3=paste(RESULTS_DIR,"/summary1M",modelName,"ICT_",ICspecTRUE,"ICF_ALL",small,mod,jDiff,".dat",sep="")
write(t(sumstatsALL),file=file2,ncolumns=ncol(sumstatsALL))
write(t(rall),file=file3,ncolumns=ncol(rall))


#biasPlot <- ggplot(toPlot, aes(FittedIC, Bias)) + 
#  geom_point(aes(colour = N)) +
#  facet_wrap(~ parType, ncol = 1, scales = "free") +
#  labs(title = "Bias") +
#  theme(plot.background = element_rect(fill = "grey80", colour = "green"), 
#        text = element_text(size = 16), plot.title = element_text(size = 16),
#        strip.text.x = element_text(size=11), legend.position = "none", axis.title.y = element_blank())
#
#ciPlot <- ggplot(toPlot, aes(FittedIC, cov)) + 
#  geom_point(aes(colour = N)) +
#  facet_wrap(~ parType, ncol = 1, scales = "free") +
#  labs(title = "Coverage Rates") +
#  theme(plot.background = element_rect(fill = "grey80", colour = "green"), 
#        text = element_text(size = 16), plot.title = element_text(size = 16),
#        strip.text.x = element_text(size=11), legend.position = "none", axis.title.y = element_blank())
#
#sePlot <- ggplot(toPlot, aes(FittedIC, sdTheta - meanSE)) + 
#  geom_point(aes(colour = N)) +
#  facet_wrap(~ parType, ncol = 1, scales = "free") +
#  labs(title = "SD - SE") +
#  theme(plot.background = element_rect(fill = "grey80", colour = "green"), 
#        text = element_text(size = 16), plot.title = element_text(size = 16),
#        strip.text.x = element_text(size=11), legend.position = "none", axis.title.y = element_blank())
#
#grid.arrange(biasPlot, ciPlot, sePlot, ncol = 3)
#
#ggplot(toPlot, aes(FittedIC, Bias)) + 
#  geom_point(aes(colour = N)) +  scale_colour_discrete(name  ="N/T Condition",
#                                                       breaks=c("20", "200"),
#                                                       labels=c("Intensive Repeated Measures", "Panel Data")) 
#
#
#postBox <- ggplot(data, aes(factor(cid), StudentMeansPost)) + 
#  geom_boxplot(aes(fill = factor(tid)))+ labs(title = "Post-Test") +
#  facet_grid(~ scid, scales = "free", space = "free", labeller = middleSchool_labeller) +
#  theme(plot.background = element_rect(fill = "grey80", colour = "green"), 
#        text = element_text(size = 16), plot.title = element_text(size = 16),
#        strip.text.x = element_text(size=11), legend.position = "none") + 
#  ylab("Mean Scores") + xlab("Class") + stat_summary(fun.data = n_fun, geom = "text")
#
#grid.arrange(preBox, cjBox, postBox, ncol = 3)
#  geom_(aes(fill = factor(tid)))+ labs(title = "Middle School") + 
#  facet_grid(~ scid, scales = "free", space = "free", labeller = middleSchool_labeller) + 
#  theme(plot.background = element_rect(fill = "grey80", colour = "green"), text = element_text(size = 16), plot.title = element_text(size = 16), strip.text.x = element_text(size=11)) + ylab("Mean Scores") + xlab("Class") +
#  scale_fill_discrete(name = "Teacher ID") + 
#  stat_summary(fun.data = n_fun, geom = "text")
#
#
if (diffOnly){
  noPars = 17
  compDiff = .0001
  isDiff = matrix(0, length(unique(outputs$rep)), noPars)
  rowNum = 1
  for (i in 1:(noMC*3)){
    temp <- outputs[outputs$rep == i & (outputs[, 3] == "deJong" | outputs[, 3] == "EKF"), ]
    if (nrow(temp) < 2) next
    for (p in 1:noPars){
      if ((as.numeric(temp[1, p + 6])- as.numeric(temp[2, p + 6])) > compDiff){
        isDiff[rowNum, p] = 1
        print(i)
      }
    }
    rowNum = rowNum + 1
  }
  which(isDiff > 0)
  nrow(outputs[outputs[, 3] == "deJong", ])
  nrow(outputs[outputs[, 3] == "EKF", ])
  nrow(outputs[outputs[, 3] == "largeK", ])
}