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


currentFitted = allICspecTRUE[fittedI]
outputs <- as.data.frame(MCfile, stringsAsFactors=FALSE)
outputs = outputs[, -ncol(outputs)]
outputs[outputs == "."] = NA
outputs[,c(4,7:23)]=apply(outputs[,c(4,7:23)], 2, as.numeric) 
sumstatsALL = NULL
for (dp in 1:length(sampleSizes)){
  tempDP = outputs[outputs[, 5] == theTs[dp], ]
  for (i in 1:length(fittedI)){
    m = currentFitted[i]
    tempModel = tempDP[tempDP[, 3] == m, ]
    sumstats = NULL
    otherstats = NULL
    if (m == "FreeParm"){
      popValues = c(popValuesi[[model]],1,.5,1.2,.3,.7)
      colnames(tempModel) = PFAcolnamesFP
    } else {
      popValues = popValuesi[[model]]
      colnames(tempModel) = c(PFAcolnames)
    }
    noPars = length(popValues)
    if (nrow(tempModel) == 0) next
    numbsTemp = lapply(tempModel[, c(7:(noPars * 2 + 1 + 6), 4,
                                     which(names(tempModel) == "LL"),
                                     which(names(tempModel) == "time"))], as.numeric)
    numbsTemp = as.data.frame(numbsTemp)
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
      sumstats = rbind(sumstats, c(allICspecTRUE[trueI], m, sampleSizes[dp], theTs[dp], 
                                   popValues[(p)], meantemp, sdtemp, meanSE, tempbias,
                                   temprelbias, tempRMSE, percovtemp, perpowtemp))
    }#end p loop
    sumstatsALL = rbind(sumstatsALL, sumstats)
  }
}
sumstatsALL <- as.data.frame(sumstatsALL, stringsAsFactors = FALSE)
sumstatsALL[,c(6:(ncol(sumstatsALL)))] <- apply(sumstatsALL[,c(6:(ncol(sumstatsALL)))], 2, function(x) round(as.numeric(x), 2))

cnames = c("TrueIC","FittedIC","N", "T", "TrueValue","meanTheta","sdTheta","meanSE","Bias","RB","RMSE","cov","pow")
names(sumstatsALL) = cnames

rnamesFP = c("Z21","Z31","Z52","Z62","V11","V21","V22","T11","T21","T12","T22","U11","U22","U33","U44","U55","U66",
             "X01","X02","P011","P012","P022")
rnames = c("Z21","Z31","Z52","Z62","V11","V21","V22","T11","T21","T12","T22","U11","U22","U33","U44","U55","U66")

rnamesAll <- NULL
for (s in 1:length(sampleSizes)){
  for (i in 1:length(currentFitted)){
    if (currentFitted[i] == "FreeParm"){
      rnamesAll = c(rnamesAll, rnamesFP)
    } else {
      rnamesAll = c(rnamesAll, rnames)
    }
  }
}
sumstatsALL <- cbind(sumstatsALL, rnamesAll)