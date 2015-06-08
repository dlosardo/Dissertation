#####################################
# Compile results from dissertation #
# Author: Diane Losardo             #
#####################################
library(xtable)
library(outliers)
library(plyr)
library(reshape2)
library(ggplot2)

source('src/R/functions.R')

################################################################
NO_MC = 500 # Number of Monte Carlo runs
SAMPLE_SIZES = c(200, 20) # Sample sizes
THE_TS = c(5, 50) # Number of time points
IC_SPECS = c("ModelImplied","FreeParm","Null","deJong","EKF","largeK")
MODEL_NAMES = c("PFAstat","PFAnons")
PFA_COLNAMES = c("modelName","ICspecTRUE","ICfitted", "RMSE", "T","N","Z21","Z31","Z52","Z62",
                "V11","V21","V22","T11","T21","T12","T22","U11","U22","U33","U44","U55","U66",
                "seZ21","seZ31","seZ52","seZ62","seV11","seV21","seV22","seT11","seT21","seT12",
                "seT22","seU11","seU22","seU33","seU44","seU55","seU66","LL","conv","invertSym","invertDec","time")
PFA_COLNAMESFP = c("modelName","ICspecTRUE","ICfitted", "RMSE","T","N","Z21","Z31","Z52","Z62",
                "V11","V21","V22","T11","T21","T12","T22","U11","U22","U33","U44","U55","U66",
                "X01","X02","P011","P012","P022",
                "seZ21","seZ31","seZ52","seZ62","seV11","seV21","seV22","seT11","seT21","seT12",
                "seT22","seU11","seU22","seU33","seU44","seU55","seU66",
                "seX01","seX02","seP011","seP012","seP022",
                "LL","conv","invertSym","invertDec","time")
PAR_NAMES=c("Z21","Z31","Z52","Z62","V11","V21","V22","T11","T21","T12","T22","U11","U22","U33","U44","U55","U66",
           "X01","X02","P011","P012","P022")
PARAM_TYPE=c(rep("measurement", 4), rep("process_noise", 3), rep("time_series", 4), rep("measurement", 6), rep("init_cond", 5))
pop_values_freeparm_extras <- c(1,.5,1.2,.3,.7)
POP_VALUES_STAT <- c(1.2,.8,.9,1.1,1,.4,1,.5,-.3,-.1,.6,.8,.6,2,1,1.5,.4, pop_values_freeparm_extras)
POP_VALUES_NONSTAT <- c(1.2,.8,.9,1.1,.5,.1,.5,1.2,-.4,.6,.7,.8,.6,2,1,1.5,.4, pop_values_freeparm_extras)

# create a population values dataframe that contains the following info
pop_values_dat <- data.frame(param_name = rep(PAR_NAMES, length(MODEL_NAMES))
                             , model_type = do.call("c", sapply(MODEL_NAMES, function(x) rep(x, length(PAR_NAMES)), simplify=F))
                             , pop_values = c(POP_VALUES_STAT, POP_VALUES_NONSTAT)
                             , param_type = rep(PARAM_TYPE, length(MODEL_NAMES)))

#all results files:
# 1. Nonstationary diffuse
# 2. Nonstationary diffuse Moderate 
# 3. Nonstationary Free Parameter
# 4. Nonstationary Null
# 5. Stationary Free Parameter
# 6. Stationary Model Implied
# 7. Stationary Null
#PFAnons deJong
#PFAnons FreeParm
#PFAnons Null
#PFAstat ModelImplied
#PFAstat FreeParm
#PFAstat Null
model_names <- c(rep("PFAstat", 3), rep("PFAnons", 3))
true_ics <- c("ModelImplied", "FreeParm", "Null", "deJong", "FreeParm", "Null")
fitted_ics <- c("ModelImplied","FreeParm","Null","deJong","EKF","largeK")
# read in sim results
result_list <- mapply(function(model_name, true_ic){
  list(read.table(file = sprintf("data/output/results/M%sICT_%sICF_ALL.dat", model_name, true_ic)
             , stringsAsFactors = FALSE, fill = TRUE))
}, model_names, true_ics)
# name sim results
result_list <- llply(result_list, name_results)
# subsetting results so there are NO_MC reps for every simulation condition
result_list <- llply(result_list
                        , function(x) ddply(x, .(modelName, ICspecTRUE, ICfitted, N, T), function(y) y[1:NO_MC, ]))
result_list <- llply(result_list
                        , function(x) ddply(x, .(modelName, ICspecTRUE, ICfitted, N, T), add_reps))


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

# obtain convergence statistics
convergence_stats <- llply(result_list, get_convergence_stats)

# subset to only results that converged properly
result_list_subsetted <- llply(result_list, get_proper_results)

# get the number of reps remaining for each condition
reps_used <- llply(result_list_subsetted
                   , function(x) ddply(x, .(modelName, ICspecTRUE, ICfitted, N, T), summarize, nreps = length(RMSE)))
# melt the results based on parameters
results_melted <- llply(result_list_subsetted, get_melted_results)
# get the reps within each simulation condition that qualify as outliers for each parameter
outlier_reps <- llply(results_melted
                      , function(x) ddply(x, .(modelName, ICspecTRUE, ICfitted, N, T, param_name), summarize
           , which_outliers = outDET1(estimate)))
outlier_reps <- outlier_reps[llply(outlier_reps, nrow)>0]
# total number of outlier reps within each condition
noutliers <- llply(outlier_reps
                   , function(x) ddply(x, .(modelName, ICspecTRUE, ICfitted, N, T)
                                       , summarize, ntotal_outliers = sum(length(unique(which_outliers)))))
# adding outlier and reps used information  to convergence stats dat
convergence_dat <- merge(ldply(noutliers), ldply(convergence_stats)
                         , by = c("modelName", "ICspecTRUE", "ICfitted", "N", "T"), all=T)
convergence_dat <- merge(convergence_dat, ldply(reps_used)
                         , by = c("modelName", "ICspecTRUE", "ICfitted", "N", "T"), all=T)
# getting rid of rows that have NA
if(any(apply(convergence_dat, 1, function(x) all(is.na(x[5]))))){
  convergence_dat <- convergence_dat[!apply(convergence_dat, 1, function(x) all(is.na(x[5]))), ]
}
convergence_dat <- convergence_dat[, names(convergence_dat)[!names(convergence_dat) %in% c(".id.x", ".id.y", ".id")]]
convergence_dat[convergence_dat$ICspecTRUE %in% "deJong", "ICspecTRUE"] <- "diffuse"
write.csv(convergence_dat, file="data/output/results/convergence_dat.csv", row.names=F)

# unique reps for each condition that have AT LEAST one parameter that qualifies as an outlier
unique_outliers_dat <- ldply(outlier_reps, function(x) unique(x[, c("modelName", "ICspecTRUE", "ICfitted", "N", "T", "which_outliers")]))
unique_outliers_dat$N <- as.numeric(unique_outliers_dat$N)
unique_outliers_dat$T <- as.numeric(unique_outliers_dat$T)
# results with outliers removed
results_dat_subsetted_outliers_removed <- ldply(result_list_subsetted, function(x) ddply(x, .(modelName, ICspecTRUE, ICfitted, N, T)
                                                , function(y) remove_outliers(y, unique_outliers_dat)))

#convDir = paste(RESULTS_DIR, "/ConvStats_M", modelName, "TIC", ICspecTRUE, small, mod, jDiff,".txt",sep="")
#write.table(convStats, file=convDir, append=FALSE, col.names = TRUE)

# melt results again with outliers removed
results_melted <- melt(results_dat_subsetted_outliers_removed, id.vars = c("modelName", "ICspecTRUE", "ICfitted"
                                                          , "N", "T", "rep"), measure.vars = PAR_NAMES
                       , variable.name = "param_name", value.name = "estimate")
# merge parameter meta data into results
results_melted_params <- merge(results_melted, pop_values_dat
                               , by.x = c("param_name", "modelName"), by.y = c("param_name", "model_type"))
# repeat process for standard error simulation outcomes
results_melted_ses <- melt(results_dat_subsetted_outliers_removed, id.vars = c("modelName", "ICspecTRUE", "ICfitted"
                                                                           , "N", "T", "rep"), measure.vars = sprintf("se%s", PAR_NAMES)
                       , variable.name = "se_param_name", value.name = "se_estimate")
results_melted_ses$param_name <- gsub("se", "", results_melted_ses$se_param_name)
# merge everything together
results_melted_params_ses <- merge(results_melted_params, results_melted_ses
           , by=c("modelName", "ICspecTRUE", "ICfitted", "N", "T", "param_name", "rep"))

summary_params <- ddply(results_melted_params_ses, .(modelName, ICspecTRUE, ICfitted, N, T, param_name), summarize
                        , true_value = unique(pop_values)
                        , param_type = unique(param_type)
                        , param_mean = as.numeric(mean(as.numeric(estimate), na.rm=T))
                        , param_sd = as.numeric(sd(as.numeric(estimate), na.rm=T))
                        , se_mean = as.numeric(mean(as.numeric(se_estimate), na.rm=T))
                        , se_sd = as.numeric(sd(as.numeric(se_estimate), na.rm=T))
                        , sd_minus_se = as.numeric(sd(as.numeric(estimate), na.rm=T)) - as.numeric(mean(as.numeric(se_estimate), na.rm=T))
                        , bias = Bias(as.numeric(estimate), unique(pop_values))
                        , rel_bias = relBias(as.numeric(estimate), unique(pop_values))
                        , rmse = RMSE(as.numeric(estimate), unique(pop_values))
                        , coverage = ifelse(any(is.na(as.numeric(estimate))), NA
                                            , mean(cov(lcl(as.numeric(estimate), as.numeric(se_estimate))
                                    , ucl(as.numeric(estimate), as.numeric(se_estimate))
                                    , unique(pop_values))))
                        , power = ifelse(any(is.na(as.numeric(estimate))), NA
                                         , mean(pow(as.numeric(estimate), as.numeric(se_estimate))))
                        )
summary_params[summary_params$ICspecTRUE %in% "deJong", "ICspecTRUE"] <- "diffuse"
summary_parmtype <- ddply(summary_params, .(modelName, ICspecTRUE, ICfitted, N, T, param_type), summarize
                          , bias = mean(bias, na.rm=T)
                          , rel_bias = mean(rel_bias, na.rm=T)
                          , rmse = mean(rmse, na.rm=T)
                          , coverage = mean(coverage, na.rm=T)
                          , sd_minus_se = mean(sd_minus_se, na.rm=T)
                          , power = mean(power, na.rm=T)
                          )
summary_parmtype[summary_parmtype$ICspecTRUE %in% "deJong", "ICspecTRUE"] <- "diffuse"
results_dat_subsetted_outliers_removed$AIC <- with(results_dat_subsetted_outliers_removed
                                                   , AIC(as.numeric(LL), length(which(!is.na(PAR_NAMES)))))
results_dat_subsetted_outliers_removed$BIC <- with(results_dat_subsetted_outliers_removed
                                                   , BIC(as.numeric(LL), length(which(!is.na(PAR_NAMES))), N))
best_info_criterias <- ddply(results_dat_subsetted_outliers_removed, .(modelName, ICspecTRUE, rep), summarize
      , bestAIC = ICfitted[min(AIC) == AIC]
      , bestBIC = ICfitted[min(BIC) == BIC])
summary_best_info_criterias <- ddply(best_info_criterias, .(modelName, ICspecTRUE), function(chunk){
                                     tmp <- data.frame(apply(chunk[, c("bestAIC", "bestBIC")], 2, table))
                                     data.frame(bestAIC = tmp$bestAIC, bestBIC = tmp$bestBIC, winning_model = row.names(tmp))
                                     })
summary_best_info_criterias[summary_best_info_criterias$ICspecTRUE %in% "deJong", "ICspecTRUE"] <- "diffuse"
summary_overall <- ddply(results_dat_subsetted_outliers_removed
                         , .(modelName, ICspecTRUE, ICfitted, N, T)
                         , summarize
                         , AIC = mean(as.numeric(AIC), na.rm=T)
                         , BIC = mean(as.numeric(BIC), na.rm=T)
                         , finalN = length(RMSE)
                         , lv_rmse = mean(as.numeric(RMSE), na.rm=T)
                         , time = mean(as.numeric(time), na.rm=T))
summary_overall[summary_overall$ICspecTRUE %in% "deJong", "ICspecTRUE"] <- "diffuse"


t <- results_melted[results_melted$ICfitted %in% "FreeParm" & results_melted$T == 50 & 
                      results_melted$param_name %in% "Z21", ]
x <- results_dat_subsetted[results_dat_subsetted$ICfitted %in% "FreeParm" & results_dat_subsetted$T == 50, ]
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

pdf(paste(RESULTS_DIR, "/Plot_M", models[model], "_TI", IC_SPECS[trueI], mod, jDiff,".pdf", sep = ""),
    width=10, height=8)
grid.arrange(biasPlotAgg, ciPlotAgg, sePlotAgg, ncol = 3)
dev.off()

pdf(paste(RESULTS_DIR, "/Plot_NoSE_M", models[model], "_TI", IC_SPECS[trueI], mod, jDiff,".pdf", sep = ""),
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

pdf(paste(RESULTS_DIR, "/Plot_NONULL_M", models[model], "_TI", IC_SPECS[trueI], mod, jDiff,".pdf", sep = ""),
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