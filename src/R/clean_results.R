#####################################
# Compile results from dissertation #
# Author: Diane Losardo             #
#####################################
library(xtable)
library(outliers)
library(plyr)
library(reshape2)
library(ggplot2)
source('src/R/simulation_outcome_functions.R')
source('src/R/functions.R')
#############
# CONSTANTS #
#############
NO_MC = 500 # Number of Monte Carlo runs
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
POP_VALUES_FREEPARM_EXTRAS <- c(1,.5,1.2,.3,.7)
POP_VALUES_STAT <- c(1.2,.8,.9,1.1,1,.4,1,.5,-.3,-.1,.6,.8,.6,2,1,1.5,.4, POP_VALUES_FREEPARM_EXTRAS)
POP_VALUES_NONSTAT <- c(1.2,.8,.9,1.1,.5,.1,.5,1.2,-.4,.6,.7,.8,.6,2,1,1.5,.4, POP_VALUES_FREEPARM_EXTRAS)
MODEL_NAMES <- c("PFAstat", "PFAnons")
# create a population values dataframe that contains the following info
POP_VALUES_DAT <- data.frame(param_name = rep(PAR_NAMES, length(MODEL_NAMES))
                             , model_type = do.call("c", sapply(MODEL_NAMES, function(x) rep(x, length(PAR_NAMES)), simplify=F))
                             , pop_values = c(POP_VALUES_STAT, POP_VALUES_NONSTAT)
                             , param_type = rep(PARAM_TYPE, length(MODEL_NAMES)))

MODEL_NAMES <- c(rep("PFAstat", 3), rep("PFAnons", 3))
TRUE_ICS <- c("ModelImplied", "FreeParm", "Null", "deJong", "FreeParm", "Null")
FITTED_ICS <- c("ModelImplied","FreeParm","Null","deJong","EKF","largeK")
#############
# FUNCTIONS #
#############
#' Names the results - the tricky part is that the free parameter fitted condition has
#'  more parameters (the ones in the initial condition specification)
name_results <- function(results_dat, pfa_colnames, pfa_colnames_fp){
  free_parm_fitted <- results_dat[results_dat$V3 %in% "FreeParm", ]
  names(free_parm_fitted) <- pfa_colnames_fp
  other_fitted <- results_dat[!results_dat$V3 %in% "FreeParm", 1:length(pfa_colnames)]
  names(other_fitted) <- pfa_colnames
  all_results <- rbind.fill(free_parm_fitted, other_fitted)
  return(all_results)
}
#' Reduce dataframe to only those replications with properly converged results
get_proper_results <- function(results_dat){
  results_dat[results_dat$RMSE != "Inf" & !is.nan(results_dat$RMSE) & results_dat$conv %in% "Strong"
              & !results_dat$seZ21 %in% "." & is.na(results_dat$invertSym) & !is.na(results_dat$ICfitted), ]
}
#' Melt results dataframe, i.e., convert to long form
get_melted_results <- function(results_dat, param_names){
  melt(results_dat, id.vars = c("modelName", "ICspecTRUE", "ICfitted"
                                , "N", "T"), measure.vars = param_names
       , variable.name = "param_name", value.name = "estimate")
}
# convergence statistics
#' For each simulation condition, reports the number of:
#'   1. RMSEs for latent variables scores that are either NaN or Inf
#'   2. Replications with Strong convergence
#'   3. Replications with No convergence
#'   4. Replications with Weak convergence
#'   5. Replications that failed when inverting matrix
#'   6. Replications that failed in matrix decomposition
#'   7. Replications that could not calculate standard errors
get_convergence_stats <- function(results_dat){
  ddply(results_dat, .(modelName, ICspecTRUE, ICfitted, N, T), summarize
        , NAN = length(which(is.nan(RMSE))) + length(which(RMSE %in% "Inf"))
        , StrongConv = length(which(conv %in% "Strong"))
        , NoConv = length(which(conv %in% "No"))
        , WeakConv = length(which(conv %in% "Weak"))
        , failedInvertSym = length(which(invertSym %in% "failedInvertSym"))
        , failedDecomp = length(which(invertDec %in% "failedDecomp"))
        , noSE = length(which(seZ21 %in% ".")))
}
# remove rows (i.e., reps) with at least one outlier for a parameter
remove_outliers <- function(x, unique_outliers_dat){
  current_condition <- unique(x[, c("modelName", "ICspecTRUE", "ICfitted", "N")])
  condition_has_outliers <- apply(unique_outliers_dat[, c("modelName", "ICspecTRUE", "ICfitted", "N")]
                                  , 1, function(y){
                                    y["N"] <- as.numeric(y["N"])
                                    all(y == current_condition)
                                  })
  if (any(condition_has_outliers)){
    return(x[-unique_outliers_dat[condition_has_outliers, "which_outliers"],])
  } else {return(x)}
}
#' adds a reps column to a dataframe
add_reps <- function(x){
  x$rep <- 1:nrow(x)
  return(x)
}
#' Read in results in the form of a list
get_result_list <- function(model_names, true_ics, no_mc, pfa_colnames, pfa_colnames_fp){
  # read in sim results
  result_list <- mapply(function(model_name, true_ic){
    list(read.table(file = sprintf("data/output/results/M%sICT_%sICF_ALL.dat", model_name, true_ic)
                    , stringsAsFactors = FALSE, fill = TRUE))
  }, model_names, true_ics)
  # name sim results
  result_list <- llply(result_list, function(x) name_results(x, pfa_colnames, pfa_colnames_fp))
  # subsetting results so there are NO_MC reps for every simulation condition
  result_list <- llply(result_list
                       , function(x) ddply(x, .(modelName, ICspecTRUE, ICfitted, N, T), function(y) y[1:no_mc, ]))
  result_list <- llply(result_list
                       , function(x) ddply(x, .(modelName, ICspecTRUE, ICfitted, N, T), add_reps))
  return(result_list)
}
#' adding outlier and reps used information  to convergence stats dat
update_convergence_stats <- function(convergence_stats, noutliers, reps_used){
  convergence_dat <- merge(ldply(noutliers), ldply(convergence_stats)
                           , by = c("modelName", "ICspecTRUE", "ICfitted", "N", "T"), all=T)
  convergence_dat <- merge(convergence_dat, ldply(reps_used)
                           , by = c("modelName", "ICspecTRUE", "ICfitted", "N", "T"), all=T)
  # getting rid of rows that have NA
  if(any(apply(convergence_dat, 1, function(x) all(is.na(x['modelName']))))){
    convergence_dat <- convergence_dat[!apply(convergence_dat, 1, function(x) all(is.na(x['modelName']))), ]
  }
  convergence_dat <- convergence_dat[, names(convergence_dat)[!names(convergence_dat) %in% c(".id.x", ".id.y", ".id")]]
  convergence_dat[convergence_dat$ICspecTRUE %in% "deJong", "ICspecTRUE"] <- "diffuse"
  return(convergence_dat)
}

remove_all_outliers <- function(outlier_reps, result_list_subsetted){
  # unique reps for each condition that have AT LEAST one parameter that qualifies as an outlier
  unique_outliers_dat <- ldply(outlier_reps, function(x) unique(x[, c("modelName", "ICspecTRUE", "ICfitted", "N", "T", "which_outliers")]))
  # results with outliers removed
  results_dat_subsetted_outliers_removed <- ldply(result_list_subsetted, function(x) ddply(x, .(modelName, ICspecTRUE, ICfitted, N, T)
                                                                                           , function(y) remove_outliers(y, unique_outliers_dat)))
  return(results_dat_subsetted_outliers_removed)
}

get_param_summary <- function(results_dat, param_names, pop_values_dat){
  # melt results again with outliers removed
  results_melted <- melt(results_dat, id.vars = c("modelName", "ICspecTRUE", "ICfitted"
                                                  , "N", "T", "rep"), measure.vars = param_names
                         , variable.name = "param_name", value.name = "estimate")
  # merge parameter meta data into results
  results_melted_params <- merge(results_melted, pop_values_dat
                                 , by.x = c("param_name", "modelName"), by.y = c("param_name", "model_type"))
  # repeat process for standard error simulation outcomes
  results_melted_ses <- melt(results_dat, id.vars = c("modelName", "ICspecTRUE", "ICfitted"
                                                      , "N", "T", "rep"), measure.vars = sprintf("se%s", param_names)
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
  return(summary_params)
}
get_paramtype_summary <- function(summary_params){
  ddply(summary_params, .(modelName, ICspecTRUE, ICfitted, N, T, param_type), summarize
        , bias = mean(bias, na.rm=T)
        , rel_bias = mean(rel_bias, na.rm=T)
        , rmse = mean(rmse, na.rm=T)
        , coverage = mean(coverage, na.rm=T)
        , sd_minus_se = mean(sd_minus_se, na.rm=T)
        , power = mean(power, na.rm=T)
  )
}
get_summary_best_info_criterias <- function(results_dat){
  best_info_criterias <- ddply(results_dat, .(modelName, ICspecTRUE, rep), summarize
                               , bestAIC = ICfitted[min(AIC) == AIC]
                               , bestBIC = ICfitted[min(BIC) == BIC])
  summary_best_info_criterias <- ddply(best_info_criterias, .(modelName, ICspecTRUE), function(chunk){
    tmp <- data.frame(apply(chunk[, c("bestAIC", "bestBIC")], 2, table))
    data.frame(bestAIC = tmp$bestAIC, bestBIC = tmp$bestBIC, winning_model = row.names(tmp))
  })
  return(summary_best_info_criterias)
}
get_overall_summary <- function(results_dat){
  ddply(results_dat
        , .(modelName, ICspecTRUE, ICfitted, N, T)
        , summarize
        , AIC = mean(as.numeric(AIC), na.rm=T)
        , BIC = mean(as.numeric(BIC), na.rm=T)
        , finalN = length(RMSE)
        , lv_rmse = mean(as.numeric(RMSE), na.rm=T)
        , time = mean(as.numeric(time), na.rm=T))
}
main <- function(model_names, true_ics, no_mc, pop_values_dat, pfa_colnames, pfa_colnames_fp, param_names){
  # read in results
  result_list <- get_result_list(model_names, true_ics, no_mc, pfa_colnames, pfa_colnames_fp)
  # obtain convergence statistics
  convergence_stats <- llply(result_list, get_convergence_stats)
  # subset to only results that converged properly
  result_list_subsetted <- llply(result_list, get_proper_results)
  # get the number of reps remaining for each condition
  reps_used <- llply(result_list_subsetted
                     , function(x) ddply(x, .(modelName, ICspecTRUE, ICfitted, N, T), summarize, nreps = length(RMSE)))
  # melt the results based on parameters
  results_melted <- llply(result_list_subsetted, function(x) get_melted_results(x, param_names))
  # get the reps within each simulation condition that qualify as outliers for each parameter
  outlier_reps <- llply(results_melted
                        , function(x) ddply(x, .(modelName, ICspecTRUE, ICfitted, N, T, param_name), summarize
                                            , which_outliers = outDET1(estimate)))
  outlier_reps <- outlier_reps[llply(outlier_reps, nrow)>0]
  # total number of outlier reps within each condition
  noutliers <- llply(outlier_reps
                     , function(x) ddply(x, .(modelName, ICspecTRUE, ICfitted, N, T)
                                         , summarize, ntotal_outliers = sum(length(unique(which_outliers)))))
  # update convergence stats with outlier and reps used info
  convergence_dat <- update_convergence_stats(convergence_stats, noutliers, reps_used)

  results_dat_subsetted_outliers_removed <- remove_all_outliers(outlier_reps, result_list_subsetted)
  results_dat_subsetted_outliers_removed[results_dat_subsetted_outliers_removed$ICspecTRUE %in% "deJong", "ICspecTRUE"] <- "diffuse"
  
  # obtaining a dataframe of simulation outcomes for parameters
  summary_params <- get_param_summary(results_dat_subsetted_outliers_removed, param_names, pop_values_dat)
  # obtaining a dataframe of simulation outcomes aggregated by parameter type
  summary_paramtype <- get_paramtype_summary(summary_params)
  results_dat_subsetted_outliers_removed$AIC <- with(results_dat_subsetted_outliers_removed
                                                     , AIC(as.numeric(LL), length(which(!is.na(param_names)))))
  results_dat_subsetted_outliers_removed$BIC <- with(results_dat_subsetted_outliers_removed
                                                     , BIC(as.numeric(LL), length(which(!is.na(param_names))), N))
  summary_best_info_criterias <- get_summary_best_info_criterias(results_dat_subsetted_outliers_removed)
  summary_overall <- get_overall_summary(results_dat_subsetted_outliers_removed)

  # write out convergence dat
  write.csv(convergence_dat, file="data/output/results/convergence_dat.csv", row.names=F)
  # write out summary_params dat
  write.csv(summary_params, file="data/output/results/summary_params_dat.csv", row.names=F)
  # write out summary_paramtype dat
  write.csv(summary_paramtype, file="data/output/results/summary_paramtype_dat.csv", row.names=F)
  # write out summary_best_info_criterias dat
  write.csv(summary_best_info_criterias, file="data/output/results/summary_best_info_criterias_dat.csv", row.names=F)
  # write out summary overall dat
  write.csv(summary_overall, file="data/output/results/summary_overall_dat.csv", row.names=F)
}

main(MODEL_NAMES, TRUE_ICS, NO_MC, POP_VALUES_DAT, PFA_COLNAMES, PFA_COLNAMESFP, PAR_NAMES)