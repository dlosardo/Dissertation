library(ggplot2)
library(reshape2)

prep_param_dat_for_plots <- function(dat){
  # remove initial condition
  dat <- dat[!dat$param_type %in% "init_cond", ]
  # create a sample size/number of time points factor variable
  dat$NT_cond <- with(dat, factor(paste0(N, "-", T), levels = unique(paste0(N, "-", T))
                           , labels = unique(paste0("N = ", N, "\nT = ", T))))
  # create a parameter type model type factor variable
  dat$model_paramtype <- with(dat, factor(paste0(modelName, "-", param_type)
                                    , levels = unique(paste0(modelName, "-", param_type))
                                    , labels = unique(paste0(modelName, ": ", param_type))))
  # create a character variable for Stationary vs. Non-Stationary model types
  dat$stationarity <- with(dat, ifelse(modelName %in% "PFAstat", "Stationary", "Non-Stationary"))
  # Rename simulation outcome variables
  dat <- rename(dat, c("bias" = "Bias", "rel_bias" = "Relative Bias", "rmse" = "RMSE", "coverage" = "Coverage"
                       , "sd_minus_se" = "SD MINUS SE", "power" = "Power"))
  # make factor variable for param_type
  dat$param_type <- factor(dat$param_type, levels = sort(unique(dat$param_type)), labels = c("Measurement", "Process Noise", "Time Series"))
  return(dat)
}

plot_simulation_outcomes <- function(dat, facet_by, sim_outcome){
  ggplot(dat
         , aes_string(y = sim_outcome, x = "NT_cond"
                      , group = "ICfitted", color = "ICfitted"
                      , shape = "ICfitted")) + geom_line(size = 1.2) + geom_point(size = 4) +
    facet_wrap(as.formula(paste("~", facet_by)), scales = "free_y") +
    scale_shape_discrete(name = "Fitted Initial Condition") +
    scale_colour_brewer(name = "Fitted Initial Condition", palette="Set1") +
    theme(legend.position="bottom",
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=16),
          axis.text.x = element_text(size= 14),
          axis.text.y = element_text(size=14),
          legend.text = element_text(size =12),
          legend.position = "bottom",
          legend.title = element_text(size = 16),
          strip.text.y = element_text(size = 12),
          strip.text.x = element_text(size = 12)) + 
    xlab("Sample Size and Time Point Condition") +
    ggtitle(paste0(unique(dat$stationarity), " Model\nTrue Initial Condition: ", unique(dat$ICspecTRUE)))
}

main <- function(convergence_dat_file, summary_params_file, summary_paramtype_file
                 , summary_best_info_criterias_file, summary_overall_file){
  convergence_dat <- read.csv(convergence_dat_file, stringsAsFactors=F)
  summary_params <- read.csv(summary_params_file, stringsAsFactors=F)
  summary_paramtype <- read.csv(summary_paramtype_file, stringsAsFactors=F)
  summary_best_info_criterias <- read.csv(summary_best_info_criterias_file, stringsAsFactors=F)
  summary_overall <- read.csv(summary_overall_file, stringsAsFactors=F)
  
  summary_params_forplots <- prep_param_dat_for_plots(summary_params)
  summary_paramtype_forplots <- prep_param_dat_for_plots(summary_paramtype)  
  
  #plot_simulation_outcomes(summary_paramtype_forplots[summary_paramtype_noinit$stationarity %in% "Non-Stationary" &
  #                                                    summary_paramtype_noinit$ICspecTRUE %in% "diffuse" &
  #                                                    (summary_paramtype_noinit$ICfitted %in% "deJong" |
  #                                                       summary_paramtype_noinit$ICfitted %in% "EKF" |
  #                                                       summary_paramtype_noinit$ICfitted %in% "largeK"), ]
  #                         , "param_type", "Bias")
  #plot_simulation_outcomes(summary_params_forplots[summary_params_noinit$stationarity %in% "Non-Stationary" &
  #                                                    summary_params_noinit$ICspecTRUE %in% "diffuse" &
  #                                                    (summary_params_noinit$ICfitted %in% "deJong" |
  #                                                       summary_params_noinit$ICfitted %in% "EKF" |
  #                                                       summary_params_noinit$ICfitted %in% "largeK"), ]
  #                         , "param_name", "Bias")

  sim_outcomes <- c("Bias", "`Relative Bias`", "RMSE", "Coverage", "`SD MINUS SE`", "Power")
  pdf("data/output/results/sim_outcomes_paramtype.pdf")
  sapply(sim_outcomes, function(sim_outcome) d_ply(summary_paramtype_forplots
                                                   , .(stationarity, ICspecTRUE)
                                                   , function(x) print(plot_simulation_outcomes(x, "param_type", sim_outcome))))
  dev.off()
  pdf("data/output/results/sim_outcomes_params.pdf")
  sapply(sim_outcomes, function(sim_outcome) d_ply(summary_params_forplots
                                                   , .(stationarity, ICspecTRUE)
                                                   , function(x) print(plot_simulation_outcomes(x, "param_name", sim_outcome))))
  dev.off()
  
  
}
  
CONVERGENCE_DAT_FILE = "data/output/results/convergence_dat.csv"
SUMMARY_PARAMS_FILE = "data/output/results/summary_params_dat.csv"
SUMMARY_PARAMTYPE_FILE = "data/output/results/summary_paramtype_dat.csv"
SUMMARY_BEST_INFO_CRITERIAS_FILE = "data/output/results/summary_best_info_criterias_dat.csv"
SUMMARY_OVERALL_FILE = "data/output/results/summary_overall_dat.csv"

main(CONVERGENCE_DAT_FILE, SUMMARY_PARAMS_FILE, SUMMARY_PARAMTYPE_FILE, SUMMARY_BEST_INFO_CRITERIAS_FILE, SUMMARY_OVERALL_FILE)