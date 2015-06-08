library(ggplot2)


main <- function(convergence_dat_file, summary_params_file, summary_paramtype_file
                 , summary_best_info_criterias_file, summary_overall_file){
  convergence_dat <- read.csv(convergence_dat_file, stringsAsFactors=F)
  summary_params <- read.csv(summary_params_file, stringsAsFactors=F)
  summary_paramtype <- read.csv(summary_paramtype_file, stringsAsFactors=F)
  summary_best_info_criterias <- read.csv(summary_best_info_criterias_file, stringsAsFactors=F)
  summary_overall <- read.csv(summary_overall_file, stringsAsFactors=F)
  summary_paramtype_noinit <- summary_paramtype[!summary_paramtype$param_type %in% "init_cond", ]
  summary_paramtype_noinit$NT_cond <- with(summary_paramtype_noinit, factor(paste0(N, "-", T)
                                           , levels = unique(paste0(N, "-", T))
                                           , labels = unique(paste0("N = ", N, "\nT = ", T))))
  summary_paramtype_noinit$model_paramtype <- with(summary_paramtype_noinit, factor(paste0(modelName, "-", param_type)
                                           , levels = unique(paste0(modelName, "-", param_type))
                                           , labels = unique(paste0(modelName, ": ", param_type))))
  summary_paramtype_noinit$stationarity <- with(summary_paramtype_noinit, ifelse(modelName %in% "PFAstat", "Stationary", "Non-Stationary"))
  
  plot_simulation_outcomes <- function(dat, sim_outcome){
    ggplot(dat
           , aes_string(y = sim_outcome, x = "NT_cond"
                        , group = "ICfitted", color = "ICfitted"
                        , shape = "ICfitted")) + geom_line() + geom_point() +
      facet_wrap(~param_type, scales = "free_y") +
      scale_colour_discrete(name = "Fitted Initial Condition") +
      scale_shape_discrete(name = "Fitted Initial Condition") +
      theme(legend.position="bottom",
            axis.text.x = element_text(size= 12),
            legend.text = element_text(size =12),
            legend.position = "bottom",
            legend.title = element_text(size = 16),
            strip.text.y = element_text(size = 12),
            strip.text.x = element_text(size = 12)) + 
      xlab("Sample Size and Time Point Condition") +
      ggtitle(paste0(unique(dat$stationarity), " Model\nTrue Initial Condition: ", unique(dat$ICspecTRUE)))
  }
  sim_outcomes <- c("bias", "rel_bias", "rmse", "coverage", "sd_minus_se", "power")
  sapply(sim_outcomes, function(sim_outcome) d_ply(summary_paramtype_noinit
                                                   , .(stationarity, ICspecTRUE)
                                                   , function(x) print(plot_simulation_outcomes(x, sim_outcome))))
 
  ggplot(summary_paramtype_noinit[summary_paramtype_noinit$ICspecTRUE %in% "ModelImplied", ]
         , aes(y = bias, x = NT_cond, group = ICfitted, color = ICfitted)) + geom_line() + geom_point() +
    facet_wrap(~model_paramtype, scales = "free_y")
  
}
  

CONVERGENCE_DAT_FILE = "data/output/results/convergence_dat.csv"
SUMMARY_PARAMS_FILE = "data/output/results/summary_params_dat.csv"
SUMMARY_PARAMTYPE_FILE = "data/output/results/summary_paramtype_dat.csv"
SUMMARY_BEST_INFO_CRITERIAS_FILE = "data/output/results/summary_best_info_criterias_dat.csv"
SUMMARY_OVERALL_FILE = "data/output/results/summary_overall_dat.csv"