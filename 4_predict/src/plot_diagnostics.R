plot_Boruta <- function(brf_model, model_name, out_dir){
  #' 
  #' @description Plots the Boruta feature importance plot
  #'
  #' @param brf_model output of Boruta()
  #' @param model_name name to append to file name
  #' @param out_dir output directory
  #'
  #' @return filepath to resulting plot
  #' 
  
  fileout <- file.path(out_dir, paste0('Boruta_', model_name, '.png'))
  
  png(fileout, width = 8, height = 4, units = 'in', res = 200)
  #plot without outlier points
  plot(brf_model, outpch = NA, show.names = FALSE)
  dev.off()
  
  return(fileout)
}

plot_hyperparam_opt_results_RF <- function(opt_result, model_name, out_dir){
  #' 
  #' @description Plots hyperparameter optimization results for RF models
  #'
  #' @param opt_result output of RF model hyperparameter optimization
  #' @param model_name name to append to file name
  #' @param out_dir output directory
  #'
  #' @return filepath to resulting plot
  #' 
  
  fileout <- file.path(out_dir, paste0('hyperparam_diagnostic_', 
                                       model_name, '.png'))
  
  p1 <- opt_result %>% 
    tune::collect_metrics() %>%
    ggplot(aes(mtry, mean, color = min_n)) +
    geom_line(size = 1.5, alpha = 0.6) +
    geom_point(size = 2) +
    facet_wrap(~ .metric, scales = "free", nrow = 2) +
    scale_color_viridis_c(option = "plasma", begin = .9, end = 0) +
    ggtitle(model_name)
  
  ggsave(filename = fileout, plot = p1, device = 'png')
  
  return(fileout)  
}

plot_hyperparam_opt_marginals <- function(opt_result, model_name, 
                                          plt_type = "marginals",
                                          perf_metric = NULL, out_dir){
  #' @description Plots hyperparameter optimization results
  #'
  #' @param opt_result output of model hyperparameter optimization
  #' @param model_name name to append to file name
  #' @param plt_type passed to tune::autoplot type parameter
  #' @param perf_metric performance metric passed to tune::autoplot metric parameter
  #' Leave as NULL to plot all computed metrics.
  #' @param out_dir output directory
  #'
  #' @return filepath to resulting plot
  
  fileout <- file.path(out_dir, paste0('hyperparam_marginals_', 
                                       model_name, '.png'))
  
  p1 <- tune::autoplot(object = opt_result, type = plt_type, 
                       metric = perf_metric) +
    ggtitle(model_name)
  
  ggsave(filename = fileout, plot = p1, device = 'png')
  
  return(fileout)
}

plot_vip <- function(RF_model, model_name, num_features, out_dir){
  #' 
  #' @description Plots the variable importance plot from a RF model
  #'
  #' @param RF_model workflow containing the best trained RF model, fit to all training data 
  #' @param model_name name to append to file name
  #' @param num_features select the top num_features number of features to plot
  #' @param out_dir output directory
  #' 
  #' @return filepath to resulting plot
  #' 
  
  fileout <- file.path(out_dir, paste0('vip_', model_name, '.png'))
  
  p1 <- vip::vip(RF_model %>% extract_fit_parsnip(), 
            num_features = num_features, aesthetics = list(width = 0.6)) +
    ggtitle(model_name) +
    theme(axis.title.x = element_text(size = 18),
          axis.text.y = element_text(size = 18))
  
  ggsave(filename = fileout, plot = p1, device = 'png')
  
  return(fileout)
}


plot_pred_obs <- function(df_pred_obs, model_name, out_dir,
                          from_predict = FALSE, model_wf = NULL, pred_data = NULL,
                          pred_var = NULL){
  #' @description returns a scatterplot comparing the predicted and observed values
  #'
  #' @param df_pred_obs df with obs and .pred columns
  #' @param model_name name to append to file name
  #' @param out_dir output directory
  #' @param from_predict logical stating if predictions should be made within
  #' this function using the provided model_wf and pred_data
  #' @param model_wf model workflow
  #' @param pred_data new_data for predict.workflow
  #' @param pred_var the column name of the variable to be predicted
  #'
  #' @return filepath to the resulting plot
  
  fileout <- file.path(out_dir, paste0('pred_obs_scatter', model_name, '.png'))
  
  if(from_predict){
    #predict from provided workflow and data
    df_pred_obs <- predict(model_wf, new_data = pred_data, type = 'numeric') %>%
      mutate(obs = pred_data[[pred_var]])
  }
  
  plt_lim <- max(c(df_pred_obs$obs, df_pred_obs$.pred))
  
  png(filename = fileout, width = 4, height = 4, units = 'in', res = 200)
  plot(df_pred_obs$obs, df_pred_obs$.pred,
       xlim = c(1,plt_lim), ylim = c(1,plt_lim),
       xlab = 'Observed', ylab = 'Predicted', cex = 0.4, pch = 16,
       main = paste0('Model: ', model_name),
       cex.main = 0.8, log = 'xy')
  lines(c(1,plt_lim), c(1,plt_lim), col = 'red')
  dev.off()
  
  return(fileout)
}


plot_metric_boxplot <- function(data_split, model_name, pred_var, out_dir){
  #' @description returns boxplots comparing the training and testing splits
  #' for the pred_var.
  #'
  #' @param data_split the training and testing split. Example: p6_Boruta_CONUS_g2$input_data
  #' @param model_name name to append to file name
  #' @param pred_var the column name of the variable to be predicted
  #' @param out_dir output directory
  #'
  #' @return filepath to the resulting plot
  
  fileout <- file.path(out_dir, paste0('train_test_boxplot_', model_name, '_', pred_var, '.png'))
  
  png(filename = fileout, width = 4, height = 4, units = 'in', res = 200)
  boxplot(data_split$training[[pred_var]],
          data_split$testing[[pred_var]], 
          names = c('Training', 'Testing'),
          main = paste0('Model: ', model_name),
          cex.main = 0.8, log = 'y')
  dev.off()
  
  return(fileout)
}


barplot_compare_RF <- function(mod, model_name, pred_var, perf_metric, out_dir){
  #'
  #' @description makes barplots of RMSEs for each of the supplied models
  #'
  #' @param mod best fit model evaluated on the test dataset.
  #' @param model_name name to append to file name
  #' @param pred_var the column name of the predicted variable
  #' @param perf_metric performance metric name
  #' @param out_dir output directory
  #'
  #' @return filepath to the resulting plot
  
  fileout <- file.path(out_dir, paste0('compare_models_', model_name, '_', pred_var, '_', perf_metric, '_CV.png'))
  
  #CV performances dataframe
  plt_df <- data.frame(perf = c(tune::show_best(mod$grid_params, n = 1, metric = perf_metric)$mean,
                                get_perf_metric(mod$best_fit$.metrics[[1]], perf_metric = perf_metric)
                                ),
                       sd = c(tune::show_best(mod$grid_params, n = 1, metric = perf_metric)$std_err,
                              NA),
                       Dataset = c('Val', 'Test'),
                       grp = c("RF-Static","RF-Static"))
  
  p1 <- ggplot(data = plt_df, aes(x = grp, y = perf, fill = Dataset)) +
    geom_bar(stat="identity", position=position_dodge(), width = 0.6) +
    theme_bw() +
    scale_fill_brewer(palette="Paired") +
    geom_errorbar(aes(ymin = perf - 2*sd, ymax = perf + 2*sd), width = .2,
                  position = position_dodge(0.6)) +
    xlab('') +
    ylab(perf_metric) + 
    scale_x_discrete(limits=c("RF-Static")) +
    theme(axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 14)) +
    ggtitle(paste0(model_name, ', ', pred_var))
  
  ggsave(filename = fileout, plot = p1, device = 'png')
  
  return(fileout)
}

get_perf_metric <- function(model_fit, perf_metric){
  #' @description returns the performance metric for a fitted model
  #'
  #' @param model_fit fitted model
  #' @param perf_metric performance metric name
  #'
  #' @return performance metric value
  
  model_fit$.estimate[model_fit$.metric == perf_metric]
}