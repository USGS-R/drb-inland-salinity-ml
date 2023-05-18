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
    ggtitle(model_name, 
            subtitle = 'min_n: minimum node size\nmtry: number of attributes to randomly sample in each tree split')
  
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
    theme(axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 10))
  
  ggsave(filename = fileout, plot = p1, device = 'png')
  
  return(fileout)
}


plot_pred_obs <- function(df_pred_obs, model_name, out_dir,
                          from_predict = FALSE, model_wf = NULL, pred_data = NULL,
                          pred_var = NULL, count_shade = FALSE){
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
  #' @param count_shade logical indicating whether or not to make a count shaded
  #' density scatterplot instead of a traditional scatterplot
  #'
  #' @return filepath to the resulting plot
  
  if(from_predict){
    #predict from provided workflow and data
    df_pred_obs <- predict(model_wf, new_data = pred_data, type = 'numeric') %>%
      mutate(obs = pred_data[[pred_var]])
  }
  
  #count shaded plot
  if(count_shade){
    fileout <- paste0('pred_obs_scatter_', model_name, '_density.png')
    
    #.pred is a column name
    p1 <- ggplot(df_pred_obs, aes(x=log10(obs), y=log10(.pred))) +
      geom_bin2d(bins = 100) +
      scale_fill_distiller(palette = 7, direction = 1) +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      xlim(0,5) + ylim(0,5) +
      xlab(expression(paste('Observed Specific Conductivity (', mu, 'S/cm)', sep = ''))) + 
      ylab(expression(paste('Predicted Specific Conductivity (', mu, 'S/cm)', sep = ''))) +
      ggtitle(paste0('Model: ', model_name)) +
      geom_abline(slope=1, intercept=0)
    
    ggsave(filename = fileout, plot = p1, device = 'png', path = out_dir)
    
    fileout <- file.path(out_dir, fileout)
  }else{
    fileout <- file.path(out_dir, paste0('pred_obs_scatter_', model_name, '.png'))
    
    plt_lim <- max(c(df_pred_obs$obs, df_pred_obs$.pred))
    
    png(filename = fileout, width = 4, height = 4, units = 'in', res = 200)
    plot(df_pred_obs$obs, df_pred_obs$.pred,
         xlim = c(1,plt_lim), ylim = c(1,plt_lim),
         xlab = expression(paste('Observed Specific Conductivity (', mu, 'S/cm)', sep = '')), 
         ylab = expression(paste('Predicted Specific Conductivity (', mu, 'S/cm)', sep = '')), 
         cex = 0.4, pch = 16,
         main = paste0('Model: ', model_name),
         cex.main = 0.8, log = 'xy')
    lines(c(1,plt_lim), c(1,plt_lim), col = 'red')
    dev.off()
  }
  
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
          ylab = expression(paste('Specific Conductivity (', mu, 'S/cm)', sep = '')),
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
                       grp = c(model_name, model_name))
  
  p1 <- ggplot(data = plt_df, aes(x = grp, y = perf, fill = Dataset)) +
    geom_bar(stat="identity", position=position_dodge(), width = 0.6) +
    theme_bw() +
    scale_fill_brewer(palette="Paired") +
    geom_errorbar(aes(ymin = perf - 2*sd, ymax = perf + 2*sd), width = .2,
                  position = position_dodge(0.6)) +
    xlab('') +
    ylab(bquote(.(perf_metric) ~ (mu * S/cm))) + 
    scale_x_discrete(limits = model_name) +
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

plot_barplot <- function(attr_data, file_path,
                         model_name, plot_month_names = FALSE,
                         panel = FALSE, label_sequence = NULL){
  #' 
  #' @description Creates a barplot for each of the columns in attr_data
  #'
  #' @param attr_data data frame for which the first column is the x-axis of the 
  #' barplot and the remaining columns are y-axes
  #' @param file_path a character string that indicates the location of the saved plot
  #' @param model_name character string describing the model. Will be added 
  #' to the end of the filename before the file extension, and also be the plot title.
  #' @param plot_month_names if TRUE, then the x-axis will be converted from
  #' numeric to month.abb month names
  #' @param panel if TRUE, columns 2 and 3 of attr_data will be plotted in a
  #' panel plot.
  #' @param label_sequence the indices of labels to plot on the x-axis. NULL
  #' plots all labels, which can get crowded for some plots.
  #'
  #' @return Returns the path to png files containing barplots of each attribute
  
  plot_names <- vector('character', length = 0L)
  plt_lst <- list()
  
  # For each column, create a barplot
  for(i in 2:dim(attr_data)[2]){
    
    dat_subset <- attr_data[,c(1,i)]
    col_name <- names(dat_subset)[2]
    
    #y limits
    plt_lim <- c(min(0, dat_subset[[2]]), max(0, dat_subset[[2]]))
    
    # barplot
    cols <- colnames(dat_subset)
    attr_plot <- ggplot() + 
      geom_abline(slope = 0, intercept = 0) +
      geom_col(data = dat_subset, aes(x=.data[[cols[1]]], 
                                      y=.data[[cols[2]]]),
               width = 0.5) + 
      theme_classic() + 
      theme(axis.text.y = element_text(size = 10),
            axis.text.x = element_text(angle = 45, size = 8),
            axis.title = element_text(size = 12)) +
      ggtitle(model_name) +
      ylim(plt_lim) +
      ylab(bquote(.(cols[2]) ~ (mu * S/cm))) + 
      if (plot_month_names){
        if(!is.null(label_sequence)){
          scale_x_continuous(breaks = dat_subset[[1]][label_sequence], 
                             labels = month.abb[dat_subset[[1]]][label_sequence])
        }else{
          scale_x_continuous(breaks = dat_subset[[1]], labels = month.abb[dat_subset[[1]]])
        }
      }else{
        if(!is.null(label_sequence)){
          scale_x_continuous(breaks = dat_subset[[1]][label_sequence], 
                             labels = dat_subset[[1]][label_sequence])
        }else{
          scale_x_continuous(breaks = dat_subset[[1]], labels = dat_subset[[1]])
        }
      }
    
    plot_name <- paste0(file_path,"/",col_name,'_',model_name,".png")
    plot_names <- c(plot_names,plot_name)
    
    suppressWarnings(ggsave(plot_name,plot = attr_plot,width=5,height=3,device = "png"))
    
    if(panel){
      plt_lst <- c(plt_lst, list(attr_plot))
    }
  }
  
  if(panel){
    plot_name <- paste0(file_path,"/panel_",model_name,".png")
    plot_names <- c(plot_names,plot_name)
    
    attr_plot <- plt_lst[[1]] + 
      plt_lst[[2]]+ggtitle('') + 
      patchwork::plot_layout(nrow=2)
    suppressWarnings(ggsave(plot_name,plot = attr_plot,width=5,height=6,device = "png"))
  }
  
  return(plot_names)
}


plot_timeseries <- function(pred_df, network_geometry, model_name, out_dir){
  #' 
  #' @description Creates a timeseries plot for each of the reaches in pred_df.
  #'
  #' @param pred_df dataframe with columns for 'PRMS_segid', 'Date', 'obs', 
  #' '.pred', and 'training' as output from `predict_test_data`
  #' @param network_geometry sf object containing the network flowline geometry; 
  #' must include columns "subsegid" and "geometry"
  #' @param model_name character string describing the model. Will be added 
  #' to the end of the filename before the file extension, and also be the plot title.
  #' @param out_dir output directory
  #'
  #' @return Returns the path to png files containing observed and predicted timeseries
  
  #number of plots equals number of unique reaches in pred_df
  reaches <- unique(pred_df$PRMS_segid) %>%
    sort()
  filesout <- vector('character', length = length(reaches))
  
  #for plotting observation locations, add column of y=0
  pred_df$y0 <- 0
  
  for (i in 1:length(filesout)){
    filesout[i] <- file.path(out_dir, 
                             paste0('timeseries_', model_name, '_reach-', reaches[i], '.png'))
    
    #get all data for this reach in time order
    plt_df <- filter(pred_df, PRMS_segid == reaches[i]) %>%
      arrange(Date)
    
    #for plotting training and testing indicators
    if(!('training' %in% colnames(plt_df))){
      #add training column. Data are all testing.
      plt_df$training <- 0
      plt_labs <- c('testing', 'observed', 'predicted')
      plt_colors <- c('0' = "#F8766D", observed = 'gray20', predicted = "#C77CFF")
    }else{
      if(all(plt_df$training == 0)){
        plt_labs <- c('testing', 'observed', 'predicted')
        plt_colors <- c('0' = "#F8766D", observed = 'gray20', predicted = "#C77CFF")
      }else if (all(plt_df$training == 1)){
        plt_labs <- c('training', 'observed', 'predicted')
        plt_colors <- c('1' = "#7CAE00", observed = 'gray20', predicted = "#C77CFF")
      }else{
        plt_labs <- c('testing', 'training', 'observed', 'predicted')
        plt_colors <- c('0' = "#F8766D", '1' = "#7CAE00", observed = 'gray20', predicted = "#C77CFF")
      }  
    }
    
    #timeseries lineplot
    p_time <- ggplot(data = plt_df, aes(x = Date, y = obs)) +
      {if (nrow(plt_df) == 1){
        geom_point(mapping = aes(color = 'observed'))
      }else{
        geom_line(mapping = aes(color = 'observed'))
      }}+
      #observation locations
      geom_point(shape = '|', mapping = aes(x = Date, y = y0, color = as.character(training))) +
      {if (nrow(plt_df) == 1){
        #.pred is a column name
        geom_point(mapping = aes(x = Date, y = .pred, color = 'predicted'))
      }else{
        geom_line(mapping = aes(x = Date, y = .pred, color = 'predicted'))
      }}+
      theme_bw() +
      theme(legend.position = "bottom") +
      xlab('Date') +
      ylab(expression(paste('Specific Conductivity (', mu, 'S/cm)', sep = ''))) + 
      ggtitle(model_name, subtitle = paste0('reach ', reaches[i])) +
      scale_color_manual('', values = plt_colors, labels = plt_labs)
    
    #spatial location indicator
    p_space <- attr_plot_spatial <- ggplot() + 
      geom_sf(data = network_geometry, 
              size = 0.3, color = 'gray') +
      #specific reach
      geom_sf(data = filter(network_geometry, subsegid == reaches[i]),
              mapping = aes(color='red'),
              size = 1) + 
      theme_bw() + 
      theme(plot.margin = unit(c(0,0,0,0), "cm"),
            axis.text = element_text(size = 4),
            legend.position = "none")
    
    # create combined plot showing timeseries plot and spatial location
    p_combined <- p_time + p_space + patchwork::plot_layout(ncol=2, widths = c(2,1))
    
    ggsave(filename = filesout[i], plot = p_combined, device = 'png')
  }
  
  return(filesout)
}


#maps for different time periods
plot_maps <- function(pred_df, network_geometry, file_path, filename_end = '_full',
                      reservoirs = NULL, 
                      time_aggregation = c('all', 'season', 'month', 'year')){
  #' 
  #' @description Creates maps of error metrics within the pred_df.
  #'
  #' @param pred_df dataframe with columns for 'PRMS_segid', 'Date', 'Month', 
  #' 'Year', and 'errsq' as output from `predict_test_data`
  #' @param network_geometry sf object containing the network flowline geometry; 
  #' must include columns "subsegid" and "geometry"
  #' @param file_path a character string that indicates the location of the saved plot
  #' @param filename_end optional character string to add to the end of the filename
  #' before the file extension.
  #' @param reservoirs shapefile containing reservoir locations. When specified, 
  #' these will be added to the plots.
  #' @param time_aggregation how to aggregate results for each reach. 'all' 
  #' aggregates over all available data. 'season' creates a map for each water 
  #' year season. 'month' creates a map for each calendar month. 'year' creates
  #' a map for each calendar year.
  #'
  #' @return Returns the path to png files containing a violin plot showing
  #'  the distribution of each attribute in the pred_df
  
  filesout <- vector('character', length = 0L)
  
  if('all' %in% time_aggregation){
    #Average RMSE over all time for each PRMS segment
    PRMS_seg_RMSE <- summarize(group_by(pred_df, PRMS_segid), 
                               RMSE = sqrt(mean(errsq, na.rm = TRUE)),
                               RMSE_log10 = log10(RMSE))
    
    filesout <- c(filesout, 
                  plot_nhdv2_attr(attr_data = PRMS_seg_RMSE,
                    network_geometry = network_geometry,
                    file_path = file_path,
                    filename_end = filename_end,
                    reservoirs = reservoirs)
                )
  }
  
  if('year' %in% time_aggregation){
    #Average RMSE within each year for each PRMS segment
    PRMS_seg_RMSE <- summarize(group_by(pred_df, PRMS_segid, Year), 
                               RMSE = sqrt(mean(errsq, na.rm = TRUE)),
                               RMSE_log10 = log10(RMSE))
    
    #Make a map for each year
    years <- sort(unique(PRMS_seg_RMSE$Year))
    for(y in 1:length(years)){
      filename_end_y <- paste0('_year_', years[y], filename_end)
      
      filesout <- c(filesout, 
                    plot_nhdv2_attr(attr_data = filter(PRMS_seg_RMSE, Year == years[y]) %>%
                                      select(-Year),
                      network_geometry = network_geometry,
                      file_path = file_path,
                      filename_end = filename_end_y,
                      reservoirs = reservoirs)
                  )
    }
  }
  
  if('month' %in% time_aggregation){
    #Average RMSE in each calendar month for each PRMS segment
    PRMS_seg_RMSE <- summarize(group_by(pred_df, PRMS_segid, Month), 
                               RMSE = sqrt(mean(errsq, na.rm = TRUE)),
                               RMSE_log10 = log10(RMSE))
    
    #Make a map for each calendar month
    months <- sort(unique(PRMS_seg_RMSE$Month))
    for(m in 1:length(months)){
      filename_end_m <- paste0('_month_', month.name[months[m]], filename_end)
      
      filesout <- c(filesout, 
                    plot_nhdv2_attr(attr_data = filter(PRMS_seg_RMSE, Month == months[m]) %>%
                                      select(-Month),
                                    network_geometry = network_geometry,
                                    file_path = file_path,
                                    filename_end = filename_end_m,
                                    reservoirs = reservoirs)
      )
    }
  }
  
  if('season' %in% time_aggregation){
    #Average RMSE in each water year season for each PRMS segment
    pred_df$Season <- case_when(pred_df$Month %in% c(10,11,12) ~ 'OND',
                                pred_df$Month %in% c(1,2,3) ~ 'JFM',
                                pred_df$Month %in% c(4,5,6) ~ 'AMJ',
                                pred_df$Month %in% c(7,8,9) ~ 'JAS')
    
    PRMS_seg_RMSE <- summarize(group_by(pred_df, PRMS_segid, Season), 
                               RMSE = sqrt(mean(errsq, na.rm = TRUE)),
                               RMSE_log10 = log10(RMSE))
    
    #Make a map for each calendar month
    seasons <- sort(unique(PRMS_seg_RMSE$Season))
    for(s in 1:length(seasons)){
      filename_end_s <- paste0('_season_', seasons[s], filename_end)
      
      filesout <- c(filesout, 
                    plot_nhdv2_attr(attr_data = filter(PRMS_seg_RMSE, Season == seasons[s]) %>%
                                      select(-Season),
                                    network_geometry = network_geometry,
                                    file_path = file_path,
                                    filename_end = filename_end_s,
                                    reservoirs = reservoirs)
      )
    }
  }
  
  return(filesout)
}


#SHAP values
plot_shap_global <- function(shap, model_name, out_dir, num_features = 40,
                             dynamic_attrs_txt = NULL){
  #' 
  #' @description Creates SHAP global importance plot
  #'
  #' @param shap SHAP value results from compute_SHAP
  #' @param model_name character string describing the model. Will be added 
  #' to the end of the filename before the file extension, and also be the plot title.
  #' @param out_dir output directory
  #' @param num_features number of features to plot
  #' @param dynamic_attrs_txt Text file providing the dynamic attribute names as rows. 
  #' When specified, makes a panel plot with static features
  #' in one panel and dynamic attributes in another
  #'
  #' @return Returns the paths to png files of SHAP global feature importance
  
  #Plot for all attributes:
  fileout <- file.path(out_dir, 
                       paste0('SHAP_global_', model_name, '_vars', num_features, '.png'))
  
  p1 <- autoplot(shap, type = "importance", num_features = num_features) +
    ggtitle(model_name) + 
    theme(axis.text.y = element_text(size = 5))
  
  ggsave(filename = fileout, plot = p1, device = 'png')
  
  if (!is.null(dynamic_attrs_txt)){
    #Plot for panels of top static only and dynamic only attributes:
    fileout <- c(fileout, 
                 file.path(out_dir, 
                           paste0('SHAP_global_panels_', model_name, '_vars', num_features, '.png'))
                 )
    
    dynamic_attrs <- read_csv(dynamic_attrs_txt, col_names = FALSE,
                             show_col_types = FALSE)
    
    x_lims <- c(0, ceiling(max(colMeans(abs(shap)))))
    
    p2 <- autoplot(shap[, -which(colnames(shap) %in% dynamic_attrs[[1]])], type = "importance", num_features = num_features) +
      ggtitle(model_name, subtitle = 'Static Attributes') +
      theme(axis.text.y = element_text(size = 5)) +
      ylim(x_lims)
    p3 <- autoplot(shap[, which(colnames(shap) %in% dynamic_attrs[[1]])], type = "importance", num_features = num_features) +
      ggtitle(model_name, subtitle = 'Dynamic Attributes') +
      theme(axis.text.y = element_text(size = 5)) +
      ylim(x_lims)
    
    ggsave(filename = fileout[2], plot = p2+p3, device = 'png')
  }
  
  return(fileout)
}
plot_shap_global_sv <- function(shap, data, model_name, out_dir, num_features = 40,
                                sv_kind = 'beeswarm', drop_columns, scale_shap = NULL,
                                xlims = NULL, dynamic_attrs_txt = NULL){
  #' 
  #' @description Creates SHAP global importance plot using the shapviz package
  #'
  #' @param shap SHAP value results from compute_SHAP
  #' @param data attributes data for the columns within shap
  #' @param model_name character string describing the model. Will be added 
  #' to the end of the filename before the file extension, and also be the plot title.
  #' @param out_dir output directory
  #' @param sv_kind kind of shapviz plot. bar, beeswarm, or both.
  #' @param drop_columns character vector of columns to remove 
  #' (e.g., because they're identifiers or dates)
  #' @param scale_shap if not NULL, scales the SHAP value by raising to the power of
  #' scale_shap. Can be useful when SHAP values span orders of magnitude, but
  #' this may change the order of variables due to nonlinear scaling
  #' @param xlims if not NULL, a vector containing the desired x and y axis limits
  #' @param dynamic_attrs_txt Text file providing the dynamic attribute names as rows. 
  #' When specified, makes a panel plot with static features
  #' in one panel and dynamic attributes in another
  #'
  #' @return Returns the paths to png files of SHAP dependence plots for each feature
  
  #remove desired columns
  shap <- shap[,-which(colnames(shap) %in% drop_columns)]
  data <- select(data, -all_of(drop_columns))
  
  #scale SHAP values so that distributions are visible along the x-axis
  if(!is.null(scale_shap)){
    shap <- as.matrix(shap)
    #get indices of negative values
    ind_neg <- which(shap < 0)
    shap[ind_neg] <- shap[ind_neg]*-1
    shap <- shap^scale_shap %>%
      as.matrix()
    shap[ind_neg] <- shap[ind_neg]*-1
  }
  
  filesout <- file.path(out_dir, 
                        paste0('SHAP_global_bee_', model_name, '_vars', num_features, '.png'))
  
  p1 <- sv_importance(shapviz(shap, X = data), 
                      kind = sv_kind, max_display = num_features, fill = 'black',
                      alpha = 0.5) +
    #Note, sv_importance flips the x and y axes internally, so the plotted x-axis
    # initially is the y-axis.
    {if(!is.null(xlims)){ylim(xlims)}} +
    ggtitle(model_name)
  
  ggsave(filename = filesout, plot = p1, device = 'png')
  
  if (!is.null(dynamic_attrs_txt)){
    #Plot for panels of top static only and dynamic only attributes:
    filesout <- c(filesout, 
                 file.path(out_dir, 
                           paste0('SHAP_global_bee_panels_', model_name, '_vars', num_features, '.png'))
                 )
    
    dynamic_attrs <- read_csv(dynamic_attrs_txt, col_names = FALSE,
                              show_col_types = FALSE)
    
    if(is.null(xlims)){
      x_lims <- c(floor(min(shap)), ceiling(max(shap)))
    }
    
    p2 <- sv_importance(shapviz(shap[, -which(colnames(shap) %in% dynamic_attrs[[1]])], X = data), 
                        kind = sv_kind, max_display = num_features, fill = 'black',
                        alpha = 0.5) +
      #Note, sv_importance flips the x and y axes internally, so the plotted x-axis
      # initially is the y-axis.
      ylim(xlims) +
      ggtitle(model_name, subtitle = 'Static Attributes') +
      #hide legend from the first panel because it's the same as the second.
      theme(legend.position="none")
    
    p3 <- sv_importance(shapviz(shap[, which(colnames(shap) %in% dynamic_attrs[[1]])], X = data), 
                        kind = sv_kind, max_display = num_features, fill = 'black',
                        alpha = 0.5) +
      #Note, sv_importance flips the x and y axes internally, so the plotted x-axis
      # initially is the y-axis.
      ylim(xlims) +
      ggtitle(model_name, subtitle = 'Dynamic Attributes')
    
    ggsave(filename = filesout[2], plot = p2+p3, device = 'png', width = 12, height = 8, units = 'in')
  }
  
  return(filesout)
}


plot_shap_dependence <- function(shap, data, model_name, out_dir, ncores = 1){
  #' 
  #' @description Creates SHAP dependence plots for each feature
  #'
  #' @param shap SHAP value results from compute_SHAP
  #' @param data the X dataframe used to compute SHAP values
  #' @param model_name character string describing the model. Will be added 
  #' to the end of the filename before the file extension, and also be the plot title.
  #' @param out_dir output directory
  #' @param ncores number of cores to use for parallel plot creation
  #'
  #' @return Returns the paths to png files of SHAP dependence plots for each feature
  
  #number of features to make plots for
  n_plts <- ncol(shap)
  
  cl = parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  
  filesout <- foreach(i = 1:n_plts, .inorder = TRUE, .combine = c, 
                      .packages = c('ggplot2', 'fastshap')) %dopar% {
    fileout <- file.path(out_dir, 
                             paste0('SHAP_dependence_', colnames(shap)[i], '_',
                                    model_name, '.png'))
    
    p <- autoplot(shap, type = "dependence", feature = colnames(shap)[i], 
                  X = data, 
                  alpha = 0.5, smooth = TRUE, smooth_color = "blue") +
      ggtitle(model_name)
    
    ggsave(filename = fileout, plot = p, device = 'png')
    
    fileout
  }
  
  parallel::stopCluster(cl)
  
  return(filesout)
}
plot_shap_dependence_sv <- function(shap, data, model_name, out_dir, ncores = 1){
  #' 
  #' @description Creates SHAP dependence plots for each feature using the shapviz package
  #'
  #' @param shap SHAP value results from compute_SHAP
  #' @param data the X dataframe used to compute SHAP values
  #' @param model_name character string describing the model. Will be added 
  #' to the end of the filename before the file extension, and also be the plot title.
  #' @param out_dir output directory
  #' @param ncores number of cores to use for parallel plot creation
  #'
  #' @return Returns the paths to png files of SHAP dependence plots for each feature
  
  cl = parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  
  #number of features to make plots for
  n_plts <- ncol(shap)
  
  #convert to shapviz object
  shap <- shapviz(shap, X = data)
  
  filesout <- foreach(i = 1:n_plts, .inorder = TRUE, .combine = c, 
                      .packages = c('ggplot2', 'fastshap', 'shapviz')) %dopar% {
                        fileout <- file.path(out_dir, 
                                             paste0(model_name, '_', 
                                                    'SHAP_dependence_', 
                                                    colnames(shap$X)[i], 
                                                    '.png'))
                        
                        p <- sv_dependence(shap, 
                                           v = colnames(shap$X)[i],
                                           alpha = 0.5) +
                          geom_smooth(method = 'loess', se = FALSE, show.legend = FALSE) +
                          ggtitle(model_name)
                        
                        ggsave(filename = fileout, plot = p, device = 'png')
                        
                        fileout
              }
  
  parallel::stopCluster(cl)
  
  return(filesout)
}

plot_shap_individual <- function(shap, data, reach, date, model_name, out_dir,
                                 num_features = 40){
  #' 
  #' @description Creates a SHAP contribution plot for an individual prediction index.
  #'
  #' @param shap SHAP value results from compute_SHAP
  #' @param data dataframe with PRMS_segid and Date columns with rows in the
  #' same order as shap
  #' @param reach PRMS_segid of observation to plot
  #' @param date date of observation to plot as YYYY-MM-DD
  #' @param model_name character string describing the model. Will be added 
  #' to the end of the filename before the file extension, and also be the plot title.
  #' @param out_dir output directory
  #'
  #' @return Returns the path to the png file of feature contributions to the index prediction
  
  #row index for which to compute plot
  ind_plt <- which(data$PRMS_segid == reach & data$Date == date)
  
  fileout <- file.path(out_dir, paste0('SHAP_individual_', model_name, 
                                       '_reach-', reach, 
                                       '_date-', date, '.png'))
  
  p1 <- autoplot(shap[ind_plt,], type = "contribution", num_features = num_features) +
    ggtitle(model_name, subtitle = paste0('reach ', reach,
                   ', Date ', date)) +
    theme(axis.text.y = element_text(size = 5))
  
  ggsave(filename = fileout, plot = p1, device = 'png')
  
  return(fileout)
}


#' plot_pdp <- function(data, model_name, out_dir){
#'   #' 
#'   #' @description Creates PDP and ICE plots for each feature in data
#'   #'
#'   #' @param data the dataframe used to make model predictions within the workflow
#'   #' @param model_name character string describing the model. Will be added 
#'   #' to the end of the filename before the file extension, and also be the plot title.
#'   #' @param out_dir output directory
#'   #'
#'   #' @return Returns the paths to png files of PDP and ICE for each feature
#'   
#'   #number of features to make plots for
#'   n_plts <- ncol(data)
#'   
#'   filesout_pdp <- vector('character', length = n_plts)
#'   filesout_ice <- vector('character', length = n_plts)
#'   
#'   for(i in 1:n_plts){
#'     filesout_pdp[i] <- file.path(out_dir, 
#'                              paste0('PDP_', colnames(data)[i], '_',
#'                                     model_name, '.png'))
#'     filesout_ice[i] <- file.path(out_dir, 
#'                                  paste0('ICE_', colnames(data)[i], '_',
#'                                         model_name, '.png'))
#'     
#'     #PDP
#'     
#'     #get data into class partial to make plots
#'     pdp::partial(object = ..., pred.var = ..., ice = FALSE, train = ..., type = 'regression', pred.fun = predict_shap_data,
#'                  plot = TRUE, rug = TRUE, smooth = TRUE, plot.engine = 'ggplot2', )
#'     
#'     partial_1 <- pdp::partial(object = p4_train_RF_min_static_dynamic_temporal$workflow, 
#'                  pred.var = colnames(p4_train_RF_min_static_dynamic_temporal$best_fit$splits[[1]]$data)[1],
#'                  plot = FALSE, 
#'                  train = as.data.frame(p4_train_RF_min_static_dynamic_temporal$best_fit$splits[[1]]$data[p4_train_RF_min_static_dynamic_temporal$best_fit$splits[[1]]$in_id,]), 
#'                  type = 'regression', 
#'                  pred.fun = predict_shap_data,
#'                  quantiles = TRUE, probs = seq(0,1,0.1), grid.resolution = NULL)
#'                  
#'     p <- autoplot(partial_1, rug = TRUE,
#'                   train = as.data.frame(p4_train_RF_min_static_dynamic_temporal$best_fit$splits[[1]]$data[p4_train_RF_min_static_dynamic_temporal$best_fit$splits[[1]]$in_id,])) +
#'       ggtitle(model_name)
#' 
#'     ggsave(filename = filesout_pdp[i], plot = p, device = 'png')
#'     
#'     ggsave(filename = filesout_ice[i], plot = p, device = 'png')
#'   }
#'   
#'   filesout <- c(filesout_pdp, filesout_ice)
#'   return(filesout)
#' }