#' @title Plot empirical CDFs of model performance
#' 
#' @description
#' Function to plot empirical CDFs of model performance and make one
#' curve per model. The user can adjust the plot aesthetics using 
#' several arguments that control plot formatting.
#' 
#' @param model_results data frame containing the model results. Must contain
#' columns "model", "PRMS_segid", "Date", and "errsq".
#' @param plot_type character string indicating the type of eCDF plot to return.
#' Options include 1) "all_reaches" to plot one CDF over all reaches; 
#' 2) "select_reaches" to return one CDF for each reach within `segids_select`; and
#' 3) "select_times" to return a grid of CDF plots for each month and year in 
#' `months_select` and `years_select`, respectively. Defaults to "all_reaches".
#' @param fileout character string with the name of the saved cdf plot, including
#' the file path and extension. 
#' @param log_x_axis logical; should the CDF plot include a logged x axis?
#' Defaults to TRUE.
#' @param segids_select character string indicating which segment identifiers to 
#' include in the CDF plot. Only used if `plot_type` is "select_reaches".
#' @param months_select character string or character vector indicating which months
#' to include in the CDF plots. Format is 2-digit integer months (e.g. "01" for January). 
#' Only used if `plot_type` is "select_times".
#' @param years_select character string or character vector indicating which years
#' to include in the CDF plots. Format is 4-digit integer years (e.g. "2020"). 
#' Only used if `plot_type` is "select_times".
#' 
#' @returns 
#' Saves png file containing the eCDF plot(s) of model performance. 
#' 
plot_ecdf <- function(model_results, plot_type = "all_reaches", fileout, 
                      log_x_axis = TRUE, plot_points = TRUE,
                      segids_select, months_select, years_select,
                      plot_width_in = 5, plot_height_in = 4, 
                      points_size = 0.5, line_size = 0.25, 
                      axis_text_size = 10, axis_title_size = 12, 
                      plot_title = "", plot_title_size = 12, plot_title_face = "plain",
                      facet_strip_color = "lightgray"){
  
  # don't use scientific notation for plot axes
  options(scipen = 999)
  
  # To plot one CDF over all reaches, compute the average RMSE over all dates
  # for each reach, and then make the CDF for those average values.
  if(plot_type == "all_reaches"){
    preds <- model_results %>%
      group_by(model, PRMS_segid) %>%
      summarize(rmse = sqrt(mean(errsq)),
                rmse_log10 = log10(rmse),
                .groups = "drop")
    
    p <- ggplot(preds, aes(x = rmse, color = model)) + 
      stat_ecdf(geom = "step", pad = FALSE, size = line_size) +
      scale_color_scico_d(palette = 'berlin')
    
    if(plot_points){
      p1 <- p + stat_ecdf(geom = "point", pad = FALSE, size = points_size)
    } else {
      p1 <- p
    }
  }

  # To plot one CDF plot for each reach, compute the RMSE for each date
  # for each reach, and then make the CDF for those values. (This procedure
  # is equal to taking the sqrt of errsq for each row of model_results$pred
  # since each segment has only one error value for every date.)
  if(plot_type == "select_reaches"){
    preds <- model_results %>%
      filter(PRMS_segid %in% segids_select) %>%
      group_by(model, PRMS_segid, Date) %>%
      summarize(rmse = sqrt(mean(errsq)),
                rmse_log10 = log10(rmse),
                .groups = "drop")
    
    p <- ggplot(preds, aes(x = rmse, color = model)) + 
      stat_ecdf(geom = "step", pad = FALSE, size = line_size) 
    
    if(plot_points){
      p1 <- p + stat_ecdf(geom = "point", pad = FALSE, size = points_size) +
        facet_wrap(~PRMS_segid, scales = "free") + 
        theme(strip.background = element_rect(fill = facet_strip_color))
    } else {
      p1 <- p +
        facet_wrap(~PRMS_segid, scales = "free") + 
        theme(strip.background = element_rect(fill = facet_strip_color))
    }
  }
    
  # Plot one CDF for selected months/years
  if(plot_type == "select_times"){
    preds <- model_results %>%
      mutate(month = lubridate::month(Date),
             year = lubridate::year(Date)) %>%
      filter(month %in% months_select, 
             year %in% years_select) %>%
      group_by(model, year, month, PRMS_segid) %>%
      summarize(rmse = sqrt(mean(errsq)),
                rmse_log10 = log10(rmse),
                .groups = "drop")  
    
    p <- ggplot(preds, aes(x = rmse, color = model)) + 
      stat_ecdf(geom = "step", pad = FALSE, size = line_size)
    
    if(plot_points){
      p1 <- p + stat_ecdf(geom = "point", pad = FALSE, size = points_size) + 
        facet_grid(month ~ year, scales = "free") + 
        theme(strip.background = element_rect(fill = facet_strip_color))
    } else {
      p1 <- p + 
        facet_grid(month ~ year, scales = "free") + 
        theme(strip.background = element_rect(fill = facet_strip_color))
    }
  }
  
  # Edit plot aesthetics
  if(log_x_axis){
    p2 <- p1 + scale_x_log10()
  } else {
    p2 <- p1
  }
  
  p3 <- p2 + 
    labs(x = expression(RMSE~(mu*S/cm)),
         y = "Empirical CDF") +
    theme_bw() +
    ggtitle(plot_title) +
    theme(axis.text = element_text(size = axis_text_size, color = "black"),
          axis.title = element_text(size = axis_title_size, color = "black"),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 12)),
          plot.title = element_text(size = plot_title_size, face = plot_title_face))
  
  # save plot and return file
  ggsave(fileout, plot = p3, width = plot_width_in, height = plot_height_in)
  return(fileout)
}

