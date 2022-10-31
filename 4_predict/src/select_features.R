drop_high_corr_attrs <- function(features, drop_columns_contains = NULL, 
                                 drop_columns_select = NULL,
                                 dynamic_contains,
                                 threshold_corr, first_drop_var_prefix = NULL,
                                 cor_method){
  #' 
  #' @description Function to remove attributes that are undesired or are 
  #' highly correlated with other attributes. 
  #'
  #' @param features table of features (columns) for each prediction point (rows).
  #' @param drop_columns_contains character vector of column names to remove from features. 
  #' This is passed to dplyr::contains()
  #' @param drop_columns_select character vector of exact column names to remove from features.
  #' This is passed to dplyr::select()
  #' @param dynamic_contains character vector of column name formats that indicate
  #' an attribute is dynamic. This is passed to dplyr::contains()
  #' @param threshold_corr correlation threshold used to drop attributes.
  #' Absolute value of correlation is used.
  #' @param first_drop_var_prefix if not NULL, an initial screening will be made
  #' and highly correlated features with the specified prefix will be dropped. 
  #' Example prefix for NHD features: "CAT", "ACC", or "TOT"
  #' @param cor_method correlation method for cor function.
  #' 
  #' @return character vector of features that are retained
  
  if (!is.null(drop_columns_contains)){
    print(paste('Removing feature:', select(features, contains(drop_columns_contains)) %>% 
              colnames()))
    features <- select(features, !contains(drop_columns_contains))
  }
  if (!is.null(drop_columns_select)){
    print(paste('Removing feature:', select(features, all_of(drop_columns_select)) %>% 
                  colnames()))
    features <- select(features, !all_of(drop_columns_select))
  }
  
  #Drop rows with NAs
  features <- drop_na(features)
  
  #get all of the features that are highly correlated
  high_corr_features <- get_high_corr_features(features, cor_method, threshold_corr)
  
  if (!is.null(first_drop_var_prefix)){
    #initial screening
    #Find where first_drop_var_prefix features are highly correlated with 
    # non-first_drop_var_prefix features and remove
    names_cor <- colnames(features)
    remove_vars <- vector('character', length = 0L)
    for(i in 1:nrow(high_corr_features)){
      name <- rownames(high_corr_features)[i]
      if(substr(name, 1,3) == first_drop_var_prefix){
        #Check that the column is not a first_drop_var_prefix
        if(substr(names_cor[high_corr_features[i,2]], 1, 3) != first_drop_var_prefix){
          #add the first_drop_var_prefix name to vector for removal
          remove_vars <- c(remove_vars, name)
        }
      }
    }
    remove_vars <- unique(remove_vars)
    
    #Drop all of the remove_vars
    features <- select(features, -{{remove_vars}})
    
    #Correlation matrix for features without the remove_vars
    high_corr_features <- get_high_corr_features(features, cor_method, threshold_corr)
  }
  
  #Methods used to manually check hydrologic properties for preferential inclusion/removal
  #sort(unique(rownames(high_corr_features)))
  #get names of features that are highly correlated with selected feature
  #colnames(features[,as.numeric(high_corr_features[rownames(high_corr_features) == 'CAT_LSTFZ6190_area_wtd',2])])
  #look at scatterplot
  #plot(p2_all_attr_SC_obs$TOT_MAJOR_0, p2_all_attr_SC_obs$TOT_BASIN_AREA, log = 'y')
  #see what the correlation is
  #abs(cor(p2_all_attr_SC_obs$sph_0_1days_mean, p2_all_attr_SC_obs$sph_0, method = cor_method))
  
  #Automatic removal of all other highly correlated features
  #Check static attributes:
  static_check <- features %>% 
    select(!contains(dynamic_contains))
  #Correlation matrix for only static features
  high_corr_features <- get_high_corr_features(static_check, cor_method, threshold_corr)
  while (nrow(high_corr_features) > 0){
    #randomly select an attribute and remove all correlated attributes
    tmp_col <- sample(unique(rownames(high_corr_features)), size = 1)
    tmp_rm <- static_check[, high_corr_features[rownames(high_corr_features) == tmp_col,2] %>%
                             as.numeric()] %>%
      colnames()
    
    static_check <- static_check %>% select(-{{tmp_rm}})
    #Correlation matrix for features without the remove_vars
    high_corr_features <- get_high_corr_features(static_check, cor_method, threshold_corr)
  }
  
  
  #Check dynamic attributes:
  dynamic_check <- features %>% 
    select(contains(dynamic_contains))
  dynamic_check_vars <- dynamic_check
  #Correlation matrix for dynamic features
  high_corr_features <- get_high_corr_features(dynamic_check_vars, cor_method, 
                                               threshold_corr)
  #In this loop, we'll retain all unique prefixes
  checked_vars <- vector('character', length = 0L)
  while (nrow(high_corr_features) > 0){
    #randomly select an attribute and gather all of its correlated attributes
    # all of these are candidates for removal
    tmp_col <- sample(unique(rownames(high_corr_features)), size = 1)
    tmp_rm <- dynamic_check_vars[, high_corr_features[rownames(high_corr_features) == tmp_col,2] %>%
                             as.numeric()] %>%
      colnames()
    tmp_rm <- c(tmp_col, tmp_rm)
    checked_vars <- c(checked_vars, tmp_col)
    if (tmp_col %in% colnames(dynamic_check)){
      #Want to retain the smallest timestep for each prefix
      #Find each unique prefix
      prefixes <- get_attr_prefixes(dynamic_contains, dynamic_attrs = tmp_rm)
      
      #Loop over the prefixes
      for(prefix in prefixes){
        #retain the smallest timestep for each prefix
        tmp_split <- str_split(string = tmp_rm, 
                               pattern = paste0(prefix,'_'), 
                               simplify = T)[,2]
        prefix_inds <- which(str_length(tmp_split) > 0)
        tmp_split <- tmp_split[prefix_inds]
        
        if ('0' %in% tmp_split){
          #retain the 0s and drop the rest. This is day-of information.
          tmp_rm_prefix <- grep(tmp_rm[prefix_inds], pattern = paste0(prefix, '_0$'), 
                                invert = TRUE, value = TRUE)
        }else{
          #search for days, weeks, months, years in column names
          #and convert all to days to compare their lengths
          days_equivalent <- vector('numeric', length = length(tmp_split))
          for (date_length in c('days', 'weeks', 'months', 'years')){
            if(length(grep(date_length, tmp_split)) > 0){
              #convert to days to compare the length
              days_equivalent[grep(date_length, tmp_split)] <- convert_to_days(grep(date_length, 
                                                                                    tmp_split, 
                                                                                    value = TRUE), 
                                                                               pattern = date_length)
              
            }
          }
          tmp_split <- tmp_split[which(days_equivalent == min(days_equivalent))]
          if (length(tmp_split) > 1){
            #this happens when the average over all days and the exact value on
            #the lagged day are selected.
            #select averaged values instead of exact values
            tmp_split <- grep(tmp_split, pattern = '_0_', value = TRUE)
          }
          tmp_rm_prefix <- grep(tmp_rm[prefix_inds], 
                                pattern = paste0(prefix, '_', tmp_split, '$'), 
                                invert = TRUE, value = TRUE)
        }
        
        #Remove columns from dynamic_check
        var_select <- tmp_rm_prefix[tmp_rm_prefix %in% colnames(dynamic_check)]
        if (length(var_select) > 0){
          dynamic_check <- dynamic_check %>% select(-{{var_select}})
        }
      }
    }
    
    #Remove columns from dynamic_check_vars, which is used to ensure that 
    #the while loop is not infinite. Once all variables are checked, it will stop
    var_select <- checked_vars[checked_vars %in% colnames(dynamic_check_vars)]
    if (length(var_select) > 0){
      dynamic_check_vars <- dynamic_check_vars %>% select(-{{var_select}})
    }
    if (ncol(dynamic_check_vars) > 0){
      #Correlation matrix
      high_corr_features <- get_high_corr_features(dynamic_check_vars, cor_method, 
                                                   threshold_corr)
    }else{
      #All features checked. End loop.
      high_corr_features <- high_corr_features[0,]
    }
  }
  
  retain_features <- sort(c(colnames(static_check), colnames(dynamic_check)))
  
  return(retain_features)
}


convert_to_days <- function(string_vec, pattern){
  #' 
  #' @description Function to convert a string from #days #months, #weeks, #years to 
  #' equivalent number of days 
  #'
  #' @param string_vec character string containing the specified pattern preceeded
  #' by a number (e.g., 10months)
  #' @param pattern character string stating which time period to look for (days,
  #' weeks, months, or years)
  #' 
  #' @return Returns numeric days
  
  #remove any underscores
  tmp_str <- str_split(string_vec, pattern = '_', simplify = TRUE)
  #find the pattern and retrieve the number of days, which preceedes the pattern
  tmp_str <- str_split(grep(tmp_str, pattern = pattern, value = TRUE), pattern = pattern, 
            simplify = TRUE)[,1] %>% 
    as.numeric()
  #get equivalent days. I don't know of a lubridate function that will work
  #because they need to know how many days are in each month/year 
  #(leap years cause differences). So, I'm doing a manual conversion here. 
  #For its intended use, the conversion can be approximate because we're trying to
  #find which of the specified time periods among many string_vecs are shorter.
  if(pattern == 'weeks'){
    tmp_str <- tmp_str*7 
  }else if(pattern == 'months'){
    tmp_str <- tmp_str*30
  }else if(pattern == 'years'){
    tmp_str <- tmp_str*365 
  }
  
  return(tmp_str)
}

get_high_corr_features <- function(features, cor_method, threshold_corr){
  #' 
  #' @description Function to get the indices of highly correlated features.
  #'
  #' @param features table of features (columns) for each prediction point (rows).
  #' @param cor_method correlation method for cor function.
  #' @param threshold_corr correlation threshold used to drop attributes.
  #' Absolute value of correlation is used.
  #' 
  #' @return matrix of features that have correlation greater than threshold_corr.
  #' row and column indicies listed in the matrix correspond to the columns in features.
  
  #Correlation matrix
  cor_mat <- abs(cor(features, method = cor_method))
  
  #Find all of the features that are highly correlated
  high_corr_features <- which(cor_mat >= threshold_corr, arr.ind = T)
  #Remove diagonal
  high_corr_features <- high_corr_features[
    -which(high_corr_features[,1] == high_corr_features[,2]),]
  
  return(high_corr_features)
}

get_attr_prefixes <- function(dynamic_contains, dynamic_attrs){
  #' 
  #' @description Finds the unique attribute prefixes for dynamic attribute names
  #' that are provided in dynamic_attrs
  #'
  #' @param dynamic_contains character vector of column name formats that indicate
  #' an attribute is dynamic. This is passed to dplyr::contains().
  #' @param dynamic_attrs character vector of attribute names. These names have
  #' the format prefix_dynamic_contains.
  #' 
  #' @return character vector of attribute prefixes
  
  prefixes <- vector('character', length = 0L)
  #loop over each of the dynamic column name formats
  for (fmt1 in 1:length(dynamic_contains)){
    tmp_split <- str_split(string = dynamic_attrs, 
                           pattern = dynamic_contains[fmt1], 
                           simplify = T)
    #gather all prefixes
    for(fmt2 in 1:(length(dynamic_contains)-1)){
      if (fmt2 == 1){
        tmp_prefix <- grep(pattern = dynamic_contains[-fmt1][fmt2], 
                           x = tmp_split[,1], 
                           value = TRUE, invert = TRUE)
      }else{
        tmp_prefix <- grep(pattern = dynamic_contains[-fmt1][fmt2], 
                           x = tmp_prefix, 
                           value = TRUE, invert = TRUE)
      }
    }
    prefixes <- unique(c(prefixes, tmp_prefix))
  }
  
  return(prefixes)
}
