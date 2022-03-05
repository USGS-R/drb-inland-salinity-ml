library(targets)
library(dpyr)
library(tidyr)

tar_make(p1_prms_reach_attr)
tar_make(p2_FORESCE_LC_per_catchment_reclass)
tar_load(p1_prms_reach_attr)
tar_load(p2_FORESCE_LC_per_catchment_reclass)

#####

reach_attr_table <- p1_prms_reach_attr %>%
  dplyr::select(subseg_id,subseg_seg,from_segs,to_seg) %>% 
  mutate(from_segs = stringr::str_split(string = from_segs, pattern = ';', simplify = F))

rec_fun<- function(x, df, col1, col2){
  #' @x subseg_seg object
  #' @df dataframe to loop up 
  #' @col1 the name of first col (as str) of the value x
  #' @col2 the name of second col (as str) of the source segs
  
  # get row number where df$col1 is x 
  row_num <- which(df[[col1]] == x)
  
  #get the from_segs and recursively loop over them
  val <- df[[col2]][row_num][[1]]
  for (i in 1:length(val)){
    if(!is.na(val[i])){
      return(c(val, Recall(x = val[i],df = df, col1 = col1, col2 = col2)))
    }else{
      #this returns nothing, otherwise the last element in the vector is NA. 
      return(NULL)
    }
  }
}

## across full reach df
reach_attr_table$all_from_segs <- lapply(reach_attr_table$subseg_seg, function(x) rec_fun(x = x, df = reach_attr_table, col1 = 'subseg_seg', col2 = 'from_segs')) 

View(reach_attr_table %>% mutate(tmp = ifelse(is.null(all_from_segs) == T, TRUE, c(all_from_segs,subseg_seg))))

all_upstream_segs <- reach_attr_table %>%
  ## unest to have new rows for each upstream catchment
  tidyr::unnest(all_from_segs) %>%
  ## change col type to be able to compute
  mutate(all_from_segs = as.integer(all_from_segs)) %>%
  ## join prop calculations
  left_join(p2_FORESCE_LC_per_catchment_reclass[[1]], by = c('all_from_segs' = 'hru_segment')) %>%
  ## group by PRMS id 
  group_by(PRMS_segid) %>% 
  
  summarise(
    ## cal total area
    total_upstream_PRMS_area = sum(total_PRMS_area),
    ## get proportions for the new total area
    across(starts_with('prop'), ~(sum((.x*total_PRMS_area)/total_upstream_PRMS_area))))
