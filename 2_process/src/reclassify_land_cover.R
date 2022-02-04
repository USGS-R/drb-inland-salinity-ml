library(tidyr)
library(stringr)
library(dplyr)
library(stringr)
library(data.table)

reclassify_land_cover <- function(land_cover_df,
                                  reclassify_table_path,
                                  reclassify_table_lc_col, reclassify_table_reclass_col,
                                  sep = ',',
                                  pivot_longer_contains){
  
  reclassify_table <- read.csv(reclassify_table_path, sep = sep) %>% 
    select(reclassify_table_lc_col, reclassify_table_reclass_col)
  print(reclassify_table)
  
  a <- as.data.frame(land_cover_df) %>% 
  pivot_longer(cols = contains(pivot_longer_contains),
               names_to = 'old_class',
               values_to = 'Prop_class_in_catchment') %>% 
    mutate(merge_col = as.numeric(stringr::str_extract(old_class, '\\d+$'))) %>% 
    left_join(reclassify_table, by = c('merge_col' = reclassify_table_lc_col))
  
  print(a)
  
  b <- a %>% 
    mutate(new_class = paste0(pivot_longer_contains, '_', .[[reclassify_table_reclass_col]])) %>% 
    select(-c(old_class, merge_col, reclassify_table_reclass_col))

  c <- pivot_wider(b, names_from = new_class, names_prefix = 'prop_', values_from = Prop_class_in_catchment, values_fn = sum)
  
  return(c)
  
  }

tmp <- reclassify_land_cover(land_cover_df = p2_FORESCE_LC_per_catchment[[4]],
                             reclassify_table_path = '1_fetch/in/Legend_FORESCE_Land_Cover.csv', 
                             reclassify_table_lc_col = 'FORESCE_value', reclassify_table_reclass_col = 'Reclassify_match',
                             sep = ',',
                             pivot_longer_contains = 'lcClass')
dim(tmp)

tmp2 <- reclassify_land_cover(land_cover_df = p2_PRMS_lc_proportions,
                             reclassify_table_path = '1_fetch/in/Legend_NLCD_land_Cover.csv', 
                             reclassify_table_lc_col = 'NLCD_value', reclassify_table_reclass_col = 'Reclassify_match',
                             sep = ',',
                             pivot_longer_contains = 'NLCD11')
