library(tidyr)
library(stringr)
library(dplyr)
library(stringr)
library(data.table)

reclassify_land_cover <- function(land_cover_df, reclassify_table_path, sep = ',', pivot_longer_starts_with, pivot_names_to, pivot_values_to){
  
  reclassify_table <- read.csv(reclassify_table_path, sep = sep)
  merge_colname <- names(reclassify_table)[1]
  
  as.data.frame(land_cover_df) %>% 
  pivot_longer(cols = starts_with(pivot_longer_starts_with),
               names_to = pivot_names_to,
               values_to = pivot_values_to) %>% 
  mutate(merge_colname = as.numeric(str_extract(pivot_names_to, '\\d'))) %>% 
  left_join(reclassify_table, by = merge_colname) %>%
   # select(-starts_with('FORESCE')) %>% 
  group_by(Reclassify_match, Reclassify_description) %>% 
  summarise(sum_prop_class_in_catchment = sum(Prop_class_in_catchment, na.rm = T))
  
  
  
}