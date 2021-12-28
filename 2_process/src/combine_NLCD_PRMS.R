# gather land cover attributes for PRMS segments
library(readr)
library(sf)
library(targets)
library(data.table)

# Read in prms-nhd crosswalk table
tar_load(p2_prms_nhdv2_xwalk)
tar_load(p1_NLCD_df)

# Create long table with all comids identified for DRB
drb_comids <- p2_prms_nhdv2_xwalk %>%
  # split df by PRMS_segid() -  
  split(.,.$PRMS_segid) %>%
  # loop through each row element of this list and str split such that each new col is a cell
  lapply(.,function(x){
    comids <- data.frame(PRMS_segid = x$PRMS_segid,comid=unlist(strsplit(x$comid_all,split=";")))
  }) %>%
  bind_rows()

lc <- p1_NLCD_df$NLCD_LandCover_2011
head(lc)

# land cover attributes are given in percentages, so need to gather catchment areas from nhd vaa tables
tar_load(p1_nhdv2reaches_sf)
area_att <- p1_nhdv2reaches_sf %>% st_drop_geometry() %>% select(COMID,AREASQKM,TOTDASQKM)

# Filter land cover attributes for DRB comids that intersect PRMS segments
lc_drb <- lc %>% filter(COMID %in% drb_comids$comid) %>%
  left_join(., area_att, by="COMID") %>%
  mutate(COMID = as.character(COMID))

# Try out land cover aggregation for just one NLCD class 
# [Deciduous forest, class 41, https://www.mrlc.gov/data/legends/national-land-cover-database-2011-nlcd2011-legend]
dat <- drb_comids %>%
  ## Add PRMS codes
  left_join(.,lc_drb, by=c("comid"="COMID")) %>%
  ## select only relevant cols - here just CAT_NLCD_41
  select(PRMS_segid,comid,AREASQKM,CAT_NLCD11_41) %>%
  ## Create new column multiplying % to area
  mutate(AREA_CAT_NLCD11_41 = AREASQKM * (CAT_NLCD11_41/100)) %>%
  ## group by PRMS - so no more comid ID index by aggregating the areas for each comid to the PRMS level (sum)
  group_by(PRMS_segid) %>% summarize(AREA_CAT_NLCD11_41 = sum(AREA_CAT_NLCD11_41),
            AREA_sum_km2 = sum(AREASQKM),
            .groups="drop") %>% mutate(AREA_PCT_NLCD11_41 = (AREA_CAT_NLCD11_41/AREA_sum_km2) * 100)

dat_cat <- drb_comids %>%
  left_join(.,lc_drb,by=c("comid"="COMID")) %>%
  mutate(across(starts_with("CAT"),~(.x/100)*AREASQKM,.names="AREA_{col}")) %>%
  group_by(PRMS_segid) %>%
  summarize(across(starts_with("AREA_CAT"),sum,.names="PRMS_CAT_PERCENT_{col}"),
            across(starts_with("PRMS_CAT"),round,2),
            .groups="drop")

