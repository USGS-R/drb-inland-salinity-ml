source("1_fetch/src/get_nwis_sites.R")
source("1_fetch/src/get_daily_nwis_data.R")
source("1_fetch/src/get_inst_nwis_data.R")

p1_targets_list <- list(
  
  # Load harmonized WQP data product for discrete samples
  tar_target(
    p1_wqp_data,
    readRDS(file = "1_fetch/in/DRB.WQdata.rds")),
  
  # Identify NWIS sites with SC data 
  tar_target(
    p1_nwis_sites,
    {
      dummy <- dummy_date
      get_nwis_sites(drb_huc8s,pcodes_select,site_tp_select,stat_cd_select)
    }
    ),
  
  # Subset daily NWIS sites
  tar_target(
    p1_nwis_sites_daily,
    p1_nwis_sites %>%
      filter(data_type_cd=="dv",!(site_no %in% omit_nwis_sites)) %>%
      group_by(site_no) %>% slice(1)),
  
  # Download NWIS daily data
  tar_target(
    p1_daily_data,
    get_daily_nwis_data(p1_nwis_sites_daily,parameter,stat_cd_select),
    pattern = map(p1_nwis_sites_daily)),
  
  # Subset NWIS sites with instantaneous (sub-daily) data
  tar_target(
    p1_nwis_sites_inst,
    p1_nwis_sites %>%
      filter(data_type_cd=="uv",!(site_no %in% omit_nwis_sites)) %>%
      group_by(site_no) %>% slice(1)),
  
  # Download NWIS instantaneous data
  tar_target(
    p1_inst_data,
    get_inst_nwis_data(p1_nwis_sites_inst,parameter),
    pattern = map(p1_nwis_sites_inst))
  
)  

