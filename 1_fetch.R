source("1_fetch/src/Identify_SpC_sites_nwis.R")

p1_targets_list <- list(
  # Load harmonized WQP data product
  tar_target(
    p1_wqp_data,
    readRDS(file = "1_fetch/in/DRB.WQdata.rds")),
  tar_target(
    p1_SpC_sites,
    get_drb_SpC_sites(drb_huc8s,SpC_pcodes,site_tp_select)),
  tar_target(
    p1_nwis_daily_SpC_data_ls,
    {
      # Filter SpC sites for sites with daily means
      daily_sites <- filter(p1_SpC_sites,data_type_cd=="dv",stat_cd=="00003")
      # For each site, download daily SpC means (daily data are all reported with parameterCd="00095"; mean statistic denoted by "00003")
      lapply(unique(daily_sites$site_no),function(x)
        readNWISdv(siteNumbers = x,parameterCd="00095",statCd="00003",startDate = "",endDate = ""))
    }
  )
)  

