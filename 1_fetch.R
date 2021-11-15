source("1_fetch/src/get_nwis_sites.R")

p1_targets_list <- list(
  # Load harmonized WQP data product
  tar_target(
    p1_wqp_data,
    readRDS(file = "1_fetch/in/DRB.WQdata.rds")),
  tar_target(
    p1_nwis_SpC_sites,
    {
      dummy <- dummy_date
      get_nwis_sites(drb_huc8s,SpC_pcodes,site_tp_select)
    }
    ),
  tar_target(
    p1_nwis_SpC_daily_data,
    {
      # Filter SpC sites for sites with daily stat codes of interest
      daily_sites <- filter(p1_nwis_SpC_sites,data_type_cd=="dv",stat_cd %in% c("00001","00003"))
      # For each site, download daily SpC means
      lapply(unique(daily_sites$site_no),function(x)
        readNWISdv(siteNumbers = x,parameterCd=SpC_pcodes,statCd=c("00001","00003"),startDate = "",endDate = ""))
    }
  )
)  

