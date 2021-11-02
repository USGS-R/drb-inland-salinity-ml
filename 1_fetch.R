#source("1_fetch/src/XX")

p1_targets_list <- list(
  # Load harmonized WQP data product
  tar_target(
    p1_wqp_data,
    readRDS(file = "1_fetch/in/DRB.WQdata.rds"))
)  

