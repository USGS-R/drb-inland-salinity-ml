#Load the command line arguments
arg = commandArgs(T)

source('_targets.R')

tar_make_clustermq(workers = as.numeric(arg[1]), log_worker = TRUE)