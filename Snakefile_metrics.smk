import os
import numpy as np

#river-dl
code_dir = config['code_dir']
# if using river_dl installed with pip this is not needed
import sys
sys.path.insert(1, code_dir)

from river_dl.preproc_utils import asRunConfig

#user-defined functions
from utils import *

out_dir = config["out_dir"]

model_types = ['RF', 'RGCN']
train_splits = ['temporal', 'spatial']
model_attrs = ['min_static_dynamic', 'static_dynamic', 'dynamic']
season_splits = ['AMJ', 'JAS', 'OND', 'JFM']
lulc_splits = ['high_urban', 'high_forest']
physio_splits = ['appalachian', 'coastal', 'interior']
metric_types = ['overall', 'month', 'reach', 'month_reach', 'monthly_all_sites', 'monthly_site_based', 'monthly_reach', 'biweekly_all_sites', 'biweekly_site_based', 'biweekly_reach', 'year', 'year_reach', 'yearly_all_sites', 'yearly_site_based', 'yearly_reach']

rule all:
	input:
		#Note: this is used only for RF models, so doesn't use model_types
		expand("{outdir_1}/{split_1}/pred_obs/RF_{attrs_1}/{metric_type_1}_metrics.csv",
				outdir_1 = out_dir,
				split_1 = train_splits,
				attrs_1 = model_attrs,
				metric_type_1 = metric_types
		),
		expand("{outdir_2}/{split_2}/pred_obs/{model_type_2}_{attrs_2}/seasonal/{seasons_2}/{lulcs_physios_2}/{metric_type_2}_metrics.csv",
				outdir_2 = out_dir,
				split_2 = train_splits,
				model_type_2 = model_types,
				attrs_2 = model_attrs,
				seasons_2 = season_splits,
				lulcs_physios_2 = physio_splits + lulc_splits,
				metric_type_2 = metric_types
		),
		expand("{outdir_3}/{split_3}/pred_obs/{model_type_3}_{attrs_3}/lulc/{lulcs_3}/{metric_type_3}_metrics.csv",
				outdir_3 = out_dir,
				split_3 = train_splits,
				model_type_3 = model_types,
				attrs_3 = model_attrs,
				lulcs_3 = lulc_splits,
				metric_type_3 = metric_types
		),
		expand("{outdir_4}/{split_4}/pred_obs/{model_type_4}_{attrs_4}/physio/{physios_4}/{metric_type_4}_metrics.csv",
				outdir_4 = out_dir,
				split_4 = train_splits,
				model_type_4 = model_types,
				attrs_4 = model_attrs,
				physios_4 = physio_splits,
				metric_type_4 = metric_types
		),
		expand("{outdir_5}/{split_5}/pred_obs/{model_type_5}_{attrs_5}/seasonal/{seasons_5}/{metric_type_5}_metrics.csv",
				outdir_5 = out_dir,
				split_5 = train_splits,
				model_type_5 = model_types,
				attrs_5 = model_attrs,
				seasons_5 = season_splits,
				metric_type_5 = metric_types
		),
		expand("{outdir}/metrics_asRunConfig.yml",  outdir = out_dir),
		expand("{outdir}/Snakefile_metrics", outdir = out_dir),

#save the as-run config settings to a text file
rule as_run_config:
	group: "prep"
	output:
		"{outdir}/metrics_asRunConfig.yml"
	run:
		asRunConfig(config, code_dir, output[0])

#save the as-run snakefile to the output
rule copy_snakefile:
	group: "prep"
	output:
		"{outdir}/Snakefile_metrics"
	shell:
		"""
		scp Snakefile_insal_rgcn_pytorch.smk {output[0]}
		"""

#Order in the list is: 
# spatial (bool), temporal (False or timestep to use), time_aggregation (bool), site_based (bool)
def get_grp_arg(wildcards):
	if wildcards.metric_type == 'overall':
		return [False, False, False, False]
	elif wildcards.metric_type == 'month':
		return [False, 'M', False, False]
	elif wildcards.metric_type == 'reach':
		return [True, False, False, False]
	elif wildcards.metric_type == 'month_reach':
		return [True, 'M', False, False]
	elif wildcards.metric_type == 'monthly_site_based':
		return [False, 'M', True, True]
	elif wildcards.metric_type == 'monthly_all_sites':
		return [False, 'M', True, False]
	elif wildcards.metric_type == 'monthly_reach':
		return [True, 'M', True, False]
	elif wildcards.metric_type == 'year':
		return [False, 'Y', False, False]
	elif wildcards.metric_type == 'year_reach':
		return [True, 'Y', False, False]
	elif wildcards.metric_type == 'yearly_site_based':
		return [False, 'Y', True, True]
	elif wildcards.metric_type == 'yearly_all_sites':
		return [False, 'Y', True, False]
	elif wildcards.metric_type == 'yearly_reach':
		return [True, 'Y', True, False]
	elif wildcards.metric_type == 'biweekly_site_based':
		return [False, '2W', True, True]
	elif wildcards.metric_type == 'biweekly_all_sites':
		return [False, '2W', True, False]
	elif wildcards.metric_type == 'biweekly_reach':
		return [True, '2W', True, False]

#RF model
#compute performance metrics
rule write_preds_obs_1:
	group: 'train_predict_evaluate'
	input:
		expand("{outdir_1}/{split_1}/pred_obs/RF_{attrs_1}/pred_obs.txt",
				outdir_1 = out_dir,
				split_1 = train_splits,
				attrs_1 = model_attrs
		)
	output:
		expand("{outdir_1}/{split_1}/pred_obs/RF_{attrs_1}/{{metric_type, [^\\\\]+}}_metrics.csv",
				outdir_1 = out_dir,
				split_1 = train_splits,
				attrs_1 = model_attrs
		)
	params:
		grp_arg = get_grp_arg
	run:
		for i in range(len(input)):
			model_metrics(pred_obs_csv = input[i], 
						 spatial_idx_name = "PRMS_segid", 
						 time_idx_name = "Date",
						 group_spatially = params.grp_arg[0],
						 group_temporally = params.grp_arg[1],
						 time_aggregation = params.grp_arg[2],
						 site_based = params.grp_arg[3], 
						 outfile = output[i])

rule write_preds_obs_2:
	group: 'train_predict_evaluate'
	input:
		expand("{outdir_2}/{split_2}/pred_obs/{model_type_2}_{attrs_2}/seasonal/{seasons_2}/{lulcs_physios_2}/pred_obs.txt",
				outdir_2 = out_dir,
				split_2 = train_splits,
				model_type_2 = model_types,
				attrs_2 = model_attrs,
				seasons_2 = season_splits,
				lulcs_physios_2 = physio_splits + lulc_splits
		)
	output:
		expand("{outdir_2}/{split_2}/pred_obs/{model_type_2}_{attrs_2}/seasonal/{seasons_2}/{lulcs_physios_2}/{{metric_type, [^\\\\]+}}_metrics.csv",
				outdir_2 = out_dir,
				split_2 = train_splits,
				model_type_2 = model_types,
				attrs_2 = model_attrs,
				seasons_2 = season_splits,
				lulcs_physios_2 = physio_splits + lulc_splits
		)
	params:
		grp_arg = get_grp_arg
	run:
		for i in range(len(input)):
			model_metrics(pred_obs_csv = input[i], 
						 spatial_idx_name = "PRMS_segid", 
						 time_idx_name = "Date",
						 group_spatially = params.grp_arg[0],
						 group_temporally = params.grp_arg[1],
						 time_aggregation = params.grp_arg[2],
						 site_based = params.grp_arg[3], 
						 outfile = output[i])

rule write_preds_obs_3:
	group: 'train_predict_evaluate'
	input:
		expand("{outdir_3}/{split_3}/pred_obs/{model_type_3}_{attrs_3}/lulc/{lulcs_3}/pred_obs.txt",
				outdir_3 = out_dir,
				split_3 = train_splits,
				model_type_3 = model_types,
				attrs_3 = model_attrs,
				lulcs_3 = lulc_splits
		)
	output:
		expand("{outdir_3}/{split_3}/pred_obs/{model_type_3}_{attrs_3}/lulc/{lulcs_3}/{{metric_type, [^\\\\]+}}_metrics.csv",
				outdir_3 = out_dir,
				split_3 = train_splits,
				model_type_3 = model_types,
				attrs_3 = model_attrs,
				lulcs_3 = lulc_splits
		)
	params:
		grp_arg = get_grp_arg
	run:
		for i in range(len(input)):
			model_metrics(pred_obs_csv = input[i], 
						 spatial_idx_name = "PRMS_segid", 
						 time_idx_name = "Date",
						 group_spatially = params.grp_arg[0],
						 group_temporally = params.grp_arg[1],
						 time_aggregation = params.grp_arg[2],
						 site_based = params.grp_arg[3], 
						 outfile = output[i])

rule write_preds_obs_4:
	group: 'train_predict_evaluate'
	input:
		expand("{outdir_4}/{split_4}/pred_obs/{model_type_4}_{attrs_4}/physio/{physios_4}/pred_obs.txt",
				outdir_4 = out_dir,
				split_4 = train_splits,
				model_type_4 = model_types,
				attrs_4 = model_attrs,
				physios_4 = physio_splits
		)
	output:
		expand("{outdir_4}/{split_4}/pred_obs/{model_type_4}_{attrs_4}/physio/{physios_4}/{{metric_type, [^\\\\]+}}_metrics.csv",
				outdir_4 = out_dir,
				split_4 = train_splits,
				model_type_4 = model_types,
				attrs_4 = model_attrs,
				physios_4 = physio_splits
		)
	params:
		grp_arg = get_grp_arg
	run:
		for i in range(len(input)):
			model_metrics(pred_obs_csv = input[i], 
						 spatial_idx_name = "PRMS_segid", 
						 time_idx_name = "Date",
						 group_spatially = params.grp_arg[0],
						 group_temporally = params.grp_arg[1],
						 time_aggregation = params.grp_arg[2],
						 site_based = params.grp_arg[3], 
						 outfile = output[i])

rule write_preds_obs_5:
	group: 'train_predict_evaluate'
	input:
		expand("{outdir_5}/{split_5}/pred_obs/{model_type_5}_{attrs_5}/seasonal/{seasons_5}/pred_obs.txt",
				outdir_5 = out_dir,
				split_5 = train_splits,
				model_type_5 = model_types,
				attrs_5 = model_attrs,
				seasons_5 = season_splits
		)
	output:
		expand("{outdir_5}/{split_5}/pred_obs/{model_type_5}_{attrs_5}/seasonal/{seasons_5}/{{metric_type, [^\\\\]+}}_metrics.csv",
				outdir_5 = out_dir,
				split_5 = train_splits,
				model_type_5 = model_types,
				attrs_5 = model_attrs,
				seasons_5 = season_splits
		)
	params:
		grp_arg = get_grp_arg
	run:
		for i in range(len(input)):
			model_metrics(pred_obs_csv = input[i], 
						 spatial_idx_name = "PRMS_segid", 
						 time_idx_name = "Date",
						 group_spatially = params.grp_arg[0],
						 group_temporally = params.grp_arg[1],
						 time_aggregation = params.grp_arg[2],
						 site_based = params.grp_arg[3], 
						 outfile = output[i])