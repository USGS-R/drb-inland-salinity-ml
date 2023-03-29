import os
import numpy as np
import torch
import torch.optim as optim

#river-dl
code_dir = config['code_dir']
# if using river_dl installed with pip this is not needed
import sys
sys.path.insert(1, code_dir)

from river_dl.preproc_utils import asRunConfig
from river_dl.preproc_utils import prep_all_data
from river_dl.torch_utils import train_torch
from river_dl.torch_utils import rmse_masked
from river_dl.evaluate import combined_metrics
from river_dl.torch_models import RGCN_v1
from river_dl.predict import predict_from_io_data

#user-defined functions
from utils import *

out_dir = config['out_dir']

#spatial holdout info
train_segs = check_spatial_segs(config['train_segs_f'])
val_segs = check_spatial_segs(config['val_segs_f'])
test_segs = check_spatial_segs(config['test_segs_f'])

rule all:
	input:
		f"{out_dir}/finetuned_weights.pth",
		f"{out_dir}/finetune_log.csv",
		expand("{outdir}/{metric_type}_metrics.csv",
				outdir=out_dir,
				metric_type=['overall', 'month', 'reach', 'month_reach', 'monthly_all_sites', 'monthly_site_based', 'monthly_reach', 'biweekly_all_sites', 'biweekly_site_based', 'biweekly_reach', 'year', 'year_reach', 'yearly_all_sites', 'yearly_site_based', 'yearly_reach'],
		),
		expand("{outdir}/asRunConfig.yml",  outdir = out_dir),
		expand("{outdir}/Snakefile", outdir = out_dir),
		f"{out_dir}/trn_preds_obs.csv",
		f"{out_dir}/val_preds_obs.csv",
		f"{out_dir}/tst_preds_obs.csv"

#save the as-run config settings to a text file
rule as_run_config:
	group: "prep"
	output:
		"{outdir}/asRunConfig.yml"
	run:
		asRunConfig(config, code_dir, output[0])

#save the as-run snakefile to the output
rule copy_snakefile:
	group: "prep"
	output:
		"{outdir}/Snakefile"
	shell:
		"""
		scp Snakefile_insal_rgcn_pytorch.smk {output[0]}
		"""

rule prep_io_data:
	group: "prep"
	input:
		config['attrs_file'],
		config['obs_file'],
		config['dist_matrix_file']
	output:
		"{outdir}/prepped.npz"
	threads: 3
	run:
		prep_all_data(
				x_data_file = input[0],
				y_data_file = input[1],
				distfile = input[2],
				x_vars = txt_to_list(config['x_vars_file']),
				spatial_idx_name = "PRMS_segid",
				time_idx_name = "Date",
				y_vars_finetune = config['y_vars'],
				train_start_date = txt_to_list(config['train_start_date_f']),
				train_end_date = txt_to_list(config['train_end_date_f']),
				val_start_date = txt_to_list(config['val_start_date_f']),
				val_end_date = txt_to_list(config['val_end_date_f']),
				test_start_date = txt_to_list(config['test_start_date_f']),
				test_end_date = txt_to_list(config['test_end_date_f']),
				val_sites = val_segs,
				test_sites = test_segs,
				explicit_spatial_partition = True,
				earliest_time = config['earliest_date'],
				latest_time = config['latest_date'],
				out_file = output[0],
				trn_offset = config['trn_offset'],
				tst_val_offset = config['tst_val_offset'],
				seq_len = config['seq_len'],
				log_y_vars = config['log_y_vars']
				)


rule finetune_train:
	group: "train"
	input:
		"{outdir}/prepped.npz"
	output:
		"{outdir}/finetuned_weights.pth",
		"{outdir}/finetune_log.csv"
	threads: 3
	run:
		data = np.load(input[0], allow_pickle=True)
		num_segs = len(np.unique(data['ids_trn']))
		adj_mx = data['dist_matrix']
		in_dim = len(data['x_vars'])
		device = torch.device('cuda:0' if torch.cuda.is_available() else 'cpu')
		model = RGCN_v1(input_dim = in_dim, 
						hidden_dim = config['hidden_size'], 
						adj_matrix = adj_mx, 
						recur_dropout = config['recurrent_dropout'], 
						dropout = config['dropout'], 
						device = device, 
						seed = config['seed'])
		opt = optim.Adam(model.parameters(), lr = config['learning_rate'])
		train_torch(model,
					loss_function = rmse_masked,
					optimizer = opt,
					x_train = data['x_trn'],
					y_train = data['y_obs_trn'],
					x_val = data['x_val'],
					y_val = data['y_obs_val'],
					max_epochs = config['epochs'],
					early_stopping_patience = config['early_stopping'],
					batch_size = num_segs,
					weights_file = output[0],
					log_file = output[1],
					device = device)


rule make_predictions:
	input:
		"{outdir}/finetuned_weights.pth",
		"{outdir}/prepped.npz"
	output:
		"{outdir}/{partition}_preds.feather"
	group: "train_predict_evaluate"
	run:
		data = np.load(input[1])
		adj_mx = data['dist_matrix']
		in_dim = len(data['x_vars'])
		device = torch.device('cuda:0' if torch.cuda.is_available() else 'cpu')
		model = RGCN_v1(input_dim = in_dim, 
						hidden_dim = config['hidden_size'], 
						adj_matrix = adj_mx, 
						recur_dropout = config['recurrent_dropout'], 
						dropout = config['dropout'], 
						device = device, 
						seed = config['seed'])
		opt = optim.Adam(model.parameters(), lr = config['learning_rate'])
		model.load_state_dict(torch.load(input[0]))
		predict_from_io_data(model = model, 
							io_data = input[1],
							partition = wildcards.partition,
							outfile = output[0],
							trn_offset = config['trn_offset'],
							tst_val_offset = config['tst_val_offset'],
							spatial_idx_name = "PRMS_segid",
							time_idx_name = "Date",
							log_vars = config['log_y_vars'])


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

#compute performance metrics
rule combine_metrics:
	group: 'train_predict_evaluate'
	input:
		config['obs_file'],
		"{outdir}/trn_preds.feather",
		"{outdir}/val_preds.feather",
		"{outdir}/tst_preds.feather"
	output:
		"{outdir}/{metric_type}_metrics.csv"
	params:
		grp_arg = get_grp_arg
	run:
		combined_metrics(obs_file = input[0],
						pred_trn = input[1],
						pred_val = input[2],
						pred_tst = input[3],
						train_sites = train_segs,
						val_sites = val_segs,
						test_sites = test_segs,
						group_spatially = params.grp_arg[0],
						group_temporally = params.grp_arg[1],
						time_aggregation = params.grp_arg[2],
						site_based = params.grp_arg[3],
						outfile = output[0],
						spatial_idx_name = "PRMS_segid",
						time_idx_name = "Date")

#write prediction+obs files to use in R plot targets
rule write_preds_obs:
	group: 'train_predict_evaluate'
	input:
		config['obs_file'],
		"{outdir}/trn_preds.feather",
		"{outdir}/val_preds.feather",
		"{outdir}/tst_preds.feather"
	output:
		"{outdir}/trn_preds_obs.csv",
		"{outdir}/val_preds_obs.csv",
		"{outdir}/tst_preds_obs.csv"
	run:
		write_preds_obs(pred_file = input[1], 
						obs_file = input[0],
						partition = 'trn',
						spatial_idx_name = "PRMS_segid", 
						time_idx_name = "Date",
						filepath = output[0],
						spatial = config['spatial_write'],
						train_sites = train_segs, 
						val_sites = val_segs,
						test_sites = test_segs)
		write_preds_obs(pred_file = input[2], 
						obs_file = input[0], 
						partition = 'val',
						spatial_idx_name = "PRMS_segid", 
						time_idx_name = "Date",
						filepath = output[1],
						spatial = config['spatial_write'],
						train_sites = train_segs, 
						val_sites = val_segs,
						test_sites = test_segs)
		write_preds_obs(pred_file = input[3],
						obs_file = input[0],
						partition = 'tst',
						spatial_idx_name = "PRMS_segid", 
						time_idx_name = "Date",
						filepath = output[2],
						spatial = config['spatial_write'],
						train_sites = train_segs, 
						val_sites = val_segs,
						test_sites = test_segs)