# -*- coding: utf-8 -*-
"""
Created on Wed Mar  1 10:16:29 2023

@author: jsmith
"""
import numpy as np
import pandas as pd
from river_dl.postproc_utils import fmt_preds_obs
from river_dl.evaluate import calc_metrics

def txt_to_list(file, sep='\n'):
    """
    Converts a sing-column txt file to a Python list.

    Parameters
    ----------
    file : TYPE str 
        DESCRIPTION path to a text file with one column whose rows are converted to list elements
    sep : TYPE str
        DESCRIPTION the separator for each row. Typically new line (default)

    Returns
    -------
    Python list with elements corresponding to the rows of file

    """
    
    f = open(file, 'r')
    r = [line.split(sep)[0] for line in f]
    return r

def check_spatial_segs(segs, sep='\n'):
    """
    Checks if segs is None or a file name. 
    If a file name, it uses txt_to_list to convert to a list and returns the list.

    Parameters
    ----------
    segs : TYPE str
        DESCRIPTION 'None' or the path to a text file that is used in txt_to_list
    sep : TYPE str
        DESCRIPTION the separator for each row. Typically new line (default)

    Returns
    -------
    None or a Python list with elements corresponding to the rows of the segs file

    """
    
    if segs != 'None':
        r = txt_to_list(segs, sep)
    else:
        r = None
    return r

def write_preds_obs(pred_file, obs_file, partition, spatial_idx_name, 
                    time_idx_name, filepath, spatial=False, train_sites=None, 
                    val_sites=None, test_sites=None):
    """
    Joins the predictions and observations, and writes the result to a csv file
    
    Parameters
    ----------
    pred_file : TYPE str
        DESCRIPTION filepath to the predictions file
    obs : TYPE str
        DESCRIPTION filepath to the observations file
    partition : TYPE str
        DESCRIPTION one of 'trn', 'val', or 'tst'
    spatial_idx_name : TYPE str 
        DESCRIPTION name of column that is used for spatial
            index (e.g., 'seg_id_nat')
    time_idx_name : TYPE str
        DESCRIPTION name of column that is used for temporal index
            (usually 'time')
    filepath : TYPE str
        DESCRIPTION path name of the output file
    spatial : TYPE bool
        DESCRIPTION when True, the pred_file is trimmed to the reaches according
        to each of the provided splits
    val_sites : TYPE list 
        sites to exclude from training and test metrics
    test_sites : TYPE list 
        sites to exclude from validation and training metrics
    train_sites : TYPE list 
        sites to exclude from validation and test metrics

    Returns
    -------
    None.
    Writes a file with columns for the time_idx_name, spatial_idx_name, obs, and pred
    """
    var_data = fmt_preds_obs(pred_file, obs_file, 
                             spatial_idx_name, time_idx_name)

    for data_var, data in var_data.items():
        #reset the index so that the time and space indicies are attributes
        data.reset_index(inplace=True)
        
        if spatial:
            # mask out validation and test sites from trn partition
            if train_sites and partition == 'trn':
                # simply use the train sites when specified.
                data = data[data[spatial_idx_name].isin(train_sites)]
            else:
                #check if validation or testing sites are specified
                if val_sites and partition == 'trn':
                    data = data[~data[spatial_idx_name].isin(val_sites)]
                if test_sites and partition == 'trn':
                    data = data[~data[spatial_idx_name].isin(test_sites)]
            # mask out training and test sites from val partition
            if val_sites and partition == 'val':
                data = data[data[spatial_idx_name].isin(val_sites)]
            else:
                if test_sites and partition=='val':
                    data = data[~data[spatial_idx_name].isin(test_sites)]
                if train_sites and partition=='val':
                    data = data[~data[spatial_idx_name].isin(train_sites)]
            # mask out training and validation sites from val partition
            if test_sites and partition == 'tst':
                data = data[data[spatial_idx_name].isin(test_sites)]
            else:
                if train_sites and partition=='tst':
                    data = data[~data[spatial_idx_name].isin(train_sites)]
                if val_sites and partition=='tst':
                    data = data[~data[spatial_idx_name].isin(val_sites)]
            
        data.to_csv(filepath)

def model_metrics(pred_obs_csv, spatial_idx_name, time_idx_name,
                     group_spatially=False, group_temporally=False,
                     time_aggregation=False, site_based=False,
                     outfile=None):
    data = pd.read_csv(pred_obs_csv)
    data[time_idx_name] = pd.to_datetime(data[time_idx_name])
    data.set_index([time_idx_name, spatial_idx_name], inplace=True)
    
    var_metrics_list = []
    
    if not group_spatially and not group_temporally:
        metrics = calc_metrics(data)
        # need to convert to dataframe and transpose so it looks like the
        # others
        metrics = pd.DataFrame(metrics).T
    elif group_spatially and not group_temporally:
        #note: same as data.groupby(level=spatial_idx_name)
        metrics = (data.groupby(pd.Grouper(level=spatial_idx_name))
        .apply(calc_metrics)
        .reset_index()
        )
    elif not group_spatially and group_temporally:
        if time_aggregation:
            #performance metrics computed at the group_temporally timestep
            #for some reason, no `.` calculations are allowed after .mean(),
            #so calc_metrics() is called first.
            if site_based:
                #create a group_temporally timeseries for each observation site
                metrics = calc_metrics(data
                #filter the data to remove nans before computing the sum
                #so that the same days are being summed in the month.
                .dropna()
                .groupby([pd.Grouper(level=time_idx_name, freq=group_temporally),
                         pd.Grouper(level=spatial_idx_name)])
                .mean()
                )
            else:
                #create a group_temporally timeseries using data from all reaches
                data_sum = (data
                .dropna()
                .groupby(pd.Grouper(level=time_idx_name, freq=group_temporally))
                .mean()
                )
                #For some reason, with pd.Grouper the sum is computed as 0
                # on days with no observations. Need to remove these days
                # before calculating metrics. Get the indicies with 0 obs:
                data_count_0 = np.where(data
                #filter the data to remove nans before computing the sum
                #so that the same days are being summed in the month.
                .dropna()
                .groupby(pd.Grouper(level=time_idx_name, freq=group_temporally))
                .count()
                .reset_index()
                .obs == 0
                )[0]
                if len(data_count_0) > 0:
                    data_sum = data_sum.drop(index=data_sum.index[data_count_0])
                metrics = calc_metrics(data_sum)
            metrics = pd.DataFrame(metrics).T
        else:
            if group_temporally != 'M':
                #native timestep performance metrics within the group_temporally groups
                #This method will report one row per group_temporally group
                # examples: year-month-week would be a group when group_temporally is 'W'
                # year would be a group when group_temporally is 'Y'
                metrics = (data
                .groupby(pd.Grouper(level=time_idx_name, freq=group_temporally))
                .apply(calc_metrics)
                .reset_index()
                )
            else:
                #This method reports one row per calendar month (1-12)
                metrics = (data.reset_index()
                .groupby(data.reset_index()[time_idx_name].dt.month)
                .apply(calc_metrics)
                .reset_index()
                )                
    elif group_spatially and group_temporally:
        if time_aggregation:
            #performance metrics for each reach computed at the group_temporally timestep
            data_calc = (data
            .dropna()
            .groupby([pd.Grouper(level=time_idx_name, freq=group_temporally),
                      pd.Grouper(level=spatial_idx_name)])
            .mean()
            )
            #unable to apply any other . functions after .mean().
            metrics = (data_calc.groupby(pd.Grouper(level=spatial_idx_name))
            .apply(calc_metrics)
            .reset_index()
            )
        else:
            if group_temporally != 'M':
                metrics = (data
                .groupby([pd.Grouper(level=time_idx_name, freq=group_temporally),
                          pd.Grouper(level=spatial_idx_name)])
                .apply(calc_metrics)
                .reset_index()
                )
            else:
                metrics = (data.reset_index()
                .groupby([data.reset_index()[time_idx_name].dt.month, spatial_idx_name])
                .apply(calc_metrics)
                .reset_index()
                ) 

    metrics["variable"] = 'mean_value'
    metrics["partition"] = 'tst'
    var_metrics_list.append(metrics)
    var_metrics = pd.concat(var_metrics_list).round(6)
    if outfile:
        var_metrics.to_csv(outfile, header=True, index=False)
    return var_metrics