#!/bin/bash
#SBATCH --job-name=insal_mk             # human-readable label for squeue output
#SBATCH --time=8:00:00                  # maximum time for this job
#SBATCH --output=insal_mk_%u.out        # user's output file (password here!)
#SBATCH --partition=cpu                 # which SLURM partition to use
#SBATCH --nodes=1                       # only request one node
#SBATCH --ntasks-per-node=8             # request 8 cores
#SBATCH --mail-type=ALL                 # enable email notifications for job status. Sends to submitting user

#################################################################################
#This script is designed to be run with the container name as an sbatch argument. 
#The container needs to be located in /caldera/projects/usgs/water/impd/pump/drb-inland-salinity/containers/
#Example:
# sbatch run-tar_make_cmq.slurm mycontainer.sif
#################################################################################

module load singularity

#Container path
#argument 1 is the name of the container to use.
CONTAINER="/caldera/projects/usgs/water/impd/pump/drb-inland-salinity/containers/${1}"

# Run the tar_make_clustermq() command with the specified number of clusters
singularity exec ${CONTAINER} Rscript './slurm/tar_make_cmq.R' "$SLURM_JOB_CPUS_PER_NODE"
