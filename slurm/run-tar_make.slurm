#!/bin/bash
#SBATCH --job-name=insal_mk             # human-readable label for squeue output
#SBATCH --time=8:00:00                  # maximum time for this job
#SBATCH --output=insal_mk_%u.out        # user's output file (password here!)
#SBATCH --partition=cpu                 # which SLURM partition to use
#SBATCH --nodes=1                       # only request one node
#SBATCH --ntasks-per-node=1             # number of cores
#SBATCH --mail-type=ALL                 # enable email notifications for job status. Sends to submitting user

####################
#This script is designed to be run with the container name as an sbatch argument. 
#The container needs to be located in /caldera/projects/usgs/water/impd/pump/drb-inland-salinity/containers/
#Example:
# sbatch run-tar_make.slurm mycontainer.sif
####################

module load singularity

#Container path
#argument 1 is the name of the container to use.
CONTAINER="/caldera/projects/usgs/water/impd/pump/drb-inland-salinity/containers/${1}"

# Run the tar_make() command
singularity exec ${CONTAINER} Rscript -e "source('_targets.R'); tar_make()"
