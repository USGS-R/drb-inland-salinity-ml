#!/bin/bash
#SBATCH --job-name=rstudio              # human-readable label for squeue output
#SBATCH --time=4:00:00                  # maximum time for this job
#SBATCH --output=rstudio_%u.out         # user's output file (password here!)
#SBATCH --partition=cpu                 # which SLURM partition to use
#SBATCH --nodes=1                       # only request one node
#SBATCH --mem=32GB                      # don't settle for less than 32 GB
#SBATCH --mail-type=ALL                 # enable email notifications for job status. Sends to submitting user

################################################################################
#This script is designed to be run with the container name as an sbatch argument. 
#The container needs to be located in /caldera/projects/usgs/water/impd/pump/drb-inland-salinity/containers/
#Example:
# sbatch launch-rstudio-container.slurm mycontainer.sif
################################################################################

###################################  NOTE  #####################################
# This submission script is based on the rocker project's singularity docs as  #
# of 2022-01-18, and should work with rocker/rstudio:4.1.2 or any images       #
# descended from it (e.g. rocker/geospatial).                                  #
#                                                                              #
# https://www.rocker-project.org/use/singularity/                              #
# https://rocker-project.org/use/singularity.html#slurm-job-script             #
#                                                                              #
# RStudio Server is an evolving product, and if new versions break our setup,  #
# then this documentation is a good place to look for what changes may be      #
# necessary.
################################################################################

module load singularity python36

#Container path
#argument 1 is the name of the container to use.
CONTAINER="/caldera/projects/usgs/water/impd/pump/drb-inland-salinity/containers/${1}"

# Create temporary directory to be populated with directories to bind-mount in
# the container where writable file systems are necessary.
# By default the only host file systems mounted within the container are $HOME, /tmp, /proc, /sys, and /dev.
workdir=$(python -c 'import tempfile; print(tempfile.mkdtemp())')

mkdir -p -m 700 ${workdir}/run ${workdir}/tmp ${workdir}/var/lib/rstudio-server
cat > ${workdir}/database.conf <<END
provider=sqlite
directory=/var/lib/rstudio-server
END

# Set OMP_NUM_THREADS to prevent OpenBLAS (and any other OpenMP-enhanced
# libraries used by R) from spawning more threads than the number of processors
# allocated to the job.
#
# Set R_LIBS_USER to a path specific to rocker/rstudio to avoid conflicts with
# personal libraries from any R installation in the host environment

cat > ${workdir}/rsession.sh <<END
#!/bin/sh
export OMP_NUM_THREADS=${SLURM_JOB_CPUS_PER_NODE}
export R_LIBS_USER=${HOME}/R/rocker-rstudio/4.0
exec /usr/lib/rstudio-server/bin/rsession "\${@}"
END

chmod +x ${workdir}/rsession.sh

export SINGULARITY_BIND="${workdir}/run:/run,${workdir}/tmp:/tmp,${workdir}/database.conf:/etc/rstudio/database.conf,${workdir}/rsession.sh:/etc/rstudio/rsession.sh,${workdir}/var/lib/rstudio-server:/var/lib/rstudio-server,${workdir}/singularity_vars:/usr/local/lib/R/etc/Renviron.site"
touch ${workdir}/singularity_vars

# Do not suspend idle sessions.
# Alternative to setting session-timeout-minutes=0 in /etc/rstudio/rsession.conf
# https://github.com/rstudio/rstudio/blob/v1.4.1106/src/cpp/server/ServerSessionManager.cpp#L126
export SINGULARITYENV_RSTUDIO_SESSION_TIMEOUT=0

# Set the local directory as the place for session information. This should make
# command line history more relevant, as it will be restricted to the project
# currently being worked on.
# Based on:
# Pointer here: https://support.rstudio.com/hc/en-us/articles/218730228-Resetting-a-user-s-state-on-RStudio-Workbench-RStudio-Server
# RStudio Workbench admin guide here: https://docs.rstudio.com/ide/server-pro/r_sessions/customizing_session_settings.html
# XDG Base Directory Specification here: https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
export SINGULARITYENV_XDG_DATA_HOME=$(pwd)/.local_${USER}/share

#RStudio login info
export SINGULARITYENV_USER=$(id -un)
export SINGULARITYENV_PASSWORD=$(openssl rand -base64 15)

# get unused socket per https://unix.stackexchange.com/a/132524
# tiny race condition between the python & singularity commands
readonly PORT=$(python -c 'import socket; s=socket.socket(); s.bind(("", 0)); print(s.getsockname()[1]); s.close()')
cat 1>&2 <<END
1. Open a new terminal on your workstation and SSH tunnel using the following command:

   ssh -N -L 8787:${HOSTNAME}:${PORT} ${SINGULARITYENV_USER}@tallgrass.cr.usgs.gov

2. Point your web browser to http://localhost:8787 and log in to RStudio Server using the following credentials:

   user: ${SINGULARITYENV_USER}
   password: ${SINGULARITYENV_PASSWORD}

When done using RStudio Server, terminate the job by:

1. Exit the RStudio Session ("power" button in the top right corner of the RStudio window)
2. Issue the following command on the login node:

      scancel -f ${SLURM_JOB_ID}

The tmp directory ${workdir} will be automatically removed after the job is cancelled.
END

# Bind mount the above specified directories on the host into the Singularity container.
singularity exec ${CONTAINER} \
    bash -c "cp /usr/local/lib/R/etc/Renviron.site.orig /usr/local/lib/R/etc/Renviron.site; \
            echo SINGULARITY_CONTAINER=\$SINGULARITY_CONTAINER >> /usr/local/lib/R/etc/Renviron.site; \
            echo SINGULARITY_BIND=\$SINGULARITY_BIND >> /usr/local/lib/R/etc/Renviron.site; \
            echo SLURM_JOB_CPUS_PER_NODE=\$SLURM_JOB_CPUS_PER_NODE >> /usr/local/lib/R/etc/Renviron.site; \
            rserver --www-port=${PORT} \
                    --server-user=${USER} \
                    --auth-none=0 \
                    --auth-pam-helper-path=pam-helper \
                    --auth-stay-signed-in-days=30 \
                    --auth-timeout-minutes=0 \
                    --rsession-path=/etc/rstudio/rsession.sh"
printf 'rserver exited' 1>&2

# secure cookie key is needed if using multiple nodes
#uuidgen > "$TMPDIR/tmp/rstudio-server/secure-cookie-key"
#chmod 0600 "$TMPDIR/tmp/rstudio-server/secure-cookie-key"
