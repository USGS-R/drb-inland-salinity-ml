#Starting with the lastest stable geospatial R image as of 1/20/2022
#Dockerfile references are here:
#https://github.com/rocker-org/rocker-versioned2/tree/master/dockerfiles
#includes Linux Debian OS (stable release)
#includes R version 4.1.2
#includes all packages and Linux libraries in the Dockerfile for verse, 
#which is based on tidyverse, which is based on rstudio, which is based on r-ver.
FROM rocker/geospatial:4.1.2

#If needed, install Linux libraries and remove cache
#you'll know to add libraries if you get an error installing R/Python/etc. packages that you add.
RUN apt-get update && apt-get install -y \
  xdg-utils \
  && rm -rf /var/lib/apt/lists/*
  # Add libraries in alphanumeric order
  # place a space and backslash after the library name (no backslash if it's last in the list).

#add additional R packages to install:
#I'm not worried about specifying versions here because we specify the R subsubversion in FROM
#and packages are downloaded from MRAN for the date that the subsubversion was released. 
RUN install2.r --error \
  dataRetrieval \
  nhdplusTools \
  sbtools \
  scico \
  targets \
  tarchetypes \
  visNetwork 
  # Add packages in alphanumeric order
  # place a space and backslash after the package name (no backslash if it's last in the list).

# Need this if running python in the project directory
# RUN python3.6 -m pip install --upgrade pip
# RUN /rocker_scripts/install_binder.sh

#add additional Python packages to install:
#RUN pip install \
#	dask \
#	jupyterlab \
#	matplotlib \
#	netCDF4 \
#	pandas \
#	pyarrow \
#	scikit-learn \
#	scipy \
#	seaborn \
#	statsmodels \
#	tensorflow==2.5.0 \
#	tensorflow-gpu==2.5.0 \
#	tensorflow-estimator==2.5.0 \
#	torch \
#	xarray \
#	zarr
