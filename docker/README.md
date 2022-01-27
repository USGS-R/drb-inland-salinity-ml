#Instructions for building the Docker container from the Dockerfile and running within the container

##Starting docker daemon within WSL2 or Linux OS##
sudo service docker start
sudo dockerd > /dev/null 2>&1 &


##Building an image from the Dockerfile##
#path can be on Windows (/mnt/c) or WSL Linux filesystem
docker build -t drb-insal-gs:v0.1 <path to Dockerfile>
#Windows
docker build -t drb-insal-gs:v0.1 /mnt/c/users/jsmith/OneDrive\ -\ DOI/Documents/Projects/DRB-InlandSalinity/drb-inland-salinity-ml/docker/
#Linux - I cloned our repo to /home/jsmith/
docker build -t drb-insal-gs:v0.1 /home/jsmith/drb-inland-salinity-ml/docker/


##Running the container and testing the build of our repo##
#-v mounts your files to the server
#--rm removes the container after the session ends
#-e sets your username
#your password will print to the screen
#go to localhost:8787 in a browser to access your Rstudio session
#For git repo stored in WSL or Linux directory
docker run --rm -p 8787:8787 -v <path_to_repo>:/home/<Linux_username>/<working_directory_name> -e USER="<Linux_username>" <container_name:tag>
docker run --rm -p 8787:8787 -v /home/jsmith/drb-inland-salinity-ml/:/home/jsmith/ -e USER="jsmith" drb-insal-gs:v0.1
#For git repo stored in Windows filesystem
docker run --rm -p 8787:8787 -v /mnt/<path_to_repo>:/home/<USGS_username>/<working_directory_name> -e USER="<USGS_username>" <container_name:tag>
docker run --rm -p 8787:8787 -v /mnt/c/Users/jsmith/OneDrive\ -\ DOI/Documents/Projects/DRB-InlandSalinity/drb-inland-salinity-ml/:/home/jsmith -e USER="jsmith" drb-insal-gs:v0.1

##Commands to run on Rstudio server##
#You will start in the '/home/<username>/' directory, which will show your mounted files
#Load the main file and tar_make
source('_targets.R')
tar_make()

##Exit Rstudio and Ctrl+C in Ubuntu to close the session##


##Next Steps:##
- How to run on HPCs. Example [here](https://code.usgs.gov/wwatkins/hpc_container_blog)
- How to run on a Mac
- Set up pushing image to GitLab registry and pull from there instead of building locally
- Set up docker-compose file to build