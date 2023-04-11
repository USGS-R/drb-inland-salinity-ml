# drb-inland-salinity-ml
This repository contains a pipeline for data gathering, processing, modeling, and visualizing results for machine learning models that predict salinity in inland reaches of the Delaware River Basin (DRB). This pipeline is built with targets version 0.11.0 and may not work with earlier versions.

Note: several targets are currently set to never update: NWIS specific conductivity data (described further in the "Building locally" section of this README), and Boruta attribute screening. The attribute screening is not being updated because it determines which features are used in the models.

# New Users and Reviewers
Follow the steps below to build the pipeline with S3 within the singularity container on Tallgrass. For reviews, the code developer should provide the directory of the code to review, the name of the container used, and the slurm script to run the targets or to inspect the generated data.

# Building the pipeline with S3 (default)
The default behavior is to build the pipeline with S3 as a shared data repository. To build the pipeline with the S3 storage, you will need to provide AWS credentials. We have been doing this using the [`saml2aws` tool](https://github.com/Versent/saml2aws). See also @amsnyder's [post on this](https://github.com/amsnyder/s3_demo/blob/main/usgs_access.md). 
- On Tallgrass, you should install the saml2aws tool following @amsnyder's post, but change `'/home/<username>/'` to `'~/saml2aws-files'`. 
- Within the `~/saml2aws-files` directory, add a file called `pass.txt` that contains one line with your AD password. 
- Set the permissions of that file so that only you have access to it `chmod go-rwx ~/saml2aws-files/pass.txt`.
- run `~/saml2aws-files/saml2aws configure` following @amsnyder's post
- With this setup, a credentials file will be automatically generated by the `p1_aws_credentials` target when the pipeline is run on Tallgrass.

Note: If an error is thrown when fetching a target from S3 (e.g., time out while downloading), that will cause the targets meta file to have an error in it for that target, and the target will be rebuilt the next time `tar_make()` is run. If you do not want that target to rebuild and instead want to try fetching it again, you can `git reset _targets/meta/meta` and then `tar_make()`.

## Access to credentials when using a container
Singularity on Tallgrass will generate and access the credentials without any extra steps because it uses the same file system as the host.
Docker, however, does not use the host file system and therefore the credentials file would need to be mounted from the host onto the Docker file system. To do this, you can generate credentials manually with `saml2aws login` and paste the `.aws/credentials` file into the mounted repository directory. Note - when you do `saml2aws login` make sure you choose the `gs-chs-wma-dev` option because that is where the S3 bucket is.
- On your local computer, saml2aws can be installed and configured following @amsnyder's post. You do not need to install saml2aws within Docker.
- To run the pipeline, the `p1_aws_credentials` target will expect `~/saml2aws-files/pass.txt` and `~/saml2aws-files/saml2aws` to exist. In Docker, `~/` corresponds to the mounted repository directory (drb-inland-salinity-ml). Create this directory and add a blank `pass.txt` file and `saml2aws` file to it.
To run the pipeline locally outside of a container (if needed, not recommended), you can follow the same steps as described for Docker. `~/` corresponds to your local file system's `~/` directory.

# Building locally
Most of the time you shouldn't have to build the pipeline with locally stored targets - you usually will be building it with S3 as the repository (see above). If for some reason local builds are needed for a particular target, you can add `repository = 'local'` to that `tar_target()`'s argument. To build the full pipeline locally, change the `repository` option in `tar_objects_set` in the `_targets.R` file and set `NWIS_repository` to `'local'` in `1_fetch.R`. WARNING - using a global `repository = 'local'` will trigger a rebuild of all targets. Alternatively, certain NWIS targets can be set to never rebuild, even when the global `repository = 'local'`. To prevent NWIS download targets from building when `repository = 'local'`, keep `NWIS_repository = 'aws'` in `1_fetch.R` so that these targets are pulled from S3. Note that if these targets are pulled from S3, then you will need to log in with aws credentials to build the pipeline.
Commiting the targets meta file is not recommended after switching to 'local' because switching back to using `repository = 'aws'` will also trigger a rebuild of the targets that were locally built.


# References

The study area segment shapefiles are from:
```
Oliver, S.K., Appling, A.A., Atshan, R., Watkins, W.D., Sadler, J., Corson-Dosch, H., Zwart, J.A., and Read, J.S., 2021, Data release: Predicting water temperature in the Delaware River Basin: U.S. Geological Survey data release, https://doi.org/10.5066/P9GD8I7A.
```
