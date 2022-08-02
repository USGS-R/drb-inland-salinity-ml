# drb-inland-salinity-ml
This repository contains a pipeline for data gathering, processing, modeling, and visualizing results for machine learning models that predict salinity in inland reaches of the Delaware River Basin (DRB).

# Building the pipeline with S3 (default)
The default behavior is to build the pipeline with S3 as a shared data repository. To build the pipeline with the S3 storage, you will need to provide AWS credentials. We have been doing this using the [`saml2aws` tool](https://github.com/Versent/saml2aws). See also @amsnyder's [post on this](https://github.com/amsnyder/s3_demo/blob/main/usgs_access.md). Note - when you do `saml2aws login` make sure you choose the `gs-chs-wma-dev` option because that is where the S3 bucket is. With those credentials in the `~/.aws/credentials` file, R should be able to use them to upload/download to the inland salinity bucket. If working on Tallgrass, you can generate the credentials locally and then use `scp` to transfer them to tallgrass, for example:

```
scp /home/jsadler/.aws/credentials jsadler@tg-dtn1.cr.usgs.gov:/home/jsadler/.aws/credentials
```

Note: If an error is thrown when fetching a target from S3 (e.g., time out while downloading), that will cause the targets meta file to have an error in it for that target, and the target will be rebuilt the next time `tar_make()` is run. If you do not want that target to rebuild and instead want to try fetching it again, you can `git reset _targets/meta/meta` and then `tar_make()`.

## Access to credentials when using a container
Singularity should be able to access the credentials without any extra steps because it uses the same file system as the host.
Docker, however, does not use the host file system and therefore the credentials file would need to be mounted from the host onto the Docker file system. To do this, you can paste the `.aws/credentials` file into the mounted repository directory and add the following environment variable to the `_targets.R` script:   
`Sys.setenv(AWS_SHARED_CREDENTIALS_FILE = "./credentials")`

# Building locally
Most of the time you shouldn't have to build the pipeline with locally stored targets - you usually will be building it with S3 as the repository (see above). If for some reason local builds are needed for a particular target, you can add `repository = 'local'` to that `tar_target()`'s argument. To build the full pipeline locally, you can change the `repository` option in `tar_objects_set` in the `_targets.R` file. Note: you will still need to log in with aws credentials to build the pipeline because some download targets are set to never rebuild and instead pull the existing targets from S3. WARNING - using a global `repository = 'local'` will trigger a rebuild of all targets (except those that are set to pull from S3). Commiting the targets meta file is not recommended after switching to 'local' because switching back to using `repository = 'aws'` will also trigger a rebuild of the targets that were locally built.


# References

The study area segment shapefiles are from:
```
Oliver, S.K., Appling, A.A., Atshan, R., Watkins, W.D., Sadler, J., Corson-Dosch, H., Zwart, J.A., and Read, J.S., 2021, Data release: Predicting water temperature in the Delaware River Basin: U.S. Geological Survey data release, https://doi.org/10.5066/P9GD8I7A.
```

# Disclaimer
This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information.
