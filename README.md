# drb-inland-salinity-ml
This repository contains a pipeline for data gathering, processing, modeling, and visualizing results for machine learning models that predict salinity in inland reaches of the Delaware River Basin (DRB).

# Building the pipeline with S3 storage
To build the pipeline with the S3 storage, you will need to provide AWS credentials. We have been doing this using the [`saml2aws` tool](https://github.com/Versent/saml2aws). See also @amsnyder's [post on this](https://github.com/amsnyder/s3_demo/blob/main/usgs_access.md). Note - when you do `saml2aws login` make sure you choose the `gs-chs-wma-dev` option because that is where the S3 bucket is. With those credentials in the `~/.aws/credentials` file, R should be able to use them to upload/download to the inland salinity bucket.


# References

The study area segment shapefiles are from:
```
Oliver, S.K., Appling, A.A., Atshan, R., Watkins, W.D., Sadler, J., Corson-Dosch, H., Zwart, J.A., and Read, J.S., 2021, Data release: Predicting water temperature in the Delaware River Basin: U.S. Geological Survey data release, https://doi.org/10.5066/P9GD8I7A.
```

# Disclaimer
This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information.
