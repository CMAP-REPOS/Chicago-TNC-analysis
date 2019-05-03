# Chicago-TNC-analysis
Initial analysis of TNC trip data posted by the city of Chicago in April 2019

This R script generates the charts associated with [CMAP's TNC policy update.](https://www.cmap.illinois.gov/updates/all/-/asset_publisher/UIMfSLnFfMB6/content/new-data-allows-an-initial-look-at-ride-hailing-in-chicago)

##
Note that this script is currently set up to download all available data from the [Chicago Data Portal.](https://data.cityofchicago.org/) At time of publishing this is just over 17 million records, but this should increase as additional data is added by the city. Editing the data request from the city to just download the data you need will likely speed up your analysis. 

The city has posted three files related to Transportation Network Companies (TNC) or as the City refers to them, Transportation Networking Providers (TNP). This analysis focuses on the [‘Trips’]( https://data.cityofchicago.org/Transportation/Transportation-Network-Providers-Trips-Dashboard/pvbr-dkbf) file. ‘Driver’ and ‘Vehicle’ files are also available.
