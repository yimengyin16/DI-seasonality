## Overview/Workflow of the code for the DI seasonality paper


## Loading packages and define functions ---- 
#   -  General.R


## Data ----

# Downloading the MOWL data
# source(Data.download.R)

# Loading the processing MOLW data
#  - Data.import.MOWL.R

# Loading and processing BLS (employment) data
#  - Data.import.BLS.R

# Loading and processing BEA (annual employment by industry and state) data
#  - Data.import.BEA.R

# Loading and processing CES data (monthly employment by industry and state)
#  - Data.import.CESemployment.R

# Loading and processing UI data
#  - Data.import.UI.R


# The following two must run together whenever data are updated
# Combine MOWL, employment and UI data
source("Data.Combine.R")
# Detecting and correcting outliers in DI series
source("Data.outliers.R")




## Producing results for the paper ----

#  - Seasonality.Pattern.Rmd (w/ index for figures and tables)
#  - Seasonality.SeasonUnroot.Rmd
#  - Seasonality.Univariate_Granger.Rmd

# - Paper_Seasonality.R. Not used 



