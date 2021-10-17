# This script imports employment data by industry and state from BEA

# Data are imported through BEA API using bea.R package
# Tutorial for bea.R
#   https://us-bea.github.io/econ-visual-guide/access-economic-data-via-the-bea-api.html#get-data
# A good article
#   https://jwrchoi.com/post/how-to-use-bureau-of-economic-analysis-bea-api-in-r/


# Output:
#  Data/emplyIndustry.R


# Last updated: 
#   - download: 2021-10-16
#   - data up to: 2020


# Packages and tools -----------------------------------------------------------
rm(list = ls())
source("General.R")
library(bea.R)
select <- dplyr::select



# Setting up bea.R -------------------------------------------------------------

beaKey <- "D76ED0B1-4CED-4B30-81B4-3B293AAE5217"


## Check the list of dateset names (datesetname) 
#  - https://apps.bea.gov/API/signup/index.cfm
#  - "Regional" is the dataset to use here
# Or using "beaSets"
beaSets(beaKey = beaKey)





## Find parameters for "Regional"
params_Regional <- beaParams(beaKey, "Regional")$Parameter
params_Regional # see ParameterDescription for more details about the parameter

# 1       GeoFips            string:  # state
# 2      LineCode           integer:  # Variable name
# 3     TableName            string:  # SAEMP25N   ("N" for NAICS, from 1998) 
# 4          Year            string

## Check parameter values
# beaParamVals(beaKey, "Regional", "TableName")
# beaParamVals(beaKey, "Regional", "GeoFips")
# beaParamVals(beaKey, "Regional", "LineCode")
# beaParamVals(beaKey, "Regional", "Year")



## Try loading a single variable
#    Note that beaGet can only obtain 1 variable at a time from "Regional" dataset

specs <- list(
  'UserID' = beaKey,
  'Method' = 'GetData',
  'datasetname' = 'Regional',
  'TableName'   = 'SAEMP25N',
  "GeoFips"     = "STATE",    # STATE for all states, US total, and regions
  'LineCode'    = '10',       # Only allows for a single value. 
  'Year'        = 'ALL'       # or selecting years using paste(2000:2019, collapse = ",")
  # 'ResultFormat' = 'json'
)

(df <- 
  beaGet(specs, asWide = FALSE) %>%  # getting long-form data by asWide = FALSE
  as_tibble()
)

rm(df)
rm(specs)
#df$GeoName %in% c(state.name, "District of Columbia") %>% sum


## Loading all variable needed



## Using the BEA interactive data tool to explore and determine variables needed
#   - https://apps.bea.gov/itable/iTable.cfm?ReqID=70&step=1
#   - "ANNUAL PERSONAL INCOME AND EMPLOYMENT BY STATE" - 
#       - "Total Full-Time and Part-Time Employment by Industry (SAEMP25)"

## Var structure in BEA data, by industry
#  10 Total employment 
#    - 70 Farm
#    - 80 Nonfarm
#       - 90 Private nonfarm
#          - 100~1904
#       - 2000 Government and government enterprises
#          - 2001~2012

## Selecting seasonal industries based on Geremew and Gourio (2018), see Figure 4 (p6)
#   - selecting the 5 industries with the highest seasonal factors (seas > 1%)
#   - Industries selected
#      - Construction
#      - Retail trade
#      - Government
#      - Leisure (may include two industries: Arts..., Accommodation and food...)
#      - Mining

# Also select farm employment and forestry and fishing employment 

df_varSelect <- 
  tribble(
    ~LineCode,  ~VarName,     
        10,     "emply_Tot",          # Total employment (number of jobs) 
        70,     "emply_farm",         # Farm 
        100,    "emply_forestry",     # Forestry and fishing  (this is nonfarm)
        200,    "emply_mining",       # mining, quarrying, and oil and gas
        400,    "emply_construction", # Construction
        700,    "emply_retail",       # retail trade
        1700,   'emply_entertain',    # Arts, entertainment, and recreation,
        1800,   'emply_accomm',       # Accommodation and foo
        2000,   "emply_gov"           # Government and government enterprises
)


## create a function that extracts a single variable


get_beaVar <- function(lineCode){
  # get a single var from the SAEMP25N table in Regional dataset
  #lineCode <- 10
  
  specs <- list(
    'UserID' = beaKey,
    'Method' = 'GetData',
    'datasetname' = 'Regional',
    'TableName'   = 'SAEMP25N',
    "GeoFips"     = "STATE",    # STATE for all states, US total, and regions
    'LineCode'    = lineCode,   # Only allows for a single value. 
    'Year'        = 'ALL'       # or selecting years using paste(2000:2019, collapse = ",")
    # 'ResultFormat' = 'json'
  )
  
  beaGet(specs, asWide = FALSE) %>% as_tibble()
  
}

## Retrieve all variables
df_beaData_raw <- purrr::map_dfr(df_varSelect$LineCode, get_beaVar)



## Processing the data

df_stateNames <- tibble(
  State   = c("US", "DC", state.abb), 
  GeoName = c("United States", "District of Columbia", state.name))


df_emplyInd <- 
  df_beaData_raw %>% 
  separate(Code, into = c("tableName", "LineCode"), sep = "-", convert = TRUE) %>% 
  select(LineCode, GeoName, year = TimePeriod, value = DataValue) %>% 
  left_join(df_varSelect, by = "LineCode") %>% 
  left_join(df_stateNames, by = "GeoName") %>% 
  filter(GeoName %in% df_stateNames$GeoName) %>% # remove non-state(or DC/US) regions
  select(State, year, VarName, value) %>% 
  spread(VarName, value)


# df_emplyInd$State %>% unique %>% length

## Saving data
save(df_emplyInd, file = "Data/emplyIndustry.RData")



