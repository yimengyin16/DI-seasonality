# This script imports monthly average temperature data by state


# Data source: U.S. Climate Divisions Current Dataset
#  - webpage https://www.ncdc.noaa.gov/monitoring-references/maps/us-climate-divisions.php#grdd
#  - Data directory https://www.ncei.noaa.gov/pub/data/cirs/climdiv/
#  - Readme: state-readme.txt
#  - monthly avg temperature by state: climdiv-tmpcst-v1.0.0-20210907 
#     - data up to 2021-09-07, downloaded on 2021-09-20
#     - format: separated with two spaces with no header
#     - saved in Data/Temperature/

# Output:
#  Data/temperature.RData

# Monthly avg temerature from 1895-2021
# 49 states (Hawaii not included) and national (contiguous 48 states)




# Packages and tools -----------------------------------------------------------

source("General.R")
library(readr)
select <- dplyr::select



# Loading data -----------------------------------------------------------------

df_temperature_raw <- 
  read_delim("Data/Temperature/climdiv-tmpcst-v1.0.0-20210907", delim = "  ", col_names = FALSE)


df_temerature <- 
  df_temperature_raw %>% 
  select(1:13) %>% 
  mutate(
    state_code = str_sub(X1, 1, 3), 
    year       = str_sub(X1, -4, -1) %>% as.numeric,
    X1 = NULL
  ) %>% 
  gather(month, tempF, -state_code, -year) %>% 
  mutate(month = as.numeric(str_extract(month, "\\d+")) - 1,
         tempF = as.numeric(tempF))


# Matching state code with state name ------------------------------------------

# state code and state names
{
df_stateCode <- 
  tribble(
~ state_code, ~state_name, 
 "001", "Alabama",         
 "002", "Arizona",         
 "003", "Arkansas",       
 "004", "California",      
 "005", "Colorado",        
 "006", "Connecticut",     
 "007", "Delaware",        
 "008", "Florida",         
 "009", "Georgia",         
 "010", "Idaho",           
 "011", "Illinois",        
 "012", "Indiana",         
 "013", "Iowa",            
 "014", "Kansas",          
 "015", "Kentucky",        
 "016", "Louisiana",       
 "017", "Maine",           
 "018", "Maryland",        
 "019", "Massachusetts",   
 "020", "Michigan",        
 "021", "Minnesota",       
 "022", "Mississippi",     
 "023", "Missouri",        
 "024", "Montana",         
 "025", "Nebraska",        
 "026", "Nevada",          
 "027", "New Hampshire",   
 "028", "New Jersey",      
 "029", "New Mexico",      
 "030", "New York",
 "031", "North Carolina",
 "032", "North Dakota",
 "033", "Ohio",
 "034", "Oklahoma",
 "035", "Oregon",
 "036", "Pennsylvania",
 "037", "Rhode Island",
 "038", "South Carolina",
 "039", "South Dakota",
 "040", "Tennessee",
 "041", "Texas",
 "042", "Utah",
 "043", "Vermont",
 "044", "Virginia",
 "045", "Washington",
 "046", "West Virginia",
 "047", "Wisconsin",
 "048", "Wyoming",
 "050", "Alaska",  
 "101", "Northeast Region",
 "102", "East North Central Region",  
 "103", "Central Region",  
 "104", "Southeast Region",  
 "105", "West North Central Region",  
 "106", "South Region",  
 "107", "Southwest Region",  
 "108", "Northwest Region",  
 "109", "West Region",
 "110", "National (contiguous 48 States)" 
)
}

#df_stateCode

# check if state names are consistent with built-in names
intersect(state.name, df_stateCode$state_name) %>% length
setdiff(state.name, df_stateCode$state_name) 
setdiff(df_stateCode$state_name, state.name) 
# the data does not include Hawaii 




df_state_abb <- 
  tibble(state_name = c(state.name, "National (contiguous 48 States)"),
         state_abb  = c(state.abb, "US"))

df_stateCode <- 
  df_stateCode %>% 
  left_join(df_state_abb, by = "state_name")


# Merging to temperature data

df_temerature <- 
  df_temerature %>% 
  left_join(df_stateCode, by = "state_code") %>% 
  filter(state_abb %in% df_state_abb$state_abb) %>%  # removing regions
  select(state_abb, state_name, year, month, tempF)

# df_temerature$state_abb %>% unique %>% length


## Saving data
save(df_temerature, file = "Data/temperature.RData")



