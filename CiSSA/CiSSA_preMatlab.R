# This script prepares data for the Matlab functions for CiSSA


library(tidyverse)
library(readr)


# load("Data/Panel.RData") %>% print
# 
# df_DINW <- 
# 	Panel %>% 
# 	select(State, year = Cal.Year, month = Month, date_yearmon,  SSDI = Title.2, SSI =  Title.16, Concurrent) %>% 
# 	filter(State == "NW") 
# 
# df_DINW_preCovid <- 
#   df_DINW %>% 
# 	filter(date_yearmon <= as.yearmon("2020-02"))

# df_DINW <-
# 	df_DINW %>% filter(Formatted.Date != "2021-08")


## Using data adjusted for TC May.2020 and AO Aug.2021
df_DINW <- readRDS("CiSSA/data_NWno.otl.rds")

df_DINW_preCovid <-
  df_DINW %>%
	filter(date_yearmon <= as.yearmon("2020-02"))



write_csv(df_DINW, file = "CiSSA/data_DINW.csv")
write_csv(df_DINW_preCovid, file = "CiSSA/data_DINW_preCovid.csv")
