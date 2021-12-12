# This script prepares data for the Matlab functions for CiSSA


library(tidyverse)
library(readr)


load("Data/Panel.RData") %>% print

df_DINW <- 
	Panel %>% 
	select(State, year = Cal.Year, month = Month, Formatted.Date, SSDI = Title.2, SSI =  Title.16, Concurrent) %>% 
	filter(State == "NW") 


df_DINW <-
	df_DINW %>% filter(Formatted.Date != "2021-08")



write_csv(df_DINW, file = "CiSSA/data_DINW.csv")

