# Loading UI initial claims data

# Inputs:
#  - FRED monthly non-farm employment by industry and state

# Output:
#  "Data/UI/employment/data_raw_CESemployment.RData"
#  "Data/data_employment_MonInd.RData"

## Note on outputs:
#   - Data for 50 states (no DC) and national total
#   - numbers in thousands
#   - Constructing and mining data do not contain DE and HI 

## Last updated:
#   - download: 2021-12-18
#   - data up to: 2021-11


rm(list = ls())

library(fredr)
library(dplyr)
library(lubridate)
library(zoo)
library(readr)
library(magrittr)
library(stringr)
library(tidyr)



## Loading data ----

fredr_set_key("2275862454ff25d4b5f50436d306736b")

state.abb_dc <- c(state.abb, "DC")

var_names_tot <- c(paste0(state.abb, "NAN"), "PAYNSA")
names(var_names_tot) <- c(state.abb, "AG")

var_names_construction <- c(paste0(setdiff(state.abb, c("MD", "NE")), "CONSN"), 
														"SMU24000002000000001SA", "SMU31000002000000001",
														"CEU2000000001")
names(var_names_construction) <- c(setdiff(state.abb, c("MD", "NE")) , "MD", "NE", "AG")


var_names_leisure <- c(paste0(state.abb, "LEIHN"), "CEU7000000001")
names(var_names_leisure) <- c(state.abb, "AG")


var_names_mining <- c(paste0(setdiff(state.abb, c("MD", "NE")), "NRMNN"), 
											"SMU24000001000000001", "SMU31000001000000001", 
											"CEU1000000001")
names(var_names_mining) <- c(setdiff(state.abb, c("MD", "NE")), "MD", "NE", "AG")


var_names_trade <- c(paste0(state.abb, "TRADN"), "CEU4000000001")
names(var_names_trade) <- c(state.abb, "AG")

var_names_gov <- c(paste0(state.abb, "GOVTN"), "CEU9000000001")
names(var_names_gov) <- c(state.abb, "AG")




get_UIdata <- function(key){
	data_UI <- 
		fredr(
			series_id = key,
			observation_start = as.Date("2000-01-01"),
			observation_end   = as.Date("2021-12-01")
		)
}

#### Run once for each update ----
# data_tot_raw <-
# 	purrr::map_dfr(var_names_tot, get_UIdata, .id = "State") %>%
# 	mutate(VarName = "emplyMon_tot")
# 
# 
# data_construction_raw <-
# 	purrr::map_dfr(var_names_construction[-c(8, 11)], get_UIdata, .id = "State") %>%
# 	mutate(VarName = "emplyMon_construction")
# 
# data_leisure_raw <-
# 	purrr::map_dfr(var_names_leisure, get_UIdata, .id = "State") %>%
# 	mutate(VarName = "emplyMon_leisure")
# 
# data_mining_raw <-
# 	purrr::map_dfr(var_names_mining[-c(8, 11)], get_UIdata, .id = "State") %>%
# 	mutate(VarName = "emplyMon_mining")
# 
# data_trade_raw <-
# 	purrr::map_dfr(var_names_trade, get_UIdata, .id = "State") %>%
# 	mutate(VarName = "emplyMon_trade")
# 
# data_gov_raw <-
# 	purrr::map_dfr(var_names_gov, get_UIdata, .id = "State") %>%
# 	mutate(VarName = "emplyMon_gov")
# 
# save(
#      data_tot_raw,
# 		 data_construction_raw,
# 		 data_leisure_raw,
# 		 data_mining_raw,
# 		 data_trade_raw,
# 		 data_gov_raw,
# 		 file = paste0("Data/employment/", "data_raw_CESemployment.RData"))
####

load(paste0("Data/employment/", "data_raw_CESemployment.RData"))




data_emplyMon <- 
	bind_rows(data_tot_raw,
						data_construction_raw,
						data_leisure_raw,
						data_mining_raw,
						data_trade_raw,
						data_gov_raw) %>% 
	mutate(year  = year(date),
				 month = month(date),
				 # UItype  = str_extract(series_id, "ICLAIMS|CCLAIMS"),
				 #UI = value
				 ) %>% 
	dplyr::select(VarName, State, year, month, value) %>% 
	spread(VarName, value)
#data_emplyMon


save(data_emplyMon, file = "Data/data_emplyMon.RData")







