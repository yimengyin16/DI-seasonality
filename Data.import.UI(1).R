# Loading UI initial claims data

# Inputs:
#  - Data/MOWL/SSA_DATES1-2021.csv
#  - FRED initial and continued UI claims data
# Output:
#  "Data/UI/data_raw_UIclaims.RData"
#  "Data/data_UI_SSA.RData"


## Last updated:
#  - download:   2021-10-16
#  - data up to: 2021-12-11

rm(list = ls())
library(fredr)
library(dplyr)
library(lubridate)
library(zoo)
library(readr)
library(magrittr)
library(stringr)
library(tidyr)




## Loading Initial claims data ----

## Note:
#   Dates are Saturdays. 
#   All series are non-seasonally adjusted.


fredr_set_key("2275862454ff25d4b5f50436d306736b")

var_names_IC <- c(paste0(state.abb, "ICLAIMS"), "ICNSA")
names(var_names_IC) <- c(state.abb, "US")

var_names_CC <- c(paste0(state.abb, "CCLAIMS"), "CCNSA")
names(var_names_CC) <- c(state.abb, "US")


get_UIdata <- function(key){
	data_UI <- 
		fredr(
			series_id = key,
			observation_start = as.Date("2000-01-01"),
			observation_end = as.Date("2021-12-18")
		)
}

## run once for each data update
# data_UI_IC_raw <-
# 	purrr::map_dfr(var_names_IC, get_UIdata, .id = "state") %>%
# 	mutate(UItype = "IC")
# 
# data_UI_CC_raw <-
# 	purrr::map_dfr(var_names_CC, get_UIdata, .id = "state") %>%
# 	mutate(UItype = "CC")
# 
# save(data_UI_IC_raw,
# 		 data_UI_CC_raw,
# 		 file = paste0("Data/UI/", "data_raw_UIclaims.RData"))


load(paste0("Data/UI/", "data_raw_UIclaims.RData"))

data_UI <- 
	bind_rows(data_UI_IC_raw,
						data_UI_CC_raw) %>% 
	mutate(year  = year(date),
				 month = month(date),
				 # UItype  = str_extract(series_id, "ICLAIMS|CCLAIMS"),
				 UI = value) %>% 
	select(UItype, state, date, year, month, UI) 
# data_UI

# data_UI %>% filter(year == 2004, month == 12)


# data_UI %>% filter(state == "US") %>% select(date, year, value = UI) %>%
# 	filter(year >= 2017) %>% 
# 	ggplot2::qplot(x = date, y = value, data=., geom = "line") 


## Loading SSA dates----

dir_data <- "Data/MOWL/"

SSA_Dates <- 
	read_csv(paste0(dir_data, "SSA_DATES1-2021.csv"), col_names = TRUE) %>% 
	select(date_Fri = 5,
				 month_SSA = 6,
				 n_SSAweek = 7,
				 notes = 15) %>% 
	mutate(date_Fri  = ymd(date_Fri),
				 year  = year(date_Fri),
				 month = month(date_Fri)) %>% 
	select(date_Fri, year, month, month_SSA,  everything())

# SSA_Dates 

# SSA_Dates %>% filter(year == 2004, month == 12)



## Special treatment of 3 months:

# 2005-9
#  Change number of weeks from 4 to 5
#  Note: 4 weeks are used in the model for MOWL 

# 2011-9
#  Change number of weeks from 4 to 5
#  Note: 5 weeks are used in the model for MOWL 

# 2016-9
#  Change number of weeks from 4 to 5
#  Note: 5 weeks are used in the model for MOWL 


SSA_Dates %<>% 
	mutate(n_SSAweek = ifelse(year == 2005 & month == 9, 5, n_SSAweek),
				 n_SSAweek = ifelse(year == 2011 & month == 9, 5, n_SSAweek),
				 n_SSAweek = ifelse(year == 2016 & month == 9, 5, n_SSAweek))



## Creating initial UI claims in SSA month

# Obtaining the date for Saturdays for UI weeks

data_UI %<>% 
	mutate(date_Fri = date - 1) %>% 
	left_join(select(SSA_Dates, date_Fri, n_SSAweek,  yearmon_SSA = month_SSA), by = "date_Fri") %>% 
	# filter(year == 2005, month == 1)
	filter(date_Fri >= as.Date("2000-10-6")) %>% 
	mutate(yearmon_SSA = as.yearmon(yearmon_SSA), 
		     year_SSA = year(yearmon_SSA),
				 month_SSA = month(yearmon_SSA)
		     #year_SSA = year(date_Fri),
				 #month_SSA = month(date_Fri),
				 # yearmon_SSA = paste0(year_SSA, "-", month_SSA)
				 )
# data_UI

# data_UI %>% filter(year == 2004, month == 12)
# data_UI %>% filter(year == 2005, month == 1)

data_UI %>% 
	filter(is.na(UItype))



avg_nWeek <- 1461/336 # (number of weeks / number of months in a 28-year cycle: 4.38)  

data_UI_SSA <- 
  data_UI %>% 
	group_by(UItype, state, yearmon_SSA) %>% 
	summarise(year  = unique(year_SSA),
						month = unique(month_SSA),
						n_SSAweek = unique(n_SSAweek),
		        UI = sum(UI),
						.groups = "drop") %>% 
	spread(UItype, UI) %>% 
	rename(UIIC = IC,
				 UICC = CC) %>% 
	
	mutate(UIIC_unadj = UIIC,
				 UIIC       = avg_nWeek / n_SSAweek * UIIC,
				 UICC_unadj = UICC,
				 UICC       = avg_nWeek / n_SSAweek * UICC) %>% 
	arrange(state, year, month)
# data_UI_SSA

#data_UI_SSA %>% filter(year == 2004, month == 12)
# data_UI_SSA %>% filter(year == 2005, month == 1)


data_UI_NW <- 
  data_UI_SSA %>% 
	filter(state == "US")

data_UI_SSA <- 
	bind_rows(data_UI_SSA,
						mutate(data_UI_NW, state = "NW"),
		        mutate(data_UI_NW, state = "AG"),
						mutate(data_UI_NW, state = "A8"),
						mutate(data_UI_NW, state = "A9")
	)

	
# Notes on data_UI_SSA:
#  - UItype: 
#      - ICLAIMS for initial claims
#      - CCLAIMS for continued claims
#      - all are non-seasonally adjusted
#  - year, month, yearmon_SSA all in SSA working month (by date of Friday)
#  - UI: UI in SSA working month, adjusted for number of weeks in SSA working month
#        (average weekly number * 4.35)
#  - UI_unadj: Total UI in SSA working month. (simple sum of weekly numbers in an SSA month)

#  - US total coded as "US"


save(data_UI_SSA, file = "Data/data_UI_SSA.RData")





