# Loading Total nonfarm job opening

# Inputs:
#  - https://fred.stlouisfed.org/series/JTUJOL
# Output:
#  "Data/data_jobOpen.RData"

# NOte: 
#  - national level only
#  - levels in 000s. 

# Var definitions
#  - https://www.bls.gov/news.release/jolts.tn.htm
# The job openings rate is computed by dividing the number of job openings by the sum of 
#   employment and job openings and multiplying that quotient by 100.


library(fredr)
library(dplyr)
library(lubridate)
library(zoo)
library(readr)
library(magrittr)
library(stringr)
library(tidyr)




## Loading Job opening ----


fredr_set_key("2275862454ff25d4b5f50436d306736b")

var_names <- c("JTUJOL", # Job oepening level, seasonally unadjusted
							 "JTUJOR", # Job oepening rate,  seasonally unadjusted
							 "JTSJOL", # Job oepening level, seasonally adjusted
							 "JTSJOR") # Job oepening rate,  seasonally adjusted
	


get_FREDdata <- function(key){
	data_UI <- 
		fredr(
			series_id = key,
			observation_start = as.Date("2000-01-01"),
			observation_end = as.Date("2021-09-01")
		)
}


data_jobOpen <-
	purrr::map_dfr(var_names, get_FREDdata) %>% 
	mutate(year  = year(date),
				 month = month(date)) %>% 
	select(series_id, year, month, value) %>% 
	spread(series_id, value) %>% 
	rename(jobOpen_u = JTUJOL,
				 jobOpen_s = JTSJOL,
				 jobOpenRate_u = JTUJOR,
				 jobOpenRate_s = JTSJOR
				 ) %>% 
	mutate(jobOpenRate_s = jobOpenRate_s/100,
				 jobOpenRate_u = jobOpenRate_u/100)

data_jobOpen


save(data_jobOpen, file = "Data/jobOpen.RData")







