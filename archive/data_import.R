

library(knitr)
library(data.table)
library(gdata) # read.xls
library(plyr)
library(dplyr)
options(dplyr.print_min = 100) # default is 10
options(dplyr.print_max = 100) # default is 20
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread0
library(foreach)
library(doParallel)
library(microbenchmark)
library(readxl)
library(readr)
library(lubridate)
library(stringr)
library(zoo)

library("XLConnect") # slow but convenient because it reads ranges; NOTE: I had to install Java 64-bit on Windows 10 64-bit to load properly
# library(xlsx)
library("btools")
options(dplyr.print_min = 60) # default is 10




#********************************************************************************
##                      Global Settings ####
#********************************************************************************


dir_data <- "Data/"


#********************************************************************************
##                      Importing Data ####
#********************************************************************************


data_raw <- read_csv(paste0(dir_data, "SSA-SA-MOWL-2018-12.csv"), col_names = FALSE) %>% 
	select( 
		 region    = X4,     # region code 
		 state     = X5,     # state code
		 date = X7,     # Date formated
		 Rcpt_tot  = X9,     # Total
		 SSDI      = X14,    # SSDI only
		 SSI       = X19,    # SSI only
		 concurrent= X24     #  concurrent only
		) %>% 
	mutate(date  = mdy(date),
				 year  = year(date),
				 month = month(date)) %>% 
	select(region, state, date, year, month, everything())
	

SSA_Dates <- read_excel(paste0(dir_data, "SSA-Dates1.xls"), col_names = FALSE, skip = 1) %>% 
	select(date = 5,
				 n_SSAweek = 7,
				 notes = 15) %>% 
	mutate(date  = ymd(date),
				 year  = year(date),
				 month = month(date)) %>% 
	select(date, year, month, everything())


data_raw <- left_join(data_raw, SSA_Dates, by = c("date", "year", "month"))


save(data_raw, file = paste0(dir_data, "data_raw.RData"))










