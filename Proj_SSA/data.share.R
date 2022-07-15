
## Edit and share data with group

# What to share: based on Data/Panel.RData



# Output:
#  - DI-seasonality/Proj_SSA


## Tools -----------------------------------------------------------------------
source("General.R")
library(tidyverse)
library(here)
library(magrittr)



## Loading data ----------------------------------------------------------------
load("Data/Panel.RData") %>% print




## cleaning data for sharing ---------------------------------------------------
Panel

vars_slc <- 
  c("State",
  	"Date_yearmon",
  	"Cal.Year",
  	"Month",
  	"Region",
    "No.Week",
  	"adj.factor"
  	)

	
Panel %<>% 
	select(State, Region, date_yearmon, Cal.Year, Month, Formatted.Date, No.week, adj.factor,
			   Title.2, Title.16, Concurrent,
				 Det.Title.2, Det.Title.16, Det.Concurrent,
				 Allow.Title.2, Allow.Title.16, Allow.Concurrent,
				 uRate_S, uRate_U, unemply_S, unemply_U, emply_S, emply_U, labor_S, labor_U, UIIC,
				 UICC, UIIC_unadj, UICC_unadj, 
				 jobOpen_s, jobOpenRate_s, jobOpen_u, jobOpenRate_u, 
				 emplyMon_construction, emplyMon_gov, emplyMon_leisure, emplyMon_mining, emplyMon_tot, emplyMon_trade
					 )

Panel$State %>% unique

saveRDS(Panel, file = "Proj_SSA/data_MOWL&EconVars.rds")

readr::write_csv(Panel, file = "Proj_SSA/data_MOWL&EconVars.csv")




