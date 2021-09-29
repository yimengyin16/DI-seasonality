# Explore if the outliers in NY and NJ data are transcriptional errors


#*******************************************************************************
##  Libraries and tools ----
#*******************************************************************************

library(plyr)
library(ggplot2)
library(scales)
library(xtable)
#library(DataCombine) # it loads dplyr!
library(magrittr)
library(tidyr)
library(knitr)
library(lubridate)
library(tsibble)
library(fable)
library(feasts)
library(dplyr)
library(purrr)
library(grid)
library(gridExtra)
library(readxl)
library(zoo)

options(xtable.include.rownames = F,
				xtable.booktabs = T,
				xtable.caption.placement = "top")

#*******************************************************************************
##  Loading data ----
#*******************************************************************************

data_dir <- "Data/MOWL/CheckOutliers/"


# FYWL data
df_FYWL_raw <-
	read_xlsx(paste0(data_dir, "SSA-SA-FYWL.xlsx"), sheet = "SSA-SA-FYWL(2)")

df_FYWL <- 
  df_FYWL_raw %>% 
	select(state = "State Code", year_fy = Date, DI_FY = `Adult Receipts`, DC_FY = "SSI Disabled Child (DC) Receipts" )


df_FYWL <- 
	bind_rows(
		df_FYWL, 
    df_FYWL %>% 
			group_by(year_fy) %>% 
			summarise(DI_FY = sum(DI_FY),
							  DC_FY = sum(DC_FY)) %>% 
			mutate(state = "US")
	)


# MOWL data
df_MOWL_raw <- 
	read_xls(paste0(data_dir, "SSA-SA-MOWL.xls"), sheet = "SSA-SA-MOWL.xls", skip = 7) 

 	
df_MOWL <- 
	df_MOWL_raw %>% 
	select(state = 5, yearmon = 8, DI_MO = "Receipts \n(All Initial)", DC_MO = "Receipts \n(Initial SSI DC Only)") %>% 
	filter(!state %in% c("FE", "EA","EM","EO","EV")) %>% 
	group_by(state) %>% 
	mutate(year_cy = year(as.yearmon(yearmon)),
				 year_fy = lead(year_cy, 3, default = max(year_cy))) %>% 
	relocate(state, year_fy, yearmon, DI_MO, DC_MO)
	

df_MOWL_fy <- 
	df_MOWL %>% 
	group_by(state, year_fy) %>% 
	summarise(DI_FY_sum = sum(DI_MO),
						DC_FY_sum = sum(DC_MO))




df_MOWL_fy <- 
	bind_rows(
	df_MOWL_fy, 
	df_MOWL %>% 
		group_by(year_fy) %>% 
		summarise(DI_FY_sum = sum(DI_MO),
							DC_FY_sum = sum(DC_MO)) %>% 
		mutate(state = "US")
	)

df_MOWL_fy %>% 
	filter(state == "US")

# Joining data
df_WL <- 
	left_join(df_FYWL, df_MOWL_fy)






#*******************************************************************************
##  Comparing results ----
#*******************************************************************************


state_slc <- "NJ"

df_WL %>% 
	filter(state == state_slc) %>% 
	select(state, year_fy, DI_FY, DI_FY_sum) %>% 
	gather(Var, value, -c(1, 2)) %>% 
	mutate(Var = factor(Var, levels = c("DI_FY_sum", "DI_FY"),
											     labels = c("Sum of monthly", "Reported FY total"))) %>% 
	ggplot(aes(x = year_fy, y = value, color = Var)) + 
	geom_line() + 
	geom_point() + 
	coord_cartesian(ylim = c(0, NA)) + 
	scale_x_continuous(breaks = 2000:2021) +
	labs(title = state_slc) + 
	geom_vline(xintercept = 2001, linetype = 2)


df_WL %>% 
	filter(state == state_slc) %>% 
	select(state, year_fy, DC_FY, DC_FY_sum) %>% 
	gather(Var, value, -c(1, 2)) %>% 
	ggplot(aes(x = year_fy, y = value, color = Var)) + 
	geom_line() + 
	geom_point() + 
	coord_cartesian(ylim = c(0, NA)) + 
	scale_x_continuous(breaks = 2000:2021 )





state_slc <- "NY"

df_WL %>% 
	filter(state == state_slc) %>% 
	select(state, year_fy, DI_FY, DI_FY_sum) %>% 
	gather(Var, value, -c(1, 2)) %>% 
	mutate(Var = factor(Var, levels = c("DI_FY_sum", "DI_FY"),
											labels = c("Sum of monthly", "Reported FY total"))) %>% 
	ggplot(aes(x = year_fy, y = value, color = Var)) + 
	geom_line() + 
	geom_point() + 
	coord_cartesian(ylim = c(0, NA)) + 
	scale_x_continuous(breaks = 2000:2021) +
	labs(title = state_slc) + 
	geom_vline(xintercept = 2002, linetype = 2)




state_slc <- "US"

df_WL %>% 
	filter(state == state_slc) %>% 
	select(state, year_fy, DI_FY, DI_FY_sum) %>% 
	gather(Var, value, -c(1, 2)) %>% 
	mutate(Var = factor(Var, levels = c("DI_FY_sum", "DI_FY"),
											labels = c("Sum of monthly", "Reported FY total"))) %>% 
	ggplot(aes(x = year_fy, y = value, color = Var)) + 
	geom_line() + 
	geom_point() + 
	coord_cartesian(ylim = c(0, NA)) + 
	scale_x_continuous(breaks = 2000:2021) +
	labs(title = state_slc) + 
	geom_vline(xintercept = 2002, linetype = 2)


