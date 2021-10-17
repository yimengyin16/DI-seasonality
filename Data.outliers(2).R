

## Notes
# - Outliers of applications in 2004:6 and 2014:11 are adjusted in Data.import.MOWL.R
# - This script 
#     - detect and adjust outliers in DI application and allowance series
#     - MI SSI 2004:5 and 2004:6 are adjusted properly by "tsoutlier", they are mannually adjusted

## Regions adjusted
#  - 50 states + DC (5 agencies are not adjusted) 
#  - AG: sum of adjusted values of 50 states + DC
#  - NW: sum of adjusted values of 50 states + DC + unadjusted values for 5 agencies)
#
## Variables adjusted:
#  - number of apps
#  - number of determination
#  - number of allowance
#
## varibles updated:
#  - allowance rates and log of applications are updated.



## Using method described in 
# https://robjhyndman.com/hyndsight/tsoutliers/


#*******************************************************************************
##  Libraries and tools ----
#*******************************************************************************

library(plyr)
library(ggplot2)
library(scales)
library(xtable)
#library(DataCombine) # it loads dplyr!
library(magrittr)
library("tidyr")
library("knitr")
library(lubridate)
library(tsibble)
library(fable)
library(feasts)
library(dplyr)
library(purrr)
library(grid)
library(gridExtra)

options(xtable.include.rownames = F,
				xtable.booktabs = T,
				xtable.caption.placement = "top")

source("General.R")
# source("Forecasting.main.R")

select <- dplyr::select

#*******************************************************************************
##  Loading data ----
#*******************************************************************************

load("Data/Panel.RData")

plot.path = "paper_plot/"
dir.fig_newVer = "paper_plot/fig_newVer/"


Panel <- 
	Panel %>% 
	mutate(date_yearmon =  as.yearmon(Formatted.Date))


## Keep the original data without adjustments to outliers
Panel_unadj <- Panel



#*******************************************************************************
##  Detecting and correcting outliers: explore ----
#*******************************************************************************


## Note
# For applications series: 
#  - Only for series adjusted for SSA weeks: Title.2, Title.16, Concurrent
#  - First adjust for each state
#  - Then construct the national aggregate series "AG" again. (50 states + DC) 

# Do the same for determination and allowance series, 
# then calculate the allowance rates. 

# Check if COVID will be detected as outlier 

# df <- 
# Panel %>% 
# 	# filter(State=="NY") %>% 
# 	select(State, index, date_yearmon, 
# 				 Title.2, Title.16, Concurrent, 
# 				 Det.Title.2, Det.Title.16, Det.Concurrent, 
# 				 Allow.Title.2, Allow.Title.16, Allow.Concurrent
# 				 )

# v1 <- "MI"
# v2 <- "Title.16"
# 
# Panel %>% filter(State == v1) %>% pull(v2) 
# Panel %>% filter(State == v1) %>% pull(v2) %>% tsclean()
# Panel %>% filter(State == v1) %>% pull(v2) %>% tsoutliers()
# 
# 
# 
# 
# df <- 
# 	gather(df, Var, value, -(1:3))
# 
# 
# 
# state_slc <- "NY"
# vn <- "Title.2"
# 
# df %>% 
# 	filter(State == state_slc)
# 
# Outliers <- 
#   df %>% 
# 	filter(State == state_slc) %>%
# 	pull(vn) %>% 
# 	forecast::tsoutliers()
# 	
# 
# 
# var_slc <- c(
# 						 "Title.2", "Title.16", "Concurrent", 
#   					 "Det.Title.2", "Det.Title.16", "Det.Concurrent", 
#   					 "Allow.Title.2", "Allow.Title.16", "Allow.Concurrent",
# 						 "unemply_U", "uRate_U", "labor",
# 						 "UIIC", "UICC"
# 						 )
# 
# Panel %>% names
# 
# df <- 
#   Panel %>% 
# 	select(1:8, date_yearmon, any_of(var_slc)) %>% 
# 	gather(var, value, -(1:9)) %>% 
# 	mutate(value = as.numeric(value)) %>% 
# 	as_tibble()
# 
# 
# df %<>% 
# 	group_by(State, var) %>% 
# 	mutate(value_adjOut = tsclean(value))
# 
# 
# 
# df_out <- 
# 	df %>% 
# 	filter(value != value_adjOut)
# 
# 
# df_out1 <- 
# df_out %>% 
# 	filter(Cal.Year < 2019)


#*******************************************************************************
##  Detecting and correcting outliers ----
#*******************************************************************************


## Note
# For applications series: 
#  - Only for series adjusted for SSA weeks: Title.2, Title.16, Concurrent
#  - Detect outliers using the entire series, but only repace detected outliers before 
#    Covid (2020-2 and before)
#  - First adjust for each state
#  - Then construct the national aggregate series "AG" again. (50 states + DC) 

# Do the same for determination and allowance series, 
# then calculate the allowance rates. 

# Check if COVID will be detected as outlier 



vars_adj <- c(
	"Title.2", "Title.16", "Concurrent", 
	"Det.Title.2", "Det.Title.16", "Det.Concurrent", 
	"Allow.Title.2", "Allow.Title.16", "Allow.Concurrent"
)



## Selecting variables to be adjusted for outliers
df_adj <- 
	Panel %>% 
	select(State, date_yearmon, all_of(vars_adj)) %>% 
	gather(Var, value, -State, -date_yearmon) %>% 
	group_by(State, Var) %>% 
	mutate(value = as.numeric(value))

## Create series with outliers adjusted
df_adj <- 
	df_adj %>% 
  mutate(value_adjOut = tsclean(value))


## Check the number of outliers
# df_outOnly  <-
# 	df_adj %>%
# 	filter(value != value_adjOut)
# 
# df_outOnly %>%
# 	filter(Var == "Title.2",
# 				 date_yearmon <= as.yearmon("2020-02") # zoo
# 				 )
# 
# df_outOnly_N <-
#   df_outOnly %>%
# 	filter(date_yearmon <= as.yearmon("2020-02")) %>%  # zoo
# 	summarize(N = n()) %>%
# 	arrange(Var, State)
 	


## Adjusting outliers
df_adj <-
  df_adj %>%
	mutate(value = ifelse(date_yearmon > as.yearmon("2020-02") | (State %in% agencyNames), value, value_adjOut),
				 value_adjOut = NULL)


# df_adj %>% 
# 	filter(State == "MI", Var == "Title.16", date_yearmon >= "2004-01")
# df_adj$State %>% unique 





## Further adjusting SSI in MI, 2004:05 and 2004:06
# Note 2021-10-17 
#  -  It looks the outliers in MI has been corrected in the latest version 
#     of the original data. 

# df_adj %>% 
# 	filter(State == "MI", 
# 				 Var %in% c("Title.2", "Title.16", "Concurrent")) %>% 
# 	ggplot(aes(x = date_yearmon, y = value, color = Var)) + 
# 	geom_line()+
# 	geom_vline(xintercept =  as.yearmon(c("2004-04", "2004-05")) )
# 	
# Panel_unadj %>% 
# 	filter(State == "MI") %>% 
# 	select(State, date_yearmon, Title.2) %>% 
# 	ggplot(aes(x = date_yearmon, y =Title.2)) + 
# 	geom_line()


# Simple adjustment: average of the same month 1yr before and 1yr after
# df_adj <-
#   df_adj %>%
# 	mutate(value = ifelse(State == "MI" & Var == "Title.16" & date_yearmon == "2004-05",
# 												(value[State == "MI" & Var == "Title.16" & date_yearmon == "2003-05"] +
# 												 value[State == "MI" & Var == "Title.16" & date_yearmon == "2005-05"])/2,
# 												 value),
# 				 value = ifelse(State == "MI" & Var == "Title.16" & date_yearmon == "2004-06",
# 				 			          (value[State == "MI" & Var == "Title.16" & date_yearmon == "2003-06"] +
# 				 			  	       value[State == "MI" & Var == "Title.16" & date_yearmon == "2005-06"])/2,
# 				 			           value)
# 				 )

## Try applying the outlier adjustment procedture multiple times 


## National aggregate (AG: 50 states + DC, no PR)

df_adj <- 
bind_rows(

df_adj %>% 
	filter(State %in% c(statesAll, agencyNames)) %>% 
	# gather(Var, value, -State, -index, -date_yearmon) %>% 
	mutate(State = as.character(State)) %>% 
	ungroup %>% 
	spread(Var, value),


df_adj %>% 
	filter(State %in% statesAll) %>% 
	#gather(Var, value, -State, -index, -date_yearmon) %>% 
	group_by(Var, date_yearmon) %>% 
	summarise(State = "AG",
						date_yearmon = unique(date_yearmon),
						value = sum(value, na.rm = TRUE)) %>% 
	ungroup %>% 
	spread(Var, value),
 
df_adj %>% 
	filter(State %in% c(statesAll, agencyNames))  %>% 
	# gather(Var, value, -State, -index, -date_yearmon) %>% 
	group_by(Var, date_yearmon) %>% 
	summarise(State = "NW",
						date_yearmon = unique(date_yearmon),
						value = sum(value, na.rm = TRUE)) %>% 
	ungroup %>% 
	spread(Var, value)
)
	
# df_adj <- 
#   df_adj %>% 
# 	# select(-value_adjOut) %>% 
# 	spread(Var, value)

# df_adj %>% filter(State == "AG")

#*******************************************************************************
##  Combine adjusted and unadjusted variables and updating log and lag  ----
#*******************************************************************************

Panel <- 
left_join(df_adj,
					Panel %>% select(-all_of(vars_adj)) %>% mutate(State = as.character(State))
					) %>% 
relocate(State, date_yearmon, Cal.Year, Month, Formatted.Date, Region, No.week, adj.factor) %>% 
mutate(State = factor(State))


# x <- 
# 	df_adj %>% filter(State == "NY") %>% 
# 	dplyr::select(State, date_yearmon, Title.16)



#Creating Log variables, update allowance rates

Panel = transform(Panel,  
									lTotal      = log(Total),
									lTitle.2    = log(Title.2),
									lTitle.16   = log(Title.16),
									lConcurrent = log(Concurrent),
									
									AllowRate.Total      = Allow.Total / Det.Total, 
									AllowRate.Title.2    = Allow.Title.2 / Det.Title.2,
									AllowRate.Title.16   = Allow.Title.16 / Det.Title.16,
									AllowRate.Concurrent = Allow.Concurrent / Det.Concurrent
									
)

# Creating lags of log 

# Panel = pdata.frame(Panel, c("State","index"))
# 
# Panel$LlTotal       = lag(Panel$lTotal)
# Panel$LlTitle.2     = lag(Panel$lTitle.2)
# Panel$LlTitle.16    = lag(Panel$lTitle.16)
# Panel$LlConcurrent  = lag(Panel$lConcurrent)
# 
# 
# 
# 
# #Creating Log differenced variables.
# 
# Panel$dlTotal       = diff(Panel$lTotal)
# Panel$dlTitle.2     = diff(Panel$lTitle.2)
# Panel$dlTitle.16    = diff(Panel$lTitle.16)
# Panel$dlConcurrent  = diff(Panel$lConcurrent)
# 
# 
# 
# # Creating lags of log difference
# 
# # 1st Lag
# Panel$LdlTotal       = lag(Panel$dlTotal)
# Panel$LdlTitle.2     = lag(Panel$dlTitle.2)
# Panel$LdlTitle.16    = lag(Panel$dlTitle.16)
# Panel$LdlConcurrent  = lag(Panel$dlConcurrent)
# 
# 
# # 2nd Lag
# Panel$L2dlTotal       = lag(Panel$dlTotal, 2)
# Panel$L2dlTitle.2     = lag(Panel$dlTitle.2, 2)
# Panel$L2dlTitle.16    = lag(Panel$dlTitle.16, 2)
# Panel$L2dlConcurrent  = lag(Panel$dlConcurrent, 2)
# 
# 
# # 3rd Lag
# Panel$L3dlTotal       = lag(Panel$dlTotal, 3)
# Panel$L3dlTitle.2     = lag(Panel$dlTitle.2, 3)
# Panel$L3dlTitle.16    = lag(Panel$dlTitle.16, 3)
# Panel$L3dlConcurrent  = lag(Panel$dlConcurrent, 3)
# 
# #fix(Panel)
# 
# Panel <-  as_tibble(Panel)
# Panel$index <-  as.numeric(Panel$index)


# Panel_unadj %>% filter(State == "AG")


#*******************************************************************************
##  Saving results  ----
#*******************************************************************************
save(Panel, Panel_unadj, file = "Data/Panel.RData")




# Panel %>%
# 	mutate(index = date_yearmon) %>%
# 	filter(State %in% c("AG",  "NW")) %>%
# 	select(State, index,
# 				 Title.2, Title.16, Concurrent
# 				 #Det.Title.2, Det.Title.16, Det.Concurrent,
# 				 #AllowRate.Title.2, AllowRate.Title.16, AllowRate.Concurrent
# 				 )  %>%
# 	gather(Var, value, -State, -index) %>%
# 	ggplot(aes(x = index, y =value, color = State)) + facet_grid(Var~.) +
# 	geom_line() +
# 	coord_cartesian(ylim = c(0, NA))
# 
# 
# Panel %>%
# 	mutate(index = date_yearmon) %>%
# 	filter(State %in% agencyNames) %>%
# 	#filter(State %in% "FE") %>%
# 	select(State, index, Title.2, Title.16, Concurrent) %>%
# 	gather(Var, value, -State, -index) %>%
# 	ggplot(aes(x = index, y =value, color = State)) + facet_grid(Var~.) +
# 	geom_line()











