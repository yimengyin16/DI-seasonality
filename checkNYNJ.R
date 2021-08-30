# Check potential data issue in NY and NJ for DI applications


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


load("Data/Panel.RData")




plot.path = "paper_plot/"
dir.fig_newVer = "paper_plot/fig_newVer/"


monthName = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
							"Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

Panel <- 
	Panel %>% 
	mutate(date_yearmon =  as.yearmon(Formatted.Date))


Panel_all <- Panel
Panel <- filter(Panel,date_yearmon <= "2020-2")



Panel_all %<>%
	mutate(date_yearmon = yearmonth(date_yearmon)) %>% 
	rename(idx = index) %>%  
	tsibble(key = State, index = date_yearmon) 
	#select(State, Cal.Year, Month, date_yearmon, Title.2, Title.16, Concurrent)


# Additive model for rates, multiplicative model for numbers
ets_fml_AA <- ~ error("A") + trend("Ad") + season("A")
ets_fml_MM <- ~ error("M") + trend("Ad") + season("M")




## Plotting data

Panel_all %>%
	filter(State == "NY") %>% 
	ggplot(aes(x = date_yearmon, y = Title.16.o)) + 
	geom_line() + 
	geom_point()

Panel_all %>%
	filter(State == "NY") %>% 
	ggplot(aes(x = date_yearmon, y = Title.2.o)) + 
	geom_line() + 
	geom_point()


Panel_all %>%
	filter(State == "NY") %>% 
	ggplot(aes(x = date_yearmon, y = Concurrent)) + 
	geom_line() + 
	geom_point()



Panel_all %>%
	filter(State == "NJ") %>% 
	ggplot(aes(x = date_yearmon, y = Title.2)) + 
	geom_line() + 
	geom_point()

	




## Examine NJ

df_nj <- 
	Panel_all %>%
	filter(State == "NJ",
				 date_yearmon <= yearmonth("2020-02"))

df_nj2 <- 
	Panel_all %>%
	filter(State == "NJ",
				 date_yearmon <= yearmonth("2020-02"),
				 date_yearmon > yearmonth("2000-11"))	
	

mod_nj <- list(
	SSDI = df_nj %>% rename(y = Title.2)    %>%  model(SSDI = ETS(update(ets_fml_MM, y~.))),
	SSI  = df_nj %>% rename(y = Title.16)   %>%  model(SSI  = ETS(update(ets_fml_MM, y~.))),
	conc = df_nj %>% rename(y = Concurrent) %>%  model(conc = ETS(update(ets_fml_MM, y~.)))
)	


mod_nj2 <- list(
	SSDI = df_nj2 %>% rename(y = Title.2)    %>%  model(SSDI = ETS(update(ets_fml_MM, y~.))),
	SSI  = df_nj2 %>% rename(y = Title.16)   %>%  model(SSI  = ETS(update(ets_fml_MM, y~.))),
	conc = df_nj2 %>% rename(y = Concurrent) %>%  model(conc = ETS(update(ets_fml_MM, y~.)))
)	



components(mod_nj$SSDI) %>% gg_season(season)
components(mod_nj$SSI)  %>% gg_season(season)
components(mod_nj$conc) %>% gg_season(season)


components(mod_nj2$SSDI) %>% gg_season(season)
components(mod_nj2$SSI)  %>% gg_season(season)
components(mod_nj2$conc) %>% gg_season(season)





## Examine NY

df_ny <- 
	Panel_all %>%
	filter(State == "NY",
				 date_yearmon <= yearmonth("2020-02"))

df_ny2 <- 
	Panel_all %>%
	filter(State == "NY",
				 date_yearmon <= yearmonth("2020-02"),
				 date_yearmon > yearmonth("2001-11"))	


mod_ny <- list(
	SSDI = df_ny %>% rename(y = Title.2)    %>%  model(SSDI = ETS(update(ets_fml_MM, y~.))),
	SSI  = df_ny %>% rename(y = Title.16)   %>%  model(SSI  = ETS(update(ets_fml_MM, y~.))),
	conc = df_ny %>% rename(y = Concurrent) %>%  model(conc = ETS(update(ets_fml_MM, y~.)))
)	


mod_ny2 <- list(
	SSDI = df_ny2 %>% rename(y = Title.2)    %>%  model(SSDI = ETS(update(ets_fml_MM, y~.))),
	SSI  = df_ny2 %>% rename(y = Title.16)   %>%  model(SSI  = ETS(update(ets_fml_MM, y~.))),
	conc = df_ny2 %>% rename(y = Concurrent) %>%  model(conc = ETS(update(ets_fml_MM, y~.)))
)	



components(mod_ny$SSDI) %>% gg_season(season)
components(mod_ny$SSI) %>% gg_season(season)
components(mod_ny$conc) %>% gg_season(season)


components(mod_ny2$SSDI) %>% gg_season(season)
components(mod_ny2$SSI)  %>% gg_season(season)
components(mod_ny2$conc) %>% gg_season(season)














