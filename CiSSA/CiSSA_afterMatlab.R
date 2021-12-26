# This script load and process output from the matlab CiSSA script


library(tidyverse)
library(readr)
library(ggplot2)
library(magrittr)

## loading CiSSA results
df_rc_SSDI.NW <- read_csv("CiSSA/rc_SSDI_NW.csv", col_names = FALSE)
df_rc_SSI.NW  <- read_csv("CiSSA/rc_SSI_NW.csv", col_names = FALSE)
df_rc_conc.NW <- read_csv("CiSSA/rc_conc_NW.csv", col_names = FALSE)

df_rc_SSDI.NW_pre <- read_csv("CiSSA/rc_SSDI_NW_pre.csv", col_names = FALSE)
df_rc_SSI.NW_pre  <- read_csv("CiSSA/rc_SSI_NW_pre.csv", col_names = FALSE)
df_rc_conc.NW_pre <- read_csv("CiSSA/rc_conc_NW_pre.csv", col_names = FALSE)


names(df_rc_SSDI.NW) <- c("SSDI_rc.trend", "SSDI_rc.cycle", "SSDI_rc.seasonal")
names(df_rc_SSI.NW)  <- c("SSI_rc.trend",  "SSI_rc.cycle",  "SSI_rc.seasonal")
names(df_rc_conc.NW) <- c("conc_rc.trend", "conc_rc.cycle", "conc_rc.seasonal")

names(df_rc_SSDI.NW_pre) <- c("SSDI_rc.trend", "SSDI_rc.cycle", "SSDI_rc.seasonal")
names(df_rc_SSI.NW_pre)  <- c("SSI_rc.trend",  "SSI_rc.cycle",  "SSI_rc.seasonal")
names(df_rc_conc.NW_pre) <- c("conc_rc.trend", "conc_rc.cycle", "conc_rc.seasonal")


## adding dates

load("Data/Panel.RData") %>% print

df_index <- 
	Panel %>% 
	filter(State == "NW") %>% 
	select(State, Cal.Year, Month, Formatted.Date, date_yearmon, SSDI = Title.2, SSI = Title.16, conc = Concurrent) 
	#filter(Formatted.Date != "2021-08")

df_CiSSA.NW <- 
	bind_cols(df_index, df_rc_SSDI.NW, df_rc_SSI.NW, df_rc_conc.NW) %>% 
	mutate(SSDI_rc.seaAdj = SSDI - SSDI_rc.seasonal,
				 SSI_rc.seaAdj  = SSI - SSI_rc.seasonal,
				 conc_rc.seaAdj = conc - conc_rc.seasonal,
				 
				 SSDI_rc.seasfct = SSDI_rc.seasonal/(SSDI_rc.trend + SSDI_rc.cycle) + 1,
				 SSI_rc.seasfct  = SSI_rc.seasonal/(SSI_rc.trend + SSI_rc.cycle) + 1,
				 conc_rc.seasfct = conc_rc.seasonal/(conc_rc.trend + conc_rc.cycle) + 1,
				 
				 seasType = "postCovid"
				 ) %>% 
	rename(SSDI.original = SSDI,
				 SSI.original  = SSI,
				 conc.original = conc
		
	)


df_index_pre <- 
	df_index %>% filter(date_yearmon <= zoo::as.yearmon("2020-02"))

df_CiSSA.NW_pre <- 
	bind_cols(df_index_pre, df_rc_SSDI.NW_pre, df_rc_SSI.NW_pre, df_rc_conc.NW_pre) %>% 
	mutate(SSDI_rc.seaAdj = SSDI - SSDI_rc.seasonal,
				 SSI_rc.seaAdj  = SSI - SSI_rc.seasonal,
				 conc_rc.seaAdj = conc - conc_rc.seasonal,
				 
				 SSDI_rc.seasfct = SSDI_rc.seasonal/(SSDI_rc.trend + SSDI_rc.cycle) + 1,
				 SSI_rc.seasfct  = SSI_rc.seasonal/(SSI_rc.trend + SSI_rc.cycle) + 1,
				 conc_rc.seasfct = conc_rc.seasonal/(conc_rc.trend + conc_rc.cycle) + 1,
				 
				 seasType = "preCovid"
	) %>% 
	rename(SSDI.original = SSDI,
				 SSI.original  = SSI,
				 conc.original = conc
				 
	)

	


# plot(df_CiSSA.NW$SSDI_rc.seasonal, type = "l")
# plot(df_CiSSA.NW$SSDI_rc.cycle, type = c("p") )
# plot(df_CiSSA.NW$SSDI_rc.trend, type = "l")
# 
# plot(df_CiSSA.NW$SSI_rc.seasonal, type = "l")
# plot(df_CiSSA.NW$SSI_rc.cycle, type = "l")
# plot(df_CiSSA.NW$SSI_rc.trend, type = "l")
# 
# plot(df_CiSSA.NW$conc_rc.seasonal, type = "l")
# plot(df_CiSSA.NW$conc_rc.cycle, type = "l")
# plot(df_CiSSA.NW$conc_rc.trend, type = "l")


## Save results

saveRDS(df_CiSSA.NW, file = "CiSSA/data_CiSSA.NW.rds")
saveRDS(df_CiSSA.NW_pre, file = "CiSSA/data_CiSSA.NW_pre.rds")










