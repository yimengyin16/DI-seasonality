# This script load and process output from the matlab CiSSA script


library(tidyverse)
library(readr)
library(ggplot2)
library(magrittr)

## loading CiSSA results
df_rc_SSDI.NW <- read_csv("CiSSA/rc_SSDI_NW.csv", col_names = FALSE)
df_rc_SSI.NW  <- read_csv("CiSSA/rc_SSI_NW.csv", col_names = FALSE)
df_rc_conc.NW <- read_csv("CiSSA/rc_conc_NW.csv", col_names = FALSE)


names(df_rc_SSDI.NW) <- c("SSDI_rc.trend", "SSDI_rc.cycle", "SSDI_rc.seasonal")
names(df_rc_SSI.NW)  <- c("SSI_rc.trend",  "SSI_rc.cycle",  "SSI_rc.seasonal")
names(df_rc_conc.NW) <- c("conc_rc.trend", "conc_rc.cycle", "conc_rc.seasonal")



## adding dates

load("Data/Panel.RData") %>% print

df_index <- 
	Panel %>% 
	filter(State == "NW") %>% 
	select(State, Cal.Year, Month, Formatted.Date) %>%   
	filter(Formatted.Date != "2021-08")

df_CiSSA.NW <- bind_cols(df_index, df_rc_SSDI.NW, df_rc_SSI.NW, df_rc_conc.NW)


plot(df_CiSSA.NW$SSDI_rc.seasonal, type = "l")
plot(df_CiSSA.NW$SSDI_rc.cycle, type = c("p") )
plot(df_CiSSA.NW$SSDI_rc.trend, type = "l")

plot(df_CiSSA.NW$SSI_rc.seasonal, type = "l")
plot(df_CiSSA.NW$SSI_rc.cycle, type = "l")
plot(df_CiSSA.NW$SSI_rc.trend, type = "l")

plot(df_CiSSA.NW$conc_rc.seasonal, type = "l")
plot(df_CiSSA.NW$conc_rc.cycle, type = "l")
plot(df_CiSSA.NW$conc_rc.trend, type = "l")


## Save results

saveRDS(df_CiSSA.NW, file = "CiSSA/data_CiSSA.NW.rds")










