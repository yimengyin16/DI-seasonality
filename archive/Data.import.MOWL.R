

# This file creates a data file "all.RData" containing two data frames:
  # all0: Original MOWL data with Total, SSI only, SSDI only, SSI, SSDI applications at national  and state levels
  # all : The same as all0, but all data entries are adjusted for the length of "SSA working month"


# Notes on collection of states

# NW: all states and regions
# AG: 50 states + DC
# A8: 48 continental states (DC excluded)
# A9: 49 continental states (DC included)


## including AG and DC
#  states52 
#
## Only states and DC, no NW
#  statesAll
#
## including AG, but no DC
#  states51
#
## withou NW and DC
#  states50 
#
## 48 continental states, AK and HI excluded and DC excluded. 
#  states48
#
## 49 continental states, AK and HI excluded and DC included. 
#  states49 
#
## states.usaww


#top5 = c("NW","CA","TX","FL","NY","PA")
#top10 = c("CA", "TX", "FL", "NY", "PA", "OH", "MI", "IL", "NC", "GA")




source("General.R")

# Importing raw Data ----------------------------------------------------------


# all0 <- read.csv(paste0(path_proj, "/Original Data/SSA-SA-MOWL-2014.8.csv"), header = FALSE)
# all0 <- read.csv(paste0(path_proj, "/Original Data/SSA-SA-MOWL-2019.08.csv"), header = FALSE)
all0 <- read.csv("Data/MOWL/SSA-SA-MOWL-2021-04.csv", header = FALSE)



# "SSA-SA-MOWL-2021.08.csv" contains no variable names, need to extract variable names from "SSA-SA-MOWL-2014.7.csv"
NAME = names(read.csv("Data/MOWL/SSA-SA-MOWL-2014-07.csv"))
names(all0) = NAME
rm(NAME)




# Date translation table
# date <- read.csv("Data/MOWL/SSA-DATES1-extended.csv")[,1:14]
# date <- date[!is.na(date$File.Name), ]

date <- read.csv("Data/MOWL/SSA_DATES1-2021.csv")[,1:14]
date <- date[!is.na(date$File.Name), ]





# Create a data frame containing variables to be used.

varlist <-  c("Receipts..All.Initial.",
              "Receipts..Initial.SSDI.Only.",
              "Receipts..Initial.SSI.Only.",
              "Receipts..Initial.Concurrent.Only.",
              
              "Determinations..All.Initial.",
              "Allowances..All.Initial.",
              
              "Determinations..Initial.SSDI.Only.",
              "Allowances..Initial.SSDI.Only.",
              
              "Determinations..Initial.SSI.Only.",
              "Allowances..Initial.SSI.Only.",
              
              "Determinations..Initial.Concurrent.Only.",
              "Allowances..Initial.Concurrent.Only."
              )

all0 = all0[ c(  "Region.Code",
                 "State.Code",
                 "Formatted.Date",
                 varlist
                 )]

# Create variables "DI" = SSDI only + Concurrent, and "SSI" = SSI only + concurrent
all0 <- all0 %>%  mutate(across(!c(Region.Code:Formatted.Date), ~ as.numeric(str_remove(.x, ","))))

all0$DI  = with(all0, Receipts..Initial.SSDI.Only. + Receipts..Initial.Concurrent.Only.)
all0$SSI = with(all0, Receipts..Initial.SSI.Only.  + Receipts..Initial.Concurrent.Only.)

all0

# Creating nationwide sum for each month in each year.
nation = aggregate(all0[c(varlist, "DI", "SSI")],
                   list(Formatted.Date = all0$Formatted.Date),
                   sum)

nation$State.Code = "NW"
nation$Region.Code = "NW"
nation$Date = all0$Date[all0$State.Code == "DC"]



# Creating aggregate sereis of 50 states + DC for each month in each year.
aggregated = aggregate(all0[all0$State.Code %in% statesAll, c(varlist, "DI", "SSI")],
                   list(Formatted.Date = all0[all0$State.Code %in% statesAll,]$Formatted.Date),
                   sum)

aggregated$State.Code = "AG"
aggregated$Region.Code = "AG"
aggregated$Date = all0$Date[all0$State.Code == "DC"]


# Creating aggregate sereis of 48 continental states each month in each year. (DC excluded)
aggregated48 = aggregate(all0[all0$State.Code %in% states48, c(varlist, "DI", "SSI")],
                       list(Formatted.Date = all0[all0$State.Code %in% states48,]$Formatted.Date),
                       sum)

aggregated48$State.Code = "A8"
aggregated48$Region.Code = "A8"
aggregated48$Date = all0$Date[all0$State.Code == "DC"]

# Creating aggregate sereis of 49 continental states each month in each year. (DC included)
aggregated49 = aggregate(all0[all0$State.Code %in% states49, c(varlist, "DI", "SSI")],
                         list(Formatted.Date = all0[all0$State.Code %in% states49,]$Formatted.Date),
                         sum)

aggregated49$State.Code = "A9"
aggregated49$Region.Code = "A9"
aggregated49$Date = all0$Date[all0$State.Code == "DC"]


#Merge the national sum to "all0"

all0 = rbind(all0,nation, aggregated, aggregated49, aggregated48)


# Create varialbe "Cal.Year" and "Month" in "all0"
all0$Cal.Year = as.integer(substring(all0$Formatted.Date,1,4))
all0$Month    = as.integer(substring(all0$Formatted.Date,6,7))

all0 = all0[order(as.character(all0$State.Code),all0$Cal.Year,all0$Month),]
all0$State.Code = factor(as.character(all0$State.Code))


# Create in "all0" a variable "No.week" by matching the varialbe "SSA.Month" in "date".
all0$No.week = 0

for (i in 1:nrow(all0))
  all0$No.week[i] = with(date,
                    mean(SSA.Weeks.in.Month[as.character(SSA.Month) == as.character(all0$Formatted.Date[i])]))



# correcting the number of weeks in 2011-9 from 4 to 5.
# TODO: May not be necessary now. 


# Also check 2005-9, and 2016-9 data

all0[all0$Formatted.Date == "2011-09", "No.week"] = 5
all0[all0$Formatted.Date == "2016-09", "No.week"] = 5



# Rename "all0"
all0 = plyr::rename(all0, c(
                        Region.Code = "Region",
                        State.Code = "State",
                        Receipts..All.Initial. = "Total.o",
                        Receipts..Initial.SSDI.Only. = "Title.2.o",
                        Receipts..Initial.SSI.Only. = "Title.16.o",
                        Receipts..Initial.Concurrent.Only. = "Concurrent.o",
                        DI = "DI.o",
                        SSI = "SSI.o",
                        
                        Determinations..All.Initial. = "Det.Total.o",
                        Allowances..All.Initial.     = "Allow.Total.o",
                        
                        Determinations..Initial.SSDI.Only. = "Det.Title.2.o",
                        Allowances..Initial.SSDI.Only.     = "Allow.Title.2.o",
                        
                        Determinations..Initial.SSI.Only.  = "Det.Title.16.o",
                        Allowances..Initial.SSI.Only.      = "Allow.Title.16.o",
                        
                        Determinations..Initial.Concurrent.Only. = "Det.Concurrent.o",
                        Allowances..Initial.Concurrent.Only.     = "Allow.Concurrent.o"
                        ))




# Length of month adjustment ----------------------------------------------
  # Adjsut all series in "all0" for number of week, and save as "all"
  # Use the same average no of week for all months

avg = 1461/336 # (number of weeks / number of months in a 28-year cycle: 4.38)  
all0$adj.factor = avg/all0$No.week


all = all0

all = transform(all, Total      = Total.o*adj.factor,
                     Title.2    = Title.2.o*adj.factor,
                     Title.16   = Title.16.o*adj.factor,
                     Concurrent = Concurrent.o*adj.factor,
                     DI         = DI.o*adj.factor,
                     SSI        = SSI.o*adj.factor,
                     
                     Det.Total   = Det.Total.o*adj.factor,
                     Allow.Total =  Allow.Total.o*adj.factor,
                    
                     Det.Title.2   = Det.Title.2.o*adj.factor,
                     Allow.Title.2 = Allow.Title.2.o*adj.factor,
                    
                     Det.Title.16   = Det.Title.16.o*adj.factor,
                     Allow.Title.16 = Allow.Title.16.o*adj.factor,
                    
                     Det.Concurrent   = Det.Concurrent.o*adj.factor,
                     Allow.Concurrent = Allow.Concurrent.o*adj.factor,
                    
                     Total.o = NULL,
                     Title.2.o = NULL,
                     Title.16.o = NULL,
                     Concurrent.o = NULL,
                     DI.o = NULL,
                     SSI.o = NULL,
                     
                     Det.Total.o = NULL,
                     Allow.Total.o = NULL,
                     Det.Title.2.o = NULL,
                     Allow.Title.2.o = NULL,
                     Det.Title.16.o = NULL,
                     Allow.Title.16.o = NULL,
                     Det.Concurrent.o = NULL,
                     Allow.Concurrent.o = NULL
                     )


## Adjusting outlier in 2004:6
# TODO: see if the issue still exist in the lastest data

New_Jun = function(category, region, Data){
#                 category = "Title.2"
#                 region = "NW"
#                 Data = all 
              value.May <- Data[Data$State == region & Data$Formatted.Date == "2004-05", category]
              x <- window(select(category, region, Data), end = c(2007,12))
              x <- diff(log(x))
              
              #x = window(select("dlTotal", "NW", Panel), end = c(2007,12))
              
              odummy1 <- numeric(length(x))
              odummy2 <- numeric(length(x))
              odummy1[44] <- 1
              odummy2[45] <- 1
              
              fit <- lm(x ~ factor(cycle(x))+ odummy1 + odummy2)
              #summary(fit)
              ol.adj <- summary(fit)$coef["odummy1","Estimate"]
              ol.adj
              
              dlvalue.Jun = x[44] - ol.adj 
              new.value.Jun = exp(log(value.May) + dlvalue.Jun)
            }
 

for (i in statesAll){
  for (j in c("Title.2", "Title.16", "Concurrent",
              "Det.Total",   
              "Allow.Total", 
              "Det.Title.2",   
              "Allow.Title.2", 
              "Det.Title.16",   
              "Allow.Title.16", 
              "Det.Concurrent",
              "Allow.Concurrent")){
    all[all$State == i & all$Formatted.Date == "2004-06", j] = 
      New_Jun(category = j, region = i, all)  
  }
  
  all[all$State == i & all$Formatted.Date == "2004-06", "Total"] = 
    all[all$State == i & all$Formatted.Date == "2004-06", "Title.2"] + 
    all[all$State == i & all$Formatted.Date == "2004-06", "Title.16"] +
    all[all$State == i & all$Formatted.Date == "2004-06", "Concurrent"]
  
  all[all$State == i & all$Formatted.Date == "2004-06", "DI"] = 
    all[all$State == i & all$Formatted.Date == "2004-06", "Title.2"] + 
    all[all$State == i & all$Formatted.Date == "2004-06", "Concurrent"]
  
  all[all$State == i & all$Formatted.Date == "2004-06", "SSI"] = 
    all[all$State == i & all$Formatted.Date == "2004-06", "Title.16"] + 
    all[all$State == i & all$Formatted.Date == "2004-06", "Concurrent"]
  
  print(i)
}


for (j in c("Total", "Title.2", "Title.16", "Concurrent", "DI", "SSI",
            "Det.Total",   
            "Allow.Total", 
            "Det.Title.2",   
            "Allow.Title.2", 
            "Det.Title.16",   
            "Allow.Title.16", 
            "Det.Concurrent",
            "Allow.Concurrent")){
  all[all$State == "NW" & all$Formatted.Date == "2004-06", j] <- 
    sum(subset(all, (!State %in% c("NW", "AG", "A8", "A9")) & Formatted.Date == "2004-06", j))
  
  all[all$State == "AG" & all$Formatted.Date == "2004-06", j] <- 
    sum(subset(all, (State %in% statesAll) & Formatted.Date == "2004-06", j))
  
  all[all$State == "A8" & all$Formatted.Date == "2004-06", j] <- 
    sum(subset(all, (State %in% states48) & Formatted.Date == "2004-06", j))
  
  all[all$State == "A9" & all$Formatted.Date == "2004-06", j] <- 
    sum(subset(all, (State %in% states49) & Formatted.Date == "2004-06", j))
}




## Adjusting data error in 2014:11: application, determiniation and allowance are the same for SSI and concurrent. 
# TODO check if the error still exist in the latest data


all <- 
	all %>% 
	dplyr::mutate(Total     = ifelse(Cal.Year == 2014 & Month == 11, 0.5 * (dplyr::lag(Total, 12) + dplyr::lead(Total, 12)), Total),
								Title.2   = ifelse(Cal.Year == 2014 & Month == 11, 0.5 * (dplyr::lag(Title.2, 12) + dplyr::lead(Title.2, 12)), Title.2),
								Title.16  = ifelse(Cal.Year == 2014 & Month == 11, 0.5 * (dplyr::lag(Title.16, 12) + dplyr::lead(Title.16, 12)), Title.16),
								Concurrent= ifelse(Cal.Year == 2014 & Month == 11, 0.5 * (dplyr::lag(Concurrent, 12) + dplyr::lead(Concurrent, 12)), Concurrent),
								
								Det.Total      = ifelse(Cal.Year == 2014 & Month == 11, 0.5 * (dplyr::lag(Det.Total, 12) + dplyr::lead(Det.Total, 12)), Det.Total),
								Det.Title.2    = ifelse(Cal.Year == 2014 & Month == 11, 0.5 * (dplyr::lag(Det.Title.2, 12) + dplyr::lead(Det.Title.2, 12)), Det.Title.2),
								Det.Title.16   = ifelse(Cal.Year == 2014 & Month == 11, 0.5 * (dplyr::lag(Det.Title.16, 12) + dplyr::lead(Det.Title.16, 12)), Det.Title.16),
								Det.Concurrent = ifelse(Cal.Year == 2014 & Month == 11, 0.5 * (dplyr::lag(Det.Concurrent, 12) + dplyr::lead(Det.Concurrent, 12)), Det.Concurrent),
								
							
								Allow.Total      = ifelse(Cal.Year == 2014 & Month == 11, 0.5 * (dplyr::lag(Allow.Total, 12) + dplyr::lead(Allow.Total, 12)), Allow.Total),
								Allow.Title.2    = ifelse(Cal.Year == 2014 & Month == 11, 0.5 * (dplyr::lag(Allow.Title.2, 12) + dplyr::lead(Allow.Title.2, 12)), Allow.Title.2),
								Allow.Title.16   = ifelse(Cal.Year == 2014 & Month == 11, 0.5 * (dplyr::lag(Allow.Title.16, 12) + dplyr::lead(Allow.Title.16, 12)), Allow.Title.16),
								Allow.Concurrent = ifelse(Cal.Year == 2014 & Month == 11, 0.5 * (dplyr::lag(Allow.Concurrent, 12) + dplyr::lead(Allow.Concurrent, 12)), Allow.Concurrent)
								
								)
	



# Calculating Allowance rates as the ratios of  determinations to allowances. 

all0 = transform(all0, AllowRate.Total.o      = Allow.Total.o / Det.Total.o, 
                       AllowRate.Title.2.o    = Allow.Title.2.o / Det.Title.2.o,
                       AllowRate.Title.16.o   = Allow.Title.16.o / Det.Title.16.o,
                       AllowRate.Concurrent.o = Allow.Concurrent.o / Det.Concurrent.o)


all = transform(all, AllowRate.Total      = Allow.Total / Det.Total, 
                     AllowRate.Title.2    = Allow.Title.2 / Det.Title.2,
                     AllowRate.Title.16   = Allow.Title.16 / Det.Title.16,
                     AllowRate.Concurrent = Allow.Concurrent / Det.Concurrent)




save(all0, all, file = "Data/all.RData")


rm(i,
   nation,
   aggregated,
   aggregated48,
   aggregated49,
   avg,
   varlist
   )

