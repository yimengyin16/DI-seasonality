# Importing raw Data ----------------------------------------------------------

dir <- "E:/SkyDrive/Working/Third year/Original Data/"
input.file <- "SSA-SA-MOWL-2013.1.csv" 
date.file <- "SSA-DATES1.csv"

raw.data <- data.frame(read.csv(paste(dir, input.file, sep = "")))
date <- data.frame(read.csv(paste(dir, date.file, sep = "")))


# Adding missing value for AK 2011.3 --------------------------------------

missing.AK = data.frame(read.csv(paste(dir,"SSA-SA-MOWL-2013.1-AK2011.3.csv", sep = "")))
raw.data = rbind(raw.data, missing.AK)


# Create a data frame containing variables to be used.

all0 = raw.data[c(
                 "Region.Code",
                 "State.Code",
                 "Date",
                 "Formatted.Date",
                 "Receipts..All.Initial.",
                 "Receipts..Initial.SSDI.Only.",
                 "Receipts..Initial.SSI.Only.",
                 "Receipts..Initial.Concurrent.Only."
                 )]

# Create variables "DI" = SSDI only + Concurrent, and "SSI" = SSI only + concurrent

all0$DI = with(all0, Receipts..Initial.SSDI.Only. + Receipts..Initial.Concurrent.Only.)
all0$SSI = with(all0, Receipts..Initial.SSI.Only. + Receipts..Initial.Concurrent.Only.)


# Creating nationwide sum for each month in each year.

nation = aggregate(all0[c("Receipts..All.Initial.", "Receipts..Initial.SSDI.Only.",
                          "Receipts..Initial.SSI.Only.", "Receipts..Initial.Concurrent.Only.",
                          "DI", "SSI")],
                   list(Formatted.Date = all0$Formatted.Date),
                   sum
                   )

nation$State.Code = "NW"
nation$Region.Code = "NW"
nation$Date = all0$Date[all0$State.Code == "DC"]

# Creating aggregate sereis of 50 states + DC for each month in each year.

aggregated = aggregate(all0[all0$State.Code %in% statesAll, c("Receipts..All.Initial.", "Receipts..Initial.SSDI.Only.",
                          "Receipts..Initial.SSI.Only.", "Receipts..Initial.Concurrent.Only.",
                          "DI", "SSI")],
                   list(Formatted.Date = all0[all0$State.Code %in% statesAll,]$Formatted.Date),
                   sum
)

aggregated$State.Code = "AG"
aggregated$Region.Code = "AG"
aggregated$Date = all0$Date[all0$State.Code == "DC"]

# Creating aggregate sereis of 48 continental states each month in each year. (DC excluded)

aggregated48 = aggregate(all0[all0$State.Code %in% states48, c("Receipts..All.Initial.", "Receipts..Initial.SSDI.Only.",
                                                              "Receipts..Initial.SSI.Only.", "Receipts..Initial.Concurrent.Only.",
                                                              "DI", "SSI")],
                       list(Formatted.Date = all0[all0$State.Code %in% states48,]$Formatted.Date),
                       sum
)

aggregated48$State.Code = "A8"
aggregated48$Region.Code = "A8"
aggregated48$Date = all0$Date[all0$State.Code == "DC"]

# Creating aggregate sereis of 49 continental states each month in each year. (DC included)

aggregated49 = aggregate(all0[all0$State.Code %in% states49, c("Receipts..All.Initial.", "Receipts..Initial.SSDI.Only.",
                                                               "Receipts..Initial.SSI.Only.", "Receipts..Initial.Concurrent.Only.",
                                                               "DI", "SSI")],
                         list(Formatted.Date = all0[all0$State.Code %in% states49,]$Formatted.Date),
                         sum
)

aggregated49$State.Code = "A9"
aggregated49$Region.Code = "A9"
aggregated49$Date = all0$Date[all0$State.Code == "DC"]


# Create regional sum for each month in each year. For convenience, the 
# regional code is used as State Code.

by.Region = aggregate(all0[c("Receipts..All.Initial.", "Receipts..Initial.SSDI.Only.",
                             "Receipts..Initial.SSI.Only.", "Receipts..Initial.Concurrent.Only.",
                             "DI","SSI")],
                      list(Formatted.Date = all0$Formatted.Date, Region.Code = all0$Region.Code),
                      sum
                      )

by.Region$State.Code = by.Region$Region.Code
by.Region$Date = all0$Date[all0$State.Code == "DC"]


#Merge the national sum to "all"

all0 = rbind(all0,nation, aggregated, aggregated49, aggregated48, by.Region)


# Create varialbe "Cal.Year" and "Month" in "all"
all0$Cal.Year = as.integer(substring(all0$Formatted.Date,1,4))
all0$Month = as.integer(substring(all0$Formatted.Date,6,7))

all0 = all0[order(as.character(all0$State.Code),all0$Cal.Year,all0$Month),]
all0$State.Code = factor(as.character(all0$State.Code))

# Create in "all" a variable "No.week" by matching the varialbe "Week.Ending.Date"
# in "date".

all0$No.week = 0
date.no.NA = date[is.na(date$SSA.Weeks.in.Month) == FALSE,]

for (i in 1:nrow(all0))
  all0$No.week[i] = with(date.no.NA,
    mean(SSA.Weeks.in.Month[as.character(SSA.Month) == as.character(all0$Formatted.Date[i])]))

# Rename "all0"
library(reshape)

all0 = rename(all0, c(
                        Region.Code = "Region",
                        State.Code = "State",
                        Receipts..All.Initial. = "Total",
                        Receipts..Initial.SSDI.Only. = "Title.2",
                        Receipts..Initial.SSI.Only. = "Title.16",
                        Receipts..Initial.Concurrent.Only. = "Concurrent"
                        ))

# Length of month adjustment ----------------------------------------------
  # Adjsut all series in "all0" for number of week, and save as "all"

## Use different average no of week for each months
## cycle.week contains the number of weeks in each month in a 28 year cycle. Although this is calculated based on Saturday, the long-run
## average number of weeks in a working month calculated based on it (ave.week) is still correct. 
cycle.week =  data.frame(read.table(paste(dir,"no.week.28year.txt",sep=""),header=TRUE,sep="\t"))

## generate a vector containing the  average number of weeks in each month in the long-run (28 years)
ave.week = numeric(12)
for (i in 1:12){
  ave.week[i]=mean(cycle.week$no.week[cycle.week$Month==i])
}

## generate a adjustor. For each month, it is equal to the long run average number of weeks in working month divided by the actual number of weeks (Fridays) in it  

adj.week.diff = numeric(nrow(all0))
for (i in 1:nrow(all0)){
  adj.week.diff[i] = ave.week[all0$Month[i]]/all0$No.week[i]
}

## Use the same average no of week for all months
avg = 1461/336
adj.week.same = avg/all0$No.week


all = all0
all.df = all0
all.same = all0

all[c("Total","Title.2","Title.16","Concurrent","DI","SSI")] = all0[c("Total","Title.2","Title.16","Concurrent","DI","SSI")]*adj.week.diff
all.df[c("Total","Title.2","Title.16","Concurrent","DI","SSI")] = all0[c("Total","Title.2","Title.16","Concurrent","DI","SSI")]*adj.week.diff
all.same[c("Total","Title.2","Title.16","Concurrent","DI","SSI")] = all0[c("Total","Title.2","Title.16","Concurrent","DI","SSI")]*adj.week.same

save(all, all.df,all.same,all0, file = "all.RData")


rm(dir,
   date.file,
   input.file,
   
   raw.data,
   
   missing.AK,
   
   i,
   
   nation,
   aggregated,
   aggregated48,
   aggregated49,
   by.Region,
   date.no.NA,
   date,
   
   cycle.week,
   
   adj.week.diff,
   adj.week.same,
   #ave.week,
   avg
   )


