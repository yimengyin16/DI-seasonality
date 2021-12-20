# This program imports monthly employment/unemployment data from 
#  Bureau of Labor Statistics(BLS).

# This file generates a data file "emply.RData" containing a data frame "emply"
  # emply: Sesonally adjusted and unadjusted series of unemployment rate, -
  #        unemployment, employment, and labor force, at state and national level. 
  # Note: all types of national levels are the same: the original data from BLS that is -
  #        calculated based on all states. 


  # measure_code  measure_text
  # 03	          unemployment rate
  # 04	          unemployment
  # 05           	employment
  # 06          	labor force
# The output is saved to "emply.RData"


source("General.R")

# Import employment statistics from Local Area Unemployment Statistics(LAUS).----------------

# Last run on 12/18/2021
# Data up to 11/2021

## Import seasonally unadjusted series at state level
# emplyU = read.table("http://download.bls.gov/pub/time.series/la/la.data.2.AllStatesU",
#                    header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
## Import seasonally adjusted series at state level
# emplyS = read.table("http://download.bls.gov/pub/time.series/la/la.data.3.AllStatesS",
#                    header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
# 
# save(emplyU, emplyS, file = "Data/emplyData.RData")

load("Data/emplyData.RData")

emply = rbind(emplyU,emplyS)
emply = transform(emply, 
                  adj = substr(series_id,3,3),
                  State = factor(substr(series_id,6,7)),
                  Cal.Year = year,
                  Month = as.integer(substr(period,2,3)),
                  value = as.numeric(value),
                  measure = as.factor(substr(series_id,19,20)),
                  #footnote = footnote_codes,
                  series_id = NULL,
                  period = NULL,
                  year = NULL,
                  footnote_codes = NULL
                  )

#emply$Formatted.Date = with(emply, as.yearmon(paste(Cal.Year,Month,sep = "-")))

emply = emply[emply$Month != 13,] # delete the year average 

# Create variables: Unemployment rate, unemployment, employment, and labor forece
emply = dcast(emply, adj + State + Cal.Year + Month ~ measure)
emply = plyr::rename(emply, c("03" = "uRate", "04" = "unemply", "05" = "emply", "06" = "labor"))


# Convert elements in State into state name abbreviations.

convertAbbr = function(x){
# This function convert number valued elements in emply$State in to conrresponding
# state name abbreviations. 
  value = levels(x)
  abbr = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", 
           "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", 
           "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
  convert = function(y, value = value, abbr = abbr) abbr[which(value == y)]
  state = sapply(x,convert, value = value, abbr = abbr)  
}
emply$State = as.factor(convertAbbr(emply$State))

# emply$unemply1 = NA
# emply$unemply2 = NA
# emply$unemply3 = NA


# Import national level employment statistics from CPS --------------------

# Last run on 10/16/2021
# Data up to 11/2021
# CPSdata = read.table("http://download.bls.gov/pub/time.series/ln/ln.data.1.AllData",
#                     header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
#This will take a while to download
#save(CPSdata, file = "Data/CPSdata.RData")

load("Data/CPSdata.RData")


emplyNW = CPSdata[CPSdata$series_id %in% c("LNS11000000", #adjusted labor force
                                           "LNS12000000", #adjusted employment
                                           "LNS13000000", #adjusted unemployment
                                           "LNS14000000", #adjusted unemployment rate
                                           "LNU01000000", #unadjusted labor force
                                           "LNU02000000", #unadjusted employment
                                           "LNU03000000", #unadjusted unemployment
                                           "LNU04000000"  #unadjusted unemployment rate
                                           #"LNS13027659",  #adjusted unemployment for less than high school
                                           #"LNS13027660",  #adjusted unemployment for high school no college
                                           #"LNS13027689"   #adjusted unemployment for some college
                                           ),]

emplyNW = transform(emplyNW, 
                  adj = substr(series_id,3,3),
                  State = "NW",
                  Cal.Year = year,
                  Month = as.integer(substr(period,2,3)),
                  value = as.numeric(value),
                  measure = as.factor(paste0(substr(series_id,5,5), substr(series_id,10,11))),
                  #footnote = footnote_codes,
                  series_id = NULL,
                  period = NULL,
                  year = NULL,
                  footnote_codes = NULL
)




#emplyNW$Formatted.Date = with(emplyNW, as.yearmon(paste(Cal.Year,Month,sep = "-")))
emplyNW = emplyNW[emplyNW$Month != 13,] # delete the year average in unajusted series 


# Create variables: Unemployment rate, unemployment, employment, and labor forece
emplyNW = dcast(emplyNW, adj + State + Cal.Year + Month ~ measure)
emplyNW = plyr::rename(emplyNW, c("400" = "uRate", "300" = "unemply", "200" = "emply", "100" = "labor"
                            #"359" = "unemply1", "360" = "unemply2", "389" = "unemply3"
                            ))

emplyNW = transform(emplyNW, labor = labor*1000, emply = emply*1000, unemply = unemply*1000
                             #unemply1 = unemply1*1000, unemply2 = unemply2*1000, unemply3 = unemply3*1000
                    )

emplyAG = emplyNW
emplyA8 = emplyNW
emplyA9 = emplyNW
emplyAG$State = "AG"
emplyA8$State = "A8"
emplyA9$State = "A9"

rm(CPSdata)


# Merge the state level and national level data ---------------------------

  #emply = rbind(emply,emplyNW, emplyAG, emplyA8, emplyA9)
# emply %>% head()
# emplyNW %>% head
# 
# emply1 = bind_rows(emply,emplyNW, emplyAG, emplyA8, emplyA9)
# 
# emply1 = melt(emply1, id = c("adj", "State", "Cal.Year","Month"))
# emply2 = dcast(emply1, State + Cal.Year + Month ~ variable + adj) 


emply = bind_rows(emply,emplyNW, emplyAG, emplyA8, emplyA9)

emply = melt(emply, id = c("adj", "State", "Cal.Year","Month"))
emply = dcast(emply, State + Cal.Year + Month ~ variable + adj) 



# Create seasonal factors for each variables
# 
# emply$uRate.factor = with(emply, uRate_U/uRate_S)
# emply$emply.factor = with(emply, emply_U/emply_S)
# emply$unemply.factor = with(emply, unemply_U/unemply_S)
# emply$labor.factor = with(emply, labor_U/labor_S)

emply <-   
  emply %>% 
  dplyr::select(-starts_with("07"), 
                -starts_with("08"),
                -starts_with("09"))

# Save the data into emply.RData
save(emply, file = "Data/emply.RData")

rm(emplyNW,emplyS,emplyU,emplyAG, emplyA8, emplyA9)

