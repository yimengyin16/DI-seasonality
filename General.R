# This file contains functions and vectors that could be frequenctly used in 
# other codes. 


#path_proj <- "C:/Dropbox/AB_Projects_Personal/Third_year"
#path_work <- "C:/Dropbox/AB_Projects_Personal/Third_year/00_R_programs/02_Seasonal_Adjustment/New"
#setwd(path_work)


#getOption("scipen")
options(scipen=999)
options(digits = 4)

# Loading packages --------------------------------------------------------


library(forecast)
library(tseries)
library(dynlm)
library(zoo)
library(uroot)
#library(tsDyn)
#library(TSA)

#library(vars)
library(sandwich)
library(AER)
library(car)
library(plm)
#library(nlme)
#library(splm)
#library(spdep)
#library(systemfit)
#library(Matrix)


library(stringr)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)

library(DataCombine)
library(dummies)
#library(foreign)
#library(maps)

library(ggplot2)
library(RColorBrewer)
library(lattice)
#library(knitr)
#library(texreg)
library(xtable)
library(gt)
library(broom)


library(doParallel)
library(foreach)
#library(doSNOW)

# detach("package:TSA", unload=TRUE)
# detach("package:dplyr", unload=TRUE)


# User defined functions-------------------------------------------------------


select = function(category, region, data, start = c(2000, 10),end = c(2020,2), zoo = FALSE,
                  na.rm = FALSE, sea.rm = FALSE){
# A function for selecting a particular seires.
# "zoo" is required. 

#Input:
  # category: One of Total, Title.2, Title.16, Concurrent, DI, SSI.
  # region: state or region abbreviation.
  # data: dataframe containing the series.
  # start, end: starting and ending date
#Output:
  # A time series of the class "ts" or "zoo"/"zooreg"

  if(zoo){ require("zoo")
           series = zooreg(data[category][data$State == region,], start = start, end = end, f = 12)
           names(series) = paste(category, region)}
  else{series = ts(data[data$State == region, category], start = start, end = end, f = 12)
       names(series) = paste(category, region)}
  
  if(na.rm) series = na.omit(series)
  
  
  return(series)
}



select2 = function( region, data = reg.data, start = c(2000, 10),end = c(2018,12)){
  # A function for selecting a particular state in a data frame.
  # "zoo" is required. 
  
  #Input:
  # region: state or region abbreviation.
  # data: dataframe containing the series.
  # start, end: starting and ending date
  #Output:
  # A time series of the class "zoo"/"zooreg"
  
  series = data[data$State == region & levels(data$Cal.Year)[data$Cal.Year] <= end[1],]
  exclude = c("State","Cal.Year", "Month","LMonth","L2Month","L3Month", "Region", "Date", "Formatted.Date")
  
  for (i in names(series)[!names(series) %in% exclude])
  series[,i] = zooreg(series[,i], start = start, end = end, f = 12)
  series = series[order(series$Cal.Year,series$Month),]
  series
}


plotSeries = function( category, region, data = all.same, mline = TRUE, yline = TRUE, ...){  
# A function for drawing a series from a selected dataframe 
# The selection of series is done by calling function "select".
# This function does not allow user to specify starting and ending date of the series.
  
# Input
  # category: One of Total, Title.2, Title.16, Concurrent, DI, SSI.
  # region: state or region abbreviation.
  # data: dataframe containing the series.
  # mline: Month lines are added if TRUE.
  # yline: Year lines are added if TRUE.
  # ... :Additional parameters passed to function "plot".
  
# Output
  # Graph of of the two series   
  
  series = select(category, region, data = data)
  
  plot(series, type="l",lwd = 2,
       main=paste(region,category), 
       xlab="", ylab="Number of Applications", xaxt="n", ... )
  axis(1, xaxp = c(2001,2013,12))
  
  if(mline) {
    a = c(1,3,5,7,8,10,12)/12
    b = rep(2001:2013,each = 7)
    c = a+b    
    d = c(2000+c(10,12)/12,c)-1/12
    abline(v = d, col="grey")
    
    e = c(2,4,6,9,11)/12
    b = rep(2001:2013,each = 5)
    f = e+b
    g = c(2000+11/12,f)-1/12
    abline(v = g, col = "grey", lty = "dotted")
  }
  
  if(yline) abline (v = 2001:2013, lty = 2)
  
}

plotSeries2 = function( category, region, data1 = all0, data2 = all.same, mline = TRUE, yline = TRUE, ...){  
# A function for drawing two series from different dataframes on the same graph. 
# The selection of series is done by calling function "select".
# This function does not allow user to specify starting and ending date of the series.
  
# Input
  # category: One of Total, Title.2, Title.16, Concurrent, DI, SSI.
  # region: state or region abbreviation.
  # data1: dataframe containing the first series.
  # data2: dataframe containing the second series.
  # mline: Month lines are added if TRUE.
  # yline: Year lines are added if TRUE.
  # ... :Additional parameters passed to function "plot".
  
# Output
  # Graph of of the two series   
  
  series1 = select(category, region, data = data1)
  series2 = select(category, region, data = data2)
  
  plot(series1, type="l",lwd = 2,col="blue",lty = "dotted",
       main=paste(region,category,"Original + Adjusted.same"), 
       xlab="", ylab="Number of Applications", xaxt="n", ... )
  lines(series2 ,type="l",lwd = 2, col="black")
  axis(1, xaxp = c(2001,2013,12))
  
  if(mline) {
  a = c(1,3,5,7,8,10,12)/12
  b = rep(2001:2013,each = 7)
  c = a+b    
  d = c(2000+c(10,12)/12,c)-1/12
  abline(v = d, col="grey")
  
  e = c(2,4,6,9,11)/12
  b = rep(2001:2013,each = 5)
  f = e+b
  g = c(2000+11/12,f)-1/12
  abline(v = g, col = "grey", lty = "dotted")
  }
  
  if(yline) abline (v = 2001:2013, lty = 2)
  
}

plotSpec = function(category, region, data = all.same, Diff = TRUE, spans = c(4,4), lwd = 2, ...){

# A function for drawing estimated spectrum of a selected series. 
# The selection of series is done by calling function "select".
# This function does not allow user to specify starting and ending date of the series.

# Input
  # category: One of Total, Title.2, Title.16, Concurrent, DI, SSI.
  # region: state or region abbreviation.
  # data: dataframe containing the series.
  # Diff: Logical, whether the series is first differenced to suppress the trend frequency
  # spans: parameter of "spec" for smoothing.
  # lwd: line width
  # ... :Additional parameters passed to function "spec".
  
# Output
  # Graph of the estimated 
    
  series = select(category, region, data = data)
  if (Diff) series = diff(series)
  spec(series, spans = spans , lwd = lwd, 
       xlab = "Frequency", ylab = "Estimated Spectrum", sub = "",
       main = paste("Estimated Spectrum of",category,"in", region), ...)
  abline(v = 1:6/12, lty = 3, col = "blue")
}

plotSpec1 = function(category, region, data = all.same, Diff = TRUE, spans = c(4,4), lwd = 2, ...){
  
  # A function for drawing estimated spectrum of a selected series. Main title can be specified by user.
  # The selection of series is done by calling function "select".
  # This function does not allow user to specify starting and ending date of the series.
  
  # Input
  # category: One of Total, Title.2, Title.16, Concurrent, DI, SSI.
  # region: state or region abbreviation.
  # data: dataframe containing the series.
  # Diff: Logical, whether the series is first differenced to suppress the trend frequency
  # spans: parameter of "spec" for smoothing.
  # lwd: line width
  # ... :Additional parameters passed to function "spec".
  
  # Output
  # Graph of the estimated 
  
  series = select(category, region, data = data)
  if (Diff) series = diff(series)
  spec.pgram(series, spans = spans , lwd = lwd, 
       xlab = "Frequency", ylab = "Estimated Spectrum", sub = "",
        ...)
  abline(v = 1:6, lty = 3)
  # abline(v = 1:6/12, lty = 3, col = "blue")
}

plotDummy2 = function( category1, category2, region, data = reg.data){
# This function draw graph for the estimated coefficients of seasonal dummy 
# variables of two series, and compare them in the same graph.
  
  model1 = seaDummy(category1,region, data = data)
  model2 = seaDummy(category2,region, data = data)
  
  path = "E:\\SkyDrive\\Working\\Third year\\X13output.graph\\CompareCoefficients\\"
  pdf(file = paste0(path,region,"-CompareCoeff.pdf"), 
      width = 9 , heigh = 5)
  plot(model1$model$coefficients, type = "l", lty = 2, lwd = 2, col = "blue",
       xlab = "", ylab = "coefficients", ylim = c(-0.15,0.2),
       xaxt="n", cex = 1, main = region)
  lines(model2$model$coefficients, type = "l",lty = 2, lwd = 2, col = "red", cex = 1)
  legend("topleft",c(category1, category2), col = c("blue","red"),lwd = 2,bg = "white")
  abline(h = 0, lty = 3)
  axis(1, xaxp = c(1,12,11))
  dev.off() 
  
}

# used in paper graphs.R

seaDummy = function(category,region, data = reg.data, dif = TRUE, ...){
# This function runs OLS regression of a selected series on seasonal dummies
# using "lm" function. The coefficients are then tested using NeweWest autocorrelation
# robust standard errors. The test result and adjusted R squared are reported.

# Input
  # dif: whether y is first differenced, defult is "TRue". 
  
# Output: A list with 4 elements
  # Model: The regression model of the class lm
  # test: The test result with NewyWest standard errors.
  # R2: R squared
  # R2adj: adjusted R squared. 
  
  df = select2(region, data = data, ...)
  y =  df[,category]
  x = df$Month
  
  if(dif)
    m = lm(diff(y) ~ x[-1] -1)
    else m = lm( y ~ x )
  test = coeftest(m, vcov = NeweyWest)
  result = list(model = m, test = test, R2 = summary(m)$r.squared, R2adj = summary(m)$adj.r.squared)
  #print(paste(category,region,"------------------------------------------------------------"))
  #print(result)
  #return(result)
  invisible(result)
}

transMA1 = function(theta, M = NULL, dim = NULL,  C.only = TRUE){
# This function implement the transformation described in Balestra(1980) and 
# Baltagi(2008 p88)

# Input:
  # theta: parameter of MA(1) precess
  # M: the data matrix to be transformed. 
  # C.only: if TRUE, only the transforming matrix is returned.
# Output: C.only = FALSE: Transformed matrix, adjusted for MA(1) in error terms.
  t = ifelse(!is.null(M), nrow(M), dim)

  # Create a vector containing a_t = 1 + theta^2 + ... + theta^(2t), a_0 = 1
  # so we have at = a_t[t+1]
  a_t = numeric(t+1);
  names(a_t) = paste0("a",0:t)
    A = numeric(t+1) # ith element is theta^[i-1]
    for (i in 1:(t+1)) A[i] = theta^(2*(i-1))
  for (i in 1:length(a_t)) a_t[i] = sum(A[1:i])
  
  
  # Create matrix D
    # create the diagonal elements of D in vector "d"
    d = numeric(t)
    for (i in 1:length(d)) d[i] = a_t[i]*a_t[i+1]
      # D = diag(d)
    
  
  # Create matrix P
   # P can be computed as element by element product of two t*t matrices.
    P1 = matrix(0, t, t)
    P2 = matrix(0, t, t)
    
    for (i in 1:ncol(P1)) P1[i:nrow(P1), i] = A[1:(nrow(P1) -i +1)]
    for (i in 1:ncol(P2)) P2[i:nrow(P2), i] = a_t[i]
  
    P = P1*P2
  
  # Create C = D^(-1/2)P
    C = diag((1/d)^0.5)%*%P
  
  if(C.only) output = C
    else output = C%*%M  # Adjust data matrix 
   
  invisible(output)  
}

transMA1Panel = function(theta, N, t, theta.same = TRUE){
# This function create MA1 transforming matrix for a panel data. 
# The transforming matrix is a block diagonal matrix with the blocks produced by
# function "transMA1". The MA1 parameters for each individual in the panel are allowed
# to be different by the parameter theta.same.

# input:
  # theta: The MA1 parameter. scaler when theta.same is TRUE; vecter when theta.same is FALSE.
  # N: Number of individual in the panel, only used when theta.same = TRUE.
  # t: Number of observations for each individual.
  # theta.same: Logical. Whether the MA1 parameters are different across individuals.

# Output: Block diagonal transforming matrix for panel data of NT by NT. 

  require(plyr)
  
  if(theta.same) {
                 C = transMA1(theta, dim = t)
                 C.panel = diag(N) %x% C 
                 }
                 
                 else{ if(length(theta != N)) warning("N is overrided")   
                 require(Matrix, quietly = TRUE)
                 C.list = alply(theta, 1, function(x) transMA1(x, dim = t)) # require "plyr" 
                 C.panel = as.matrix(bdiag(C.list))
                 }
  invisible(C.panel)  
}

# Compute M^(-1/2), M is a positive semi-definite matrix.

matrix.ginv.sqrt = function(x){
  x = ginv(x)
  eig <- eigen(x)
  sqrt <- eig$vectors %*% diag(sqrt(eig$values)) %*% solve(eig$vectors)
}



Prewhiten = function (x, y, x.model = ar.res, ylab = "CCF", ...) {
  filter.mod = function(x, model) {
    if (length(model$Delta) >= 1) 
      x = stats::filter(x, filter = c(1, -model$Delta), method = "convolution", 
                 sides = 1)
    if (length(model$phi) >= 1 && all(model$phi != 0)) 
      x = stats::filter(na.omit(x), filter = c(1, -model$phi), method = "convolution", 
                 sides = 1)
    if (length(model$theta) >= 1 && all(model$theta != 0)) 
      x = stats::filter(na.omit(x), filter = -model$theta, method = "recursive", 
                 sides = 1)
    x
  }
  if (!missing(x.model)) {
    x = filter.mod(x, model = x.model$model)
    y = filter.mod(y, model = x.model$model)
  }
  else {
    ar.res = ar.ols(x, ...)
    x = stats::filter(x, filter = c(1, -ar.res$ar), method = "convolution", 
               sides = 1)
    y = stats::filter(y, filter = c(1, -ar.res$ar), method = "convolution", 
               sides = 1)
  }
  ccf.xy = ccf(x = x, y = y, na.action = na.omit, ylab = ylab, 
               ...)
  invisible(list(ccf = ccf.xy, model = x.model))
}


Filter.mod = function(x, model) {
      if (length(model$Delta) >= 1) 
          x = filter(x, filter = c(1, -model$Delta), method = "convolution", 
                                        sides = 1)
      if (length(model$phi) >= 1 && all(model$phi != 0)) 
          x = filter(na.omit(x), filter = c(1, -model$phi), method = "convolution", 
                                         sides = 1)
     if (length(model$theta) >= 1 && all(model$theta != 0)) 
          x = filter(na.omit(x), filter = -model$theta, method = "recursive", 
                                         sides = 1)
       x
     }


tsDiag = function(res, lag.max = 16){

rm.TSA = FALSE
if("package:TSA" %in% search()){detach("package:TSA", unload=TRUE); rm.TSA = TRUE}

# Setting Layout and parameters
layout(matrix(c(1,1,2,3, 4, 5), 3, 2, byrow = TRUE), 
       widths=c(1,1), heights=c(1,1,1))
par(mar = c(4,4,3,1)+ 0.1)

# Standardized residual
#res = residuals(fit)
sd.res = res/var(res)^0.5
plot(as.numeric(sd.res), type = "p", ylab = "Standardized Residuals", xlab = "")
abline(h = c(1.96, - 1.96), lty = 2, col = "red")

# ACF and PACF
Acf(res, main = "")
Pacf(res, main = "")


# QQ plot
qqnorm(res);qqline(res, col = "red")
jbtest = jarque.bera.test(res)

# Ljung-Box Test 
lag.max = 16

Box.p = numeric(lag.max)
for(i in 1:lag.max){
  Box.p[i] = Box.test(res, lag = i,  type= "Ljung-Box")$p.value
}
plot(1:lag.max, Box.p, ylim = c(0, min(1,max(Box.p) + 0.05)), xlab = "", ylab = "p-Value of Ljung-Box Test")
abline(h = 0.05, col = "red", lty = 2)

par(mfrow = c(1,1))
if(rm.TSA) require(TSA)
print(jbtest)

}


lagpad <- function(x, k=1) {
  # does not work with ts objects.
  i<-is.vector(x)
  if(is.vector(x)) x<-matrix(x) else x<-matrix(x,nrow(x))
  if(k>0) {
    x <- rbind(matrix(rep(NA, k*ncol(x)),ncol=ncol(x)), matrix(x[1:(nrow(x)-k),], ncol=ncol(x)))
  }
  else {
    x <- rbind(matrix(x[(-k+1):(nrow(x)),], ncol=ncol(x)),matrix(rep(NA, -k*ncol(x)),ncol=ncol(x)))
  }
  if(i) x[1:length(x)] else x
}


seasonDummy = function(ts){
  # This function runs OLS regression of a selected series on seasonal dummies
  # using "lm" function. The coefficients are then tested using NeweWest autocorrelation
  # robust standard errors. The test result and adjusted R squared are reported.
  
  # Input
  # ts: A time series (ts) object. 
  
  # Output: A list with 4 elements
  # Model: The regression model of the class lm
  # test: The test result with NewyWest standard errors.
  # R2: R squared
  # R2adj: adjusted R squared. 
  
  month = factor(cycle(ts))
  m = lm( ts ~ month - 1 )
  test = coeftest(m, vcov = NeweyWest)
  
  result = list(model = m, test = test, R2 = summary(m)$r.squared, R2adj = summary(m)$adj.r.squared)
  invisible(result)
}




# seaDummy("lunemply_U","NW", end = c(2007,12))
# seaDummy("lunemply_U","NW", end = c(2013,1))
# seaDummy("lTitle.2","NW", end = c(2007,12))
# seaDummy("lDI","NW", end = c(2013,1))
# With the period affected by recession, coefficient of Aug becomes smaller, weired.
# 
# plot.ts(ts(emply$unemply.factor[emply$State == "NW"],s = c(1948,1),f = 12))


# Variables ---------------------------------------------------------------

# including AG and DC
states52 = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
             "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", 
             "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", 
             "NJ", "NM", "NV", "AG", "NY", "OH", "OK", "OR", "PA", "RI", 
             "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

# Only states and DC, no NW
statesAll = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
              "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", 
              "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", 
              "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", 
              "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

# including AG, but no DC
states51 = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA",
             "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", 
             "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", 
             "NJ", "NM", "NV", "AG", "NY", "OH", "OK", "OR", "PA", "RI", 
             "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

# withou NW and DC
states50 = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA",
             "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", 
             "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", 
             "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", 
             "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

# 48 continental states, AK and HI excluded and DC excluded. 

states48 = c("AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA",
             "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", 
             "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", 
             "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", 
             "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

# 49 continental states, AK and HI excluded and DC included. 

states49 = c("AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
             "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", 
             "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", 
             "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", 
             "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")


# states.usaww

states.usaww = c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
                 "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
                 "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
                 "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
                 "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")




top5 = c("NW","CA","TX","FL","NY","PA")
top10 = c("CA", "TX", "FL", "NY", "PA", "OH", "MI", "IL", "NC", "GA")

STATE51 = c(
  "alaska",
  "alabama",
  "arkansas",
  "arizona",
  "california",
  "colorado",
  "connecticut",
  "delaware",
  "florida",
  "georgia",
  "hawaii",
  "iowa",
  "idaho",
  "illinois",
  "indiana",
  "kansas",
  "kentucky",
  "louisiana",
  "massachusetts",
  "maryland",
  "maine",
  "michigan",
  "minnesota",
  "missouri",
  "mississippi",
  "montana",
  "north carolina",
  "north dakota",
  "nebraska",
  "new hampshire",
  "new jersey",
  "new mexico",
  "nevada",
  "nation",
  "new york",
  "ohio",
  "oklahoma",
  "oregon",
  "pennsylvania",
  "rhode island",
  "south carolina",
  "south dakota",
  "tennessee",
  "texas",
  "utah",
  "virginia",
  "vermont",
  "washington",
  "wisconsin",
  "west virginia",
  "wyoming"
) 


## Spatial weighting Matrix for 48 states and 48 states+DC
data("usaww", package = "splm");
usaw = usaww; rm(usaww)
rownames(usaw) <- states.usaww -> colnames(usaw) # Change row and column names
usaw = usaw[states48,] # adjust the row order of the matrix. 


# dim(usaw)


# Month names
monthName = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


regionNames <- c("SEA", "ATL", "DAL", "SFO", "DEN", "BOS", "PHL",  "KCM",  "CHI",  "NYC")

agencyNames <- c("EA", "EM", "EO", "EV", "FE")




