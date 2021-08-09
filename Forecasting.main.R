### This file contains global settings for forecasting models and frequently used user-defined functions 
### for forcasting evaluation. 


### 0. Loading data 

load("Panel.RData")

### 1. Global settings---------------------
AGGREGATE = "A8"
STATES = states48

VARNAME = "Total"

lVARNAME    = paste0("l" , VARNAME)
dlVARNAME   = paste0("dl", VARNAME)
LdlVARNAME  = paste0("Ldl", VARNAME)
L2dlVARNAME = paste0("L2dl", VARNAME)
L3dlVARNAME = paste0("L3dl", VARNAME)
LlVARNAME   = paste0("Ll", VARNAME)

LINAME  = c("Ldlunemply_S")

train.START = 1  # 2000:12

train.END   = 75  # 2006:12
test.START  = 76  # 2007:1

max.INDEX = max(Panel$index)


### 2. Forecasting leading indicator.----------------------

# Uncomment this piece of code when reconstruction of the leading indicator is needed. 
# 
# data.unemply_U = subset(Panel, State %in% c(AGGREGATE, STATES), select = c("index", "State", "Ldlunemply_U", "dlunemply_U"))
# data.unemply_S = subset(Panel, State %in% c(AGGREGATE, STATES), select = c("index", "State", "Ldlunemply_S", "dlunemply_S"))
# 
# data.unemply_U[,c("H1dlunemply_U", "H2dlunemply_U","H3dlunemply_U", "H4dlunemply_U","H5dlunemply_U")] = NA
# data.unemply_S[,c("H1dlunemply_S", "H2dlunemply_S","H3dlunemply_S", "H4dlunemply_S","H5dlunemply_S")] = NA
# 
# 
# for(i in c(AGGREGATE, STATES)){
#   for(j in (train.END - 20):max.INDEX){
#     data.unemply_U[data.unemply_U$State == i & data.unemply_U$index == j, c("H1dlunemply_U", "H2dlunemply_U","H3dlunemply_U", "H4dlunemply_U","H5dlunemply_U")] = 
#       forecast(auto.arima(ts(subset(data.unemply_U, State == i & index<= j, "dlunemply_U"), f = 12), seasonal = TRUE, d= 0 , D= 0), 5)$mean
#     print(paste(i,j))
#   }
# }
# 
# for(i in c(AGGREGATE, STATES)){
#   for(j in (train.END - 20):max.INDEX){
#     data.unemply_S[data.unemply_S$State == i & data.unemply_S$index == j, c("H1dlunemply_S", "H2dlunemply_S","H3dlunemply_S", "H4dlunemply_S","H5dlunemply_S")] = 
#       forecast(auto.arima(ts(subset(data.unemply_S, State == i & index<= j, "dlunemply_S"), f = 12), seasonal = FALSE, d= 0 , D= 0), 5)$mean
#     print(paste(i,j))
#   }
# }
# save(data.unemply_U, data.unemply_S, file = "fcast.unemply.RData")

 load("fcast.unemply.RData")


### 3. Functions---------------

## Functins to compute forecast accuracy. 

fcastAcc.AG = function(Test.data = Test.AG, start = min(Test.data$index), end = max(Test.data$index), panel = FALSE){
  ## This function compute measures of forecast accuracy of national level forecasts.
  ## Input: 
  # Test.data: data frame containing ture(ture) and forecasted series(fcasth?, fcasth?a, ? = 1,3,6), - 
  #            the default value is "Test.AG" data frame from the forecast step. 
  # start    : index from which the measures are calculated. default is the min index.
  # end      : index to which the measures are calculated.   default is the max index. 
  ## Output: a matrix containing the measures.
  
  
  Test.data = subset(Test.data, index>=start & index <= end)
  
  if(!panel){
  measure.AG = rbind(with(Test.data, accuracy(fcasth1, true)),
                     with(Test.data, accuracy(fcasth1a, true)),
                     
                     with(Test.data, accuracy(fcasth3, true)),
                     with(Test.data, accuracy(fcasth3a, true)),
                     
                     with(Test.data, accuracy(fcasth6, true)),
                     with(Test.data, accuracy(fcasth6a, true)) 
  ) 
  rownames(measure.AG) = c("Directh1", "Aggh1", "Directh3", "Aggh3", "Directh6", "Aggh6")
  #print(paste(start, "-", end))
  }
  
  if(panel){
    measure.AG = rbind(with(Test.data, accuracy(fcasth1a, true)),
                      
                       with(Test.data, accuracy(fcasth3a, true)),
                       
                       with(Test.data, accuracy(fcasth6a, true)) 
    ) 
    rownames(measure.AG) = c("Aggh1", "Aggh3", "Aggh6")}
  
  attr(measure.AG, "span") = paste(paste(start, "-", end))
  measure.AG  
}

fcastAcc.state = function(Test.data = Test.state, start = min(Test.data$index), end = max(Test.data$index)){
  ## This function compute measures of forecast accuracy of state level forecasts.
  ## Input: 
  # Test.data: data frame containing ture(ture) and forecasted series(fcasth?, fcasth?a, ? = 1,3,6), - 
  #            the default value is "Test.state" data frame from the forecast step. 
  # start    : index from which the measures are calculated. default is the min index.
  # end      : index to which the measures are calculated.   default is the max index. 
  ## Output: A matrix containing the measures.
  
  Test.data = subset(Test.data, index>=start & index <= end)
  STATES = levels(factor(Test.data$State))
  
  
  measure.stateh1 = data.frame(State = STATES, RMSE = NA, MAPE = NA)
  for(i in STATES)
    measure.stateh1[measure.stateh1$State == i, c("RMSE", "MAPE")] = with(Test.data[Test.data$State == i,], accuracy(fcasth1, true))[,c("RMSE", "MAPE")] 
  
  
  measure.stateh3 = data.frame(State = STATES, RMSE = NA, MAPE = NA)
  for(i in STATES)
    measure.stateh3[measure.stateh3$State == i, c("RMSE", "MAPE")] = with(Test.data[Test.data$State == i,], accuracy(fcasth3, true))[,c("RMSE", "MAPE")] 
  
  
  measure.stateh6 = data.frame(State = STATES, RMSE = NA, MAPE = NA)
  for(i in STATES)
    measure.stateh6[measure.stateh6$State == i, c("RMSE", "MAPE")] = with(Test.data[Test.data$State == i,], accuracy(fcasth6, true))[,c("RMSE", "MAPE")] 
  
  measure.state = cbind(measure.stateh1, measure.stateh3[-1], measure.stateh6[-1])
  names(measure.state)[-1] = c("RMSEh1", "MAPEh1", "RMSEh3", "MAPEh3", "RMSEh6", "MAPEh6")
  attr(measure.state, "span") = paste(start, "-", end)
  measure.state
}

fcastAcc.sum = function(Test.data = Test.state, start = min(Test.data$index), end = max(Test.data$index)){
  ## This function compute OVERALL measures of forecast accuracy of state level forecasts.
  ## Input: 
  # Test.data: data frame containing ture(ture) and forecasted series(fcasth?, fcasth?a, ? = 1,3,6), - 
  #            the default value is "Test.state" data frame from the forecast step. 
  # start    : index from which the measures are calculated. default is the min index.
  # end      : index to which the measures are calculated.   default is the max index. 
  ## Output: A data.frame containing the measures.
  
  Test.data = subset(Test.data, index>=start & index <= end)
  
  measure.state = fcastAcc.state(Test.data, start, end)
  
  measure.sumh1 = c(RMSE.All = sqrt(mean((Test.data$true - Test.data$fcasth1)^2)),  # RMSE with all states
                    MAPE.All = mean(abs(Test.data$true - Test.data$fcasth1)/abs(Test.data$true))*100, # MAPE with all states
                    RMSE.Md  = median(measure.state$RMSEh1),
                    RMSE.Max = max(measure.state$RMSEh1),
                    RMSE.Min = min(measure.state$RMSEh1),
                    MAPE.Md  = median(measure.state$MAPEh1),
                    MAPE.Max = max(measure.state$MAPEh1),
                    MAPE.Min = min(measure.state$MAPEh1))
  
  measure.sumh3 = c(RMSE.All = sqrt(mean((Test.data$true - Test.data$fcasth3)^2, na.rm = TRUE)),  # RMSE with all states
                    MAPE.All = mean(abs(Test.data$true - Test.data$fcasth3)/abs(Test.data$true), na.rm = TRUE)*100, # MAPE with all states
                    RMSE.Md  = median(measure.state$RMSEh3),
                    RMSE.Max = max(measure.state$RMSEh3),
                    RMSE.Min = min(measure.state$RMSEh3),
                    MAPE.Md  = median(measure.state$MAPEh3),
                    MAPE.Max = max(measure.state$MAPEh3),
                    MAPE.Min = min(measure.state$MAPEh3))
  
  measure.sumh6 = c(RMSE.All = sqrt(mean((Test.data$true - Test.data$fcasth6)^2, na.rm = TRUE)),  # RMSE with all states
                    MAPE.All = mean(abs(Test.data$true - Test.data$fcasth6)/abs(Test.data$true), na.rm = TRUE)*100, # MAPE with all states
                    RMSE.Md  = median(measure.state$RMSEh6),
                    RMSE.Max = max(measure.state$RMSEh6),
                    RMSE.Min = min(measure.state$RMSEh6),
                    MAPE.Md  = median(measure.state$MAPEh6),
                    MAPE.Max = max(measure.state$MAPEh6),
                    MAPE.Min = min(measure.state$MAPEh6))
  
  
  
  measure.sum   = rbind(measure.sumh1, measure.sumh3, measure.sumh6)
  rownames(measure.sum) = c("Overallh1", "Overallh3", "Overallh6")
  attr(measure.sum, "span") = paste(start, "-", end)
  
  measure.sum
}



# Function for plotting national level forecasts
plot.AG = function(data, h, model = model.name, colors = c("gray40", "blue", "red"), 
                   shape = c(21,20, 18), Lshape = c("dotted","dotted", "dotted"), panel = FALSE){
  ## Data must contain index, true, fcast1,3,6. 
  ## The input must be the data frame Test.AG
  
  #data = Test.AG;  h =1; model.name = model.name; colors = c("gray40", "blue", "red"); 
  #shape = c(21,20, 19); Lshape = c("dotted","dotted", "dotted")
  
  if(!panel){
  data = na.omit(data[,c("index", "true", paste0("fcasth", h), paste0("fcasth", h, "a"))]) 
  
  if(is.factor(data$index))  start.index = min(as.numeric(levels(data$index)[data$index])) else 
    if(is.numeric(data$index)) start.index = min(data$index)
  
  start.Year  = Panel[Panel$State == Aggregate & Panel$index == start.index, "Cal.Year"]
  start.Month = Panel[Panel$State == Aggregate & Panel$index == start.index, "Month"]
  
  true   = ts(data[, "true"],                   start = c(start.Year, start.Month), f = 12)
  fcast  = ts(data[, paste0("fcasth", h)],      start = c(start.Year, start.Month), f = 12)
  fcasta = ts(data[, paste0("fcasth", h, "a")], start = c(start.Year, start.Month), f = 12)
  
  par(mar = c(4,4,3,1)+ 0.1)
  
  plot(  true, type = "b", lty = Lshape[1], col = colors[1], pch = shape[1], xlab = "", ylab = "",
         main = paste("Forecasting National Level Applications:", model, "horizon =", h)
  )
  lines(fcast, type = "b", lty = Lshape[2], col = colors[2], pch = shape[2])
  lines(fcasta, type = "b", lty = Lshape[3], col = colors[3], pch = shape[3])
  
  #abline(h = 0, , lty = "dotted", col = "grey")
  legend("topleft", c("True", "Direct", "Aggregated"),  lty = Lshape, col = colors, pch = shape)
  }
  
  ####
  if(panel){
    data = na.omit(data[,c("index", "true", paste0("fcasth", h, "a"))]) 
    
    if(is.factor(data$index))  start.index = min(as.numeric(levels(data$index)[data$index])) else 
      if(is.numeric(data$index)) start.index = min(data$index)
    
    start.Year  = Panel[Panel$State == Aggregate & Panel$index == start.index, "Cal.Year"]
    start.Month = Panel[Panel$State == Aggregate & Panel$index == start.index, "Month"]
    
    true   = ts(data[, "true"],                   start = c(start.Year, start.Month), f = 12)
    fcasta = ts(data[, paste0("fcasth", h, "a")], start = c(start.Year, start.Month), f = 12)
    
    par(mar = c(4,4,3,1)+ 0.1)
    
    plot(  true, type = "b", lty = Lshape[1], col = colors[1], pch = shape[1], xlab = "", ylab = "",
           main = paste("Forecasting National Level Applications:", model, "horizon =", h)
    )
    lines(fcasta, type = "b", lty = Lshape[3], col = colors[3], pch = shape[3])
    
    #abline(h = 0, , lty = "dotted", col = "grey")
    legend("topleft", c("True", "Aggregated"),  lty = Lshape[c(1,3)], col = colors[c(1,3)], pch = shape[c(1,3)])
  }

}


# Function for plotting state level forecasts
plot.state = function(data, h, state, model = model.name, colors = c("gray40", "blue", "red"), 
                      shape = c(21,20, 18), Lshape = c("dotted","dotted", "dotted")){
  ## Data must contain index, true, fcast1,3,6. 
  ## The input must be the data frame Test.state[Test.state == i, ]
  
  data = na.omit(data[,c("index", "true", paste0("fcasth", h))]) 
  
  if(is.factor(data$index))  start.index = min(as.numeric(levels(data$index)[data$index])) else 
    if(is.numeric(data$index)) start.index = min(data$index)
  
  start.Year  = Panel[Panel$State == Aggregate & Panel$index == start.index, "Cal.Year"]
  start.Month = Panel[Panel$State == Aggregate & Panel$index == start.index, "Month"]
  
  
  true   = ts(data[, "true"],                   start = c(start.Year, start.Month), f = 12)
  fcast  = ts(data[, paste0("fcasth", h)],      start = c(start.Year, start.Month), f = 12)
  
  par(mar = c(4,4,3,1)+ 0.1)
  
  plot(  true, type = "b", lty = Lshape[1], col = colors[1], pch = shape[1], xlab = "", ylab = "",
         main = paste("Forecasting State Level Applications:", model, "horizon =", h, state))
  lines(fcast, type = "b", lty = Lshape[2], col = colors[2], pch = shape[2])
  
  #abline(h = 0, , lty = "dotted", col = "grey")
  legend("topleft", c("True", "Forecast"),  lty = Lshape, col = colors, pch = shape)
}



## Defining conversion functions between index and date



i2d = function(i, i.min = 1, i.max = max.INDEX, start = c(2000,10), f = 12){
  t = ts(i.min:max.INDEX, start = c(2000,10), f = 12)
  c(trunc(time(t))[t == i],cycle(t)[t == i])
}


d2i = function(y,m, i.min = 1, i.max = max.INDEX, start = c(2000,10), f = 12){
  t = ts(i.min:max.INDEX, start = c(2000,10), f = 12)
  as.numeric(window(t, start = c(y,m), end = c(y,m)))
  }









