# This program employ Pasaran and Yamataga(2008) to test for heterogeneity in seasonal dummies.
# This is a simplified version of PY test since all FE are reduced to pooled OLS since there is no 
# single intercept in the regression. 
# In order to complare the seasonal pattern, not the average growth rate across states, the dependent 
# variable is the demeanded log difference applications. 


library(plm)
library(plyr)
library(dummies)
library(Matrix)


# 1. Functions for building the Swamy(1970) Sh(S^hat) and St(S^tilde) statistics. 
# 1.1 Getting the state specific OLS with demeaned dependent variables, and state specific variance (sigmaHat)

getOLS = function(depVar, indepVar, data, state){
  ## Inputs: 
  # The dependent and independent variables
  # Data frame to be used.
  # States to be compared:state
  # 
  ##Outputs:
  # a list:
  # $beta: A matrix, each column contains the OLS coefficients of a state
  # $sigma: A vector, estimated standard deviation of error term of each state.  
  
#   data = PanelDummy
#   state = statesAll
#   depVar = "dlTitle.2"
#   indepVar = paste0("Month",1:12)
  
  fml = as.formula(paste(depVar, "~", paste(indepVar, collapse = " + "), "-1"))
  
  
  ## Select subset of data
  Data = subset(data, State %in% c(state), select = c("index", "State",  depVar, indepVar))
  Data = na.omit(Data)
  Data = pdata.frame(Data, index = c("State","index"))
  
  n = nrow(Data[Data$State == state[1],]) # length of time
  k = length(indepVar)                    # Number of variables
  
  
  ## Create the demeaned data.
  Data.demean = Data
  Data.demean[,c(depVar)] =  Within(Data[[depVar]])
  Data.demean
  
  
  ## Obtain coefficients and variance
  beta.i = aaply(state, 1, function(x) lm(fml, data = Data.demean[Data.demean$State == x,])$coef)
  sigma.i = aaply(state, 1, function(x) summary(lm(fml, data = Data.demean[Data.demean$State == x,]))$sigma)
  
  rownames(beta.i) = state
  names(sigma.i) = state
  
  list(beta = beta.i, sigma = sigma.i)  
} 

getPOLS = function(depVar, indepVar, data, state){
  ## Inputs: 
  # The dependent and independent variables
  # Data frame to be used.
  # States to be compared:state
  # 
  ##Outputs:
  # a list:
  # $beta: A matrix, each column contains the OLS coefficients of a state
  # $sigma: A vector, estimated standard deviation of error term of each state.  
  
  
  fml = as.formula(paste(depVar, "~", paste(indepVar, collapse = " + "), "-1"))
  
  ## Select subset of data
  Data = subset(data, State %in% c(state), select = c("index", "State",  depVar, indepVar))
  Data = na.omit(Data)
  Data = pdata.frame(Data, index = c("State","index"))
  
  n = nrow(Data[Data$State == state[1],]) # length of time
  k = length(indepVar)                    # Number of variables
  
  
  ## Create the demeaned data.
  Data.demean = Data
  Data.demean[,c(depVar)] =  Within(Data[[depVar]])
  Data.demean
  
  ## Obtain coefficients and variance
  fit = lm(fml, data = Data.demean)
  beta.i = fit$coef
  
  e = residuals(fit)
  sigma.i = aaply(state, 1, function(x){r = e[Data.demean$State == x]; sqrt(sum(r^2)/(n-1))})
  
  names(sigma.i) = state
  
  list(beta = beta.i, sigma = sigma.i)  
} 

# 1.2 Compute the Weighted FE estimators

getWPOLS = function(depVar, indepVar, data, state, sigma){
  ## Inputs: 
  # The dependent and independent variables
  # Data frame to be used.
  # States to be compared:state
  # sigma: state specific variance. 
  
  ## Outputs:
  # a list:
  # $beta: A matrix, each column contains the OLS coefficients of a state
  
  fml = as.formula(paste(depVar, "~", paste(indepVar, collapse = " + "), "-1"))
  
  ## Select subset of data
  Data = subset(data, State %in% c(state), select = c("index", "State",  depVar, indepVar))
  Data = na.omit(Data)
  Data = pdata.frame(Data, index = c("State","index"))
  
  n = nrow(Data[Data$State == state[1],]) # length of time
  k = length(indepVar)                    # Number of variables
  
  
  ## Create the demeaned data.
  Data.demean = Data
  Data.demean[,c(depVar)] =  Within(Data[[depVar]])
  Data.demean

  ## Create the weighted data.
  Data.W = Data.demean
  for(i in state) Data.W[Data.W$State == i, c(depVar,indepVar)] = Data[Data$State == i, c(depVar,indepVar)]/sigma[i]
  
  beta = lm(fml, data = Data.W)$coef  
}

# 1.3 Compute the X'X/sigma2 for each state and use them to form the block diagonal matrix.
getMeat = function(indepVar,data, state, sigma){
  
  # Inputs: 
  # indepVar: independent variables
  # data:     The serial correlation adjusted dataframes, like reg.data.MA1.Title.2.
  # state:  States to be compared
  # sigma: Estimated sigma
  # Output:
  # A block diagonal matrix to be used to compute Swamy's test statistic.
  
  ## Select subset of data
  Data = subset(data, State %in% c(state), select = c("index", "State", indepVar))
  Data = na.omit(Data)
  Data = pdata.frame(Data, index = c("State","index"))
   
  matrix.list = alply(state, 1,  function(x){
    sigma2 = sigma[names(sigma) == x]^2
    crossprod(as.matrix(Data[Data$State == x, indepVar]))*(1/sigma2)})
  
  meat = as.matrix(bdiag(matrix.list))
}

# 1.4 Compute Swamy's Sh, St, and Peseran and Yamataga(2008)'s Dh and Dt
PYSeason.test = function(depVar, indepVar, data, state){
  # This function utilized the getOLS, getFE, getWFE, and getMeat functions to compute
  # Swamy's staticstic, and the three modified versions from Peseran and Yamataga(2008).
  # Inputs:
  # depVar, indepVar: dependent and independent variables
  # data:   data set.
  # state:  States to be compared
  # Output: a list of two elements
  # $stat: 4 statistics
  # $p   : corresponding p-values
  # All outputs are printed.
  
  # Obtain components. 
  par.OLS = getOLS(depVar = depVar, indepVar = indepVar, data = data, state = state)
  beta.OLS = par.OLS$beta
  sigma.OLS = par.OLS$sigma
  
  par.POLS = getPOLS(depVar = depVar, indepVar = indepVar, data = data, state = state)
  beta.POLS = par.POLS$beta
  sigma.POLS = par.POLS$sigma
  
  beta.WPOLS.h = getWPOLS(depVar = depVar, indepVar = indepVar, data = data, state = state, sigma = sigma.OLS)
  beta.WPOLS.t = getWPOLS(depVar = depVar, indepVar = indepVar, data = data, state = state, sigma = sigma.POLS)
  
  meath = getMeat(indepVar = indepVar, data = data, state = state, sigma = sigma.OLS)
  meatt = getMeat(indepVar = indepVar, data = data, state = state, sigma = sigma.POLS)
  
  breadh = as.vector(t(beta.OLS)) - beta.WPOLS.h
  breadt = as.vector(t(beta.OLS)) - beta.WPOLS.t
  
  # Compute test statist
  
  N = length(state)     # Number of States
  k = length(indepVar)  # Number of variables
  
  Sh = breadh %*% meath %*% breadh
  St = breadt %*% meatt %*% breadt
  Dh = N^(0.5)*(((1/N)*Sh-k)/(2*k)^0.5)
  Dt = N^(0.5)*(((1/N)*St-k)/(2*k)^0.5)
  
  p.Sh = 1 - pchisq(Sh, k*(N-1))
  p.St = 1 - pchisq(St, k*(N-1))
  p.Dh = 1 - pnorm(Dh)
  p.Dt = 1 - pnorm(Dt)
  
  # Outputs
  result = matrix(c(Sh, St, Dh, Dt, p.Sh, p.St, p.Dh, p.Dt), 4,2)
  colnames(result) = c("Statistic", "p-value")
  rownames(result) = c("Sh", "St", "Dh", "Dt")
  print(result)
  
  output = data.frame(Z = result[,1], p.value = result[,2])
  
  invisible(output)
} 


## Testing the Function with "Panel" 

# PanelDummy = cbind(Panel, dummy("Month",Panel)) 
# PYSeason.Title.2 = PYSeason.test("dlTitle.2", paste0("Month",1:12), PanelDummy, statesAll)
# PYSeason.Title.16 = PYSeason.test("dlTitle.16", paste0("Month",1:12), PanelDummy, statesAll)
# PYSeason.Concurrent = PYSeason.test("dlConcurrent", paste0("Month",1:12), PanelDummy, statesAll)

## Result: The results of the 2 Pesaran-Yamataga tests suggests heterogeneous slopes across states. 
