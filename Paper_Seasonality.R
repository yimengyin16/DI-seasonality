# Creates figures and tables for the seasonality paper. 


## Loading packages and data ####

library(plyr)
library(ggplot2)
library(scales)
library(xtable)
#library(DataCombine) # it loads dplyr!
library(magrittr)
library("tidyr")
library("knitr")

options(xtable.include.rownames = F,
				xtable.booktabs = T,
				xtable.caption.placement = "top")

source("General.R")
source("Forecasting.main.R")
source("Modeling.HeteroPYtestSeason.R")

load("Panel.RData")

plot.path = paste0(path_work, "/paper_plot/")

monthName = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
							"Jul", "Aug", "Sep", "Oct", "Nov", "Dec")





## Functions and tools ####

OCSB.TEST = function(x, lags = c(1,5), period = 12){
	## This function implement OCSB test for seasonal unit root based on OCSB(1988)
	## Critical values can be found in Franses and Hobijin (1997).(CV are not provided for all sample size, but eht Gauss code is provided.)
	## Inputs
	# x: a ts object.
	# lags: a vector containing lags of dependent variable to be included in test regression.
	## Outputs: A list with following components:
	# $reg: test regression
	# $bgtest: Bruesch-Godfrey test for autocorrelation in residuals
	# $t: 2 t statistics for beta_1 = 0 and beta_2 = 0
	# $F: F statistics for beta_1 = beta_2 = 0
	
	# x = lTota
	# period = 12
	y1s = diff(x, lag = period, differences = 1)
	y1s = diff(y1s, lag = 1, differences = 1)
	y1 = diff(x, lag = 1, differneces = 1)
	ys = diff(x, lag = period, differences = 1)
	
	Y = cbind(y1s, y1, ys, cycle = cycle(x), trend = 1:length(x))
	
	lmOCSB = dynlm(y1s ~ L(ys,1) + L(y1, period) + L(y1s, lags)+ factor(cycle), data= Y)
	bg = bgtest(lmOCSB, 12)
	
	# t statistics
	t.one = summary(lmOCSB)$coef[2,3]
	t.two = summary(lmOCSB)$coef[3,3]
	
	# F statistics
	Fstat = linearHypothesis(lmOCSB, c("L(ys, 1) = 0", "L(y1, period) = 0"))$F[2]
	
	print(bg)
	print(t.one)
	print(t.two)
	print(Fstat)
	print(paste("critical value of t:", forecast:::calcOCSBCritVal(period)))
	
	output = list(reg = lmOCSB, bgtest = bg, t = c(t.one = t.one, t.two = t.two), F = Fstat)
}


seaDummy = function(category,region, data, dif = TRUE, ...){
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
	x = as.factor(df$Month)
	
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


select2 = function( region, data = reg.data, start = c(2000, 10),end = c(2018,12)){
	# A function for selecting a particular state in a data frame.
	# "zoo" is required. 
	
	#Input:
	# region: state or region abbreviation.
	# data: dataframe containing the series.
	# start, end: starting and ending date
	#Output:
	# A time series of the class "zoo"/"zooreg"
	
	# data = Panel
	# region = "AG"
	# end = c(2018, 12)
	
	series = data[data$State == region & data$Cal.Year <= end[1],]
	exclude = c("State","Cal.Year", "Month","LMonth","L2Month","L3Month", "Region", "Date", "Formatted.Date")
	
	for (i in names(series)[!names(series) %in% exclude])
		series[,i] = zooreg(series[,i], start = start, end = end, f = 12)
	series = series[order(series$Cal.Year,series$Month),]
	series
}

# select2("NW", data = Panel)




# Section 2 ####

#Figure1: Plot of 3 series ####
#Seasonality.Pattern.Rmd

AppSeries = subset(Panel, State == "AG", c("Formatted.Date", "Title.2", "Title.16", "Concurrent"))

AppSeries$Formatted.Date = as.Date(as.yearmon(AppSeries$Formatted.Date))
AppSeries = melt(AppSeries, id = "Formatted.Date")


ThreeSeries = ggplot(AppSeries, aes(x = Formatted.Date, y = value, colour = variable)) + theme_bw() + 
	geom_line(size = 1 ) + 
	geom_vline(xintercept = as.numeric(as.Date(paste0(2001:2018, "-1-1"))), linetype = 3, size = 0.5) +            
	
	scale_color_manual(values = c("red","blue","orange"), labels = c("SSDI", "SSI", "Concurrent")) + 
	scale_x_date(date_breaks = "1 year", labels = date_format("%Y"), limits = as.Date(c("2000-10-1", "2018-1-1"))) + 
	scale_y_continuous(breaks = seq(0, 200000, 10000), labels = comma) +
	labs(x = NULL, y = "Number of Applications", colour = NULL)+
	theme(legend.justification=c(0,1), legend.position=c(0.01,0.99))
ThreeSeries

cairo_pdf(paste0(plot.path, "3series.pdf"), width = 10, height = 4.8, onefile = T)
ThreeSeries
dev.off()




#Figure2: Spectral Density of first difference ####
#Seasonality.Pattern.Rmd

pdf(file = paste0(plot.path,"spec.pdf"), 
		width = 9 , heigh = 3
)
par(mfrow = c(1,3) )
plotSpec1("Title.2",   "NW", data = Panel, main = "SSDI only")
plotSpec1("Title.16",  "NW",data = Panel,  main = "SSI only")
plotSpec1("Concurrent","NW", data = Panel,  main = "Concurrent")
dev.off()



# Section 3 ####

# Table 1 Canova_Hansen test for deterministic seasonality ####

## Settings for CH.test 
CH.f0 = 1          # Whether the 1st lag of dependent variable is included in auxiliar regression
CH.frec = NULL      # Test for all frequencies
# CH.frec = c(1,rep(0,5))   # only pi/6 tested
# CH.frec = c(0,1,rep(0,4)) # only pi/3 tested
# CH.frec = c(1,1,rep(0,4)) # only pi/6 and pi/3 tested

## log differenced series, seasonal dummies not removed.

#f0 = CH.f0, frec = CH.frec)
ch.test(select("dlTitle.2",    "NW",  data = Panel, na.rm = T, end = c(2018, 12)), lag1 = TRUE) # 1.94 
ch.test(select("dlTitle.16",   "NW",  data = Panel, na.rm = T, end = c(2018, 12)), lag1 = TRUE) # 1.89
ch.test(select("dlConcurrent", "NW",  data = Panel, na.rm = T, end = c(2018, 12)), lag1 = TRUE) # 1.75
ch.test(select("dlunemply_U",  "NW",  data = Panel, na.rm = T, end = c(2018, 12)), lag1 = TRUE) # 2.12


ch.test(select("Title.2",    "NW",  data = Panel, na.rm = T, end = c(2018, 12)), lag1 = TRUE)   # 1.87 
ch.test(select("Title.16",   "NW",  data = Panel, na.rm = T, end = c(2018, 12)), lag1 = TRUE)   # 1.89
ch.test(select("Concurrent", "NW",  data = Panel, na.rm = T, end = c(2018, 12)), lag1 = TRUE)   # 1.52
ch.test(select("unemply_U",  "NW",  data = Panel, na.rm = T, end = c(2018, 12)), lag1 = TRUE)   # 1.89


# Results: to be updated
# 	
# 	* When seasonal unit roots at all frequencies are tested,the $H_0$ of sationarity cannot be rejected even at 10% level for all series.
#   * When only pi/6 is tested, SSI is significant at 10% level, and unemployment is almost significant at 10% level.
#   * When only pi/3 is tested, DI is significant at 10% level, all others are insignificant.
#   * When only pi/6 and pi/3 are tested, none is significant. 
#   * All results are robust to the inclusion of a lag term in the auxiliar regression.
#   * All results are the same for logged series and log differenced series. 

# Notes:
# 	
# 	1. Data set used: 2000:10:2014:5  
#   2. The seasonal dummies are automatically romoved when using `CH.test`.


# OCSB test ####

OCSB.TEST(select("lTotal",      "NW", Panel, end = c(2018, 12)), c(1:3))
OCSB.TEST(select("lTitle.2",    "NW", Panel, end = c(2018, 12)), c(1:3))
OCSB.TEST(select("lTitle.16",   "NW", Panel, end = c(2018, 12)), c(1:9))
OCSB.TEST(select("lConcurrent", "NW", Panel, end = c(2018, 12)), c(1,12:15))
OCSB.TEST(select("lunemply_U",  "NW", Panel, end = c(2018, 12)), c(1:5))

#* Critical values from Franses and Hobijin(1997):

# t statistics (c, d, nt): 
# Year|  0.01|0.025| 0.05| 0.10 | 0.01 |0.025| 0.05| 0.10 
# |----------------------------------------------------****
# 10  | -5.39|-5.01|-4.68| -4.31| -3.42|-3.08|-2.78| -2.48
# 20  | -5.40|-5.05|-4.74| -4.38| -3.40|-3.07|-2.81| -2.52
# 
# F statistic (c, d, nt): c, d, nt 
# Year|  0.01 | 0.025 | 0.05  | 0.10 
# ----|-------------------------------****
# 10  | 11.21 | 13.11 | 14.87 | 17.16
# 20  | 11.51 | 13.26 | 14.87 | 16.91


# Results
# 
# * t tests for $\beta_1$ indicate regular differences are needed in all series.
# * t tests for $\beta_2$ indicate no seasonal difference is need in all series.
# * F tests indicate the null of I(1,1) is strongly rejected in all series. 




# Section 4 patterns ####

# Seasonal Dummies regressions #### 


# Seasonal dummies for national level 

# Table 2 R^2 of seasonal dummy regressions ####

# Note that seaDummy use first difference by default.

variables = c("lTitle.2", "lTitle.16", "lConcurrent")
dummyNW = sapply(variables, function(x) seaDummy(x,"NW"))
dummyNW

#      SSDI only  SSI only    Concurrent
# R2    0.764      0.7295       0.5613




# Table 3: Coefficients of seasonal dummies for national aggregates ####
dummyNW_coeff <- sapply(variables, function(x) seaDummy(x,"NW")$model$coeff)
dummyNW_coeff %>% t


# Figure 3: Seasonal patterns, Boxplots for Coefficients of seasonal dummies


# Running seasonal dummy regression on all states
dummy.Title2_state   = sapply(states52, function(x){m = seaDummy("lTitle.2",x, data = Panel); c(R2 = m$R2, m$model$coeff)}) %>% as.data.frame
dummy.Title2_state$vars <- c("R2", monthName)
dummy.Title2_state %<>% 
	as.data.frame() %>% 
	tidyr::gather(state, value, -vars) %>% 
	dplyr::mutate(vars = factor(vars, levels = monthName))


dummy.Title16_state   = sapply(states52, function(x){m = seaDummy("lTitle.16",x, data = Panel); c(R2 = m$R2, m$model$coeff)}) %>% as.data.frame
dummy.Title16_state$vars <- c("R2", monthName)
dummy.Title16_state %<>% 
	as.data.frame() %>% 
	tidyr::gather(state, value, -vars) %>% 
	dplyr::mutate(vars = factor(vars, levels = monthName))

dummy.Concurrent_state   = sapply(states52, function(x){m = seaDummy("lConcurrent",x, data = Panel); c(R2 = m$R2, m$model$coeff)}) %>% as.data.frame
dummy.Concurrent_state$vars <- c("R2", monthName)
dummy.Concurrent_state %<>% 
	as.data.frame() %>% 
	tidyr::gather(state, value, -vars) %>% 
	dplyr::mutate(vars = factor(vars, levels = monthName))


# Boxplots seasonal dummy coefficients  
pdf(file = paste0(plot.path,"BoxplotDummycoeff.pdf"), width = 6 , heigh = 9)
par(mfrow = c(3,1), mar = c(4,4,2,1)+ 0.1)

bp.dm.Title.2 = boxplot(value ~ vars, data = dummy.Title2_state[dummy.Title2_state$vars != "R2", ], main = "SSDI only")
abline(h = 0, col = "blue", lwd = 1.5, lty = 3  )
text(x = bp.dm.Title.2$group + 0.2, 
		 y = bp.dm.Title.2$out, 
		 dummy.Title2_state$state[which(dummy.Title2_state$value %in% bp.dm.Title.2$out)],
		 adj = 0.2, cex = 0.45)


bp.dm.Title.16 = boxplot(value ~ vars, data = dummy.Title16_state[dummy.Title16_state$vars != "R2", ], main = "SSDI only")
abline(h = 0, col = "blue", lwd = 1.5, lty = 3  )
text(x = bp.dm.Title.16$group + 0.2, 
		 y = bp.dm.Title.16$out, 
		 dummy.Title16_state$state[which(dummy.Title16_state$value %in% bp.dm.Title.16$out)],
		 adj = 0.2, cex = 0.45)


bp.dm.Concurrent = boxplot(value ~ vars, data = dummy.Title2_state[dummy.Title2_state$vars != "R2", ], main = "SSDI only")
abline(h = 0, col = "blue", lwd = 1.5, lty = 3  )
text(x = bp.dm.Concurrent$group + 0.2, 
		 y = bp.dm.Concurrent$out, 
		 dummy.Title2_state$state[which(dummy.Title2_state$value %in% bp.dm.Concurrent$out)],
		 adj = 0.2, cex = 0.45)

dev.off()



# Table 5 PY test for heterogeneity ####

PanelDummy = cbind(Panel, dummy("Month",Panel)) 

PYSeason.Title.2    = PYSeason.test("dlTitle.2", paste0("Month",1:12), 
																		cbind(Panel, dummy("Month",Panel)), states50)
PYSeason.Title.16   = PYSeason.test("dlTitle.16", paste0("Month",1:12), 
																		cbind(Panel, dummy("Month",Panel)), states50)
PYSeason.Concurrent = PYSeason.test("dlConcurrent", paste0("Month",1:12), 
																		cbind(Panel, dummy("Month",Panel)), states50)

PYSeason.test = rbind( cbind(Category = "SSDI", PYSeason.Title.2[3,], PYSeason.Title.2[4,]),
											 cbind(Category = "SSI" , PYSeason.Title.16[3,], PYSeason.Title.16[4,]),
											 cbind(Category = "Concurrent", PYSeason.Concurrent[3,], PYSeason.Concurrent[4,]))
names(PYSeason.test) = c("Category", "Dh", "p-value", "Dt", "p-value")

PYSeason.test

print(xtable(PYSeason.test))




# Figure 4 Seasonal patterns of DB application and Unemployment ####

model.unemply <-  seaDummy("lunemply_U","NW", data = Panel)
model.Title.2 <-  seaDummy("lTitle.2","NW", data = Panel)


pdf(file = paste0(plot.path,"apply&unemply.pdf"), 
		width = 9 , heigh = 5)
plot(model.Title.2$model$coefficients, type = "l", lty = 1, lwd = 2, col = "blue",
		 xlab = "", ylab = "coefficients", ylim = c(-0.15,0.2),
		 xaxt="n", cex = 1)
lines(model.unemply$model$coefficients, type = "l",lty = 2, lwd = 2, col = "red", cex = 1)
legend("topright",c("SSDI applications", "Unemployment"),lty = c(1,2), col = c("blue","red"),lwd = 2,bg = "white")
abline(h = 0, lty = 3)
axis(1, xaxp = c(1,12,11))
dev.off()

# Compare the Month plot of SSDI only application and unemployment with unemployment lagged

model.unemply = seaDummy("lunemply_U","NW", data = Panel)
model.Title.2 = seaDummy("lTitle.2","NW", data = Panel)


pdf(file = paste0(plot.path,"apply&unemplyLag.pdf"), 
		width = 9 , heigh = 5)
plot(model.Title.2$model$coefficients, type = "l", lty = 1, lwd = 2, col = "blue",
		 xlab = "", ylab = "coefficients", ylim = c(-0.15,0.2),
		 xaxt="n", cex = 1)
lines(model.unemply$model$coefficients[c(12,1:11)], 
			type = "l",lty = 2, lwd = 2, col = "red", cex = 1)

legend("topright",c("SSDI applications", "Unemployment 1 month lag"),lty = c(1,2), col = c("blue","red"),lwd = 2,bg = "white")
abline(h = 0, lty = 3)
axis(1, xaxp = c(1,12,11))
dev.off()





# Figure 5 choherence between seasonal dummy adjusted growth rate of DB application and unemployment ####

 # Check the coherence between the series net of deterministic seasonality

plot.coherence = function(region, category1, category2 = "lunemply_U", title = "", data){
	
	res.apply = ts(as.numeric(summary(seaDummy(category1,region, data)$model)$residuals, s = c(2000,11), end = c(2018, 12), f=12))
	res.emply = ts(as.numeric(summary(seaDummy(category2,region, data)$model)$residuals, s = c(2000,11), end = c(2018, 12), f=12))
	sRes = spec.pgram(cbind(res.apply, res.emply),
										#spans = c(4,4), 
										kernel("daniell", c(3,3)),
										taper = 0, plot = FALSE)
	plot(sRes, plot.type = "coh", ci.lty = 2, main = title)
	abline(v = 1:6/12, lty = 3)
}

plot.coherence("NW","lTitle.2", title = "SSDI", data = Panel)


pdf(file = paste0(plot.path,"coherenceNW.pdf"), 
		width = 9 , heigh = 3
)
par(mfrow = c(1,3) )
plot.coherence("NW","lTitle.2",    title = "SSDI Only",  data = Panel)
plot.coherence("NW","lTitle.16",   title = "SSI Only",   data = Panel)
plot.coherence("NW","lConcurrent", title = "Concurrent", data = Panel)
dev.off()


# Table 6 








