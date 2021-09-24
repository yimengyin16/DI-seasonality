# Workflow of Data process (Original)

## 1. Importing the original data:  Data.import

## 2. Preparing X13 input files: Data.inputX-13ARIMA

## 3. Reading X13 output: SA.import.df.R,  SA.import.same.R,  SA.import.same.dl.R

## 4. Import unemployment data: importBLS.R

## 5. Combine to generate reg.data for seasonality paper

## 6. Combine to generate panel data for forecasting paper.(Unemployment data from reg.data)

## 7. Preparing X13 input files for log growth rates Data.inputX-13ARIMA.same.dl

## 8. Reading X13 output of log growth rates: SA.import.same.dl.R (outliers are subtracted from the series)

## 9. Google search series are imported using Data.import.keywords.R


## X13:
  # Data is stored in "X13data.var", "X13data.var.same", "X13data.var.same.dl"
  # In each folder, there are meta data files (.dta) for each of 5 types of applications
  
  # To adjust using automatic procedure
    # The auto procedure using log transformation(multiplicative) is in "E:\SkyDrive\Working\Third year\X13spc\Auto.log.spc"
    # The auto procedure using no transformation(additive)        is in "E:\SkyDrive\Working\Third year\X13spc\Auto.add.spc"
    # To adjust first chose a mata data file, and then chose Auto.log.spc
    # Need to check adjustment failure and create specific file to adjust that sereis (For Single Series)
    # Failure found:
      # X13data.var.same: Title.2_VT
      # X13data.var  : Totle_VT, Title.2_VT, SSI_PR
      # X13data.var.dl: dlTotal_ID


        