
## MOWL documentation
# https://www.ssa.gov/disability/data/ssa-sa-mowl.htm 


## Checked on June 9, 2021
#   - Lastest data release: May 4, 2021
#   - Data up to 4-2021


dir_data <- "Data/"

# Date of downloading: 5/9/2021. up to 2021-04
download.file("https://www.ssa.gov/disability/data/SSA-SA-MOWL.xls", paste0(dir_data, "/MOWL/SSA-SA-MOWL-2021-04.xls"))
download.file("https://www.ssa.gov/disability/data/SSA-SA-MOWL.csv", paste0(dir_data, "/MOWL/SSA-SA-MOWL-2021-04.csv"))


## Date translation table. Date of downloading: 5/9/2021
download.file("https://www.ssa.gov/disability/data/SSA_DATES1.xlsx", paste0(dir_data, "/MOWL/SSA_DATES1-2021.xlsx"))
download.file("https://www.ssa.gov/disability/data/SSA_DATES1.csv",  paste0(dir_data, "/MOWL/SSA_DATES1-2021.csv"))
