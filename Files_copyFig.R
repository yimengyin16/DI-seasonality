

dir_to <- "C:\\Users\\yimen\\Google Drive\\AB_Projects_Personal\\Third_year\\02.2 Seasonality in Disability Applications Revision_2021\\Figures"

fileNames <- 
	c("paper_plot/3series.pdf",
		"paper_plot/spec.pdf",
		"paper_plot/apply&unemplyLag.pdf",
		"paper_plot/BoxplotDummycoeff.pdf",
		"paper_plot/SeasonalDummies_AllStates.pdf",
		"paper_plot/Allowance/Allow_App_Dummies_AG.pdf"
		)
	
	
file.copy(fileNames, dir_to, overwrite = TRUE)
