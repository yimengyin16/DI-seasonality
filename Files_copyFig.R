

dir_to <- "G:/My Drive/AB_Projects_Personal/Third_year/02.2 Seasonality in Disability Applications Revision_2021/Figures_v2"

# fileNames <- 
# 	c("paper_plot/3series.pdf",
# 		"paper_plot/spec.pdf",
# 		"paper_plot/apply&unemplyLag.pdf",
# 		"paper_plot/BoxplotDummycoeff.pdf",
# 		"paper_plot/SeasonalDummies_AllStates.pdf",
# 		"paper_plot/Allowance/Allow_App_Dummies_AG.pdf"
# 		)

fileNames <- dir("paper_plot/fig_newVer/")

fileNames <- paste0("paper_plot/fig_newVer/", fileNames)
	
	
file.copy(fileNames, dir_to, overwrite = TRUE)



# latex to Word
# https://medium.com/@zhelinchen91/how-to-convert-from-latex-to-ms-word-with-pandoc-f2045a762293

# pandoc "Seasonality in U.S. Disability Applications v2.tex" --filter pandoc-crossref --bibliography="C:\OneDrive\Study\BibTex\library.bib" -o mydoc.docx