## Bran

This repository is named after Bran Stark from Game of Thrones, who used his Greensight ability to discover Jon Snow's Targaryen ancestry. This tool builds a family tree from data exported from Transnetyx.


## Installation

Install the following packages in R:

```R
install.packages('kinship2')
install.packages('dplyr')
install.packages('wrapr')
install.packages("readxl")
install.packages("RColorBrewer")
install.packages("optparse")
```

## Getting Started


1. Weave your old annotated data with new export from Transnetyx.

	```bash
	Rscript R/weave_data.R -i data/initial_data.csv -n data/My_Mice.xlsx
	```

2. Generate a pedigree figure. You can either use data directly from Transnetyx or the cleaned output of `weave_data.R`.
	
	```bash
	Rscript R/plot_family_tree.R -i path/to/My_Mice.xlsx
	```

3. Test on `sample_ped_tab.csv`. The input file is already the default option of the script.

	```bash
	Rscript R/plot_family_tree.R -i data/sample_ped_tab.csv -p 1
	```


## Copyright

This code is copyright by Harrison Wang in 2023.