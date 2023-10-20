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

1.	**Test that this repository runs.** Use the included file `sample_ped_tab.csv`:

	```bash
	Rscript R/plot_family_tree.R -i data/sample_ped_tab.csv -p 1 -s
	```

2.	**Plot the family tree.** Use the "Export to Excel" function in the Dashboard/Mouse section of Transnetyx to obtain a `My_Mice.xlsx` file. Move it into `data`, then run the same script as above.	

	```bash
	Rscript R/plot_family_tree.R -i data/My_Mice.xlsx
	```

	This outputs two files, the pedegree into `figures` and a cleaned up csv file into `data/troubleshooting`. Open the cleaned up csv file, then update it with genotyping information and colors.
	
3. **Merge your annotated file with new data.** In the future, when you download a new `My_Mice.xlsx` file, to bring in your annotations from you old file, run the following script:

	```bash
	Rscript R/weave_data.R -i data/initial_data.csv -n data/My_Mice.xlsx
	```

## Data Requirements

At minimum, your exported data needs the following columns: Sex, Mouse ID, Father ID, and Mother ID. The rest of the columns are optional. If you include the DOD column, the 'alive' field will automatically be computed for you.

I recommend setting a default view with the following columns: Use, Strain, Sex, Age, DOB, DOD, Mouse ID, Genotype, Labels, Cage ID, Notes, SystemId, Father ID, Mother ID, Rack, Position.

## Contributing

Please create a Github issue or reach out to me directly.

## Copyright

This code is copyright by Harrison Wang in 2023.