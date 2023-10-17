## Bran

This repository is named after Bran Stark from Game of Thrones, who used his Greensight ability to discover Jon Snow's Targaryen ancestry. This tool builds a family tree from data exported from Transnetyx.


## Installation

Install the following packages in R:

```R
install.packages('kinship2')
install.packages('dplyr')
install.packages("readxl")
install.packages("RColorBrewer")
install.packages("optparse")
```

## Getting Started

Test on sample\_ped\_tab.csv:

```bash
Rscript R/plot_family_tree.R -i data/family-tree/sample_ped_tab.csv -p 1
```

Export data from your favorite strain in Transnetyx:

```bash
Rscript R/plot_family_tree.R -i path/to/My_Mice.xlsx
```


## Copyright

This code is copyright by Harrison Wang in 2023.