# dom-WestMed


### Requirements
* R-studio with R >= 3.1.0
* Packages:
  
  
## Installation
  
The R code can be downloaded from the following [link](https://github.com/dmarch/dom-wmed/archive/master.zip). Additionaly, check out this guideline at the [Rstudio website](https://support.rstudio.com/hc/en-us/articles/200532077-Version-Control-with-Git-and-SVN) for installing Git on your computer and creating a new project.


## Getting started

There several steps that need to be conducted before running the scripts. 

* Download raw data from third-party providers (see bellow)
* Edit the paths to those datasets
* Run the scripts following the suggested order from folders and scripts


## Datasets

### Raw data

* Jurisdictional waters
* Finwhale habitat model: [JRC] (https://data.jrc.ec.europa.eu/collection/emis)


## Workflow

### Stakeholders geodatabase
Folder `/geodatabase` contains R scripts for geocoding addresses of organizations

### DOM zones
Folder `/zoning` contains R scripts to create hypothetical restricted areas.

### Stakeholder selection
Folder `/identification` contains R scripts to select stakehoders based on: (1) distance to MPA, (2) overlap with jurisdictionwal waters



## License

Copyright (c) 2024 Federico Fabbri  
Licensed under the [MIT license](https://github.com/dmarch/abigoos/blob/master/LICENSE).


## Citation
Cite the code: [![DOI](https://zenodo.org/badge/753592347.svg)](https://zenodo.org/doi/10.5281/zenodo.10624747)

## Acknowledgements

FF acknowledges funding support from the Erasmus Mundus Master Course on Maritime Spatial Planning (EMMCMSP) program to conduct a research internship at the Balearic Islands Coastal Observing and Forecasting System (ICTS SOCIB) and the University of Aix-Marseille through the Inter-ED Thesis funding program. 
DM acknowledges support from the European Union's Horizon 2020 research and innovation programme under the Marie Skłodowska-Curie grant agreement (no 794938) and the Generalitat Valenciana through the plan GenT program (CIDEGENT/2021/058).
