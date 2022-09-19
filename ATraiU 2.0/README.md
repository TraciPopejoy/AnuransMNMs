# Purpose
----------
This directory contains all of the Rmarkdowns used to combine and analyze ATraiU-TaDS: Anuran Traits of the United States with Thermal data for Southeastern Species. The database files can be found at ScienceBase and a manuscript is in prep for Icthyology and Herpetology. 

# Files
----------
### Folder: code
This folder contains the code used to start, build and analyze ATraiU-TaDs. 

### Folder: manuscript
This folder houses manuscript outputs, mainly figures.

## All csv files can be found on ScienceBase and reside in the top folder for use in the RMarkdowns

#### ATraiU2_full_2022SEPT.csv
This file is considered the primary dataset for ATraiU-TaDS and contains the trait data in long format. Each row contains a unique trait value for a single species and trait from a single reference. Most traits are numeric but Activity traits are categorical. 
#### ATraiU2_reference_list_2022JULY.csv
This file contains the citation information and corresponding unique identifiers and tier IDs for all 122 references contributing trait data to the ATraiU database.
#### ATraiU2_summary_values_2022SEPT.csv
This file contains a dataset summarized from “ATraiU2_full_2022SEPT.csv”. Each species has one row containing the summarized data, representing traits for the adult life stage. For categorical activity data, a species given “TRUE” for any category has at least one reference with the trait listed as that category in “ATraiU2_full_2022JULY.csv”. If none of the references for a species reported a category as “TRUE”, it received a “FALSE”. For continuous traits, if the trait is a minimum trait (i.e. critical thermal minima, emergence temperature, minimum foraging temperature), we retain the minimum trait data point across all references. If the trait is a maximum trait (i.e. critical thermal maxima, maximum foraging temperature), we retain the maximum trait data point across all references. For all other traits (i.e., mass, thermal preference, optimum foraging temperature), we calculated the mean across all references. 
