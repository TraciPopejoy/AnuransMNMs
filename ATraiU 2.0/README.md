# Purpose
----------
This directory contains all of the elements used to combine and analyze ATraiU-TaDs. 

# Files
----------
### Folder: code
This folder contains the code used to start, build and analyze ATraiU-TaDs. 

### Folder: intermediate_results
This folder houses files that are used to document processes or as building blocks to the final database files.

### Folder: manuscript
This folder houses manuscript outputs, mainly figures.

#### ATraiU2_full_2022JULY.csv
This file is considered the primary dataset for ATraiU and contains the trait data in long format. Each row contains a unique trait value for a single species and trait from a single reference. Most traits are numeric but Activity traits are categorical. 
#### ATraiU2_reference_list_2022JULY.csv
This file contains the citation information and corresponding unique identifiers and tier IDs for all 122 references contributing trait data to the ATraiU database.
#### ATraiU2_summary_values_2022JULY.csv
This file contains a dataset summarized from “ATraiU2_full_2022JULY.csv”. Each species has one row containing the summarized data, representing traits for the adult life stage. For categorical activity data, a species given “TRUE” for any category has at least one reference with the trait listed as that category in “ATraiU2_full_2022JULY.csv”. If none of the references for a species reported a category as “TRUE”, it received a “FALSE”. For continuous traits, if the trait is a minimum trait (i.e. critical thermal minima, emergence temperature, minimum foraging temperature), we retain the minimum trait data point across all references. If the trait is a maximum trait (i.e. critical thermal maxima, maximum foraging temperature), we retain the maximum trait data point across all references. For all other traits (i.e., mass, thermal preference, optimum foraging temperature), we calculated the mean across all references. 
#### ATraiU2_taxonomic_key_2022JULY.csv
This file contains the taxonomic key used to bridge the original taxa within the raw files to an up-to-date taxonomy based on the GBIF Backbone Taxonomy.

