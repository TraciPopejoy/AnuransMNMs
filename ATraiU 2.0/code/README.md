# Purpose
----------
This directory contains code used to start the ATraiU-TaDS trait search, build the resulting trait database, and analyze the contents of said database. 

# Scripts
----------
#### 0_identifying_SE_Anurans.Rmd
This Rmarkdown document was used to subsample across a rarity gradient and decide which taxa to start searching for first. 

#### 1_Organizing_physio_data.Rmd
This Rmarkdown uses googledrive4 and googlesheets R packages to download the trait values from their raw data forms. We collate that trait data, update the taxonomy and then add in reference information. We output four main documents: a taxonomy key, a reference key, the full trait database, and a summarized trait database. 

#### 2_Describing_ATraiU2.Rmd
This Rmarkdown summarizes the information within the trait database and creates figures for the manuscript.

#### 3_TraitDB_comparisions.Rmd
This Rmarkdown brings in two trait databases that contain similar information to ATraiU-TaDs. This code roughly compares the two but generally, the comparisons are done by hand because of taxonomic differences. 

