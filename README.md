# AnuransMNMs
Traci P. DuBose Updated 07/08/2022

-------------------------------

# Purpose

This repository contains R code for organizing physiological trait information for anurans and building mechanistic niche models using those traits. 

-------------------------------

# Intended Uses

Code is currently in draft form and preliminary.

-------------------------------

# Files

### Folder: ANURA
This contains the IUCN shapefiles, downloaded in October 2020

### Folder: ATraiU 2.0
This contains all the code and files used to build and analyze ATraiU-TaDS. 

### Folder: input_data
This folder contains data that is used to parameterize or build scripts within this folder. Most notably is a phylogeny and image for trait imputation, a trait table to designate different activities for anurans (from ATraiU, Moore et al. 2020), and micro climate data for the mechanistic niche models. 

### Folder: results
This folder contains figures and tables produced from the code within this folder

#### 0_Species_Selection.Rmd
This Rmarkdown identifys 40 species that have < 1% of their International Union for the Conservation of Nature range within the region we are deeming the Southeast. It also pulls in ATraiU-TaDS to describe potential species pool sizes based on the physiological trait data we have available. 

#### 1_missing_trait_imputation.Rmd
This Rmarkdown has code to imputate physiological, continuous trait data. We use the 40 species that occur within the southeast and imputate traits based on the available trait data. This Rmarkdown also explores the resulting imputed trat data. 

#### 2_Scaling_Up_MNM_models.Rmd
This Rmarkdown contains for loops and preliminary code for mechanistic niche models built from NicheMapR.

#### x_NicheMapper_sensitivity_20210629.Rmd
This Rmarkdown contains the preliminary structure for a sensitivity analysis of NicheMapR.  

#### x_old_phys_notes.Rmd
This Rmarkdown contains random bits of code I've used to increase my knowledge of this project. 

#### focal_taxa.csv
This contains a 40x1 matrix that has our 40 focal species. It is created when running 0_Species_Selection.Rmd. 

