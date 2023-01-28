# AnuransMNMs
Traci P. DuBose Updated 01/27/2023

-------------------------------

# Purpose

This repository contains R code for organizing physiological trait information for anurans and building mechanistic niche models using those traits. 

-------------------------------

# Intended Uses

Code is currently in draft form and preliminary.

-------------------------------

# Files


### Folder: ATraiU 2.0
This contains all the code and files used to build and analyze ATraiU-TaDS. 

### Folder: input_data
This folder contains data that is used to parameterize or build scripts within this folder. Most notably is a phylogeny and image for trait imputation, a trait table to designate different activities for anurans (from ATraiU, Moore et al. 2020), and micro climate data for the mechanistic niche models. 

### Folder: intermediate_results
This folder contains outputs from the high power computer or other analyses that do not directly contribute to the manuscript.

### Folder: results
This folder contains figures and tables produced from the code within this folder

#### 0_Species_Selection.Rmd
This Rmarkdown identifys 40 species that have < 1% of their International Union for the Conservation of Nature range within the region we are deeming the Southeast. It also pulls in ATraiU-TaDS to describe potential species pool sizes based on the physiological trait data we have available. 

#### 1_Hypothesis_Building.Rmd
This Rmarkdown has plots and other analyses that informed the creation of our hypotheses, especially within a pitch document sent to coauthors. 

#### 2_BuildingInputTraits_Pts.Rmd
This Rmarkdown builds the input datasets into the MNMs: thermoregulation traits and the points dataframe to run the MNMs across. We first remove trait values measured on populations far outside our study range. We then recalculate interspecific trait values and use those to imputate physiological, continuous trait data. We use the 36 species that occur within the southeast and imputate traits based on the available trait data. This Rmarkdown also explores the resulting imputed trait data. It also contains information about the spatial sampling we do with these MNMs. It contains the code used to output the list of points at which to run MNMs and describes the latitudinal gradient sampled in reference to species IUCN ranges.  

#### 3_OnePtAnalysis.Rmd
This Rmarkdown contains code to run MNMs at a single point where CTmax was determined. This analysis is described in Appendix 1. 

#### 4a_MNMAcrossSpaceCode.Rmd
This Rmarkdown contains code that is run on the HPC to run the MNMs across he Southeast United States. We use parallelized code to speed up these calculations. MNMs are all built using NicheMapR.

#### 4b_MNMAcrossSpaceResults.Rmd
This Rmarkdown contains code used to write the assess the output of the MNMs and write the results section of the associated manuscript. All main figures and tables are produced in this Rmd.

#### 5_IntraspecificTraitEffect.Rmd
This Rmarkdown contains the code to run MNMs for a subset of points and vary intraspecific traits within the ectotherm model. The output table is pulled into 4b_MNMAcrossSpaceResults.Rmd to create Figure 3B. It also contains the outputs of the trait monster analysis, which is in Appendix 3. 

#### x_NicheMapper_sensitivity_20210629.Rmd
This Rmarkdown contains the preliminary structure for a sensitivity analysis of NicheMapR.  

#### x_old_phys_notes.Rmd
This Rmarkdown contains random bits of code I've used to increase my knowledge of this project. 

#### x_complicated models.Rmd
Here, I explored how putting American Bullfrogs in water instead of air adjusted the MNM estimates of warming tolerance. I also tried to get the life state DEB model running to incorporate differences in thermal tolerances across life stages.  
