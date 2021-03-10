# AnuransMNMs
Traci P. DuBose Updated 03/10/2021

-------------------------------

# Purpose

This repository contains R code for organizing physiological trait information for anurans and building mechanistic niche models using those traits. 

-------------------------------

# Scope

Currently focusing on anurans within the southeast USGS region and Virginia. Our goal is trait information and MNMs for 8 species.

-------------------------------

# Intended Uses

Code is currently in draft form and preliminary.

-------------------------------

#Scripts  
in order from most used to least, currently

organizing_phyio_data.R - code to join all individual species googlesheets together, visualize missing traits, and consolidate / organize the reference list.

choosing_MNM_taxa.R - by using occurrence data from our RCS project, I identified species resident in the southeastern US, stratified them by sensitivity, and then chose 4 species from each sensitivity group to a) chose species to get traits for and b) make sure those species encompassed a range of climate sensitivity.

mnm_tutorial.R - worked through tutorial with Vj on NicheMapper inputs, outputs and possibilities.

deb_mnm_model_notes.R - early notes on which MNM models are available and possibly building own model

prep_undergrad_phys_traits.R - early script to identify traits already found in other trait databases (GlobalTherm and AmphiBio) and count the references for the combination of different traits and the term 'anuran'.

