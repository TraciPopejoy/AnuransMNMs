# checking out mnm models
#process explicit --> physiological traits of the organism determine survival at map.

#to make it comparable to rcs and sdm, do I need to have it on a map?
# pretty sure I do.

#process: 
# build a 'model'/function
      # NicheMapR DEB+ectotherm model 
      # table of physiological limits (a la CLIMEX)
      # code up my own (Bayesian style - physiological limits related to survival prob)
          # incorporate stage population model, only include raster cells where eggs, tadpoles, and adults survive
# extrapolate function across a climate raster of US
    # raster should have 
              # temperature
              # ppt
              # known bodies of water (size limit?)
              # salinity intrusion?

#life stages linked through stage-based matrix
# survival probability, Pi,i+1, is the probability that an individual in size class i will survive and move into size class i+1
# Fi = (bi x Pii)+(bii x Pii1)
# subdiagnol is transition probabilities between classes
# diagnol is survival probability without transition
# Pii + Pii1 is the total rate of survival for individuals in a particular stage
t(data.frame(egg=c("Pee", "Ft","Fsa","Fa"),
           tadpole=c("Pet","Ptt", 0, 0),
           subadult=c( 0, "Ptsa","Psasa", 0),
           adult=c(0,0,"Psaa","Paa")))
#would use it only to predict 1 year into the future
# what would drive survival?
#temperature, water availability, salinity, food avail
# what would drive fecundity?
#water availability, water permanence, water temperature







library(devtools)
devtools::install_github('mrke/NicheMapR')
library(NicheMapR)
