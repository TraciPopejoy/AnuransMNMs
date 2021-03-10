# Mechanistic Niche Model Tutorial ----
#install.packages('NicheMapR')
library(NicheMapR); library(tidyverse)
get.global.climate(folder = 'global_micro_climate') #downloads global microclimate models
# https://mrke.github.io/NicheMapR/inst/doc/microclimate-model-tutorial.html explains the microclimate data

c('Apple', 'orange','Yellow orange', 'bananas') %>% #pipe = and then
  grep('orange', .)

# https://mrke.github.io/NicheMapR/inst/doc/ectotherm-model-tutorial.html
longlat <- c(146.77, -19.29) # Townsville, northern Australia
longlat <- c(-80.41, 37.23) # Blacksburg, VA
longlat <- c(-97.44, 35.22) # Norman, OK
longlat <- c(-87.63, 41.88) # Chicago, MI
micro <- micro_global(loc = longlat) #assumes this climate model is reflecting reality
ecto <- ectotherm() #default is for a skink 
str(ecto) #str just shows us the structure of the dataframe
#enbal is the energy balance
#environ gives us info on temperature and activity
#masbal is the mass balance
ecto$environ
ecto$environ %>% as_tibble() %>% count(DOY)
?ectotherm

#trying our own data
library(readxl)
anax_punc<-read_excel('C:/Users/Owner/Downloads/Anaxyrus punctatus MNM traits.xlsx')
#using defaults for alpha_max, alpha_min, pct_wet,
#       also mindepth, maxdepth, shade_seek
#       burrow, climb, #NOTE need to look at AtraiU for this
?ectotherm
(ap<-anax_punc %>% filter(`Life Stage` %in% c('unknown','adult')) %>%
    dplyr::select(Trait, Value, Units))
ap[ap$Trait=='Mass','Value']

ecto <- ectotherm(Ww_g = as.numeric(ap[ap$Trait=='Mass', 'Value']), 
                  T_F_max = as.numeric(ap[ap$Trait=='CTmax','Value'])-6, 
                  T_F_min = as.numeric(ap[ap$Trait=='Tmerge','Value'])+4, 
                  T_B_min = as.numeric(ap[ap$Trait=='Tmerge','Value']), 
                  T_RB_min = as.numeric(ap[ap$Trait=='Tmerge','Value']),
                  CT_max = as.numeric(ap[ap$Trait=='CTmax','Value']), 
                  CT_min = as.numeric(ap[ap$Trait=='CTmin','Value']), 
                  T_pref = as.numeric(ap[ap$Trait=='Tpref', 'Value']),
                  nocturn = 1, diurn = 0, crepus = 0,
                  shape = 4, #frog shape based on leopard frog
                  minshades = rep(0,12), # min available shade
                  maxshades = micro$MAXSHADES #max available shade
                  )
(body_temp<-as_tibble(ecto$environ) %>%
  mutate(date_x=round(DOY+TIME/24, 3)) )
ggplot()+
  geom_line(data=body_temp,
             aes(x=TIME,y=TC))+
  geom_hline(yintercept=as.numeric(ap[ap$Trait=='CTmax','Value']),
             color='red')+
  geom_hline(yintercept=as.numeric(ap[ap$Trait=='CTmin','Value']),
             color='blue')+
  facet_wrap(~DOY, nrow=1)+
  ggtitle(paste(longlat[1],longlat[2], sep=', '))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=30, size=6, hjust=.9))

# how to translate this into vulnerability
# where do we want to run the models?  - like within the range etc.
