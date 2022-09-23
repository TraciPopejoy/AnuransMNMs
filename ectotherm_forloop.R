library(NicheMapR)
for(rw in 2:10){
  dir.create(paste0('microclimate_data/pt',rw))
  micro<-micro_ncep(loc = c(as.numeric(va_pts_df[rw,1]),as.numeric(va_pts_df[rw,2])),
                    dstart = "01/01/2020", dfinish = "31/12/2020",
                    dem.res=1000, # requested resolution of the DEM from elevatr in m
                    DEP = c(0, 2.5,  5,  10,  15,  20,  30,  50,  100,  200), #cm
                    minshade = 0, maxshade = 90, runshade=1,
                    write_input=0, save=1,
                    soilgrids=1,
                    spatial='../ncep_data',
                    Usrhyt = 0.01) # local height for organism
  file.copy(list.files(pattern='.Rda'), 
            paste0('microclimate_data/pt', rw, '/',list.files(pattern='.Rda')))
  for(spp in test_spp){
    if(grepl(spp, va_pts_df[rw,4])){
    spp_traits <- traits %>% filter(species == spp)
    ecto<-ectotherm(Ww_g=spp_traits["Mass"], 
                    CT_max=spp_traits["CTmax"], 
                    CT_min=spp_traits["CTmin"], 
                    T_pref=spp_traits["Tpref"],
                    diurn=spp_traits["diurnal"],
                    nocturn=spp_traits["nocturnal"],
                    crepus=spp_traits["crepuscular"],
                    burrow=spp_traits["s.fossorial"],
                    climb=spp_traits["s.arboreal"],
                    shdburrow = 1, #the animal's retreat is in the open (0), in the shade when above or below CTmin in sun (1) or in shade always
                    maxdepth = 2, #maximum depth of the burrow
                    T_F_min=19,
                    T_F_max=35,
                    T_RB_min=spp_traits["Tmerge"],
                    T_B_min=spp_traits["Tmerge"])
    saveRDS(ecto,
            paste0('microclimate_data/pt',rw,'/!',spp,'.rds'))}
    print(paste('done with ',spp,' point', rw, 'of a lot'))
  }
}
