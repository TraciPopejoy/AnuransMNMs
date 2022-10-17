library(NicheMapR)
ecto_out<-NULL
for(rw in 1:20){
  #dir.create(paste0('microclimate_data/pt',rw))
  micro<-micro_ncep(loc = c(as.numeric(se_pts_df[rw,1]),as.numeric(se_pts_df[rw,2])),
                    dstart = "01/01/2020", dfinish = "31/12/2020",
                    dem.res=1000, # requested resolution of the DEM from elevatr in m
                    DEP = c(0, 2.5,  5,  10,  15,  20,  30,  50,  100,  200), #cm
                    minshade = 0, maxshade = 90, runshade=1,
                    write_input=0, #save=1,
                    #soilgrids=1, #CHECK WHY THIS ISN'T WORKING
                    spatial='../ncep_data',
                    Usrhyt = 0.01) # local height for organism
  #file.copy(list.files(pattern='.Rda'), 
  #          paste0('microclimate_data/pt', rw, '/',list.files(pattern='.Rda')))
  
  for(spp in test_spp){
    if(grepl(spp, se_pts_df[rw,4])){
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
                    maxdepth = 10, #maximum depth of the burrow
                    T_F_min=max(c(19, as.numeric(spp_traits["Tmerge"]))),
                    T_F_max=min(c(35, as.numeric(spp_traits["CTmax"]))),
                    T_RB_min=spp_traits["Tmerge"],
                    T_B_min=spp_traits["Tmerge"])
    #saveRDS(ecto,
    #        paste0('microclimate_data/pt',rw,'/!',spp,'.rds'))}
    
    body_temps<-ecto$environ[,1:5]
    ecto_out<-bind_rows(ecto_out,
                        body_temps %>% as_tibble() %>%
                          mutate(hot_tsm=as.numeric(spp_traits["CTmax"])-TC) %>% 
                          group_by(DOY) %>% 
                          mutate(dayrange=max(TC)-min(TC)) %>%
                          ungroup() %>%
                          summarize(n_total=n(),
                                    n_above=sum(hot_tsm < 0),
                                    max_TC=max(TC),
                                    tsm=min(hot_tsm),
                                    max_dayrange=max(dayrange),
                                    avg_dayrange=mean(dayrange),
                                    X=as.numeric(se_pts_df[rw,1]),
                                    Y=as.numeric(se_pts_df[rw,2]),
                                    species=spp,
                                    rowid=rw))
    }
    print(paste('done with ',spp,' point', rw, 'of', length(rw)))
    }
  }
}

