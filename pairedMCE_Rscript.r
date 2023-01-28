# bash script to programatically run Mechanistic Niche models ----
# by Traci DuBose, last edited 12/16/2022
# increased time length to 10 years, concentrating on points with n spp > 2,
# removed mclapply argument that allowed it to hang, added a second run to the bash script, 
# increased maximum iterations to solve to microclimate and had precipitation fall evenly throughout the day instead of just on midnight

# set up the libraries
.libPaths('/home/tracidubose/R/OOD/Ubuntu-20.04-4.1.0')
print(.libPaths())
#library(devtools)
#devtools::install_github('TraciPopejoy/NicheMapR')
#devtools::install_github('ilyamaclean/microclima')
#install.packages('RNCEP')
library(tidyverse); library(maps);  library(sf); library(parallel); library(stars); library(NicheMapR)

# DATA INPUTS ------------
coress <- 60
#npts<- 4

# paths
PATH <- "./" #arc
PATH_out <- paste0(PATH, "z_results/") # where to save the output
# traits used to parameterize the ectotherm function
traits <- read.csv(paste0(PATH, "inputed_physio_traits_011923.csv")) %>%
  mutate(across(c('s.fossorial', 's.arboreal', 'nocturnal','diurnal','crepuscular'),
                as.numeric),
	 across(c('Tmerge','Tforage_min','Tforage_max','Tbask','Tpref','CTmin','CTmax','Mass'),
		~round(.x, 4))) 
focal_spp<-traits %>% pull(species) %>% unique() # focal species to run models for

# completed points
pts_done <- read.csv('summarized_bodytemps.csv') %>% pull(rowid)

# points at which to run the microclimate model
pts_df<-read.csv(paste0(PATH, "points_to_run.csv")) %>%
	  filter( !(rowid %in% pts_done) )
	 # grepl(',', spp_all)) # prioritize cells that have more than one species 
npts<-nrow(pts_df)

# model parameters
start_date="01/01/2011"
end_date="31/12/2020"

cat('\ndata loaded\n')

# FUNCTION WHICH WE USE TO PARALLALIZE CODE ----------
# it should run the microclimate model and then an ectotherm model for each species found in that grid
anuranMNMs<-function(rw, write.micro.out=F){
  pt<-pts_df[pts_df$rowid==rw,] # identify one row to run based on the argument input into the parellization call
    #cat(paste0(rw, ' row number \n', round(pt[1,1],3), '  ',round(pt[1,2], 3),'\n'))
  ecto_out <- NULL # initialize the dataframe to keep the ectotherm outputs in
  ERR <- 1.5 # error value; changed if microclimate model cannot converge
 # MICROCLIMATE MODEL -------
  micro <- micro_ncep(loc = c(as.numeric(pt[1,1])-+0.00001,as.numeric(pt[1,2])+0.00001), # where to run the model
                     dstart = start_date, dfinish = end_date, # when to run the model
                     DEP = c(0, 2.5,  5,  10,  15,  20,  30,  50,  100,  200), # soil nodes where to estimate soil temperature, cm 
                     minshade = 0, maxshade = 90, runshade=1, # run the model twice, at min and max shade %
                    # soilgrids=1,
		     evenrain=1, # spread daily rain evenly across the day instead of at midnight
                     spatial='/fastscratch/tracidubose/AnuranMNMs/ncep_data/ncep_data', # local repository of climate data ncs
   		     dem.res=1000, # requested resolution DEM from elevatr in meters
                     Usrhyt = 0.01, # local height for organism
		     ERR = ERR, MAXCOUNT=1000, # model convergence parameters
		     run.gads=2) # run GAD simulation in R to avoid errors
  gc() # garbage collection to clear up working memory
  # if the microclimate model didn't converge or encountered an error, adjust the error tolerance
  # the microclimate model below should match the one above
  while(min(micro$metout[,1])==0 & ERR <= 6){
        cat("model crashed, trying a higher error tolerance \n")
        ERR <- ERR + 0.5
  	micro <- micro_ncep(loc = c(as.numeric(pt[1,1])-0.00001,as.numeric(pt[1,2])+0.00001),
                     dstart = start_date, dfinish = end_date,
                     DEP = c(0, 2.5,  5,  10,  15,  20,  30, 50, 100, 200), #cm
                     minshade = 0, maxshade = 90, runshade=1,
                    #soilgrids=1,
		     evenrain=1,
                     spatial='/fastscratch/tracidubose/AnuranMNMs/ncep_data/ncep_data',
                     dem.res=1000, # requested resolution DEM from elevatr in meters
                     Usrhyt = 0.01, # local height for organism
                     ERR = ERR, MAXCOUNT=1000,
		     run.gads=2)
        gc()
	cat(paste('row',rw,'tried error ', ERR, 'and it worked ==', min(micro$metout[,1])!=0, '\n'))
      }

# save the output of the microclimate model in case we need to contextualize specific points
# point level information that doesn't change annually
if(min(micro$metout[,1])==0){ # if the model didn't work, record that
mc.df<-data.frame(rowid=rw,
           lat=micro$longlat[2],
           long=micro$longlat[1],
	   notes='broken model',
	   err=ERR) 
write.csv(mc.df, paste0(PATH_out, rw,'_microloc.csv'), row.names=F)
	}else{
mc.df<-data.frame(rowid=rw,
           lat=micro$longlat[2],
           long=micro$longlat[1],
           tRainfall=sum(micro$RAINFALL),
	   aRainfall=sum(micro$RAINFALL)/(length(micro$dates2)/365),
           evel=micro$elev,
           slope=micro$SLOPE,
           aspect=micro$ASPECT,
	   err=ERR) %>% 
mutate(across(-c(rowid, lat, long), ~round(.x, 3))) 
write.csv(mc.df, paste0(PATH_out, rw,'_microloc.csv'), row.names=F)
# annual microclimate information
bind_cols(ppt=micro$RAINFALL, dat=micro$dates2) %>% mutate(year=substr(dat,1,4)) %>% 
	group_by(year) %>% 
	summarize(mppt=mean(ppt), sdppt=sd(ppt), tppt=sum(ppt), .groups='drop') %>%
left_join(micro$metout %>%
	     bind_cols(dat=micro$dates) %>%
	     mutate(year=substr(dat, 1,4)) %>% 
	     group_by(year) %>% summarize(across(c(TALOC, TAREF), list(mean=mean, sd=sd, min=min, max=max)), .groups='drop'),
	  by='year') %>%
mutate(across(-c(year), ~round(.x, 2)), rowid=rw) %>%
write.csv(paste0(PATH_out, rw, '_ppt_dat.csv'), row.names=F) 
gc()

# ECTOTHERM MODEL STARTS -------
# for loop that only runs a  model if the species is found in that grid cell
  for(spp in focal_spp){
    if(grepl(spp, pt$spp_all)){
      spp_traits <- traits %>% filter(species == spp) # using the traits for each species
      cat(paste('ectotherm model for', spp, '\n'))
      ecto<-ectotherm(Ww_g=spp_traits["Mass"], 
                      shape = 4, # shape based on leopard frog
                      CT_max=spp_traits["CTmax"], 
                      CT_min=spp_traits["CTmin"], 
                      T_pref=spp_traits["Tpref"],
                      # when is activity allowed
                      diurn=spp_traits["diurnal"],
                      nocturn=spp_traits["nocturnal"],
                      crepus=spp_traits["crepuscular"],
                      # can it go into a burrow or climb to cool off
                      burrow=spp_traits["s.fossorial"],
                      climb=spp_traits["s.arboreal"],
                      shdburrow = 1, #the animal's retreat is in the open (0), in the shade when above or below CTmin in sun (1) or in shade always (2)
                      maxdepth = 10, #maximum node depth of the burrow
                      T_F_min=max(c(as.numeric(spp_traits["Tforage_min"]), as.numeric(spp_traits["Tmerge"]))),
                      T_F_max=min(c(as.numeric(spp_traits["Tforage_max"]), as.numeric(spp_traits["CTmax"]))),
                      T_RB_min=spp_traits["Tmerge"],
                      T_B_min=spp_traits["Tmerge"],
                      # microclimate port from parent environment
                      nyears= micro$nyears, 
                      minshades = micro$minshade, 
                      maxshades = micro$maxshade, 
                      alpha_sub = (1 - micro$REFL), 
                      DEP = micro$DEP, KS = micro$KS, b = micro$BB, PE = micro$PE,
		      metout = micro$metout, shadmet = micro$shadmet, 
                      soil = micro$soil, shadsoil = micro$shadsoil, soilmoist = micro$soilmoist, 
                      shadmoist = micro$shadmoist, humid = micro$humid, shadhumid = micro$shadhumid, 
                      soilpot = micro$soilpot, shadpot = micro$shadpot, tcond = micro$tcond, 
                      shadtcond = micro$shadtcond, rainfall = micro$RAINFALL, 
                      preshr = rep(101325 * ((1 - (0.0065 * as.numeric(micro$elev)/288))^(1/0.190284)), 
                                   nrow(micro$metout)),
		      elev = as.numeric(micro$elev), 
                      longitude = as.numeric(micro$longlat[1]), 
                      latitude = as.numeric(micro$longlat[2]),
		      enberr=ifelse(spp == "Lithobates catesbeianus", .015, .055)) # multiplier on mass for error tolerance in heat budget

# write out the outputs of the ectotherm model
bodytemps<-ecto$environ %>% as_tibble() %>%
  mutate(WT=as.numeric(spp_traits["CTmax"])-TC) %>% 
  group_by(YEAR, DOY) %>% 
  summarize(activity0=sum(ACT == 0),
         activity1=sum(ACT == 1),
         activity2=sum(ACT == 2),
         shadeMean=round(mean(SHADE),2),
         across(c(WT,TC,TA,TSUB, TSKY, DEP), list(mean=mean, max=max, min=min)),
         nHoursAboveCTmax=sum(WT < 0),
	 species=spp,
	 rowid=rw, .groups='drop') %>%
	 mutate(across(c(ends_with('mean'), ends_with('min'), ends_with('max')), ~round(.x, 3))) %>%
  group_by(species, rowid) %>%
  summarize(nDays=sum(nHoursAboveCTmax != 0),
  	meannH=mean(nHoursAboveCTmax), 
  	WT_ptmin=min(WT_min),
  	WT_min25=quantile(WT_min, .25),
	WT_min50=median(WT_min),
  	WT_25=quantile(WT_mean, .25),
  	WT_75=quantile(WT_mean, .75),
  	WT_ptmean=mean(WT_mean),
  	nhrF=sum(activity2), .groups='drop')
ecto_out<-bind_rows(ecto_out, bodytemps)
gc()
}
}
write.csv(ecto_out, paste0(PATH_out, rw, "_bodytemps_sum.csv"), row.names=F)
}
return(ecto_out)
}

# function to force-stop any cores that hang with an indefinite error message from FORTRAN
# found here: https://stackoverflow.com/questions/7891073/time-out-an-r-command-via-something-like-try
eval_fork <- function(row, timeout=720){ # feed it a rownumber and the amount of time to wait until killing core

  #this limit must always be higher than the timeout on the fork!
  setTimeLimit(timeout+5);      
  starttime <- Sys.time()

  #dispatch based on method
  ##NOTE!!!!! Due to a bug in mcparallel, we cannot use silent=TRUE for now.
  # what to send to each core (so our MNM function needs to live here)
  myfork<-parallel::mcparallel(anuranMNMs(row))

  #wait max n seconds for a result.
  myresult <- parallel::mccollect(myfork, wait=FALSE, timeout=timeout)
  
  enddtime <-  Sys.time()
  totaltime <- as.numeric(enddtime - starttime, units='secs')  
  #try to avoid bug/race condition where mccollect returns null without waiting full timeout.
  #see https://github.com/jeroenooms/opencpu/issues/131
  #waits for max another 2 seconds if proc looks dead 
  while(is.null(myresult) && totaltime < timeout && totaltime < 2) {
     Sys.sleep(.1)
     enddtime <- Sys.time();
     totaltime <- as.numeric(enddtime - starttime, units="secs")
     myresult <- parallel::mccollect(myfork, wait = FALSE, timeout = timeout);
  }

  #kill fork after collect has returned
  tools::pskill(myfork$pid, tools::SIGKILL);    
  tools::pskill(-1 * myfork$pid, tools::SIGKILL);  

  #clean up:
  parallel::mccollect(myfork, wait=FALSE);

  #timeout?
  if(is.null(myresult)){
    stop("R call did not return within ", timeout, " seconds. Terminating process.", call.=FALSE);      
  }

  #move this to distinguish between timeout and NULL returns
  myresult <- myresult[[1]];

  #reset timer
  setTimeLimit();     

  #forks don't throw errors themselves
  if(inherits(myresult,"try-error")){
    #stop(myresult, call.=FALSE);
    stop(attr(myresult, "condition"));
  }

  #send the buffered response
  return(myresult);  
  
}

# run the eval_fork function, which contains the anuranMNMs function, across many cores
results<-mclapply(pts_df$rowid[sample(1:nrow(pts_df),npts)], # row numbers to run across each core
		  eval_fork,
		 # mc.preschedule = FALSE,
		  mc.cores=coress)
cat(paste(results[[1]]))
cat('\n')

#save_this <- do.call('rbind', results)
# save the outputs of this code
#write.table(save_this, file = paste0("models_run", Sys.Date(), ".csv"),
#	    sep = ",", append = TRUE, row.names = FALSE)

