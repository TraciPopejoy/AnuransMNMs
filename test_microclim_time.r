.libPaths(.libPaths()[3:1])
#library(devtools)
#devtools::install_github('mrke/NicheMapR')
library(tidyverse); library(sf); library(parallel); library(NicheMapR); library(lubridate)
#install.packages('futile.logger')
library(futile.logger)

testmc<-function(x){
end_day<-"31/12/2020"
start_day<-paste0("01/01/",x)
#start_day<-format(std, "%d/%m/%Y")
start.t<-Sys.time()
micro <- micro_ncep(loc = c(-78.40086, 37.31045),
                     dstart = start_day, dfinish = end_day,
                     DEP = c(0, 2.5,  5,  10,  15,  20,  30,  50, 75,  100), #cm
                     minshade = 0, maxshade = 90, runshade=1,
                    # soilgrids=1,
                     spatial='./ncep_data/ncep_data',
                     dem.res=1000, # requested resolution DEM from elevatr in meters
                     Usrhyt = 0.01)
end.t<-Sys.time()
out<-data.frame(totaltime=end.t-start.t,
		st=start.t,
		et=end.t,
		start_day=start_day,
		end_day=end_day,
		obj.size=as.character(format(object.size(micro), 'Mb')),
		numrow=nrow(micro$metout),
		x=x,
		nyear=2021-as.numeric(x))
cat(paste(x, 'model output completed\n'))
return(out)
}

results <- mclapply(2020:2012, testmc, mc.cores = 4) # outputs list from each rep of function I think
save_this <- do.call('rbind', results)
# save the outputs of this code
write.csv(save_this, file = paste0("micro_", Sys.Date(), ".csv"), row.names=F)

