# set up the libraries
.libPaths('/home/tracidubose/R/OOD/Ubuntu-20.04-4.1.0')
print(.libPaths())
library(tidyverse)

micro_out<-NULL
for(i in list.files('z_results/', pattern='microloc', full.names = T)){
  micro_out<-bind_rows(micro_out,
                      read.csv(i))
}
write.csv(micro_out, 'microclimate_info.csv', row.names=F)

clim_out<-NULL
for(i in list.files('z_results/', pattern='ppt_dat', full.names=T)){
  clim_out<-bind_rows(clim_out,
		      read.csv(i))
}
write.csv(clim_out, 'climate_data.csv', row.names=F)

ecto_out<-NULL
#for(i in list.files('z_results', pattern='bodytemps_sum.csv', full.names = T)){
#  ecto_out<-bind_rows(ecto_out,
#                      read.csv(i) %>% 
#                        as_tibble() %>%
#			# need to reduce the file size so calculating summary statistics
#			left_join(data.frame(YEAR=c(2012:2020,NA),
#					     ndur=c(rep(c(366,365,365,365),2),366, 3288)/3288),
#				  by='YEAR') %>%
#                        group_by(species, rowid) %>%
#			dplyr::summarize(nDaysT=sum(nDays),
#                                 meannH=weighted.mean(meannH, ndur), 
#                                  WT_ptmin=min(WT_ptmin),
#				  WT_min25mean=weighted.mean(WT_min25, ndur),
#                                  WT_25=weighted.mean(WT_25, ndur),
#                                  WT_75=weighted.mean(WT_75, ndur),
#                                  WT_ptmean=weighted.mean(WT_mean, ndur),
#                                  nhrF=sum(nhrF), .groups='drop') %>%
#			 mutate(across(-c(species, rowid, nDays, nhrF), ~round(.x, 3))))
#}
for(i in list.files('z_results', pattern='bodytemps_sum.csv', full.names = T)){
  ecto_out<-bind_rows(ecto_out,
                      read.csv(i))
}

write.csv(ecto_out, 'summarized_bodytemps.csv', row.names=F)

cat(paste('missing bt', micro_out$rowid[!(micro_out$rowid %in% unique(ecto_out$rowid))], '\n'))
write.table(paste0('results/', micro_out$rowid[!(micro_out$rowid %in% unique(ecto_out$rowid))],'_microloc.csv'), 'missingbt.txt', sep="/t", quote=F, row.names=F)
cat(paste('missing micro', ecto_out$rowid[!(ecto_out$rowid %in% unique(micro_out$rowid))], '\n'))

# Outputs  in a raster ---------------
library(sf); library(maps);library(stars)
# study extent
se <- readRDS('spatial_extent.rds') %>%
  st_as_sf()
#st_crs(se)

# create raster of study extent
se_r<-st_rasterize(se, dx=1, dy=1) %>% #turn into a raster of 1x1 kilometers
  st_as_sf(as_points = FALSE, merge = FALSE) %>%  #turn into a polygon
  rowid_to_column() # add an id column
st_area(se_r[1,])

focal_spp <- unique(ecto_out$species)
# convert the ecto_out to a wide dataframe so that each attribute is the result of a single species
ecto_wt<-ecto_out %>% select(rowid, species, WT_ptmin) %>%
  pivot_wider(names_from=species, values_from=WT_ptmin)
#ecto_25<-ecto_out %>% select(rowid, species, WT_min25) %>%
#          pivot_wider(names_from=species, values_from=WT_min25)

# join the spatial polygon with the ectotherm output
ecto_se<-se_r %>% right_join(ecto_wt, by='rowid') %>%
  left_join(ecto_out %>% group_by(rowid) %>%
              summarize(n_spp=n(),
                        mean_WTmin=round(mean(WT_ptmin),4),
                        sd_WTmin=round(sd(WT_ptmin),4),
                        mean_WT25=round(mean(WT_min25),4),
                        sd_WT25=round(sd(WT_min25),4)),
            by='rowid')

# calculate species level sensitivity
all_res<-NULL
for(spp in focal_spp){
  tettt <- ecto_se %>% select(rowid, ID, all_of(spp)) %>%
    rename(value=all_of(spp)) %>%
    filter(!is.na(value)) 
  resul <- tettt %>% as_tibble() %>%
    summarize(species=spp,
              mean_WTmin=round(mean(value),5),
              below0=sum(value < 0),
              below2=sum(value < 2),
              n=n()) %>%
    bind_cols(area=tettt %>% st_union() %>% st_area(.))
  all_res<- bind_rows(all_res, resul)
}
all_res
write.csv(all_res, 'anuranMNMsensitivity.csv')

ecto_rast <- ecto_se %>% st_rasterize()

# export as rds
saveRDS(ecto_rast, "WT_stars.rds")

# plot most abundant species
pdf("q_WT.pdf")

# Creating a plot
plot(ecto_rast[10], breaks='equal')

# Closing the graphical device
dev.off() 
