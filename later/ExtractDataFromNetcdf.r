#--------------------------------------------------------------------------
# Extract subsets of data from the original netcdf files
# from SOUTH to NORTH
# 
# "/data02/PostClim/PostClim_qiyu"
#
#   "../obs/1970.rds,...,2005.rds" => could further include obs.2006-2015
#   "../MPI_SMHI-RCA4/19700101-19701231.rds,...,20010101-20051231.rds"
# 
#--------------------------------------------------------------------------



rm(list=ls())
library(ncdf4)


## SeNorge
#-----------

# Original netcdf is from North to South, so need to
# Flip a matrix, then convert to a vector 
mat2vec <- function(x){
  x.flip <- x[,ncol(x):1]
  c(x.flip)
}

# Boundaries
x_start <- 236; x_size <- 341
y_start <- 740; y_size <- 381

# Write 
dir <- "//hdata/grid/metdata/met_obs_v2.1/tm/archive"
files <- list.files(dir)
subfiles <- files[14:49] # since 14=year=1970; 49=year=2005
years <- seq(1970,2005,1)
filenames <- paste(dir,subfiles,sep = "/")

for (j in 1:36){
  nc <- nc_open(filenames[j],readunlim=F)
  nt <- nc$dim$time$len
  mylist <- list()
  for (i in 1:nt){
    mylist[[i]] <- ncvar_get(nc, "mean_temperature", 
                             start=c(x_start,y_start,i),
                             count=c(x_size, y_size,1))
  }
  tmlist <- lapply(mylist, mat2vec) 
  nc_close(nc)
  year <- years[j]
  dat.mat <- do.call(cbind,tmlist)
  rdsname <- paste("/data02/PostClim/PostClim_qiyu/obs/",year,".rds",sep="")
  saveRDS(dat.mat,rdsname)
}
#-------------


## RCM
#-----------

# Boundaries
x_start <- 217; x_count <- 32
y_start <- 316; y_count <- 34

# Choose one RCM
RCM <- "MPI_SMHI-RCA4"
dir <- paste("/data/MetData/",RCM,"/Temp/ctrl/netcdf",sep="")
filelist <- list.files(dir)
#"tas_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_SMHI-RCA4_v1a_day_19700101-19701231.nc"

for (j in 1:length(filelist)){
  year <- substr(filelist[j],65,81)  #"19700101-19701231"
  nc <- nc_open(file.path(dir,filelist[j]),readunlim=F)
  nt <- nc$dim$time$len
  mylist <- list()
  for (i in 1:nt){
    d <- ncvar_get(nc,"tas",start=c(x_start,y_start,i),count=c(x_count,y_count,1))
    mylist[[i]] <- c(d)
  }
  nc_close(nc) #original netcdf is from South to  North
  dat.mat <- do.call(cbind,mylist)
  rdsname <- paste("/data02/PostClim/PostClim_qiyu/",RCM,"/",year,".rds",sep="")
  saveRDS(dat.mat,rdsname)
}

