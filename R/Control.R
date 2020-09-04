# Control file for EPA's RPGen
# Designed and written for EPA by WGG of ICF, September 26, 2017
# Updated by AE of ORAU, 2020.
rm(list=ls())


RPGen.setup = function() {

  inpath   <- "./data/"
  outpath  <- "./output/"
  run      <- "runfile.txt"
  pums     <- "RPGen PUMS"
  ahs      <- "RPGen AHS.rda"
  recs     <- "RPGen RECS.rda"
  
  f        <- as.list(c(inpath,outpath,run,pums,ahs,recs))
  names(f) <- c("inpath","outpath","run","pums","ahs","recs")
  files   <<- f
  
  
  suppressPackageStartupMessages(TRUE)
  # Load required packages and source all code modules.
  library("data.table")
  library("stringr")
  library("plyr")
  library("dplyr")
  library("dtplyr")
  library("ggplot2")
  library("bit64")
  library("httk")
  library("msm")
  library("truncnorm")
  library("survey")
  source("./R/PopGen.R")
  source("./R/Housing.R")
}    

RPGen.setup()

RPGen.run = function(runfile=NULL) {
  
  pop  <<- popgen(runfile)
  dir  <- paste0(files$outpath,g$run.name)
  if(!file.exists(dir)) dir.create(dir,recursive=TRUE)
  filename <- paste0(files$outpath,g$run.name,"/pop.csv") 
  pophouse <<- match.pop.housing(pop,NULL,NULL)
  write.csv(pop,filename,row.names = FALSE)
  filename <- paste0(files$outpath,g$run.name,"/pophouse.csv") 
  write.csv(pophouse,filename,row.names = FALSE)
  gc()
  cat("Housing generator completed: R object = 'pophouse', filename =",filename,"\n")
}  

RPGen.run("test.txt")




runfile = "test.txt"
