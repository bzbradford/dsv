# Process CR1000 data logger outputs and generate DSV and PDays
# Developed by Ben Bradford, UW Madison, 2020 (bbradford@wisc.edu)

# Must call this script with the station name supplied as an argument

setwd("C:/dsv")

source("dsv.R")


# read and log script arguments
args <- commandArgs(trailingOnly = TRUE)

logtext(paste("Script called with args:", args))

arg <- args[1]

if (arg %in% stn_codes) {
  name <- stn_names[[arg]]
  file <- stn_files[[arg]]
  logtext(paste0("Code: ", arg, ", Station/sheet name: ", name, ", File: ", file))
  runDat(arg, name, file)
} else {
  logtext(paste0("ERROR: Invalid station name '", arg, "'"))
  Sys.sleep(3)
}

