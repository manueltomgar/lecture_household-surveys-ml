# Project       : Lecture on using statistical learning models with household survey microdata
# Last update   : 09/06/2026
# Author        : Manuel Tomas (manuel.tomas@bc3research.org)
# Institution   : Basque Centre for Climate Change (BC3)

# [] Objective ----

# Download and prepare Spanish Household Budget Survey microdata

# [] Preliminaries ----

# Clear workspace
rm(list = ls(all = TRUE))

# Install and load required R packages
packages.loaded <- installed.packages()
packages.needed <- c(
  "dplyr",
  "openxlsx",
  "httr",
  "downloader",
  "stringr",
  "here",
  "archive"
)

# Install and load any required R-package
packages_loaded <- installed.packages()
packages_needed <- c ( "dplyr"       ,
                       "openxlsx"    ,
                       "stringr"     ,
                       "here"        )
for ( p in packages_needed) {
  if (!p %in% row.names(packages_loaded)) install.packages(p)
  eval(bquote(library(.(p))))
}

# Define main path
path <- here()

# Set working directory
setwd(path)

# Load string vectors with auxiliary information needed
vectors <- read.table('vectors.csv',
                      header = TRUE,
                      sep = ',',
                      stringsAsFactors = FALSE,
                      skipNul = TRUE,
                      blank.lines.skip = TRUE)
for (a in names(vectors)) {
  eval(parse(text = paste0(a, " <- vectors[!is.na(vectors$", a,") & nchar(vectors$", a, ") > 0,]$", a)))
}

# [] Download surveys from the Spanish Statistical Office ----
# This applies from 2016 onwards only.
# Previous years cannot be downloaded automatically.

# Define structural part of the URL
base.url <- "https://www.ine.es/ftp/microdatos/epf2006/"

# Define initial year
initial.year <- 2023

# Define final year
final.year <- 2023

# Alternative automatic final year:
# final.year <- if (as.numeric(str_sub(Sys.Date(), start = 6, end = 7)) > 7) {
#   as.numeric(str_sub(Sys.Date(), start = 1, end = 4)) - 1
# } else {
#   as.numeric(str_sub(Sys.Date(), start = 1, end = 4)) - 2
# }

# Create main data folder if it does not exist
if (!dir.exists(file.path(path, "data"))) {
  dir.create(file.path(path, "data"))
}

# By year
for (year in initial.year:final.year) {
  
  # Set working directory to data folder
  setwd(file.path(path, "data"))
  
  # Delete the folder generated in previous runs if it exists
  year.folder <- file.path(path, "data", as.character(year))
  
  if (dir.exists(year.folder)) {
    unlink(year.folder, recursive = TRUE)
  }
  
  # Create year data folder
  dir.create(year.folder, recursive = TRUE)
  
  # Define zip file
  file <- paste0("datos_", year, ".zip")
  
  # Define URL
  url <- paste0(base.url, file)
  
  # Set destination file
  destination <- file.path(year.folder, file)
  
  # Download microdata
  downloader::download(
    url,
    destfile = destination,
    mode = "wb"
  )
  
  # Unzip main INE zip using archive
  # This avoids the "invalid multibyte string" error
  archive::archive_extract(
    archive = destination,
    dir = year.folder
  )
  
  # Delete main zip
  unlink(destination)
  
  # Create list of nested zip files
  list.folders <- list.files(
    path = year.folder,
    pattern = "\\.zip$",
    ignore.case = TRUE,
    recursive = TRUE,
    full.names = TRUE
  )
  
  # Unzip nested zip files
  if (length(list.folders) > 0) {
    
    for (folder in list.folders) {
      
      archive::archive_extract(
        archive = folder,
        dir = dirname(folder)
      )
      
      # Delete nested zip
      unlink(folder)
    }
  }
}

# [] End ----