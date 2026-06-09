# Project       : Lecture on using statistical learning models with household survey microdata
# Last update   : 09/06/2023
# Author        : Manuel Tomas (manuel.tomas@bc3research.org)
# Institution   : Basque Centre for Climate Change (BC3)

# [] Objective ----

# Load raw .csv files and generate two files:
# hbs_h -> Household data: characteristics + consumption of households
# hbs_m -> Household members data: characteristics of household members

# Clear workspace
rm(list = ls(all = TRUE))

# Set language
Sys.setenv(LANG = "en")

# Set start time
start_time = Sys.time()

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
setwd(paste0(path))

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

# [] Load hbs microdata ----

# Set working directory
setwd(paste0(path, "/data"))

# Define years
years = as.numeric(list.files())

# By year
for (year in years) {

# Set working directory
setwd(paste0(path, "/data/", year, "/CSV"))
  
# Load data by year
for (f in 1:length(files_en)) {

# Define file names
original_file = files_sp[f]
final_file    = files_en[f]

# Load file
if (year < 2016) {
eval(parse(text = paste0(final_file,  " <- read.table('", original_file, "_", year, ".csv', header = TRUE, sep = ',', stringsAsFactors = FALSE)")))
} else if (year > 2022)  {
  eval(parse(text = paste0(final_file,  " <- read.table('", original_file, "_", year, ".tab', header = TRUE, sep = '\t', stringsAsFactors = FALSE)")))
} else {
  eval(parse(text = paste0(final_file,  " <- read.table('", original_file, "_", year, ".csv', header = TRUE, sep = '', stringsAsFactors = FALSE)")))
}
}

# Harmonize variable "CODIGO"
if (typeof(hbs_e$CODIGO) == "integer"){
  hbs_e$CODIGO <- as.character(hbs_e$CODIGO)
  hbs_e[nchar(hbs_e$CODIGO)==4,]$CODIGO <- paste0("0",  hbs_e[nchar(hbs_e$CODIGO)==4,]$CODIGO)
}

# Create consumption data by type (expenditure & quantity)
for (c in seq_along(new_names)) {
  
  new_name <- new_names[c]
  old_name <- old_names[c]
  
  tmp <- hbs_e %>%
    mutate(
      CODIGO = paste0(new_name, CODIGO),          
      !!new_name := .data[[old_name]] / FACTOR    
    ) %>%
    reshape2::dcast(
      NUMERO ~ CODIGO,
      value.var = new_name,
      fun.aggregate = sum,
      na.rm = TRUE
    )
  
  if (c == 1) {
    consumption_data <- tmp
  } else {
    consumption_data <- left_join(consumption_data, tmp, by = "NUMERO")
  }
}

# Check the correct creation of the consumption data
if (!year %in% c(2016, 2020) & nrow(consumption_data) != nrow(hbs_h)){stop()} # For 2016 and 2020 there is one household that doesn't have consumption information

# Create total consumption variable
hbs_h <- hbs_h %>% mutate (EUR_HE00 = GASTOT/FACTOR)

# Transfer consumption data into household data file
hbs_h <- left_join(hbs_h, consumption_data, by = "NUMERO")

# Check procedure
if (round(sum(hbs_h$EUR_HE00, na.rm = TRUE)) == round(sum(rowSums(dplyr::select(consumption_data, contains('EUR_HE'))), na.rm = TRUE))){
  print(paste0(year, ": Done correctly")) } else { stop()}

# Set working directory
setwd(paste0(path, "/data/", year))

# Delete the folder generated in previous runs (if it exists) 
if(dir.exists(paste0(path, "/hbs/", year))){unlink(paste0(year), recursive = TRUE)}

# Set working directory
setwd(paste0(path, "/data"))

# Save input data
write.csv(hbs_h, paste0("hbs_h_", year, ".csv"), row.names = FALSE)
write.csv(hbs_m, paste0("hbs_m_", year, ".csv"), row.names = FALSE)
  
}