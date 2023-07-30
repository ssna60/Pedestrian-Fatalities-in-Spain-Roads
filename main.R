#main
#Purpose: We want to run the four files that exist for this project...
tictoc::tic()
#

files <- c("01data.R", 
           "02initial data prep.R", 
           "03finil data prep.R", 
           "04Market Basket Analysis.R")

file_path <- "~/R_Reproducible_Research/Pedestrian Fatalities in Spain Roads"


for (file in files) {
  source(file.path(file_path, file))
}

#
tictoc::toc()