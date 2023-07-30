tictoc::tic()
#--------------------------------library--------------------------------------
library(tidyverse)
#load data----
file_path1 <- "~/R_Reproducible_Research/Data/Spain data/RawData.RData"
file_path2 <- "~/R_Reproducible_Research/Data/Spain data/Merge.RData"
load(file_path1)
load(file_path2)
df <- MGE_ped_acc |> 
  filter(DEATH_30D == 1)
#
save(df,
     file = "DEATH_30D.RData")
#
tictoc::toc()