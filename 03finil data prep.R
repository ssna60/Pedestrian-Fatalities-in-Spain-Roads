tictoc::tic()
#--------------------------------library--------------------------------------
library(tidyverse)
library(vtable)
library(caret)
library(janitor)
library(tidymodels)
library(arules)
#load data----
load("InitialData.RData")

#names modification----
df <- df3
#####

str(df)
# Convert each column to factor
df <- as.data.frame(lapply(df, factor))
# Verify the conversion
str(df)

df_rec <- 
  recipe( ~ ., data = df)  |>  
  step_dummy(all_predictors(), one_hot = TRUE)  |>  
  prep()

df_rec |> 
  bake(new_data = NULL) |> 
  names()



New_colnames <- c("Young", "Middle-aged", "old", "Araba", 
  "Albacete", "Alicante", "Almería", "Ávila", 
  "Badajoz", "Balears", "Barcelona", "Burgos", 
  "Cádiz", "Castellón", "CiudadReal", "Córdoba", 
  "Coruña", "Cuenca", "Girona", "Granada", 
  "Guadalajara", "Gipuzkoa", "Huelva", "Huesca", 
  "Jaén", "León", "Lleida", "Rioja", 
  "Lugo", "Madrid", "Málaga", "Murcia", 
  "Navarra", "Ourense", "Asturias", "Palencia", 
  "Palmas", "Pontevedra", "Tenerife", "Cantabria", 
  "Segovia", "Sevilla", "Soria", "Tarragona", 
  "Teruel", "Toledo", "Valencia", "Valladolid", 
  "Bizkaia", "Zamora", "Zaragoza", "Rsp_Yes", 
  "Rsp_No", "Rsp_Unknown", "January", "February", 
  "March", "April", "May", "June", 
  "July", "August", "September", "October", 
  "November", "December", "Hour_6-10", "Hour_10-14", 
  "Hour_14-18", "Hour_18-22", "Hour_22-6", "Zone_Road", 
  "Zone_Crossing", "Zone_Street", "NoSidewalk", "Sidewalk", 
  "PedEntry_No", "PedEntry_Yes", "Female", "Male", 
  "Sunday", "Monday", "Tuesday", "Wednesday", 
  "Thursday", "Friday", "Saturday", "Specific_SpeedLimit",
  "Generic_SpeedLimit", "SingleVeh", "MultiVeh", "SinglePed", 
  "MultiPed", "NoDayLight", "DayLight", "SeparateLongitLine", 
  "SeparateMedian", "Separation_Other")


DF <- df_rec[["template"]]
DF2 <- DF
colnames(DF2)[1:dim(DF)[2]] <- New_colnames

#################################

sumtable(DF2)


#Data transformation for analysis ~ Single ------------------------------------------------------
#The goal is to fill the k matrix
#For now, all the values of the k matrix are filled with zeros
k <- matrix(0, dim(DF2)[1], 15)
for (i in 1:dim(DF2)[1]) {
  k[i,] <- which(DF2[i,] == 1)
}

a=c(t(k))
s <- t(rep(1:dim(DF2)[1], each = 15))

Single <- matrix(data = c(s, a), nrow = 15*dim(DF2)[1], ncol = 2, byrow = FALSE,
                 dimnames = NULL)
head(Single,20)
tail(Single)
#
orders <- as.data.frame(Single)
orders <-rename(orders, 
                transactionID = V1,
                item = V2)

#
# Using a CSV ####
# Create a temporary directory

dir.create(path = "tmp", showWarnings = FALSE)

# Write our data.frame to a csv
write.csv(orders, "./tmp/tall_transactions.csv")

# Read that csv back in
order_trans <- read.transactions(
  file = "./tmp/tall_transactions.csv",
  format = "single",
  sep = ",",
  cols=c("transactionID","item"),
  header = TRUE,
  rm.duplicates = T
)
summary(order_trans)
inspect (head(order_trans))
#
Data <- order_trans
names(DF2)
class(Data)
inspect (head (Data, 4))
itemLabels(Data)
for (i in 1:dim(DF)[2]) {
  itemLabels(Data)[itemLabels(Data) == as.character(i)] <- names(DF2)[i]
}
itemLabels(Data)
#
save(DF, DF2, orders, Data, file = "DF_Readyforanalysis.RData")
#
tictoc::toc()
