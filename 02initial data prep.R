tictoc::tic()
#--------------------------------library--------------------------------------
library(tidyverse)
library(vtable)
library(caret)
library(janitor)
#load data----
load("DEATH_30D.RData")

#names modification----
names(df)
df <- df |> clean_names("upper_camel", abbreviations = c("ID", "KM"))
names(df)

#variables evaluation
class(df)
str(df)
sumtable(df)
#
(nzv <- nearZeroVar(df, saveMetrics= TRUE))
dim(df)
nzv <- nearZeroVar(df)
names(df[nzv])
df1 <- df[, -nzv]
dim(df1)
sumtable(df1)
#
for (col in names(df)) {
  uniq_val <- unique(df[[col]])
  n_uniq <- length(uniq_val)
  n_miss <- sum(is.na(df1[[col]]))
  if (n_uniq < 100) {
    print(paste("Column:", col, "- Number of unique values:", n_uniq))
    print(paste("Column:", col, "- Number of missing values:", n_miss))
    tbl <- table(df[[col]])
    print(paste("Column:", col, "- Ordered Frequency Table:"))
    print(tbl[order(tbl, decreasing = TRUE)])
  } 
}
#ProvinceX vs ProvinceY
sum(is.na(df$ProvinceX))
sum(is.na(df$ProvinceY))
eval <- count(df, ProvinceX, ProvinceY,
              sort = T) |>  print()


#variables selection----
names(df)
df1 <- df |> 
  select(Age, Sex, Province = ProvinceY, AccResponsible, Month, Hour, 
         Weekdays, Zone, TotalVehicles, TotalPedestrians, 
         LightningCondition, Speed, Sidewalk, 
         SeparateLongitLine, SeparateMedian, PedestrianEntry)
#
for (col in names(df1)) {
  uniq_val <- unique(df1[[col]])
  
  n_uniq <- length(uniq_val)
  n_miss <- sum(is.na(df1[[col]]))
  if (n_uniq < 100) {
    print(paste("Column:", col, "- Number of unique values:", n_uniq))
    print(paste("Column:", col, "- Number of missing values:", n_miss))
    tbl <- table(df1[[col]])
    print(paste("Column:", col, "- Ordered Frequency Table:"))
    print(tbl[order(tbl, decreasing = TRUE)])
  } 
}
##
df2 <- replace(df1, df1 == 999 | df1 == 998, NA)
#
for (col in names(df2)) {
  uniq_val <- unique(df2[[col]])
  
  n_uniq <- length(uniq_val)
  n_miss <- sum(is.na(df2[[col]]))
  if (n_uniq < 100) {
    print(paste("Column:", col, "- Number of unique values:", n_uniq))
    print(paste("Column:", col, "- Number of missing values:", n_miss))
    tbl <- table(df2[[col]])
    print(paste("Column:", col, "- Ordered Frequency Table:"))
    print(tbl[order(tbl, decreasing = TRUE)])
  } 
}
#

df3 <- df2
#AccResponsible 
df3 <- mutate(df3, AccResponsible = 
                 case_when( AccResponsible  == 1
                            ~ 1, #"Yes"
                            AccResponsible  == 2
                            ~ 2, #"No"
                            TRUE 
                            ~ 3)) #"Unknown"
#Age
df3 <- mutate(df3, Age = 
                case_when(Age > 25 & Age < 65
                          ~ 2,
                          Age >= 65 
                          ~ 3,
                          TRUE
                          ~ 1))
#Sex
df3 <- mutate(df3, Male = 
                case_when( Sex == 1
                           ~ 1,
                           TRUE 
                           ~ 0))

#weekday / day_number 
#Sunday1
#Monday2
#Tuesday3
#Wednesday4
#Thursday5
#Friday6
#Saturday7
day_names <- c("Sunday", "Monday", "Tuesday", "Wednesday", 
               "Thursday", "Friday", "Saturday")
df3$day_num <- match(df3$Weekdays, day_names)

#zone
#1 Road
#2 Crossing
#3 Street

#speed
df3 <- mutate(df3, GenericSpeedLimit = 
                case_when(Speed == 1
                           ~ 1, 
                           TRUE
                           ~ 0))

#Hour
df3 <- mutate(df3, Hour = 
                case_when( Hour >= 6 &  Hour < 10
                           ~ 1,
                           Hour >= 10 &  Hour < 14
                           ~ 2,
                           Hour >= 14 &  Hour < 18
                           ~ 3,
                           Hour >= 18 &  Hour < 22
                           ~ 4,
                           TRUE
                           ~ 5))

#TotalVehicles vs TotalPedestrians
eval <- count(df3, TotalVehicles, TotalPedestrians,
              sort = T) |>  print()

#MultiVeh
df3 <- mutate(df3, MultiVeh = 
                case_when( TotalVehicles == 1
                           ~ 0,
                           TotalVehicles > 1
                           ~ 1))
#MultiPed
df3 <- mutate(df3, MultiPed = 
                case_when( TotalPedestrians == 1
                           ~ 0,
                           TotalPedestrians > 1
                           ~ 1))
#MultiVeh vs MultiPed
eval <- count(df3, MultiVeh, MultiPed,
              sort = T) |>  print()

#DayLight
df3 <- mutate(df3, DayLight = 
                case_when( LightningCondition == 1
                           ~ 1,
                           TRUE
                           ~ 0))
#Sidewalk
df3 <- mutate(df3, Sidewalk = 
                case_when( Sidewalk == 3 | Sidewalk == 4
                           ~ 1, #Yes
                           TRUE
                           ~ 0)) #No

#SeparateMedian  vs SeparateLongitLine 
eval <- count(df3, SeparateLongitLine , SeparateMedian,
              sort = T) |>  print()

#Separation
df3 <- mutate(df3, Separation = 
                case_when(SeparateLongitLine == 1 & SeparateMedian == 0
                           ~ 1, #"SeparateLongitLine"
                          SeparateLongitLine == 0 & SeparateMedian == 1
                          ~ 2, #"SeparateMedian"
                           TRUE
                           ~ 3)) #"Other"



df3 <- subset(df3, select = -c(Sex,TotalVehicles, TotalPedestrians,
                               LightningCondition, SeparateLongitLine,
                               SeparateMedian, Weekdays, Speed))

#
cat("\014")
#
str(df3)
sumtable(df3)
#
for (col in names(df3)) {
  uniq_val <- unique(df3[[col]]) 
  n_uniq <- length(uniq_val)
  n_miss <- sum(is.na(df3[[col]]))
  if (n_uniq < 100) {
    print(paste("Column:", col, "- Number of unique values:", n_uniq))
    print(paste("Column:", col, "- Number of missing values:", n_miss))
    tbl <- table(df3[[col]])
    print(paste("Column:", col, "- Ordered Frequency Table:"))
    print(tbl[order(tbl, decreasing = TRUE)])
  } 
}
#
#
InitialData <- df3
save(InitialData,
     file = "InitialData.RData")
#
tictoc::toc()






