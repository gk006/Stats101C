suppressPackageStartupMessages(library(dplyr))
library(readr)
library(readxl)
library(xgboost)

# Read in all relevant data
# Training data:
lafd <- read_csv("D:/UCLA SENIOR YEAR/Spring 2017/Stats 101C/Final/lafdtraining.csv")

# Testing data:
testing <- read_csv("D:/UCLA SENIOR YEAR/Spring 2017/Stats 101C/Final/testing.without.response.csv")

# Fire station data:
FireStations <- read_csv("D:/UCLA SENIOR YEAR/Spring 2017/Stats 101C/Final/FireStations.csv", 
                         col_types = cols(HSENO = col_skip(),PREF_DIR = col_skip(), SUFF_DIR = col_skip(), 
                                          UNITDESC = col_skip(), X_COORD = col_skip(), 
                                          Y_COORD = col_skip(), the_geom = col_skip()))
# Household income and population data
MedianZIP <- read_excel("D:/UCLA SENIOR YEAR/Spring 2017/Stats 101C/Final/MedianZIP-3.xlsx")

''---------------------------------------------------------------------------------------------------''

# Basic information about and modification to the data
# Remove missing values from lafd
lafd <- na.omit(lafd)
# Sample training data, use 20% + 530352(use this amount to estimate error)
sample_size <- 0.2*nrow(lafd)+nrow(testing)
lafd_sample_all <- sample_n(lafd,sample_size)
index <- sample(1:sample_size,nrow(testing))
lafd_sample <- lafd_sample_all[-index,]
validation_sample <- lafd_sample_all[index,]
rm(lafd_sample_all,index,lafd)

# Renaming columns of lafd_sampling and testing
names(lafd_sample)
lafd_sample <- rename(lafd_sample,First_in_District=`First in District`,Emergency_Dispatch_Code=`Emergency Dispatch Code`,Dispatch_Sequence=`Dispatch Sequence`,
        Dispatch_Status=`Dispatch Status`,Unit_Type=`Unit Type`,PPE_Level=`PPE Level`, ICT=`Incident Creation Time (GMT)`)
testing <- rename(testing,First_in_District=`First in District`,Emergency_Dispatch_Code=`Emergency Dispatch Code`,Dispatch_Sequence=`Dispatch Sequence`,
                      Dispatch_Status=`Dispatch Status`,Unit_Type=`Unit Type`,PPE_Level=`PPE Level`, ICT=`Incident Creation Time (GMT)`)

# Modify FireStation data
FireStations <- select(FireStations,First_in_District=FS_CD,Zip=ZIP,Sttype=STTYPE)
nrow(distinct(FireStations,Zip))             # In total, contains 78 unique zipcode of 106 fire stations

# Modify Income and population data
MedianZIP <- select(MedianZIP,Zip,Median,Pop)


lafd_sample <- merge(lafd_sample,MedianZIP,by="ZIP",all.x = T)
testing <- merge(testing,MedianZIP,by="ZIP",all.x = T)

''---------------------------------------------------------------------------------------------------''

# New variables that are created from existing variables
# Help Functions:
# Categorize night as after 18:00 and before 06:00 (ICT is bimodial)
is_night <- function(x){
  return (x>=18*3600 || x<=6*3600)
}
# Categorize as transportations begin from 7:00 to 9:00 am and 16:00 to 19:00
rush_hour <- function(x){
  if(x>=7*3600 && x<=9*3600){
    r <- T
  }else if(x>=16*3600 && x<=19*3600){
    r <- T
  }else{
    r <- F
  }
  return (r)
}
# Hour of the day (May collinear with "night")
hour_of_day <- function(x){
  return (x %/% 3600)
}


#lafd_sample <- lafd_sample %>% rowwise() %>% mutate(night = is_night(ICT))
# sapply seems quicker...
lafd_sample <- mutate(lafd_sample,night=sapply(as.numeric(ICT),is_night),
                      rush_hour=sapply(as.numeric(ICT),rush_hour),
                      hour=sapply(as.numeric(ICT),hour_of_day))

testing <- mutate(testing,night=sapply(as.numeric(ICT),is_night),
                  rush_hour=sapply(as.numeric(ICT),rush_hour),
                  hour=sapply(as.numeric(ICT),hour_of_day))

''---------------------------------------------------------------------------------------------------''

# New variables that are found using external sources/ Merge dataframes

# can safely use inner_join because all cases of fire stations and income are included
lafd_sample <- inner_join(lafd_sample,FireStations,by="First_in_District")
lafd_sample <- inner_join(lafd_sample,MedianZIP,by="Zip")

# Boosting

## Q2
```{r}
require(MASS)
data(Boston)
View(Boston)
help(Boston)

library(xgboost)
set.seed(385)
train <- sample(1:nrow(Boston),nrow(Boston)/2)
boston.train <- Boston[train,]
boston.train.data <- as.matrix(boston.train[,1:13])
boston.train.label <- as.matrix(boston.train[,14])
boston.train <- list(boston.train.data,boston.train.label)

boston.test.data <- as.matrix(boston.test[,1:13])
boston.test.label <- as.matrix(boston.test[,14])
boston.test <- list(boston.test.data,boston.test.label)

# Boosting
xgboost.boston <- xgboost(data = boston.train[[1]], label = boston.train[[2]],verbose = 0, objective = "reg:linear", max_depth = 2, nround = 300)   #???nround? max_depth?
pred <- predict(xgboost.boston,boston.test[[1]])
mean((pred-boston.test[[2]])^2)
xgboost.boston <- xgboost(data = boston.train[[1]], label = boston.train[[2]],verbose = 0, objective = "reg:linear", max_depth = 3, nround = 300)   #???nround? max_depth?
pred <- predict(xgboost.boston,boston.test[[1]])
mean((pred-boston.test[[2]])^2)
xgboost.boston <- xgboost(data = boston.train[[1]], label = boston.train[[2]],verbose = 0, objective = "reg:linear", max_depth = 4, nround = 300)   #???nround? max_depth?
pred <- predict(xgboost.boston,boston.test[[1]])
mean((pred-boston.test[[2]])^2)

# Use max_depth = 3, get MSE 11.75, it is slightly lower than 11.8 in book
```
