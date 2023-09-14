#Load the required libraries
library(readxl)
library(airGR)
library(tidyverse)


#import the excel files for precipitation,evaporation and inflow.
#the location path will change as per the directory.
FOL_45_Precip <-
  read_excel("C:/Users/Hp/Desktop/R Programming/ecornell/FOL_45 Precip.xlsx")

FOL_74_evap <-
  read_excel("C:/Users/Hp/Desktop/R Programming/ecornell/FOL_74 evap.xlsx")

FOL_76_Inflow <-
  read_excel("C:/Users/Hp/Desktop/R Programming/ecornell/FOL_76 Inflow.xlsx")

#clean and tidy the data before using.
#first we convert the dates into usable format
FOL_45_Precip$`OBS DATE` <-
  as.Date(as.character(FOL_45_Precip$`OBS DATE`), format = "%Y%m%d")

#now convert the dates into calender time.
DatesR <- FOL_45_Precip$`OBS DATE` %>% as.POSIXct()

#subset the inflow dataframe,as only 7th column is needed.
inflows <- FOL_76_Inflow[, 7]

#Convert the data into numeric form. Also there are 3 NAs in inflow,they cant be put to zero
# so this part needs some review.
inflows <- as.numeric(gsub(",", "", inflows$VALUE, fixed = T))

#convert the precipitation into numeric and remove the NAs                                     
Precip <- FOL_45_Precip$VALUE %>% as.numeric()
Precip[is.na(Precip)] = 0

#convert the evaporation into numeric
Evap <- FOL_74_evap$VALUE %>% as.numeric()


#Run the model.(conversion of units  applied)
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR4J, DatesR = DatesR,
                                 Precip = Precip*25.4,PotEvap = Evap*0.529)

Ind_Run <- seq(which(format(DatesR, format = "%Y-%m-%d")=="2012-01-01"),
               which(format(DatesR, format = "%Y-%m-%d")=="2022-01-01"))

RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run)

Param <- c(X1 = 350, X2 = 1, X3 = 90, X4 = 1.7)


OutputsModel <- RunModel_GR4J(InputsModel = InputsModel,
                              RunOptions = RunOptions, Param = Param)

plot(OutputsModel)
plot(OutputsModel, Qobs = inflows[Ind_Run])

