library(dplyr)
library(airGR)
library(airGRteaching)
library(shiny)
data(L0123001)
view(BasinInfo)
BV_L0123001 <- BasinObs[0001:6000, c("DatesR", "P", "E", "Qmm", "T")]
BI_L0123001 <- BasinInfo
data(L0123002)
BV_L0123002 <- BasinObs[5000:9999, c("DatesR", "P", "E", "Qmm", "T")]
BI_L0123002 <- BasinInfo

if(interactive()) {
   ShinyGR(Dates = BV_L0123002$DatesR,
           Precip= BV_L0123002$P,
           PotEvap= BV_L0123002$E,
           Qobs= BV_L0123002$Qmm,
           Temp= BV_L0123002$T,
           ZInputs= median(BI_L0123002$HypsoData),
           HypsoData = BI_L0123002$HypsoData,
           NLayers= 5, SimPer = c("2004-01-01","2006-12-31"),
           NamesObsBV = "Some Basin", theme = "Saclay")
   }


library(rgdal)
library(raster)
library(hydroTSM)
data("EbroDEM1000m")
dem <-EbroDEM1000m

spplot(dem , scales=list( draw=TRUE, y=list(rot=90)),
       col.regions = topo.colors)


hypso_basin = quantile(as.matrix(dem), probs = seq(0, 1, 0.01),
                       na.rm = TRUE)
plot(hypso_basin)

head(hypso_basin)
inputmodel = CreateInputsModel(FUN_MOD = RunModel_CemaNeigeGR4J,
                               DatesR=BasinObs$DatesR,
                               Precip = BasinObs$P,
                               PotEvap = BasinObs$E,
                               TempMean = BasinObs$T,
                               HypsoData=BasinInfo$HypsoData,
                               ZInputs=median(BasinInfo$HypsoData))
                               
                               
start <- "19900101"
end <- "20081231"
                               

indRun <- seq(from = which(format(BasinObs$Date, format= "%Y%m%d") == start),
              to = which(format(BasinObs$Date, format= "%Y%m%d") == end))
                               
                               
                               


runOptions <- CreateRunOptions(FUN_MOD= RunModel_CemaNeigeGR4J,
                               InputsModel= inputmodel,
                               IndPeriod_Run= indRun,
                               IniStates= NULL, 
                               IniResLevels= NULL,
                               IndPeriod_WarmUp= NULL)
                               
                               
                               
                               
inputsCritEval <- CreateInputsCrit(FUN_CRIT  = ErrorCrit_NSE,
                                   InputsModel= inputmodel,
                                   RunOptions= runOptions,
                                   Obs= BasinObs$Qmm[indRun])

CalibOptions <- CreateCalibOptions(FUN_MOD= RunModel_CemaNeigeGR4J,
                                   FUN_CALIB= Calibration_Michel)



Calib <- Calibration(InputsModel= inputmodel,
                          RunOptions= runOptions,
                          InputsCrit= inputsCritEval,
                          FUN_MOD= RunModel_CemaNeigeGR4J,
                          FUN_CALIB= Calibration_Michel,
                     CalibOptions=CalibOptions)

plot <-Calib$ParamFinalR


sims_calib <- RunModel_CemaNeigeGR4J(InputsModel = inputmodel,
                                     RunOptions = runOptions,
                                     Param = params)
plot(sims_calib$DatesR[366:730], sims_calib$Qsim[366:730], type='l')




startV <- 20090101
endV<- 20121213
indRunV<- seq(from = which(format(BasinObs$Date, format= "%Y%m%d") == startV),
              ro = which(format(BasinObs$Date, format= "%Y%m%d") == endV))

runOptionsV <- CreateRunOptions(FUN_MOD = RunModel_CemaNeigeGR4J,
                                InputsModel= inputsmodel,
                                IndPeriod_Run= indRunV,
                                IniStates=





























                               
