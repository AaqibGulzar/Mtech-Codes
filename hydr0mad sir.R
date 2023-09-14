#install.packages(c("zoo", "latticeExtra", "polynom", "car", "Hmisc","reshape"))

#install.packages("hydromad", 
#                 repos="http://hydromad.catchment.org")
library("hydromad")
library("DEoptim")
library("dream")
library("zoo")
library("sensitivity")

#source(file = "/Users/munirnayak/Desktop/IIT-Indore/snow_sac_new.R")

Folsom_area <- 4740 #km2, actually 4856km2, USBR https://www.usbr.gov/projects/index.php?id=74



setwd(dir = "/Users/munirnayak/Desktop/NIT_Sgr/Courses_taught/Autumn 2021/Programming in Hydrology/Class_tutorials/")

qdat <- read.table( "Folsom.inflow.data.txt", sep = " ", 
                    header=TRUE )

head(qdat)
tail(qdat)

q2007 <- qdat[(qdat$year >=1950 & qdat$year <=2007), ]
p2007 <- read.table("areal_average_precip_1950-2007.csv",  sep = ",", 
                    header=FALSE)

tmax2007 <- read.table("areal_average_tmax_1950-2007.csv", sep = ",", 
                       header=FALSE)

tmin2007 <- read.table( "areal_average_tmin_1950-2007.csv", sep = ",", 
                        header=FALSE)


pqdat <- cbind(q2007, p2007, tmax2007, tmin2007)
colnames(pqdat) <- c("Year",  "Month" ,"Day" ,  "flow" , "P", "Tmax.daily", "Tmin.daily")
row.names(pqdat) <- NULL

#pqdat$Date <- paste(pqdat$year, pqdat$month,pqdat$day, sep="/")
#pqdat$Date <- as.Date(cbind(pqdat$day, pqdat$month, pqdat$year) , "%d/%m/%Y")

pqdat$Date <- with(pqdat, as.Date(ISOdate(Year,Month, Day)))
pqdat$P[pqdat$P < 0] <- NA
pqdat$Q[pqdat$flow < 0] <- NA
pqdat$Q <- convertFlow(pqdat$flow,  from="m^3 / day", to = "mm", area.km2 = Folsom_area) # to mm for example 3401229/4746000000 = m, then *1000 = mm


folsomPQ <- zoo(cbind(P = pqdat$P, Q = pqdat$Q, E = pqdat$Tmax.daily), pqdat$Date, frequency = 1)

# folsomPQ <- zoo(cbind(P = pqdat$P, Q = pqdat$Q, E = rowMeans( cbind(pqdat$Tmax.daily, pqdat$Tmin.daily)  )), pqdat$Date, frequency = 1)


folsomPQ50_80 <- window(folsomPQ, start = "1950-01-01", end = "1979-12-31")
folsomPQ80_08 <- window(folsomPQ, start = "1980-01-01", end = "2007-12-31")

summary(folsomPQ)
xyplot(folsomPQ[1:365])

#### GET three day aggregates##
dates_p <- seq(as.Date("1950-01-01", format="%Y-%m-%d"), as.Date("2007-12-31", format="%Y-%m-%d"), by=1)

start <- as.Date("1950-01-01",format="%Y-%m-%d")
end   <- as.Date("2007-12-31",format="%Y-%m-%d")

theDate <- start+1
row_no = 1

while (theDate <= end)
{
  start_row = (row_no-1)*3+1
  end_row = row_no*3
  dates_p[start_row:end_row] <- theDate
  theDate <- theDate + 3
  row_no = row_no + 1
}


folsomPQ3Day <- aggregate(folsomPQ, dates_p, sum, regular = T)
# temperature has to be average for the model parameters
folsomPQ3Day[, "E"] = folsomPQ3Day[, "E"]/3  

folsomPQ3Day50_80 <- window(folsomPQ3Day, start = "1950-01-01", end = "1979-12-31")

folsomPQ3Day80_08 <- window(folsomPQ3Day, start = "1980-01-01", end = "2007-12-31")

##--another way to compute three day averages or sums---
folsomroll = rollsum(x=folsomPQ, k=3, align = "left")
folsomroll1 = folsomroll[seq(from=1, to = dim(folsomroll)[1], by = 3)]
folsomroll1[, "E"] = folsomroll1[, "E"]/3  
folsomPQ3dayroll50_80 <- window(folsomroll1, start = "1950-01-01", end = "1979-12-31")
folsomPQ3dayroll80_08 <- window(folsomroll1, start = "1980-01-01", end = "2007-12-31")




xyplot(folsomPQ3Day, xlim = extendrange(as.Date(c("1996-10-01", "1997-03-01"))))
xyplot(folsomroll1, xlim = extendrange(as.Date(c("2002-01-01", "2005-01-01"))))
runoffratio <- with(folsomPQ, sum(Q)/sum(P))
x <- rollccf(folsomPQ3Day) # rolling cross correlation
xyplot(x, xlim = extendrange(as.Date(c("1950-01-01", "2008-01-01"))))


hydromod1 <- hydromad(folsomPQ3Day, sma = "cwi", routing = "expuh",
                      tau_s = c(5, 100), tau_q = c(0, 5), v_s = c(0,1))


#Set Up the Hydrologic Model
cmd_e <- 0.166 # used by Andrew's et al. HYDROMAD paper 2011
cmd_d <- 200 # used by  Andrew's et al. HYDROMAD paper 2011
# rest two parameters, shape and f will be calibrated
cmd_shape <- c(0,100) # optimize shape
cmd_f <- c(0.01, 1) # optimize it

#snow model is attached with cmd, so when sma="snow", it means "cmd and snow models"
mod0 <- hydromad(folsomPQ50_80, sma = "snow", routing = "powuh",
                 e=cmd_e, d=cmd_d,  shape=cmd_shape, f=cmd_f,
                 Tmax = 15, Tmin = 5) # snow model coupled with cmd soil moisture model

mod0