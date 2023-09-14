library(dplyr)
library(readr)
library(lubridate)
install.packages("macleish")

library(macleish) # package contains weather data
library(ggplot2)
library(ggcorrplot)
library(gridExtra)
library(ggpubr)
install.packages("patchwork")                 # Install & load patchwork package
library("patchwork")
library(scales)
p1<-ggplot(data = mod1, aes(x = datetime, y = tair)) +
  geom_line(color='red')+xlab("") +ylab(expression((""^0*C)~Air~ Temperature))
p2<-ggplot(data = mod1, aes(x = datetime, y = wspd)) +
  geom_line(color='green')+xlab("")+ylab(expression((ms^-1)~Wind~Speed))
p3<-ggplot(data = mod1, aes(x = datetime, y = RH)) +
  geom_line(color='blue')+xlab("")+ylab("(%) Relative Humidity")
p4<-ggplot(data = mod1, aes(x = datetime, y = press)) +
  geom_line(color='brown')+xlab("")+ylab("(kPa) Pressure")
p5<-ggplot(data = mod1, aes(x = datetime, y = swdown)) +
  geom_line(color='steel blue')+xlab("")+ylab(expression((Wm^-2)~Shortwave~Radiations))
p6<-ggplot(data = mod1, aes(x =datetime, y = lwdown)) +
  geom_line(color='orange')+xlab("")+ylab(expression((Wm^-2)~Longwave~Radiations))
p7<-ggplot(data = mod1, aes(x = datetime, y = precip)) +
  geom_line(color='#00AFBB')+xlab("")+ylab("(mm) Precipitation")
par(mfrow=c(4,2))
png("plots.png",res = 300,height = 8,width = 10,units='in')
ggp_all <- (p1 + p2) / (p3 + p4)/(p5+p6) /(p7)+    # Create grid of plots with title
  plot_annotation(title = "Timeseries Plot for all Metereological Data") &
  theme(plot.title = element_text(hjust = 0.5))
ggp_all
dev.off()

sw_up<-function(sw_down,albedo)
{
  albedo*sw_down
}
Ts<-function(TK,H,ra){
    TK+(H*ra)/(1.23*1000)
  }
  
lw_up<-function(e,Ts){
  e*5.67*10^(-8)*Ts^4
  }

Rn<-function(sw_down,sw_up,lw_down,lw_up)
{ 
  (sw_down - sw_up)+(lw_down - lw_up)
}

H<-function(Rn,ET,G){
  (Rn-G)-ET
}
G<-function(Rn){
  0.5*Rn
}
  
ET<-function(SVPG,Rn,VPD,ra,rs,G){
  (SVPG*(Rn-G)+(1.23*VPD)/ra)/(SVPG+(0.067*(1+(rs/ra))))
}
EVT<-function(ET){
  (1.59*10^-3)*ET
}

SVP= function(Temp){
  0.611*exp((17.5027*Temp)/(240.97+Temp))
}
SVPG= function(SVP,Temp){
  (4098*SVP/(237.3+Temp)^2)/1000
}
VP=function(RH,SVP){
  (RH*SVP)/100
}
VPD=function(VP,SVP){
  SVP-VP
}
ra_grass=function(Uz){
  368.22/Uz
}


gR_g<-function(S){
  (1200*S)/(1000*(S+200))}
gD_g<-function(VPD){
  1+0.307*VPD+0.019*VPD^2}
gT_g<-function(TK){
  ((TK-273)*(313-TK)/400)}        #Temp in K
gM_g<-function(SM){
  1-20*exp((-0.3)*(SM))}
gs_g<-function(gR_g,gD_g,gT_g,gM_g){
  0.030*gR_g*gD_g*gT_g*gM_g}

DATA<-read.table("C:/Users/Madiha Farooq/Desktop/input.dat",header=TRUE)
DATA $ month = c(6) 
DATA <-DATA %>%
  mutate(datetime=make_datetime(YYYY,month,DoY,HH))
DATA$TK=(273+DATA$tair) 
sw_down=DATA[1,8]
albedo=0.25  
sw_up_1= sw_up(sw_down,albedo)
Ts_1=DATA[1,13]
lw_down=DATA[1,9]
e=0.97
lw_up_1=lw_up(e,Ts_1)
Rn_1=Rn(sw_down,sw_up_1,lw_down,lw_up_1) 
G_1=G(Rn=Rn_1)
Temp=DATA[1,4]
SVP_1=SVP(Temp)
SVPG_1=SVPG(SVP_1,Temp)
RH=DATA[1,6]
VP_1=VP(RH,SVP_1)
VPD_1=VPD(VP_1,SVP_1)
Uz=DATA[1,5]
ra_1=ra_grass(Uz)
S=sum(DATA[1,8],DATA[1,9])
SM_1=15
gR_1=gR_g(S)
gD_1=gD_g(VPD_1)
gT_1=gT_g(TK=Ts_1)
gM_1=gM_g(SM_1)
gs_1=gs_g(gR_1,gD_1,gT_1,gM_1)
rs_1=1/gs_1
ET_1=ET(SVPG_1,Rn_1,VPD_1,ra_1,rs_1,G_1)
EVT_1=EVT(ET_1)
H_1=H(Rn_1,ET_1,G_1)
sw_up_all<-matrix(data=NA,nrow=192,ncol=1)
for(i in 1:192){
  
  sw_up_all[i,]<-sw_up(sw_down=DATA[i,8],albedo)
}

mod1<-data.frame(DATA,sw_up=sw_up_all,lw_up=lw_up_1,Rn=Rn_1,ET=ET_1,ra=ra_1,rs=rs_1,H=H_1,Ts=Ts_1,SVP=SVP_1,SVPG=SVPG_1,VP=VP_1,VPD=VPD_1,gR=gR_1,gD=gD_1,gM=gM_1,gT=gT_1,gs=gs_1,SM=SM_1,EVT=EVT_1,G=G_1)

View(mod1)

s_max=30
for (i in 2:192){
 
   mod1$SM[i]=mod1$SM[i] + mod1$precip[i]
  if (mod1$SM[i]>= s_max)
    {
    runoff=mod1$SM[i]-s_max
    mod1$SM[i]=s_max
    }
  mod1$lw_up[i]=lw_up(e=0.97,Ts= mod1$Ts[i-1])
  mod1$Rn[i]<-Rn(sw_down =mod1$swdown[i],sw_up=sw_up_all[i,1],lw_down=mod1$lwdown[i] ,lw_up=mod1$lw_up[i]) 
 
  for (j in 2:192){
    if (mod1$HH[j] == 0 ||
        mod1$HH[j] == 1 ||
        mod1$HH[j] == 2 ||
        mod1$HH[j]== 3 ||
       mod1$HH[j]== 4 ||
        mod1$HH[j]== 5 ||
        mod1$HH[j]== 6 ||
        mod1$HH[j]== 19 || mod1$HH[j] == 20 ||
        mod1$HH[j]== 21 || mod1$HH[j] == 22 || mod1$HH [j]== 23) {
      mod1$G[j] = 0.5 * mod1$Rn[j]
    }
         else if(mod1$HH[j] == 7 ||
   mod1$HH[j] == 8 ||
   mod1$HH[j] == 9 ||
   mod1$HH[j]== 10 ||
   mod1$HH[j]== 11 ||
   mod1$HH[j]== 12 ||
  mod1$HH[j]== 13 ||
 mod1$HH[j]== 14 || mod1$HH[j] == 15 ||
  mod1$HH[j]== 16 || mod1$HH[j] == 17 || mod1$HH[j] == 18) 
            {
           mod1$G[j] = 0.1 * mod1$Rn[j]
    }
  }
    
  mod1$SVP[i]=SVP(Temp=mod1$tair[i])
  mod1$SVPG[i]=SVPG(SVP=mod1$SVP[i],Temp=mod1$tair[i])
  mod1$VP[i]=VP(RH=mod1$RH[i],SVP=mod1$SVP[i])
  mod1$VPD[i]=VPD(VP=mod1$VP[i],SVP=mod1$SVP[i])
  mod1$ra[i]=ra_grass(Uz=mod1$wspd[i])
  mod1$gR[i]=gR_g(S=sum(mod1$swdown[i],mod1$lwdown[i]))
  for (k in 2:192){
    if (mod1$gR[k]>1){
      mod1$gR[k] =1
    }
  }
  mod1$gD[i]=gD_g(VPD=mod1$VPD[i])
  for (l in 2:192){
    if (mod1$gD[l]>1){
     mod1$gD[l] =1
    }
  }
  mod1$gM[i]=gM_g(SM=mod1$SM[i])
  mod1$gT[i]=gT_g(TK=mod1$TK[i])
  mod1$gs[i]=gs_g(gR_g=mod1$gR[i],gD_g=mod1$gD[i],gM_g=mod1$gM[i],gT_g=mod1$gT[i])
  mod1$rs[i]=1/mod1$gs[i]
  mod1$ET[i]=ET(SVPG=mod1$SVPG[i],Rn=mod1$Rn[i],VPD=mod1$VPD[i],ra=mod1$ra[i],rs=mod1$rs[i],G=mod1$G[i])
  mod1$EVT[i]=mod1$ET[i]*(1.59*10^-3)
  mod1$SM[i+1]=mod1$SM[i]+mod1$precip[i]-mod1$EVT[i]
  mod1$H[i]=H(Rn=mod1$Rn[i],ET=mod1$ET[i],G=mod1$G[i])
  mod1$Ts[i]=Ts(TK=mod1$TK[i],H=mod1$H[i],ra=mod1$ra[i])
  }
View(mod1)
p8<-ggplot(data = mod1, aes(x = datetime, y = Rn)) +
  geom_line(color='red')+xlab("") +ylab(expression((Wm^-2)~Net~Radiations))
p9<-ggplot(data = mod1, aes(x = datetime, y = swdown)) +
  geom_line(color='red')+xlab("") +ylab(expression((Wm^-2)~Downward~SW~Radiations))
p10<-ggplot(data = mod1, aes(x = datetime, y = sw_up)) +
  geom_line(color='red')+xlab("") +ylab(expression((Wm^-2)~Upward~SW~Radiations))
p11<-ggplot(data = mod1, aes(x = datetime, y = lwdown)) +
  geom_line(color='red')+xlab("") +ylab(expression((Wm^-2)~Downward~LW~Radiations))
p12<-ggplot(data = mod1, aes(x = datetime, y = lw_up)) +
  geom_line(color='red')+xlab("") +ylab(expression((Wm^-2)~Upward~LW~Radiations))
par(mfrow=c(3,2))
png("plots1.png",res = 300,height = 8,width = 10,units='in')
ggp_all <- (p8 + p9) / (p10 + p11)/(p12) +    # Create grid of plots with title
  plot_annotation(title = "Radiation Balance components for Green Grass ") &
  theme(plot.title = element_text(hjust = 0.5))
ggp_all
dev.off()

plot(mod1$datetime,mod1$ET,type = "l")
plot(mod1$datetime,mod1$H,type = "l")
plot(mod1$datetime,mod1$precip,type = "l")
plot(mod1$datetime,mod1$SM,type = "l")

plot(mod1$datetime,mod1$rs,type = "l")
plot(mod1$datetime,mod1$gR,type = "l")
plot(mod1$datetime,mod1$gD,type = "l")
plot(mod1$datetime,mod1$gM,type = "l")
plot(mod1$datetime,mod1$gT,type = "l")
plot(mod1$datetime,mod1$gs,type = "l")
