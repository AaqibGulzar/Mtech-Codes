a=-17.75/3600 + 50.06/60^3
b=3.92/60^0.3 - 31.1/60^0.6 + 34.86/60^0.9
c=-7.31/60^0.59 + 45.9/60^1.18 - 86.5/60^1.77

v=(6/60 + a)*(1+ (9/6 + b)*(-0.2)^2  + (15/48 + c)*(-0.2^4))

#A and B constants
Rmusk$A=matrix(data=NA,nrow = 44,ncol=1)
A1=-0.33 + 0.08*abs(Rmusk$Tsk)
A2=-0.52+ 0.3*abs(Rmusk$Tsk)
  for (i in 1:nrow(Rmusk)) {
    if (abs(Rmusk[i,"Tsk"]) <= 0.9) {
      Rmusk$A[i]=A1[i]
    } else {
      Rmusk$A[i]=A2[i]
    }
  }

Rmusk$B=matrix(data=NA,nrow = 44,ncol=1)
B1=0.94 - 0.26*abs(Rmusk$Tsk)
B2=0.55
for (i in 1:nrow(Rmusk)) {
  if (abs(Rmusk[i, "Tsk"]) <= 1.5) {
    Rmusk$B[i] = B1[i]
  } else {
    Rmusk$B[i] = B2
  }
}
Rmusk$MSEg=(10)^(Rmusk$A - Rmusk$B*log10(Rmusk$N/10))
Rmusk$wsk=(0.302*Rmusk$Tsk + Rmusk$MSEg*0.6844)/(0.302 + Rmusk$MSEg)

