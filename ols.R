A1 = -0.33 + 0.08 * MuskingumVariables$ABS
A2 = -0.52 + 0.3 * MuskingumVariables$ABS
for (i in MuskingumVariables[ ,"ABS"])
{
  if (i <= 0.9)
    {print(A1)}
   else if (i > 0.9)
    {print(A2)
   }
    }

x=MuskingumVariables2[ ,c(4,5,14)]
x$lnl=log10(x$`length(miles)`)
ols=lm(x$`weighted skew coeff`~x$lnl+x$`Slope(ft/miles)`)
e=matrix(data=residuals(ols),nrow=44)
edash=t(e)
