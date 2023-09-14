library(Deriv)
f=expression(x^2+3*x)
D(f,"x")
D(D(f,"x"),"x")
#Tabulate function f (f=x3-2x2+3) values and its derivative values
#in the range -3,3 with the increment value of 0.1 using R statements.
f2=expression(x^3-2*x^2 + 3)
df = D(f2,"x")
for(x in seq(-3,3,0.1)) {
  print(paste(round(eval(f2),digits=2),round(eval(df),digits=2)))
}

exp<- expression(sin(cos(x + y^2)))

#The following command’s give derivative with respect to x and displays the same.

dx = D(exp,"x"); dx

#The following command’s compute derivative with respect to y and displays the same.

dy = D(exp,"y"); dy


# We can also calculate all the (partial) derivatives of an equation
# using deriv() function. It returns the result as an expression.

dxy<-deriv(exp,c("x","y"));dxy;typeof(dxy)

# In the following, we are defining some set of values for x and y
# and then computing the partial derivatives with respect to x and y using 
# their representation in expression form in dxy.

x<-seq(-pi,pi,pi/4)

y<-pi

eval(dxy)

# We can also use an option func=T with derive function such that it returns 
# all the derivatives of the expression as a function which we can invoke at
# any time. 

pp<-deriv(exp,c("x","y"),func=T)
pp(x,y)

#function to calculate the derivates of higher order.
DD<-function(expr,name,order=1){
  if(order<1) stop("Order must be >=1")
  if(order==1)D(expr,name)
  else DD(D(expr,name),name,order-1)
}
I = diag(nrow=92,ncol=92)
exr = solve(x*I +var_mat)

D(exr,"x")
X = model.matrix(ols_reis)
MLE = matrix(c(function(x,bet){
  0.5*log(det(x*I + var_mat),base=exp(1)) + 
    0.5*t((Reis_Et_Al_2020_Skew$skew - X%*%bet)) %*% solve((x*I +var_mat)) %*%(Reis_Et_Al_2020_Skew$skew - X%*%bet)
}
)
)


y = matrix(c(function(a,b){a^3+b},function(a,b){a^2-b},function(a,b){a*b},function(a){1}),ncol=2)
matrix.derv(y)


















