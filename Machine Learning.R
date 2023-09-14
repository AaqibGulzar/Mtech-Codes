ols1 = lm(regression_data.2$Aaqib_skew~regression_data.2$`Mean precipitation`)
ols2 = lm(regression_data.2$Aaqib_skew~regression_data.2$`hydrologic soil index`)
ols3 = lm(regression_data.2$Aaqib_skew~regression_data.2$`soil drainage index`)





library(h2o)
library(datasets)
h2o.init(nthreads=-1)
iris.hex = as.h2o(iris)
h2o.ls()
h2o.describe(iris.hex)
h2o.hist(iris.hex$Sepal.Length)
r_df = as.data.frame(iris.hex)
splits = h2o.splitFrame(data=iris.hex,
                        ratios=c(0.8),
                        seed=198)
train = splits[[1]]
test = splits[[2]]

rf = h2o.randomForest(x = c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"),
                      y = c("Species"),
                      training_frame=train,
                      model_id="our_rf",
                      seed=1234
                      
                      )

rf_perf1 = h2o.performance(model=rf,newdata=test)
predictions = h2o.predict(rf,test)
