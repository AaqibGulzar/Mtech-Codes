pollutantmean = function(directory, pollutant, id = 1:322) {
  mylist = list.files(path = directory, pattern = ".csv")
  x = numeric()
  for (i in id) {
    mydata = read.csv(mylist[i])
    if (pollutant == "sulphate") {
      x = c(x, mydata[, 2])
      
    }
    else{
      x = c(x, mydata[, 3])
    }
  }
  mean(x, na.rm = T)
  
}