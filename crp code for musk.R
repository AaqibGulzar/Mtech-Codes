

aqib_func<- function(stationA, stationB)
{
  l1<- length(stationA)
  l2<- length(stationB)
  inters<- intersect(stationA, stationB)
  crp <- length(inters)
  x_ob_A=length(setdiff(stationA,inters))
  x_ob_B=length(setdiff(stationB,inters))
  output <- list(crp=crp,x_ob_A=x_ob_A,x_ob_B=x_ob_B)
  return(output)
}

out <- aqib_func(stationA = station1, stationB = station2)

View(out)
