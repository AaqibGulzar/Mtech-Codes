best=function(state,outcome){
data=read.csv("c:/Users/Hp/Desktop/R Programming/coursera/outcome-of-care-measures.csv",colClasses = "character")
fd=as.data.frame(cbind(data[,2],data[ ,7],data[,11],data[ ,17],data[ ,23],stringAsFctors=F))
colnames(fd)=c("hospital","state","heart attack","heart failure","pneumonia")
if(!state %in% fd[ ,"state"]){
  stop("invalid state")
} else if (!outcome %in% c(" heart attack","heart failure","pneumonia")){
  stop("invalid outcome")
} else {
  si=which(fd[ ,"state"]==state)
  ts=fd[si,]
  oi=as.numeric(ts[ ,eval(outcome)])
  min_val=min(oi,na.rm = T)
  result=ts[ ,"hospital"][which(oi==min_val)]
  output=result[order(result)]
} 
return(output)
}
