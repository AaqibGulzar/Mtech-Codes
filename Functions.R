pptdata<-read.csv("D:\\mehvish_phd\\daily_ppt\\pptdata.csv")# final daily ppt for all stations

# for removing xtra row at end and adding ppt to stream data in dataframe format 

df_fun = function(i) {
  for (i in i) {
    df = matrix(data = NA,
                nrow = 7792,
                ncol = 4)
    
    df =  data[[i]][-7793, ]
    
    df = tibble(df)
    df$Rainfall <- pptdata[,i + 1]
  }
  return(list(df))
}

# write df into list

final.list=list()
for(i in 1:1161){
  final.list[i] = list(df_fun(i))
}

#filter those stations that do not have flow component that means they were empty while dwnloading
exact.list <- Filter(function(x) ("Flow" %in% names(x[[1]])), final.list)

##to chk continue zero rain days(<2mm) for min 12 days length for all stations,finding start and end of events 
new.df = function(i) {
  
  x <- rle(exact.list[[i]][[1]][["Rainfall"]])
  mycode = which(x[["values"]] <= 2 & x[["lengths"]] >= 12)
  ends = (cumsum(rle(exact.list[[i]][[1]][["Rainfall"]])$"lengths")[mycode])
  
  newindex = ifelse(mycode > 1, mycode - 1, 0)
  starts = cumsum(rle(exact.list[[i]][[1]][["Rainfall"]])$"lengths")[newindex] + 1
  if (0 %in% newindex){
    starts = c(1, starts)
  }
  m = matrix(data = NA,
             nrow = length(starts),
             ncol = 2)
  m = data.frame(starts, ends)
  
  return(list(m))
}

## write this in list format
final.list2=list()
for(i in 1:908){
  final.list2[i] = new.df(i)
}


##extract all other variables from above indicies and remove first 5 days to each event of <2mm rain for min 12 days 

dryevents = function(A) {
  tryflow = list()
  if(nrow(final.list2[[A]]) != 0){
    for (i in 1:nrow(final.list2[[A]])) {
      tryflow[[i]] <-sapply(exact.list[[A]][[1]],"[",final.list2[[A]]$starts[i]:final.list2[[A]]$ends[i])
      tryflow[[i]] <- tryflow[[i]][-c(1:5), ]
    }
    return(tryflow)
  } else {print("This station has gone to vacation")}
}

##

find_decreasing_event <- function(Station, Event) {
  differences = dryevents(Station)[[Event]][, 4] %>% diff()
  indices <- which(differences <= 0)
  
  event <- dryevents(Station)[[Event]][indices, ]
  if (nrow(event) >= 7) {
    D_flowevents = list()
    D_flowevents <- event
    return(D_flowevents)} else {
      0
    }
}

k_for_events <- function(Station, Event) {
  if(length(find_decreasing_event(Station,Event)) > 1){
    logy <- log10(find_decreasing_event(Station,Event)[,4])
    logy = matrix(logy,nrow = length(logy),ncol = 1) %>% as.data.frame()
    mt = matrix(1:nrow(logy),ncol = 1)
    model <- lm(logy$V1 ~ mt)
    k <- -1/coef(model)[2]
    return(k)} else {"more crazy"}
}

for(i in 1:908) {
  if (stat_events$evt[i] > 0) {
    k_for_events(i, stat_events$evt[i])
  } else print("zero event station")
}

k_fun = function(i) {
  if (all(filtered3[[i]][, 4]) > 0) {
    logy <- log10(filtered3[[i]][, 4])
    
    logy = matrix(logy, nrow = length(logy), ncol = 1) %>% as.data.frame()
    
    mt = matrix(1:nrow(logy), ncol = 1)
    
    model <- lm(logy$V1 ~ mt)
    
    k <- -1 / coef(model)[2]
    return(k) } else{0}
}

k.matrix = matrix(data=NA,nrow=2045,ncol = 1)
for(i in 1:2045){
  k.matrix[i] = round(k_fun(i),digits=2) 
}



