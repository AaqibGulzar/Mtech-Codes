colnames(Record_Periods)="RP"
Record_Periods=Record_Periods %>% separate(RP,c("FROM","TO"))
Record_Periods$FROM=as.numeric(Record_Periods$FROM)
Record_Periods$TO=as.numeric(Record_Periods$TO)
start=Record_Periods$FROM
end=Record_Periods$TO

station1 <- start[1]:end[1]
station2 <- start[2]:end[2]
station3 <- start[3]:end[3]
station4 <- start[4]:end[4]
station5 <- start[5]:end[5]
station6 <- start[6]:end[6]
station7 <- start[7]:end[7]
station8 <- start[8]:end[8]
station9 <- start[9]:end[9]
station10 <-start[10]:end[10]
station11 <- start[11]:end[11]
station12 <- start[12]:end[12]
station13 <- start[13]:end[13]
station14 <- start[14]:end[14]
station15 <- start[15]:end[15]
station16 <- start[16]:end[16]
station17 <- start[17]:end[17]
station18 <- start[18]:end[18]
station19 <- start[19]:end[19]
station20 <- start[20]:end[20]
station21 <- start[21]:end[21]
station22 <- start[22]:end[22]
station23 <- start[23]:end[23]
station24 <- start[24]:end[24]
station25 <- start[25]:end[25]
station26 <- start[26]:end[26]
station27 <- start[27]:end[27]
station28 <- start[28]:end[28]
station29 <- start[29]:end[29]
station30 <- start[30]:end[30]
station31 <- start[31]:end[31]
station32 <- start[32]:end[32]
station33 <- start[33]:end[33]
station34 <- start[34]:end[34]
station35 <- start[35]:end[35]
station36 <- start[36]:end[36]
station36 <- start[37]:end[37]
station38 <- start[38]:end[38]
station39 <- start[39]:end[39]
station40 <- start[40]:end[40]
station41 <- start[41]:end[41]
station42 <- start[42]:end[42]
station43 <- start[43]:end[43]
station44 <- start[44]:end[44]

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
