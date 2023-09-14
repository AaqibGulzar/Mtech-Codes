library(tidyverse)                    # FOR CLEANING.# FOR PLOTTING.
library(dataRetrieval)                # FOR DATA RETRIEVAL.
                
site_numbs <- c("02412000","02413475","02415000","02188500","02191200","02191300","02191970","02192300"
                ,"02193500","02204500","02208450","02212600","02213050","02217500","02217900"
                ,"02219000","02219500","02220550","02220900","02221000","02221525","02335700"
                ,"02337000","02337400","02338660","02340500","02344500","02344700","02345000"
                ,"02346500","02347500","02394400","02411800","02413000","02413200","02077200",
                "02081500","02081747","02082770","02082950","02085000","0208521324","02085500"
                ,"02086000","02087500","02093800","02094000","02095000","02096500"
                ,"02098500","02099500","02100500","02101800","02102000","02114450","02120780"
                ,"02121500","02123500","02125000","02126000","02127000","02128000","02142900"
                ,"02143500","02144000","02146900","02147500","02157500","02159000"
                ,"02160000","02160500","02162010","02165000","02165200","02186000"
                ,"02192500","02196000","02044000","02044200","02044500","02046000","02051000"
                ,"02051500","02051600","02052500","02058400","02064000"
                ,"02065500","02066500","02075500","02076500","02079640")



data = readNWISpeak(site_numbs,endDate="2021-12-31")
coords = readNWISsite(site_numbs)

data %>% head()

Match = function(siteA, siteB){ 
  
  if(siteA != siteB){
    A = (data %>% filter(site_no == siteA))[3]$peak_dt
    A = as.Date(A) %>% na.omit()
    B = (data %>% filter(site_no == siteB))[3]$peak_dt
    B = as.Date(B) %>% na.omit()
    
    # Find common date range between the two stations
    start.date <- max(min(A), min(B))
    end.date <- min(max(A), max(B))
    
    if(start.date < end.date){
      common.dates <- seq(from = start.date, to = end.date, by = "year")
      
      # Create an empty list to store all the dates within a 7 day window for each peak date
      dates_within_windowA <- list()
      
      # Loop through each peak date in peak.datesA
      for (i in 1:length(A)) {
        # Create a vector of all the dates within a 7 day window of the peak date
        dates_in_window <- seq(A[i] - 3, A[i] + 3, by = "day")
        
        # Add the vector of dates to the list
        dates_within_windowA[[i]] <- dates_in_window
      }
      
      datesA = dates_within_windowA %>% unlist() %>% as.Date()
      
      p = length(datesA[datesA %in% B])/length(common.dates) 
    } else {
      p = 0
    }
  } else {
    p = 1
  }
  
  return(p*100)
}

Match(site_numbs[1],site_numbs[2])
Match(site_numbs[2],site_numbs[2])



Match(site_numbs[2],site_numbs[1])


library(foreach)
library(doParallel)
cl = makeCluster(10)
registerDoParallel(cl)
clusterCall(cl,function() library(dplyr))

ratio_mat =
  foreach(i = 1:92,combine = cbind) %:%
  foreach (j = 1:92)  %dopar% {
    Match(site_numbs[i],site_numbs[j]) 
  } %>% unlist() %>% matrix(nrow = 92, ncol = 92)

stopCluster(cl)


# Save p_matrix as a table
#write.table(ratio_mat, file = "ratio matrix.txt", sep = "\t", quote = FALSE)





# Define the breaks and colors for the heatmap
# breaks <- seq(0,100,10)
# colors <- c("grey", "dodgerblue4", "red", "gold")

# Reshape the matrix into a dataframe for plotting with ggplot2

df <- reshape2::melt(ratio_mat)
colnames(df) <- c("x", "y", "value")


# better colors
# Define breaks and colors

breaks <- seq(0, 100, 10)
colors <- viridisLite::turbo(10) # Red and vibrant Colors
my_colors <- RColorBrewer::brewer.pal(10, "RdYlBu") # Light Publication Colors

my_breaks <- seq(0, 100, 10)
my_labels <- c("90 to 100","80 to 90", "70 to 80", "60 to 70", "50 to 60",
               "40 to 50","30 to 40", " 20 to 30","10 to 20","0 to 10")
my_colors <- RColorBrewer::brewer.pal(10, "RdYlBu")


color1 <- viridisLite::turbo(10)
color2 <- MetBrewer::met.brewer("Paquin",type="discrete",n=10)
color3 <- RColorBrewer::brewer.pal(10,"RdYlGn")
color4 <- RColorBrewer::brewer.pal(10,"Spectral")
mj_colors <- c("yellow","red","skyblue","coral","black","limegreen",
               "dodgerblue","darkred","navyblue","turquoise")
orrd = c("#FFF7EC","#FEE8C8","#FDD49E","#FDBB84","#FC8D59","#EF6548",
         "#D7301F","#B30000","#7F0000","black")
purples = c("#F7F4F9","#E7E1EF","#D4B9DA","#C994C7",
            "#DF65B0","#E7298A","#CE1256","#980043","#67001F","red")

sfd = ggsci::pal_simpsons(palette="springfield")(10)
heat = heat.colors(10)
manual = c("#F7F4F9","lightblue","#C7EF34FF","#72FE5EFF","limegreen",
           "#FABA39FF","#F66B19FF","#CB2A04FF","#7A0403FF","black")
manual <- c("#990000", "#FF0000", "#FF6600", "#FFCC00", "#FFFF00","lightgreen",
            "#00FF00", "dodgerblue1","dodgerblue3","dodgerblue4" ,"#000099")


oh = c("black","#115f9a", "#1984c5", "#22a7f0", "#48b5c4", "#76c68f", 
             "#a6d75b", "#C7EF34FF","#72FE5EFF","limegreen")
# Create heatmap
ggplot(data = df, aes(x = x, 
                           y = y,
                           fill = cut(value, breaks = breaks, labels = rev(my_labels),
                                      include.lowest = TRUE))) +
  geom_tile() +
  scale_fill_manual(values = rev(manual), name = "Percentage") +
  ggtitle("Ratio of Matched peak dates in a 7-day window\nand the Common record period between two stations")+
  theme_void() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + coord_equal()

#ggsave("Heatmap2.jpeg",dpi=1000)

st_coord = function(site){
  lat = filter(coords,site_no == site )$dec_lat_va
  long = filter(coords,site_no == site )$dec_long_va
  return(c(lat ,long))
}


df_coord = matrix(data=NA,nrow=92,ncol=2)

for (i in 1:92) {
  df_coord[i,] = st_coord(site_numbs[i]) 
}
df_coord = data.frame(df_coord)
colnames(df_coord) = c("lat","long")

print(df_coord)


dist_mat = matrix(data=NA,nrow=92,ncol=92)
for(i in 1:92){
  for (j in 1:92){
    dist_mat[i,j]= geosphere::distm(
      c(df_coord$long[i], df_coord$lat[i]),
      c(df_coord$long[j], df_coord$lat[j])
    )
  }
}

dist_mat = 0.000621*dist_mat  #the distances will be in metres,so we convert those into miles.
dist_vec <- reshape2::melt(dist_mat)
colnames(dist_vec) <- c("x", "y", "dist")
dist_cor = cbind(df,dist_vec)
dist_cor = dist_cor[,c(6,3)]

# Sort the dataframe based on distance
dist_cor <- dist_cor[order(dist_cor$dist), ] %>% as_tibble()

dist_cor
dist_cor %>% summary()
dist_cor %>% na.omit() %>% head()


#----------------------Station Wise ----------------------------------------------


fun = function(n) {
  d = numeric()
  for (i in 1:92) {
    d[i] =  geosphere::distm(c(df_coord$long[n], df_coord$lat[n]),
                             c(df_coord$long[i], df_coord$lat[i]))
  }
  for (j in 1:92) {
    df = data.frame(site_numbs, round(d * 0.000621, digits = 2))
  }
  colnames(df) = c("Site", "Dis(miles)")
  df2 = df %>% arrange(`Dis(miles)`)
  
  for (i in 1:92) {
    df2$Cor[i] = Match(site_numbs[n], df2$Site[i])
  }
  
  label = paste0("S", n)  # Dynamically create label based on the input number
  
  
  return(df2 = df2)
}

fun(1)
fun(2)
fun(3)
fun(77)

#-------------------------------------------------------------------------------
# Create an empty dataframe to store the combined data
final_df <- data.frame()

# Loop through each individual dataframe and append it to the final dataframe
for (i in 1:92) {
  individual_df <- fun(i)  # Call your function to get the individual dataframe
  final_df <- rbind(final_df, individual_df)  # Append the individual dataframe to the final dataframe
}

# Print the final dataframe
print(final_df)

# Create the factor variable based on the ratio values
final_df$bin <- cut(final_df$Cor, breaks = my_breaks, labels = rev(my_labels))

ggplot(na.omit(final_df)) +
  geom_point(aes(x = `Dis(miles)`, y = Site, color = bin), size = 2.5,shape = 15) +
  scale_color_manual(values = manual, name = "Percentage") +
  #guide = guide_colorbar(barwidth = 1, barheight = 13)) 
  labs(y = "") + theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position=c(0.9,0.5),
        legend.key.height=unit(1,"cm"),
        legend.key.width=unit(0.5,"cm"),
        legend.spacing=unit(0.1,"cm"),
        axis.text.y = element_blank()) + theme(panel.border = element_rect(color = "black",
                                                                           fill = NA,
                                                                           linewidth = 1)) +
  scale_x_continuous(breaks=seq(0,600,100))
#ggsave("New Cor.png",dpi=1100,width=35,height=20,units="cm")

library(RColorBrewer)
final = c("#67000D","#A50F15", "#3E4A89FF", "#31688EFF", "#1F9E89FF",
"#35B779FF", "#6DCD59FF", "#B4DE2CFF", "#FDE725FF","#ADD8E6")

ggplot(final_df,aes(x = `Dis(miles)`,
                    y = Site,
                    color = cut(Cor, breaks = breaks, labels = rev(my_labels),
                                include.lowest = TRUE))) +
  geom_point(size = 2.5,shape = 15) + 
  scale_color_manual(values = rev(manual), name = "Percentage") +
  #guide = guide_colorbar(barwidth = 1, barheight = 13)) 
  labs(y = "",x = "Distance(miles)") + theme_bw() +
  theme(text=element_text(size=15),
    panel.grid = element_blank(),
        legend.position=c(0.9,0.5),
        legend.key.size = unit(0.1, "cm"),
        axis.text.y = element_blank()) + theme(panel.border = element_rect(color = "black",
                                                                           fill = NA,
                                                                           linewidth = 1)) +
  scale_x_continuous(breaks=seq(0,600,100))

#ggsave("New Cor.png",dpi=600,width=35,height=20,units="cm")


# CHECK THE ui.R for further shiny development

