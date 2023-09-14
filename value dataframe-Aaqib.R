library(data.table)
library(tidyverse)
library(readxl)
file.path= "c:/Users/ASUS/OneDrive/Desktop/temp_folsom"

file.list <- list.files(path = file.path, pattern = '*.xlsx')
setwd("c:/Users/ASUS/OneDrive/Desktop/temp_folsom")


# Create all the files from the above list
df.list <- lapply(file.list,read_xlsx) #read all the files
df.list <- lapply(df.list,data.frame)  #convert all into dataframes
df.list[[1]]$VALUE #subset the value column
s1=df.list[[1]]
m=matrix(data = NA,nrow =  3654,ncol = 17) #create an empty matrix

for(i in 1:17) {
  m[ ,i]=df.list[[i]]$VALUE          #overwrite the empty matrix
}

value_df=data.frame(m) # convert back into dataframe
value_df$date=s1$OBS.DATE  #add the date column
value_df <- value_df[ ,c(18,1:17)] # rearrange the columns









