 #ggplot,leaflet,simplevis,
#gganimate,ggvis,rgl,rayshader,
#rayrender,highcharter,p-lotly,lattice,

#Tutorial 1 on Tidyverse booster by Gumroad
library(janitor)
mpg_new = read_csv("c:/Users/Hp/Desktop/R Programming/mpg.csv", show_col_types = F) %>% 
  clean_names() %>%
  select(c(manufacturer, model)) %>% 
  glimpse()

read_csv("c:/Users/Hp/Desktop/R Programming/mpg.csv", 
  show_col_types = F,name_repair = make_clean_names) %>% 
  glimpse()

#we can alsoreplace and remove charcterstrings
make_clean_names(c("A","B%","C"),replace=c("%" = "_Percent"))

#snake naming convention separates with an underscore,
#case="none",will do nothing,also check all the other cases from pdf.
make_clean_names(c("myHouse,mygarden"),case = "snake")

# for mpg dataset,thedot denotes the vector of column names
read_csv("c:/Users/Hp/Desktop/R Programming/mpg.csv", 
  show_col_types = F,
  name_repair = ~make_clean_names(.,case = "all_caps")) %>% 
  glimpse()


#Apart from renaming columns directly inside read functionss,we can select too.
read_csv("c:/Users/Hp/Desktop/R Programming/mpg.csv", 
         show_col_types = F,
         name_repair = make_clean_names,col_select = c(manufacturer,model)) %>% 
  glimpse()

#TUTORIAL 2 on Tidyverse booster by Gumroad


#creating 25 csv files from first 20 rows of mpg dataset
library(fs)
dir_create("many_files")   #create a temporary directory
mpg_samples <- map(1:25,~ slice_sample(mpg,n=20))
iwalk(mpg_samples, ~ write.csv(. , paste0("many_files/",.y,".csv")))  #these files are stored in 
                                                                    #Rprogramming>new project

#creating a character vector for the file paths
(csv_files_list_files <- list.files(path = "many_files",pattern = "csv",full.names = T)) #fuulnames is important
(csv_files_dir_ls<-dir_ls(path = "many_files/",glob="*.csv",type = "file"))

#Reading multiple files
data_frames <- map_dfr(csv_files_dir_ls,~ read_csv(.x,show_col_types = F))
glimpse(data_frames)

read_csv(csv_files_dir_ls,id="filename",show_col_types = F) %>% glimpse()#another appraoch

#when the column names are inconsistent
#create a messy data,with random colnames 

mpg_samples <- map(1:10, ~ slice_sample(mpg,n=20))
inconsistent_dframes <- map(mpg_samples,
                            ~ janitor::clean_names(dat=.x, case="random"))
map(inconsistent_dframes,~colnames(.x)) %>% head

inconsistent_dframes<-map(inconsistent_dframes, ~.x[sample(1:length(.x),sample(1:length(.x),1))])
map(inconsistent_dframes,~colnames(.x)) %>% head()


dir_create("unclean_files")   #save to the disk
iwalk(inconsistent_dframes, ~ write.csv(.x,paste0("unclean_files/", .y,".csv")))
many_columns_dataframe<- dir_ls(path = "unclean_files/",glob = "*.csv",type="file") %>%
  map_dfr( ~read_csv(.x,show_col_types = F) %>%
             mutate(filename=.x))
colnames(many_columns_dataframe) %>% sort
           
#time to clean up the column names

many_columns_dataframe = dir_ls(path = "c:/Users/Hp/Desktop/R Programming/new project/unclean_files",
                                glob = "*.csv",type="file") %>%
  map_dfr( ~ read_csv(.x,name_repair = tolower,show_col_types = F) %>%
             mutate(filename=.x))
glimpse(many_columns_dataframe) 
#this code isnt working !!!!!!

#the next parts deal with the files being in different folders and when some arent needed.
           















