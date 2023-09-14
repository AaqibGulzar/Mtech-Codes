#library tidyverse ,renaming multiple columns at once
#using rename_with function
mpg %>%
  rename_with(.data = .,
              .fn = toupper,
              .cols = everything())

#library(janitor)

iris %>%
  rename_with( ~ janitor::make_clean_names(., case = "big_camel")) %>%
  colnames()


#replacing in all columns

mpg %>%                                    
  rename_with( ~ gsub("a", "e", .)) %>%
  colnames()

mpg %>%
  rename_with( ~  str_replace(.,"a", "e")) %>%
  colnames()

#inserting underscore between colnames,a bit tricky

anscombe %>%
  rename_with( ~ str_replace(., pattern = "(\\d+)",
                             replacement = "_\\1")) %>%
  colnames()

anscombe %>%
  rename_with( ~ str_replace(., "(y)([1-2])",
                             "\\1epsilon\\2_")) %>%
  colnames()

#change certain columns specifically,here we change numeric columns

mpg %>%
  rename_with( ~ toupper(.), where(is.numeric)) %>%
  colnames()

#changing columns with certain words,like here starting
#sepal,the dots in them are changed into underscores

iris %>%
  rename_with( ~ str_replace(., "\\.", "_"),
               starts_with("Sepal")) %>%
  colnames()

#another important function is matches

iris %>%
  rename_with( ~ str_replace(., "\\.", "_"),
               matches("[Ww]idth$")) %>%
  colnames()
  
  
  
  
  
  
  
  
  





























































