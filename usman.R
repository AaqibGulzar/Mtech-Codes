library(readxl)
library(ggplot2)




give_summary <- function(year) {
  # Construct file path
  file_name <- paste0(year, ".xlsx")
  file_path <- file.path(getwd(), file_name)
  
  # Read in file and extract columns
  data <- read_excel(file_path)
  ols = lm(Kc ~ `Eto (mm/day )`, data = data)
  
  #ggplot and fit the linear model
  gg =   ggplot(data, aes(x = `Eto (mm/day )`, y = Kc)) + 
    geom_point() + 
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    annotate("text", x = max(data$`Eto (mm/day )`), y = max(data$Kc), 
             label = paste0("y = ", round(coef(ols)[1],2), " + ", 
                            round(coef(ols)[2],2), "x\n", 
                            "R² = ", round(summary(ols)$r.squared, 3)), 
             hjust = 1, vjust = 4.5, size = 5, color = "black") + theme_minimal() + ggtitle(year)
  
  return(gg)
}


give_summary(2019)






















kvt <- read_excel("~/Day wise final.xlsx")

kvt = kvt %>% na.omit()

polynomial2 = lm(data=kvt, Kc ~ `t/T` + I((`t/T`)^2))


gg = ggplot(data=kvt)  +
  theme_classic() + geom_smooth(aes(x = `t/T`,y = Kc)) + 
  geom_point(aes(x=`t/T`,y = Kc),color = "dodgerblue4",size = 6,alpha = 0.5)
gg

coefs = polynomial2$coefficients
x = kvt$`t/T`
eq = coefs[1] + coefs[2]*x + coefs[3]*(x^2)
plot(eq,col = "red",xlab= "-4.33x^2 + 4.71x + 0.12",ylab = " ",type="l")



gg + geom_line(aes(x  = seq(0,1,1/36),y =eq)) 




#===============================================================================
# Load required package
library(readxl)
library(ggplot2)

# Set working directory, same as the root folder where all the files are located.
setwd("d:/Usman Data/")

# Function to plot two columns from a file
plot_columns <- function(year) {
  # Construct file path
  file_name <- paste0(year, ".xlsx")
  file_path <- file.path(getwd(), file_name)
  
  # Read in file and extract columns
  data <- read_excel(file_path)

  #ggplot and fit the linear model
  gg = ggplot(data = data) +
    geom_smooth(
      aes(x = `Eto (mm/day )`, y = Kc),
      color = "steelblue",
      linewidth = 2,
      method = "lm"
    ) +
    geom_point(aes(x = `Eto (mm/day )`, y = Kc),color = "darkred") + 
    theme_minimal() + ggtitle(year)
  
  return(gg)
}

# Example usage: plot columns from 2014 file
plot_columns(2014)

#===============================================================================



































