library(ggplot2)
library(ggpubr)
library(readxl)
library(readxl)
P_E=read_excel("C:/Users/ASUS/OneDrive/Documents/meh maam/Copy of GR4J_INPUTS(63).xlsx")
#View(P_E)
P=P_E$P
##-----PLOT high and Low flow hydrograph uncertainties----
##-----Input and Output Uncertainties----------
#input error CI
mean_Qvalid1 = read.csv("C:/Users/ASUS/OneDrive/Documents/meh maam/mean_Qvalid1gr4jinp6.csv",
                        header = T)
qq_valid = read.csv("C:/Users/ASUS/OneDrive/Documents/meh maam/qq_validgr4jinp7.csv",
                    header = T)


#discharge error CI
mean_Qvalid1O= read.csv("C:/Users/ASUS/OneDrive/Documents/meh maam/mean_Qvalid1O.csv",
                        header = T)

qq_validO = read.csv("C:/Users/ASUS/OneDrive/Documents/meh maam/qq_validO.csv",
                     header = T)

Qtrue=read.csv("C:/Users/ASUS/OneDrive/Documents/meh maam/Qtruegr4jinp3.csv",
               header = T)


colors1 <- c("Qtrue" = "dodgerblue3", "Qpredicted" = "lightgreen")
colors2 <- c("CI for input Uncertainity" = "black")
#first plot
gg1 = 
  ggplot() +
  geom_point(aes(
    x = as.Date(P_E$DatesR[2612:2629]),
    y = Qtrue[, 2][2612:2629],
    color = "Qtrue"
  )) +
  geom_line(aes(
    x = as.Date(P_E$DatesR[2612:2629]),
    y = mean_Qvalid1[55:72,],
    colour = "Qpredicted"
  ), size = 0.9) +
  labs(x = "dates", y = "Discharge",color = "legend") +
  geom_ribbon(aes(
    x = as.Date(P_E$DatesR[2612:2629]),
    ymin = qq_valid[, 1][55:72],
    ymax = qq_valid[, 2][55:72],
    fill = "CI for input Uncertainity"
  ),
  alpha = 0.2) + 
  scale_color_manual(values=colors1) + 
  scale_fill_manual(values=colors2) + 
  scale_x_date(breaks = "3 day", date_labels = "%b %d") + 
  theme_minimal() + 
  theme(legend.title=element_blank())

gg2 = 
  ggplot() +
  geom_point(aes(
    x = as.Date(P_E$DatesR[2627:2653]),
    y = Qtrue[, 2][2627:2653],
    color = "Qtrue"
  )) +
  geom_line(aes(
    x = as.Date(P_E$DatesR[2627:2653]),
    y = mean_Qvalid1[70:96,],
    colour = "Qpredicted"
  ), size = 0.9) +
  labs(x = "dates", y = "Discharge", color = "legend") +
  geom_ribbon(aes(
    x = as.Date(P_E$DatesR[2627:2653]),
    ymin = qq_valid[, 1][70:96],
    ymax = qq_valid[, 2][70:96],
    fill = "CI for input Uncertainity"
  ),
  alpha = 0.2) + 
  scale_color_manual(values=colors1) + 
  scale_fill_manual(values=colors2) + 
  scale_x_date(breaks = "3 day", date_labels = "%b %d") + 
  theme_minimal() + 
  theme(legend.title=element_blank())


gg3 = 
  ggplot() +
  geom_point(aes(
    x = as.Date(P_E$DatesR[3048:3100]),
    y = Qtrue[, 2][3048:3100],
    color = "Qtrue"
  )) +
  geom_line(aes(
    x = as.Date(P_E$DatesR[3048:3100]),
    y = mean_Qvalid1[491:543,],
    colour = "Qpredicted"
  ), size = 0.9) +
  labs(x = "dates", y = "Discharge",color = "legend") +
  geom_ribbon(aes(
    x = as.Date(P_E$DatesR[3048:3100]),
    ymin = qq_valid[, 1][491:543],
    ymax = qq_valid[, 2][491:543],
    fill = "CI for input Uncertainity"
  ),
  alpha = 0.2) + 
  scale_color_manual(values=colors1) + 
  scale_fill_manual(values=colors2) + 
  scale_x_date(breaks = "5 day", date_labels = "%b %d") + 
  theme_minimal() + 
  theme(legend.title=element_blank())

gg4 = 
  ggplot() +
  geom_point(aes(
    x = as.Date(P_E$DatesR[2961:2978]),
    y = Qtrue[, 2][2961:2978],
    color = "Qtrue"
  )) +
  geom_line(aes(
    x = as.Date(P_E$DatesR[2961:2978]),
    y = mean_Qvalid1[404:421,],
    colour = "Qpredicted"
  ), size = 0.9) +
  labs(x = "dates", y = "Discharge", color = "legend") +
  geom_ribbon(aes(
    x = as.Date(P_E$DatesR[2961:2978]),
    ymin = qq_valid[, 1][404:421],
    ymax = qq_valid[, 2][404:421],
    fill = "CI for input Uncertainity"
  ),
  alpha = 0.2) + 
  scale_color_manual(values=colors1) + 
  scale_fill_manual(values=colors2) + 
  scale_x_date(breaks = "3 day", date_labels = "%b %d") + 
  theme_minimal() + 
  theme(legend.title=element_blank())

gg5 = 
  ggplot() +
  geom_point(aes(
    x = as.Date(P_E$DatesR[3297:3337]),
    y = Qtrue[, 2][3297:3337],
    color = "Qtrue"
  )) +
  geom_line(aes(
    x = as.Date(P_E$DatesR[3297:3337]),
    y = mean_Qvalid1[740:780,],
    colour = "Qpredicted"
  ), size = 0.9) +
  labs(x = "dates", y = "Discharge",color = "legend") +
  geom_ribbon(aes(
    x = as.Date(P_E$DatesR[3297:3337]),
    ymin = qq_valid[, 1][740:780],
    ymax = qq_valid[, 2][740:780],
    fill = "CI for input Uncertainity"
  ),
  alpha = 0.2) + 
  scale_color_manual(values=colors1) + 
  scale_fill_manual(values=colors2) + 
  scale_x_date(breaks = "4 day", date_labels = "%b %d") + 
  theme_minimal() + 
  theme(legend.title=element_blank())

gg6 = 
  ggplot() +
  geom_point(aes(
    x = as.Date(P_E$DatesR[3357:3421]),
    y = Qtrue[, 2][3357:3421],
    color = "Qtrue"
  )) +
  geom_line(aes(
    x = as.Date(P_E$DatesR[3357:3421]),
    y = mean_Qvalid1[800:864,],
    colour = "Qpredicted"
  ), size = 0.9) +
  labs(x = "dates", y = "Discharge", color = "legend") +
  geom_ribbon(aes(
    x = as.Date(P_E$DatesR[3357:3421]),
    ymin = qq_valid[, 1][800:864],
    ymax = qq_valid[, 2][800:864],
    fill = "CI for input Uncertainity"
  ),
  alpha = 0.2) + 
  scale_color_manual(values=colors1) + 
  scale_fill_manual(values=colors2) + 
  scale_x_date(breaks = "5 day", date_labels = "%b %d") + 
  theme_minimal() + 
  theme(legend.title=element_blank())

figure <- 
  ggarrange(gg1, gg2, gg3,gg4,gg5,gg6,
            ncol = 2, nrow = 3,common.legend=T,legend = "bottom")

