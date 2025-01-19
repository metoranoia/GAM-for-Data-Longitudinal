library(lattice)
library(ggeffects)
library(gratia)
library(ggplot2)
library(readxl)
library(PerformanceAnalytics)
library(geepack)

data=read_excel("C:/Users/tori/OneDrive - Universitas Airlangga/Dokumen/data adl 3.xlsx")
str(data)
names(data)
View(data)

#Visualisasi LVEF per Periode pada 47 Pasien
# Compute the mean LVEF for each period
library(dplyr)

mean_data <- data %>%
  group_by(Time) %>%
  summarise(mean_LVEF = mean(Y_LVEF, na.rm = TRUE))

# Plot with the trend line
ggplot(data, aes(x = Time, y = Y_LVEF, color = ID)) +
  geom_point(size = 3) +    
  geom_line(aes(group = ID)) +
  geom_line(data = mean_data, aes(x = Time, y = mean_LVEF), 
            color = "red", size = 1.5) +
  labs(title = "LVEF Graph of Each Observation in 47 Patients",
       x = "Period",
       y = "LVEF") +
  theme_minimal()


ggplot(data, aes(x = factor(Time), y = X1_BMI, fill = factor(Time))) +
  geom_boxplot() +
  labs(title = "BMI Boxplot of Each Observation in 47 Patients",
       x = "Period",
       y = "BMI") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(data, aes(x = factor(Time), y = X2_VitD, fill = factor(Time))) +
  geom_boxplot() +
  labs(title = "Vitamin D Boxplot of Each Observation in 47 Patients",
       x = "Period",
       y = "Vitamin D") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(data, aes(x = factor(Time), y = Y_LVEF, fill = factor(Time))) +
  geom_boxplot() +
  labs(title = "LVEF Boxplot of Each Observation in 47 Patients",
       x = "Period",
       y = "LVEF") +
  theme_minimal() +
  theme(legend.position = "none")


#Scatter Plot Korelasi Y antar Periode
library(tidyr)
data_scatter_plot <- data.frame(Periode = data$Time,
                                ID = data$Patient_ID,
                                LVEF = data$Y_LVEF)

data_wide <- data_scatter_plot %>%
  pivot_wider(names_from = Periode, values_from = LVEF)
head(data_wide)

panel.fixed.cor <- function(x, y, digits = 2, prefix = "", cex.cor = 2, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use = "complete.obs") 
  txt <- format(c(r, 0.123456789), digits = digits)[1]  
  txt <- paste0(prefix, txt)
  text(0.5, 0.5, txt, cex = cex.cor) 
}
suppressWarnings(pairs(data_wide[,-1], lower.panel = panel.smooth, upper.panel = panel.fixed.cor))

#Matriks Korelasi antar Y dan Variabel Prediktor
my_data <- data.frame(LVEF= data$Y_LVEF,
                      Periode = data$Time,
                      BMI = data$X1_BMI,
                      Vit_D = data$X2_VitD)
suppressWarnings(chart.Correlation(my_data, histogram=TRUE, pch=19))

#Pemodelan GAM
library(splines)
longitudinal_data <- read_excel("C:/Users/tori/OneDrive - Universitas Airlangga/Dokumen/data adl 3.xlsx")
#Model 1 (Independence)
model1 <- geeglm(Y_LVEF ~ X1_BMI + bs(X2_VitD,degree=3), data = longitudinal_data,
                id = as.factor(ID), family = gaussian, corstr = "independence")
summary(model1)
QIC(model1)

predicted_values <- predict(model1, type = "response")
observed_values <- longitudinal_data$Y_LVEF
# Calculate the pseudo R-squared
pseudo_r_squared <- cor(observed_values, predicted_values)^2
print(pseudo_r_squared)

#Model 2 (Exchangeable)
model2 <- geeglm(Y_LVEF ~ X1_BMI + bs(X2_VitD, 3), 
                 id = Patient_ID, data = longitudinal_data, 
                 corstr = "exchangeable")
summary(model2)
QIC(model2)

predicted_values <- predict(model2, type = "response")
observed_values <- longitudinal_data$Y_LVEF
# Calculate the pseudo R-squared
pseudo_r_squared <- cor(observed_values, predicted_values)^2
print(pseudo_r_squared*100)

#Model 3 (Unstructured)
model3 <- geeglm(Y_LVEF ~ X1_BMI + bs(X2_VitD, 3), 
                 id = Patient_ID, data = longitudinal_data, 
                 corstr = "unstructured")
summary(model3)
QIC(model3)

predicted_values <- predict(model3, type = "response")
observed_values <- longitudinal_data$Y_LVEF
# Calculate the pseudo R-squared
pseudo_r_squared <- cor(observed_values, predicted_values)^2
print(pseudo_r_squared)

#Model 4 (AR-1)
model4 <- geeglm(Y_LVEF ~ X1_BMI + bs(X2_VitD, 3), 
                 id = Patient_ID, data = longitudinal_data, 
                 corstr = "ar1")
summary(model4)
QIC(model4)

predicted_values <- predict(model4, type = "response")
observed_values <- longitudinal_data$Y_LVEF
# Calculate the pseudo R-squared
pseudo_r_squared <- cor(observed_values, predicted_values)^2
print(pseudo_r_squared)
