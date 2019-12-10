
library(readxl)
library(ggplot2)
library(mgcv)
library(akima)
library(plotly)

setwd("C:/Users/Bjorn/Documents/LIU/machine_learning/labs")

### Assignment 1. Using GAM and GLM to examine the mortality rates ###

## 1
data = read_excel("Influenza.xlsx")
ggplot(data=data, aes(x=Time, y=Mortality))+ 
  geom_bar(stat = "identity")
ggplot(data=data, aes(x=Time, y=Influenza))+ 
  geom_bar(stat = "identity")
# There does not seem to be a clear relationship between influenza and mortality by viewing
# the plots. There is perhaps a peak of influenza when there is a relative peak of mortality
# during the year of 2000. 

## 2
gam_model = gam(Mortality~Year+s(Week, k=length(unique(data$Week))), data=data, 
                family = gaussian(link = "identity"))
gam_model2 = gam(Mortality~Year+s(Week)+s(Year, k=length(unique(data$Year))),
                 data=data, family = gaussian(link = "identity"))
summary(gam_model)
plot(gam_model2)

## 3
gam_model$sp
#plot_ly(x=~gam_model$x , y=~gam_model$y, z=~gam_model$z, type="surface")
plot(gam_model) 
plot(gam_model, shift=mean(data$Mortality), residuals=T, pch=1, xlab="") #plot with data points included.
gam.check(gam_model) #Gives some interesting inforamtion about the model. 
s=interp(data$Year,data$Week, fitted(gam_model))
gam_fitted.results = predict(gam_model, newdata=data) 

ggplot(data=data, aes(x=Time, y=gam_fitted.results))+ 
  geom_bar(stat = "identity") # plot shows the fitted values against time.
# As is seen in the histogram of the residuals, the residuals have a normal distribution around 0.
# This indicats that the model is a good fit. 
# The mortality rate seems to go in waves or what seems to be many normal distributions "squished" together.
# From viwing the spline plot it seems that the week nr is an important contributing factor. The mortality
# is high around the end and start of the new year and drops around the middle of the year. 

## 4
k = seq(3,length(unique(data$Week)),5)
for(i in k){
model = gam(Mortality~Year+s(Week, k=i), data=data, 
                        family = gaussian(link = "identity"))
plot(model)
}
# As can be seen from the plots above, the lower penatly factor makes the model fit the data more loosly
# whereas a larger penalty factor gives the model a exact fit. The fit of the model does however not change
# much after a penalty factor of 13 is introduced. 

## 5
ggplot(data=data, aes(x=Time))+ 
  geom_point(aes(y=gam_model$residuals), color="darkred")+
  geom_point(aes(y=Influenza), color="steelblue")+
  ggtitle("Residuals/ Influenza values against time")
# Viewing the plot above it would seem viable to say that the temporal pattern in the
# residuals correlate to the outbreaks of influenza.

## 6
gam_model3 = gam(Mortality~ s(Year, k=length(unique(data$Year)))+s(Week, k=length(unique(data$Week)))+
                   s(Influenza, k=length(unique(data$Influenza))), data=data, family = gaussian(link = "identity"))
plot(gam_model3) # plot new model.
ggplot(data=data, aes(x=Time))+ 
  geom_point(aes(y=gam_model3$fitted.values), color="darkred")+
  geom_point(aes(y=Influenza), color="steelblue")+
  ggtitle("Mortality/ Fitted Influenza values values against time") #plot shows some degree of rise of mottality together with influenza.
plot(gam_model) # previoud model.
# If mortality is influenced by influenza then the plot of mortality against influenza should be some what linear, meaning
# as influenza cases grows so should mortality rates. According to the graph above there is slight overall linearity but the 
# the plot is still varied to a large degree. Therefor it does not seem the case that mortality is not influenced by influenza greatly.
# The model for mortality against week is a bit more even and does not very as much as prevously, it can therefor be concluded that 
# the new model does seem to be better than the previous model.



