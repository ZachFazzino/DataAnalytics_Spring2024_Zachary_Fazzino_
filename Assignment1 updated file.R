library(tidyverse)
library(ggplot2)

#We will be using 2 different data sets in this file seen below

EPI = read.csv("Assignment 1/2010EPI_data(updated).csv")
grump = read.csv("Assignment 1/GRUMP_DATA.csv")

#These are large data sets and will be the raw data. these data sets will be 
#modified and changes in ways to understand the data better.

#We will first look at the EPI data set then move on to the GRUMP data set

summary(EPI$EPI)
fivenum(EPI$EPI)
hist(EPI$EPI)
stem(EPI$EPI)

#Note the code below all work together to create a more complex graph.

hist(EPI$EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI$EPI,na.rm=TRUE,bw=1.)) 
rug(EPI$EPI)

#Now we will look at a ggplot of the histogram allowing for more customization. 

EPI_plot <- ggplot(EPI, aes(x = EPI))+
  geom_histogram(aes(y = after_stat(density)),binwidth = 2,color = 'black', fill = 'seagreen')+
  labs(x = "EPI", y = "Frequency", title =  "EPI Histogram")+
  geom_density(aes(y = after_stat(density)),color = 'blue')
EPI_plot

#Now I'll do a cumulative density function. This can also be done in ggplot if necessary

plot(ecdf(EPI$EPI), do.points = FALSE, verticals = TRUE)

#I'll now look at a Q-Q plot.

ggplot(EPI, aes(sample = EPI))+
  stat_qq(color = 'purple')+
  labs(title = "EPI Q-Q Plot")

#Make a Q-Q plot against the generating distribution by:

x <- seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t
dsn")
qqline(x)

#To continue we will look at different variables than EPI in the EPI data set.

#BIODIVERSITY
summary(EPI$BIODIVERSITY)
fivenum(EPI$BIODIVERSITY)
hist(EPI$BIODIVERSITY)
stem(EPI$BIODIVERSITY)

BIODIVERSITY_plot <- ggplot(EPI, aes(x = BIODIVERSITY))+
  geom_histogram(aes(y = after_stat(density)),binwidth = 3,color = 'black', fill = 'seagreen')+
  labs(x = "BIODIVERSITY", y = "Frequency")+
  geom_density(aes(y = after_stat(density), color = 'orange',))
  
BIODIVERSITY_plot

plot(ecdf(EPI$BIODIVERSITY), do.points = FALSE, verticals = TRUE)

ggplot(EPI, aes(sample = BIODIVERSITY))+
  stat_qq()+
  labs(main = "BIODIVERSITY Q-Q Plot")

#This is the same data but now for biodiversity. You will see the same done with 
#climate below

summary(EPI$CLIMATE)
fivenum(EPI$CLIMATE)
hist(EPI$CLIMATE)
stem(EPI$CLIMATE)

CLIMATE_plot <- ggplot(EPI, aes(x = CLIMATE))+
  geom_histogram(aes(y = after_stat(density)),binwidth = 3,color = 'black', fill = 'seagreen')+
  labs(x = "CLIMATE", y = "Frequency")+
  geom_density(aes(y = after_stat(density), color = 'orange',))

CLIMATE_plot

plot(ecdf(EPI$CLIMATE), do.points = FALSE, verticals = TRUE)

ggplot(EPI, aes(sample = CLIMATE))+
  stat_qq()+
  labs(main = "CLIMATE Q-Q Plot")

#Now we will look at some ways to use boxplots

boxplot(EPI$EPI,EPI$CLIMATE)
boxplot(EPI$BIODIVERSITY,EPI$CLIMATE)
boxplot(EPI$EPI, EPI$BIODIVERSITY)

#these are 3 simple box plots but we could do better using ggplot

EBC = pivot_longer(EPI, cols = c(EPI,BIODIVERSITY,CLIMATE), names_to = "EBC", values_to = "Value")

boxplot = ggplot(EBC, aes(x = EBC, y = Value ))+
  labs(title = "EPI vs BIODIVERSITY vs CLIMATE", x = "")+
  geom_boxplot()
boxplot

#as seen you can express all 3 box plots in one graph

#now we will intercompare many of the available variables to have good comparisons 
#of all different categories.

boxplot(EPI$EPI,EPI$BIODIVERSITY)
boxplot(EPI$EPI,EPI$ENVHEALTH)
boxplot(EPI$EPI,EPI$ECOSYSTEM)
boxplot(EPI$EPI,EPI$DALY)
boxplot(EPI$EPI,EPI$AIR_H)
boxplot(EPI$EPI,EPI$WATER_H)
boxplot(EPI$EPI,EPI$AIR_E)
boxplot(EPI$EPI,EPI$WATER_E)

#Next step is to begin filtering EPI to create more concise and easy to manage data sets.
#there are many ways to do this.

Europe_EPI = EPI%>%
  filter(EPI_regions == "Europe")

#as seen above we can name a new data set and have to original large data set filtered to
#only show a certain subset of the data

EPI_By_Subregion = EPI%>%
  arrange(GEO_subregion)

split_subregions = split(EPI_By_Subregion, EPI_By_Subregion$GEO_subregion)

x_subregion = split_subregions[[""]]
#above is a line that will give you a data set for an individual region if added
#to the function. Example seen below

northern_africa_subregion = split_subregions[["Northern Africa"]]

#With the above function you can easily isolate any group or region from the EPI
#dataset and make a more concise dataset representing that specific subgroup

#Now we will make some new data sets and analyze them

#No Surface Water
NSW_EPI = EPI%>%
  filter(No_surface_water == 1)

#Presence of a desert
Desert_EPI = EPI%>%
  filter(Desert == 1)

#High Population Density
HPD_EPI = EPI %>% 
  filter(High_Population_Density == 1)

#these 3 new data sets only contain data where: There is no surface water(NSW),
#there is a desert(Desert), or where there is a high population density(HPD).
 
#Now we can compare these data sets to get info about how these particular 
#features impact variables such as EPI

#we can now have box plots comparing EPI under certain circumstances:

boxplot(Desert_EPI$EPI, HPD_EPI$EPI)
boxplot(Desert_EPI$EPI, NSW_EPI$EPI)
boxplot(NSW_EPI$EPI, HPD_EPI$EPI)

#we can also perform everything from exercise 1 on these data sets

#Desert
summary(Desert_EPI$EPI)
fivenum(Desert_EPI$EPI)
hist(Desert_EPI$EPI)
plot(ecdf(Desert_EPI$EPI), do.points = FALSE, verticals = TRUE)

ggplot(Desert_EPI, aes(sample = EPI))+
  stat_qq(color = 'darkgreen')+
  labs(title = "Desert EPI Q-Q Plot")

#High Population Density
summary(HPD_EPI$EPI)
fivenum(HPD_EPI$EPI)
hist(HPD_EPI$EPI)
plot(ecdf(HPD_EPI$EPI), do.points = FALSE, verticals = TRUE)

ggplot(HPD_EPI, aes(sample = EPI))+
  stat_qq(color = 'purple')+
  labs(title = "HPD EPI Q-Q Plot")

#No Surface Water
summary(NSW_EPI$EPI)
fivenum(NSW_EPI$EPI)
hist(NSW_EPI$EPI)
plot(ecdf(NSW_EPI$EPI), do.points = FALSE, verticals = TRUE)

ggplot(NSW_EPI, aes(sample = EPI))+
  stat_qq(color = 'brown')+
  labs(title = "NSW EPI Q-Q Plot")

#Now we will do the same with the GRUMP data set

#I want to trim down the data quite a bit here so the first thing ill do is select 
#3 different UN regions and compare them

#Western Africa
WA = grump%>%
  filter(UNRegion == "Western Africa")

summary(WA$PopulationPerUnit)
fivenum(WA$PopulationPerUnit)
hist(WA$PopulationPerUnit)
plot(ecdf(WA$PopulationPerUnit), do.points = FALSE, verticals = TRUE)

ggplot(WA, aes(sample = PopulationPerUnit ))+
  stat_qq(color = 'black')+
  labs(title = "Western Africa Q-Q Plot")

#Central America
CA = grump%>%
  filter(UNRegion == "Central America")

summary(CA$PopulationPerUnit)
fivenum(CA$PopulationPerUnit)
hist(CA$PopulationPerUnit)
plot(ecdf(CA$PopulationPerUnit), do.points = FALSE, verticals = TRUE)

ggplot(CA, aes(sample =PopulationPerUnit ))+
  stat_qq(color = 'darkblue')+
  labs(title = "Central America Q-Q Plot")

#Northern Europe
NE = grump%>%
  filter(UNRegion == "Northern Europe")

summary(NE$PopulationPerUnit)
fivenum(NE$PopulationPerUnit)
hist(NE$PopulationPerUnit)
plot(ecdf(NE$PopulationPerUnit), do.points = FALSE, verticals = TRUE)

ggplot(NE, aes(sample =PopulationPerUnit ))+
  stat_qq(color = 'darkred')+
  labs(title = "Northern Europe Q-Q Plot")

#Now that I have separated the grump data into 3 different regions I can compare them
#to gather data on how these regions differ based on population per unit area

df = bind_rows(WA, CA, NE)

#this new data frame now contains all of the data in one spot making it easier to manage.

ggplot(df, aes(x = UNRegion, y = PopulationPerUnit))+
  geom_boxplot()+
  labs(x = '', y = 'Population Per Unit Area')+
  ggtitle('Population Density of WA vs CA vs NE')











 


