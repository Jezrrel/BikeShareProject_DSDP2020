## Packages to use
library(tidyverse)
library(lubridate)
library(ggcorrplot)
library(lattice)
library(psych)
library(DataExplorer)
library(reshape2)
library(car)
library(caret)
library(cowplot)
library(scales)
library(RColorBrewer)
library(packHV)
library(e1071)
library(vip)
library(xgboost) 
library(ranger)   
library(rpart) 
options(warn=-1)
par(mfrow=c(1,1))

## Data Import from CSV
bike <- readr::read_csv("C:/DataScience/datafile/Operations-Dem-Planning_-BikeShare_transformed_dataset.csv")


#format the Month and day as a number that can be ordered as a factor
bike$Date <- as.character(bike$Date, format = "%e/%d/%y")
# create month day as a number that we can order as a factor
bike$Year <- as.factor(as.character(bike$Year, format = "%Y"))
bike$Month <- as.factor(as.character(bike$Month, format = "%M"))
bike$month <- as.character(bike$Month, format = "%m")
bike$Day <- as.factor(as.character(bike$Date, format = "%u"))
bike$day <- as.character(bike$Date, format = "%u")

##Factor Recode

### --- Month --- ####
bike$month <- factor(
  bike$Month, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"),
  labels = c('JA','FB', 'MR', 'AP', 'MY', 'JN', 'JL', 'AU','SP','OT', 'NV', 'DR'),
  ordered = TRUE)

bike$Month <- factor(
  bike$Month, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"),
  labels = c('January','February', 'March', 'April',
             'May', 'June', 'July', 'August',
             'September','October', 'November', 'December'),
  ordered = TRUE)

#### ---  Day ----#####

bike$Weekday <- factor(
  bike$weekday,levels = c(1,2,3,4,5,6,0),
  labels = c('M','T','W','TH','F','SA','SU'),
  ordered = TRUE)

bike$weekday <- factor(
  bike$Weekday,levels = c(1,2,3,4,5,6,0),
  labels = c('Monday','Tuesday','Wednesday','Thursday',
             'Friday', 'Saturday','Sunday'),
  ordered = TRUE)

###  ---  Season ----  ####
bike$Season <- factor(
  bike$Season, levels = c(1,2,3,4),
  labels = c('Winter', 'Spring', 'Summer','Autumn'),
  ordered = TRUE)

#### ---  Workday --- ####
#table(df$Workday)
bike$Workday <- factor(bike$Workday,
                     levels = c(0,1),
                     labels = c('Non-Workday', 'Workday'))

#### ---- Holiday ---  ####
#table(df$Holiday)
bike$Holiday <- factor(bike$Holiday,
                     levels = c(0,1),
                     labels = c('No-Holiday', 'Holiday'))

####  weather    ####
#table(df$Weather)
bike$Weather <- factor(bike$Weather,
                       levels = c(1,2,3,4),
                       labels = c('Sunny', 'Cloudy','Rainy','Snowy'))

glimpse(bike)
class(bike)
##Visualize Data

# set the color palette
pal = c("olivedrab3", 'yellow', 'orange', 'grey50')

#####   Riders by Season & Temperature    #####

options(repr.plot.width=12, repr.plot.height=7)
ggplot(bike, aes(Temperature, Riders, color = Season)) + geom_point() +
  theme_bw(base_size = 18) + scale_color_manual(values = pal) +
  labs(title = "Riders by Season & Temperature", x = "Temperature", y = "Total Riders") +
  scale_y_continuous(labels = scales::label_comma())



# ----   Riders & Temperature by Weather
pal = c("olivedrab3", 'yellow', 'orange', 'grey50')
W = c('skyblue1','skyblue2','skyblue3','skyblue4','grey40')
ggplot(bike, aes(Temperature, Riders, color = Weather)) + 
  geom_jitter(width = 0.25, show.legend = F) + 
  scale_color_manual(values = W) +
  labs(y="Count of Riders", title = "Riders & Temperature by Weather") + 
  facet_grid(Workday ~ Weather) + theme_bw(base_size = 18) +
  theme(strip.background = element_rect(fill="grey90"))

#### ---  Riders by hour & Workday--- #####

cols = c("#41B6C4", "#C7E9B4", "#7FCDBB", "#1D91C0")
options(repr.plot.width=14, repr.plot.height=20)
bike %>% 
  filter(Hour %in% c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) %>%
  ggplot(aes(x=factor(Hour),y=Riders, fill=Workday)) +
  geom_boxplot(show.legend = F, lwd = 1.0) + theme_bw(base_size = 10) +
  facet_grid(Workday~.) + scale_fill_manual(values=cols)+
  theme(strip.background = element_rect(fill="grey90"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title ="Riders by Hour & Workday",
     x = "Hour of the Day", y = "Count of Riders")

#### Data Distributions - Is data normally distributed?

par(mfrow=c(2,2))
options(repr.plot.width=12, repr.plot.height=7)
hist_boxplot(bike$Riders, main = "Riders", col = "aquamarine3", xlab = "Riders");
hist_boxplot(bike$Temperature, main = "Temperature", col = "darkgoldenrod3", xlab = "Temperature");
hist_boxplot(bike$Humidity, main = "Humidity", col = "coral4",xlab = "Humidity");
hist_boxplot(bike$Wind, main = "Wind", col = "darkslategray4", xlab = "Wind");
par(mfrow=c(1,1))


### ---   QQ-Plots ----- ###

par(mfrow=c(2,2))

options(repr.plot.width=19, repr.plot.height=10)

qqnorm(bike$Riders, main = "Normal Q-Q Plot of Riders", 
       col = "lightblue");qqline(bike$Riders); 
qqnorm(bike$Temperature, main = "Normal Q-Q Plot of Temperature", 
       col = "lightblue");qqline(bike$Temperature); 
qqnorm(bike$Humidity, main = "Normal Q-Q Plot of Humidity", 
       col = "lightblue",);qqline(bike$Humidity); 
qqnorm(bike$Wind, main = "Normal Q-Q Plot of Wind", 
       col = "lightblue");qqline(bike$Wind); 
par(mfrow=c(1,1))


#####    Skew of the Distirbutions     ######

# does skewness deviate a lot from 1 ?
skewness(bike$Riders); 
skewness(bike$Temperature); 
skewness(bike$Humidity);
skewness(bike$Wind)


####       variable importance using Ranger      #####

fit_m <- rpart(Riders ~ ., data = bike);
options(repr.plot.width=12, repr.plot.height=7)
vi_m <- fit_m$variable.importance;
barplot(vi_m, horiz = F, las = 1, col = "#1D91C1", 
        main = "Variable importance - Riders")
glimpse(bike)

#####       Select only variables with high importance      #####

bike2 <- bike %>% select(Riders, Hour, Workday, Season, 
                      Weather, Holiday,Weekday,Date,Month)
names(bike2)
glimpse(bike2)

####          Machine Learning - Bike Rentals           ######
#####         Cross Validation Model Comparison          ######

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=10,
                        selectionFunction = "oneSE")

sample = sample_n(bike2, size = 15000, replace = TRUE)

######      Linear model        #####

m_lm <- train(Riders~., data=sample, method="lm", trControl=control);

######        Partial Least Squares         ######

m_pls <- train(Riders ~., data = sample,
               method = 'pls', 
               preProcess = c("center", "scale"),
               trControl=control,
               tunelength = 5);

######-----  Visualize the Cross Validation  ----- #####
options(repr.plot.width=10, repr.plot.height=5)
plot(m_pls, main = "Partial Least Squares Model");


#######        Ridge Regression       ######


set.seed(400) # Ridge Regression with Variable Selection
m_ridge <- train(Riders~., data=sample, method = 'ridge',
                 preProcess = c("center", "scale"),
                 trControl=control);


#######        Lasso Regression       ######


set.seed(400) # Quantile Regression with LASSO penalty
m_lasso <- train(Riders ~., data=sample, method = 'lasso', 
                 preProcess = c("center", "scale"),
                 trControl=control);

######-----  Visualize the Cross Validation  ----- #####
#####-------- & best tuned model parameters  ----- #####

par(mfrow=c(1,1))
options(repr.plot.width=14, repr.plot.height=5)
a = plot(m_ridge, main = "Ridge Regressio Model");
b = plot(m_lasso, main = "Lasso Regression Model");
plot_grid(a,b, ncol = 2, nrow = 1)
