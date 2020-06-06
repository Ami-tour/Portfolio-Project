#Final Project

#reading .csv files
nwewage <- read.csv("MWE_2018complete_CSV.csv")
state_price_parity <- read.csv("state_price_parity.csv") %>%
  filter(!grepl('Alaska|Hawaii', Region))
metro_price_parity <- read.csv("metro_price_parity.csv")
state_real_gdp <- read.csv("Real_GDP_CSV.csv")
state_employment <- read.csv("State_Emp18_CSV.csv")
state_educational_level <- read.csv("Educated_States_CSV.csv")
state_ll <- read.csv("States_LL.csv")
metro_ll <- read.csv("metro_ll.csv")

#installing packages
install.packages("tidyverse")
install.packages("plotly")
install.packages("ggmap")
install.packages("maps")
install.packages("mapdata")
install.packages("DT")
install.packages("ggrepel")  
install.packages("car")       
install.packages("modelr")    
install.packages("scales")    
install.packages("gridExtra")
install.packages("dplyr")

#loading packages
library(tidyverse)
library(plotly)    
library(ggmap)     
library(maps)      
library(mapdata)   
library(DT)        
library(ggrepel)   
library(car)       
library(modelr)    
library(scales)    
library(gridExtra)
library(dplyr)

##Data Preparation
wage <-
  nwewage %>%
  select (Average.hourly.wage, Area.level, Region,Occupation.text) %>%
  filter(!grepl('Alaska|Hawaii|*AK|*HI', Region))
states <- map_data("state")
  
#Table
head(wage,100) %>%
  datatable(options = list (
    columnDefs = list(list(className= 'dt-center', targets= 1:4))
  ))  

##Data Analysis

#Comparing Wages among different Data Analyst related jobs on a National Level

National_Wage <-
  wage %>%
  group_by(Occupation.text) %>%
  filter(grepl('National',Area.level)) %>%
  filter(grepl('Analysts|Data|Intelligence|Software|Solutions|Consultant|Operation|
                Computer Systems|Information Security', `Occupation.text`)) %>%
  filter(!grepl('Keyers|Clerk', `Occupation.text`))

National_Wage$Average.hourly.wage<-as.character(National_Wage$Average.hourly.wage)
National_Wage$Average.hourly.wage<-as.numeric(National_Wage$Average.hourly.wage)

anova <- aov(`Average.hourly.wage`~`Occupation.text`, data = National_Wage)
summary(anova)

#Boxplots
ggplotly(
  National_Wage %>%
    ggplot(aes(reorder(Occupation.text,Average.hourly.wage), Average.hourly.wage)) +
    labs (x= 'Data Analyst Related Jobs') +
    geom_boxplot()+
    scale_y_continuous()+
    ggtitle("Wages of Data Analyst Related Jobs") +
    theme_minimal()+
    theme(panel.grid.minor = element_blank(),
          text = element_text(family = "Helvetica"),
          plot.title = element_text(size = 20, margin = margin(b=11))) +
    coord_flip(),
  width = 800, height = 650)

##Comparing Wages among different Data Analyst related jobs on a State Level

wage$Average.hourly.wage<-as.character(wage$Average.hourly.wage)
wage$Average.hourly.wage<-as.numeric(wage$Average.hourly.wage)
wage$Region<-as.character(wage$Region)
state_ll$Region<-as.character(state_ll$Region)

orAnalysts <-
  wage %>%
  group_by(Region) %>%
  filter(grepl('State',Area.level)) %>%
  filter(grepl('Operations Research Analysts',Occupation.text)) %>%
  summarise(median_wage = median(Average.hourly.wage)) %>%
  left_join(state_ll, by = "Region")


state_price_parity$Region<-as.character(state_price_parity$Region)
wage$Region<-as.character(wage$Region)
wage$Average.hourly.wage<-as.character(wage$Average.hourly.wage)
wage$Average.hourly.wage<-as.numeric(wage$Average.hourly.wage)

newwage <-
  wage %>%
  filter(grepl('Analysts|Data|Intelligence|Software|Solutions|Consultant|Operation|
                Computer Systems|Information Security', Occupation.text)) %>%
  filter(!grepl('Keyers|Clerk', `Occupation.text`)) %>%
  select(Average.hourly.wage,Area.level,Region,Occupation.text) %>%
  filter(grepl('State',Area.level)) %>%
  left_join(state_price_parity) %>%
  mutate(adjusted_wage = Average.hourly.wage/RPP*100)

Adjusted_ORAnalysts <-
  newwage %>%
  group_by(Region) %>%
  filter (grepl('Operations Research Analysts', Occupation.text)) %>%
  summarise (median_wage = median(Average.hourly.wage), median_adjusted_wage=median(adjusted_wage)) %>%
  left_join(state_ll, by = "Region")

#Table
Adjusted_ORAnalysts %>%
  mutate(median_adjusted_wage = format(round(median_adjusted_wage, 2), nsmall = 2),
         median_wage = format(round(median_wage, 2), nsmall = 2)) %>%
  select(Region, median_wage, median_adjusted_wage) %>%
  arrange(desc(median_adjusted_wage)) %>%
  datatable(options = list(columnDefs = list(list(className = 'dt-center', targets = 1:3))))

#Considering other factors

state_educational_level$Region<-as.character(state_educational_level$Region)
state_real_gdp$Region<-as.character(state_real_gdp$Region)
state_employment$TOT_EMP<-as.character(state_employment$TOT_EMP)
regression_wage<-
  newwage%>%
  left_join(state_real_gdp)%>%
  left_join(state_employment)%>%
  mutate(TOT_EMP = as.numeric(as.character(TOT_EMP))) %>%
  left_join(state_educational_level) %>%
  select(Region,Occupation.text, Average.hourly.wage, adjusted_wage, Year18, TOT_EMP, Total.Score) %>%
  group_by(Region) %>%
  summarise(median_wage = median(Average.hourly.wage, na.rm = TRUE), 
            median_adjusted_wage = median(adjusted_wage, na.rm = TRUE), 
            Per_Capita_Real_GDP = median(Year18, na.rm = TRUE),
            Employment = median(TOT_EMP, na.rm = TRUE),      
            Education_Level = median(Total.Score, na.rm = TRUE)) %>%
  na.omit()

#Correlation
cor(regression_wage$median_wage, regression_wage$Per_Capita_Real_GDP)
cor(regression_wage$median_wage, regression_wage$Employment)
cor(regression_wage$median_wage, regression_wage$Education_Level)
cor(regression_wage$median_adjusted_wage, regression_wage$Per_Capita_Real_GDP)
cor(regression_wage$median_adjusted_wage, regression_wage$Employment)
cor(regression_wage$median_adjusted_wage, regression_wage$Education_Level)

#Plots
plot1 <-
  regression_wage %>%
  ggplot(aes(median_wage,Per_Capita_Real_GDP)) +
  geom_point(alpha = .3) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  geom_smooth() +
  geom_smooth(method = "lm", color = "green")

plot2 <-
  regression_wage%>%
  ggplot(aes(median_wage, Employment)) +
  geom_point(alpha = .3) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = comma) +
  geom_smooth() +
  geom_smooth(method = "lm", color = "green")

plot3 <-
  regression_wage %>%
  ggplot(aes(median_wage,Education_Level)) +
  geom_point(alpha = .3) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = comma)+
  geom_smooth() +
  geom_smooth(method = "lm", color = "green")
grid.arrange(plot1, plot2, plot3, ncol=3)

#Regression Models

set.seed(31)
sample <- sample(c(TRUE, FALSE), nrow(regression_wage), replace = T, prob = c(0.6, 0.4))
train <- regression_wage[sample, ]

model1 <- lm(median_wage ~ `Per_Capita_Real_GDP`+Employment+`Education_Level`, data = train)
summary(model1)

model2 <- lm(median_wage ~ `Per_Capita_Real_GDP`+Employment, data = train)
summary(model2)

model3 <- lm(median_adjusted_wage ~ `Per_Capita_Real_GDP`+Employment, data = train)
summary(model3)

#Plots of models
par(mfrow=c(2,3))
plot(model2, which = 1)
plot(model2, which = 2)
plot(model2, which = 3)
plot(model2, which = 4)
plot(model2, which = 5)

#Checking multicollinearity and heteroscedasticity
vif(model2)
ncvTest(model2)

#Testing model 2
test<- regression_wage[!sample,]
test %>%
  select(median_wage, `Per_Capita_Real_GDP` , Employment) %>%
  add_predictions(model2) %>%
  summarise(MSE = mean(model2$residuals^2))

#Comparing most significant factors

state_employment %>%
  group_by(Region) %>%
  mutate(TOT_EMP = as.numeric(as.character(TOT_EMP))) %>%
  summarise(Employment = median(TOT_EMP, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(Region,Employment), y = Employment)) +
  geom_bar(aes(fill = Employment),stat = "identity")+
  labs(x = "States") +
  ggtitle("Employment Across States") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(family = "Helvetica", size = 7),
        plot.title = element_text(size = 20, margin = margin(b = 10))) +
  coord_flip()

date()