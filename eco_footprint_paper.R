##------------------##
## WD and Libraries ##
##------------------##
setwd("C:/Users/pagesim/Desktop/ps369/final_paper/")
library(tidyverse)
library(cowplot)
library(readxl)

##-----------------------------##
## Loading & Manipulating Data ##
##-----------------------------##
df <- read.csv("countries.csv")

df2 <- read_xlsx("dataweb-queryExport.xlsx")

dat <- merge.data.frame(df,df2,by = "Country",all = T)

dat <- dat %>% 
  filter(!is.na(Region)) %>%
  select(-`Data Type`,) #%>%
  #print

## Changing GDP Column to Numeric, Removing Symbols
dat$GDP.per.Capita <-  as.numeric(gsub('[$,]', '', 
                                       dat$GDP.per.Capita))
##--------------##
## Checking NAs ##
##--------------##
datNAs <- dat %>% summarise_all(~(sum(is.na(.))/n())) %>% 
  gather(key = "variables", value = "percent_missing") %>% 
  filter(percent_missing>0)

ggplot(datNAs, aes(fct_reorder(variables,percent_missing),
                   percent_missing)) +
  geom_col(fill="pink") +
  coord_flip() +
  labs(x="Predictors",y="Proportion of Data Missing") +
  ggtitle("Proportion of Missing Data (if Data is Missing)") +
  theme(plot.title = element_text(hjust = 0.5))

##----------##
## Plotting ##
##----------##

## Plotting Total Footprint
ggplot(dat,aes(Total.Ecological.Footprint)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "lightblue") +
  geom_density(alpha = 0.2, fill = "blue") +
  geom_vline(xintercept = mean(dat$Total.Ecological.Footprint), 
             size = 1, linetype = 3) +
  annotate("text",5,0.3,size = 5,
           label = "Average=3.32") +
  labs(x="Total Ecological Footprint",y="Density") + 
  ggtitle("Total per Capita Footprint") +
  theme(plot.title = element_text(hjust = 0.5))

## Plotting Trade Balance
n<-dim(df2)[1] # Removing Final Row
df2<-df2[1:(n-1),] # Since it is a Total Row

df2 %>% 
  select(-1,-23) %>%
  pivot_longer(cols = 2:21,names_to = "Years") %>%
  mutate(Years=str_sub(Years,-4),
         Years=as.numeric(Years)) %>%
  rename(trade_balance=value) %>% 
  group_by(Years) %>% 
  summarise(mean_trade_balance=mean(trade_balance,na.rm=T)) %>% 
  print %>% 
    ggplot(aes(Years,mean_trade_balance)) +
    geom_col(fill="pink") +
    labs(x="Years 2000-2020(Year-to-Date)",
         y="Average Trade Balance of US") +
    ggtitle("Average US Trade Balance") +
    theme(plot.title = element_text(hjust = 0.5))

## Plotting Relationship between GDP and Footprint
# ggplot(dat,aes(GDP.per.Capita,Total.Ecological.Footprint)) +
#   geom_point() +
#   geom_smooth(method = "gam",se=F) +
#   geom_smooth(method = "loess",col="red",se=F) +
#   labs(x="GDP per Capita",y="Total Ecological Footprint") +
#   ggtitle("Total Ecological Footprint vs. GDP per Capita for Each Country") +
#   theme(plot.title = element_text(hjust = 0.5))

##------------------------##
## Tables and Regressions ##
##------------------------##

## Largest Polluters
dat %>% 
  select(Country,Population..millions.,
         Total.Ecological.Footprint) %>% 
  arrange(desc(Total.Ecological.Footprint)) %>%
  rename("Population(Millions)"="Population..millions.",
         "Footprint.Per.Person"="Total.Ecological.Footprint") %>%
  head(n=15)

## Correlation between GDP per Capita and Footprint
cor.test(dat$GDP.per.Capita,
         dat$Total.Ecological.Footprint) # High Corr, Small Interval

## Linear Model of Footprint vs. GDP and Population 
model <- lm(Total.Ecological.Footprint~GDP.per.Capita,dat)
summary(model)

## Randomly Selecting a Trade Year (set.seed for Reproducibility)
set.seed(12345)
sample(dat[,c(22:41)],1)

## Multiple Linear Model, Adding Population and Trade Year (Above)
model2 <- lm(Total.Ecological.Footprint~
               GDP.per.Capita*Population..millions.*`Year 2013`,dat)
summary(model2)

## Plotting Residuals of 2nd Model for Randomness

# plot(model2$residuals)
#   abline(h = 0,col="red")
# 
# hist(model2$residuals)





