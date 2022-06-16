library(tidyverse)
library(dplyr)
library(ggplot2)
install.packages("dineq")
library(dineq)
library(stargazer)


# Weighted mean MPCE and weighted mean calorie intake per person per day for each year in the sample 
NSSO.mean <- NSSO1 %>%  
  group_by(year) %>% 
  summarise(mean.mpce = weighted.mean(mpce, wt_int, na.rm=T), mean.cal = weighted.mean(cal_pcpd, wt_int, na.rm=T))

# making line plot: Calorie Consumption Puzzle
graphing <- NSSO.mean %>%
  ggplot() +
  geom_line(aes(x=year, y=mean.mpce, colour="red")) +
  geom_line(aes(x=year, y= mean.cal, colour= "blue"))+
  xlab("Year")+
  ylab("MPCE and Calorie")+
  scale_color_manual(labels=c("Daily Calorie Consumption (KCal)","MPCE (INR)"),
                     values = c("red", "blue"))+
  ggtitle("Monthly Per Capita Expenditure and Daily Calorie Consumption by Year")
graphing


# splitting the dataset into urban/rural for the year 2011 and
# calculating the share for each expenditure decile
urban <- NSSO1 %>% 
  filter(year== 2011, urban==1)

urban.share <- NSSO1 %>% 
  filter(year== 2011, urban==1) %>%  
  mutate(exp.dec= ntiles.wtd(te,10,wt_int)) %>% 
  group_by(exp.dec) %>% 
  summarise(food.share = weighted.mean(foodshare, wt_int))

rural <- NSSO1 %>% 
  filter(year== 2011, urban==0)

rural.share <- NSSO1 %>% 
  filter(year== 2011, urban==0) %>%  
  mutate(exp.dec= ntiles.wtd(te,10,wt_int)) %>% 
  group_by(exp.dec) %>% 
  summarise(food.share = weighted.mean(foodshare, wt_int))

# calculations as Bar graph of urban.share and rural.share

bar.urban.share <- ggplot(urban.share, aes(x=exp.dec, y= food.share)) +
  geom_col(position = "dodge", fill="blue") +
  labs( x = "expenditure deciles", y = "Expenditure share of food in total expenditure") +
  ggtitle("Expenditure share of food for each expenditure decile in urban India, 2011")
bar.urban.share

bar.rural.share <- ggplot(rural.share, aes(x=exp.dec, y= food.share)) +
  geom_col(position = "dodge", fill="red") +
  labs( x = "expenditure deciles", y = "Expenditure share of food in total expenditure") +
  ggtitle("Expenditure share of food for each expenditure decile in rural India, 2011")
bar.rural.share

## . The report of the Rangarajan Committee on poverty measurement (2014) came up with the following recommendation:
## ". . . [the] new poverty line thus work out to MPCE of Rs.972 in rural areas and Rs.1,407 in urban areas in 2011-12."

# Poverty rate
# Urban
urban$line <- if_else(urban$mpce < 1407, 1, 0)
urban %>%
  summarise(poverty_rate = weighted.mean(line,wt_int), na.rm=T)

#Rural
rural$line <- if_else(rural$mpce < 972, 1,0)
rural.poverty_rate <- rural %>%
  summarise(poverty_rate = weighted.mean(line,wt_int), na.rm=T)


# Poverty rate urban religion and caste
urban.religion <- urban %>%
  group_by(religion) %>% 
  summarise(poverty_rate.religion = weighted.mean(line,wt_int))

urban.religion$religion <- factor(urban.religion$religion,
                                  levels = c("1", "2", "3"),
                                  labels = c("Hindu", "Muslim", "Other"))
stargazer(as.data.frame(urban.religion), 
          summary = FALSE,
          title = "Poverty Rate for different religions in urban India (2011)",
          out = "povertyrate.urban.religion.html",
          type = "html",
          covariate.labels = c("S. No.", "Religion", "Poverty Rate"))

urban.caste <- urban %>%
  group_by(caste) %>% 
  summarise(poverty_rate.caste = weighted.mean(line,wt_int)) %>% 
  na.omit()

urban.caste$caste <- factor(urban.caste$caste,
                            levels = c("0","1", "2"),
                            labels = c("Others", "ST", "SC"))

stargazer(as.data.frame(urban.caste), 
          summary = FALSE,
          title = "Poverty Rate for different castes in urban India (2011)",
          out = "povertyrate.urban.caste.html",
          type = "html",
          covariate.labels = c("S. No.", "Caste", "Poverty Rate"))

# Poverty rate rural religion and caste
rural.religion <- rural %>%
  group_by(religion) %>% 
  summarise(poverty_rate.religion = weighted.mean(line,wt_int)) %>% 
  na.omit()

rural.religion$religion <- factor(rural.religion$religion,
                                  levels = c("1", "2", "3"),
                                  labels = c("Hindu", "Muslim", "Other"))

stargazer(as.data.frame(rural.religion), 
          summary = FALSE,
          title = "Poverty Rate for different religions in rural India (2011)",
          out = "povertyrate.rural.religion.html",
          type = "html",
          covariate.labels = c("S. No.", "Religion", "Poverty Rate"))

rural.caste <- rural %>%
  group_by(caste) %>% 
  summarise(poverty_rate.caste = weighted.mean(line,wt_int)) %>% 
  na.omit()

rural.caste$caste <- factor(rural.caste$caste,
                            levels = c("0","1", "2"),
                            labels = c("Others", "ST", "SC"))

stargazer(as.data.frame(rural.caste), 
          summary = FALSE,
          title = "Poverty Rate for different castes in rural India (2011)",
          out = "povertyrate.rural.caste.html",
          type = "html",
          covariate.labels = c("S. No.", "Caste", "Poverty Rate"))