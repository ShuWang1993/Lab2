library(tidyverse)
library(ggplot2)

### Section 1: Recreate the graph from Problem 4 in Activity 2. ###

data_1 <- read.csv('~/Desktop/Fall2018_Biostatistics_Lab/20180919_Lab2/lab2.csv')

## Tidy the data.
data_2 <- data_1 %>%
  gather('base_sport', 'base_pain', 'base_qol', 'first_sport', 'first_pain', 'first_qol', 
         'second_sport', 'second_pain', 'second_qol',
         key="time_category", value="score") %>%
  separate(time_category, into=c("time", "category")) %>%
  mutate(time=fct_recode(factor(time), 
                         "baseline"="base", "first year"="first", "second year"="second")) 

## Summarize by time and category (mean and standard deviation)
data_2 %>%
  group_by(time, category) %>%
  summarise(mean=mean(score), sd=sd(score)) %>%
  ggplot(aes(x=time, y=mean, group=category, color=category)) +
  geom_point() +
  geom_line() +
  ylim(0,100) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width = 0.125) +
  labs(x=c("Time of report"), y="Sample mean and standard deviation")


### Section 2: Tidy two another data files. ###
## Load two files.
data_cov <- read.csv('~/Desktop/Fall2018_Biostatistics_Lab/homework/homework02/coverage.csv', skip=2)
data_exp <- read.csv('~/Desktop/Fall2018_Biostatistics_Lab/homework/homework02/expenditures.csv', skip=2)
data_cov <- slice(data_cov, 1:52)
data_exp <- slice(data_exp, 1:52)

## Make two data files tidy. 
data_cov_tidy <- data_cov %>%
  gather('X2013__Employer', 'X2013__Non.Group', 'X2013__Medicaid', 'X2013__Medicare', 
         'X2013__Other.Public', 'X2013__Uninsured', 'X2013__Total', 'X2014__Employer', 'X2014__Non.Group',
         'X2014__Medicaid', 'X2014__Medicare', 'X2014__Other.Public', 'X2014__Uninsured',
         'X2014__Total', 'X2015__Employer', 'X2015__Non.Group', 'X2015__Medicaid', 'X2015__Medicare',
         'X2015__Other.Public', 'X2015__Uninsured', 'X2015__Total', 'X2016__Employer', 'X2016__Non.Group',
         'X2016__Medicaid', 'X2016__Medicare', 'X2016__Other.Public', 'X2016__Uninsured', 'X2016__Total',
         key="year_category", value="score") %>%
  separate(year_category, into=c("year", "category")) %>%
  mutate(category=fct_recode(factor(category), "Non Group"="Non", "Oteher Public"="Other"),
         year=fct_recode(factor(year), "2013"="X2013", "2014"="X2014", "2015"="X2015", "2016"="X2016"))

data_exp_tidy <- data_exp %>%
  gather('X1991__Total.Health.Spending', 'X1992__Total.Health.Spending', 'X1993__Total.Health.Spending',
         'X1994__Total.Health.Spending', 'X1995__Total.Health.Spending', 'X1996__Total.Health.Spending',
         'X1997__Total.Health.Spending', 'X1998__Total.Health.Spending', 'X1999__Total.Health.Spending',
         'X2000__Total.Health.Spending', 'X2001__Total.Health.Spending', 'X2002__Total.Health.Spending',
         'X2003__Total.Health.Spending', 'X2004__Total.Health.Spending', 'X2005__Total.Health.Spending',
         'X2006__Total.Health.Spending', 'X2007__Total.Health.Spending', 'X2008__Total.Health.Spending',
         'X2009__Total.Health.Spending', 'X2010__Total.Health.Spending', 'X2011__Total.Health.Spending',
         'X2012__Total.Health.Spending', 'X2013__Total.Health.Spending', 'X2014__Total.Health.Spending',
         key="year_category", value="score") %>%
  separate(year_category, into=c("year", "category")) %>%
  mutate(year=fct_recode(factor(year), "1991"="X1991", "1992"="X1992", "1993"="X1993", "1994"="X1994",
                         "1995"="X1995", "1996"="X1996", "1997"="X1997", "1998"="X1998", "1999"="X1999",
                         "2000"="X2000", "2001"="X2001", "2002"="X2002", "2003"="X2003", "2004"="X2004",
                         "2005"="X2005", "2006"="X2006", "2007"="X2007", "2008"="X2008", "2009"="X2009",
                         "2010"="X2010", "2011"="X2011", "2012"="X2012", "2013"="X2013", "2014"="X2014"),
         category=fct_recode(factor(category), "Total Health Spending" = "Total"))

##  Merge two data frames.
data_merged <- rbind(data_cov_tidy, 
                     filter(data_exp_tidy, year == "2013"), filter(data_exp_tidy, year == "2014"))
data_final <- arrange(data_merged, year)
View(data_final)
