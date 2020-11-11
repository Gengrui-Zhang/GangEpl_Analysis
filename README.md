# GangEpl_Analysis
This is a repository to do the data analysis on the Gang and Employment Study

---
title: "GangEpl_Analysis_1104"
author: "Jimmy Zhang"
date: "11/4/2020"
output: html_document
---

```{r load packages}
library(psych)  # for scatterplot matrix
library(here)  # makes reading data more consistent
library(tidyverse)  # for data manipulation and plotting
library(car)  # some useful functions for regression
library(modelsummary)  # for making tables
library(sjPlot)  # for plotting slopes
library(interactions)  # for plotting interactions
library(haven)  # for importing SPSS/SAS/Stata data
library(lme4)  # for multilevel analysis
library(lattice)  # for dotplot (working with lme4)
library(MuMIn)  # for computing r-squared
library(broom.mixed)  # for summarizing results
install.packages("funModeling")
install.packages("Hmisc")
library(funModeling)
library(Hmisc)
library(ggplot2)
library(dplyr)
install.packages("hrbrthemes")
library(hrbrthemes)
theme_set(theme_bw())  # Theme; just my personal preference

```

```{r import data}
GangEpl <- read_sav(here("Fall 2020/MMM Lab/Gang and Employment", "Weekly Data Final Version Missing Data Addressed-12-23-09.sav"))
GangEpl
```

```{r histograms of selected variables}
hist(GangEpl[5:6]) #Histograms of EmployHrs and WmployWg
hist(GangEpl[7:12]) #Histograms of Del1a to Del1f
hist(GangEpl[13:18]) #Histograms of Del2a to Del2f
hist(GangEpl[19]) #Histograms of Del3
hist(GangEpl[20]) #Histograms of Del4
hist(GangEpl[21:26]) #Histograms of Del5a to Del5f
hist(GangEpl[27:30]) #Histograms of GangInv1 to GangInv4
```

```{r scatter plots matrix}
pairs.panels(GangEpl[5:6],  # not plotting the first column
             ellipses = FALSE)
pairs.panels(GangEpl[7:12],  # not plotting the first column
             ellipses = FALSE)
pairs.panels(GangEpl[13:18],  # not plotting the first column
             ellipses = FALSE)
pairs.panels(GangEpl[21:26],  # not plotting the first column
             ellipses = FALSE)
pairs.panels(GangEpl[27:30],  # not plotting the first column
             ellipses = FALSE)
```

```{r scale scores}
average_Del1 <- GangEpl %>% 
  group_by(ID) %>% 
  summarise(average = mean(Del1a + Del1b + Del1c + Del1d + Del1e + Del1f))

average_Del2 <- GangEpl %>% 
  group_by(ID) %>% 
  summarise(average = mean(Del2a + Del2b + Del2c + Del2d + Del2e + Del2f))

average_Del5 <- GangEpl %>% 
  group_by(ID) %>% 
  summarise(average = mean(Del5a + Del5b + Del5c + Del5d + Del5e + Del5f))

average_GangInv <- GangEpl %>% 
  group_by(ID) %>% 
  summarise(average = mean(GangInv1 + GangInv2 + GangInv3 + GangInv4))

average_GangMem <- GangEpl %>% 
  group_by(ID) %>% 
  summarise(average = mean(GangMem1 + GangMem2 + GangMem3 + GangMem4 + GangMem5 + GangMem6 + GangMem7 + GangMem8 + GangMem9 + GangMem10 + GangMem11 + GangMem12 + GangMem13 + GangMem14 + GangMem15)) #Missing data, computing error

average_DC <- GangEpl %>% 
  group_by(ID) %>% 
  summarise(average = mean(DC1 + DC2 + DC3 + DC4 + DC5 + DC6 + DC7 + DC8 + DC9 + DC10 + DC11 + DC12))
```


```{r ran_int}
ran_int_Del1 <- lmer(Del1b ~ 1 + (1 | ID), data = GangEpl)
summary(ran_int_Del1)

ran_int_Del3 <- lmer(Del3 ~ 1 + (1 | ID), data = GangEpl)
summary(ran_int_Del3)
```

#An overview of the study design and variables in the GangEpl (Gang Employment) dataset:\

  This is a long form dataset in which each participant was measured multiple times (indicated by the variable Session). We have 337 observations and 57 variables in total.
  
  Within-subject design: Youth were randomly assigned to either BEP or usual services (US), and data were collected at each intervention session. Hypothesis: Increases in employment among BEP participants would be related to reductions in gang involvement over the course of treatment.
  
  Between-subject design: Compare BEP and US participants at the 3 month and 6 month assessment periods. Hypothesis: BEP would lead to higher employment and less gang involvement than US, and that changes in employment would mediate the effect of treatment on gang involvement.
  
  Variables:
  
  ID -> Pariticipant ID
  Age -> The age of participant at each session
  Session -> Time point at which each participant was recorded
  Date -> The date is in year-month-date format (read in haven package)
  EmployHrs -> The hours of working in the current week
  EmployWg -> Hourly wage in the current week
  Del -> Participants' report on the number of gang activities such as stealing over the previous week
  GangInv1 to GangInv4 -> Participants' report on questions about the level of Gang Involvment
  GangMem1 to GangMem15 -> Participants' report on Gang Membership Inventory (the measure of gang membership)
  DC1 to DC12 -> Dummy coded variables for each participant (In the draft paper, the author stated that to identify sources of contamination expected from pooling data from multiple participants, a least squares with dummy variables (LSDV) approach was used. However, I really think a codebook is needed to address how these variables are dummy coded).
  
Concerns: some '88' and '99' value under the column of Del2d and Del2e. Still not really sure what they meant. They are separate from NA.

#Plot the gang involvement and trajectory of each participant. Scale score
