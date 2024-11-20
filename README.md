# 632HW1
632 HW1 Repo
---
title: "HW2"
output:
  html_document:
    df_print: paged
---

# Summary

We are going to produce an R package with functions with documentation, error messages, and tests (extra credit). The application is functionality to visualize and process data and estimates on family planning worldwide. Note that we already worked with the estimates in Intermediate R, HW1. Relevant code and functions from that HW are included in this Rmd in the section below, followed by the exercises for this HW. 



## What to hand in

An R notebook called hw1_exercises.Rmd and associated knitted html that shows the function calls and output as displayed in the hw1 starter Rmd/pdf with example solutions, as well as the link to your GitHub repo.


Grading: 20 points

- 15 points for exercise 1
- 5 points for exercise 2

```{r}
install.packages("testthat")
install.packages("assertthat")
install.packages("devtools")
```

```{r setup}
library(tidyverse)
library(readxl)
library(testthat)
library(assertthat)
library(devtools)
#knitr::opts_chunk$set(include = FALSE)
```

# Starter code for visualizing family planning estimates (from Intermediate R, HW1)
In HW1, we worked with table Model-based_estimates_Countries_2019.xlsx from https://www.un.org/en/development/desa/population/publications/dataset/contraception/data/Table_Model-based_estimates_Countries_2019.xlsx#Main, which contains estimates of modern contraceptive use (mCPR) among married women in the sheet "FP Indicators". In this HW, we are using the processed version of this data set stored in est.csv, with 

- Country or area = country name
- iso	= country iso code 
- Year = the reference year that the estimate refers to 
- Median = point estimate of modern use among married women, in percentage	
- L95	U95 = lower and upper bounds of 95% uncertainty intervals, respectively. 
- L80	U80 = lower and upper bounds of 80% uncertainty intervals, respectively. 

In HW1, we wrote code to produce the plot below, which shows mCPR (%) among married women against time in Afghanistan (country iso code 4), lines are median estimates. I name the plotting here plot_cp_hw1 to avoid confusion/conflicts with the function that you will store in your package. 
```{r}
est <- read_csv("est.csv")
```

```{r}
# note: just adding _hw1 to the function name here to not get confused with the function 
# that you will add to your package (which will be called plot_cp).
plot_cp_hw1 <- function(est, iso_code) {
  est2 <- est %>%
    filter(iso == iso_code)

  est2 %>%
    ggplot(aes(x = Year, y = Median)) +
    geom_line() +
    geom_smooth(
      stat = "identity",
      aes(ymax = U95, ymin = L95)
    ) +
    labs(x = "Time", y = "Modern use (%)", title = est2$`Country or area`[1])
}
```

```{r, include = T}
plot_cp_hw1(est, iso_code = 4)
```


# HW1 exercises

In HW2, we set up a package and add the plotting function, called `plot_cp`. We will add observed mCPR data (from surveys) and some other functionality to the visualization.


## Exercise 1: extend function plot_cp() to also plot observed data


The data set csv file contraceptive_use.csv contains observed mCPR values. Relevant variable info is as follows:

- contraceptive_use_modern = observed mCPR 
- division_numeric_code = country iso code
- is_in_union == "Y" refers to observations among married women. 
- (start_date + end_date)/2 is the reference time for the observation. 

Extend the plotting function to produce the plots below, with additional arguments `CI`, which indicates what uncertainty interval to plot, or none uncertainty intervals shown when `CI` is set to be missing. 

The first plot shows mCPR (%) among married women against time in Afghanistan (country iso code 4) as before but with observed data added from contraceptive_use.csv. Dots are observed data, lines are point estimates and the shaded areas represent 95% uncertainty intervals. 

The second plot shows mCPR (%) among married women against time in Afghanistan (country iso code 4) without uncertainty intervals. 

The third plot shows mCPR (%) among married women against time in Kenya (country iso code 404) with the shaded areas representing 80% uncertainty intervals. 

Note that in the example solution, `dat` refers to the processed data set with variables iso, year (referring to reference year), and cp (contraceptive_use_modern * 100). 

```{r}
dat <- read_csv("contraceptive_use.csv")
dat <- dat %>%
  mutate(avg_year = ((start_date + end_date) / 2)) %>%
  rename(mCPR = "contraceptive_use_modern") %>%
  filter(is_in_union == "Y")

plot_cp <- function(dat, est, iso_code, CI = 95) {
  est <- est %>%
    filter(iso == iso_code)
  dat <- dat %>%
    rename(iso = "division_numeric_code") %>%
    filter(iso == iso_code)
  if (is.na(CI)) {
    est %>%
      ggplot() +
      geom_line(aes(x = Year, y = Median)) +
      geom_point(data = dat, aes(x = avg_year, y = mCPR * 100)) +
      labs(x = "Time",
           y = "Modern use (%)",
           title = est$`Country or area`[1])
  } else
    if (CI == 80) {
     est %>%
        ggplot() +
        geom_line(aes(Year, Median)) +
        geom_smooth(aes(Year, Median, ymax = U80, ymin = L80), stat = "identity") +
                      geom_point(data = dat, aes(x = avg_year, y = mCPR * 100)) +
        labs(x = "Time",
             y = "Modern use (%)",
             title = est$`Country or area`[1])
    }
    else if (CI == 95) {
      est %>%
        ggplot() +
        geom_line(aes(Year, Median)) +
        geom_smooth(aes(Year, Median, ymax = U95, ymin = L95), stat = "identity") +
                      geom_point(data = dat, aes(x = avg_year, y = mCPR * 100)) +
        labs(x = "Time",
             y = "Modern use (%)",
             title = est$`Country or area`[1])
    }
  }
      
```
 

```{r, include = T, warning=F}
plot_cp(dat, est, iso_code = 4)
plot_cp(dat, est, iso_code = 4, CI = NA)
plot_cp(dat, est, iso_code = 404, CI = 80)
```

## Exercise 2: GitHub

Create a repo on your GitHub. Commit and push your Rmd and knitted html to your repo. Paste the link to your GitHub repo below.


