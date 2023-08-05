cat("\014")     # Clear  Console
rm(list=ls())   # clear all variables 
graphics.off()  # clear all plots

library(tidyverse)  # load functions for data manipulation and visualization
library(imputeTS)   # imputation of missing values
library(here)
library(readxl)

myRemover   <- function(df, fld){
  
  with_dirt <- df[[fld]] # extract vector to be cleaned
  outliers  <- boxplot.stats(with_dirt)$out # use box plots to identify outliers
  with_na   <- ifelse(with_dirt %in% outliers, NA, with_dirt) # replace outliers with NA
  cleanVals <- na_interpolation(with_na) # replace NA with imputed values
  df[[paste(fld,'clean',sep = '_')]] <- cleanVals # assign the vector with clean data back to dataframe
  return(df) # return results back to the function call
  
}

## monthly clean

data <- read_excel("data_facility.xlsx")

data <- data %>%
  janitor::clean_names() %>%
  filter(periodname != "July 2023") %>%
  pivot_longer(cols = 6:15,names_to = "variable",values_to = "values")%>%
  select(-c(orgunitlevel4))

names(data) <- c("periodname","region","district","facility","variable","values")

data_clean <- data

data_clean$period <- lubridate::my(data_clean$periodname)

## orgunit 
org_unit <- data %>%
  group_by(region,district,facility) %>%
  count()


flagger <- function(facility_i, variable_i){
  
  #facility_i <- "Buyengo Health Centre II"
  #variable_i <- "x105_ep01b_malaria_total"
  
  
  data_interest <- data_clean %>%
    filter(variable == variable_i & facility == facility_i) %>%
    group_by(facility,period,variable) %>%
    summarise(variable = sum(values,na.rm = T)) 
  
  l <- arrange(data_interest,period)
  l <- myRemover(l,"variable")
  
  l <- l %>%
    mutate(flag = ifelse(variable==variable_clean,"None","Outlier"))
  
  return(l)
  
}

flagger("Agule Prisons Health Centre II","x105_dt01_deaths_in_opd")
#---------

facilities <- unique(data_clean$facility)
elements <- unique(data_clean$variable)


i <- 1

for (fac in facilities){
  for (element  in elements) {
    
    x <- flagger(fac,element)
    x$indicator <- element
    
    print(i)
    
    if(i == 1){
      data_df <- x
    }else{
      data_df <- rbind(data_df,x)
    }
    i <- i+1
  }
  
}

data_df_c <- data_df %>%
  left_join(org_unit) %>%
  select(-n)


saveRDS(data_df_c,"impute.rds")

## summary 

data_summary <- data_df %>%
  mutate(outlier = is.na(flag)) %>%
  group_by(district,outlier)  %>%
  count() %>%
  filter(outlier == FALSE)


write_csv(data_df,("data_df.csv"))

##---------------  testing 
plotter <- function(district, variable){
  
  district <- "Abim District"
  variable <- "x105_an01a_anc_1st_visit_for_women"
  
  data_interest <- data_monthly %>%
    group_by(orgunitlevel3,period) %>%
    summarise(variable = sum(get(variable),na.rm = T)) %>%
    filter(orgunitlevel3 == district)
  
  l <- arrange(data_interest,period)
  l <- myRemover(l,"variable")
  
  l <- l %>%
    mutate(flag = ifelse(variable==variable_clean,NA,variable_clean))
  
  if (is.na(sum(l$flag))){
    l %>%
      ggplot(aes(x=period)) +
      geom_step(aes(y=variable))+
      labs(title="There is no outlier")
  } else {
    l %>%
      ggplot(aes(x=period)) +
      geom_step(aes(y=variable) ) +
      geom_point(aes(y=flag), color="red" ) +
      facet_wrap( vars(orgunitlevel3) )+
      labs(title = "There is an outlier in red")
    
  }
  
}

plotter("Amuru District","x105_an02_anc_4th_visit_for_women")




