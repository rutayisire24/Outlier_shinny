cat("\014")     # Clear  Console
rm(list=ls())   # clear all variables 
graphics.off()  # clear all plots

library(tidyverse)  # load functions for data manipulation and visualization
library(imputeTS)   # imputation of missing values
library(here)
library(readxl)

#source("API.R")

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
data_p <- read_rds("impute_i.rds")

pattern <- "(.{4})(.*)$"

data_p$periodname <- gsub(pattern, "\\1/\\2", data_p$periodname)

data_f <- data_p%>%
  janitor::clean_names() %>%
  mutate(period = lubridate::ym(periodname),
         values = as.numeric(values)) %>%
  filter(period< "2023-07-01")



# %>%
#   pivot_longer(cols = 6:15,names_to = "variable",values_to = "values")%>%
#   select(-c(orgunitlevel4))

#names(data_f) <- c("periodname","region","district","facility","variable","values")

data_clean <- data_f

#data_clean$period <- lubridate::my(data_clean$periodname)

## orgunit 
org_unit <- data_p %>%
  group_by(region,district,facility) %>%
  count()


flagger <- function(facility_i, variable_i){
  
  #facility_i <- "Kitwe Health Centre IV"
  #variable_i <- sample(data_clean$variable,1)
  
  
  data_interest <- data_clean %>%
    filter( facility == facility_i & variable == variable_i ) %>%
    group_by(facility,period,variable) %>%
    summarise(variable = sum(values,na.rm = T)) 
  
  l <- arrange(data_interest,period)
  l <- myRemover(l,"variable")
  
  l <- l %>%
    mutate(flag = as.character(ifelse(variable==variable_clean,"None","Outlier")))
  
  return(l)
  
}

flagger("Life Star Clinic",sample(data_clean$variable,1))
#---------

## Sample districts 

districts_sample <- sample(unique(data_clean$district),20,replace = FALSE)

sample_df <- data_clean %>%
  filter(district %in% districts_sample)

length(unique(sample_df$facility))

facilities <- unique(sample_df$facility)
elements <- sample(unique(data_clean$variable),9)


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
write_csv(data_df_c,"impute.csv")

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




