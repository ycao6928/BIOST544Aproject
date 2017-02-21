options(digits = 3) ## Formats output to 3 digits
library(ggplot2)
library(dplyr)
library(data.table)



#
# CREATION OF DATAFRAME AND PREPROCESSING
#

dir <- getwd()
setwd(dir)
all.df <- read.csv("./data/prp_tendon_cleaned_reshaped_delim.csv")

## Missingness by feature, ie how many N/As per column
apply(all.df, 2, function(x){sum(is.na(x))}) 
complete <- all.df %>% complete.cases()
mean(complete)  ## proportion of cases complete
length(unique(all.df$patientid))  ## total unique patients

## subset for metadata features
meta.df <- all.df %>% select(patientid, treatment, age, race, gender, duration_months)  # subset for features we'll be using 
apply(meta.df, 2, function(x){sum(is.na(x))}) 
complete <- meta.df %>% complete.cases()


## Summary statistics
ages <- all.df %>% 
    group_by(as.factor(gender)) %>% 
    summarise(mean(duration_months))

hist(all.df$age)
hist(all.df$duration_months)


