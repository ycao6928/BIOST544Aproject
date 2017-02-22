options(digits = 3) ## Formats output to 3 digits
library(ggplot2)
library(dplyr)
library(data.table)



#
# CREATION OF DATAFRAME AND PREPROCESSING
#

dir <- getwd()
setwd(dir)
all.df <- read.csv("./data/prp_tendon_cleaned2_reshaped_delim.csv")

## Subset for the variables I know
cols <- colnames(all.df)
keepcols <- c(1,2,3,4,5,6,28,29,30,31,33,34,36,37,38,40,41,42,43,44,45,46,49,50,52,53,55,56,57,58)
subset.df <- all.df[, keepcols]

## Missingness by feature, ie how many N/As per column
apply(subset.df, 2, function(x){sum(is.na(x))}) 
complete <- subset.df %>% complete.cases()
mean(complete)  ## proportion of cases complete
length(unique(subset.df$patientid))  ## total unique patients

## subset for metadata features
meta.df <- subset.df %>% select(patientid, treatment, age, race, gender, duration_months)  # subset for features we'll be using 
apply(meta.df, 2, function(x){sum(is.na(x))}) 
complete <- meta.df %>% complete.cases()


## Summary statistics
ages <- subset.df %>% 
    group_by(as.factor(gender)) %>% 
    summarise(mean(duration_months))

hist(subset.df$age)
hist(subset.df$duration_months)


