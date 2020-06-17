rm(list = ls())


graphics.off()

library(tidyverse)

library(lavaan)

library(lavaanPlot)

# Reading the data

df <- read_csv("Reading_Header.csv")



df <- df %>% 
  mutate(across(.cols = c(starts_with('INT'), starts_with('FAM')), .fns = ~as.character(.x))) %>% 
  mutate(across(.cols = where(is.numeric), .fns = ~ifelse(.x == 999999, NA, .x))) %>% 
  mutate(across(.cols = where(is.character), .fns = ~ifelse(.x %in% '999999', NA, .x)))

glimpse(df)

summary(df)

head(df)



myModel <- readLines("model.lav")


cat(myModel)


fit <- sem(model = myModel, data = df, cluster = 'SUBJECT')

summary(fit, standardized=TRUE)


labels = list(lat_INT = "Interest", lat_FAM = "Familarity", lat_WMC = "WMC", lat_COM = "Reading", VOL = "Voluntary MW", INV = "Involuntary MW")

lavaanPlot(model = fit, stand = TRUE, 
           labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), coef = TRUE)


