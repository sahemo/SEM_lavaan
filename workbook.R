rm(list = ls())


graphics.off()

library(tidyverse)

library(lavaan)

library(lavaanPlot)

library(psych)

library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# Reading the data

df <- read_csv("Reading_Header.csv", na = '999999')



df <- df %>% 
  mutate(across(.cols = c(starts_with('INT'), starts_with('FAM')), .fns = ~as.character(.x)))
  # mutate(across(.cols = where(is.numeric), .fns = ~ifelse(.x == 999999, NA, .x))) %>% 
  # mutate(across(.cols = where(is.character), .fns = ~ifelse(.x %in% '999999', NA, .x)))

glimpse(df)

summary(df)

head(df)



myModel <- readLines("model.lav")


cat(myModel, fill = TRUE)


fit <- sem(model = myModel, data = df, cluster = 'SUBJECT', ordered = paste0(rep(c("INT", "FAM"), each = 3), 1:3))

summary(fit, standardized=TRUE)


labels = list(lat_INT = "Interest", lat_FAM = "Familarity", lat_WMC = "WMC", lat_COM = "Reading", VOL = "Voluntary MW", INV = "Involuntary MW")


grph <- lavaanPlot(model = fit, stand = TRUE, 
                   labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), 
                   edge_options = list(color = "grey"), coef = TRUE)


tmp<-capture.output(rsvg_png(charToRaw(export_svg(grph)),'SEM.png'))

cat('![Structural equation model with estimated standardized coefficients.](stnds.qa.png){#fig:SEM}\n\n')


corr.lv.model <- '

# latent variable definitions 

lat_COM =~ COM1 + COM2 + COM3 
lat_INT =~ INT1 + INT2 + INT3
lat_FAM =~ FAM1 + FAM2 + FAM3
lat_WMC =~ OSPAN + SSPAN

VOL ~~ INV
lat_INT ~~ lat_WMC
lat_INT ~~ lat_FAM
lat_INT ~~ lat_COM
lat_COM ~~ lat_FAM
lat_COM ~~ lat_WMC
lat_FAM ~~ lat_WMC



'




fit.lv.model <- cfa(model = corr.lv.model, data = df, cluster = 'SUBJECT', ordered = paste0(rep(c("INT", "FAM"), each = 3), 1:3))

summary(fit.lv.model, standardized=TRUE)




alpha(df %>% select(starts_with("COM")))
alpha(df %>% select(starts_with("INT")) %>% mutate_all(as.numeric))
alpha(df %>% select(starts_with("FAM")) %>% mutate_all(as.numeric))
alpha(df %>% select(ends_with("PAN")))





df %>% select(-SUBJECT, -TEXT) %>% lowerCor(digits = 2)


describe(df %>% select(-SUBJECT, -TEXT)) %>% rownames_to_column() %>% kable(digits = 2)


