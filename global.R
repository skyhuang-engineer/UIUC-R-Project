
library(shiny)
library(shinydashboard)
library(plyr)
library(dplyr)
library(ggplot2)
library(countrycode)
library(ggvis)
library(ggmap)
library(googleVis)
library(data.table)
library(xts)
library(zoo)
library(dygraphs)
library(devtools)
library(readr)


load("All_controls.Rda")
load("counts.Rda")
load("Standard_countries.Rda")
load("Timed_set.Rda")
load("IMS_GMS.Rda")
load("Summary.Rda")
load("Summary1.Rda")
load("region_counts.Rda")


All_controls[is.na(All_controls)] <- ""
Standard_countries <- unique(All_controls[c("Country")])
Standard_countries <- plyr::arrange(Standard_countries, Country)

FIDE_dataset <- read.csv("FIDE_dataset.csv")
Other_titles <- FIDE_dataset %>% filter(OTit != "" )

df <- as.data.frame(Other_titles$OTit)

names(df) <- "Column_of_strings"

library(tidyr)
library(reshape2)
df2 <- separate(df, Column_of_strings, c('str1', 'str2', 'str3'), sep = ',', fill = 'right')
df2 <- cbind(id = seq_along(df2$str1), df2)
df3 <- melt(df2, id = 'id', na.rm = TRUE)

Other_titles$id <- df2$id
Other_titles$str1 <- df2$str1[match(Other_titles$id, df2$id)]
Other_titles$str2 <- df2$str2[match(Other_titles$id, df2$id)]
Other_titles$str3 <- df2$str3[match(Other_titles$id, df2$id)]

Other_titles$str1[Other_titles$str1 == "IO"] <- "International Organizer"
Other_titles$str2[Other_titles$str2 == "IO"] <- "International Organizer"
Other_titles$str3[Other_titles$str3 == "IO"] <- "International Organizer"

Other_titles$str1[Other_titles$str1 == "FI"] <- "FIDE trainer"
Other_titles$str2[Other_titles$str2 == "FI"] <- "FIDE trainer"
Other_titles$str3[Other_titles$str3 == "FI"] <- "FIDE trainer"
Other_titles$str1[Other_titles$str1 == "FT"] <- "FIDE trainer"
Other_titles$str2[Other_titles$str2 == "FT"] <- "FIDE trainer"
Other_titles$str3[Other_titles$str3 == "FT"] <- "FIDE trainer"

Other_titles$str1[Other_titles$str1 == "FST"] <- "FIDE senior trainer"
Other_titles$str2[Other_titles$str2 == "FST"] <- "FIDE Senior trainer"
Other_titles$str3[Other_titles$str3 == "FST"] <- "FIDE Senior trainer"

Other_titles$str1[Other_titles$str1 == "NI"] <- "National Instructor"
Other_titles$str2[Other_titles$str2 == "NI"] <- "National Instructor"
Other_titles$str3[Other_titles$str3 == "NI"] <- "National Instructor"

Other_titles$str1[Other_titles$str1 == "FA"] <- "FIDE Arbiter"
Other_titles$str2[Other_titles$str2 == "FA"] <- "FIDE Arbiter"
Other_titles$str3[Other_titles$str3 == "FA"] <- "FIDE Arbiter"

Other_titles$str1[Other_titles$str1 == "DI"] <- "Developmental Instructor"
Other_titles$str2[Other_titles$str2 == "DI"] <- "Developmental Instructor"
Other_titles$str3[Other_titles$str3 == "DI"] <- "Developmental Instructor"

Other_titles$str1[Other_titles$str1 == "IA"] <- "International Arbiter"
Other_titles$str2[Other_titles$str2 == "IA"] <- "International Arbiter"
Other_titles$str3[Other_titles$str3 == "IA"] <- "International Arbiter"

# ggplot(Other_titles, aes(c(str1))) + geom_bar(aes(fill = str1)) +  xlab('Name of "other" title') + ylab("Number of players")

