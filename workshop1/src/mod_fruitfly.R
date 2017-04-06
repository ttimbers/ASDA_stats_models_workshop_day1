## Transformation of fruitfly data from faraway R package to two suitable datasets for ANOVA workshop
# https://cran.r-project.org/web/packages/faraway/faraway.pdf

# load libraries
library(tidyverse)
library(faraway)

# make dataframe with only longevity and activity columns, and in activity column, 
# only have isolated and high group
fruitfly_2_groups <- fruitfly %>% 
  select(longevity, activity) %>% 
  filter(activity == "isolated" | activity == "high")

fruitfly_2_groups$activity <- as.character(fruitfly_2_groups$activity)
fruitfly_2_groups$activity[fruitfly_2_groups$activity == "isolated"] <- "No"
fruitfly_2_groups$activity[fruitfly_2_groups$activity == "high"] <- "Yes"

colnames(fruitfly_2_groups) <- c("longevity", "sexually_active")

write_csv(fruitfly_2_groups, "data/fruitfly_2_groups.csv")


# make data frame with only longevity and activity columns, and groups 
# isolated, low and high
fruitfly_3_groups <- fruitfly %>% 
  select(longevity, activity) %>% 
  filter(activity == "isolated" | activity == "high" | activity == "low")

fruitfly_3_groups$activity <- as.character(fruitfly_3_groups$activity)
fruitfly_3_groups$activity[fruitfly_3_groups$activity == "isolated"] <- "none"

write_csv(fruitfly_3_groups, "data/fruitfly_3_groups.csv")

# make a data frame with only two groups (isolated and high group) and where thorax 
# length is is short or long (where long > 0.8) and therefore categorical
fruitfly_thorax_len <- fruitfly %>% 
  filter(activity == "isolated" | activity == "high")

fruitfly_thorax_len$activity <- as.character(fruitfly_thorax_len$activity)
fruitfly_thorax_len$activity[fruitfly_thorax_len$activity == "isolated"] <- "No"
fruitfly_thorax_len$activity[fruitfly_thorax_len$activity == "high"] <- "Yes"
colnames(fruitfly_thorax_len) <- c("thorax", "longevity", "sexually_active")

fruitfly_thorax_len$thorax[fruitfly_thorax_len$thorax > 0.8] <- "long"
fruitfly_thorax_len$thorax[fruitfly_thorax_len$thorax <= 0.8] <- "short"

write_csv(fruitfly_thorax_len, "data/fruitfly_thorax_len.csv")
