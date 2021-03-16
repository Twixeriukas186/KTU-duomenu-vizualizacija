### 11 variantas (eco: 561000  Matas	P	2021-03-23	10:20)

library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(lubridate)

### 1

data <- read_csv("../data/lab_sodra.csv")
eco.act.code.data <- data %>% 
  filter(ecoActCode == "561000")

eco.act.code.data %>% 
  ggplot(aes(x = avgWage)) + 
  geom_histogram(bins = 90, col = 18, fill = 1)
ggsave("uzduotis_1.png", path = "../img")

### 2

sorted.by.avgWage <- eco.act.code.data %>% 
  group_by(code) %>% 
  summarise(avg = mean(avgWage)) %>% 
  arrange(desc(avg)) %>% 
  head(5)



eco.act.code.data$month <- format(parse_date(as.character(eco.act.code.data$month), format = "%Y %m"), format = "%Y %m")

eco.act.code.data %>% 
  filter(code %in% sorted.by.avgWage$code) %>% 
  ggplot(aes(x = month, y = avgWage, group = name, color = name)) + 
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Average Wage") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 30, size = 10))
ggsave("uzduotis_2.png", path = "../img")

### 3

eco.act.code.data %>% 
  filter(code %in% sorted.by.avgWage$code) %>% 
  group_by(name) %>% slice_max(numInsured, with_ties = FALSE) %>% 
  ggplot(aes(x = reorder(name, -numInsured), y = numInsured)) + 
  geom_col(aes(fill = name)) + 
  theme(axis.text.x = element_blank()) +
  xlab('name') + 
  ylab('Apdrausti')
ggsave("uzduotis_3.png", path = "../img")


