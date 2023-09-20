rm(list=ls())
library(googlesheets4)
library(tidyverse)

getwd()
setwd("/Users/IDECOR/Documents/Code/Eco-Pol/")

datos <- read_sheet('https://docs.google.com/spreadsheets/d/1N_tbyCh6_1AdAFlEhW5HSpMJ_048gIiBM8-ffJix50U/edit?usp=sharing')

head(datos)
names(datos)

##########################################


df = datos[,c("voto", "voto_paso")]
df$id = 1:nrow(datos)
df = gather(df, voto, paso, voto:voto_paso, factor_key=TRUE)


df %>%
  ggplot(aes(x = voto, fill = paso)) +
  geom_bar(stat = "count") +
  scale_fill_viridis_d()

library(ggalluvial)
df %>%
  ggplot(aes(x = voto,
             stratum = paso,
             alluvium = id,
             fill = paso)) +
  geom_stratum(alpha = 0.5) +
  geom_flow() +
  # scale_fill_viridis_d() +
  scale_fill_manual(values=c("white", "red", "blue",
                                    "yellow", "violet", "grey","skyblue")) +
                                      theme_minimal() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(angle = 0, hjust = 0)) +
  geom_segment(aes( # adds an arrow to indicate that time is moving from right to left
    x = 0.75, xend = 2.16,
    y = 0, yend = 0),
    arrow = arrow(length=unit(0.30,"cm"), 
                  ends="first", 
                  type = "closed"))