library(scales)
library(tidyverse)

sections_colors <- read_csv("sections_colors.csv")

original <- sections_colors$section_color
original <- original[!is.na(original)]

colors <- crossing(col1 = original, col2 = original) %>% 
  rowwise() %>% 
  mutate(colf = colorRampPalette(c(col1, col2))(10)[5]) %>% 
  distinct(colf) %>% 
  pull()

show_col(colors)

dfcolors <- as_tibble(t(col2rgb(colors)))

rgb2hex <- function(r,g,b) rgb(r, g, b, maxColorValue = 255)

colors_n <- kmeans(dfcolors, 4)$centers %>% 
  as_tibble() %>% 
  mutate(across(everything(), as.integer)) %>% 
  mutate(color = rgb2hex(red, green, blue)) %>% 
  arrange(red, green, blue) %>% 
  pull(color)

show_col(colors_n)

sections_colors$section_color[is.na(sections_colors$section_color)] <- colors_n 

readr::write_csv(sections_colors, "sections_colors.csv")
