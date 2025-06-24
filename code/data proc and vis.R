library(openxlsx)
library(dplyr)
library(ggplot2)
library(viridisLite)

# data intake
data <- read.xlsx(xlsxFile = "data/Raw measurements.xlsx", sheet = 1, skipEmptyRows = FALSE)

data$Element <- ordered(data$Element, levels = c("UP2", "UdP4","UM1", "UM2"))

# remove one data point that has a missing data
Lang <- data %>% filter(Locality == "Langebaanweg" & !is.na(MD))

# Find the convex hull of the Langebaanweg points 
# function in dplyr
# grouped by element
hull_tooth <- Lang %>%
  group_by(factor(Element, levels = c("UP2", "UdP4","UM1", "UM2"))) %>%
  slice(chull(BL, MD))

# plotting grid by element for better visuals
# shape and size by locality 
ggplot(data, aes(BL, MD)) + 
  aes(fill = Element ) + 
  geom_polygon(data = hull_tooth, alpha = 0.65) +
  geom_point(aes(shape = Locality, size = Locality)) +
  facet_grid(~ factor(Element, levels = c("UP2", "UdP4","UM1", "UM2"))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs( x = "BL (mm)", y = "MD (mm)")

#### crown proportion analysis for UM1 and UM2 ####
data <- data %>% mutate(Proportion = BL/MD)

M1 <- data %>% filter(Element == "UM1" )

M2 <- data %>% filter(Element == "UM2" )

ggplot(M1, aes(Locality, Proportion)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(width = 0.2) +
  labs(title= "UM1 crown proportion", y = "BL/MD")

ggplot(M2, aes(Locality, Proportion)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(width = 0.2) +
  labs(title= "UM2 crown proportion", y = "BL/MD")



