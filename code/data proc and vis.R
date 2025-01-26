library(openxlsx)
library(dplyr)
library(ggplot2)

# data intake
data <- read.xlsx(xlsxFile = "data/Raw measurements.xlsx", sheet = 1, skipEmptyRows = FALSE)

# remove one data point that has a missing data
Lang <- data %>% filter(Locality == "Langebaanweg" & !is.na(MD))

# Find the convex hull of the Langebaanweg points 
# function in dplyr
# grouped by element
hull_tooth <- Lang %>%
  group_by(factor(Element, levels = c("UP2", "UM1", "UM2"))) %>%
  slice(chull(BL, MD))

# plotting grid by element for better visuals
# shape and size by locality 
ggplot(data, aes(BL, MD)) + 
  aes(fill = Element) + 
  geom_point(aes(shape = Locality, size = Locality)) +
  geom_polygon(data = hull_tooth, alpha = 0.5) +
  facet_grid(~ factor(Element, levels = c("UP2", "UM1", "UM2"))) +
  theme(legend.position = "bottom") +
  labs( x = "BL (mm)", y = "MD (mm)")

