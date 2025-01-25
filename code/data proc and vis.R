library(openxlsx)
library(dplyr)
library(ggplot2)

# data intake
data <- read.xlsx(xlsxFile = "data/Raw measurements.xlsx", sheet = 1, skipEmptyRows = FALSE)

# Find the convex hull of the points being plotted

hull_tooth <- Lang %>%
  group_by(Element) %>%
  slice(chull(BL, MD))


ggplot(data, aes(BL, MD)) + 
  aes(fill = Element) + 
  geom_point(aes(shape = Locality, size = Locality)) +
  geom_polygon(data = hull_tooth, alpha = 0.5) +
  facet_grid(~ factor(Element, levels = c("UP2", "UM1", "UM2"))) +
  theme(legend.position = "bottom") +
  labs( x = "BL (mm)", y = "MD (mm)")

