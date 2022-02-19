# practice code
library(palmerpenguins)
library(tidyverse)
library(DT)

# filter for body masses
body_mass_df <- penguins %>% 
  filter(body_mass_g %in% 3000:4000)

# scatter plot
ggplot(na.omit(body_mass_df), 
       aes(x = flipper_length_mm, y = bill_length_mm, 
           color = species, shape = species)) +
  geom_point(size = 6) +
  scale_color_manual(values = c("Adelie" = "#FEA346", "Chinstrap" = "#B251F1", "Gentoo" = "#4BA4A4")) +
  scale_shape_manual(values = c("Adelie" = 19, "Chinstrap" = 17, "Gentoo" = 15)) +
  labs(x = "Flipper length (mm)", y = "Bill length (mm)", 
       color = "Penguin species", shape = "Penguin species") +
  theme_minimal() +
  theme(legend.position = c(0.85, 0.2),
        legend.background = element_rect(color = "white"))

# datatable

DT::datatable(penguins,
              options = list(pageLength = 5),
              caption = tags$caption(
                style = 'caption-side: top; text-align: left;',
                'Table 1: ', tags$em('Size measurements for adult foraging penguins near Palmer Station, Antarctica')))

# histogram

ggplot(na.omit(island_df()), aes(x = flipper_length_mm, fill = species)) +
  geom_histogram(alpha = 0.7) +
  scale_fill_manual(values = c("Adelie" = "#FEA346", "Chinstrap" = "#B251F1", "Gentoo" = "#4BA4A4")) +
  labs(x = "Flipper length (mm)", y = "Frequency", 
       fill = "Penguin species") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.background = element_rect(color = "white"))

