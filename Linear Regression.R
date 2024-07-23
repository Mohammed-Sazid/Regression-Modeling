# Install and load required packages
install.packages("tidyverse")
install.packages("mdsr")
install.packages("mosaic")

library(tidyverse)
library(mdsr)
library(mosaic)

# Examine the structure of the RailTrail dataset
glimpse(RailTrail)

# if one wants to get data in csv file this will help with loaction of file
# save the dataset as a CSV file and get working directory
write_csv(RailTrail, "RailTrail.csv")
getwd()
dim(RailTrail)


### Regressing volume on hightemp 
mod <- lm(volume ~ hightemp, data = RailTrail)
library(broom)
tidy(mod)
mod2 <- lm(volume ~ poly(hightemp, 2), data = RailTrail)
tidy(mod2)
glance(mod)
glance(mod2)
# These models examine the relationship between temperature and trail volume.
# mod: Linear model
# mod2: Polynomial model (quadratic)
# Results will show if there's a linear or non-linear relationship between
# temperature and trail usage.



# Visualize linear and quadratic models
ggplot(RailTrail, aes(x = hightemp, y = volume)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 2))

