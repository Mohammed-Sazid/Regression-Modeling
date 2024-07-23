# Install and load required packages
install.packages("tidyverse")
install.packages("mdsr")
install.packages("mosaic")

library(tidyverse)
library(mdsr)
library(mosaic)
library(broom)

# Categorical Explanatory Variable
mod_cat <- RailTrail %>% lm(volume ~ weekday,data=.) %>% coef()
coef(lm(volume ~ weekday, data = RailTrail))

ggplot(RailTrail,aes(weekday,volume)) +
  geom_point(position = position_jitter()) 

# This explores the effect of weekday on trail volume,
# providing insights into usage patterns between weekdays and weekends.

RailTrail <- RailTrail %>%
  mutate(day = ifelse(weekday == 1, "weekday", "weekend/holiday"))

### Parallel slope model
mod_hightemp <- RailTrail %>%  lm(volume ~ hightemp, data =. )
tidy(mod_hightemp)
glance(mod_hightemp)
mod_parallel <- RailTrail %>%  lm(volume ~ hightemp + weekday, data =. )
mod_intersect <- RailTrail %>%  lm(volume ~ hightemp + weekday + hightemp*weekday ,
                                   data =. )
tidy(mod_parallel)
glance(mod_parallel)
tidy(mod_intersect)
# These models examine the combined effects of temperature 
# and weekday on trail volume.
# The parallel slope model assumes the effect of temperature is consistent 
# across weekdays and weekends.

# Visualize parallel slope model
mod_parallel %>%
  augment() %>%
  ggplot(aes(x = hightemp, y = volume, color = weekday)) +
  geom_point() +
  geom_line(aes(y = .fitted)) +
  labs(color = "Is it a\nweekday?")

# This plot visualizes how the relationship between temperature and
# volume differs between weekdays and weekends.

#### Planes model (multiple regression)
mod_plane <- RailTrail %>% lm(volume ~ hightemp + precip, data = .)
tidy(mod_plane)

# This model examines the combined effects of temperature and precipitation on trail volume.

# Interaction model
mod_interact <- lm(volume ~ hightemp + weekday + hightemp * weekday, 
                   data = RailTrail)
tidy(mod_interact)
glance(mod_interact)

# This model allows the effect of temperature on volume to differ between weekdays and weekends.

# Visualize interaction model
mod_interact %>%
  augment() %>%
  ggplot(aes(x = hightemp, y = volume, color = weekday)) +
  geom_point() + 
  geom_line(aes(y = .fitted)) + 
  labs(color = "Is it a\nweekday?")

# This plot visualizes how the relationship between temperature and 
# volume differs between weekdays and weekends,
# accounting for the interaction between temperature and weekday.

# More complex interaction model
mod_interact_2 <- lm(volume ~ hightemp + weekday + hightemp * weekday + precip
                     + precip*weekday, 
                     data = RailTrail)
tidy(mod_interact_2)

# This model further explores interactions, including how precipitation might differently affect weekday vs. weekend usage.

### Modeling Non-Linear Relationship
install.packages("NHANES")
library(NHANES)

# Create plots to compare linear and non-linear fits
plot1 <- NHANES %>% sample(300) %>% filter(Gender == 'female') %>% 
  ggplot(aes(Age,Height)) +
  geom_point() +
  geom_smooth(method = lm, se = TRUE) +
  geom_smooth(method = loess, se = TRUE,color = 'green')  + 
  xlab("Age (in years)") + 
  ylab("Height (in cm)")

plot2 <- ggplot(data = RailTrail, aes(x = hightemp, y = volume)) + 
  geom_point() +
  geom_smooth(method = lm) +
  geom_smooth(method = loess, color = "green") + 
  ylab("Number of trail crossings") + 
  xlab("High temperature (F)") 

grid.arrange(plot1, plot2, ncol = 2)

# These plots compare linear and non-linear (LOESS) fits for two different datasets,
# illustrating when non-linear models might be more appropriate.