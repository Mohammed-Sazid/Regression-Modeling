# Install and load required packages
install.packages("tidyverse")
install.packages("mdsr")
install.packages("mosaic")

library(tidyverse)
library(mdsr)
library(mosaic)

### Compare our model to a null model
mod_avg <- RailTrail %>%
  lm(volume ~ 1, data = .) %>%
  augment(RailTrail)
mod_temp <- RailTrail %>%
  lm(volume ~ hightemp, data = .) %>%
  augment(RailTrail)
mod_data <- bind_rows(mod_avg, mod_temp) %>%
  mutate(model = rep(c("null", "slr"), each = nrow(RailTrail)))
print(mod_data,n=10)

# This comparison helps determine if our model is more useful than a 
# null model (intercept-only).
# It provides a baseline for assessing the improvement offered by 
# including hightemp as a predictor.



# This achieves the same result as the piped version above, 
# demonstrating different R coding styles.

# Visualize null and simple linear regression models
ggplot(data = mod_data, aes(x = hightemp, y = volume)) + 
  geom_smooth(
    data = filter(mod_data, model == "null"), 
    method = "lm", se = FALSE, formula = y ~ 1, 
    color = "dodgerblue", size = 0.5
  ) + 
  geom_smooth(
    data = filter(mod_data, model == "slr"),
    method = "lm", se = FALSE, formula = y ~ x, 
    color = "dodgerblue", size = 0.5
  ) + 
  geom_segment(
    aes(xend = hightemp, yend = .fitted), 
    arrow = arrow(length = unit(0.1, "cm")), 
    size = 0.5, color = "darkgray"
  ) + 
  geom_point(color = "dodgerblue") + 
  facet_wrap(~model)

# This plot visually compares the null model and simple linear regression model,
# helping to illustrate the improvement in fit when including hightemp as a predictor.

# Calculate R-squared manually
n <- nrow(RailTrail)
SST <- var(pull(RailTrail, volume)) * (n - 1)
SSE <- var(residuals(mod)) * (n - 1)
1 - SSE / SST

# This manual calculation of R-squared provides a measure of how much 
# variance in volume is explained by the model.