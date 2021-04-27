library(tidyverse)
library(emmeans)

data(mtcars)

lm.model <- lm(data = mtcars, mpg ~ cyl * wt)

summary(lm.modil)
emmeans(lm.model, ~ cyl | wt)
