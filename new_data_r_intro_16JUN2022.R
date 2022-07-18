# CT max data ------------------------------------------------------------------
# Thomas O'Leary

# Load library
library(tidyverse)

# Load data
df <- read_delim("Populations_CTmax.csv", delim = ";")

# Get averages and standard deviations
df %>%
  group_by(Population, Treatment) %>%
  summarise(ct_max = mean(CTmax),
            sd = sd(CTmax))

# Make plots ----

# Make a box plot
df %>%
  ggplot() +
  geom_boxplot(aes(x = Treatment,
                   y = CTmax)) +
  theme_classic()

# Color by population
df %>%
  ggplot() +
  geom_boxplot(aes(x = Treatment,
                   y = CTmax,
                   color = Population)) +
  theme_classic()


# Run analysis
mod <- aov(CTmax ~ Population*Treatment,
           data = df)

broom::tidy(mod)

