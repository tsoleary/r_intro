# CT max data
# Thomas O'Leary

# Load library
library(tidyverse)

# Load data
df <- read_delim("Populations_CTmax.csv", delim = ";")

# Get averages
df %>%
  group_by(Population, Treatment) %>%
  summarise(ct_min = mean(CTmax),
            sd = sd(CTmax))

# Make plots ----
df %>%
  ggplot() +
  geom_boxplot(aes(x = Treatment,
                   y = CTmax)) +
  theme_classic()

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

