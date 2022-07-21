# ------------------------------------------------------------------------------
# Figure design with REU Students
# July 19, 2022
# TS O'Leary
# ------------------------------------------------------------------------------

# Load libraries
require(tidyverse)

# Load data
iris

# Analyze data ----

# Mean septal length regardless of species
mean(iris$Sepal.Length)

# Group mean for setosa septal length -- base R
mean(iris$Sepal.Length[iris$Species == "setosa"])
mean(iris$Sepal.Length[iris$Species == "versicolor"])
mean(iris$Sepal.Length[iris$Species == "virginica"])

# Tidyverse version of getting group means for stuff
iris %>%
  group_by(Species) %>%
  summarise(avg = mean(Sepal.Length))

# Plot this data
ggplot(data = iris) +
  geom_point(aes(x = Sepal.Length,
                 y = Sepal.Width))

# Include species
ggplot(data = iris) +
  geom_point(aes(x = Sepal.Length,
                 y = Sepal.Width,
                 color = Species)) +
  theme_minimal()

# Whst about including zero in the axes
ggplot(data = iris) +
  geom_point(aes(x = Sepal.Length,
                 y = Sepal.Width,
                 color = Species)) +
  lims(x = c(0, 8),
       y = c(0, 4.5)) +
  theme_minimal()
# Well maybe that isn't necessary in this context when you are just correlating
#  between variables

# But in this sorta context it maight be more important
ggplot(data = iris) +
  geom_boxplot(aes(y = Sepal.Length,
                  color = Species)) +
  ylim(c(0, 8)) +
  theme_minimal()


# What if we wanted to do shape too?
ggplot(data = iris) +
  geom_point(aes(x = Sepal.Length,
                 y = Sepal.Width,
                 color = Species,
                 shape = Species)) +
  theme_minimal()


# How 'bout changing the types of shapes?
ggplot(data = iris) +
  geom_point(aes(x = Sepal.Length,
                 y = Sepal.Width,
                 color = Species,
                 shape = Species)) +
  scale_shape_manual(values = c(21, 22, 23)) +
  theme_minimal()

# What if I wanted specific colors though?
ggplot(data = iris) +
  geom_point(aes(x = Sepal.Length,
                 y = Sepal.Width,
                 color = Species,
                 shape = Species)) +
  scale_color_manual(values = c("lavenderblush2", "azure3", "wheat2")) +
  theme_minimal()
# That looks horrible -- LOL


ggplot(data = iris) +
  geom_point(aes(x = Sepal.Length,
                 y = Sepal.Width,
                 color = Species,
                 shape = Species),
             size = 3) +
  scale_color_manual(values = c("#828489", "#9e90a2", "#b6c2d9")) +
  theme_minimal()
# Still horrible --- how sad -- but the size kinda helps


# Facet wrap time :) -------

# Minimal product
ggplot(data = iris) +
  geom_point(aes(x = Sepal.Length,
                 y = Sepal.Width)) +
  facet_wrap(~ Species)
# In this context grey is kinda a good thing

# 'Cause look at this
ggplot(data = iris) +
  geom_point(aes(x = Sepal.Length,
                 y = Sepal.Width)) +
  theme_minimal() +
  facet_wrap(~ Species)

# Challenge time -----------------
iris %>%
  mutate(Species = paste("N.", Species)) %>%
  ggplot() +
  geom_boxplot(aes(x = Species,
                   y = Sepal.Length,
                   fill = Species)) +
  scale_fill_manual(values = c("grey90", "grey90", "lavenderblush2")) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "virginica has the greatest median sepal length") +
  ylim(c(0, 8))

# Okay let's make this a final figure and then move on to your data
iris %>%
  mutate(Species = paste("N.", Species)) %>%
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_boxplot(width = 0.6,
               outlier.shape = NA) +
  geom_jitter(width = 0.1,
              shape = 21) +
  scale_fill_manual(values = c("grey90", "grey90", "lavenderblush2")) +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color = "grey90"),
        panel.grid.minor.y = element_line(color = "grey96")) +
  ylim(c(0, 8))




