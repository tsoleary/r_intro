# Simple dplyr verbs

starwars

glimpse(starwars)

starwars %>%
  select(name, height, mass)

starwars %>%
  filter(height < 100)

starwars %>%
  filter(eye_color == "red")

starwars %>%
  arrange(height)

starwars %>%
  arrange(desc(height))

starwars %>%
  group_by(hair_color) %>%
  summarize(mass = mean(mass))

starwars %>%
  group_by(hair_color) %>%
  summarize(mass = mean(mass, na.rm = TRUE))

starwars %>%
  group_by(hair_color) %>%
  tally()

# ggplot stuff
mpg %>%
  ggplot() +
  geom_histogram(aes(x = hwy), bins = 20)

mpg %>%
  ggplot() +
  geom_histogram(aes(x = hwy),
                 bins = 20,
                 color = "grey50",
                 fill = "grey70")

mpg %>%
  ggplot() +
  geom_density(aes(x = hwy))

mpg %>%
  ggplot() +
  geom_density2d_filled(aes(x = hwy,
                     y = cty))

mpg %>%
  ggplot(aes(x = hwy, y = cty)) +
  geom_point()

mpg %>%
  ggplot(aes(x = displ,
             y = hwy)) +
  geom_point()

mpg %>%
  ggplot() +
  geom_point(aes(x = hwy,
                 y = cty))

mpg %>%
  ggplot(aes(x = as.factor(cyl),
             y = hwy)) +
  geom_boxplot()

mpg %>%
  group_by(cyl) %>%
  tally()

mpg %>%
  ggplot(aes(x = as.factor(cyl),
             y = hwy)) +
  geom_violin()

mpg %>%
  ggplot() +
  geom_jitter(aes(x = drv, y = hwy),
              width = 0.2,
              shape = 21)

mpg %>%
  ggplot() +
  geom_jitter(aes(x = manufacturer, y = hwy),
              width = 0.2,
              shape = 21)

# Statistics on mpg data frame ---------
# one-way anova
mod <- aov(hwy ~ cyl,
           mpg %>%
             filter(cyl != 5))

summary(mod)

tidy(mod)$p.value[1]

starwars %>%
  ggplot() +
  geom_histogram(aes(x = height),
                 bins = 20)

starwars %>%
  filter(mass < 1000) %>%
  ggplot(aes(x = height,
             y = mass)) +
  geom_point() +
  geom_smooth(method = "lm")

starwars %>%
  filter(mass < 1000) %>%
  ggplot(aes(x = height,
             y = mass)) +
  geom_point(shape = 21, color = "grey50") +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "grey50",
              linetype = 2)

mod <- lm(mass ~ height, data = starwars %>% filter(mass < 1000))
tidy(mod)


# Final day of Part I ----------------------------------------------------------

# A few more plots together

ggplot(data = starwars) +
  geom_point(aes(x = height,
                 y = mass))



# Statistics and experimental design -------------------------------------------

# Hypothesis tests

# t-test
sleep %>%
  mutate(group = as.factor(group)) %>%
  ggplot(aes(x = group, y = extra)) +
  geom_boxplot()

mod <- t.test(extra ~ group, data = sleep %>%
              mutate(group = as.factor(group)))
tidy(mod)



# one-way anova ----
PlantGrowth %>%
  ggplot(aes(x = group,
             y = weight)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.1,
              shape = 21) +
  theme_minimal()

mod <- aov(weight ~ group, data = PlantGrowth)
tidy(mod)

# Post-hoc test
TukeyHSD(mod)

# two-way anova -----
ToothGrowth %>%
  ggplot(aes(x = as.factor(dose),
             y = len,
             color = supp)) +
  geom_boxplot(width = 0.5,
               outlier.shape = NA) +
  theme_minimal()

mod <- aov(len ~ dose*supp,
           data = ToothGrowth %>%
             mutate(dose = as.factor(dose)))
summary(mod)
tidy(mod)
res_tukey <- TukeyHSD(mod)
tidy(res_tukey)

# PCA
biopsy <- read_csv("https://wilkelab.org/classes/SDS348/data_sets/biopsy.csv")

pca_fit <- biopsy %>%
  select(where(is.numeric)) %>% # retain only numeric columns
  prcomp(scale = TRUE) # do PCA on scaled data

pca_fit <- biopsy %>%
  select(where(is.numeric)) %>% # retain only numeric columns
  scale() %>% # scale data
  prcomp() # do PCA

pca_fit %>%
  augment(biopsy) %>% # add original dataset back in
  ggplot(aes(.fittedPC1, .fittedPC2, color = outcome)) +
  geom_point(size = 1.5) +
  scale_color_manual(
    values = c(malignant = "#D55E00", benign = "#0072B2")
  ) +
  theme_half_open(12) + background_grid()

pca_fit %>%
  tidy(matrix = "rotation")

# define arrow style for plotting
arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
)

# plot rotation matrix
pca_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 1, nudge_x = -0.02,
    color = "#904C2F"
  ) +
  xlim(-1.25, .5) + ylim(-.5, 1) +
  coord_fixed() +
  theme_minimal()

pca_fit %>%
  tidy(matrix = "eigenvalues")

pca_fit %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))
  ) +
  theme_minimal(12)

# linear regression
starwars

# Integrating final challenge --------------------------------------------------



