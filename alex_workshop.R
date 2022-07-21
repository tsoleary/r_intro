# ------------------------------------------------------------------------------
# Alex's Survival Data Analysis with Workshop
# July 20, 2022
# TS O'Leary
# ------------------------------------------------------------------------------

# Load libraries
require(tidyverse)
require(drc)

# Load data
df <- read_csv("student_data/reu_data_alex.csv") %>%
  dplyr::select(-survival)

# Analyze data
df %>%
  group_by(Genotype, Temperature) %>%
  summarize(prop_hatch = sum(Hatched_48)/sum(Eggs))

# But this is obviously not linear
df %>%
  filter(Genotype == "CH") %>%
  ggplot(aes(x = Temperature,
             y = Hatched_48/Eggs)) +
  geom_point() +
  geom_smooth(method = "lm")


# Lets fit a dose response curve to the data instead...
# And let's make it a little pretty
df %>%
  filter(Genotype == "CH") %>%
  ggplot(aes(x = Temperature,
             y = Hatched_48/Eggs)) +
  geom_point() +
  geom_smooth(method = drm,
              method.args = list(fct = LL.3()),
              se = FALSE)

# Final pretty  figure for Chiapas
#   The Mexican state that border Belize & Guatemala
df %>%
  filter(Genotype == "CH") %>%
  ggplot(aes(x = Temperature,
             y = Hatched_48/Eggs)) +
  geom_smooth(method = drm,
              method.args = list(fct = LL.3()),
              se = FALSE,
              color = "lavenderblush2") +
  geom_jitter(width = 0.1,
              shape = 21,
              size = 2,
              fill = "lavenderblush2",
              alpha = 0.8) +
  geom_hline(yintercept = 0.5,
             color = "grey80",
             linetype = 1) +
  theme_classic() +
  labs(x = "Acute heat shock temperature (°C)",
       y = "Proportion hatched") +
  theme(panel.grid.major.y = element_line(color = "grey85",
                                          linetype = 3),
        axis.line = element_line(color = "grey70"),
        axis.ticks = element_line(color = "grey70"))

# Chiapas & VTECK_8
df %>%
  filter(Genotype == "CH" | Genotype == "VTECK_8") %>%
  ggplot(aes(x = Temperature,
             y = Hatched_48/Eggs,
             color = Genotype,
             fill = Genotype)) +
  geom_smooth(method = drm,
              method.args = list(fct = LL.3()),
              se = FALSE) +
  geom_jitter(width = 0.1,
              shape = 21,
              size = 2,
              color = "grey70",
              alpha = 0.8) +
  geom_hline(yintercept = 0.5,
             color = "grey80",
             linetype = 1) +
  scale_color_manual(values = c("lavenderblush2",
                                "azure3")) +
  scale_fill_manual(values = c("lavenderblush2",
                                "azure3")) +
  theme_classic() +
  labs(x = "Acute heat shock temperature (°C)",
       y = "Proportion hatched") +
  theme(panel.grid.major.y = element_line(color = "grey85",
                                          linetype = 3),
        axis.line = element_line(color = "grey70"),
        axis.ticks = element_line(color = "grey70"))

# With two genotypes from Alex's data
df %>%
  filter(Genotype == "FLCK 06" | Genotype == "FRMO-01") %>%
  ggplot(aes(x = Temperature,
             y = Hatched_48/Eggs,
             color = Genotype,
             fill = Genotype,
             shape = Genotype)) +
  geom_smooth(aes(linetype = Genotype),
              method = drm,
              method.args = list(fct = LL.3()),
              se = FALSE) +
  geom_jitter(width = 0.1,
              size = 3,
              color = "grey70",
              alpha = 0.8) +
  geom_hline(yintercept = 0.5,
             color = "grey80",
             linetype = 1) +
  scale_color_manual(values = c("lavenderblush2",
                                "azure3")) +
  scale_fill_manual(values = c("lavenderblush2",
                               "azure3")) +
  scale_shape_manual(values = c(21, 22)) +
  scale_x_continuous(breaks = c(26, 28, 30, 32, 34, 36)) +
  theme_classic() +
  labs(x = "Acute heat shock temperature (°C)",
       y = "Proportion hatched") +
  theme(panel.grid.major.y = element_line(color = "grey85",
                                          linetype = 3),
        axis.line = element_line(color = "grey70"),
        axis.ticks = element_line(color = "grey70"))


# What does it look like for a bunch of genotypes at the same time?
df %>%
  mutate(region = ifelse(is.na(region),
                         case_when(str_detect(Genotype, "FLCK") ~ "Florida",
                                   str_detect(Genotype, "FR") ~ "France",
                                   str_detect(Genotype, "JP") ~ "Japan"),
                         str_to_sentence(region))) %>%
  mutate(region = factor(region, levels = c("Japan",
                                            "France",
                                            "Florida",
                                            "Temperate",
                                            "Tropical")), level=TRUE) %>%
  filter(!str_detect(Genotype, "Canton") & !str_detect(Genotype, "X")) %>%
  #arrange(desc(region)) %>%
  ggplot(aes(x = Temperature,
             y = Hatched_48/Eggs,
             color = region)) +
  geom_hline(yintercept = 0.5,
             color = "grey80",
             linetype = 1) +
  geom_smooth(aes(group = Genotype),
              method = drm,
              method.args = list(fct = LL.3()),
              se = FALSE) +
  scale_color_manual(values = c("skyblue",
                                "pink",
                                "green",
                                "orange",
                                "firebrick"),
                     name = "Locale") +
  theme_classic() +
  labs(x = "Acute heat shock temperature (°C)",
       y = "Proportion hatched") +
  theme(panel.grid.major.y = element_line(color = "grey85",
                                          linetype = 3),
        axis.line = element_line(color = "grey70"),
        axis.ticks = element_line(color = "grey70"))

# Let's do some math and stats
surv_curv <- df %>%
  group_by(Genotype) %>%
  nest() %>%
  mutate(fit = map(data, ~ drm(Hatched_48/Eggs ~ Temperature,
                               data = .x,
                               weight = Eggs,
                               fct = LL.3(names = c("slope",
                                                    "upper limit",
                                                    "LT50")),
                               type = "binomial"))) %>%
  mutate(tidy_fit = map(fit, broom::tidy))




