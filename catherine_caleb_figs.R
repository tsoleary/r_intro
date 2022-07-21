# ------------------------------------------------------------------------------
# Catherine & Caleb data
# July 19, 2022
# TS O'Leary
# ------------------------------------------------------------------------------

# Load libraries
require(tidyverse)

# Load data
df <- read_csv("student_data/changes_in_AA.csv")

# Plot data

# Minimal plot
ggplot(data = df,
       aes(y = value,
           x = Adult_Acclimation_State,
           fill = Direction)) +
  ylab("Number of differentially expressed genes") +
  geom_bar(stat = "identity")

# With the position dodge
ggplot(data = df,
       aes(y = value,
           x = Adult_Acclimation_State,
           fill = Direction)) +
  ylab("Number of differentially\nexpressed genes") +
  geom_bar(stat = "identity",
           position = "dodge") +
  theme_classic(base_size = 16)

# Those underscores kinda stink
ggplot(data = df,
       aes(y = value,
           x = Adult_Acclimation_State,
           fill = Direction)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_x_discrete(labels = c("18°C", "30°C")) +
  ylab("Number of differentially\nexpressed genes") +
  xlab("Adult Acclimation State") +
  theme_classic(base_size = 16)


# Okay lets try the thing where the directions depend on the directions
df %>%
  mutate(value = ifelse(Direction == "Down", -value, value)) %>%
  ggplot(aes(x = value,
             y = Adult_Acclimation_State,
             fill = Direction)) +
  geom_bar(stat = "identity",
           position = "identity",
           width = 0.5) +
  geom_vline(xintercept = 0) +
  geom_text(aes(label = abs(value),
                hjust = 1)) +
  scale_y_discrete(labels = c("18°C", "30°C"),
                   name = "Adult Acclimation State") +
  scale_x_continuous(position = "top",
                     name = "Number of differentially expressed genes",
                     limits = c(-500, 500),
                     breaks = seq(from = -400, to = 400, by = 200),
                     labels = abs(seq(from = -400, to = 400, by = 200))) +
  scale_fill_manual(values = c("#F58161", "#B3D89C")) +
  theme_classic(base_size = 16) +
  theme(axis.line.y = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_line(color = "grey95"),
        axis.title.x = element_blank())

ggsave("cath_cal_fig.pdf", height = 4, width = 7)

