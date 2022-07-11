# ------------------------------------------------------------------------------
# Learning R with the REU Students
# June 13, 2022
# TS O'Leary
# ------------------------------------------------------------------------------

# Day One ----------------------------------------------------------------------

# Basic Math ----
3 + 3
(3 + 2)^4 - 1
(3 + 2)^(4 - 1)

# Assigning variables
x <- 1
y <- 1

x + y

# Also you can save characters as variables
x <- "meow"

# Naming variables ----
greet <- "hello"
name <- "Tom"

print(c(greet, name))

c(greet, name)

# Cases
snake_case <- "This is snake case"
camelCase <- "This is camel case"
chaoscase <- "I don't recommend this"

# Forbidden characters
# _var <- 1
# 1_var <- 1
F <- 1 # This is a bad idea
T <- 1 # Bad

# Data types -------------------------------------------------------------------
char <- "letters or anything -- 1 $ % !"
char

# Check the class with the class function
class(char)
# You can use the question mark to ask what a function does
?class

# Numeric variable
num <- 1
class(num)

# Logical
log <- FALSE
class(log)
?log

# Factors are sorta a specific type of character or numeric data type with
#   levels
fac <- as.factor("cats")
fac


# Objects can be of different length
# Vectors

vec <- c(1, 2, 4, 6, 10)
vec
class(vec)

?c

# You can have characters, but it will make everything else into a character
vec_1 <- c(1:5, 10.5, "next")
class(vec_1)

# Let's sequence some stuff for fun
100000:1

?seq

vec_5 <- seq(from = 0, to = 100000, by = 5)

# What if we wanted specific items from this vector?
 # Indexing
# Starts at 1
vec_5[1]

vec_5[990]

# This gets the first two elements from the vec_5 object
vec_5[c(1, 2)]

# These are equivalent
vec_5[c(1:10)]
vec_5[1:10]

# What is the length of vec_5
last_index <- length(vec_5)
?length

# What is the last number in vec_5
vec_5[length(vec_5)]
vec_5[last_index]
vec_5[200001]

# What if we wanted the last 10 elements?
# Hard coding - BOOOO -- JK not really... but there are other ways
vec_5[199991:200001]

# Let's soft-code it
vec_5[(length(vec_5) - 10):length(vec_5)]
# When it gets complicated break it down!!

# But what if R already has a way to do that?????
head(vec_5, n = 10)
tail(vec_5, n = 10)

# Assessing equality
# Create two vectors
x <- sample(30:1)
y <- sample(c(1:6, 8:33))

x[x %in% y]
x[!(x %in% y)]
y[!(y %in% x)]

# Data frames ------------------------------------------------------------------
sample_id <- paste("sample", 1:16, sep = "_")
x <- 1:16
sqrt_x <- sqrt(x)

df <- data.frame(sample_id = sample_id,
                 x = x,
                 sqrt_x = sqrt_x)

class(df)

# Indexing a data frame ----
# Second column
df[2]
# Get the element in the first row second column
df[1, 2] # row, column format
df[16, 3]

# You can use the names of the columns like this
df[16, "sqrt_x"]

# Or like this
df$sqrt_x[16]

# What if we want to add a column
df$y <- df$sqrt_x + 1

# What are the dimensions of the data.frame in row, column
dim(df)
length(df) # gives us the number of columns
nrow(df)
ncol(df)

# Little sneak peak at visualization
plot(df$x, df$y)
# This is base R plotting, which we won't do that much of, but we will talk
#  viz in more detail later

# Look at R built in data
data()

dim(ToothGrowth)

str(ToothGrowth)

# It is good to explore your data
# What is the max length?
max(ToothGrowth$len)

# What is the min length?
min(ToothGrowth$len)

# There has gotta be a better way
summary(ToothGrowth) # This will give you way more information

hist(ToothGrowth$len, breaks = 20)

boxplot(formula = len ~ supp + dose,
        data = ToothGrowth)

boxplot(formula = len ~ dose,
        data = ToothGrowth)

boxplot(formula = len ~ supp,
        data = ToothGrowth)

# You can plot through in ggplot
require(ggplot2)
require(dplyr)

# Here is a quick example of plotting ToothGrowth data
ToothGrowth %>%
  ggplot() +
  geom_boxplot(aes(y = len,
                   x = as.factor(dose),
                   fill = supp)) +
  theme_classic()

# Day Two ----------------------------------------------------------------------

# Continue with the ToothGrowth data
head(ToothGrowth,
     n = 10)

summary(ToothGrowth)
str(ToothGrowth)

# Get just the unique numbers that are present in this column
unique(ToothGrowth$dose)

# Get the mean of the lengths overall
mean(ToothGrowth$len)

# Get the mean len for VC group
mean(ToothGrowth$len[ToothGrowth$supp == "VC"])

# There are other ways to subset a data.frame
summary(subset(ToothGrowth, supp == "VC")) # This gives you the whole df
mean(subset(ToothGrowth, supp == "VC")$len)

# Same for OJ
mean(subset(ToothGrowth, supp == "OJ")$len)



# But really there are 6 groups ---
# Hard code ...
# Lets say we want the OJ at 0.5 mg/ml
subset(ToothGrowth, supp == "OJ")[1:10, ]$len
subset(ToothGrowth, supp == "OJ")[1:10, "len"]
subset(ToothGrowth, supp == "OJ")[1:10, 1]
# Woah that was fun

# But there are other ways
mean(subset(ToothGrowth, dose == 0.5 & supp == "OJ")$len)
mean(subset(ToothGrowth, dose == 1 & supp == "OJ")$len)
mean(subset(ToothGrowth, dose == 2 & supp == "OJ")$len)
mean(subset(ToothGrowth, dose == 0.5 & supp == "VC")$len)
mean(subset(ToothGrowth, dose == 1 & supp == "VC")$len)
mean(subset(ToothGrowth, dose == 2 & supp == "VC")$len)
# The tidyverse has better ways...


# You can create your own functions
fancy_math_eqn <- function(x, y) {
  (x + y)^x + (1 - y)^(x+10)
}

fancy_math_eqn(y = 2,
               x = 2)

# Loops can be useful sometimes
for (i in 1:10) {
  print(paste("loop #", i))
}

vec <- c("hi", "this", "is", "a", "loop")
for (i in vec) {
  print(i)
}

# Tidyverse --------------------------------------------------------------------

# You need to install the tidyverse package before you begin here
# install.packages("tidyverse") # I only needed to do this once... but this is
#   how you download packages for future reference

# Load library
library(tidyverse)
# library(broom) # We aren't actually using this one right now

# What is tidy data? ----

# MDH example
set.seed(1)
mdh_df <- data.frame(adductor = rnorm(n = 15, mean = 7, sd = 1),
                     mantle = rnorm(n = 15, mean = 12, sd = 1),
                     gill = rnorm(n = 15, mean = 3, sd = 1))

mdh_df <- pivot_longer(mdh_df,
                       everything(),
                       names_to = "tissue_type",
                       values_to = "mdh_act")

# Iris challenge
iris
str(iris)

head(iris)
iris <- iris %>%
  pivot_longer(c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
               names_to = "measurement",
               values_to = "mm")


iris_tidy <-  pivot_longer(iris,
                      c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
                      names_to = "measurement",
                      values_to = "mm")

iris_avg <- iris_tidy %>%
  group_by(Species, measurement) %>%
  summarise(avg = mean(mm),
            sd = sd(mm))

iris_avg %>%
  mutate(avg_sd = paste(signif(avg, 3),
                        "±",
                        signif(sd, 3))) %>%
  select(-c(avg, sd)) %>%
  pivot_wider(names_from = Species,
              values_from = avg_sd)

# Day three --------------------------------------------------------------------

# Load library
library(tidyverse)

# We will be working with the starwars data
starwars

# Explore the Star Wars data together ----
glimpse(starwars)

# dplyr functions ----

# select takes columns
starwars %>%
  select(height)
select(starwars, height)

starwars %>%
  select(name, mass, height)

starwars %>%
  select(-sex)

# filter is for rows
starwars %>%
  filter(height < 100)

starwars %>%
  filter(eye_color == "red")

starwars %>%
  filter(height == 172)


# mutate is for creating new columns
# 2.54 cm in an inch
starwars %>%
  select(name, height) %>%
  mutate(height_inch = height/2.54) %>%
  arrange(height_inch)

# averages with summarize
starwars %>%
  summarise(height = mean(height, na.rm = TRUE),
            mass = mean(mass, na.rm = TRUE))

# group averages with group_by and summarize
starwars %>%
  group_by(hair_color) %>%
  summarise(height = mean(height, na.rm = TRUE),
            mass = mean(mass, na.rm = TRUE),
            n = n()) %>%
  filter(n > 5)

# Oh yeah how do you subset based on hair color again?
starwars %>%
  filter(hair_color == "auburn")

# You can still use base R like functions in the pipe
# so you can use the head function like this
starwars %>%
  arrange(desc(height)) %>%
  head(n = 20)

# There is a way to do this with dplyr function
starwars %>%
  slice_max(n = 20, order_by = height)

sw_mass <- starwars %>%
  slice_min(n = 70, order_by = mass)

# Data visualization -----------------------------------------------------------

# ggplot2

# Histogram plot
ggplot(data = mpg) +
  geom_histogram(mapping = aes(x = hwy), binwidth = 1)

ggplot(data = mpg) +
  geom_histogram(mapping = aes(x = hwy),
                 bins = 25,
                 color = "grey50",
                 fill = "grey70") +
  xlab("hwy (mpg)") +
  theme_classic()

# A histogram with just honda
mpg %>%
  filter(manufacturer == "honda") %>%
  ggplot() +
    geom_histogram(mapping = aes(x = hwy),
                   bins = 5,
                   color = "grey50",
                   fill = "grey70") +
    xlab("hwy (mpg)") +
    theme_classic()

# An x y scatter plot of all data
mpg %>%
  ggplot() +
  geom_point(aes(x = displ,
                 y = hwy,
                 color = as.factor(cyl)))

# Color by manufacturer instead
# Kinda too many colors
mpg %>%
  mutate(cyl = as.factor(cyl)) %>%
  ggplot() +
  geom_point(aes(x = displ,
                 y = hwy,
                 color = manufacturer))

# Scatter plot by manufacturer

mpg %>%
  ggplot() +
  geom_point(aes(y = hwy,
                 x = manufacturer))
# But this has a bunch of over lapping points that you cannot see

# Better to do with the circle point shape = 21 and add jitter
# Also flipping the axis here
mpg %>%
  ggplot() +
  geom_jitter(aes(y = hwy,
                  x = manufacturer),
              width = 0.1,
              shape = 21,
              color = "grey10",
              fill = "grey",
              alpha = 0.3) +
  coord_flip() +
  theme_minimal()

# Boxplots
mpg %>%
  ggplot() +
  geom_boxplot(aes(y = hwy,
                  x = manufacturer),
              width = 0.5,
              shape = 21,
              color = "grey10",
              fill = "grey",
              alpha = 0.3) +
  coord_flip() +
  theme_minimal()

# Let's try combining two types of plots
mpg %>%
  ggplot() +
  geom_boxplot(aes(y = hwy,
                   x = manufacturer),
               width = 0.5,
               # Need to add this so outliers are not plotted twice
               outlier.shape = NA,
               shape = 21,
               color = "grey10",
               fill = "grey",
               alpha = 0.3) +
  geom_jitter(aes(y = hwy,
                  x = manufacturer),
              width = 0.1,
              shape = 21,
              color = "grey10",
              fill = "grey",
              alpha = 0.3) +
  coord_flip() +
  theme_minimal()

# You can define the mapping at the begining
# The mapping ie the aes() of the plots can be put in the top ggplot function
# and then it is carried over to the other functions below
mpg %>%
  ggplot(aes(y = hwy,
             x = manufacturer)) +
  geom_boxplot(width = 0.5,
               outlier.shape = NA,
               shape = 21,
               color = "grey10",
               fill = "grey",
               alpha = 0.3) +
  geom_jitter(width = 0.1,
              shape = 21,
              color = "grey10",
              fill = "grey",
              alpha = 0.3) +
  coord_flip() +
  theme_minimal()

# You can also plot text and whatnot
starwars %>%
  ggplot() +
  geom_text(aes(x = height,
                y = mass,
                label = name))

# Plotting without
starwars %>%
  filter(mass < 1000) %>%
  ggplot(aes(x = height,
             y = mass)) +
  geom_point(aes(color = species)) +
  geom_smooth(method = "lm",
              se = FALSE)

# Final figure of some data! -- pick your own

# Day Four ---------------------------------------------------------------------
# Final day of Part I

# Load libraries
library(tidyverse)
library(broom)

# A few more plots together ----
starwars %>%
  ggplot(aes(x = height,
             y = mass)) +
  geom_point()

# Plotting the height and mass of Star Wars characters
ggplot(data = starwars) +
  geom_point(mapping = aes(x = height,
                           y = mass,
                           color = species))

ggplot(data = starwars) +
  geom_point(mapping = aes(x = height,
                           y = mass),
                           shape = 21)


# Adding a linear regression
starwars %>%
  filter(mass < 1000) %>%
  ggplot(aes(x = height,
             y = mass)) +
  geom_point(aes(color = species)) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "grey50",
              linetype = 2)

# Save the plots
ggsave("sw_height_mass_dotted.pdf",
       width = 7,
       height = 5)
# Saving as a pdf, or tiff is prefered as it won't pixelate when made large
#   for presentations



# A few challenge plots at the end -----

# Make a plot for tooth growth
ToothGrowth %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(dose),
                   y = len,
                   fill = supp),
               color = "grey50") +
  labs(y = "Length (mm)",
       x = "Dose (mg/mL)") +
  scale_fill_manual(values = c("grey20", "grey80")) +
  theme_minimal()

# Iris data
ggplot(iris,
       aes(x = Species,
           y = Sepal.Length)) +
  geom_boxplot() +
  labs(title="Sepal Length", y = "Length")

# Statistics and experimental design -------------------------------------------

# Hypothesis tests

# t-test of the sleep data
sleep

?t.test
t.test(extra ~ group,
       data = sleep)

# ANOVA -----

# One-way Anova

# Plant growth data
PlantGrowth

# Quickly vizualize the plant growth data
plot(weight ~ group, data = PlantGrowth)
# This is the piping equivalent to the above
PlantGrowth %>%
  plot(weight ~ group,
       data = .)

# Run an ANOVA model and save as mod
mod <- aov(weight ~ group, data = PlantGrowth)

# Get summary of mod object
summary(mod)
# Summary results in tidy format below
#   "broom::" is only necessary if you have not loaded the broom package
broom::tidy(mod)

# Post-hoc differences in pairwise comparisons with TukeyHSD
broom::tidy(TukeyHSD(mod))

# Two-way ANOVA on ToothGrowth data
mod <- aov(len ~ supp + dose + supp:dose,
           data = ToothGrowth %>%
             mutate(dose = as.factor(dose)))

tidy(mod)
tidy(TukeyHSD(mod))
