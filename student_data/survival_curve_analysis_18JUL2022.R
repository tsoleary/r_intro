# ------------------------------------------------------------------------------
# Survival data analysis
# July 18, 2022
# Alex Lewis
# ------------------------------------------------------------------------------

# Load libraries
require(tidyverse)
require(drc)

surv <- read_csv("raw_survival_alex_18JUL2022.csv")


#Create Graph---------------------------------------------------------------
surv %>%
  filter(!str_detect(Genotype, "Canton") & 
           !str_detect(Genotype, "FLCK")) %>%
  mutate(Prop_Hatched = Hatched_48/Eggs) %>%
  ggplot(aes(x = Temperature,
             y = Prop_Hatched,
             color = Genotype)) +
  geom_point() +
  geom_smooth(method = drm,
              method.args = list(fct = LL.3()),
              se = FALSE) +
  geom_hline(yintercept = 0.5,
             color = "grey50",
             linetype = 2) +
  labs(x = "Heat Shock Temperature (°C)",
       y = "Proportion Hatched") +
  ylim(c(0,1)) +
  theme_minimal()



# LT50-----------------------------------------------------------------------
lt_surv_curv <- surv %>%
  filter(!str_detect(Genotype, "Canton")) %>%
  group_by(Genotype) %>%
  nest() %>%
  mutate(fit = map(data, ~ drm(Hatched_48/Eggs ~ Temperature, 
                               data = .x, 
                               weight = Eggs,
                               fct = LL.3(names = c("slope", 
                                                    "upper limit", 
                                                    "LT50")),
                               type = "binomial")),
         lt_s = map(fit, ~ ED(.x, 
                              c(10, 50, 90), 
                              interval = "delta",
                              display = FALSE))) %>%
  mutate(tidy_fit = map(fit, broom::tidy))  %>%
  mutate(lt_10 = lt_s[[1]][[1]],
         lt_50 = lt_s[[1]][[2]],
         lt_90 = lt_s[[1]][[3]])




lt_surv_curv  
