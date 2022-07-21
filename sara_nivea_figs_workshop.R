# ------------------------------------------------------------------------------
# Nivea & Sara -- rRNA depl data -- figs and stuff
# July 21, 2022
# TS O'Leary
# ------------------------------------------------------------------------------

# Load libraries
require(tidyverse)
require(DESeq2)
require(ggrepel)

# Load data
dds <- readRDS("student_data/reu_workshop_nivea_dds_rRNA.rds")

# Load specific result comparisions
res_37 <- results(dds,
                  name = "condition_Hot37_vs_Control",
                  alpha = 0.05)
res_34 <- results(dds,
                  name = "condition_Hot34_vs_Control",
                  alpha = 0.05)
res_10 <- results(dds,
                  name = "condition_Cold10_vs_Control",
                  alpha = 0.05)
res_04 <- results(dds,
                  name = "condition_Cold4_vs_Control",
                  alpha = 0.05)

# Take a peak at the summary values
summary(res_37)
summary(res_34)
summary(res_10)
summary(res_04)

# Convert to a tibble because TSO likes that format better
res_37 <- res_37 %>%
  as_tibble(rownames = "FBgn")
res_34 <- res_34 %>%
  as_tibble(rownames = "FBgn")
res_10 <- res_10 %>%
  as_tibble(rownames = "FBgn")
res_04 <- res_04 %>%
  as_tibble(rownames = "FBgn")

# Combine all these results together
res <- bind_rows(list("res_37" = res_37,
                      "res_34" = res_34,
                      "res_10" = res_10,
                      "res_04" = res_04),
                 .id = "temp")

# Let's count 'em up
res %>%
  filter(padj < 0.05) %>%
  group_by(temp, log2FoldChange > 0) %>%
  tally() %>%
  dplyr::rename(direction = `log2FoldChange > 0`) %>%
  mutate(direction = ifelse(direction, "Up", "Down"))

# Volcano Plots --
# Cold only
res %>%
  filter(temp == "res_10" | temp == "res_04") %>%
  mutate(temp = case_when(temp == "res_04" ~ "4°C vs 25°C",
                          temp == "res_10" ~ "10°C vs 25°C",
                          temp == "res_34" ~ "34°C vs 25°C",
                          temp == "res_37" ~ "37°C vs 25°C")) %>%
  mutate(temp = factor(temp, levels = c("4°C vs 25°C",
                                        "10°C vs 25°C",
                                        "34°C vs 25°C",
                                        "37°C vs 25°C"))) %>%
  filter(!is.na(padj)) %>%
  ggplot() +
  geom_point(aes(x = log2FoldChange,
                 y = -log10(padj),
                 color = padj < 0.05)) +
  geom_vline(xintercept = 0,
             color = "grey70") +
  geom_hline(yintercept = -log10(0.05),
             color = "grey70") +
  scale_color_manual(values = c("grey70",
                                "firebrick",
                                "grey80"),
                     name = "Differentially expressed genes",
                     label = c("Not signigicant", "Significant")) +
  xlim(c(-14,14)) +
  theme_classic() +
  theme(legend.position = "top") +
  facet_wrap(~ temp)

# Hot only
res %>%
  filter(temp == "res_34" | temp == "res_37") %>%
  mutate(temp = case_when(temp == "res_04" ~ "4°C vs 25°C",
                          temp == "res_10" ~ "10°C vs 25°C",
                          temp == "res_34" ~ "34°C vs 25°C",
                          temp == "res_37" ~ "37°C vs 25°C")) %>%
  mutate(temp = factor(temp, levels = c("4°C vs 25°C",
                                        "10°C vs 25°C",
                                        "34°C vs 25°C",
                                        "37°C vs 25°C"))) %>%
  filter(!is.na(padj)) %>%
  ggplot() +
  geom_point(aes(x = log2FoldChange,
                 y = -log10(padj),
                 color = padj < 0.05)) +
  geom_vline(xintercept = 0,
             color = "grey70") +
  geom_hline(yintercept = -log10(0.05),
             color = "grey70") +
  scale_color_manual(values = c("grey70",
                                "firebrick",
                                "grey80"),
                     name = "Differentially expressed genes",
                     label = c("Not signigicant", "Significant")) +
  xlim(c(-14,14)) +
  theme_classic() +
  theme(legend.position = "top") +
  facet_wrap(~ temp)

# All four comparisons
res %>%
  mutate(temp = case_when(temp == "res_04" ~ "4°C vs 25°C",
                          temp == "res_10" ~ "10°C vs 25°C",
                          temp == "res_34" ~ "34°C vs 25°C",
                          temp == "res_37" ~ "37°C vs 25°C")) %>%
  mutate(temp = factor(temp, levels = c("4°C vs 25°C",
                                        "10°C vs 25°C",
                                        "34°C vs 25°C",
                                        "37°C vs 25°C"))) %>%
  filter(!is.na(padj)) %>%
  ggplot() +
  geom_point(aes(x = log2FoldChange,
                 y = -log10(padj),
                 color = padj < 0.05)) +
  geom_vline(xintercept = 0,
             color = "grey70") +
  geom_hline(yintercept = -log10(0.05),
             color = "grey70",
             linetype = 2) +
  scale_color_manual(values = c("grey70",
                                "firebrick",
                                "grey80"),
                     name = "Differentially expressed genes",
                     label = c("Not signigicant", "Significant")) +
  xlim(c(-14,14)) +
  theme_classic() +
  theme(legend.position = "top") +
  facet_wrap(~ temp)



# Let's convert to gene symbol from FBgn
library(AnnotationDbi)
library(org.Dm.eg.db)

res$symbol <- mapIds(org.Dm.eg.db,
                     keys = res$FBgn,
                     column = "SYMBOL",
                     keytype = "FLYBASE",
                     multiVals = "first")


# Let's try and add symbols to the volcano plot
res %>%
  filter(temp == "res_37") %>%
  filter(!is.na(padj)) %>%
  ggplot(aes(x = log2FoldChange,
             y = -log10(padj))) +
  geom_point(aes(color = padj < 0.05)) +
  geom_vline(xintercept = 0,
             color = "grey70") +
  geom_hline(yintercept = -log10(0.05),
             color = "grey70") +
  geom_text_repel(data = res %>%
                    filter(temp == "res_37") %>%
                    arrange(padj) %>%
                    head(10),
                  aes(label = symbol)) +
  scale_color_manual(values = c("grey70",
                                "firebrick",
                                "grey80"),
                     name = "Differentially expressed genes",
                     label = c("Not signigicant", "Significant")) +
  xlim(c(-14,14)) +
  theme_classic() +
  theme(legend.position = "top")



