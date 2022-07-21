#-------------------------------------------------------------------------------
# Catherine Fauver
# 7/12/2022
# Transcriptional change in all basic conditions
#-------------------------------------------------------------------------------
# Load in libraries
library(tidyverse)
library(ggplot2)
library(data.table)

# Set working directory
setwd("/Users/catherinefauver/reu_workshop/ifm_gene_expression")

#2.2 Read in counts matrix and sample descriptors 
Counts <- read.csv("0616aa-ifm-counts.csv", check.names = FALSE, row.names = 1)
Conds <- read.delim("0616aa-ifm-factors.txt", header=TRUE, 
                    stringsAsFactors=TRUE, row.names=1)

# Create new columns with averages of individual sample counts for 18_NS, 25_NS, and 
# 30_NS
Counts$adv_25_NS <- rowMeans(Counts[ ,c(1:4)], na.rm = TRUE)
Counts$adv_18_NS <- rowMeans(Counts[ ,c(13:16)], na.rm = TRUE)
Counts$adv_30_NS <- rowMeans(Counts[ ,c(33:36)], na.rm = TRUE)

# Calculate p values for each of the gene comparisons
pairedSamplesTTest( formula = adv_25_NS + adv_18_NS , data= Counts)

pairwise.t.test( x = Counts$adv_18_NS,   # outcome variable
                 g = Counts$adv_25_NS,        # grouping variable
                 p.adjust.method = "none"    # which correction to use?
)

t.test(formula = adv_25_NS, 
        data = Counts, 
        subset = adv_25_NS %in% c(adv_30_NS,adv_18_NS), 
        var.equal = TRUE)

#2.3 Subset data to only include comparison of interest
##Here, all the No Shock samples (exluding 25H3)
CountsHM <- Counts[,c(1:4, 17:20)] 
CondsHM <- Conds[,c(1:4, c(1, 2, 3, 4), 17:20),]

# Get list of DEGs for comparison of interest (only baseline)
degs <- N_AA_Acc %>% filter(padj < 0.05)
up_degs <- degs %>% filter(log2FoldChange > 0)
down_degs <- degs %>% filter(log2FoldChange < 0)

# Make data frames
direction <- c("up_degs", "down_degs")
number <- c(1082, 1383)

df <- data.frame(direction = direction,
                 number = number)

# Plot into boxplots
ggplot(data = df) +
  geom_col(mapping = aes(x = direction,
                         y = number),
           fill = "lightpink") +
  geom_text(aes(x = direction,
                y = number,
                label = number),
            vjust = 1.5, 
            color = "black") +
  labs(y = "Number of DEGs",
       x = "Direction of Regulation",
       title = "Differentially Expressed Genes Under Acclimation Conditions 
       (No Shock)") +
  theme_minimal()


# Comparing affect of acclimation DEGs with no shock versus heat shock---------

# Load counts
N_AA_Acc <- read.csv("0616IFM_N_AA_resAcc.csv")
H_AA_Acc <- read.csv("0616IFM_H_AA_resAcc.csv")

# Get list of DEGs for comparison of interest (only baseline)
N_degs <- N_AA_Acc %>% filter(padj < 0.05)
N_up_degs <- N_degs %>% filter(log2FoldChange > 0)
N_down_degs <- N_degs %>% filter(log2FoldChange < 0)

H_degs <- H_AA_Acc %>% filter(padj < 0.05)
H_up_degs <- H_degs %>% filter(log2FoldChange > 0)
H_down_degs <- H_degs %>% filter(log2FoldChange < 0)

# Make data frames
N_direction <- c("N_up_degs", "N_down_degs")
N_number <- c(1082, 1383)

H_direction <- c("H_up_degs", "H_down_degs")
H_number <- c(1049, 1267)

Ndf <- data.frame(direction = N_direction,
                  number = N_number)

Hdf <- data.frame(direction = H_direction,
                  number = H_number)

df <- rbind(Ndf, Hdf)

# Plot into boxplots
ggplot(data = df) +
  geom_col(mapping = aes(x = direction,
                         y = number),
           fill = "lightpink") +
  labs(y = "Number of DEGs",
       x = "Direction of Regulation",
       title = "Differentially Expressed Genes Under Adult Acclimation 
       (No Shock vs Heat Shock)") +
  scale_fill_discrete(labels = c("Heat Shocked Downregulated DEGs",
                                 "Heat Shocked Upregulated DEGs",
                                 "No Shock Downregulated DEGs",
                                 "No Shock Upregulated DEGs")) +
  theme_minimal()

# Comparing affect of acclimation DEGs with no shock versus cold shock---------

# Load counts
N_AA_Acc <- read.csv("0616IFM_N_AA_resAcc.csv")
C_AA_Acc <- read.csv("0616IFM_C_AA_resAcc.csv")

# Get list of DEGs for comparison of interest (only baseline)
N_degs <- N_AA_Acc %>% filter(padj < 0.05)
N_up_degs <- N_degs %>% filter(log2FoldChange > 0)
N_down_degs <- N_degs %>% filter(log2FoldChange < 0)

C_degs <- C_AA_Acc %>% filter(padj < 0.05)
C_up_degs <- C_degs %>% filter(log2FoldChange > 0)
C_down_degs <- C_degs %>% filter(log2FoldChange < 0)

# Make data frames
N_direction <- c("N_up_degs", "N_down_degs")
N_number <- c(1082, 1383)

C_direction <- c("C_up_degs", "C_down_degs")
C_number <- c(628, 815)

Ndf <- data.frame(direction = N_direction,
                  number = N_number)

Cdf <- data.frame(direction = C_direction,
                  number = C_number)

df <- rbind(Ndf, Cdf)

# Plot into boxplots
ggplot(data = df) +
  geom_col(mapping = aes(x = direction,
                         y = number),
           fill = "lightpink") +
  labs(y = "Number of DEGs",
       x = "Direction of Regulation",
       title = "Differentially Expressed Genes Under Adult Acclimation 
       (No Shock vs Cold Shock)") +
  scale_fill_discrete(labels = c("Cold Shocked Downregulated DEGs",
                                 "Cold Shocked Upregulated DEGs",
                                 "No Shock Downregulated DEGs",
                                 "No Shock Upregulated DEGs")) +
  theme_minimal()


# Comparing affect of acclimation DEGs with no shock---------
Adult_Acclimation_State <- c("18_vs_25", "30_vs_25")
Up <- c(135, 479)
Down <- c(129, 186)

df <- data.frame(Adult_Acclimation_State, Up, Down)
df <- pivot_longer(df, cols = c("Up", "Down"), names_to = "Direction")

df$Direction <- as.factor(df$Direction)

# Plot into boxplots
ggplot(data = df) +
  geom_col(mapping = aes(x = Adult_Acclimation_State,
                         y = value,
                         fill = Direction), position = "dodge") +
  geom_text(aes(x = Adult_Acclimation_State,
                y = value, 
                label = value,
                group = value),
            position = position_dodge(width = 1),
            vjust = -.5,
            color = "black") +
  ylim(0,600) +
  labs(y = "Number of DEGs",
       x = "Acclimation Comparision",
       title = "Differentially Expressed Genes Under Adult Acclimation States
       (No Shock)") +
  theme_minimal()

# Comparing DEGs with different shock conditions (HSvsNS and CSvsNS)---------
Shock <- c("CS_vs_NS", "HS_vs_NS")
Up <- c(277, 765)
Down <- c(19, 228)

df <- data.frame(Shock, Up, Down)
df <- pivot_longer(df, cols = c("Up", "Down"), names_to = "Direction")

df$Direction <- as.factor(df$Direction)

# Plot into boxplots
ggplot(data = df) +
  geom_col(mapping = aes(x = Shock,
                         y = value,
                         fill = Direction), position = "dodge") +
  geom_text(aes(x = Shock,
                y = value, 
                label = value,
                group = value),
             position = position_dodge(width = 1),
              vjust = -.5,
             color = "black")+
  ylim(0,900) +
  labs(y = "Number of DEGs",
       x = "Shock Comparision",
       title = "Differentially Expressed Genes In Shock Conditions
       (No Acclimation)") +
  theme_minimal()

# Comparing DEGs with different shock conditions at each acclimation state------
Acclimation_Temp <- c("18", "25", "30")
Shock <- c("CSvsNS", "HSvsNS","CSvsNS", "HSvsNS","CSvsNS", "HSvsNS")
Up <- c(26, 765, 1205, 184, 1584, 1097)
Down <- c(318, 228, 1382, 499, 1561, 1614)

df <- data.frame(Acclimation_Temp, Shock, Up, Down)
df <- pivot_longer(df, cols = c("Up", "Down"), names_to = "Direction")

df$Direction <- as.factor(df$Direction)


# Plot into boxplots
ggplot(data = df) +
  geom_col(mapping = aes(x = Acclimation_Temp,
                         y = value,
                         fill = Direction), position = "dodge") +
  geom_text(aes(x = Acclimation_Temp,
                y = value, 
                label = value,
                group = Direction),
            position = position_dodge(width = 1),
            vjust = -.5,
            color = "black") +
  facet_wrap(vars(Shock)) +
  ylim(0, 1800) +
  labs(y = "Number of DEGs",
       x = "Adult Acclimation Temperature",
       title = "Differentially Expressed Genes In Adult Acclimation States
       Across All Shock Groups") +
  theme_minimal()

