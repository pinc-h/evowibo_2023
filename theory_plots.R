# Alex Pinch, last edited April 24th 2023

library(tidyverse)

df <- data.frame(genotype = c("Non-inverted", "Heterozygote inverted", "Homozygote inverted"),
                 frequency = c(0.5, 1, 0.5))

df$genotype <- factor(df$genotype, levels = c("Non-inverted", "Heterozygote inverted", "Homozygote inverted"))

# Create the bar graph using ggplot2
ggplot(df, aes(x = factor(genotype), y = frequency)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Genotype") +
  ylab("Frequency") +
  theme(text = element_text(size = 20))

