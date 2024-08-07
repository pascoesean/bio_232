# load packages

library(tidyverse)
library(readxl)

data <- read_excel("example_ex2_results.xlsx")


data |>
  ggplot(aes(x = worm_type, y = average_speed, color = plasmid)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2)) +
  scale_color_manual(values = c("#d1495b", "#003f88")) +
  facet_wrap(~day, nrow = 1) +
  labs(title = "Worm Mobility", y = "Average Speed (mm/s)", x = "Worm type") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust = 1)) 

averages <- data |>
  mutate(day = factor(day)) |>
  group_by(worm_type, plasmid, day) |>
  summarize(average = mean(average_speed),
            sd = sd(average_speed))


ggplot() +
  geom_boxplot(data = data, 
               alpha= 0.3,
               aes(x = factor(day), y = average_speed, color = interaction(worm_type, plasmid, sep=':')),
               outlier.shape = NA) +
  geom_point(data = data, 
             alpha= 0.5,
             aes(x = factor(day), y = average_speed, color = interaction(worm_type, plasmid, sep=':')),
             position = position_jitterdodge(jitter.width = 0.7)) +
  geom_line(data = averages, 
            mapping = aes(x = day, group=1, y = average, color = interaction(worm_type, plasmid, sep=':')),
            linewidth = 1) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Worm Mobility", y = "Average Speed (mm/s)", x = "Worm type", color = "Condition") +
  facet_grid(rows =vars(worm_type), cols = vars(plasmid)) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust = 1)) 

