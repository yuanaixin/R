# Plotting with duration and pitch (F0)
# Example of Changzhou Chinese 

# Load packages
Library(tidyverse)
Library(dplyr)
Library(ggplot2)

# Read file
> pitch = read.csv(file.choose(), header = T)
> filter(pitch, labelname == "single")

# Preparation
> pitch1 = pitch %>%
+ filter(labelname == "single")

# Remove NA values
> pitch1.new <- na.omit(pitch1)
> pitch1 <- pitch1.new

# Drop abnormal values
> pitch1 = subset(pitch1, pitch <= 350)

# Add label
> pitch1 <- pitch1 %>%
+ mutate(tone = case_when(endsWith(intervalname, "55") ~ "T1", endsWith(intervalname, "213") ~ "T2", endsWith(intervalname, "35") ~ "T3", endsWith(intervalname, "52") ~ "T4", endsWith(intervalname, "232") ~ "T5"))

# Plot
> pitch1.means = pitch1 %>%
+ group_by (tone, n) %>%
+ summarise(time_m = mean(time), pitch_m = mean(pitch))
> ggplot(pitch1.means, aes(time_m, pitch_m, color = tone, group = tone))+
+ geom_smooth(alpha=0.2)+
+ labs(y = "F0 (Hz)", x = "Time (s)")+
+ theme_minimal()+
+ scale_colour_manual(values = c("#FF6666", "#009966", "#FF9900", "#FF66FF", "#0099CC"), labels = c("T1 high level", "T2 low concave", "T3 high rise", "T5 high fall", "T6 low convex"))+
+ theme(legend.title = element_blank())
