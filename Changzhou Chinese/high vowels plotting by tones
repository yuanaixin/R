> library(ggrepel)
> library(ggplot2)
> library(dplyr)
> library(tidyverse)

# Load data
> czthesis.vowel = read.csv(file.choose(), header = T)
> czthesis.vowel = czthesis.vowel %>%
+ filter(Label == "single")

# Preparation, add tone column
> czthesis.vowel <- czthesis.vowel %>%
+ mutate(Tone = case_when(endsWith(Token, "55") ~ "T1", endsWith(Token, "213") ~ "T2", endsWith(Token, "35") ~ "T3", endsWith(Token, "52") ~ "T4", endsWith(Token, "232") ~ "T5"))

# Add vowel column (since i named vowel with different tones)
> czthesis.vowel <- czthesis.vowel %>%
+ mutate(Type = case_when(Token %in% c("i55", "i35", "i232", "i52", "i213") ~ "i", Token %in% c("iz55", "iz35", "iz232", "iz52", "iz213") ~ "iz", Token %in% c("i155", "i135", "i1232", "i152", "i1213", "i1") ~ "i1", Token %in% c("u155", "u135", "u1232", "u152", "u1213") ~ "u1", Token %in% c("yz55", "yz35", "yz232", "yz52", "yz213") ~ "yz", Token %in% c("u55", "u35", "u232", "u52", "u213") ~ "u", Token %in% c("ub213", "ub232", "ub35") ~ "ub", Token %in% c("uv213", "uv232", "uv35","uz") ~ "uv"))

# Calculate means
> czthesis.vowel.means = czthesis.vowel %>%
+ group_by(Type, Tone) %>%
+ summarise(f1_50_m = mean(f1_50), f2_50_m = mean(f2_50))

# Add IPA column, used as labels on the plot
> czthesis.vowel.means <- czthesis.vowel.means %>%
+ mutate(IPA = case_when(Type %in% c("i") ~ "i", Type %in% c("iz") ~ "i͡ʑ", Type %in% c("i1") ~ "ɿ", Type %in% c("u") ~ "u", Type %in% c("u1") ~ "ʮ", Type %in% c("yz") ~ "y͡ʑ", Type %in% c("ub") ~ "u͡b", Type %in% c("uv") ~ "u͡v"))

# Plotting
> ggplot(czthesis.vowel, aes(x = f2_50, y = f1_50, color = Type))+
+ geom_point(alpha = 0.3)+
+ stat_ellipse(level = 0.67, geom = "polygon", alpha = 0.1, aes(fill = Type))+
+ geom_label_repel(data = czthesis.vowel.means, aes(f2_50_m, f1_50_m, label = IPA, point.size = NA))+
+ scale_x_reverse()+
+ scale_y_reverse()+
+ theme_minimal()+
+ labs(x = "F2", y = "F1")+
+ facet_grid(~Tone)+
+ labs(title = "High vowels by tone type")
