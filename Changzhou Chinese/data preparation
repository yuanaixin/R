#Load dataset
czthesis = read.csv(file.choose(), header = T)

#Load packages
Library(tidyverse)
Library(dplyr)
Library(ggplot2)

#Add columns tone and position
> czthesis <- czthesis %>%
+ mutate(position = case_when(endsWith(Label, "st") ~ "inword", endsWith(Label, "dle") ~ "inword", endsWith(Label, "gle") ~ "single"))

> czthesis <- czthesis %>%
+ mutate(Tone = case_when(endsWith(Vowel, "55") ~ "T1", endsWith(Vowel, "213") ~ "T2", endsWith(Vowel, "35") ~ "T3", endsWith(Vowel, "52") ~ "T4", endsWith(Vowel, "232") ~ "T5"))

#Add columns vowel proportion and base
> czthesis <- czthesis %>%
+ mutate(proportion = VowelDuration.ms./Duration.ms.)
> czthesis$base = 1

#Add columns peak (of intensity)
> czthesis <- czthesis %>%
+ mutate(peak = (MaxIntensityTime.ms. - Beginning.ms.)/(End.ms. - Beginning.ms.))

#Add column "onset" based on token number
> czthesis <- czthesis %>%
+ mutate(onset = case_when(Token %in% c("1", "3", "4", "6", "8", "9") ~ "ts", Token %in% c("2", "5", "7", "10") ~ "dz", Token %in% c("11", "13", "14", "34") ~ "tc", Token %in% c("12", "15") ~ "dj", Token %in% c("16", "42") ~ "f", Token %in% c("17", "18", "19", "20", "21", "22", "25", "26", "29", "30", "31", "32", "33", "37", "38", "39", "40", "41") ~ "zero", Token %in% c("23") ~ "c", Token %in% c("24") ~ "tch", Token %in% c("27", "28", "136", "137", "138") ~ "b", Token %in% c("35") ~ "th", Token %in% c("36") ~ "p", Token %in% c("43") ~ "ph", Token %in% c("44") ~ "t", Token %in% c("45") ~ "k"))

#Filter the apical vowel "i1", and in single place, and make a new dataset
> czthesis.i1 = czthesis %>%
+ filter(Vowel %in% c("i155", "i152", "i135", "i1213", "i1232") & position == "single")
