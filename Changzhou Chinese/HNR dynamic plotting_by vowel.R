# Take apical vowel /i1/ as an example
# Load packages and read file

> hnr = hnr %>%
+ filter(Label == "single")

> hnr = hnr %>%
+ mutate(Tone = case_when(endsWith(Vowel, "55") ~ "T1", endsWith(Vowel, "213") ~ "T2", endsWith(Vowel, "35") ~ "T3", endsWith(Vowel, "52") ~ "T4", endsWith(Vowel, "232") ~ "T5"))

> hnr <- hnr %>%
+ mutate(onset = case_when(Token %in% c("1", "3", "4", "6", "8", "9") ~ "ts", Token %in% c("2", "5", "7", "10") ~ "dz", Token %in% c("11", "13", "14", "34") ~ "tc", Token %in% c("12", "15") ~ "dj", Token %in% c("16", "42") ~ "f", Token %in% c("17", "18", "19", "20", "21", "22", "25", "26", "29", "30", "31", "32", "33", "37", "38", "39", "40", "41") ~ "zero", Token %in% c("23") ~ "c", Token %in% c("24") ~ "tch", Token %in% c("27", "28", "136", "137", "138") ~ "b", Token %in% c("35") ~ "th", Token %in% c("36") ~ "p", Token %in% c("43") ~ "ph", Token %in% c("44") ~ "t", Token %in% c("45") ~ "k"))

> hnr.i1 = hnr %>%
+ filter(Vowel %in% c("i155", "i152", "i135", "i1213", "i1232"))

> hnr.i1 = hnr.i1 %>%
+ subset(select = -c(Filename:VowelDuration))
> colnames(hnr.i1) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Tone", "onset")
> hnr.i1.long <- gather(hnr.i1, time, value, -Tone, -onset)
> hnr.i1.long.new <- na.omit(hnr.i1.long)
> hnr.i1.long <- hnr.i1.long.new

> ggplot(hnr.i1.long, aes(time, value, color=Tone, group=interaction(Tone, onset)))+
+ geom_smooth()+
+ scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))+
+ facet_grid(~Tone)+
+ labs(y = "HNR", x = "Timepoint")
