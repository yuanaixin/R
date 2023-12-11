# Take T1 as an example
# Load packages and read file

> hnr.new <- hnr.new %>%
+ mutate(Type = case_when(startsWith(Vowel, "iz") ~ "Alveolar fricative", startsWith(Vowel, "yz") ~ "Alveolar fricative", startsWith(Vowel, "i1") ~ "Apical", startsWith(Vowel, "u1") ~ "Apical", startsWith(Vowel, "ub") ~ "Labial fricative", startsWith(Vowel, "uv") ~ "Labial fricative", startsWith(Vowel, "i55") ~ "Plain", startsWith(Vowel, "i35") ~ "Plain", startsWith(Vowel, "i52") ~ "Plain", startsWith(Vowel, "i213") ~ "Plain", startsWith(Vowel, "i232") ~ "Plain", startsWith(Vowel, "u55") ~ "Plain", startsWith(Vowel, "u35") ~ "Plain", startsWith(Vowel, "u52") ~ "Plain", startsWith(Vowel, "u213") ~ "Plain", startsWith(Vowel, "u232") ~ "Plain"))

> hnr.new.t1 = hnr.new %>%
+ filter(Tone == "T1")
> hnr.new.t1 = hnr.new.t1 %>%
+ subset(select = -c(Filename:Duration.ms., VowelDuration, Tone:onset))

> colnames(hnr.new.t1) <- c("Vowel", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Type")

> hnr.new.t1.long <- gather(hnr.new.t1, time, value, -Vowel, -Type)
> hnr.new.t1.long.1 <- na.omit(hnr.new.t1.long)
> hnr.new.t1.long <- hnr.new.t1.long.1

> ggplot(hnr.new.t1.long, aes(time, value, color=Type, group=interaction(Vowel)))+
+ geom_smooth(alpha=0.2)+
+ scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))+
+ labs(y = "HNR", x = "Timepoint")
+ coord_cartesian(ylim = c(-1, 21))
