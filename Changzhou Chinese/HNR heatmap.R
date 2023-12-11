#Leave hnr values only
> czthesis.i1.hnr = czthesis.i1 %>%
+ subset(select = -c(Filename:Intensity10, Pitch0:position, proportion:base, peak))

#Turn categorical into numerical
> colnames(czthesis.i1.hnr) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Tone", "onset")

#Make it into a long spreadsheet
> czthesis.i1.hnr.long <- gather(czthesis.i1.hnr, time, value, -Tone, -onset)

#Remove all rows with NA values 
czthesis.i1.hnr.long.new <- na.omit(czthesis.i1.hnr.long)

#Keep the name
czthesis.i1.hnr.long <- czthesis.i1.hnr.long.new

#Heatmap with ggplot2
> ggplot(czthesis.i1.hnr.long, aes(cate
gory, Tone, fill=value))+
+ scale_fill_gradient(low="blue", high="white") +
+ scale_x_discrete(limits = c("HNR0", "HNR1", "HNR2", "HNR3", "HNR4", "HNR5", "HNR6", "HNR7", "HNR8", "HNR9", "HNR10"))+
+ geom_tile()

#Separate each row
> ggplot(czthesis.i1.hnr.long, aes(category, Tone, fill=value))+
+ scale_fill_gradient(low="blue", high="white") +
+ scale_x_discrete(limits = c("HNR0", "HNR1", "HNR2", "HNR3", "HNR4", "HNR5", "HNR6", "HNR7", "HNR8", "HNR9", "HNR10"))+
+ geom_tile()+
+ facet_grid(Tone ~ ., scales = "free_y")
