#Leave pitch values only
> czthesis.i1.pitch = czthesis.i1 %>%
+ subset(select = -c(Filename:HNR10, H1.H2_1:position, proportion:base, peak))

#Make it into a long spreadsheet
> czthesis.i1.pitch.long <- gather(czthesis.i1.pitch, category, value, -Tone, -onset)

#Remove all rows with NA values 
czthesis.i1.pitch.long.new <- na.omit(czthesis.i1.pitch.long)

#Keep the name
czthesis.i1.pitch.long <- czthesis.i1.pitch.long.new

#Plotting
> ggplot(czthesis.i1.pitch.long, aes(category, value, color=Tone, group=interaction(Tone, onset)))+
+ geom_smooth()+
+ scale_x_discrete(limits = c("Pitch0", "Pitch1", "Pitch2", "Pitch3", "Pitch4", "Pitch5", "Pitch6", "Pitch7", "Pitch8", "Pitch9", "Pitch10"))
