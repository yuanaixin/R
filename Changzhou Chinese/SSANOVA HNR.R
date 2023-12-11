# Changzhou Chinese has five tones, so here I'm going to make 5 SSANOVA plots, one for each tone
# And for each tone, I compare three types of vowels by looking at the curves (apical, fricative, plain vowels)

# Install and load packages
> install.packages("gss")
> library(gss)
> library(ggplot2)

# Check data type of each column, convert character type of data into numeric
> sapply(hnr.new.t1.long, class)

# Here, column no.3 should be numeric
> hnr.new.t1.long[,3] <- sapply(hnr.new.t1.long[,3], as.numeric)
> hnr.new.t2.long[,3] <- sapply(hnr.new.t2.long[,3], as.numeric)
> hnr.new.t3.long[,3] <- sapply(hnr.new.t3.long[,3], as.numeric)
> hnr.new.t4.long[,3] <- sapply(hnr.new.t4.long[,3], as.numeric)
> hnr.new.t5.long[,3] <- sapply(hnr.new.t5.long[,3], as.numeric)

# Factor of the SSANOVA model is Type
> hnr.new.t1.long$Type <- factor(hnr.new.t1.long$Type)
> hnr.new.t2.long$Type <- factor(hnr.new.t2.long$Type)
> hnr.new.t3.long$Type <- factor(hnr.new.t3.long$Type)
> hnr.new.t4.long$Type <- factor(hnr.new.t4.long$Type)
> hnr.new.t5.long$Type <- factor(hnr.new.t5.long$Type)

# Run SSANOVA model, value is the y axis, which is HNR value, Type is the factor, time is timescale, I have 1~10 here, Type:time is interaction.
> hnr.t1.ss <- ssanova(value ~ Type + time + Type:time, data = hnr.new.t1.long)
> hnr.t2.ss <- ssanova(value ~ Type + time + Type:time, data = hnr.new.t2.long)
> hnr.t3.ss <- ssanova(value ~ Type + time + Type:time, data = hnr.new.t3.long)
> hnr.t4.ss <- ssanova(value ~ Type + time + Type:time, data = hnr.new.t4.long)
> hnr.t5.ss <- ssanova(value ~ Type + time + Type:time, data = hnr.new.t5.long)

# Create modeled contours, I have two because T1 and T4 contain only three types of vowels (grid1), while T2, 3, 5 contain four types of vowels (grid)
> grid <- expand.grid(time = seq(1,10, length = 100), Type = c("Apical", "Labial fricative", "Alveolar fricative", "Plain"))
> grid1 <- expand.grid(time = seq(1,10, length = 100), Type = c("Apical", "Alveolar fricative", "Plain"))

# Create columns for each tone (note to use different grids!)
> grid1$t1.Fit <- predict(hnr.t1.ss, newdata = grid1, se = T)$fit
> grid1$t1.SE <- predict(hnr.t1.ss, newdata = grid1, se = T)$se.fit

> grid$t2.Fit <- predict(hnr.t2.ss, newdata = grid, se = T)$fit
> grid$t2.SE <- predict(hnr.t2.ss, newdata = grid, se = T)$se.fit

> grid$t3.Fit <- predict(hnr.t3.ss, newdata = grid, se = T)$fit
> grid$t3.SE <- predict(hnr.t3.ss, newdata = grid, se = T)$se.fit

> grid1$t4.Fit <- predict(hnr.t4.ss, newdata = grid1, se = T)$fit
> grid1$t4.SE <- predict(hnr.t4.ss, newdata = grid1, se = T)$se.fit

> grid$t5.Fit <- predict(hnr.t5.ss, newdata = grid, se = T)$fit
> grid$t5.SE <- predict(hnr.t5.ss, newdata = grid, se = T)$se.fit

# Just a test plot, I want to see how does it look like for T2 contour
> ggplot(grid, aes(x = time, colour = Type, fill = Type))+
+ geom_line(aes(y = t2.Fit), lwd = 1)+
+ geom_ribbon(aes(ymax = t2.Fit + (1.96*t2.SE), ymin = t2.Fit - (1.96*t2.SE)), alpha = 0.2, colour = NA)+
+ xlab("Time")+
+ ylab("HNR(dB)")+
+ coord_cartesian(ylim = c(-1, 21)) #limit the scale

# Create datasets for each tone
> grid.t1 = subset(grid1, select = -c(3, 4))
> grid.t2 = subset(grid, select = -c(5, 6, 7, 8))
> grid.t3 = subset(grid, select = -c(3, 4, 7, 8))
> grid.t4 = subset(grid1, select = -c(5, 6))
> grid.t5 = subset(grid, select = -c(3, 4, 5, 6))

# Add a label to discriminate them from each other
> grid.t1$tonetype <- "T1"
> grid.t2$tonetype <- "T2"
> grid.t3$tonetype <- "T3"
> grid.t4$tonetype <- "T4"
> grid.t5$tonetype <- "T5"

# Keep the column names consistent because I'm going to stack them into a long one
> names(grid.t1) <- c("time", "Type", "Fit", "SE", "tonetype")
> names(grid.t2) <- c("time", "Type", "Fit", "SE", "tonetype")
> names(grid.t3) <- c("time", "Type", "Fit", "SE", "tonetype")
> names(grid.t4) <- c("time", "Type", "Fit", "SE", "tonetype")
> names(grid.t5) <- c("time", "Type", "Fit", "SE", "tonetype")

# Stack these datasets into a long one
> grid_long <- rbind(grid.t1, grid.t2, grid.t3, grid.t4, grid.t5)

# Here, I want to name the tones as what they actually are (no T4 in this language, so they will be T1, T2, T3, T5, T6)
# Replace T5 with T6
> grid_long <- grid_long %>%
+ mutate(tonetype = ifelse(tonetype == "T5", "T6", tonetype))

# Replace T4 with T5
> grid_long <- grid_long %>%
+ mutate(tonetype = ifelse(tonetype == "T4", "T5", tonetype))

# Plot them all 
> ggplot(grid_long, aes(x = time, colour = Type, fill = Type))+
+ geom_line(aes(y = Fit), lwd = 0.5)+
+ geom_ribbon(aes(ymax = Fit + (1.96*SE), ymin = Fit - (1.96*SE)), alpha = 0.2, colour = NA)+
+ xlab("Time scale(1-10)")+
+ ylab("HNR(dB)")+
+ facet_grid(~tonetype)+
+ theme_minimal()+
+ scale_fill_lancet()+
+ scale_colour_lancet()+
+ coord_cartesian(ylim = c(-1, 21))

---

# Next, I want to see the group and interaction effects for each tone and each vowel type 
# Create modeled contours, again, we have two sets because of the different types of vowels for different tones
> grid.effect <- expand.grid(time = seq(1,10, length = 100), Type = c("Apical", "Labial fricative", "Alveolar fricative", "Plain"))

> grid1.effect <- expand.grid(time = seq(1,10, length = 100), Type = c("Apical", "Alveolar fricative", "Plain"))

# Create columns for each tone (note to use different grids!)
> grid1.effect$t1.Fit <- predict(hnr.t1.ss, grid1, se = T, inc = c("Type", "Type:time"))$fit
> grid1.effect$t1.SE <- predict(hnr.t1.ss, grid1, se = T, inc = c("Type", "Type:time"))$se.fit
> grid.effect$t2.Fit <- predict(hnr.t2.ss, grid, se = T, inc = c("Type", "Type:time"))$fit
> grid.effect$t2.SE <- predict(hnr.t2.ss, grid, se = T, inc = c("Type", "Type:time"))$se.fit
> grid.effect$t3.Fit <- predict(hnr.t3.ss, grid, se = T, inc = c("Type", "Type:time"))$fit
> grid.effect$t3.SE <- predict(hnr.t3.ss, grid, se = T, inc = c("Type", "Type:time"))$se.fit
> grid.effect$t5.Fit <- predict(hnr.t5.ss, grid, se = T, inc = c("Type", "Type:time"))$fit
> grid.effect$t5.SE <- predict(hnr.t5.ss, grid, se = T, inc = c("Type", "Type:time"))$se.fit

# Test plotting 
> ggplot(grid1.effect, aes(x = time, colour = Type, fill = Type))+
+ geom_line(aes(y = t1.Fit), lwd = 1)+
+ geom_ribbon(aes(ymax = t1.Fit + (1.96*t1.SE), ymin = t1.Fit - (1.96*t1.SE)), alpha = 0.2, colour = NA)+
+ xlab("Time")+
+ ylab("Difference in dB")

# Create datasets for each tone
> grid.effect.t1 = subset(grid1.effect, select = -c(5, 6))
> grid.effect.t2 = subset(grid.effect, select = -c(5, 6, 7, 8))
> grid.effect.t3 = subset(grid.effect, select = -c(3, 4, 7, 8))
> grid.effect.t4 = subset(grid1.effect, select = -c(3, 4))
> grid.effect.t5 = subset(grid.effect, select = -c(3, 4, 5, 6))

# Add a label to discriminate them from each other
> grid.effect.t1$tonetype <- "T1"
> grid.effect.t2$tonetype <- "T2"
> grid.effect.t3$tonetype <- "T3"
> grid.effect.t4$tonetype <- "T4"
> grid.effect.t5$tonetype <- "T5"

# Keep the column names consistent because I'm going to stack them into a long one
> names(grid.effect.t1) <- c("time", "Type", "Fit", "SE", "tonetype")
> names(grid.effect.t2) <- c("time", "Type", "Fit", "SE", "tonetype")
> names(grid.effect.t3) <- c("time", "Type", "Fit", "SE", "tonetype")
> names(grid.effect.t4) <- c("time", "Type", "Fit", "SE", "tonetype")
> names(grid.effect.t5) <- c("time", "Type", "Fit", "SE", "tonetype")

# Combine them and make them a long dataset
> grid.effect.long <- rbind(grid.effect.t1, grid.effect.t2, grid.effect.t3, grid.effect.t4, grid.effect.t5)

# Re-name the tone labels so they look accurate in the plots
> grid.effect.long <- grid.effect.long %>%
+ mutate(tonetype = ifelse(tonetype == "T5", "T6", tonetype))

> grid.effect.long <- grid.effect.long %>%
+ mutate(tonetype = ifelse(tonetype == "T4", "T5", tonetype))

# Plot
> ggplot(grid.effect.long, aes(x = time, colour = Type, fill = Type))+
+ geom_line(aes(y = Fit), lwd = 1)+
+ geom_ribbon(aes(ymax = Fit + (1.96*SE), ymin = Fit - (1.96*SE)), alpha = 0.2, colour = NA)+
+ xlab("Time")+
+ ylab("Difference in dB")+
+ facet_grid(~tonetype)
