# independent variables (Factors):Type, Tone
# dependent variables: max HNR, mean HNR

# Factors
> hnr.new$Type <- factor(hnr.new$Type)
> hnr.new$Tone <- factor(hnr.new$Tone)

# HNR max 2-way ANOVA
> hnr.max.aov2 <- aov(max ~ Tone*Type, data = hnr.new)
> summary(hnr.max.aov2)
              Df Sum Sq Mean Sq F value  Pr(>F)    
Tone           4  11514  2878.4 137.654 < 2e-16 ***
Type           3   4255  1418.3  67.829 < 2e-16 ***
Tone:Type     10    824    82.4   3.939 2.6e-05 ***
Residuals   1344  28103    20.9                    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Visualising
> ggplot(hnr.new, aes(Tone, max, color = Type))+
+ geom_boxplot()+
+ scale_x_discrete(labels = c("T4" = "T5", "T5" = "T6"))+
+ labs(y = "Max HNR (dB)", x = "Tonal category")

#Post-hoc test
> TukeyHSD(hnr.max.aov2)
  Tukey multiple comparisons of means
    95% family-wise confidence level
Fit: aov(formula = max ~ Tone * Type, data = hnr.new)
$Tone
            diff        lwr         upr     p adj
T2-T1 -5.3883906 -6.3346833 -4.44209784 0.0000000
T3-T1 -4.9432973 -5.9530319 -3.93356283 0.0000000
T4-T1 -6.6027430 -7.6212416 -5.58424450 0.0000000
T5-T1 -6.7237817 -7.9129746 -5.53458875 0.0000000
T3-T2  0.4450932 -0.7158191  1.60600550 0.8332107
T4-T2 -1.2143525 -2.3828955 -0.04580944 0.0370606
T5-T2 -1.3353911 -2.6553659 -0.01541636 0.0457964
T4-T3 -1.6594457 -2.8799320 -0.43895943 0.0019807
T5-T3 -1.7804843 -3.1466568 -0.41431185 0.0035214
T5-T4 -0.1210386 -1.4937013  1.25162401 0.9992568
$Type
                                         diff        lwr
Apical-Alveolar fricative           1.9113658  1.0207675
Labial fricative-Alveolar fricative 3.4541746  2.1766460
Plain-Alveolar fricative            4.0910962  3.3318411
Labial fricative-Apical             1.5428088  0.2025216
Plain-Apical                        2.1797304  1.3190568
Plain-Labial fricative              0.6369216 -0.6199289
                                         upr     p adj
Apical-Alveolar fricative           2.801964 0.0000002
Labial fricative-Alveolar fricative 4.731703 0.0000000
Plain-Alveolar fricative            4.850351 0.0000000
Labial fricative-Apical             2.883096 0.0164659
Plain-Apical                        3.040404 0.0000000
Plain-Labial fricative              1.893772 0.5608152
…

#Save tukey results to a .csv file
> hnr.max.tukey <- TukeyHSD(hnr.max.aov2)
> hnr.max.tukey.data <- as.data.frame(hnr.max.tukey[1])
> write.csv(hnr.max.tukey.data, "hnr.max.tukey.data1.csv")
> hnr.max.tukey.data <- as.data.frame(hnr.max.tukey[2])
> write.csv(hnr.max.tukey.data, "hnr.max.tukey.data2.csv")
> hnr.max.tukey.data <- as.data.frame(hnr.max.tukey[3])
> write.csv(hnr.max.tukey.data, "hnr.max.tukey.data3.csv")

--------------

#HNR mean 2-way ANOVA
> hnr.mean.aov2 <- aov(means ~ Tone*Type, data = hnr.new)
> summary(hnr.mean.aov2)
              Df Sum Sq Mean Sq F value   Pr(>F)    
Tone           4  14208    3552 256.822  < 2e-16 ***
Type           3   1522     507  36.688  < 2e-16 ***
Tone:Type     10    591      59   4.275 6.89e-06 ***
Residuals   1344  18589      14                     
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Visualising
> ggplot(hnr.new, aes(Tone, means, color = Type))+
+ geom_boxplot()+
+ scale_x_discrete(labels = c("T4" = "T5", "T5" = "T6"))+
+ labs(y = "Mean HNR (dB)", x = "Tonal category")

#Post-hoc test
> TukeyHSD(hnr.mean.aov2)
  Tukey multiple comparisons of means
    95% family-wise confidence level
Fit: aov(formula = means ~ Tone * Type, data = hnr.new)
$Tone
            diff        lwr        upr     p adj
T2-T1 -5.8205767 -6.5901877 -5.0509657 0.0000000
T3-T1 -4.0560804 -4.8772880 -3.2348728 0.0000000
T4-T1 -8.2086943 -9.0370296 -7.3803590 0.0000000
T5-T1 -6.7694460 -7.7366055 -5.8022865 0.0000000
T3-T2  1.7644963  0.8203372  2.7086554 0.0000038
T4-T2 -2.3881176 -3.3384827 -1.4377525 0.0000000
T5-T2 -0.9488693 -2.0223924  0.1246538 0.1120562
T4-T3 -4.1526139 -5.1452240 -3.1600038 0.0000000
T5-T3 -2.7133656 -3.8244609 -1.6022703 0.0000000
T5-T4  1.4392483  0.3228746  2.5556220 0.0040483
$Type
                                         diff        lwr
Apical-Alveolar fricative           1.0264774  0.3021621
Labial fricative-Alveolar fricative 1.7159718  0.6769698
Plain-Alveolar fricative            2.4672132  1.8497180
Labial fricative-Apical             0.6894944 -0.4005487
Plain-Apical                        1.4407358  0.7407579
Plain-Labial fricative              0.7512413 -0.2709434
                                         upr     p adj
Apical-Alveolar fricative           1.750793 0.0015781
Labial fricative-Alveolar fricative 2.754974 0.0001352
Plain-Alveolar fricative            3.084708 0.0000000
Labial fricative-Apical             1.779538 0.3636589
Plain-Apical                        2.140714 0.0000008
Plain-Labial fricative              1.773426 0.2325823
…

#Save tukey results to a csv file
> hnr.mean.tukey <- TukeyHSD(hnr.mean.aov2)
> hnr.mean.tukey.data <- as.data.frame(hnr.mean.tukey[1])
> write.csv(hnr.mean.tukey.data, "hnr.mean.tukey.data1.csv")
> hnr.mean.tukey.data <- as.data.frame(hnr.mean.tukey[2])
> write.csv(hnr.mean.tukey.data, "hnr.mean.tukey.data2.csv")
> hnr.mean.tukey.data <- as.data.frame(hnr.mean.tukey[3])
> write.csv(hnr.mean.tukey.data, "hnr.mean.tukey.data3.csv")
