#Code for normal img_fs

formula = as.formula("label ~ markov1 + markov2 + markov3 + markov4 + markov5 +     
                     SDIntensityBG + IntensityDifference + gabormean_0_0 + gaborSD_0_0 + gabormean_0_1 + gaborSD_0_1 +
                      gabormean_0_2 + gaborSD_0_2 + gabormean_1_0 + gaborSD_1_0 + gabormean_1_1 + gaborSD_1_1 + 
                      gabormean_1_2 + gaborSD_1_2 + gabormean_2_0 + gaborSD_2_0 + gabormean_2_1 + gaborSD_2_1 + 
                      gabormean_2_2 + gaborSD_2_2 + gabormean_3_0 + gaborSD_3_0 + gabormean_3_1 + gaborSD_3_1 +
                      gabormean_3_2 + gaborSD_3_2 + Energy + Homogeneity + Entropy + 
                     thirdordermoment + Inversevariance + Sumaverage + Variance + Clustertendency + MaxProbability +
                     Circularity + Compactness + Eccentricity + Solidity + Extent + RadialDistanceSD + SecondMoment +
                     Area + ConvexArea + Perimeter + ConvexPerimeter + EquivDiameter + MajorAxisLength +
                     MinorAxisLength")
img_fs <- data[,5:69]


#############################################################

#Code for img_fs with combined gabor features

formula = as.formula("label ~ markov1 + markov2 + markov3 + markov4 + markov5 +     
                     SDIntensityBG + IntensityDifference + avg.gabor.mean + avg.gabor.SD + Energy + Homogeneity + Entropy + 
                     thirdordermoment + Inversevariance + Sumaverage + Variance + Clustertendency + MaxProbability +
                     Circularity + Compactness + Eccentricity + Solidity + Extent + RadialDistanceSD + SecondMoment +
                     Area + ConvexArea + Perimeter + ConvexPerimeter + EquivDiameter + MajorAxisLength +
                     MinorAxisLength")

gabor.features <- data[, 19:42]

avg.gabor.mean <- (gabor.features$gabormean_0_0 + gabor.features$gabormean_0_1 + gabor.features$gabormean_0_2 +
                     gabor.features$gabormean_1_0 + gabor.features$gabormean_1_1 + gabor.features$gabormean_1_2 +
                     gabor.features$gabormean_2_0 + gabor.features$gabormean_2_1 + gabor.features$gabormean_2_2 +
                     gabor.features$gabormean_3_0 + gabor.features$gabormean_3_1 + gabor.features$gabormean_3_2)/12

avg.gabor.SD <- (gabor.features$gaborSD_0_0 + gabor.features$gaborSD_0_1 + gabor.features$gaborSD_0_2 
                 + gabor.features$gaborSD_1_0 + gabor.features$gaborSD_1_1 + gabor.features$gaborSD_1_2
                 + gabor.features$gaborSD_2_0 + gabor.features$gaborSD_2_1 + gabor.features$gaborSD_2_2
                 + gabor.features$gaborSD_3_0 + gabor.features$gaborSD_3_1 + gabor.features$gaborSD_3_2)/12

avg.gabor.features <- data.frame(avg.gabor.mean, avg.gabor.SD)

avg.gab.data <- data[, -(seq(from = 19, to = 42))]
avg.gab.data <- data.frame(avg.gab.data, avg.gabor.features)

img_fs <- data[,5:45]
img_fs <- data.frame(img_fs, avg.gabor.features)
