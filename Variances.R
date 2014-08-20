load("bestorder.Rda")

meanLabel = (data["Malignancy_1"]+data["Malignancy_2"]+data["Malignancy_3"]+data["Malignancy_4"])/4
mvar = ((data["Malignancy_1"]-meanLabel)^2+(data["Malignancy_2"]-meanLabel)^2+
  (data["Malignancy_3"]-meanLabel)^2+(data["Malignancy_4"]-meanLabel)^2)/4

boxplot(mvar[which(best["I3.Label.Num"]==1)], mvar[which(best["I3.Label.Num"]==2)],
        mvar[which(best["I3.Label.Num"]==3)], mvar[which(best["I3.Label.Num"]==4)],
        xlab = "Total Labels Assigned", ylab = "All Label Variance", col = "orange",
        names = c("1", "2", "3", "4"), varwidth = TRUE)

mvar1 = (bestorder[which(best["I3.Label.Num"]==1),1]-
           (bestorder[which(best["I3.Label.Num"]==1),1]))^2
mean2 = (bestorder[which(best["I3.Label.Num"]==2),1]+
  bestorder[which(best["I3.Label.Num"]==2),2])/2
mvar2 = ((abs(bestorder[which(best["I3.Label.Num"]==2),1]-mean2)+
  abs(bestorder[which(best["I3.Label.Num"]==2),2]-mean2))^2)/2
mean3 = (bestorder[which(best["I3.Label.Num"]==3),1]+
           bestorder[which(best["I3.Label.Num"]==3),2]+
           bestorder[which(best["I3.Label.Num"]==3),3])/3
mvar3 = ((abs(bestorder[which(best["I3.Label.Num"]==3),1]-mean3)+
            abs(bestorder[which(best["I3.Label.Num"]==3),2]-mean3)+
            abs(bestorder[which(best["I3.Label.Num"]==3),3]-mean3))^2)/3
mean4 = (bestorder[which(best["I3.Label.Num"]==4),1]+
           bestorder[which(best["I3.Label.Num"]==4),2]+
           bestorder[which(best["I3.Label.Num"]==4),3]+
           bestorder[which(best["I3.Label.Num"]==4),4])/4
mvar4 = ((abs(bestorder[which(best["I3.Label.Num"]==4),1]-mean4)+
            abs(bestorder[which(best["I3.Label.Num"]==4),2]-mean4)+
            abs(bestorder[which(best["I3.Label.Num"]==4),3]-mean4)+
            abs(bestorder[which(best["I3.Label.Num"]==4),4]-mean4))^2)/4
boxplot(mvar1, mvar2, mvar3, mvar4, xlab = "Total Labels Assigned", names = c("1", "2", "3", "4"),
        ylab = "Assigned Label Variance", col = "orange", varwidth = TRUE)

#Explanation for why M3 and M4 have same accuracies
#notttt
lapply(allaccs, function(x) length(which(x["M3"]==x["M4"])))

#variance for jose model
jv1 = rep(0, 810)
jm2 = (bestorder[,1]+bestorder[,2])/2
jv2 = ((abs(bestorder[,1]-jm2)+
            abs(bestorder[,2]-jm2))^2)/2
jm3 = (bestorder[,1]+
           bestorder[,2]+
           bestorder[,3])/3
jv3 = ((abs(bestorder[,1]-jm3)+
            abs(bestorder[,2]-jm3)+
            abs(bestorder[,3]-jm3))^2)/3
jm4 = (bestorder[,1]+
           bestorder[,2]+
           bestorder[,3]+
           bestorder[,4])/4
jv4 = ((abs(bestorder[,1]-jm4)+
            abs(bestorder[,2]-jm4)+
            abs(bestorder[,3]-jm4)+
            abs(bestorder[,4]-jm4))^2)/4

boxplot(jv1, jv2, jv3, jv4, names = c("1", "2", "3", "4"),
        xlab = "Non-selective Iteration", ylab = "Assigned Label Variance", col = "orange",
        varwidth = TRUE)

sv1 = rep(0, 810)
sm2 = (best["I1.Label"]+ best["I2.Label"])/2
sm3 = (best["I1.Label"]+ best["I2.Label"]+ best["I3.Label"])/3
sm4 = (best["I1.Label"]+ best["I2.Label"]+ best["I3.Label"]+best["I4.Label"])/4
sv2 = ((abs(best["I1.Label"]-sm2)+abs(best["I2.Label"]-sm2))^2)/2
sv3 = ((abs(best["I1.Label"]-sm3)+abs(best["I2.Label"]-sm3)+abs(best["I3.Label"]-sm3))^2)/3
sv4 = ((abs(best["I1.Label"]-sm4)+abs(best["I2.Label"]-sm4)+abs(best["I3.Label"]-sm4)
       +abs(best["I4.Label"]-sm4))^2)/4

boxplot(sv1, sv2,sv3, sv4, jv4, names = c("1", "2", "3", "4", "all"),
        xlab = "Selective Iteration", ylab = "Assigned Label Variance", col = "orange",
        varwidth = TRUE)


