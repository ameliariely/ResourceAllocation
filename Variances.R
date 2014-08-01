meanLabel = (data["Malignancy_1"]+data["Malignancy_2"]+data["Malignancy_3"]+data["Malignancy_4"])/4
mvar = ((data["Malignancy_1"]-meanLabel)^2+(data["Malignancy_2"]-meanLabel)^2+
  (data["Malignancy_3"]-meanLabel)^2+(data["Malignancy_4"]-meanLabel)^2)/4

#Explanation for why M3 and M4 have same accuracies
lapply(allaccs, function(x) length(which(x["M3"]==x["M4"])))
