meanLabel = (data["Malignancy_1"]+data["Malignancy_2"]+data["Malignancy_3"]+data["Malignancy_4"])/4
mvar = ((data["Malignancy_1"]-meanLabel)^2+(data["Malignancy_2"]-meanLabel)^2+
  (data["Malignancy_3"]-meanLabel)^2+(data["Malignancy_4"]-meanLabel)^2)/4
