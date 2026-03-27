data(CO2)
View(CO2)
#1
sort(CO2$uptake, decreasing = TRUE)
#2
CO2[CO2$Type == "Quebec" & CO2$Treatment == "chilled", ]
#3 
CO2[order(CO2$conc[CO2$uptake > 40]), ][CO2$uptake > 40, ]
#4
CO2[order(runif(nrow(CO2))), ]


set.seed(123)
missCO2 <- CO2
missCO2[c(as.integer(runif(10)*nrow(missCO2)),14:16),"uptake"] <- NA
missCO2[c(as.integer(runif(10)*nrow(missCO2)),14:16),"conc"] <- NA
missCO2$weight <- paste0(as.integer(runif(nrow(missCO2))*30),"kg")

#5
missCO2[!complete.cases(missCO2), ]
#6   
missCO2$uptake[is.na(missCO2$uptake)] <- 20 
#7
missCO2$conc[is.na(missCO2$conc)] <- mean(missCO2$conc, na.rm = TRUE)
#8
missCO2$weightNumber <- as.integer(sub("kg$", "", missCO2$weight))