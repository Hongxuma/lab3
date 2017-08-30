library(dplyr)
library(tibble)
library(ggplot2)

setwd("/Users/Hongxu/Dropbox/Code/STAT_215A_git/stat215a/Lab\ 0")
data("USArrests")
statecoord = read.table("stateCoord.txt")
statecoord = rownames_to_column(statecoord)
USArrests = rownames_to_column(USArrests)

statecoord$rowname <- sub("-", " ", statecoord$rowname)
Combine <- full_join(USArrests,statecoord,by = "rowname", copy = TRUE)

#Visualizing the Data
#plot Murder vs Assault
p <- ggplot(Combine, aes(Murder, Assault))
p + geom_point()

#plot Rape vs urban population
p2 <- ggplot(Combine, aes(UrbanPop, Rape))
p2 + geom_point()
OutlierRatio <- Combine[5] / Combine[4]
OutlierMean <- mean(OutlierRatio$Rape, na.rm = TRUE)
OutlierSD <- sqrt(var(OutlierRatio))[1,1]
Outlier <- OutlierRatio > OutlierMean + 3 * OutlierSD
colnames(Outlier) <- c("RapeOutlier")
Combine <- cbind(Combine, Outlier)
p3 <- ggplot(Combine, aes(UrbanPop, Rape))
p3 + geom_point(col = ifelse(Combine["RapeOutlier"] == TRUE, 'red','black'))

#Re-mark plots with state names
p4 <- ggplot(Combine, aes(UrbanPop, Rape, label = rowname))
p4 + geom_text()

#Plot a map
library(fiftystater)

data("fifty_states") 
data("USArrests")
crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
p5 <- ggplot(crimes, aes(map_id = state)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = Murder), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())

p5 + fifty_states_inset_boxes() 


#Regression
# fit a linear regression of UrbanPop on Rape
fit1 <- lm(UrbanPop ~ Rape, data=Combine)

#plot predicted values vs residuals
FitNum <- cbind(fitted(fit1), residuals(fit1))
colnames(FitNum) <- c("predict","residuals")
Combine <- cbind(Combine, FitNum)
p6 <- ggplot(Combine, aes(predict,residuals))
p6 +geom_point()

#Replot Rape vs UrbanPop and draw a blue line with the predicted responses
p7 <- ggplot(Combine, aes(UrbanPop, Rape))+
  geom_smooth(method = "lm", se=FALSE, color="blue", formula = y ~ x,show.legend = TRUE) +
  geom_point(color = "blue")
p7

#refit without the outlier

Combine2 <- filter(Combine, RapeOutlier == FALSE)
p8 <- p7 + geom_smooth(aes(UrbanPop,Rape), data = Combine2, method = "lm", se=FALSE, color="red", formula = y ~ x, show.legend = TRUE)

#publishable graph
p8 <- p8 + ggtitle("Linear Regression of UrbanPop on Rape")
p8 +  scale_color_manual(name="Regression Lines") + legend()
