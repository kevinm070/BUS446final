---
title: "Final.BUS.446.DaBears"
output: html_document
date: "2024-04-20"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

library(readxl)
nascar_driver_statistics <- read_excel("School/nascar_driver_statistics.xlsx")
View(nascar_driver_statistics)
summary(nascar_driver_statistics)

wins = nascar_driver_statistics$Wins
avgstart = nascar_driver_statistics$AvgStart

c1 = abs(cor(wins, avgstart))
print(c1)

avgmidrace = nascar_driver_statistics$AvgMidRace
avgfinish = nascar_driver_statistics$AvgFinish
avgpos = nascar_driver_statistics$AvgPos
passdiff = nascar_driver_statistics$PassDiff
greenflagpasses = nascar_driver_statistics$GreenFlagPasses
greenflagpassed = nascar_driver_statistics$GreenFlagPassed
qualitypasses = nascar_driver_statistics$QualityPasses
percentqualitypasses = nascar_driver_statistics$PercentQualityPasses
numfastestlaps = nascar_driver_statistics$NumFastestLaps
lapsintop15 = nascar_driver_statistics$LapsInTop15
lapsled = nascar_driver_statistics$LapsLed
percentlapsled = nascar_driver_statistics$PercentLapsLed
totallaps = nascar_driver_statistics$TotalLaps
driverrating = nascar_driver_statistics$DriverRating
points = nascar_driver_statistics$Points

c2 = abs(cor(wins, avgmidrace))
c3 = abs(cor(wins, avgfinish))
c4 = abs(cor(wins, avgpos))
c5 = abs(cor(wins, passdiff))
c6 = abs(cor(wins, greenflagpasses))
c7 = abs(cor(wins, greenflagpassed))
c8 = abs(cor(wins, qualitypasses))
c9 = cor(wins, percentqualitypasses)

c10 = abs(cor(wins, numfastestlaps))
c11 = abs(cor(wins, lapsintop15))
c12 = abs(cor(wins, lapsled))
c13 = abs(cor(wins, percentlapsled))
c14 = abs(cor(wins, totallaps))
c15 = abs(cor(wins, driverrating))
c16 = abs(cor(wins, points))

categories = c("AvgStart", "AvgMidRace", "AvgFinish", "AvgPos", "PassDiff", "GreenFlagPasses", "GotPassed", "QualityPasses", "NumFastestLaps", "LapsInTop15", "LapsLed", "PercentLapsLed", "TotalLaps", "DriverRating", "Points")

values = c(c1, c2, c3, c4, c5, c6, c7, c8, c10, c11, c12, c13, c14, c15, c16)

df = data.frame(categories = c("AvgStart", "AvgMidRace", "AvgFinish", "AvgPos", "PassDiff", "GreenFlagPasses", "GotPassed", "QualityPasses", "NumFastestLaps", "LapsInTop15", "LapsLed", "PercentLapsLed", "TotalLaps", "DriverRating", "Points"),
values = c(c1, c2, c3, c4, c5, c6, c7, c8, c10, c11, c12, c13, c14, c15, c16))

print(values)

library(ggplot2)

ggplot(df, aes(x = categories, y = values)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Correlations to Win",
    y = "Values",
    x = "Stat") +
  theme_minimal()
  
sorted_df = df[order(df$values),]
print(sorted_df)

df2 = data.frame(start = nascar_driver_statistics$AvgStart,
win = nascar_driver_statistics$Wins)

sorteddf2 = df2[rev(order(df2$win)),]
print(sorteddf2)

shortdf2 = sorteddf2[1:200, ]
print(shortdf2)

ggplot(data = shortdf2, aes(x=start, y=win)) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Start vs Win + LoBF",
    x= "Start",
    y = "Win")+
  theme_minimal()


cor(lapsled, avgfinish)

df3 = data.frame(laps = nascar_driver_statistics$LapsLed,
finish = nascar_driver_statistics$AvgFinish)

sortdf3 = df3[rev(order(df3$laps)), ]
shortdf3 = sortdf3[1:200, ]
print(shortdf3)

ggplot(data = shortdf3, aes(x=laps, y=finish)) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "LapsLed vs Finish + LoBF",
    x= "LapsLed",
    y = "AvgFinish")+
  theme_minimal()
