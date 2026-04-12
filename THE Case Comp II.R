dat=TheCaseIIcsv
install.packages("ggplot2")
library("ggplot2")
library("tidyverse")
library("lubridate")

summary(dat)
mod=lm(dat$RevPar~ dat$Season)
summary(mod)

--------------------------------------------
SznAvg=dat %>%
  group_by(Season) %>%
  summarise(RevPar=mean(RevPar, na.rm= TRUE))
ggplot(SznAvg,aes(x=Season,y=RevPar,group=1))+geom_line()+geom_point()
-------------------------------------------------------------
MonAvg=dat %>%
  group_by(Month, Season) %>%
  summarise(RevPar=mean(RevPar, na.rm= TRUE), .groups="drop")

MonAvg$Month <- factor(
  MonAvg$Month,
  levels = 1:12,
  labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
)

ggplot(MonAvg, aes(x = Month, y = RevPar, group = 1, color = Season)) + geom_line(size=1.4,) +  geom_point(size=3) +
  scale_color_manual(values = c(
      "Winter" = "deepskyblue",
      "Spring" = "green",
      "Summer" = "gold",
      "Fall" = "chocolate")) +
  labs(
    title = "Overall Seasonality of RevPar"
  )+ 
  theme_classic()

ggsave("plot.pdf", width = 10, height = 6)
zz


LocMonAvg= dat %>%
  group_by(Month, Season, Location) %>%
  summarise(RevPar= mean(RevPar, na.rm= TRUE), .groups = "drop")

LocMonAvg$Month = factor(
  LocMonAvg$Month,
  levels = 1:12,
  labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
)
  
ggplot() + 
  geom_line(data = LocMonAvg,
            aes(x = Month, y = RevPar, color = Location, group = Location, alpha=0),
            size = 2) +
  geom_point(data = LocMonAvg,
             aes(x = Month, y = RevPar, color = Location, group = Location, alpha=1),
             size = 2) +
  geom_line(data = MonAvg,
            aes(x = Month, y = RevPar, group = 1),
            color = "black", size =1 ) +
  geom_point(data = MonAvg,
             aes(x = Month, y = RevPar),
             color = "black", size = 2) +
  theme_classic() +guides(alpha = "none")+
  labs(
    title = "Average RevPAR by Month and Location",
    x = "Month",
    y = "RevPAR"
  )
ggsave("Average RevPAR by Month and Location plot.pdf", width = 10, height = 6)
  
  
  
  
  
  
  
  

  
  -------------------------------------------------------
  dat$Date <- mdy(dat$Date)

WeekAvg = dat %>%
  mutate(Date = mdy(Date)) %>%
  mutate(WeekOfYear = isoweek(Date)) %>%
  group_by(WeekOfYear, Season, Month) %>%
  summarise(RevPar = mean(RevPar, na.rm = TRUE), .groups = "drop")

ggplot(WeekAvg, aes(x = WeekOfYear, y = RevPar, group = 1, color = Season)) + geom_line(size=1.5) +  geom_point(size=3) +
  scale_color_manual(values = c(
    "Winter" = "deepskyblue",
    "Spring" = "green",
    "Summer" = "gold",
    "Fall" = "chocolate")) + theme_classic()
