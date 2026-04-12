dat=TheCaseIIHalo
install.packages("ggplot2")
install.packages("lmerTest")
library("ggplot2")
library("tidyverse")
library("lubridate")
library("lmerTest")

summary(dat)
mod=lm(dat$RevPar~ dat$Season)
summary(mod)

#Season grouping-------------------------------------------------------------------------------
SznAvg=dat %>%
  group_by(Season) %>%
  summarise(RevPar=mean(RevPar, na.rm= TRUE))
ggplot(SznAvg,aes(x=Season,y=RevPar,group=1))+geom_line()+geom_point()
#Monthy Grouping-------------------------------------------------------------------------------
MonAvg=dat %>%
  group_by(Month, Season) %>%
  summarise(
    RevPar=mean(RevPar, na.rm= TRUE),
    Demand=mean(Demand, na.rm= TRUE),
             .groups="drop")

#Factor months as names not 1-12
MonAvg$Month = factor(
  MonAvg$Month,
  levels = 1:12,
  labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

#Line Grapth Monthy Avg RevPar by season
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

#ggsave("C:/Users/tarab/Documents/School/Sr Year/The Case II/Overall Seasonality of RevPar.png")


#Line Grapth Monthy Avg Deman by season
ggplot(MonAvg, aes(x = Month, y = Demand, group = 1, color = Season)) + geom_line(size=1.4,) +  geom_point(size=3) +
  scale_color_manual(values = c(
    "Winter" = "deepskyblue",
    "Spring" = "green",
    "Summer" = "gold",
    "Fall" = "chocolate")) +
  labs(
    title = "Overall Seasonality of Demand"
  )+ 
  theme_classic()
#ggsave("C:/Users/tarab/Documents/School/Sr Year/The Case II/Overall Seasonality of Demand.png")


#Line Grapth Monthy Avg Deman by season ZERO Y AXIS 
ggplot(MonAvg, aes(x = Month, y = Demand, group = 1, color = Season)) + geom_line(size=1.4,) +  geom_point(size=3) +
  scale_color_manual(values = c(
    "Winter" = "deepskyblue",
    "Spring" = "green",
    "Summer" = "gold",
    "Fall" = "chocolate")) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Overall Seasonality of Demand With Zeroed Axis"
  )+ 
  theme_classic()
#ggsave("C:/Users/tarab/Documents/School/Sr Year/The Case II/Overall Seasonality of Demand With Zeroed Axis.png")

#Location Monthly Grouping-------------------------------------------------------------------------------
LocMonAvg= dat %>%
  group_by(Month, Season, Location) %>%
  summarise(
    RevPar= mean(RevPar, na.rm= TRUE),
    Demand= mean(Demand, na.rm= TRUE),
    .groups = "drop")

#Factor months as names not 1-12

LocMonAvg$Month = factor(
  LocMonAvg$Month,
  levels = 1:12,
  labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  
#Line Grapth Monthy Avg RevPar by season and by location  
ggplot() + 
  geom_line(data = LocMonAvg,
            aes(x = Month, y = RevPar, color = Location, group = Location, alpha = 0),
            size = 2) +
  geom_point(data = LocMonAvg,
             aes(x = Month, y = RevPar, color = Location, group = Location, alpha = 1),
             size = 2) +
  geom_line(data = MonAvg,
            aes(x = Month, y = RevPar, color = "Average", group = 1, alpha = 1),
            size = 1) +
  geom_point(data = MonAvg,
             aes(x = Month, y = RevPar, color = "Average", alpha = 1),
             size = 2) +
  scale_color_manual(
    values = c(
      setNames(scales::hue_pal()(length(unique(LocMonAvg$Location))),
               unique(LocMonAvg$Location)),
      "Average" = "black"  )) +
  guides(alpha = "none") +
  theme_classic() +
  labs(
    title = "Average RevPAR by Month and Location",
    x = "Month",
    y = "RevPAR",
    color = "Legend")
#ggsave("C:/Users/tarab/Documents/School/Sr Year/The Case II/Average RevPAR by Month and Location.png")


#Line Grapth Monthy Avg Demand by season and by location 
ggplot() + 
  geom_line(data = LocMonAvg,
    aes(x = Month, y = Demand, color = Location, group = Location, alpha = 0),size = 2) +
  geom_point(data = LocMonAvg,
    aes(x = Month, y = Demand, color = Location, group = Location, alpha = 1),size = 2) +
  geom_line(data = MonAvg,
    aes(x = Month, y = Demand, color = "Average", group = 1, alpha = 1),size = 1) +
  geom_point(data = MonAvg,
    aes(x = Month, y = Demand, color = "Average", alpha = 1),size = 2) +
  scale_color_manual(
    values = c(setNames(scales::hue_pal()(length(unique(LocMonAvg$Location))),unique(LocMonAvg$Location)),"Average" = "black"  )) +
  guides(alpha = "none") +
  theme_classic() +
  labs(
    title = "Average Demand by Month and Location",
    x = "Month",
    y = "Demand",
    color = "Legend")

#Cites Baseline RevPar-------------------------------------------------------------------------------

#what is the baseline RevPAr per city 
dat %>%
  group_by(Location) %>%
  summarise(baseline = mean(RevPar, na.rm = TRUE))

#Are the Baselines stat sig from each other 
mod = aov(RevPar ~ Location, data = dat)
summary(mod)

#Which cities differ
TukeyHSD(mod)

#Cites Baseline Demand-------------------------------------
#what is the baseline RevPAr per city 
dat %>%
  group_by(Location) %>%
  summarise(baseline = mean(Demand, na.rm = TRUE))

#Are the Baselines stat sig from each other 
mod = aov(Demand ~ Location, data = dat)
summary(mod)

#Which cities differ
TukeyHSD(mod)

#Seasonality LME 1 ---------------------------------------

#Does RevPar have seasonality 
#Yes there is statically sig difference in most months
#Yesthere is statically sig difference day of the week 
#Yesthere is statically sig difference in year 
modlme = lmer(RevPar ~ factor(Month) + factor(DayOfWeek) + factor(Year) + (1 | Location), data = dat)
summary(modlme)


# Does change of all months change at the same rate 
ModYearMonDelta = lmer(RevPar ~ factor(Month) * factor(Year) + factor(DayOfWeek) + (1 | Location), data = dat)
summary(ModYearMonDelta)

dat %>%
  group_by(Year, Month) %>%
  summarise(avg_revpar = mean(RevPar, na.rm = TRUE), .groups = "drop") %>%
  mutate(Month = factor(Month,levels = 1:12,
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>%
  ggplot(aes(x = Month, y = avg_revpar, color = factor(Year), group = Year)) +
  geom_line() +
  geom_point() +
  theme_classic()+
  labs(
    title = "Year-over-Year RevPar Change by Month",
    x = "Month",
    y = "RevPar Delta",
    color = "Year" )

#REV PAR GROWTH BETWEEN YR AND MO-----------------------
datNo20n21 = dat %>%
  filter(Year != 2020,Year != 2021  )

ModYearMonDeltaNo20n21 = lmer(
  RevPar ~ factor(Month) * factor(Year) + factor(DayOfWeek) + (1 | Location),
  data = datNo20n21)
summary(ModYearMonDeltaNo20n21)

#Factor months as names not 1-12
datNo20n21$Month = factor(
  datNo20n21$Month,
  levels = 1:12,
  labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

dat %>%
  group_by(Year, Month) %>%
  summarise(avg_RevPar = mean(RevPar, na.rm = TRUE), .groups = "drop") %>%
  arrange(Month, Year) %>%
  group_by(Month) %>%
  mutate(yoy_delta = avg_RevPar - lag(avg_RevPar)) %>%
  ungroup() %>%
  filter(!is.na(yoy_delta)) %>%
  mutate(
    Month = factor(
      Month,
      levels = 1:12,
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") ))%>%
    ggplot(aes(x = Month, y = yoy_delta, color = factor(Year), group = Year)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(
    title = "Average Daily RevPar Change by Month and Year",
    x = "Month",
    y = "RevPar Delta",
    color = "Year")

# Graph Year-over-Year RevPar Growth by Month Excluding COVID
datNo20n21 %>%
  group_by(Year, Month) %>%
  summarise(avg_RevPar = mean(RevPar, na.rm = TRUE), .groups = "drop") %>%
  arrange(Month, Year) %>%
  group_by(Month) %>%
  mutate(yoy_delta = avg_RevPar - lag(avg_RevPar)) %>%
  ungroup() %>%
  filter(!is.na(yoy_delta)) %>%
  ggplot(aes(x = Month, y = yoy_delta, color = factor(Year), group = Year)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(
    title = "Average Daily RevPar Change by Month and Year Excluding COVID",
    x = "Month",
    y = "RevPar Delta",
    color = "Year"
  )
ggsave("C:/Users/tarab/Documents/School/Sr Year/The Case II/Average Daily RevPar Change by Month and Year Excluding COVID.png")


#is a mod w/ or wo interaction better
modno20n21wo = lmer(RevPar ~ factor(Month) + factor(Year) + factor(DayOfWeek) + (1 | Location), data = datNo20n21)
modno20n21w = lmer(RevPar ~ factor(Month) * factor(Year) + factor(DayOfWeek) + (1 | Location), data = datNo20n21)

#Month x Year interaction is Sig
#The effect of month changes by year, monthy growth is not the same across years
anova(modno20n21wo, modno20n21w)



#Month GROWTH DEMAND BETWEEN YR AND MO-----------------------
datNo20n21demand = dat %>%
  filter(Year != 2020,Year != 2021  )

ModYearMonDeltaNo20n21demand = lmer(
  Demand ~ factor(Month) * factor(Year) + factor(DayOfWeek) + (1 | Location),
  data = datNo20n21)
summary(ModYearMonDeltaNo20n21demand)

#Factor months as names not 1-12
datNo20n21demand$Month = factor(
  datNo20n21demand$Month,
  levels = 1:12,
  labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

#Grapth Year-over-Year Demand Change by Month Excluding COVID
datNo20n21demand %>%
  group_by(Year, Month) %>%
  summarise(avg_Demand = mean(Demand, na.rm = TRUE), .groups = "drop") %>%
  arrange(Month, Year) %>%
  group_by(Month) %>%
  mutate(yoy_delta = avg_Demand - lag(avg_Demand)) %>%
  ungroup() %>%
  filter(!is.na(yoy_delta)) %>%
  ggplot(aes(x = Month, y = yoy_delta, color = factor(Year), group = Year)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(
    title = "Average Daily Demand Change by Month and Year Excluding COVID",
    x = "Month",
    y = "Demand Delta",
    color = "Year")

#is a mod w/ or wo interaction better
modno20n21woDemand = lmer(Demand ~ factor(Month) + factor(Year) + factor(DayOfWeek) + (1 | Location), data = datNo20n21)
modno20n21wDemand = lmer(Demand ~ factor(Month) * factor(Year) + factor(DayOfWeek) + (1 | Location), data = datNo20n21)

#Month x Year interaction is Sig
#The effect of month changes by year, monthy growth is not the same across years
anova(modno20n21woDemand, modno20n21wDemand)


#Seasonality test of Revpar after controling for year, day of week, city, month is stat sig and imporoves model-----------------------

mod_season = lmer(
  RevPar ~ factor(Month) + factor(Year) + factor(DayOfWeek) + (1 | Location), data = dat, REML = FALSE)

mod_no_season = lmer(
  RevPar ~ factor(Year) + factor(DayOfWeek) + (1 | Location), data = dat, REML = FALSE)

anova(mod_no_season, mod_season)

#Does Seasonality in Revpar change across years w/o20/21-----------------------
mod_fixed_season = lmer(
  RevPar ~ factor(Month) + factor(Year) + factor(DayOfWeek) + (1 | Location),
  data = datNo20n21,
  REML = FALSE)

mod_varying_season = lmer(
  RevPar ~ factor(Month) * factor(Year) + factor(DayOfWeek) + (1 | Location),
  data = datNo20n21,
  REML = FALSE)

anova(mod_fixed_season, mod_varying_season)

#Seasonality test of DEMAND after controling for year, day of week, city, month is stat sig and imporoves model-----------------------

modSeasonDemand = lmer(
  Demand ~ factor(Month) + factor(Year) + factor(DayOfWeek) + (1 | Location), data = dat, REML = FALSE)

modNoSeasonDemand = lmer(
  Demand ~ factor(Year) + factor(DayOfWeek) + (1 | Location), data = dat, REML = FALSE)

anova(modNoSeasonDemand, modSeasonDemand)

#Does Seasonality in DEMAND change across years no 20/21-----------------------
DmodFixedSeason = lmer(
  Demand ~ factor(Month) + factor(Year) + factor(DayOfWeek) + (1 | Location),
  data = datNo20n21,
  REML = FALSE)

DmodVaryingSeason = lmer(
  Demand ~ factor(Month) * factor(Year) + factor(DayOfWeek) + (1 | Location),
  data = datNo20n21,
  REML = FALSE)

anova(DmodFixedSeason, DmodVaryingSeason)

#Friday in Jan 2020----------------------
lmer(RevPar ~ factor(Month) + factor(Year) + factor(DayOfWeek) + (1 | Location), data = dat)

month_effects = data.frame(
  Month = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  Estimate = c(0,31.6924, 24.8284, 7.9940, -1.4994, -8.9232,-9.8150, -19.4689, -11.5439, 5.8213, 1.3836, 0.3971))

year_effects = data.frame(Year = c(2020, 2021, 2022, 2023, 2024, 2025, 2026),
    Estimate = c(0,30.1433, 56.9604, 58.5213, 59.1271, 59.9275, 65.7461))

dayofweek_effects = data.frame(
  DayOfWeekef = c("Friday", "Monday", "Saturday", "Sunday", "Thursday", "Tuesday", "Wednesday"),
  Estimate = c(0,-32.3096, 7.2070, -33.2531, -20.9527, -26.3974, -23.7839))
  
#Event Effect per city RevPar------------------
#Friday in january, Arlington no event
modEventAvgRevPar=lmer(RevPar ~ Event  + factor(Month)+ factor(Year) + factor(DayOfWeek) + (1 | Location), data = dat)
summary(modEventAvgRevPar)
confint(modEventAvgRevPar, parm = "Event", method = "Wald")

lmModEventcityRevPar = lm(RevPar ~ Event * Location + factor(Month) + factor(Year) + factor(DayOfWeek),
  data = dat)
summary(lmModEventcityRevPar)

table(dat$Location, dat$Event)

EventEffectsCity = data.frame(
  Locationcity = c("Arlington", "Atlanta", "Miami", "NewOrleans", "Pasadena", "Phoenix"),
  EventEffect = c(
    65.76079,
    65.76079 - 74.60733,
    65.76079 + 87.31779,
    65.76079 + 11.82011,
    65.76079 + 77.14524,
    65.76079 - 73.23244))

EventEffectsCity

##Event Effect per city Demand ------------------
#Friday in january, Arlington no event

modEventAvgDemand=lmer(Demand ~ Event  + factor(Month)+ factor(Year) + factor(DayOfWeek) + (1 | Location), data = dat)
summary(modEventAvgDemand)
confint(modEventAvgDemand, parm = "Event", method = "Wald")

lmmodEventAvgDemand = lm(Demand ~ Event * Location + factor(Month) + factor(Year) + factor(DayOfWeek),
                           data = dat)
summary(lmmodEventAvgDemand)

#Event Effect per city OccRate------------------
#Friday in january, Arlington no event
modEventAvgOcc=lmer(OccupancyRate ~ Event  + factor(Month)+ factor(Year) + factor(DayOfWeek) + (1 | Location), data = dat)
summary(modEventAvgOcc)
confint(modEventAvgOcc, parm = "Event", method = "Wald")

lmModEventcityRevOcc = lm(OccupancyRate ~ Event * Location + factor(Month) + factor(Year) + factor(DayOfWeek),
                           data = dat)
summary(lmModEventcityRevOcc)

#Event Effect per city Supply------------------
#Friday in january, Arlington no event
modEventAvgSup=lmer(Supply ~ Event  + factor(Month)+ factor(Year) + factor(DayOfWeek) + (1 | Location), data = dat)
summary(modEventAvgSup)
confint(modEventAvgSup, parm = "Event", method = "Wald")

lmModEventcitySup = lm(Supply ~ Event * Location + factor(Month) + factor(Year) + factor(DayOfWeek),
                          data = dat)
summary(lmModEventcitySup)

#Halo Rev Par------------------
#Friday in january, Arlington no event

dat$HaloEvent = as.character(dat$Halo)
dat$HaloEvent[is.na(dat$HaloEvent) | dat$HaloEvent == ""] = "NoEvent"
dat$HaloEvent = factor(dat$HaloEvent)
dat$HaloEvent = relevel(dat$HaloEvent, ref = "NoEvent")


halomodEventAvgRevPar=lmer(RevPar ~ factor(HaloEvent)  + factor(Month)+ factor(Year) + factor(DayOfWeek) + (1 | Location), data = dat)
summary(halomodEventAvgRevPar)

HalolmModEventcityRevRevPar = lm(RevPar ~ HaloEvent * Location + factor(Month) + factor(Year) + factor(DayOfWeek),
                          data = dat)
summary(HalolmModEventcityRevRevPar)

#Plot

Rev_Par_halo = data.frame(
  HaloEvent = c(-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  Estimate = c(-19.240, -20.417, -24.124, -25.903, -9.745, 4.537, 7.209, 15.703, 37.739, 79.298,
               67.129, -3.243, -25.421, -31.547, -28.175, -25.278, -23.189, -24.350, -20.609,
               -15.584, -14.526),
  RevPar = c(-19.240, -20.417, -24.124, -25.903, -9.745, 4.537, 7.209, 15.703, 37.739, 79.298,
             67.129, -3.243, -25.421, -31.547, -28.175, -25.278, -23.189, -24.350, -20.609,
             -15.584, -14.526),
  Sig_color = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE,
                FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))

datlorevpar=Rev_Par_halo
datlorevpar$Sig_color = factor(
  datlorevpar$Sig_color,
  levels = c("TRUE", "FALSE"),
  labels = c("Not Statistically Significant", "Statistically Significant"))

ggplot(datlorevpar, aes(x = HaloEvent, y = RevPar, color = Sig_color)) + 
  geom_point(size = 4) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_line(aes(color = "RevPar")) +
  scale_color_manual(values = c(
    "Statistically Significant" = "green",
    "Not Statistically Significant" = "orange",
    "RevPar" = "black")) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  scale_y_continuous(breaks = seq(-20, 100, by = 10)) +
  labs(
    title = "The Halo Effect on RevPar",
    x = "Days from Event",
    y = "RevPar",
    color = "Significance" )

#Event Effect per city REV------------------
#Friday in january, Arlington no event
modEventAvgR=lmer(Revenue ~ factor(HaloEvent) + factor(Month)+ factor(Year) + factor(DayOfWeek) + (1 | Location), data = dat)
summary(modEventAvgR)
confint(modEventAvgR, parm = "Event", method = "Wald")

lmModEventcityR = lm(Revenue ~ Event * Location + factor(Month) + factor(Year) + factor(DayOfWeek),
                     data = dat)
summary(lmModEventcityR)


head(dat$HaloEvent)


sum(dat$Event == 1, na.rm = TRUE)

