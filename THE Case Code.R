install.packages("tidyverse")
library(tidyverse)

install.packages("dplyr")
library(dplyr)


dat=WORKMarket_Daily
mdat=WORKMarket_Class_Daily
#_______________________________________
dat$Near_Event = as.factor(dat$Near_Event)


levels(dat$Season)
dat$Season = as.factor(dat$Season)
dat$Season = relevel(dat$Season, ref = "Summer")

dat$Event_This_Date = factor(dat$Event_This_Date, levels = c(0, 1))
dat$Near_Event = factor(dat$Near_Event, levels = c(-1,0, 1,2,3,4,5))

levels(dat$Event_This_Date)


sapply(atl, function(x) length(unique(x)))


dat$Festival = as.factor(dat$Festival)
levels(dat$Festival)

dat$RDate[2023,]

dat$Festival


levels(dat$Near_Event)
lax$Near_Event = as.factor(lax$Near_Event)
#__________________________________________________________________
#__________________________________________________________________
mdat$Near_Event = as.factor(mdat$Near_Event)


levels(mdat$Season)
mdat$Season = as.factor(mdat$Season)
mdat$Season = relevel(mdat$Season, ref = "Summer")

mdat$Event_This_Date = factor(mdat$Event_This_Date, levels = c(0, 1))
mdat$Near_Event = factor(mdat$Near_Event, levels = c(-1,0, 1,2,3,4,5))

levels(mdat$Event_This_Date)


sapply(matl, function(x) length(unique(x)))


mdat$Festival = as.factor(mdat$Festival)
levels(mdat$Festival)

mdat$RDate[2023,]

mdat$Festival


levels(mdat$Near_Event)

#__________________________________________________________________



#Formating data Per city 
atl = subset(dat, Markets_Market == "Atlanta, GA")
View(atl)

chi = subset(dat, Markets_Market == "Chicago, IL")
View(chi)

veg = subset(dat, Markets_Market == "Las Vegas, NV")
View(veg)

lax = subset(dat, Markets_Market == "Los Angeles, CA")
View(lax)

nyc = subset(dat, Markets_Market == "New York, NY")
View(nyc)
#-----Other data set ---

matl = subset(mdat, Markets_Market == "Atlanta, GA")
View(matl)

mchi = subset(mdat, Markets_Market == "Chicago, IL")
View(mchi)

mveg = subset(mdat, Markets_Market == "Las Vegas, NV")
View(mveg)

mlax = subset(mdat, Markets_Market == "Los Angeles, CA")
View(mlax)

mnyc = subset(mdat, Markets_Market == "New York, NY")
View(mnyc)


dat$Event_This_Date = as.factor(dat$Event_This_Date)
dat$Near_Event[is.na(dat$Near_Event)] = -1
levels(dat$Near_Event)

dat$Festival = as.factor(dat$Festival)
levels(dat$Festival)

levels(dat$Near_Event)
lax$Near_Event = as.factor(lax$Near_Event)


mod2=lm(Rooms_Demand~Season+RDate+Event_This_Date+Day_Avg_Attendance+XEvent_in5Days+Near_Event+Day_Of_Week+`Artist/Event`,data = atl)
summary(mod2)



#THNAKS GIVING 
moddate=lm(Rooms_Demand~RDate,data = dat)
summary(moddate)


moddate=lm(Rooms_Demand~dat$RDate,data = dat)




#DEMAND

#ALL CITY 
mod1=lm(Rooms_Demand~Season+Event_This_Date+Day_Avg_Attendance+XEvent_in5Days+Near_Event+Day_Of_Week+`Artist/Event`,data = dat)
summary(mod1)


Demand_all_city_sig=lm(Rooms_Demand~Season+Event_This_Date+XEvent_in5Days+Day_Of_Week,data = dat)
summary(Demand_all_city_sig)

Demand_all_city_sig=lm(Rooms_Demand~Near_Event+Season+XEvent_in5Days+Day_Of_Week,data = dat)
summary(Demand_all_city_sig)
#all sig for all cites 


haDemand_all_city_sig=lm(Rooms_Demand~Near_Event+Season+XEvent_in5Days+Day_Of_Week+dat$Festival,data = dat)
summary(haDemand_all_city_sig)

Demand_all_city_sig_event=lm(Rooms_Demand~Near_Event+XEvent_in5Days,data = dat)
summary(Demand_all_city_sig_event)


#DEMAND ATL
atl_dem=lm(Rooms_Demand~Season+Event_This_Date+Day_Avg_Attendance+XEvent_in5Days+Near_Event+Day_Of_Week+data = atl)
summary(atl_dem)

#DEMAND CHI
chi_dem=lm(Rooms_Demand~Season+Event_This_Date+Day_Avg_Attendance+XEvent_in5Days+Near_Event+Day_Of_Week,data = chi)
summary(chi_dem)

#DEMAND LAX
lax_dem=lm(Rooms_Demand~Season+Event_This_Date+Day_Avg_Attendance+XEvent_in5Days+Near_Event+Day_Of_Week,data = lax)
summary(lax_dem)

#DEMAND VEG
veg_dem=lm(Rooms_Demand~Season+Event_This_Date+Day_Avg_Attendance+XEvent_in5Days+Near_Event+Day_Of_Week,data = veg)
summary(veg_dem)

#DEMAND NYC
nyc_dem=lm(Rooms_Demand~Season+Event_This_Date+Day_Avg_Attendance+XEvent_in5Days+Near_Event+Day_Of_Week,data = nyc)
summary(nyc_dem)

bla=lm(Rooms_Demand~Season+Event_This_Date+Day_Avg_Attendance+XEvent_in5Days+Near_Event+Day_Of_Week,data = nyc)
summary(bla)






#REV
revlm=lm(dat$Rooms_Revenue~Season+Event_This_Date+XEvent_in5Days+Near_Event+Day_Of_Week,data = dat)
summary(revlm)


#Revenue ATL
atl_Rev=lm(Rooms_Revenue~Season+Event_This_Date+Day_Avg_Attendance+XEvent_in5Days+Near_Event+Day_Of_Week,data = atl)
summary(atl_Rev)

#Revenue CHI
chi_Rev=lm(Rooms_Revenue~Season+Event_This_Date+Day_Avg_Attendance+XEvent_in5Days+Near_Event+Day_Of_Week,data = chi)
summary(chi_Rev)

#Revenue LAX
lax_Rev=lm(Rooms_Revenue~Season+Event_This_Date+Day_Avg_Attendance+XEvent_in5Days+Near_Event+Day_Of_Week,data = lax)
summary(lax_Rev)

#Revenue VEG
veg_Rev=lm(Rooms_Revenue~Season+Event_This_Date+Day_Avg_Attendance+XEvent_in5Days+Near_Event+Day_Of_Week,data = veg)
summary(veg_Rev)

#Revenue NYC
nyc_Rev=lm(Rooms_Revenue~Season+Event_This_Date+Day_Avg_Attendance+XEvent_in5Days+Near_Event+Day_Of_Week,data = nyc)
summary(nyc_Rev)


yy=lm(formula = dat$Rooms_Revenue ~ Season + Event_This_Date + XEvent_in5Days + 
     Near_Event + Day_Of_Week, data = dat)
summary(yy)


#INTERACTION!!!
model_both_interactions = lm(Rooms_Revenue ~ Season * Event_This_Date + Day_Of_Week * Event_This_Date + Near_Event + XEvent_in5Days, data = dat)
summary(model_both_interactions)

demodel_both_interactions = lm(Rooms_Demand ~ Season * Event_This_Date + Day_Of_Week * Event_This_Date + Near_Event + XEvent_in5Days, data = dat)
summary(demodel_both_interactions)



#ADR

adrmod=lm(dat$Rooms_ADR~Season+Event_This_Date+XEvent_in5Days+Near_Event+Day_Of_Week,data = dat)
summary(adrmod)

adrmonract = lm(dat$Rooms_ADR ~ Season * Event_This_Date + Day_Of_Week * Event_This_Date + Near_Event + XEvent_in5Days, data = dat)
summary(adrmonract)

mean(dat$Rooms_ADR)


vegadrmod=lm(Rooms_ADR~Season+Event_This_Date+XEvent_in5Days+Near_Event+Day_Of_Week,data = veg)
summary(vegadrmod)



#OPC RATE 
ocpmod=lm(Occupancy_Rate~Season+Event_This_Date+XEvent_in5Days+Near_Event+Day_Of_Week,data = dat)
summary(ocpmod)



max(dat$Rooms_ADR)

ocpmod=lm(Occupancy_Rate~Event_This_Date+XEvent_in5Days+Near_Event,data = dat)
summary(ocpmod)







#CITY VAR

install.packages("lme4")
library(lme4)


ppp=lmer(Rooms_Revenue ~ Event_This_Date + (1 + Event_This_Date | Markets_Market), data = dat)
summary(ppp)

ranef(ppp)$Markets_Market 

fixef(ppp)


fe = fixef(ppp)


re = ranef(ppp)$Markets_Market


cityeffects = sweep(re, 2, fe, FUN = "+")
cityeffects


confint(ppp)

#00000



dat$Near_Event_Collapsed= ifelse(dat$Near_Event == 0, 0, ifelse(dat$Near_Event == -1, -1, 2))
dat$Near_Event_Collapsed= factor(dat$Near_Event_Collapsed)





qqq=lmer(Rooms_Revenue ~ Near_Event_Collapsed + (1 + Near_Event_Collapsed | Markets_Market), data = dat)
summary(qqq)

ranef(qqq)$Markets_Market 

fixef(qqq)


fe = fixef(qqq)


re = ranef(qqq)$Markets_Market


qcityeffects = sweep(re, 2, fe, FUN = "+")
qcityeffects


confint(qqq)


model1 = lmer(Rooms_Revenue ~ Event_This_Date + (1 | Markets_Market), data = dat)
model2 = lmer(Rooms_Revenue ~ Event_This_Date + (1 + Event_This_Date | Markets_Market), data = dat)

anova(model1, model2)

#==========================================================================================================
#==========================================================================================================
#============================================= MARKET DATA SETS ===========================================
#==========================================================================================================
#==========================================================================================================



mclassrv=lm(Rooms_Demand~Season+Event_This_Date+Day_Avg_Attendance+Day_Of_Week+`Artist/Event`+mdat$`Classes - Class`,data = mdat)
summary(mclassrv)

sigmclassrv=lm(Rooms_Demand~Season+Event_This_Date+Day_Avg_Attendance+Day_Of_Week+`Artist/Event`+mdat$`Classes - Class`,data = mdat)
summary(sigmclassrv)

hoo=lm(Rooms_Revenue~Season+Event_This_Date+Day_Avg_Attendance+Day_Of_Week+`Artist/Event`+mdat$`Classes - Class`,data = mdat)
summary(hoo)

hoorea=lm(Rooms_Revenue~Season+Event_This_Date+Day_Avg_Attendance+Day_Of_Week+`Artist/Event`+`Classes - Class`,data = mdat)
summary(hoorea)

jaja = lm(Rooms_Revenue ~ Event_This_Date * `Classes - Class`, data = mdat)
summary(jaja)


hoorea=lm(Rooms_Revenue~ Markets_Market * Event_This_Date * `Classes - Class` * Event_This_Date,data = mdat)
summary(hoorea)

library(lme4)

lmjaja = lmer(Rooms_Revenue ~ Event_This_Date * `Classes - Class` + (1 | Markets_Market), data = mdat)
summary(lmjaja)



ahh = lmer(Rooms_Revenue ~ Event_This_Date * `Classes - Class` + (1 | Markets_Market), data = mdat)
summary(ahh)

classadr=lm(Rooms_ADR~Season+Event_This_Date+Attendance+Day_Of_Week+`Classes - Class`,data = mdat)
summary(classadr)


classardract=lm(Rooms_ADR ~ Event_This_Date * `Classes - Class` + Season + Day_Of_Week + Attendance, data = mdat)
summary(classardract)

classardract=lm(Rooms_ADR ~ Season * Day_Of_Week * `Classes - Class` + Event_This_Date + Attendance, data = mdat)
summary(classardract)

lmercladr = lmer(Rooms_ADR ~ Event_This_Date * `Classes - Class` + (1 | Markets_Market), data = mdat)
summary(lmercladr)


#==========================================================================================================
#FESTT=====================================================================================================
#==========================================================================================================

fesclass=lm(Rooms_Demand~Season++Day_Avg_Attendance+`Classes - Class`+mdat$Festival,data = mdat)
summary(fesclass)

fesclass=lm(Rooms_ADR~Season+Day_Avg_Attendance+`Classes - Class`+Festival,data = mdat)
summary(fesclass)

#==========================================================================================================
#Season====================================================================================================
#==========================================================================================================

sea=lm( Rooms_Demand ~ Season * Event_This_Date + Day_Of_Week + `Classes - Class` + Markets_Market, data = mdat)
summary(sea)

wee=lm( Rooms_Demand ~ Day_Of_Week * Event_This_Date + Season + `Classes - Class` + Markets_Market, data = mdat)
summary(wee)



mean(dat$Rooms_Demand[dat$Day_Of_Week == "Monday"], na.rm = TRUE)
mean(dat$Rooms_Demand[dat$Day_Of_Week == "Tuesday"], na.rm = TRUE)
mean(dat$Rooms_Demand[dat$Day_Of_Week == "Wednesday"], na.rm = TRUE)
mean(dat$Rooms_Demand[dat$Day_Of_Week == "Thursday"], na.rm = TRUE)
mean(dat$Rooms_Demand[dat$Day_Of_Week == "Friday"], na.rm = TRUE)
mean(dat$Rooms_Demand[dat$Day_Of_Week == "Saturday"], na.rm = TRUE)
mean(dat$Rooms_Demand[dat$Day_Of_Week == "Sunday"], na.rm = TRUE)
