library(dplyr)
library(ggplot2)
library(GGally)
library(MASS)
library(broom)

bike <- read.table("./bike-data.txt") %>% tbl_df 
summary(bike)

############################################################
## Helper functions
###########################################################
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.6/strwidth(txt)
  #text(0.5, 0.5, txt, cex = cex * r)
  text(0.5, 0.5, txt, cex = cex)
}

# plotHistFunc adapted from code here
# http://kldavenport.com/creating-ggplot2-graphics-in-a-loop/
plotHistFunc <- function(data) {
  nm <- names(data)
  for (i in seq_along(nm)) {
    x <- unlist(data[i])
    bin <- min(length(unique(x))+1,13)
    hist(x, main = nm[i], breaks=bin, ylab = "", xlab="", right=F)
  }
}

plotBarFunc <- function() {
  dat <- cont_bar
  nm <- names(dat)
  for (i in seq_along(nm)) {
    x <- unlist(dat[i])
    barplot(table(x), main = nm[i], col="white")
  }
}

############################################################
## EDA
###########################################################


cont <- bike[c(1,14,10:13)]
cont_bar <- bike[c(4:6,9)]
disc <- bike[c(3:5,7:9)] 
disc2 <- apply(disc, 2, as.factor)

round(cor(cont),2)
bike %>% select(Registered, Year, Month, Day, Hour, Holiday, WorkDay) %>%
  GGally::ggpairs(axisLabels = "none", discrete="ratio")+ 
  theme_minimal()

bike %>% select(Registered, Year, Month, Day, Holiday, WorkDay, Weather) %>%
  GGally::ggpairs(axisLabels = "none")+ 
  theme_minimal()

ggpairs(disc, axisLabels = "none") + theme_minimal()
ggpairs(cbind(bike$Registered,disc), axisLabels = "none") + theme_minimal()
pairs(cbind(cont,bike$Hour), lower.panel = panel.cor)
apply(disc, 2, table)

# Registered, Casual, Temp, TempFeel, Humidity, Windspeed
par(mfrow=c(2,5))
plotHistFunc()

# Year, Month, Day, Holiday, Workday, Weather
par(mfrow=c(2,3))
plotBarFunc()

summary(lm(Registered~Casual:WorkDay, data=bike))

summary(lm(Registered~Year,data=bike))

###################

############
## Manipulate dataset
bike$Holiday <- as.factor(bike$Holiday)
bike$WorkDay <- as.factor(bike$WorkDay)
ifelse(bike$WorkDay == 1, "Work Day","Not Work Day") %>% 
  as.factor %>% relevel(ref = "Work Day")
bike$Hour <-  as.factor(bike$Hour) %>% relevel(ref = 3)
bike$Year <- as.factor(bike$Year) %>% relevel(ref = 2011)
ifelse(bike$Year == 2011, "2011", "2012")  
bike$Day <- as.factor(bike$Day)
bike$Month <- as.factor(bike$Month)
bike$Weather <- as.factor(bike$Weather)
bike$NewWeather <- ifelse(bike$Weather == 3, 1, 0) %>% 
  as.factor %>% relevel(ref = "Normal")
apply(bike, 2, bike[c("Year","Month","Day")])


###################
###################


# (1) - Registered VS Casual
attach(bike)
levels
summary(lm(Registered~Casual, data=bike))
ggplot(bike, aes(x = Casual, y = Registered)) +
  geom_point() + 
  xlab("Number of Casual Bikers") + ylab("Number of Registered Bikers") +
  ggtitle("Registered ~ Casual Bikers") +
  theme_minimal()  

#facet_wrap(~ WorkDay, labeller = as_labeller(c("1" = "Workday", "0" = "Non-Workday"))) + 

# by Weekend
bike$Weekend <- ifelse(bike$Day == 0 | bike$Day == 6, "Weekend","Weekday") 
ggplot(bike, aes(x = Hour, y = Registered)) +
  geom_point() + 
  facet_wrap(~ factor(Weekend, levels= c("Weekend","Weekday"))) + 
  xlab("Number of Casual Users") + ylab("Number of Registered Users") +
  ggtitle("Registered ~ Casual Users on Weekend") +
  theme_minimal()
# Casul Vs Registered
ggplot(bike, aes(x = Casual, y = Registered)) +
  geom_point() + 
  facet_wrap(~ WorkDay, labeller = as_labeller(c("1" = "Workday", "0" = "Non-Workday"))) + 
  xlab("Number of Casual Users") + ylab("Number of Registered Users") +
  ggtitle("Registered ~ Casual Users on Workday") +
  theme_minimal()

## Casual VS Registered by Workday
##
ggplot(bike, aes(x = Casual, y = Registered)) +
  geom_point() + geom_smooth(method = "lm",se = F) +
  facet_wrap(~ WorkDay) + 
  xlab("Number of Casual Users") + ylab("Number of Registered Users") +
  ggtitle("Registered ~ Casual Users by WorkDay") +
  theme_minimal()

ggplot(bike, aes(x = Casual, y = Registered, shape = factor(WorkDay)
                 , group=factor(WorkDay))) +
  geom_point() + geom_smooth(method = "lm",se = F) +
  theme_minimal()



###################
###################
# Feel like temp VS actual temp
# No siginificant difference between temp and actual temp
# TempFeel is slightly better than Temp with other variables included

summarise(bike, mean_R = mean(Registered), sd = sd(Registered))
summary(lm(Registered~TempFeel, data=bike))
summary(lm(Registered~Temp, data=bike))
summary(lm(Registered~Humidity, data=bike))
summary(lm(Registered~Windspeed, data=bike))
summary(lm(Registered~Temp+Humidity, data=bike))
summary(lm(Registered~TempFeel+Humidity, data=bike))

# Interactions
summary(lm(Registered~TempFeel*Humidity, data=bike))
summary(lm(Registered~Temp*Humidity, data=bike))
summary(lm(Registered~Temp+Temp:Humidity, data=bike))
summary(lm(Registered~TempFeel+TempFeel:Humidity, data=bike))

###################
# Registered VS weather depends on holiday? 
# Weather*Holiday interaction doesn't add much to the model 
# Too little data
group_by(bike, Holiday, Weather) %>% 
  summarise(n = n(), mean = mean(Registered), sd = sd(Registered))

summary(lm(Registered~as.factor(Weather), data=bike))

summary(lm(Registered~Holiday, data=bike))
summary(lm(Registered~Weather+Holiday, data=bike))
summary(lm(Registered~Weather*Holiday, data=bike))
summary(lm(Registered~Weather+Weather:Holiday, data=bike))

bike$NewHoliday <- ifelse(bike$Holiday == 1, "Holiday", "Not a Holiday")
ggplot(bike, aes(x = factor(Weather), y = Registered)) + 
  geom_boxplot() + xlab("Weather") + ylab("") +
  ggtitle("Registered ~ Weather + Holiday") +
  facet_wrap(~NewHoliday) + theme_minimal() 



# WorkDay adds to the model
summary(lm(Registered~Weather+Holiday+WorkDay, data=bike))
summary(lm(Registered~Weather+WorkDay, data=bike))

# Registered VS Hour depends on WorkDay
# Yes
ggplot(bike, aes(x = Hour, y = Registered)) + 
  geom_jitter() +
  ggtitle("Registered~Hour") +
  facet_wrap(~WorkDay) + theme_minimal() +
  geom_smooth()


###
# Hour Casual VS registered
###
plot(Hour, Registered, main = "Registered~Hour")
Hour_shift <- (Hour + 5)%%23
ggplot(bike, aes(x = Hour, y = Casual)) + 
  geom_point() +
  ggtitle("Registered~Hour") +
  facet_wrap(~WorkDay)
summary(lm(Registered~Casual, data=bike))
summary(lm(Registered~Casual*Hour, data=bike))
summary(lm(Registered~Casual*factor(Hour), data=bike))
summary(lm(Registered~Casual*factor(Hour)+WorkDay*factor(Hour), data=bike))
summary(lm(Registered~Casual+Casual:WorkDay+WorkDay:factor(Hour), data=bike))


# Conditional distributions
group_by(bike, Weather) %>% 
  summarise(mean = mean(Registered), sd = sd(Registered),
            min = min(Registered), max = max(Registered))


a <- group_by(bike, WorkDay, Hour) %>% 
  summarise(mean = mean(Registered), sd = sd(Registered),
            min = min(Registered), max = max(Registered), n=n())

group_by(bike, Day) %>% 
  summarise(mean = mean(Registered), sd = sd(Registered),
            min = min(Registered), max = max(Registered), n=n())

group_by(bike, Month) %>% 
  summarise(mean = mean(Registered), mid = median(Registered), sd = sd(Registered),
            min = min(Registered), max = max(Registered), n=n())

# Weekend
weekend <- filter(bike, Day == 0 | Day == 6) %>% select(Registered) %>% unlist
notWeekend <- filter(bike, Day != 0 & Day != 6)  %>% select(Registered) %>% unlist
bike$Weekend <- ifelse(bike$Day == 0 | bike$Day == 6, "Weekend","Weekday")

sterr <- bike %>% group_by(Weekend, Hour) %>%
  summarise(n = n(), mean = mean(Registered),se_mean = sd(Registered)/sqrt(n))

# Mean registered ~ Hour by Weekend with error bars
ggplot(sterr, aes(x = Hour, y = mean)) + 
  geom_point() + geom_line() + ylab("Number of Registered Bikers") +
  geom_errorbar(aes(ymin = mean-se_mean, ymax = mean+se_mean)) +
  ggtitle("Registered~Hour by Weekend") +
  facet_wrap(~Weekend) + theme_minimal() 

sterr2 <- bike %>% group_by(WorkDay, Hour) %>%
  summarise(n = n(), mean = mean(Registered),se_mean = sd(Registered)/sqrt(n))
ggplot(sterr2, aes(x = Hour, y = mean)) + 
  geom_point() + geom_line() + ylab("Number of Registered Bikers") +
  geom_errorbar(aes(ymin = mean-se_mean, ymax = mean+se_mean)) +
  ggtitle("Registered~Hour by WorkDay") +
  facet_wrap(~WorkDay, labeller = as_labeller(c("1" = "Workday", "0" = "Non-Workday"))) + theme_minimal() 

# Workday Statistically significant
workday <- bike[which(bike$WorkDay == 1),]$Registered
notWorkday <- bike[which(bike$WorkDay == 0),]$Registered
t.test(workday,notWorkday)

# Weather 1 and 2 are not statistically different
weather1 <- bike[which(bike$Weather == 1),]$Registered
weather2 <- bike[which(bike$Weather == 2),]$Registered
t.test(weather1,weather2)


# Workday*Hour
work <- filter(bike, WorkDay == "Yes") %>% group_by(Hour) %>% 
  summarise(mean = mean(Registered), mid = median(Registered)) %>% 
  arrange(Hour) %>% ungroup
nowork <- filter(bike, WorkDay == "No") %>% group_by(Hour) %>% 
  summarise(mean = mean(Registered), mid = median(Registered))  %>% 
  arrange(Hour) %>% ungroup
dif <- group_by(bike, WorkDay, Hour) %>%
  summarise(mean_dif = mean(R))

# Workday VS Day
summary(lm(Registered~Workday, data=bike))
summary(lm(Registered~factor(Day), data=bike))
summary(lm(Registered~Workday*factor(Day), data=bike))
TueFri <- ifelse(bike$Day == 2 | bike$Day == 5, 1, 0)
summary(lm(Registered~WorkDay*TueFri, data=bike))


##############
## Time 
## Year
summary(lm(Registered~Year,data=bike))
ggplot(bike, aes(x= as.factor(Year), y = Registered)) +
  geom_boxplot() + xlab("") + ylab("") +
  ggtitle("Number of Registered Bikers on Year") + theme_minimal()
# Month
ggplot(bike, aes(x= as.factor(Month), y = Registered)) +
  geom_boxplot() + xlab("") + ylab("") +
  ggtitle("Number of Registered Bikers on Month") + theme_minimal()
# Day
ggplot(bike, aes(x= as.factor(Day), y = Registered)) +
  geom_boxplot() + xlab("") + ylab("") +
  ggtitle("Number of Registered Bikers on Day") + theme_minimal() +
  scale_x_discrete(labels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
# WorkDay
ggplot(bike, aes(x= as.factor(WorkDay), y = Registered)) +
  geom_boxplot() + xlab("") + ylab("") +
  ggtitle("Number of Registered Bikers on Work Day") + theme_minimal() +
  scale_x_discrete(labels = c("Non-Work Day", "Work Day"))
# Holiday
ggplot(bike, aes(x= as.factor(Holiday), y = Registered)) +
  geom_boxplot() + xlab("") + ylab("") +
  ggtitle("Number of Registered Bikers on Holiday") + theme_minimal() +
  scale_x_discrete(labels = c("Non-Holiday", "Holiday"))
# Weather
ggplot(bike, aes(x= as.factor(Weather), y = Registered)) +
  geom_boxplot() + xlab("") + ylab("") +
  ggtitle("Number of Registered Bikers on Weather") + theme_minimal() 

summary(lm(Registered~Year*WorkDay,data=bike))
ggplot(bike, aes(x = as.factor(Month), y = Registered)) +
  geom_point() +
  facet_grid(Workday~Year)

# T tests
weather1 <- bike[which(bike$Weather == 1),]$Registered
weather2 <- bike[which(bike$Weather == 2),]$Registered
t.test(weather1,weather2)

# difference not significant
nonwork <- bike[which(bike$WorkDay == 0),]$Registered
holiday <- bike[which(bike$Holiday == 1),]$Registered
weekend <- bike[which(bike$Day == 0 | bike$Day == 6),]$Registered
t.test(nonwork,holiday)
t.test(weekend, nonwork)
t.test(weekend, holiday)
summary(lm(Registered~Holiday, data=bike))
summary(lm(Registered~WorkDay, data=bike))
summary(lm(Registered~Holiday, data=bike))


############################################################
## Manipulate dataset 2
############################################################
newWeather <- ifelse(bike$Weather == 3, 1, 0) 
# Hour
midnight <- ifelse(bike$Hour < 6 | bike$Hour == 23 , 1, 0)
am6 <- ifelse(bike$Hour==6, 1, 0)
am7 <- ifelse(bike$Hour==7, 1, 0)
am8 <- ifelse(bike$Hour==8, 1, 0)
am9 <- ifelse(bike$Hour==9, 1, 0)
am10to11 <- ifelse(bike$Hour %in% c(10,11), 1, 0)
pm12to15 <- ifelse(bike$Hour %in% c(12:15), 1, 0)
pm12to14 <- ifelse(bike$Hour %in% c(12:14), 1, 0)
pm15 <- ifelse(bike$Hour == 15, 1, 0)
pm16 <- ifelse(bike$Hour == 16, 1, 0)
pm17to19 <- ifelse(bike$Hour %in% c(17,18,19), 1, 0)
pm17 <- ifelse(bike$Hour == 17, 1, 0)
pm18 <- ifelse(bike$Hour == 18, 1, 0)
pm19 <- ifelse(bike$Hour == 19, 1, 0)
pm1620 <- ifelse(bike$Hour %in% c(16,20), 1, 0)
pm20 <- ifelse(bike$Hour == 20, 1, 0)
pm21to22 <- ifelse(bike$Hour %in% c(21,22), 1, 0)

h1 <- summary(lm(Registered~factorsum(Hour), data=bike))
summary(lm(Registered~am6+am7+am8+am9+am10to11+pm12to15+pm16+pm17to19+pm20+
             pm21to22+WorkDay:am8+WorkDay:pm17to19+WorkDay:pm20, data=bike))


############
# Intial Model 
summary(lm(Registered~TempFeel+Windspeed+Weather+Casual+
             WorkDay+Weather:WorkDay+Weather:TempFeel, data=bike))
summary(lm(Registered~TempFeel+Windspeed+Weather+Casual+
             Weather:WorkDay+factor(Hour):WorkDay, data=bike))
summary(lm(Registered~factor(Weather):factor(Hour), data=bike))
summary(lm(Registered~NewWeather+Casual+Casual:WorkDay+
             WorkDay:factor(Hour), data=bike))
summary(lm(Registered~NewWeather+Casual+WorkDay+
             factor(Hour), data=bike))
# Best with only hours
summary(lm(Registered~morning6+morning8+morningRush2+morning+
             afternoon+eveningRush1+eveningRush2+pm21to22+
             morningRush2:WorkDay+eveningRush1:WorkDay+NewWeather+
             TempFeel), data=bike)
# Add workday interaction WorkDay:eveningRush1
summary(lm(Registered~am6+am7+am8+am9+am10to11+pm12to15+pm17to19+
             pm16+pm20+pm21to22+WorkDay:am7+WorkDay:am8+WorkDay:pm17to19
             +WorkDay:pm20, data=bike))
# Consolidate workday hour variables
summary(lm(Registered~am6+am8+am9+am10to11+pm12to15+pm17to19+
             pm16+pm20+pm21to22+WorkDay:am7+WorkDay:am8+WorkDay:pm17to19, data=bike))
# Modify Month to October or not October
october <- ifelse(bike$Month == 10, 1, 0)
summary(lm(Registered~morning6+morning8+morningRush2+morning+
             afternoon+eveningRush1+eveningRush2+pm21to22+
             morningRush2:WorkDay+eveningRush1:WorkDay+NewWeather+
             october+TempFeel:Humidity, data=bike))
# Remove October, add Year
summary(lm(Registered~am6+am8+am9+am10to11+pm12to15+pm17to19+
             pm16+pm20+pm21to22+WorkDay:am7+WorkDay:am8+WorkDay:pm17to19+Year, data=bike))
# Add weather factors (Temperature and Humidity)
summary(lm(Registered~am6+am8+am9+am10to11+pm12to15+pm17to19+
             pm16+pm20+pm21to22+WorkDay:am7+WorkDay:am8+WorkDay:pm17to19+
             Year+TempFeel+TempFeel:Humidity, data=bike))
# Add Casual
summary(lm(Registered~am6+am8+am9+am10to11+pm12to15+pm17to19+
             pm16+pm20+pm21to22+WorkDay:am7+WorkDay:am8+WorkDay:pm12to15+
             WorkDay:pm17to19+
             Year+TempFeel+TempFeel:Humidity+
             Casual+WorkDay:Casual, data=bike))
# Adjust
summary(lm(Registered~am6+am8+am9+am10to11+pm12to15+pm17to19+
             pm16+pm20+pm21to22+WorkDay:am7+WorkDay:am8+WorkDay:pm12to15+
             WorkDay:pm17to19+
             Year+TempFeel+
             Casual+WorkDay:Casual, data=bike))
# More adjust - all values positive
summary(lm(Registered~0+am6+am8+am9+am10to11+pm12to15+pm17+pm18+pm19+
             pm16+pm20+pm21to22+WorkDay:am7+WorkDay:am8+WorkDay:pm12to15+
             WorkDay:pm17+WorkDay:pm18+WorkDay:pm19+
             Year+TempFeel+
             Casual+WorkDay:Casual, data=bike))
# Remove temperature cause it's not important now
summary(lm(Registered~0+am6+am8+am9+am10to11+pm12to15+pm17to19+
             pm16+pm20+pm21to22+WorkDay:am7+WorkDay:am8+WorkDay:pm12to15+
             WorkDay:pm17to19+
             Year2012+
             Casual+WorkDay:Casual, data=bike))

##################################################################
# Model Diagnostics
##################################################################
Year2012 <- ifelse(bike$Year == 2012, 1, 0)
model1 <- lm(Registered~0+am6+am8+am9+am10to11+pm12to15+pm17to19+
              pm16+pm20+pm21to22+WorkDay:am7+WorkDay:am8+WorkDay:pm12to15+
              WorkDay:pm17to19+
              Year2012+
              Casual+WorkDay:Casual, data=bike)


# all positive numbers
Registered2 <- bike$Registered+1
model2 <- lm(Registered2^0.5~0+am6+am7+am8+am9+am10to11+pm12to15+pm17to19+
               pm16+pm20+pm21to22+WorkDay:am8+WorkDay:pm12to15+
               WorkDay:pm17to19+
               Year2012+TempFeel+TempFeel:Humidity+
               Casual+WorkDay:Casual, data=bike)

# Residual and QQ-plots
ggplot(model1, aes(x=.fitted, y=.resid)) + geom_point() +
  geom_hline(yintercept = 0) + xlab("Order") +
  ylab("Residuals") + ggtitle("Residuals ~ Order (Final Model)") +
  theme_minimal()
plot(model2$fit, model2$res, main = "Residuals~Fitted (Initial Model))",
     xlab = "Fitted Values", ylab="Residuals") 
abline(h=0)
qqnorm(model2$res, main ="QQ-Plot (Initial Model)")
qqline(model2$res)


#boxcox transformation
bc <- boxcox(model2)
lambda <- bc$x[bc$y==max(bc$y)]
model3 <- lm(Registered2^lambda~0+am6+am8+am9+am10to11+pm12to15+pm17to19+
               pm16+pm20+pm21to22+WorkDay:am7+WorkDay:am8+WorkDay:pm12to15+
               WorkDay:pm17to19+
               Year2012+
               Casual+WorkDay:Casual, data=bike)
# Final adjust, check if other variables can be added to the mode now
model4 <- lm(Registered2^lambda~0+am6+am8+am9+am10to11+pm12to15+pm17to19+
               pm16+pm20+pm21to22+
               WorkDay:am7+WorkDay:am8+WorkDay:pm17to19+
               Year2012+Casual+WorkDay:Casual, data=bike)
model5 <- lm(Registered2^0.5~0+am6+am7+am8+am9+am10to11+pm12to15+pm17to19+
               pm16+pm20+pm21to22+
               WorkDay:am8+WorkDay:pm17to19+
               Year2012+Casual+WorkDay:Casual+TempFeel, data=bike)
# Diagnostics of Model 5
plot(model5$fit, model5$res, main = "Residuals ~ Order (Final Model)",
     xlab = "Fitted Values", ylab="Residuals") 
abline(h=0)
qqnorm(model5$res, main = "QQ-Plot (Final Model)")
qqline(model5$res)
boxcox(model5, main="Boxcox (Final Model)")
par(mfrow=c(1,5))

# Rest of diagnostics
Registered2 <- bike$Registered+1
Year2012 <- ifelse(bike$Year == 2012, 1, 0)
res
time <- rep("11pmto5am",910)
time[which(am6 == 1)] <- "am6"
time[which(am7 == 1)] <- "am7"
time[which(am8 == 1)] <- "am8"
time[which(am9 == 1)] <- "am9"
time[which(am10to11 == 1)] <- "am10to11"
time[which(pm12to15 == 1)] <- "pm12to3"
time[which(pm16 == 1)] <- "pm4"
time[which(pm17to19 == 1)] <- "pm5to7"
time[which(pm20 == 1)] <- "pm8"
time[which(pm21to22 == 1)] <- "pm9to10"
time <- factor(time,levels(time)[c(1,3:11,2)]) 

residDat <- cbind(Registered2, time, Year2012, TempFeel = bike$TempFeel,
                  Casual = bike$Casual, WorkDay = bike$WorkDay,
                  res = model5$residuals) %>% as.data.frame

residDat2 <- data.frame(Registered2, am6, am7, am8, am9, am10to11, pm12to15, pm16,
                       pm17to19, pm20, pm21to22, midnight, Year = as.factor(bike$Year), 
                       TempFeel = bike$TempFeel,Casual = bike$Casual, WorkDay = as.factor(bike$WorkDay),
                       am8WorkDay = am8*bike$WorkDay, pm17to19WorkDay = pm17to19*bike$WorkDay, 
                       WC = bike$WorkDay*bike$Casual,res = model5$residuals) %>% as.data.frame

boxplot(time ~ res, data=residDat)

ggplot(residDat, aes(x = am6, y = res)) +
  geom_boxplot() + theme(axis.text.y=element_blank())

par(mfrow=c(4,4))
for(i in residDat) print(name(i))
residual_specs <- geom_boxplot() + xlab("") + ylab("Residuals") + theme_minimal() +
  theme(axis.text.x = element_blank())

p1 <- ggplot(residDat2, aes(x = am6, y = res, group=as.factor(am6))) +
  geom_boxplot() + xlab("") + ylab("Residuals") + theme_minimal() +
  theme(axis.text.x = element_blank()) + ggtitle("6am")
p2 <- ggplot(residDat2, aes(x = am7, y = res, group=(as.factor(am7)))) +
  geom_boxplot() + xlab("") + ylab("Residuals") + theme_minimal() +
  theme(axis.text.x = element_blank()) + ggtitle("7am")
p3 <- ggplot(residDat2, aes(x = am8, y = res, group=(as.factor(am8)))) +
  geom_boxplot() + xlab("") + ylab("Residuals") + theme_minimal() +
  theme(axis.text.x = element_blank()) + ggtitle("8am")
p4 <- ggplot(residDat2, aes(x = am9, y = res, group=(as.factor(am9)))) +
  geom_boxplot() + xlab("") + ylab("Residuals") + theme_minimal() +
  theme(axis.text.x = element_blank()) + ggtitle("9am")
p5 <- ggplot(residDat2, aes(x = am10to11, y = res, group=(as.factor(am10to11)))) +
  geom_boxplot() + xlab("") + ylab("Residuals") + theme_minimal() +
  theme(axis.text.x = element_blank()) + ggtitle("10 to 11am")
p6 <- ggplot(residDat2, aes(x = pm12to15, y = res, group=(as.factor(pm12to15)))) +
  geom_boxplot() + xlab("") + ylab("Residuals") + theme_minimal() +
  theme(axis.text.x = element_blank()) + ggtitle("12 to 3pm")
p7 <- ggplot(residDat2, aes(x = pm16, y = res, group=(as.factor(pm16)))) +
  geom_boxplot() + xlab("") + ylab("Residuals") + theme_minimal() +
  theme(axis.text.x = element_blank()) + ggtitle("4pm")
p8 <- ggplot(residDat2, aes(x = pm17to19, y = res, group=(as.factor(pm17to19)))) +
  geom_boxplot() + xlab("") + ylab("Residuals") + theme_minimal() +
  theme(axis.text.x = element_blank()) + ggtitle("5 to 7pm")
p9 <- ggplot(residDat2, aes(x = pm20, y = res, group=(as.factor(pm20)))) +
  geom_boxplot() + xlab("") + ylab("Residuals") + theme_minimal() +
  theme(axis.text.x = element_blank()) + ggtitle("8pm")
p10 <- ggplot(residDat2, aes(x = pm21to22, y = res, group=(as.factor(pm21to22)))) +
  geom_boxplot() + xlab("") + ylab("Residuals") + theme_minimal() +
  theme(axis.text.x = element_blank()) + ggtitle("9 to 10pm")
p11 <- ggplot(residDat2, aes(x = Year2012, y = res, group=(as.factor(Year2012)))) +
  geom_boxplot() + xlab("") + ylab("Residuals") + theme_minimal() +
  theme(axis.text.x = element_blank()) + ggtitle("Year")
p12 <- ggplot(residDat2, aes(x = Casual, y = res)) +
  geom_point() + xlab("") + ylab("Residuals") + theme_minimal() +
  theme(axis.text.x = element_blank()) + ggtitle("Casual Bikers")
p13 <- ggplot(residDat2, aes(x = TempFeel, y = res)) +
  geom_point() + xlab("") + ylab("Residuals") + theme_minimal() +
  theme(axis.text.x = element_blank()) + ggtitle("Feel like Temp")
p14 <- ggplot(residDat2, aes(x = am8WorkDay, y = res, group=(as.factor(am8WorkDay)))) +
  geom_boxplot() + xlab("") + ylab("Residuals") + theme_minimal() +
  theme(axis.text.x = element_blank()) + ggtitle("8am * Workday")
p15 <- ggplot(residDat2, aes(x = pm17to19WorkDay, y = res, group=(as.factor(pm17to19WorkDay)))) +
  geom_boxplot() + xlab("") + ylab("Residuals") + theme_minimal() +
  theme(axis.text.x = element_blank()) + ggtitle("5-7pm * Workday")
p16 <- ggplot(residDat2, aes(x = WC, y = res, group=(as.factor(WC)))) +
geom_boxplot() + xlab("") + ylab("Residuals") + theme_minimal() +
  theme(axis.text.x = element_blank()) + ggtitle("Casual * Workday")

library(gridExtra)
grid.arrange(nrow=4, p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16)

model6 <- lm(Registered2^0.5~0+am6+am7+am8+am9+am10to11+pm12to15+pm16+pm17to19+
               pm20+pm21to22+
               WorkDay:am6+WorkDay:am7+WorkDay:am8+WorkDay:pm17to19+WorkDay:pm20+
               Year2012+sqrt(Casual)+WorkDay:sqrt(Casual)+TempFeel, data=bike)

model7 <- lm(Registered2^0.5~0+am8+am9+am10to11+pm12to15+pm17to19+
               pm16+pm20+pm21to22+
               WorkDay:am8+WorkDay:pm17to19+WorkDay:am7+WorkDay:am6+
               Year2012+sqrt(Casual)+WorkDay:sqrt(Casual)+TempFeel, data=bike)

confint(model7)
