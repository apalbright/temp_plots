#Weather for SF 1995-2015, focus on 2015
#Using code from Brad Boehmke!
#See here: https://rpubs.com/bradleyboehmke/weather_graphic
#Adapted by Alex Albright 7-25-16

#Import data
#Data from Average Daily Temperature Archive (via The University of Dayton)

library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
"http://academic.udayton.edu/kissock/http/Weather/gsod95-current/CASANFRA.txt" %>%
  read.table() %>% data.frame %>% tbl_df -> data
names(data) <- c("month", "day", "year", "temp")

data<-data[which(data$year<=2015),]

data %>%
  group_by(year, month) %>%
  arrange(day) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(newday = seq(1, length(day))) %>%   # label days as 1:365 (will represent x-axis)
  ungroup() %>%
  filter(temp != -99 & (year < 2015)) %>%     # filter out missing data (identified with '-99' value) & current year data
  group_by(newday) %>%
  mutate(upper = max(temp), # identify max value for each day
         lower = min(temp), # identify min value for each day
         avg = mean(temp),  # calculate mean value for each day
         se = sd(temp)/sqrt(length(temp))) %>%  # calculate standard error of mean
  mutate(avg_upper = avg+(2.101*se),  # calculate 95% CI for mean
         avg_lower = avg-(2.101*se)) %>%  # calculate 95% CI for mean
  ungroup() -> past

# Dataframe for 2015 data
data %>%
  group_by(year, month) %>%
  arrange(day) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(newday = seq(1, length(day))) %>%  # create matching x-axis as historical data
  ungroup() %>%
  filter(temp != -99 & year == 2015) -> present   # filter out missing data & select current year data

# DF for lowest temp for each day for the historical data
pastlows <- past %>%
  group_by(newday) %>%
  summarise(Pastlow = min(temp)) # identify lowest temp for each day from 1975-2013

# DF for days in 2015 in which the temps were lower than all previous years
presentlows <- present %>%
  left_join(pastlows) %>%  # merge historical lows to current year low data
  mutate(record = ifelse(temp<Pastlow, "Y", "N")) %>% # identifies if current year was record low
  filter(record == "Y")  # filter for days that represent current year record lows

# DF for the highest temp for each day for the historical data
pasthighs <- past %>%
  group_by(newday) %>%
  summarise(Pasthigh = max(temp))  

# DF that identifies the days in 2015 in which the temps were higher than all previous years
presenthighs <- present %>%
  left_join(pasthighs) %>%  # merge historical highs to current year low data
  mutate(record = ifelse(temp>Pasthigh, "Y", "N")) %>% # identifies if current year was record high
  filter(record == "Y")  # filter for days that represent current year record highs

dgr_fmt <- function(x, ...) {
  parse(text = paste(x, "*degree", sep = ""))
}

# y-axis variable
a <- dgr_fmt(seq(35,85, by=10))

p <- ggplot(past, aes(newday, temp)) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.text.y=element_text(face="bold"),
        axis.title = element_blank()) +
  geom_linerange(past, mapping=aes(x=newday, ymin=lower, ymax=upper), colour = "wheat2", alpha=.1)

#95% confidence interval around the daily mean temperatures for 1995-2014
p <- p +
  geom_linerange(past, mapping=aes(x=newday, ymin=avg_lower, ymax=avg_upper), colour = "wheat4")

p <- p +
  geom_line(present, mapping=aes(x=newday, y=temp, group=1)) +
  geom_vline(xintercept = 0, colour = "wheat3", linetype=1, size=1)

p <- p +
  geom_vline(xintercept = 31, colour = "wheat3", linetype=3, size=.5) +
  geom_vline(xintercept = 59, colour = "wheat3", linetype=3, size=.5) +
  geom_vline(xintercept = 90, colour = "wheat3", linetype=3, size=.5) +
  geom_vline(xintercept = 120, colour = "wheat3", linetype=3, size=.5) +
  geom_vline(xintercept = 151, colour = "wheat3", linetype=3, size=.5) +
  geom_vline(xintercept = 181, colour = "wheat3", linetype=3, size=.5) +
  geom_vline(xintercept = 212, colour = "wheat3", linetype=3, size=.5) +
  geom_vline(xintercept = 243, colour = "wheat3", linetype=3, size=.5) +
  geom_vline(xintercept = 273, colour = "wheat3", linetype=3, size=.5) +
  geom_vline(xintercept = 304, colour = "wheat3", linetype=3, size=.5) +
  geom_vline(xintercept = 334, colour = "wheat3", linetype=3, size=.5) +
  geom_vline(xintercept = 365, colour = "wheat3", linetype=3, size=.5)

p <- p +
  coord_cartesian(ylim = c(35,85)) +
  scale_y_continuous(breaks = seq(35,85, by=10), labels = a) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                     labels = c("January", "February", "March", "April",
                                "May", "June", "July", "August", "September",
                                "October", "November", "December"))

p <- p +
  geom_point(data=presentlows, aes(x=newday, y=temp), colour="blue3") +
  geom_point(data=presenthighs, aes(x=newday, y=temp), colour="firebrick3")

p <- p +
  ggtitle("San Francisco Weather, 1995-2015") +
  theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=24)) +
  annotate("text", x = 74, y = 86, label = "Temperature in Fahrenheit (Average Daily Temperatures)\nCode via Brad Boehmke; Adapted by Alex Albright (thelittledataset.com)", size=4, fontface="bold")

p +
  annotate("segment", x = 360, xend = 340, y = 41.2, yend = 39.5, colour = "blue3") +
  annotate("text", x = 330, y = 39, label = "We had only 1 day that was the", size=3, colour="blue3") +
  annotate("text", x = 330, y = 38, label = "coldest since 1995", size=3, colour="blue3") +
  annotate("segment", x = 19, xend = 44, y = 81.5, yend = 81.5, colour = "firebrick3") +
  annotate("segment", x = 93, xend = 304, y = 81.5, yend = 81.5, colour = "firebrick3") +
  annotate("segment", x = 19, xend = 19, y = 58, yend = 81.5, colour = "firebrick3") +
  annotate("segment", x = 304, xend = 304, y = 65, yend = 81.5, colour = "firebrick3") +
  annotate("text", x = 69, y = 82, label = "We had 43 days that were the", size=3, colour="firebrick3") +
  annotate("text", x = 69, y = 81, label = "hottest since 1995", size=3, colour="firebrick3") -> p

present %>% filter(newday %in% c(180:185)) %>% select(x = newday, y =  temp) %>% data.frame -> legend_data
legend_data$y - 65 -> legend_data$y
p +
  annotate("segment", x = 182, xend = 182, y = 40, yend = 50, colour = "wheat2", size=3) +
  annotate("segment", x = 182, xend = 182, y = 43.3, yend = 46.6, colour = "wheat4", size=3) +
  geom_line(data=legend_data, aes(x=x-2,y=y+47)) +
  annotate("segment", x = 184, xend = 186, y = 46.6, yend = 46.6, colour = "wheat4", size=.5) +
  annotate("segment", x = 184, xend = 186, y = 43.3, yend = 43.3, colour = "wheat4", size=.5) +
  annotate("segment", x = 185, xend = 185, y = 43.3, yend = 46.6, colour = "wheat4", size=.5) +
  annotate("text", x = 198, y = 44.75, label = "Normal Range", size=3, colour="gray30", fontface="bold") +
  annotate("text", x = 167, y = 44.75, label = "2015 Temp", size=3, colour="gray30", fontface="bold") +
  annotate("text", x = 195, y = 50, label = "Record High", size=3, colour="gray30", fontface="bold") +
  annotate("text", x = 195, y = 40, label = "Record Low", size=3, colour="gray30", fontface="bold")

#save as 14in x 10in pdf