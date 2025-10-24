##############################################################################
#### Juvenile shad electrofishing survey ####
#### last modified: 10232025

# The purpose here is to recreate some of the traditional "looked at" graphics
# and/or analyses for the juvenile shad report, for DFP-25. Intent is to make
# things match the presentation quality of my other sections, now that no one
# else is around to do this work...

# In the past, they've presented as follows: 

# Table 1. total sample trips, time, ams caught, and CPUE (#/min) separated out
# for all the Santee + Congaree sites (A-G):
  # pretty straightforward here; I simply replicated this exact table to continue 
  # the story of "What sites produce.."

# Figure 3. bar chart of number of the "top 10" species caught:
 # I just made this a table instead, because it's equally informative and a better
 # quality visualization of these data.

# ----------------------------------------------------------------------------

# Figure 4.annual catch rates, plotted through the season (vs.DOY), with water
# temp as the secondary y-axis

# I'll replicate this one, with daily average catch rates (#/min), combining all
# catch/effort for Santee (A-D) and Congaree (E-G) sites. The difference here is
# I'll go ahead and separate out at the River level.

library(tidyverse)
library(lubridate)
library(ggplot2)

# 2025.SC.catch.rates file #

SC<-`2025.SC.catch.rates`
summary(SC)

SC$Date<-as.Date(SC$Date, "%m/%d/%Y" )
is.Date(SC$Date)

max(SC$Santee.shad.per.min) # 1.77
max(SC$Congaree.shad.per.min) # 0.558

max(SC$Temp) 
# ~26 C--which I had to find and throw in there--not a field measurement.


daily.catch <- ggplot(SC, aes(x=Date)) +
  geom_line(aes(y=Santee.shad.per.min * 14 / 1, color="Santee"), size=1.25, linetype="solid") +
  geom_line(aes(y=Congaree.shad.per.min * 14 / 1, color="Congaree"), size=1.25, linetype="solid") +
  geom_line(aes(y=Temp, color="Temp"), size=1.25, linetype="solid") +
  scale_color_manual(values=c("Temp"="red4", 
                              "Santee"="steelblue",
                              "Congaree"="forestgreen")) +
  labs(color='') +
  scale_x_date(date_breaks="2 weeks", date_labels="%m/%d",
               limits=as.Date(c('2025-06-01','2025-10-30')),
               expand=c(0,0.01)) +
  scale_y_continuous(
    name=expression("Water Temperature (°C)"),
    sec.axis = sec_axis(~.* 1 / 14, name = "Catch Rate (# AMS/min)", breaks=seq(0,2,0.5)),
    breaks=seq(7,28,7),
    expand=c(0,0), limits=c(0,28)) +
  
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA, size=1)) +
  theme(legend.position = c(0.75,0.6)) +
  theme(legend.background = element_rect(fill="slategray1", size=0.25, linetype="solid", color="gray20")) +
  theme(legend.direction = "horizontal") +
  theme(legend.text=element_text(size=10, family = "serif")) +
  theme(axis.ticks=element_blank())


daily.catch <- daily.catch + theme(axis.title.x = element_text(family = "serif", size=14, vjust = 0)) +
  theme(axis.title.y= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.title.y.right= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.text.x= element_text(family = "serif", color="black", size=12, angle=0)) +
  theme(axis.text.y= element_text(family = "serif", color="black", size=12)) +
  theme(axis.title.x= element_blank())

daily.catch

# That will probably suffice at telling the story--interesting that we never
# really had > 1 shad/min, aside from the July sample date on the Santee.

# -------------------------------------------------------------------------- #

# Figure 5. bar chart of # ams caught per year, fit with a line to represent total
# effort (seconds)

# I don't mind this figure--it'd probably be better to have catch as a filled
# polygon, but bars are okay too. I want to plot effort and gross (arithmetic)
# CPUE to see if they covary or what exactly that relationship is, but really, 
# the CPUE that we want to report is geometric, so perhaps I want to calculate
# a global geomean for Santee + Congaree sites for the year and plot that?

# END OF THE DAY: really, there's no reason to put global CPUE metrics out there,
# when we include the river-specific figure (below) as the baseline!

  # just reconstruct this figure as-is, aside from small changes

summary(annuals)
# pretty big conversion here--looks like 1/53

library(scales)

totals <- ggplot() +
  geom_col(data = annuals, aes(x=Year, y=AMS.collected * 50 / 1),
           fill="black", size = 1.25) +
  geom_line(data = annuals, aes(x=Year, y=Effort.seconds),
            color="orange", size = 1.25) +
  
  scale_x_continuous(breaks = seq(2009, 2025, 0002),
                     expand=c(0,0), limits=c(2008,2026)) +
  scale_y_continuous(
    name=expression("Effort (seconds)"),
    sec.axis = sec_axis(~.* 1 / 50, name = "AMS Collected (#)", breaks=seq(0,3312,828), label=comma),
    breaks=seq(0,165600,41400), label=comma,
    expand=c(0,0), limits=c(0,165600)) +
  
  theme(legend.position = c(0.6,0.85)) +
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y= element_text(family = "serif", size=14, vjust=1)) +
  theme(axis.title.y.right = element_text(family = "serif", size=14, vjust=1)) +
  theme(axis.text.x= element_text(family = "serif", color="black", size=12, vjust=1)) +
  theme(axis.text.y= element_text(family = "serif", color="black", size=12))

totals

# I think I'm okay with this--perhaps change the effort line to red color, but
# it looked easier to read with orange, for whatever reason...I'll probably
# just ID the black = AMS, color = effort in the caption of the figure.

# ---------------------------------------------------------------------------- #

# Figure 6.stacked years, ams catch by month--this was actually adult ams, but if
# we want to move forward with this (see monthly presence of the species) we should
# just adapt this to my stacked graphic paradigm.

# This is probably a good opportunity to put in the river-specific CPUE series
# to qualify how "good" each year was wrt. Q25--like all the other indices. 
# Probably good to insert this because I'm already doing it for the annual 
# compliance reporting, and intend to have it as a potential metric for annual
# sustainability...

# I wrote converted the per-second annual rates (from: 
# "Compiled_juve.shad_shocking_SUS.xlsx") to per-minute rates in 
# "annual.geos.minutes.csv"--and read that in

geos <- annual.geos.minutes
summary(geos)
# just missing the Edisto rates, because they aren't finished yet...same as the
# rest of the survey (October 2025), but we move forward!

# I'm wondering if I want individual graphics, faceted together, with individual
# Q25's established (horizontal red lines)?

quantile(geos$Santee.geo)
# 25th percentile is 0.402 shad/min 

quantile(geos$Congaree.geo)
# Q25 is 0.336 shad/min

quantile(geos$Edisto.geo, na.rm = TRUE)
# Q25 is 0.057 shad/min

quantile(geos$Savannah.geo)
# Q25 is 0.048 shad/min

quantile(geos$GPD.geo)
# Q25 is 0.270 shad/min

## Interesting, seems like Santee, Congaree, and GPD are on a similar level while
## Savannah and Edisto are similar (lower) values

Santee <- ggplot(geos, aes(x=Year)) +
  geom_line(aes(y=Santee.geo, color="steelblue"), size=1.25, linetype="solid")+
  geom_hline()

