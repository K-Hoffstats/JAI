##############################################################################
#### Juvenile shad electrofishing survey ####
#### last modified: 12-03-2025

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
library(scales)

# 2025.SC.catch.rates file #

SC<-`X2025_SC_catch_rates`
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
  geom_line(aes(y=Temp, color="Water Temperature"), size=1.25, linetype="solid") +
  scale_color_manual(values=c("Water Temperature"="red4", 
                              "Santee"="steelblue",
                              "Congaree"="forestgreen")) +
  labs(color='') +
  scale_x_date(date_breaks="2 weeks", date_labels="%m/%d",
               limits=as.Date(c('2025-06-01','2025-10-30')),
               expand=c(0,0.01)) +
  scale_y_continuous(
    name=expression("Water Temperature (?C)"),
    sec.axis = sec_axis(~.* 1 / 14, name = "Catch Rate (# AMS/min)", breaks=seq(0,2,0.5)),
    breaks=seq(7,28,7),
    expand=c(0,0), limits=c(0,28)) +
  
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA, size=1)) +
  theme(legend.position = c(0.75,0.6)) +
  theme(legend.background = element_rect(fill="slategray1", size=0.25, linetype="solid", color="gray20")) +
  theme(legend.direction = "horizontal") +
  theme(legend.text=element_text(color="black", family = "serif", size=12)) +
  theme(axis.ticks=element_blank())


daily.catch <- daily.catch + theme(axis.title.x = element_text(family = "serif", size=14, vjust = 0)) +
  theme(axis.title.y= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.title.y.right= element_text(family = "serif", size=14, vjust=2)) +
  theme(axis.text.x= element_text(family = "serif", color="black", size=12, angle=0)) +
  theme(axis.text.y= element_text(family = "serif", color="black", size=12)) +
  theme(axis.title.x= element_blank())

daily.catch

ggsave(daily.catch, filename = '2025.daily.catch.png', dpi = 300, type = 'cairo',
       width = 9, height = 7, units = 'in')

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
    expand=c(0,0.02), limits=c(0,167000)) +
  
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

ggsave(totals, filename = 'totals.png', dpi = 300, type = 'cairo',
       width = 9, height = 7, units = 'in')

# I think I'm okay with this--perhaps change the effort line to red color, but
# it looked easier to read with orange, for whatever reason...I'll probably
# just ID the black = AMS, color = effort in the caption of the figure.


# UPDATE: THIS was all well and good, but now we want to strip out all the records
# for the lake sites (any other than sites A-G)--I went back through the series
# and found these records and created a new file 'annuals.main.SaC.csv'

annuals.2<-annuals_main_SaC

summary(annuals.2)

main.totals <- ggplot() +
  geom_col(data = annuals.2, aes(x=Year, y=AMS.collected * 60 / 1),
           fill="black", size = 1.25) +
  geom_line(data = annuals.2, aes(x=Year, y=Main.effort.seconds),
            color="orange", size = 1.25) +
 # geom_line(data = annuals.2, aes(x=Year, y=Santee.seconds),
 #           color="steelblue", size = 1.25) +
 # geom_line(data = annuals.2, aes(x=Year, y=Congaree.seconds),
 #           color="darkgreen", size = 1.25) +
  
  scale_x_continuous(breaks = seq(2009, 2025, 0002),
                     expand=c(0,0), limits=c(2008,2026)) +
  scale_y_continuous(
    name=expression("Effort (seconds)"),
    sec.axis = sec_axis(~.* 1 / 60, name = "AMS Collected (#)", breaks=seq(0,2000,500), label=comma),
    breaks=seq(0,120000,30000), label=comma,
    expand=c(0,0.02), limits=c(0,120000)) +
  
  theme(legend.position = c(0.6,0.85)) +
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y= element_text(family = "serif", size=14, vjust=1)) +
  theme(axis.title.y.right = element_text(family = "serif", size=14, vjust=1)) +
  theme(axis.text.x= element_text(family = "serif", color="black", size=12, vjust=1)) +
  theme(axis.text.y= element_text(family = "serif", color="black", size=12))

main.totals

# I think that about does it--gives a like-representation, but takes out all the slop
# of including sites that no longer are sampled (Lakes and Diversion Canal).
# I added in Santee and Congaree effort lines just to show this effort doesn't 
# appear to waiver much. Don't know if I'll include that or not for the final graphic.


ggsave(main.totals, filename = 'main.totals.png', dpi = 300, type = 'cairo',
       width = 9, height = 7, units = 'in')

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

geos <- annual_geos_minutes
summary(geos)
# UPDATED: added in the earliest years for all sites!

# I'm wondering if I want individual graphics, faceted together, with individual
# Q25's established (horizontal red lines)?

quantile(geos$Santee.geo)
# 25th percentile is 0.324 shad/min (dropped from 0.402; 2013-2025) 

Santee <- ggplot(geos, aes(x=Year)) +
  geom_line(aes(y=Santee.geo, color="Santee"), size=1.25, linetype="solid") +
  geom_hline(yintercept=0.324, color ="red4", linetype = 'dotted', size=1.5) +
  scale_color_manual(values=c("Q25"="red4", 
                              "Santee"="steelblue")) +
  labs(color='') +
  scale_x_continuous(breaks = seq(2009, 2025, 0002),
                     expand=c(0.02,0.02), limits=c(2009,2025)) +
  scale_y_continuous(
    name=expression("Catch Per Minute (# AMS)"), breaks=seq(0,1.2,0.3),
    expand=c(0,0), limits=c(0,1.2)) +
  theme(legend.position = c(0.55,0.8)) +
  theme(legend.background = element_rect(fill="slategray1", size=0.25, linetype="solid", color="gray20")) +
  theme(legend.direction = "horizontal") +
  theme(legend.text=element_text(color="black", family = "serif", size=12)) +
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y= element_text(family = "serif", size=14, vjust=1)) +
  theme(axis.text.x= element_text(family = "serif", color="black", size=12, vjust=1)) +
  theme(axis.text.y= element_text(family = "serif", color="black", size=12))

Santee

#----------------------------

quantile(geos$Congaree.geo)
# Q25 is 0.310 shad/min (dropped from 0.336 shad/min; 2013-2025)

Congaree <- ggplot(geos, aes(x=Year)) +
  geom_line(aes(y=Congaree.geo, color="Congaree"), size=1.25, linetype="solid") +
  geom_hline(yintercept=0.310, color ="red4", linetype = 'dotted', size=1.5) +
  scale_color_manual(values=c("Q25"="red4", 
                              "Congaree"="steelblue")) +
  labs(color='') +
  scale_x_continuous(breaks = seq(2009, 2025, 0002),
                     expand=c(0.02,0.02), limits=c(2009,2025)) +
  scale_y_continuous(
    name=expression("Catch Per Minute (# AMS)"), breaks=seq(0,1.0,0.25),
    expand=c(0,0), limits=c(0,1.0)) +
  theme(legend.position = c(0.55,0.15)) +
  theme(legend.background = element_rect(fill="slategray1", size=0.25, linetype="solid", color="gray20")) +
  theme(legend.direction = "horizontal") +
  theme(legend.text=element_text(color="black", family = "serif", size=12)) +
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y= element_blank()) +
  theme(axis.text.x= element_text(family = "serif", color="black", size=12, vjust=1)) +
  theme(axis.text.y= element_text(family = "serif", color="black", size=12))

Congaree

#----------------------------

quantile(geos$Edisto.geo, na.rm = TRUE)
# Q25 is 0.078 shad/min (slightly up from 0.057 shad/min; 2013-2025)

Edisto <- ggplot(geos, aes(x=Year)) +
  geom_line(aes(y=Edisto.geo, color="Edisto"), size=1.25, linetype="solid") +
  geom_hline(yintercept=0.078, color ="red4", linetype = 'dotted', size=1.5) +
  scale_color_manual(values=c("Q25"="red4", 
                              "Edisto"="steelblue")) +
  labs(color='') +
  scale_x_continuous(breaks = seq(2009, 2025, 0002),
                     expand=c(0.02,0.02), limits=c(2009,2025)) +
  scale_y_continuous(
    name=expression("Catch Per Minute (# AMS)"), breaks=seq(0,1.0,0.25),
    expand=c(0,0), limits=c(0,1.0)) +
  theme(legend.position = c(0.55,0.8)) +
  theme(legend.background = element_rect(fill="slategray1", size=0.25, linetype="solid", color="gray20")) +
  theme(legend.direction = "horizontal") +
  theme(legend.text=element_text(color="black", family = "serif", size=12)) +
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y= element_text(family = "serif", size=14, vjust=1)) +
  theme(axis.text.x= element_text(family = "serif", color="black", size=12, vjust=1)) +
  theme(axis.text.y= element_text(family = "serif", color="black", size=12))

Edisto

#----------------------------

quantile(geos$Savannah.geo, na.rm = TRUE)
# Q25 is 0.051 shad/min (no change from 2013-2025 series)

Savannah <- ggplot(geos, aes(x=Year)) +
  geom_line(aes(y=Savannah.geo, color="Savannah"), size=1.25, linetype="solid") +
  geom_hline(yintercept=0.051, color ="red4", linetype = 'dotted', size=1.5) +
  scale_color_manual(values=c("Q25"="red4", 
                              "Savannah"="steelblue")) +
  labs(color='') +
  scale_x_continuous(breaks = seq(2009, 2025, 0002),
                     expand=c(0.02,0.02), limits=c(2009,2025)) +
  scale_y_continuous(
    name=expression("Catch Per Minute (# AMS)"), breaks=seq(0,1.0,0.25),
    expand=c(0,0), limits=c(0,1.0)) +
  theme(legend.position = c(0.55,0.8)) +
  theme(legend.background = element_rect(fill="slategray1", size=0.25, linetype="solid", color="gray20")) +
  theme(legend.direction = "horizontal") +
  theme(legend.text=element_text(color="black", family = "serif", size=12)) +
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y= element_blank ()) +
  theme(axis.text.x= element_text(family = "serif", color="black", size=12, vjust=1)) +
  theme(axis.text.y= element_text(family = "serif", color="black", size=12))

Savannah

#----------------------------

quantile(geos$GPD.geo, na.rm = TRUE)
# Q25 is 0.291 (slightly up from 0.270 shad/min; 2013-2025)

GPD <- ggplot(geos, aes(x=Year)) +
  geom_line(aes(y=GPD.geo, color="GPD"), size=1.25, linetype="solid") +
  geom_hline(yintercept=0.291, color ="red4", linetype = 'dotted', size=1.5) +
  scale_color_manual(values=c("Q25"="red4", 
                              "GPD"="steelblue")) +
  labs(color='') +
  scale_x_continuous(breaks = seq(2009, 2025, 0002),
                     expand=c(0.02,0.02), limits=c(2009,2025)) +
  scale_y_continuous(
    name=expression("Catch Per Minute (# AMS)"), breaks=seq(0,3.5,0.7),
    expand=c(0,0), limits=c(0,3.5)) +
  theme(legend.position = c(0.55,0.8)) +
  theme(legend.background = element_rect(fill="slategray1", size=0.25, linetype="solid", color="gray20")) +
  theme(legend.direction = "horizontal") +
  theme(legend.text=element_text(color="black", family = "serif", size=12)) +
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(panel.border = element_rect(color="black", fill=NA)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y= element_text(family = "serif", size=14, vjust=1)) +
  theme(axis.text.x= element_text(family = "serif", color="black", size=12, vjust=1)) +
  theme(axis.text.y= element_text(family = "serif", color="black", size=12))

GPD

#----------------------------

library(cowplot)
# probably easiest to see these differences side-by-side...

rivers <- plot_grid(Santee, Congaree, Edisto, Savannah, GPD, ncol=2, align="h")

ggsave(rivers, filename = 'rivers.png', dpi = 300, type = 'cairo',
       width = 9, height = 7, units = 'in')

## Interesting, seems like Santee, Congaree, and GPD generally have similar levels
## of abundance, while Savannah and Edisto have similar (lower) abundance
