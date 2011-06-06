#Specific to Eagle Creek

setwd('C:/Users/wrbrooks/git/loadings/code')

#read the data files into R
joos = read.csv('../data/joos/joosvalleycreek.csv', header=T, na.strings='na')

#put NA's in place of some missing values
blank = which(!is.na(joos[,'s']) & !joos[,'s']=='S')
joos[blank,'s']=NA

blank = which(!is.na(joos[,'p']) & !joos[,'p']=='P')
joos[blank,'p']=NA

blank = which(!is.na(joos[,'m']) & !joos[,'m']=='M')
joos[blank,'m']=NA

#turn eventtype into a factor
joos$event_type = factor(joos$event_type)


#turn year into a factor
joos$year = factor(joos$year)


#code missing values for snowmelt
joos = within(joos, melt_snow[melt_snow=='N' | melt_snow=='Z' | melt_snow=='U'] <- NA)
joos = within( joos, melt_snow <- as.numeric(levels(melt_snow)[melt_snow]) )

#tag the events that are above the 90th percentile
threshold = as.numeric( quantile(joos$logsstormtot, 0.9) )
joos = within(joos, major <- ifelse(logsstormtot>threshold,1,0))

#assign weights to each observation based on the absolute disance from the major/non-major threshold.
distance = with(joos, 2*abs(logsstormtot-threshold)/sd(logsstormtot))
joos = within( joos, weight <- ifelse(distance<=1, distance, 1) )

write.table(joos, file='../data/joos/joos_mod.csv', sep=',', row.names=F, col.names=F)
