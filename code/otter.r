#Specific to Eagle Creek

setwd('C:/Users/wrbrooks/git/loadings/code')

#read the data files into R
otter = read.csv('../data/otter/ottercreek.csv', header=T, na.strings='na')

#turn eventtype into a factor
otter$event_type = factor(otter$event_type)

#put NA's in place of some missing values
blank = which(!is.na(otter[,'s']) & !otter[,'s']=='S')
otter[blank,'s']=NA

blank = which(!is.na(otter[,'p']) & !otter[,'p']=='P')
otter[blank,'p']=NA

blank = which(!is.na(otter[,'m']) & !otter[,'m']=='M')
otter[blank,'m']=NA

#turn year into a factor
otter$year = factor(otter$year)


#code missing values for snowmelt
otter = within(otter, melt_snow[melt_snow=='N' | melt_snow=='Z' | melt_snow=='U'] <- NA)
otter = within( otter, melt_snow <- as.numeric(levels(melt_snow)[melt_snow]) )

#tag the events that are above the 90th percentile
threshold = as.numeric( quantile(otter$logsstormtot, 0.9) )
otter = within(otter, major <- ifelse(logsstormtot>threshold,1,0))

#assign weights to each observation based on the absolute disance from the major/non-major threshold.
distance = with(otter, 2*abs(logsstormtot-threshold)/sd(logsstormtot))
otter = within( otter, weight <- ifelse(distance<=1, distance, 1) )

write.table(otter, file='../data/otter/otter_mod.csv', sep=',', row.names=F, col.names=F)
