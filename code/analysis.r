library(ggplot2)

setwd('C:/Users/wrbrooks/git/loadings/code')

#read the data files into R
eagle = read.csv('../data/eagle/eaglecreek.csv', header=T, na.strings='na')
joos = read.csv('../data/joos/joosvalleycreek.csv', header=T, na.strings='na')
otter = read.csv('../data/otter/ottercreek.csv', header=T, na.strings='na')

#turn eventtype into a factor
eagle$eventtype = factor(eagle$eventtype)
joos$eventtype = factor(joos$event_type)
otter$eventtype = factor(otter$event_type)

#turn year into a factor
eagle$year = factor(eagle$year)
joos$year = factor(joos$year)
otter$year = factor(otter$year)

#code missing values for snowmelt
eagle = within(eagle, meltsnow[meltsnow=='N' | meltsnow=='Z' | meltsnow=='U'] <- NA)
eagle = within( eagle, meltsnow <- as.numeric(levels(meltsnow)[meltsnow]) )

joos = within(joos, melt_snow[melt_snow=='N' | melt_snow=='Z' | melt_snow=='U'] <- NA)
joos = within( joos, melt_snow <- as.numeric(levels(melt_snow)[melt_snow]) )

otter = within(otter, melt_snow[melt_snow=='N' | melt_snow=='Z' | melt_snow=='U'] <- NA)
otter = within( otter, melt_snow <- as.numeric(levels(melt_snow)[melt_snow]) )

#fit = read.table('../output/eagle/fitted.txt', header=T)
#p <- ggplot(fit, aes(x=node, y=observed)) 
#p+geom_jitter()

#Plot of the cumulative sum of loading, showing that the large majority of load come from the very largest individual events:
#plot(cumsum(sort(eagle$sstormtot)), type='l')

#tag the events that are above the 90th percentile
threshold = as.numeric( quantile(eagle$logsstormtot, 0.9) )
eagle = within(eagle, major <- ifelse(logsstormtot>threshold,1,0))

threshold = as.numeric( quantile(joos$logsstormtot, 0.9) )
joos = within(joos, major <- ifelse(logsstormtot>threshold,1,0))

threshold = as.numeric( quantile(otter$logsstormtot, 0.9) )
otter = within(otter, major <- ifelse(logsstormtot>threshold,1,0))

write.table(eagle, file='../data/eagle/eagle_mod.csv', sep=',', row.names=F, col.names=F)
write.table(joos, file='../data/joos/joos_mod.csv', sep=',', row.names=F, col.names=F)
write.table(otter, file='../data/otter/otter_mod.csv', sep=',', row.names=F, col.names=F)
