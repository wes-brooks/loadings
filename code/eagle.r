#Specific to Eagle Creek

setwd('C:/Users/wrbrooks/git/loadings/code')

#read the data files into R
eagle = read.csv('../data/eagle/eaglecreek.csv', header=T, na.strings='na')


#turn eventtype into a factor
eagle$eventtype = factor(eagle$eventtype)


#turn year into a factor
eagle$year = factor(eagle$year)


#code missing values for snowmelt
eagle = within(eagle, meltsnow[meltsnow=='N' | meltsnow=='Z' | meltsnow=='U'] <- NA)
eagle = within( eagle, meltsnow <- as.numeric(levels(meltsnow)[meltsnow]) )

#tag the events that are above the 90th percentile
threshold = as.numeric( quantile(eagle$logsstormtot, 0.9) )
eagle = within(eagle, major <- ifelse(logsstormtot>threshold,1,0))

#assign weights to each observation based on the absolute disance from the major/non-major threshold.
distance = with(eagle, 2*abs(logsstormtot-threshold)/sd(logsstormtot))
eagle = within( eagle, weight <- ifelse(distance<=1, distance, 1) )

write.table(eagle, file='../data/eagle/eagle_mod.csv', sep=',', row.names=F, col.names=F)
