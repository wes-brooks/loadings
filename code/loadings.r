setwd('C:/Users/wrbrooks/git/loadings/code')

#read the data files into R
eagle_file = '../data/eagle/eaglecreek.csv'
joos_file = '../data/joos/joosvalleycreek.csv'
otter_file = '../data/otter/ottercreek.csv'

data_files = list(eagle_file, joos_file, otter_file)
for(data_file in data_files) {

    loading_data = read.csv(data_file, header=T)

    threshold <- as.numeric( quantile(loading_data$logsstormtot, 0.9) )     #establish the threshold or a major event
    
    loading_data = within( loading_data, {
        event_type <- factor(event_type)                                    #turn event_type into a factor
        year <- factor(year)                                                #turn year into a factor
        melt_snow[melt_snow=='N' | melt_snow=='Z' | melt_snow=='U'] <- NA   #code missing values for snowmelt
        melt_snow <- as.numeric(levels(melt_snow)[melt_snow])               #Now snowmelt can be turned into a number
        major <- ifelse(logsstormtot>threshold,1,0)                         #tag the events that are above the 90th percentile
        } )
        


    output_file = paste(substr(data_file,0,nchar(data_file)-4), "_out.csv", sep='')
    write.table(loading_data, file=output_file, sep=',', row.names=F, col.names=F)
}