import argparse
import datetime

#define how to process command-line arguments
parser = argparse.ArgumentParser(description='Parse a data file.')
parser.add_argument('files', metavar='file_name', type=str, nargs='+', help='the name of an input file')

#parse the command line arguments
args = parser.parse_args()

#process each file that was passed as input
for file in args.files:
    infile = open(file, 'r')
    outfile = open(file[:-4]+'_out'+file[-4:], 'w')
    
    first_line = True
    separator = ','
    
    for line in infile:
    
        #what to do with the header row
        if first_line:
            columns = line.strip().split(separator)
            columns = [item.strip().lower() for item in columns]
            columns[8] = 'measurement'
            
            #deal with the date and time columns...
            columns[columns.index('yearmo')] = 'date'
            columns[columns.index('dahr')] = 'time'            
            
            #Keep just the ones we want
            header = separator.join(columns[0:11])
            outfile.write(header+'\n')
            first_line=False
            
        else:
            row = line.strip().split(separator)
            row = [item.strip() for item in row]
            
            meta = row[:7]
            n_obs = (len(row)-7) // 4
            
            #casting year and month to integers will generate a ValueError when it encounters the row of dashes that follows the header.
            #this try-except block avoids writing theose dashes to the output file.
            try:
                year = int( row[columns.index('date')][:4] )
                month = int( row[columns.index('date')][4:] )
            
                for obs in range(n_obs):
                    data = row[7+4*obs:7+4*(obs+1)]
                    
                    #deal with the date and time                        
                    day = int( data[columns.index('time')-len(meta)][:2] )
                    hour = int( data[columns.index('time')-len(meta)][2:] )
                    
                    try:
                        #If datetime barfs on a nonexistent date, then exclude it from the output
                        date = datetime.date(year, month, day)
                        
                        #If datetime barfs because the time is invalid, change it to a valid time
                        #Done because missing values are coded as hour 99, which causes a ValueError.
                        try: time = datetime.time(hour=hour)
                        except ValueError: time = datetime.time(0)
                        
                        #Substitute the date and time into the proper fields (as strings)
                        meta[columns.index('date')] = str(date)
                        data[columns.index('time')-len(meta)] = str(time)

                        #Turn the lists of data into strings.
                        metadata = separator.join(meta)
                        data = separator.join(data)
                        
                        #Write the strings to the output
                        outfile.write(metadata+',')
                        outfile.write(data+'\n')
                            
                    #end up here if the date is not valid (e.g. 2/31). Invalid dates are not written to the output file.
                    except ValueError: pass
                        
            #end up here after encountering a row of dashes instead of data.
            except ValueError: pass
                    
    infile.close()
    outfile.close()