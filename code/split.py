import numpy as np
from matplotlib import mlab

infile = '../data/otter/ottercreek.csv'

data = np.loadtxt(infile, delimiter=',', skiprows=1, dtype=str)
data = np.array([s.strip() for row in data for s in row]).reshape(data.shape)

snow_rows = mlab.find(data[:,2]=='na')
nosnow_rows = filter(lambda x: x not in snow_rows, range(data.shape[0]))

snow = data[snow_rows,:]
no_snow = data[nosnow_rows,:]

np.savetxt(fname=infile[:-4] + '_snow.csv', X=snow, delimiter=',', fmt='%s')
np.savetxt(fname=infile[:-4] + '_nosnow.csv', X=no_snow, delimiter=',', fmt='%s')
