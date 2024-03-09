
# load the model
source('./src/PPA.R')

#crown radius = 0.5*dbh^0.62; dbh in cm and crown radius in m
#crown area = phi*dbh^theta  (dbh in cm, crown area in m2)
phi = round(pi*.5^2,4); theta = 1.24

PA = 10000 # m2; 1 ha 
deltaT = 5 # years; model timestep
dnot = 1 # recruit initial dbh; 1cm
maxT = 200*deltaT # simulation time (1000 years)

mincohortN = 0.001 # minimum cohort size to keep track of
cutT = deltaT # how often to output statistics of the forest (can be in multiples of deltaT)

# run the model
run('./run/PPA_5PFTs_4years/')
