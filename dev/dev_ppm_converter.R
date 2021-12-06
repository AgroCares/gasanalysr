library(data.table)
dtm <- read.delim('inst/extdata/mgm3_25c.meas', skip = 6) |> setDT()
dtp <- read.delim('inst/extdata/ppm.meas', skip = 6) |> setDT()

dtt <- merge(dtm, dtp, by = 'Timestamp')
setnames(dtt, names(dtt), tolower(names(dtt)))

dtt <- melt(dtt, id.vars = 'timestamp')

# add gass
dtt[,gas:= substr(variable, 1,3)]
# add concunit
dtt[,c := fifelse(grepl('ppm', variable),'ppm', 'mgm3')]

# molecular weight table
mw.dt <- data.table(gas = c('co2', 'h2o', 'n2o', 'nh3'),
                    molarmass = c(44.0098, 18.0152, 44.01288, 17.03044))

# add molar mass to dtt
dtt <- merge(dtt, mw.dt, by = 'gas')

# dcast
ddtt <- dcast(dtt, timestamp + gas + molarmass ~ c, value.var = 'value')

# calculate mgm3 from ppm
ddtt[,t1 := 0.0409*ppm*molarmass]

# calculate verhouding
ddtt[,rat := round(mgm3/ppm,3)]

# calculate difference between calculated and measured mgm3
ddtt[,dif_t1 := mgm3-t1]
# calculated difference percentage
ddtt[,dif_t1_fr := dif_t1/mgm3*100]

# calculate R for 25C at 1 atm  with V = RT/P
# molar gas constant
mgc <- 8.314462618
tk <- 298.15
P <- 1*1.01325*10^5 #(convert atm to Pa)

R <- (mgc*tk)/P

test <- P/(0.08206*298)
P*1/mgc/tk

# the ideal gas law is PV = nRT
# where P is pressure (Pa)
# V = volumne
# n = number of moles
# R = universal gas constant (8.314462618)
# T = temperature in Kelvin
#
# to get the conversion factor 0.0409 above == P*V/R/T/1000 = mmol/m3
cv <- P*1/mgc/tk/1000
