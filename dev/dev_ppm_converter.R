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
ddtt[,rat := mgm3/ppm]
