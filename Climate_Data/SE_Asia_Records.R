# Stations of Climate Data Downloaded

file = "/home/CAMPUS/mwl04747/github/EA031/Climate_Data/East_Asia_Combined.csv"

import = read.csv(file)

head(import)

names(import)

levels(import$NAME)

# Requested

YANGON INTERNATIONAL, BM (Station ID: GHCND:BMM00048097)
MANILA (Station ID: GHCND:RPW00041229)
KUALA LUMPUR INTERNATIONAL, MY (Station ID: GHCND:MYM00048650)
JAKARTA TANJUNG PRIOK (Station ID: GHCND:IDM00096741)
SAIGON, VM (Station ID: GHCND:VMW00041008)
DANANG INTERNATIONAL, VM (Station ID: GHCND:VMM00048855)
VIENTIANE, LA (Station ID: GHCND:LAW00041051)
BANGKOK METROPOLIS, TH (GHCND:TH000048455)
SEOUL CITY, KS (GHCND:KSM00047108)
SEOUL ROK K 14, KS (GHCND:KSW00043201)
NANJING, CH (GHCND:CHM00058238)
TOKYO, JA (GHCND:JA000047662)
BEIJING, CH (GHCND:CHM00054511)
SINGAPORE, AS (GHCND:ASN00054094)
SINGAPORE CHANGI INTERNATIONAL, SN (GHCND:SNM00048698)
SHANGHAI HONGQIAO, CH (GHCND:CHM00058367)

plot(import$TMAX ~ import$DATE)

str(import)

# Doesn't work, not sure why...

import$NewDate <- as.Date(import$DATE, format="%m/%d/Y%")
# Note that DATE is a factor that's going to be a problem.
library(lubridate)
mydate <- mdy(import$DATE)
mydate
# Convert Dates to Date Format
import$NewDate <- mydate; head(import)

plot(import$TMAX ~ import$DATE)



# Selecting Stations

beijing = dplyr::filter(import, NAME == "BEIJING, CH")

plot(TMAX ~ NewDate, beijing)

beijing$Month = format(as.Date(beijing$NewDate), format = "%m")
beijing$Year = format(beijing$NewDate, format="%Y")

MonthlyTMAXMean = aggregate(TMAX ~ Month + Year, beijing, mean)
MonthlyTMAXMean$YEAR = as.numeric(MonthlyTMAXMean$Year)
MonthlyTMAXMean$MONTH = as.numeric(MonthlyTMAXMean$Month)

head(MonthlyTMAXMean)

plot(MonthlyTMAXMean$TMAX, ty='l')

month = "05"

plot(TMAX~YEAR, data=MonthlyTMAXMean[MonthlyTMAXMean$Month==month,],
     ty='l', xlim=c(1901, 2020))
May.lm <- lm(TMAX~YEAR, data=MonthlyTMAXMean[MonthlyTMAXMean$Month==month,])
summary(May.lm)
abline(coef(May.lm))





