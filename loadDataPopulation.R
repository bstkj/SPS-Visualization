### race.dat, raceeth.dat
race.dat <- read_excel(path="data/Data Wrangling SPSD Project.xlsx",
                       sheet=2,
                       range=anchored("A2",dim=c(10,10)),
                       col_names=T)

blocks <- c("011506-2","011505-2", # westside
            "011505-1","011505-3","011506-3", # northside, patrick marsh
            "011504-1", # ch bird, patrick marsh
            "011600-2","011700-2","011506-1") # patrick marsh
blocks <- factor(blocks,levels=blocks)
schools <- c("Westside","Northside","CH Bird","Patrick Marsh")
schools <- factor(schools,level=schools)
raceeth <- (race.dat %>% colnames)[4:10]; raceeth[5] <- "Native" # more concise
raceeth <- factor(raceeth,levels=raceeth)
raceeth.dat <- tibble(block=rep(blocks,7),
                      population=as.vector(unlist(race.dat[,4:10])),
                      "race/eth"=rep(raceeth,each=9),
                      school=rep(rep(schools,c(2,3,1,3)),7))

### age.dat, agecat.dat
age.dat <- read_excel(path="data/Data Wrangling SPSD Project.xlsx",
                      sheet=2,
                      range=anchored("A32",dim=c(10,17)),
                      col_names=T)

agecat <- (age.dat %>% colnames)[6:17]; agecat <- factor(agecat,levels=agecat)
agecat.dat <- tibble(block=rep(blocks,12),
                     population=as.vector(unlist(age.dat[,6:17])),
                     "age cat"=rep(agecat,each=9),
                     school=rep(rep(schools,c(2,3,1,3)),12))


### family.dat
family.dat <- read_excel(path="data/Data Wrangling SPSD Project.xlsx",
                         sheet=2,
                         range=anchored("A47",dim=c(10,10)),
                         col_names=T)

householdcat <- (family.dat %>% colnames)[c(5,6,7,9,10)]
householdcat <- factor(householdcat,levels=householdcat)

householdcat.dat <- tibble(block=rep(blocks,5),
                           population=as.vector(unlist(family.dat[,c(5,6,7,9,10)])),
                           "household cat"=rep(householdcat,each=9),
                           school=rep(rep(schools,c(2,3,1,3)),5))

### remove unneeded variables
rm(age.dat); rm(family.dat); rm(race.dat)
rm(agecat); rm(blocks); rm(raceeth); rm(schools); rm(householdcat)