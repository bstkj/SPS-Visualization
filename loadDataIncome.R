### household.dat, hhincome.dat 
household.dat <-  read_excel(path="data/Data Wrangling SPSD Project.xlsx",
                             sheet=5,
                             range=anchored("A19",dim=c(10,12)),
                             col_names=T)
# population % in different household income categories
blocks <- c("011506-2","011505-2", # westside
            "011505-1","011505-3","011506-3", # northside, patrick marsh
            "011504-1", # ch bird, patrick marsh
            "011600-2","011700-2","011506-1") # patrick marsh
blocks <- factor(blocks,levels=blocks)
schools <- c("Westside","Northside","CH Bird","Patrick Marsh")
schools <- factor(schools,level=schools)
hhincome.cat <- (household.dat %>% names)[3:12]
hhincome.cat <- factor(hhincome.cat,levels=hhincome.cat)

hhincome.dat <- tibble(block=rep(blocks,10),
                       "population %"=as.vector(unlist(household.dat[,3:12])),
                       "household income cat"=rep(hhincome.cat,each=9),
                       school=rep(rep(schools,c(2,3,1,3)),10))

### family.dat, famincome.dat
family.dat <- read_excel(path="data/Data Wrangling SPSD Project.xlsx",
                         sheet=5,
                         range=anchored("A34",dim=c(10,12)),
                         col_names=T)
# population % in different family income categories
famincome.cat <- (family.dat %>% names)[3:12]
famincome.cat <- factor(famincome.cat,levels=famincome.cat)

famincome.dat <- tibble(block=rep(blocks,10),
                        "population %"=as.vector(unlist(family.dat[,3:12])),
                        "family income cat"=rep(famincome.cat,each=9),
                        school=rep(rep(schools,c(2,3,1,3)),10))

### remove unneeded variables
rm(family.dat); rm(household.dat)
rm(blocks); rm(famincome.cat); rm(hhincome.cat); rm(schools)