## rent.dat, grossrent.dat
rent.dat <- read_excel(path="data/Data Wrangling SPSD Project.xlsx",
                       sheet=4,
                       range=anchored("A156",dim=c(10,12)),
                       col_names=T)
blocks <- unique(rent.dat$`Census Block`); blocks <- factor(blocks,levels=blocks)
schools <- c("Westside","Northside","CH Bird","Patrick Marsh")
schools <- factor(schools,levels=schools)
rent.cats <- (rent.dat %>% colnames)[5:12]
rent.cats <- factor(rent.cats,levels=rent.cats)

grossrent.dat <- tibble(
  population=as.vector(unlist(rent.dat[,5:12])),
  block=rep(blocks,8),
  school=rep(rep(schools,c(2,3,1,3)),8),
  "gross rent"=rep(rent.cats,each=9))

## costsm.dat, costswm.dat
costsm.dat <- read_excel(path="data/Data Wrangling SPSD Project.xlsx",
                         sheet=4,
                         range=anchored("A64",dim=c(10,8)),
                         col_names=T)
costswm.cats <- (costsm.dat %>% colnames)[4:8]
costswm.dat <- tibble(
  population=as.vector(unlist(costsm.dat[,4:8])),
  block=rep(blocks,5),
  school=rep(rep(schools,c(2,3,1,3)),5),
  "monthly costs (% total income)"=rep(costswm.cats,each=9))

## costsnm.dat, costswom.dat
costsnm.dat <- read_excel(path="data/Data Wrangling SPSD Project.xlsx",
                          sheet=4,
                          range=anchored("A94",dim=c(10,8)),
                          col_names=T)
costswom.cats <- (costsnm.dat %>% colnames)[4:8]
costswom.dat <- tibble(
  population=as.vector(unlist(costsnm.dat[,4:8])),
  block=rep(blocks,5),
  school=rep(rep(schools,c(2,3,1,3)),5),
  "monthly costs (% total income)"=rep(costswom.cats,each=9))

## remove unneeded variables
rm(rent.dat); rm(costsm.dat); rm(costsnm.dat)
rm(blocks); rm(costswm.cats); rm(costswom.cats); rm(rent.cats); rm(schools)