total18_19_20.dat <- read_excel(path="data/Data Wrangling SPSD Project.xlsx",
                                sheet=6,
                                range=anchored("A2",dim=c(5,36)),
                                col_names=T)

### subsCrime.dat 
subs.cat <- (total18_19_20.dat %>% names)[c(3,16,17)]
subs.cat <- factor(subs.cat,levels=subs.cat)
schools <- c("Westside","Northside","CH Bird","Patrick Marsh")
schools <- factor(schools,level=schools)

subsCrime.dat <- tibble("no. instances"=as.vector(unlist(total18_19_20.dat[,c(3,16,17)])),
                        "crime cat"=rep(subs.cat,each=4),
                        school=rep(schools,3))

### possCrime.dat
poss.cat <- (total18_19_20.dat %>% names)[c(9,10,11,25,26,27,30)] 
poss.cat <- factor(poss.cat,levels=poss.cat)

possCrime.dat <- tibble(
  "no. instances"=as.vector(unlist(total18_19_20.dat[,c(9,10,11,25,26,27,30)])),
  "crime cat"=rep(poss.cat,each=4),
  school=rep(schools,7))

### violCrime.dat
viol.cat <- (total18_19_20.dat %>% names)[c(6,7,36)]
viol.cat <- factor(viol.cat,levels=viol.cat)

violCrime.dat <- tibble(
  "no. instances"=as.vector(unlist(total18_19_20.dat[,c(6,7,36)])),
  "crime cat"=rep(viol.cat,each=4),
  school=rep(schools,3))

### remove unneeded variables
rm(total18_19_20.dat)
rm(poss.cat); rm(schools); rm(subs.cat); rm(viol.cat)