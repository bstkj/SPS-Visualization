### grade.dat
schools <- c("Westside","Northside","CH Bird","Patrick Marsh","Prairie Phoenix")
grade.dat <- read_excel(path="data/Data Wrangling SPSD Project.xlsx",
                        sheet=1,
                        range=anchored("B6",dim=c(13,5)),
                        col_names=F) %>% t
rownames(grade.dat) <- schools; colnames(grade.dat) <- c("K",1:12)

### days.dat
days.dat <- read_excel(path="data/Data Wrangling SPSD Project.xlsx",
                       sheet=1,
                       range=anchored("B20",dim=c(5,5)),
                       col_names=F) %>% t
# the final "transpose" step coerces it from tibble to array
rownames(days.dat) <- schools
colnames(days.dat) <- c("Mon","Tues","Wed","Thurs","Fri")
# assigning row/col names does not affect class

### month.dat
month.dat <- read_excel(path="data/Data Wrangling SPSD Project.xlsx",
                        sheet=1,
                        range=anchored("B26",dim=c(5,5)),
                        col_names=F) %>% t
rownames(month.dat) <- schools
colnames(month.dat) <- c("Sep","Oct","Nov","Dec","Jan")

### raceeth.dat
raceeth.dat <- read_excel(path="data/Data Wrangling SPSD Project.xlsx",
                          sheet=1,
                          range=anchored("B32",dim=c(7,5)),
                          col_names=F) %>% t
rownames(raceeth.dat) <- schools
colnames(raceeth.dat) <- c("Native Hawaiian","Black","White","Hispanic/Latino",
                           "Asian","Two or more Races","American Indian")

### remove unneeded variables (only keep days.dat, grade.dat, month.dat, raceeth.dat)
rm(schools)