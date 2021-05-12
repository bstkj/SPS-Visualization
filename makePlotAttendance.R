# loads days.dat, grade.dat, month.dat, raceeth.dat
source("loadDataAttendance.R")

### attendance rate by days 
days <- factor(x=c("Mon","Tues","Wed","Thurs","Fri"),
               level=c("Mon","Tues","Wed","Thurs","Fri"))

p.att.day.ws <- ggplot(tibble("attendance rate"=days.dat[1,], # westside
              "days"=days),
       aes(x=days,y=`attendance rate`,group="Westside")) + 
  geom_point() + geom_line(alpha=0.5,size=2,aes(col="Westside")) + labs(col="schools") + 
  scale_color_manual(values=c("Westside"="purple")) + 
  theme(legend.position="none")

p.att.day.ns <- ggplot(tibble("attendance rate"=days.dat[2,], # northside
              "days"=days),
       aes(x=days,y=`attendance rate`,group="Northside")) + 
  geom_point() + geom_line(alpha=0.5,size=2,aes(col="Northside")) + labs(col="schools") + 
  scale_color_manual(values=c("Northside"="green")) +
  theme(legend.position="none")

p.att.day.ch <- ggplot(tibble("attendance rate"=days.dat[3,], # ch bird
              "days"=days),
       aes(x=days,y=`attendance rate`,group="CH Bird")) + 
  geom_point() + geom_line(alpha=0.5,size=2,aes(col="CH Bird")) + labs(col="schools") + 
  scale_color_manual(values=c("CH Bird"="red")) +
  theme(legend.position="none")

p.att.day.pm <- ggplot(tibble("attendance rate"=days.dat[4,], # patrick marsh
              "days"=days),
       aes(x=days,y=`attendance rate`,group="Patrick Marsh")) + 
  geom_point() + geom_line(alpha=0.5,size=2,aes(col="Patrick Marsh")) + labs(col="schools") +
  scale_color_manual(values=c("Patrick Marsh"="blue")) + 
  theme(legend.position="none")

schools <- factor(x=c("CH Bird","Westside","Northside","Patrick Marsh"),
                  level=c("CH Bird","Westside","Northside","Patrick Marsh"))

p.att.day.all <- ggplot(tibble("attendance rate"=as.vector(days.dat[1:4,]), # combined 
              "days"=rep(days,each=4),
              "schools"=rep(schools,5)),
       aes(x=days,y=`attendance rate`,group=schools)) + 
  geom_point() + geom_line(alpha=0.5,size=1,aes(color=schools)) + 
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) +
  labs(col="school") + 
  theme(legend.justification=c("right","top"),
        legend.background=element_blank())

### attendance rate by grade
grades <- factor(c("K",1:7),levels=c("K",1:7))
schools <- c("CH Bird","Westside","Northside","Patrick Marsh")

p.att.grade.ws <- ggplot(tibble("attendance rate"=grade.dat[1,1:6], # westside
              Grade=factor(c("K",1:5),levels=c("K",1:5))),
       aes(x=Grade,y=`attendance rate`,group="Westside")) +
  geom_point() + geom_line(alpha=0.5,size=2,aes(col="Westside")) + labs(col="schools") + 
  scale_color_manual(values=c("Westside"="purple")) + 
  theme(legend.position="none")

p.att.grade.ns <- ggplot(tibble("attendance rate"=grade.dat[2,1:6], # northside
              Grade=factor(c("K",1:5),levels=c("K",1:5))),
       aes(x=Grade,y=`attendance rate`,group="Northside")) +
  geom_point() + geom_line(alpha=0.5,size=2,aes(col="Northside")) + labs(col="schools") + 
  scale_color_manual(values=c("Northside"="green")) + 
  theme(legend.position="none")

p.att.grade.ch <- ggplot(tibble("attendance rate"=grade.dat[3,1:6], # ch bird
              Grade=factor(c("K",1:5),levels=c("K",1:5))),
       aes(x=Grade,y=`attendance rate`,group="CH Bird")) +
  geom_point() + geom_line(alpha=0.5,size=2,aes(col="CH Bird")) + labs(col="schools") + 
  scale_color_manual(values=c("CH Bird"="red")) + 
  theme(legend.position="none")

p.att.grade.pm <- ggplot(tibble("attendance rate"=grade.dat[4,7:8], # patrick marsh
              Grade=factor(c(6,7),levels=c(6,7))),
       aes(x=Grade,y=`attendance rate`,group="Patrick Marsh")) +
  geom_point() + geom_line(alpha=0.5,size=2,aes(col="Patrick Marsh")) + labs(col="schools") + 
  scale_color_manual(values=c("Patrick Marsh"="blue")) + 
  theme(legend.position="none")

p.att.grade.all <- ggplot(tibble(Grade=rep(grades,c(rep(3,6),1,1)), # combined 
              School=factor(c(rep(schools[1:3],6),rep(schools[4],2)),
                            levels=schools),
              "Attendance rate"=c(as.vector(grade.dat[1:3,1:6]),
                                  as.vector(grade.dat[4,7:8]))),
       aes(x=Grade,y=`Attendance rate`,group=School)) +
  geom_point() + geom_line(alpha=0.5,size=1,aes(col=School)) + 
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) +
  labs(col="school") + 
  theme(legend.justification=c("right","top"),
        legend.background=element_blank())

### attendance rate by month
months <- factor(c("Sep","Oct","Nov","Dec","Jan"),
                 level=c("Sep","Oct","Nov","Dec","Jan"))

p.att.month.ws <- ggplot(tibble("attendance rate"=month.dat[1,], # westside
              Month=months),
       aes(x=Month,y=`attendance rate`,group="Westside")) +
  geom_point() + geom_line(alpha=0.5,size=2,aes(col="Westside")) + labs(col="schools") + 
  scale_color_manual(values=c("Westside"="purple")) + 
  theme(legend.position="none")

p.att.month.ns <- ggplot(tibble("attendance rate"=month.dat[2,], # northside
              Month=months),
       aes(x=Month,y=`attendance rate`,group="Northside")) +
  geom_point() + geom_line(alpha=0.5,size=2,aes(col="Northside")) + labs(col="schools") + 
  scale_color_manual(values=c("Northside"="green")) + 
  theme(legend.position="none")

p.att.month.ch <- ggplot(tibble("attendance rate"=month.dat[3,], # ch bird
              Month=months),
       aes(x=Month,y=`attendance rate`,group="CH Bird")) +
  geom_point() + geom_line(alpha=0.5,size=2,aes(col="CH Bird")) + labs(col="schools") + 
  scale_color_manual(values=c("CH Bird"="red")) + 
  theme(legend.position="none")

p.att.month.pm <- ggplot(tibble("attendance rate"=month.dat[4,], # patrick marsh
              Month=months),
       aes(x=Month,y=`attendance rate`,group="Patrick Marsh")) +
  geom_point() + geom_line(alpha=0.5,size=2,aes(col="Patrick Marsh")) + labs(col="schools") + 
  scale_color_manual(values=c("Patrick Marsh"="blue")) + 
  theme(legend.position="none")

schools <- factor(rownames(month.dat)[1:4],
                  levels=rownames(month.dat)[1:4])
p.att.month.all <- ggplot(tibble(Month=rep(months,each=4),
              School=rep(schools,5),
              "Attendance rate"=as.vector(month.dat[1:4,])),
       aes(x=Month,y=`Attendance rate`,group=School)) +
  geom_point() + geom_line(alpha=0.5,size=1,aes(color=School)) + 
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) + 
  labs(col="school") + 
  theme(legend.justification=c("right","top"),
        legend.background=element_blank())

### attendance rate by race/eth
raceeth <- raceeth.dat %>% colnames; raceeth <- factor(raceeth,levels=raceeth)
p.att.raceeth.ws <- ggplot(tibble("attendance rate"=raceeth.dat[1,], # westside
              "race/eth"=raceeth),
       aes(x=`race/eth`,y=`attendance rate`,group="Westside")) + 
  geom_point(col="black") + geom_line(alpha=0.5,size=2,aes(col="Westside")) + labs(col="schools") +
  scale_color_manual(values=c("Westside"="purple")) +
  theme(legend.position="none",
        axis.text.x=element_text(vjust=0.8,angle=10))

p.att.raceeth.ns <- ggplot(tibble("attendance rate"=raceeth.dat[2,], # northside
              "race/eth"=raceeth),
       aes(x=`race/eth`,y=`attendance rate`,group="Northside")) + 
  geom_point(col="black") + geom_line(alpha=0.5,size=2,aes(col="Northside")) + labs(col="schools") +
  scale_color_manual(values=c("Northside"="green")) +
  theme(legend.position="none",
        axis.text.x=element_text(vjust=0.8,angle=10))

p.att.raceeth.ch <- ggplot(tibble("attendance rate"=raceeth.dat[3,], # ch bird
              "race/eth"=raceeth),
       aes(x=`race/eth`,y=`attendance rate`,group="CH Bird")) + 
  geom_point(col="black") + geom_line(alpha=0.5,size=2,aes(col="CH Bird")) + labs(col="schools") +
  scale_color_manual(values=c("CH Bird"="red")) +
  theme(legend.position="none",
        axis.text.x=element_text(vjust=0.8,angle=10))

p.att.raceeth.pm <- ggplot(tibble("attendance rate"=raceeth.dat[4,], # patrick marsh
              "race/eth"=raceeth),
       aes(x=`race/eth`,y=`attendance rate`,group="Patrick Marsh")) + 
  geom_point(col="black") + geom_line(alpha=0.5,size=2,aes(col="Patrick Marsh")) + labs(col="schools") +
  scale_color_manual(values=c("Patrick Marsh"="blue")) +
  theme(legend.position="none",
        axis.text.x=element_text(vjust=0.8,angle=10))

p.att.raceeth.all <- ggplot(tibble("attendance rate"=as.vector(raceeth.dat[1:4,]), # combined
              "race/eth"=rep(raceeth,each=4),
              School=rep(schools,7)),
       aes(x=`race/eth`,y=`attendance rate`,group=School)) + 
  geom_point() + geom_line(alpha=0.5,size=1,aes(color=School)) +
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) +
  labs(col="school") + 
  theme(legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=10),
        legend.background=element_blank())

### remove unneeded variables
rm(days.dat); rm(grade.dat); rm(month.dat); rm(raceeth.dat)
rm(days); rm(months); rm(raceeth); rm(schools); rm(grades)