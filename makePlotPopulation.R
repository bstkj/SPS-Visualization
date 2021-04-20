# loads agecat.dat, householdcat.dat, raceeth.dat
source("loadDataPopulation.R")

### population count by race/eth
p.pop.raceeth.ws1 <- ggplot(raceeth.dat %>% filter(school=="Westside"), # westside (not combined)
       aes(x=`race/eth`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1.5,col="purple",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=10))

p.pop.raceeth.ws2 <- ggplot(raceeth.dat %>% filter(school=="Westside") %>% # westside (combined)
                              group_by(`race/eth`) %>% summarize(population=sum(population)),
                           aes(x=`race/eth`,y=population,group="Westside")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="purple") +
  theme(axis.text.x=element_text(vjust=0.8,angle=10))

p.pop.raceeth.ns1 <- ggplot(raceeth.dat %>% filter(school=="Northside"), # northside (not combined)
       aes(x=`race/eth`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1,col="green",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=10))

p.pop.raceeth.ns2 <- ggplot(raceeth.dat %>% filter(school=="Northside") %>% # northside (combined)
                              group_by(`race/eth`) %>% summarize(population=sum(population)),
                            aes(x=`race/eth`,y=population,group="Northside")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="green") +
  theme(axis.text.x=element_text(vjust=0.8,angle=10))

p.pop.raceeth.ch <- ggplot(raceeth.dat %>% filter(school=="CH Bird"), # ch bird (not combined)
       aes(x=`race/eth`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="red",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=10))

# only showing census blocks exclusive to patrick marsh, omitting the blocks
# shared with northside and ch bird
p.pop.raceeth.pm1 <- ggplot(raceeth.dat %>% filter(school=="Patrick Marsh"), # patrick marsh (not combined)
       aes(x=`race/eth`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1,col="blue",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=10))

p.pop.raceeth.pm2 <- ggplot(raceeth.dat %>% filter(school=="Patrick Marsh") %>% # patrick marsh (combined)
                              group_by(`race/eth`) %>% summarize(population=sum(population)),
                            aes(x=`race/eth`,y=population,group="Westside")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="blue") +
  theme(axis.text.x=element_text(vjust=0.8,angle=10))

p.pop.raceeth.all1 <- ggplot(raceeth.dat, # all (not combined)
       aes(x=`race/eth`,y=population,group=block,col=school)) +
  geom_point() + geom_line(alpha=0.5,size=1) + 
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=10))

p.pop.raceeth.all2 <- ggplot(raceeth.dat %>% group_by(school,`race/eth`) %>% # all (combined)
                             summarize(population=sum(population)),
                           aes(x=`race/eth`,y=population,group=school,col=school)) + 
  geom_point() + geom_line(alpha=0.5,size=1) +
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=10),
        legend.background = element_blank())

### population count by age category
p.pop.agecat.ws1 <- ggplot(agecat.dat %>% filter(school=="Westside"), # westside (not combined)
       aes(x=`age cat`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1.5,col="purple",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.pop.agecat.ws2 <- ggplot(agecat.dat %>% filter(school=="Westside") %>% # westside (combined)
                              group_by(`age cat`) %>% summarize(population=sum(population)),
                            aes(x=`age cat`,y=population,group="Westside")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="purple") +
  theme(axis.text.x=element_text(vjust=0.8,angle=30))

p.pop.agecat.ns1 <- ggplot(agecat.dat %>% filter(school=="Northside"), # northside (not combined)
       aes(x=`age cat`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1,col="green",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.pop.agecat.ns2 <- ggplot(agecat.dat %>% filter(school=="Northside") %>% # northside (combined)
                              group_by(`age cat`) %>% summarize(population=sum(population)),
                            aes(x=`age cat`,y=population,group="Northside")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="green") +
  theme(axis.text.x=element_text(vjust=0.8,angle=30))

p.pop.agecat.ch <- ggplot(agecat.dat %>% filter(school=="CH Bird"), # ch bird (not combined)
       aes(x=`age cat`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="red",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.pop.agecat.pm1 <- ggplot(agecat.dat %>% filter(school=="Patrick Marsh"), # patrick marsh (not combined)
       aes(x=`age cat`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1,col="blue",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.pop.agecat.pm2 <- ggplot(agecat.dat %>% filter(school=="Patrick Marsh") %>% # patrick marsh (combined)
                              group_by(`age cat`) %>% summarize(population=sum(population)),
                            aes(x=`age cat`,y=population,group="Patrick Marsh")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="blue") +
  theme(axis.text.x=element_text(vjust=0.8,angle=30))

p.pop.agecat.all1 <- ggplot(agecat.dat, # all (not combined)
       aes(x=`age cat`,y=population,group=block,col=school)) +
  geom_point() + geom_line(alpha=0.5,size=1) + 
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.pop.agecat.all2 <- ggplot(agecat.dat %>% group_by(school,`age cat`) %>% # all (combined)
                               summarize(population=sum(population)),
                             aes(x=`age cat`,y=population,group=school,col=school)) + 
  geom_point() + geom_line(alpha=0.5,size=1) +
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30),
        legend.background = element_blank())

### frequency of household types 
p.pop.hhcat.ws1 <- ggplot(householdcat.dat %>% filter(school=="Westside"), # westside (not combined)
       aes(x=`household cat`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1.5,col="purple",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.pop.hhcat.ws2 <- ggplot(householdcat.dat %>% filter(school=="Westside") %>% # westside (combined)
                             group_by(`household cat`) %>% summarize(population=sum(population)),
                           aes(x=`household cat`,y=population,group="Westside")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="purple") +
  theme(axis.text.x=element_text(vjust=0.8,angle=30))

p.pop.hhcat.ns1 <- ggplot(householdcat.dat %>% filter(school=="Northside"), # northside (not combined)
       aes(x=`household cat`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1,col="green",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.pop.hhcat.ns2 <- ggplot(householdcat.dat %>% filter(school=="Northside") %>% # northside (combined)
                            group_by(`household cat`) %>% summarize(population=sum(population)),
                          aes(x=`household cat`,y=population,group="Northside")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="green") +
  theme(axis.text.x=element_text(vjust=0.8,angle=30))

p.pop.hhcat.ch <- ggplot(householdcat.dat %>% filter(school=="CH Bird"), # ch bird (not combined)
       aes(x=`household cat`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="red",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.pop.hhcat.pm1 <- ggplot(householdcat.dat %>% filter(school=="Patrick Marsh"), # patrick marsh (not combined)
       aes(x=`household cat`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1,col="blue",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.4,0.4),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.pop.hhcat.pm2 <- ggplot(householdcat.dat %>% filter(school=="Patrick Marsh") %>% # patrick marsh (combined)
                            group_by(`household cat`) %>% summarize(population=sum(population)),
                          aes(x=`household cat`,y=population,group="Patrick Marsh")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="blue") +
  theme(axis.text.x=element_text(vjust=0.8,angle=30))

p.pop.hhcat.all1 <- ggplot(householdcat.dat, # all (not combined) 
       aes(x=`household cat`,y=population,group=block,col=school)) +
  geom_point() + geom_line(alpha=0.5,size=1) + 
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) + 
  theme(legend.position=c(0.2,1),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30),
        legend.background = element_blank())

p.pop.hhcat.all2 <- ggplot(householdcat.dat %>% group_by(school,`household cat`) %>% # all (combined)
                              summarize(population=sum(population)),
                            aes(x=`household cat`,y=population,group=school,col=school)) + 
  geom_point() + geom_line(alpha=0.5,size=1) +
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) + 
  theme(legend.position=c(0.2,0.1),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30),
        legend.background = element_blank())

### remove unneeded variables
rm(agecat.dat); rm(householdcat.dat); rm(raceeth.dat)