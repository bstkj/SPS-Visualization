source("loadDataHousing.R")

### gross rent
p.hous.rent.ws1 <- ggplot(grossrent.dat %>% filter(school=="Westside"), # westside (not combined)
       aes(x=`gross rent`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1.5,col="purple",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.hous.rent.ws2 <- ggplot(grossrent.dat %>% filter(school=="Westside") %>% # westside (combined)
         group_by(`gross rent`) %>% summarize(population=sum(population)),
       aes(x=`gross rent`,y=population,group="Westside")) +
  geom_point() + geom_line(alpha=0.5,size=2,col="purple") +
  theme(axis.text.x=element_text(vjust=0.8,angle=30))

p.hous.rent.ns1 <- ggplot(grossrent.dat %>% filter(school=="Northside"), # northside (not combined)
       aes(x=`gross rent`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1,col="green",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.hous.rent.ns2 <- ggplot(grossrent.dat %>% filter(school=="Northside") %>% # northside (combined)
         group_by(`gross rent`) %>% summarize(population=sum(population)),
       aes(x=`gross rent`,y=population,group="Northside")) +
  geom_point() + geom_line(alpha=0.5,size=2,col="green") +
  theme(axis.text.x=element_text(vjust=0.8,angle=30))

p.hous.rent.ch <- ggplot(grossrent.dat %>% filter(school=="CH Bird"), # ch bird
       aes(x=`gross rent`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="red") +
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.hous.rent.pm1 <- ggplot(grossrent.dat %>% filter(school=="Patrick Marsh"), # patrick marsh (not combined)
       aes(x=`gross rent`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1,col="blue",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.hous.rent.pm2 <- ggplot(grossrent.dat %>% filter(school=="Patrick Marsh") %>% # patrick marsh (combined)
         group_by(`gross rent`) %>% summarize(population=sum(population)),
       aes(x=`gross rent`,y=population,group="Patrick Marsh")) +
  geom_point() + geom_line(alpha=0.5,size=2,col="blue") +
  theme(axis.text.x=element_text(vjust=0.8,angle=30))

p.hous.rent.all1 <- ggplot(grossrent.dat, # all (not combined)
       aes(x=`gross rent`,y=population,group=block,col=school)) +
  geom_point() + geom_line(alpha=0.5,size=1) + 
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) + 
  theme(legend.position=c(0.2,1),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30),
        legend.background = element_blank())

p.hous.rent.all2 <- ggplot(grossrent.dat %>% group_by(school,`gross rent`) %>% # all (combined)
         summarize(population=sum(population)),
       aes(x=`gross rent`,y=population,group=school,col=school)) + 
  geom_point() + geom_line(alpha=0.5,size=1) +
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) + 
  theme(legend.position=c(0.2,1),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30),
        legend.background = element_blank())

### monthly owner costs (w/ mortgage)
p.hous.costswm.ws1 <- ggplot(costswm.dat %>% filter(school=="Westside"), # westside (not combined)
       aes(x=`monthly costs (% total income)`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1.5,col="purple",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.hous.costswm.ws2 <- ggplot(costswm.dat %>% filter(school=="Westside") %>% # westside (combined)
         group_by(`monthly costs (% total income)`) %>% summarize(population=sum(population)),
       aes(x=`monthly costs (% total income)`,y=population,group="Westside")) +
  geom_point() + geom_line(alpha=0.5,size=2,col="purple") +
  theme(axis.text.x=element_text(vjust=0.8,angle=30))

p.hous.costswm.ns1 <- ggplot(costswm.dat %>% filter(school=="Northside"), # northside (not combined)
       aes(x=`monthly costs (% total income)`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1,col="green",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.hous.costswm.ns2 <- ggplot(costswm.dat %>% filter(school=="Northside") %>% # northside (combined)
         group_by(`monthly costs (% total income)`) %>% summarize(population=sum(population)),
       aes(x=`monthly costs (% total income)`,y=population,group="Northside")) +
  geom_point() + geom_line(alpha=0.5,size=2,col="green") +
  theme(axis.text.x=element_text(vjust=0.8,angle=30))

p.hous.costswm.ch <- ggplot(costswm.dat %>% filter(school=="CH Bird"), # ch bird
       aes(x=`monthly costs (% total income)`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="red") +
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.hous.costswm.pm1 <- ggplot(costswm.dat %>% filter(school=="Patrick Marsh"), # patrick marsh (not combined)
       aes(x=`monthly costs (% total income)`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1,col="blue",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.hous.costswm.pm2 <- ggplot(costswm.dat %>% filter(school=="Patrick Marsh") %>% # patrick marsh (combined)
         group_by(`monthly costs (% total income)`) %>% summarize(population=sum(population)),
       aes(x=`monthly costs (% total income)`,y=population,group="Patrick Marsh")) +
  geom_point() + geom_line(alpha=0.5,size=2,col="blue") +
  theme(axis.text.x=element_text(vjust=0.8,angle=30))

p.hous.costswm.all1 <- ggplot(costswm.dat, # all (not combined)
       aes(x=`monthly costs (% total income)`,y=population,group=block,col=school)) +
  geom_point() + geom_line(alpha=0.5,size=1) + 
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) + 
  theme(legend.position=c(0.2,1),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30),
        legend.background = element_blank())

p.hous.costswm.all2 <- ggplot(costswm.dat %>% group_by(school,`monthly costs (% total income)`) %>% # all (combined)
         summarize(population=sum(population)),
       aes(x=`monthly costs (% total income)`,y=population,group=school,col=school)) + 
  geom_point() + geom_line(alpha=0.5,size=1) +
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) + 
  theme(legend.position=c(0.2,1),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30),
        legend.background = element_blank())

### monthly owner costs (w/o mortgage)
p.hous.costswom.ws1 <- ggplot(costswom.dat %>% filter(school=="Westside"), # westside (not combined)
       aes(x=`monthly costs (% total income)`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1.5,col="purple",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.hous.costswom.ws2 <- ggplot(costswom.dat %>% filter(school=="Westside") %>% # westside (combined)
         group_by(`monthly costs (% total income)`) %>% summarize(population=sum(population)),
       aes(x=`monthly costs (% total income)`,y=population,group="Westside")) +
  geom_point() + geom_line(alpha=0.5,size=2,col="purple") +
  theme(axis.text.x=element_text(vjust=0.8,angle=30))

p.hous.costswom.ns1 <- ggplot(costswom.dat %>% filter(school=="Northside"), # northside (not combined)
       aes(x=`monthly costs (% total income)`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1,col="green",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.hous.costswom.ns2 <- ggplot(costswom.dat %>% filter(school=="Northside") %>% # northside (combined)
         group_by(`monthly costs (% total income)`) %>% summarize(population=sum(population)),
       aes(x=`monthly costs (% total income)`,y=population,group="Northside")) +
  geom_point() + geom_line(alpha=0.5,size=2,col="green") +
  theme(axis.text.x=element_text(vjust=0.8,angle=30))

p.hous.costswom.ch <- ggplot(costswom.dat %>% filter(school=="CH Bird"), # ch bird
       aes(x=`monthly costs (% total income)`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="red") +
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.hous.costswom.pm1 <- ggplot(costswom.dat %>% filter(school=="Patrick Marsh"), # patrick marsh (not combined)
       aes(x=`monthly costs (% total income)`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1,col="blue",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.hous.costswom.pm2 <- ggplot(costswom.dat %>% filter(school=="Patrick Marsh") %>% # patrick marsh (combined)
         group_by(`monthly costs (% total income)`) %>% summarize(population=sum(population)),
       aes(x=`monthly costs (% total income)`,y=population,group="Patrick Marsh")) +
  geom_point() + geom_line(alpha=0.5,size=2,col="blue") +
  theme(axis.text.x=element_text(vjust=0.8,angle=30))

p.hous.costswom.all1 <- ggplot(costswom.dat, # all (not combined)
       aes(x=`monthly costs (% total income)`,y=population,group=block,col=school)) +
  geom_point() + geom_line(alpha=0.5,size=1) + 
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) + 
  theme(legend.position=c(0.2,1),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30),
        legend.background = element_blank())

p.hous.costswom.all2 <- ggplot(costswom.dat %>% group_by(school,`monthly costs (% total income)`) %>% # all (combined)
         summarize(population=sum(population)),
       aes(x=`monthly costs (% total income)`,y=population,group=school,col=school)) + 
  geom_point() + geom_line(alpha=0.5,size=1) +
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) + 
  theme(legend.position=c(0.2,1),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30),
        legend.background = element_blank())

### remove unneeded variables
rm(costswm.dat); rm(costswom.dat); rm(grossrent.dat)