# loads hhincome.dat, famincome.dat
source("loadDataIncome.R")

### population % of different household income categories
p.inc.hhinc.ws1 <- ggplot(hhincome.dat %>% filter(school=="Westside"), # westside (not combined)
       aes(x=`household income cat`,y=`population %`,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1.5,col="purple",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.inc.hhinc.ws2 <- ggplot(
  hhincome.dat %>% filter(school=="Westside") %>% # westside (combined)
    group_by(`household income cat`) %>% 
    summarize(`population %`=sum(`population %`)),
  aes(x=`household income cat`,y=`population %`,group="Westside")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="purple") +
  theme(axis.text.x=element_text(vjust=0.8,angle=30))

p.inc.hhinc.ns1 <- ggplot(hhincome.dat %>% filter(school=="Northside"), # northside (not combined)
       aes(x=`household income cat`,y=`population %`,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1,col="green",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.inc.hhinc.ns2 <- ggplot(
  hhincome.dat %>% filter(school=="Northside") %>% # northside (combined)
    group_by(`household income cat`) %>% 
    summarize(`population %`=sum(`population %`)),
  aes(x=`household income cat`,y=`population %`,group="Northside")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="green") +
  theme(axis.text.x=element_text(vjust=0.8,angle=30))

p.inc.hhinc.ch <- ggplot(hhincome.dat %>% filter(school=="CH Bird"), # ch bird
       aes(x=`household income cat`,y=`population %`,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="red",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

# possible data entry error? 011600-2 and 011506 have the exact same values.
p.inc.hhinc.pm1 <- ggplot(hhincome.dat %>% filter(school=="Patrick Marsh"), # patrick marsh (not combined)
       aes(x=`household income cat`,y=`population %`,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1,col="blue",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.97,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.inc.hhinc.pm2 <- ggplot(
  hhincome.dat %>% filter(school=="Patrick Marsh") %>% # patrick marsh (combined)
    group_by(`household income cat`) %>% 
    summarize(`population %`=sum(`population %`)),
  aes(x=`household income cat`,y=`population %`,group="Patrick Marsh")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="blue") +
  theme(axis.text.x=element_text(vjust=0.8,angle=30))

p.inc.hhinc.all1 <- ggplot(hhincome.dat, # all (not combined)
       aes(x=`household income cat`,y=`population %`,group=block,col=school)) +
  geom_point() + geom_line(alpha=0.5,size=1) + 
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) + 
  theme(legend.position=c(0.2,1),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30),
        legend.background = element_blank())

p.inc.hhinc.all2 <- ggplot(
  hhincome.dat %>% group_by(school,`household income cat`) %>% # all (combined)
    summarize(`population %`=sum(`population %`)),
  aes(x=`household income cat`,y=`population %`,group=school,col=school)) + 
  geom_point() + geom_line(alpha=0.5,size=1) +
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) +
  theme(legend.position=c(0.2,1),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30),
        legend.background = element_blank())

### population % of different family income categories
p.inc.faminc.ws1 <- ggplot(famincome.dat %>% filter(school=="Westside"), # westside (not combined)
       aes(x=`family income cat`,y=`population %`,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1.5,col="purple",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.inc.faminc.ws2 <- ggplot(
  famincome.dat %>% filter(school=="Westside") %>% # westside (combined)
    group_by(`family income cat`) %>% 
    summarize(`population %`=sum(`population %`)),
  aes(x=`family income cat`,y=`population %`,group="Westside")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="purple") +
  theme(axis.text.x=element_text(vjust=0.8,angle=30))

p.inc.faminc.ns1 <- ggplot(famincome.dat %>% filter(school=="Northside"), # northside (not combined)
       aes(x=`family income cat`,y=`population %`,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1,col="green",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.inc.faminc.ns2 <- ggplot(
  famincome.dat %>% filter(school=="Northside") %>% # northside (combined)
    group_by(`family income cat`) %>% 
    summarize(`population %`=sum(`population %`)),
  aes(x=`family income cat`,y=`population %`,group="Northside")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="green") +
  theme(axis.text.x=element_text(vjust=0.8,angle=30))

p.inc.faminc.ch <- ggplot(famincome.dat %>% filter(school=="CH Bird"), # ch bird
       aes(x=`family income cat`,y=`population %`,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="red",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

# possible data entry error? 011600-2 and 011506 have the exact same values.
p.inc.faminc.pm1 <- ggplot(famincome.dat %>% filter(school=="Patrick Marsh"), # patrick marsh (not combined)
       aes(x=`family income cat`,y=`population %`,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1,col="blue",aes(lty=block)) +
  labs(lty="Block") + 
  theme(legend.position=c(0.25,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30))

p.inc.faminc.pm2 <- ggplot(
  famincome.dat %>% filter(school=="Patrick Marsh") %>% # patrick marsh (combined)
    group_by(`family income cat`) %>% 
    summarize(`population %`=sum(`population %`)),
  aes(x=`family income cat`,y=`population %`,group="Patrick Marsh")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="blue") +
  theme(axis.text.x=element_text(vjust=0.8,angle=30))

p.inc.faminc.all1 <- ggplot(famincome.dat, # all (not combined) 
       aes(x=`family income cat`,y=`population %`,group=block,col=school)) +
  geom_point() + geom_line(alpha=0.5,size=1) + 
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) + 
  theme(legend.position=c(0.2,1),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30),
        legend.background = element_blank())

p.inc.faminc.all2 <- ggplot(
  famincome.dat %>% group_by(school,`family income cat`) %>% # all (combined)
    summarize(`population %`=sum(`population %`)),
  aes(x=`family income cat`,y=`population %`,group=school,col=school)) + 
  geom_point() + geom_line(alpha=0.5,size=1) +
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) +
  theme(legend.position=c(0.2,1),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=30),
        legend.background = element_blank())

### remove unneeded variables
rm(famincome.dat); rm(hhincome.dat)