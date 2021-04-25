# loads hhincome.dat, famincome.dat
source("loadDataIncome.R")

### population % of different household income categories
income.brackets <- c("<=10","10-20","20-30","30-40","40-50","50-75",
                     "75-100","100-150","150-200",">=200")

p.inc.hhinc.ws1 <- ggplot(hhincome.dat %>% filter(school=="Westside"), # westside (not combined)
       aes(x=`household income cat`,y=`population %`,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1.5,col="purple",aes(lty=block)) +
  labs(lty="census block group",x="income bracket (x$1000)") + scale_x_discrete(labels=income.brackets) + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background=element_blank())

p.inc.hhinc.ws2 <- ggplot(
  hhincome.dat %>% filter(school=="Westside") %>% # westside (combined)
    group_by(`household income cat`) %>% 
    summarize(`population %`=sum(`population %`)),
  aes(x=`household income cat`,y=`population %`,group="Westside")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="purple") + 
  scale_x_discrete(labels=income.brackets) + labs(x="income bracket (x$1000)") +  
  theme(axis.text.x=element_text(vjust=0,angle=0))

p.inc.hhinc.ns1 <- ggplot(hhincome.dat %>% filter(school=="Northside"), # northside (not combined)
       aes(x=`household income cat`,y=`population %`,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1,col="green",aes(lty=block)) +
  labs(lty="census block group",x="income bracket (x$1000)") + scale_x_discrete(labels=income.brackets) + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background=element_blank())

p.inc.hhinc.ns2 <- ggplot(
  hhincome.dat %>% filter(school=="Northside") %>% # northside (combined)
    group_by(`household income cat`) %>% 
    summarize(`population %`=sum(`population %`)),
  aes(x=`household income cat`,y=`population %`,group="Northside")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="green") + 
  labs(x="income bracket (x$1000)") + 
  scale_x_discrete(labels=income.brackets)
  theme(axis.text.x=element_text(vjust=0,angle=0))

p.inc.hhinc.ch <- ggplot(hhincome.dat %>% filter(school=="CH Bird"), # ch bird
       aes(x=`household income cat`,y=`population %`,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="red",aes(lty=block)) +
  labs(lty="census block group",x="income bracket (x$1000)") + scale_x_discrete(labels=income.brackets) + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background=element_blank())

# possible data entry error? 011600-2 and 011506 have the exact same values.
p.inc.hhinc.pm1 <- ggplot(hhincome.dat %>% filter(school=="Patrick Marsh"), # patrick marsh (not combined)
       aes(x=`household income cat`,y=`population %`,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1,col="blue",aes(lty=block)) +
  labs(lty="census block group",x="income bracket (x$1000)") + scale_x_discrete(labels=income.brackets) + 
  theme(legend.position=c(0.97,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background=element_blank())

p.inc.hhinc.pm2 <- ggplot(
  hhincome.dat %>% filter(school=="Patrick Marsh") %>% # patrick marsh (combined)
    group_by(`household income cat`) %>% 
    summarize(`population %`=sum(`population %`)),
  aes(x=`household income cat`,y=`population %`,group="Patrick Marsh")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="blue") +
  scale_x_discrete(labels=income.brackets) + labs(x="income bracket (x$1000)") + 
  theme(axis.text.x=element_text(vjust=0,angle=0))

p.inc.hhinc.all1 <- ggplot(hhincome.dat, # all (not combined)
       aes(x=`household income cat`,y=`population %`,group=block)) +
  geom_point() + geom_line(alpha=0.5,size=1,aes(col=school)) + 
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) + 
  scale_x_discrete(labels=income.brackets) + labs(x="income bracket (x$1000)") + 
  theme(legend.position=c(0.2,1),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background = element_blank())

p.inc.hhinc.all2 <- ggplot(
  hhincome.dat %>% group_by(school,`household income cat`) %>% # all (combined)
    summarize(`population %`=sum(`population %`)),
  aes(x=`household income cat`,y=`population %`,group=school)) + 
  geom_point() + geom_line(alpha=0.5,size=1,aes(col=school)) +
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) +
  scale_x_discrete(labels=income.brackets) + labs(x="income bracket (x$1000)") +
  theme(legend.position=c(0.2,1),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background = element_blank())

### population % of different family income categories
income.brackets <- c("<=10","10-20","20-30","30-40","40-50","50-75",
                     "75-100","100-150","150-200",">=200")

p.inc.faminc.ws1 <- ggplot(famincome.dat %>% filter(school=="Westside"), # westside (not combined)
       aes(x=`family income cat`,y=`population %`,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1.5,col="purple",aes(lty=block)) +
  labs(lty="census block group",x="income bracket (x$1000)") + scale_x_discrete(labels=income.brackets) + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background=element_blank())

p.inc.faminc.ws2 <- ggplot(
  famincome.dat %>% filter(school=="Westside") %>% # westside (combined)
    group_by(`family income cat`) %>% 
    summarize(`population %`=sum(`population %`)),
  aes(x=`family income cat`,y=`population %`,group="Westside")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="purple") + 
  scale_x_discrete(labels=income.brackets) + labs(x="income bracket (x$1000)") + 
  theme(axis.text.x=element_text(vjust=0,angle=0))

p.inc.faminc.ns1 <- ggplot(famincome.dat %>% filter(school=="Northside"), # northside (not combined)
       aes(x=`family income cat`,y=`population %`,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1,col="green",aes(lty=block)) +
  labs(lty="census block group",x="income bracket (x$1000)") + scale_x_discrete(labels=income.brackets) + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background=element_blank())

p.inc.faminc.ns2 <- ggplot(
  famincome.dat %>% filter(school=="Northside") %>% # northside (combined)
    group_by(`family income cat`) %>% 
    summarize(`population %`=sum(`population %`)),
  aes(x=`family income cat`,y=`population %`,group="Northside")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="green") + 
  scale_x_discrete(labels=income.brackets) + labs(x="income bracket (x$1000)") + 
  theme(axis.text.x=element_text(vjust=0,angle=0))

p.inc.faminc.ch <- ggplot(famincome.dat %>% filter(school=="CH Bird"), # ch bird
       aes(x=`family income cat`,y=`population %`,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="red",aes(lty=block)) +
  labs(lty="census block group",x="income bracket (x$1000)") + scale_x_discrete(labels=income.brackets) + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background=element_blank())

# possible data entry error? 011600-2 and 011506 have the exact same values.
p.inc.faminc.pm1 <- ggplot(famincome.dat %>% filter(school=="Patrick Marsh"), # patrick marsh (not combined)
       aes(x=`family income cat`,y=`population %`,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1,col="blue",aes(lty=block)) +
  labs(lty="census block group",x="income bracket (x$1000)") + scale_x_discrete(labels=income.brackets) + 
  theme(legend.position=c(0.25,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background=element_blank())

p.inc.faminc.pm2 <- ggplot(
  famincome.dat %>% filter(school=="Patrick Marsh") %>% # patrick marsh (combined)
    group_by(`family income cat`) %>% 
    summarize(`population %`=sum(`population %`)),
  aes(x=`family income cat`,y=`population %`,group="Patrick Marsh")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="blue") + 
  scale_x_discrete(labels=income.brackets) + labs(x="income bracket (x$1000)") + 
  theme(axis.text.x=element_text(vjust=0,angle=0))

p.inc.faminc.all1 <- ggplot(famincome.dat, # all (not combined) 
       aes(x=`family income cat`,y=`population %`,group=block)) +
  geom_point() + geom_line(alpha=0.5,size=1,aes(col=school)) + 
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) + 
  scale_x_discrete(labels=income.brackets) + labs(x="income bracket (x$1000)") + 
  theme(legend.position=c(0.2,1),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background = element_blank())

p.inc.faminc.all2 <- ggplot(
  famincome.dat %>% group_by(school,`family income cat`) %>% # all (combined)
    summarize(`population %`=sum(`population %`)),
  aes(x=`family income cat`,y=`population %`,group=school)) + 
  geom_point() + geom_line(alpha=0.5,size=1,aes(col=school)) +
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) + 
  scale_x_discrete(labels=income.brackets) + labs(x="income bracket (x$1000)") + 
  theme(legend.position=c(0.2,1),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background = element_blank())

### remove unneeded variables
rm(famincome.dat); rm(hhincome.dat)
rm(income.brackets)