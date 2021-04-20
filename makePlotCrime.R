# loads subsCrime.dat, possCrime.dat, violCrime.dat
source("loadDataCrime.R")

### instance count by category of substance-related crime
p.crime.subs.ws <- ggplot(subsCrime.dat %>% filter(school=="Westside"), # westside
       aes(x=`crime cat`,y=`no. instances`,fill="Westside")) +
  geom_col(alpha=0.5,col=NA,width=0.5) +
  scale_fill_manual(values=c("Westside"="purple")) + 
  labs(fill="schools") + 
  theme(legend.position="none")

p.crime.subs.ns <- ggplot(subsCrime.dat %>% filter(school=="Northside"), # northside
       aes(x=`crime cat`,y=`no. instances`,fill="Northside")) +
  geom_col(alpha=0.5,col=NA,width=0.5) +
  scale_fill_manual(values=c("Northside"="green")) + 
  labs(fill="schools") + 
  theme(legend.position="none")

p.crime.subs.ch <- ggplot(subsCrime.dat %>% filter(school=="CH Bird"), # ch bird
       aes(x=`crime cat`,y=`no. instances`,fill="CH Bird")) +
  geom_col(alpha=0.5,col=NA,width=0.5) +
  scale_fill_manual(values=c("CH Bird"="red")) + 
  labs(fill="schools") + 
  theme(legend.position="none")

p.crime.subs.pm <- ggplot(subsCrime.dat %>% filter(school=="Patrick Marsh"), # patrick marsh
       aes(x=`crime cat`,y=`no. instances`,fill="Patrick Marsh")) +
  geom_col(alpha=0.5,col=NA,width=0.5) +
  scale_fill_manual(values=c("Patrick Marsh"="blue")) + 
  labs(fill="schools") + 
  theme(legend.position="none")

p.crime.subs.all <- ggplot(subsCrime.dat, # all
       aes(x=`crime cat`,y=`no. instances`,fill=school)) + 
  geom_col(alpha=0.5,col=NA,width=0.5,position="dodge") + 
  scale_fill_manual(values=c("CH Bird"="red","Westside"="purple",
                             "Northside"="green","Patrick Marsh"="blue")) + 
  theme(legend.position=c(0.5,0.95),legend.justification=c("right","top"))


### instance count by category of possessions-related crime 
p.crime.poss.ws <- ggplot(possCrime.dat %>% filter(school=="Westside"), # westside
       aes(x=`crime cat`,y=`no. instances`,fill="Westside")) +
  geom_col(alpha=0.5,col=NA,width=0.5) +
  scale_fill_manual(values=c("Westside"="purple")) + 
  labs(fill="schools") + 
  theme(legend.position="none",
        axis.text.x=element_text(vjust=0.8,angle=10))

p.crime.poss.ns <- ggplot(possCrime.dat %>% filter(school=="Northside"), # northside
       aes(x=`crime cat`,y=`no. instances`,fill="Northside")) +
  geom_col(alpha=0.5,col=NA,width=0.5) +
  scale_fill_manual(values=c("Northside"="green")) + 
  labs(fill="schools") + 
  theme(legend.position="none",
        axis.text.x=element_text(vjust=0.8,angle=10))

p.crime.poss.ch <- ggplot(possCrime.dat %>% filter(school=="CH Bird"), # ch bird
       aes(x=`crime cat`,y=`no. instances`,fill="CH Bird")) +
  geom_col(alpha=0.5,col=NA,width=0.5) +
  scale_fill_manual(values=c("CH Bird"="red")) + 
  labs(fill="schools") + 
  theme(legend.position="none",
        axis.text.x=element_text(vjust=0.8,angle=10))

p.crime.poss.pm <- ggplot(possCrime.dat %>% filter(school=="Patrick Marsh"), # patrick marsh
       aes(x=`crime cat`,y=`no. instances`,fill="Patrick Marsh")) +
  geom_col(alpha=0.5,col=NA,width=0.5) +
  scale_fill_manual(values=c("Patrick Marsh"="blue")) + 
  labs(fill="schools") + 
  theme(legend.position="none",
        axis.text.x=element_text(vjust=0.8,angle=10))

p.crime.poss.all <- ggplot(possCrime.dat, # all
       aes(x=`crime cat`,y=`no. instances`,fill=school)) + 
  geom_col(alpha=0.5,col=NA,width=0.5,position="dodge") + 
  scale_fill_manual(values=c("CH Bird"="red","Westside"="purple",
                             "Northside"="green","Patrick Marsh"="blue")) + 
  theme(legend.position=c(0.2,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=10))

### instance count by category of violence-related crime
p.crime.viol.ws <- ggplot(violCrime.dat %>% filter(school=="Westside"), # westside
       aes(x=`crime cat`,y=`no. instances`,fill="Westside")) +
  geom_col(alpha=0.5,col=NA,width=0.5) +
  scale_fill_manual(values=c("Westside"="purple")) + 
  labs(fill="schools") + 
  theme(legend.position="none")

p.crime.viol.ns <- ggplot(violCrime.dat %>% filter(school=="Northside"), # northside
       aes(x=`crime cat`,y=`no. instances`,fill="Northside")) +
  geom_col(alpha=0.5,col=NA,width=0.5) +
  scale_fill_manual(values=c("Northside"="green")) + 
  labs(fill="schools") + 
  theme(legend.position="none")

p.crime.viol.ch <- ggplot(violCrime.dat %>% filter(school=="CH Bird"), # ch bird
       aes(x=`crime cat`,y=`no. instances`,fill="CH Bird")) +
  geom_col(alpha=0.5,col=NA,width=0.5) +
  scale_fill_manual(values=c("CH Bird"="red")) + 
  labs(fill="schools") + 
  theme(legend.position="none")

p.crime.viol.pm <- ggplot(violCrime.dat %>% filter(school=="Patrick Marsh"), # patrick marsh
       aes(x=`crime cat`,y=`no. instances`,fill="Patrick Marsh")) +
  geom_col(alpha=0.5,col=NA,width=0.5) +
  scale_fill_manual(values=c("Patrick Marsh"="blue")) + 
  labs(fill="schools") + 
  theme(legend.position="none")

p.crime.viol.all <- ggplot(violCrime.dat, # all
       aes(x=`crime cat`,y=`no. instances`,fill=school)) + 
  geom_col(alpha=0.5,col=NA,width=0.5,position="dodge") + 
  scale_fill_manual(values=c("CH Bird"="red","Westside"="purple",
                             "Northside"="green","Patrick Marsh"="blue")) + 
  theme(legend.position=c(0.2,0.95),legend.justification=c("right","top"))

### remove unneeded variables
rm(possCrime.dat); rm(subsCrime.dat); rm(violCrime.dat)