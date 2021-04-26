# loads subsCrime.dat, possCrime.dat, violCrime.dat
source("loadDataCrime.R")

### instance count by category of substance-related crime
crime.categories <- c("Alcohol violation","DUI","Drugs/Narcotics violation")

p.crime.subs.ws <- ggplot(subsCrime.dat %>% filter(school=="Westside"), # westside
       aes(x=`crime cat`,y=`no. instances`,fill="Westside")) +
  geom_col(alpha=0.5,col=NA,width=0.5) +
  scale_fill_manual(values=c("Westside"="purple")) + 
  scale_x_discrete(labels=crime.categories) + 
  labs(x="crime category") + 
  theme(legend.position="none")

p.crime.subs.ns <- ggplot(subsCrime.dat %>% filter(school=="Northside"), # northside
       aes(x=`crime cat`,y=`no. instances`,fill="Northside")) +
  geom_col(alpha=0.5,col=NA,width=0.5) +
  scale_fill_manual(values=c("Northside"="green")) + 
  scale_x_discrete(labels=crime.categories) + 
  labs(x="crime category") + 
  theme(legend.position="none")

p.crime.subs.ch <- ggplot(subsCrime.dat %>% filter(school=="CH Bird"), # ch bird
       aes(x=`crime cat`,y=`no. instances`,fill="CH Bird")) +
  geom_col(alpha=0.5,col=NA,width=0.5) +
  scale_fill_manual(values=c("CH Bird"="red")) + 
  scale_x_discrete(labels=crime.categories) + 
  labs(x="crime category") + 
  theme(legend.position="none")

p.crime.subs.pm <- ggplot(subsCrime.dat %>% filter(school=="Patrick Marsh"), # patrick marsh
       aes(x=`crime cat`,y=`no. instances`,fill="Patrick Marsh")) +
  geom_col(alpha=0.5,col=NA,width=0.5) +
  scale_fill_manual(values=c("Patrick Marsh"="blue")) + 
  scale_x_discrete(labels=crime.categories) + 
  labs(x="crime category") + 
  theme(legend.position="none")

p.crime.subs.all <- ggplot(subsCrime.dat, # all
       aes(x=`crime cat`,y=`no. instances`,fill=school)) + 
  geom_col(alpha=0.5,col=NA,width=0.5,position="dodge") + 
  scale_fill_manual(values=c("CH Bird"="red","Westside"="purple",
                             "Northside"="green","Patrick Marsh"="blue")) + 
  scale_x_discrete(labels=crime.categories) + 
  labs(x="crime category") + 
  theme(legend.position=c(0.5,0.95),legend.justification=c("right","top"),
        legend.background=element_blank())


### instance count by category of possessions-related crime
crime.categories <- c("Burglary (commercial)","Burglary (residential)",
                      "Burglary (motor vehicle)","Theft (motor vehicle)",
                      "Robbery (commercial)","Robbery (individual)",
                      "Shoplifting")

p.crime.poss.ws <- ggplot(possCrime.dat %>% filter(school=="Westside"), # westside
       aes(x=`crime cat`,y=`no. instances`,fill="Westside")) +
  geom_col(alpha=0.5,col=NA,width=0.5) +
  scale_fill_manual(values=c("Westside"="purple")) + 
  labs(x="crime category") + 
  scale_x_discrete(labels=crime.categories) + 
  theme(legend.position="none",
        axis.text.x=element_text(vjust=0.8,angle=10))

p.crime.poss.ns <- ggplot(possCrime.dat %>% filter(school=="Northside"), # northside
       aes(x=`crime cat`,y=`no. instances`,fill="Northside")) +
  geom_col(alpha=0.5,col=NA,width=0.5) +
  scale_fill_manual(values=c("Northside"="green")) + 
  labs(x="crime category") + 
  scale_x_discrete(labels=crime.categories) + 
  theme(legend.position="none",
        axis.text.x=element_text(vjust=0.8,angle=10))

p.crime.poss.ch <- ggplot(possCrime.dat %>% filter(school=="CH Bird"), # ch bird
       aes(x=`crime cat`,y=`no. instances`,fill="CH Bird")) +
  geom_col(alpha=0.5,col=NA,width=0.5) +
  scale_fill_manual(values=c("CH Bird"="red")) + 
  labs(x="crime category") + 
  scale_x_discrete(labels=crime.categories) + 
  theme(legend.position="none",
        axis.text.x=element_text(vjust=0.8,angle=10))

p.crime.poss.pm <- ggplot(possCrime.dat %>% filter(school=="Patrick Marsh"), # patrick marsh
       aes(x=`crime cat`,y=`no. instances`,fill="Patrick Marsh")) +
  geom_col(alpha=0.5,col=NA,width=0.5) +
  scale_fill_manual(values=c("Patrick Marsh"="blue")) + 
  labs(x="crime category") + 
  scale_x_discrete(labels=crime.categories) + 
  theme(legend.position="none",
        axis.text.x=element_text(vjust=0.8,angle=10))

p.crime.poss.all <- ggplot(possCrime.dat, # all
       aes(x=`crime cat`,y=`no. instances`,fill=school)) + 
  geom_col(alpha=0.5,col=NA,width=0.5,position="dodge") + 
  scale_fill_manual(values=c("CH Bird"="red","Westside"="purple",
                             "Northside"="green","Patrick Marsh"="blue")) + 
  scale_x_discrete(labels=crime.categories) + 
  labs(x="crime category") + 
  theme(legend.position=c(0.2,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0.8,angle=10),
        legend.background=element_blank())

### instance count by category of violence-related crime
crime.categories <- c("Assault (aggravated)","Assault (simple)",
                      "Weapons violation")

p.crime.viol.ws <- ggplot(violCrime.dat %>% filter(school=="Westside"), # westside
       aes(x=`crime cat`,y=`no. instances`,fill="Westside")) +
  geom_col(alpha=0.5,col=NA,width=0.5) +
  scale_fill_manual(values=c("Westside"="purple")) +
  scale_x_discrete(labels=crime.categories) + 
  labs(x="crime category") + 
  theme(legend.position="none")

p.crime.viol.ns <- ggplot(violCrime.dat %>% filter(school=="Northside"), # northside
       aes(x=`crime cat`,y=`no. instances`,fill="Northside")) +
  geom_col(alpha=0.5,col=NA,width=0.5) +
  scale_fill_manual(values=c("Northside"="green")) + 
  scale_x_discrete(labels=crime.categories) + 
  labs(x="crime category") + 
  theme(legend.position="none")

p.crime.viol.ch <- ggplot(violCrime.dat %>% filter(school=="CH Bird"), # ch bird
       aes(x=`crime cat`,y=`no. instances`,fill="CH Bird")) +
  geom_col(alpha=0.5,col=NA,width=0.5) +
  scale_fill_manual(values=c("CH Bird"="red")) + 
  scale_x_discrete(labels=crime.categories) + 
  labs(x="crime category") + 
  theme(legend.position="none")

p.crime.viol.pm <- ggplot(violCrime.dat %>% filter(school=="Patrick Marsh"), # patrick marsh
       aes(x=`crime cat`,y=`no. instances`,fill="Patrick Marsh")) +
  geom_col(alpha=0.5,col=NA,width=0.5) +
  scale_fill_manual(values=c("Patrick Marsh"="blue")) + 
  scale_x_discrete(labels=crime.categories) + 
  labs(x="crime category") + 
  theme(legend.position="none")

p.crime.viol.all <- ggplot(violCrime.dat, # all
       aes(x=`crime cat`,y=`no. instances`,fill=school)) + 
  geom_col(alpha=0.5,col=NA,width=0.5,position="dodge") + 
  scale_fill_manual(values=c("CH Bird"="red","Westside"="purple",
                             "Northside"="green","Patrick Marsh"="blue")) + 
  scale_x_discrete(labels=crime.categories) + 
  theme(legend.position=c(0.2,0.95),legend.justification=c("right","top"),
        legend.background=element_blank())

### remove unneeded variables
rm(possCrime.dat); rm(subsCrime.dat); rm(violCrime.dat)
rm(crime.categories)