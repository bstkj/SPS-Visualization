# loads agecat.dat, householdcat.dat, raceeth.dat
source("loadDataPopulation.R")

### population count by race/eth
raceeth.categories <- c("White","Hispanic","Black","Asian","Native",
                        "1-race (other)", ">=2-race")

p.pop.raceeth.ws1 <- ggplot(raceeth.dat %>% filter(school=="Westside"), # westside (not combined)
       aes(x=`race/eth`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1.5,col="purple",aes(lty=block)) +
  labs(lty="census block group",x="race/ethnicity") + 
  scale_x_discrete(labels=raceeth.categories) +
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background=element_blank())

p.pop.raceeth.ws2 <- ggplot(raceeth.dat %>% filter(school=="Westside") %>% # westside (combined)
                              group_by(`race/eth`) %>% summarize(population=sum(population)),
                           aes(x=`race/eth`,y=population,group="Westside")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="purple") + labs(x="race/ethnicity") + 
  scale_x_discrete(labels=raceeth.categories) + 
  theme(axis.text.x=element_text(vjust=0,angle=0))

p.pop.raceeth.ns1 <- ggplot(raceeth.dat %>% filter(school=="Northside"), # northside (not combined)
       aes(x=`race/eth`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1,col="green",aes(lty=block)) +
  labs(lty="census block group",x="race/ethnicity") + 
  scale_x_discrete(labels=raceeth.categories) + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background=element_blank())

p.pop.raceeth.ns2 <- ggplot(raceeth.dat %>% filter(school=="Northside") %>% # northside (combined)
                              group_by(`race/eth`) %>% summarize(population=sum(population)),
                            aes(x=`race/eth`,y=population,group="Northside")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="green") + labs(x="race/ethnicity") + 
  scale_x_discrete(labels=raceeth.categories) + 
  theme(axis.text.x=element_text(vjust=0,angle=0))

p.pop.raceeth.ch <- ggplot(raceeth.dat %>% filter(school=="CH Bird"), # ch bird (not combined)
       aes(x=`race/eth`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="red",aes(lty=block)) +
  labs(lty="census block group",x="race/ethnicity") + 
  scale_x_discrete(labels=raceeth.categories) + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background=element_blank())

# only showing census blocks exclusive to patrick marsh, omitting the blocks
# shared with northside and ch bird
p.pop.raceeth.pm1 <- ggplot(raceeth.dat %>% filter(school=="Patrick Marsh"), # patrick marsh (not combined)
       aes(x=`race/eth`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1,col="blue",aes(lty=block)) +
  labs(lty="census block group",x="race/ethnicity") + 
  scale_x_discrete(labels=raceeth.categories) + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background=element_blank())

p.pop.raceeth.pm2 <- ggplot(raceeth.dat %>% filter(school=="Patrick Marsh") %>% # patrick marsh (combined)
                              group_by(`race/eth`) %>% summarize(population=sum(population)),
                            aes(x=`race/eth`,y=population,group="Westside")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="blue") + labs(x="race/ethnicity") + 
  scale_x_discrete(labels=raceeth.categories) + 
  theme(axis.text.x=element_text(vjust=0,angle=0))

p.pop.raceeth.all1 <- ggplot(raceeth.dat, # all (not combined)
       aes(x=`race/eth`,y=population,group=block)) +
  geom_point() + geom_line(alpha=0.5,size=1,aes(col=school)) + labs(x="race/ethnicity") + 
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) + 
  scale_x_discrete(labels=raceeth.categories) + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background=element_blank())

p.pop.raceeth.all2 <- ggplot(raceeth.dat %>% group_by(school,`race/eth`) %>% # all (combined)
                             summarize(population=sum(population)),
                           aes(x=`race/eth`,y=population,group=school)) + 
  geom_point() + geom_line(alpha=0.5,size=1,aes(col=school)) + labs(x="race/ethnicity") + 
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) + 
  scale_x_discrete(labels=raceeth.categories) + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background=element_blank())

### population count by age category
age.brackets <- c("<5","5-9","10-14","15-19","20-24","25-34",
                  "35-44","45-54","55-64","65-74","75-84","85<")

p.pop.agecat.ws1 <- ggplot(agecat.dat %>% filter(school=="Westside"), # westside (not combined)
       aes(x=`age cat`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1.5,col="purple",aes(lty=block)) +
  labs(lty="census block group",x="age bracket") + 
  scale_x_discrete(labels=age.brackets) + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background=element_blank())

p.pop.agecat.ws2 <- ggplot(agecat.dat %>% filter(school=="Westside") %>% # westside (combined)
                              group_by(`age cat`) %>% summarize(population=sum(population)),
                            aes(x=`age cat`,y=population,group="Westside")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="purple") + labs(x="age bracket") +
  scale_x_discrete(labels=age.brackets) + 
  theme(axis.text.x=element_text(vjust=0,angle=0))

p.pop.agecat.ns1 <- ggplot(agecat.dat %>% filter(school=="Northside"), # northside (not combined)
       aes(x=`age cat`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1,col="green",aes(lty=block)) +
  labs(lty="census block group",x="age bracket") +
  scale_x_discrete(labels=age.brackets) + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background=element_blank())

p.pop.agecat.ns2 <- ggplot(agecat.dat %>% filter(school=="Northside") %>% # northside (combined)
                              group_by(`age cat`) %>% summarize(population=sum(population)),
                            aes(x=`age cat`,y=population,group="Northside")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="green") + labs(x="age bracket") +
  scale_x_discrete(labels=age.brackets) + 
  theme(axis.text.x=element_text(vjust=0,angle=0))

p.pop.agecat.ch <- ggplot(agecat.dat %>% filter(school=="CH Bird"), # ch bird (not combined)
       aes(x=`age cat`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="red",aes(lty=block)) +
  labs(lty="census block group",x="age bracket") + 
  scale_x_discrete(labels=age.brackets) + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background=element_blank())

p.pop.agecat.pm1 <- ggplot(agecat.dat %>% filter(school=="Patrick Marsh"), # patrick marsh (not combined)
       aes(x=`age cat`,y=population,group=block)) + 
  geom_point() + geom_line(alpha=0.5,size=1,col="blue",aes(lty=block)) +
  labs(lty="census block group",x="age bracket") +
  scale_x_discrete(labels=age.brackets) + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background=element_blank())

p.pop.agecat.pm2 <- ggplot(agecat.dat %>% filter(school=="Patrick Marsh") %>% # patrick marsh (combined)
                              group_by(`age cat`) %>% summarize(population=sum(population)),
                            aes(x=`age cat`,y=population,group="Patrick Marsh")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="blue") + labs(x="age bracket") + 
  scale_x_discrete(labels=age.brackets) +
  theme(axis.text.x=element_text(vjust=0,angle=0))

p.pop.agecat.all1 <- ggplot(agecat.dat, # all (not combined)
       aes(x=`age cat`,y=population,group=block)) +
  geom_point() + geom_line(alpha=0.5,size=1,aes(col=school)) + labs(x="age bracket") + 
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) +
  scale_x_discrete(labels=age.brackets) + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background=element_blank())

p.pop.agecat.all2 <- ggplot(agecat.dat %>% group_by(school,`age cat`) %>% # all (combined)
                               summarize(population=sum(population)),
                             aes(x=`age cat`,y=population,group=school)) + 
  geom_point() + geom_line(alpha=0.5,size=1,aes(col=school)) + labs(x="age bracket") + 
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) + 
  scale_x_discrete(labels=age.brackets) + 
  theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background = element_blank())

### frequency of household types 

# shorten the names of the x-axis levels
hh.types <- c("1-person",">=2-person","Family","Married-couple family","Non-family")
# use the shortened names in householdcat.dat
levels(householdcat.dat$`household cat`) <- hh.types
       

# redefine order so that geom_col() will stack "1 person households" on top of 
# "2 or more person households", and "Nonfamily Households" on top of 
# "Family households (Families)"
householdcat.dat$`household cat` <- factor(householdcat.dat$`household cat`,
                                           levels=c("1-person",
                                                    ">=2-person",
                                                    "Non-family",
                                                    "Family",
                                                    "Married-couple family"))
# update ordering for use of scale_x_discrete()
hh.types <- levels(householdcat.dat$`household cat`)

# define a new column (new.col) to use geom_col() to display separate bars for 
# different subsets of `household cat` levels.
hh.types.col <- householdcat.dat$`household cat`
new.col <- vector(length=nrow(householdcat.dat))
new.col[hh.types.col=="1-person"|hh.types.col==">=2-person"] <- "Household size"
new.col[hh.types.col=="Family"|hh.types.col=="Non-family"] <- "Family/Non-family"
new.col[hh.types.col=="Married-couple family"] <- "Married-couple family"
new.col <- factor(new.col,levels=c("Household size","Family/Non-family",
                                   "Married-couple family"))

p.pop.hhcat.ws1 <- ggplot(householdcat.dat %>% mutate(statcat=new.col) %>% # westside (not combined)
  filter(school=="Westside"),aes(x=statcat,y=population)) +
  geom_col(aes(fill=`household cat`),width=0.5) + 
  facet_wrap(vars(block),
             labeller=labeller(block=c("011506-2"="Census block group: 011506-2",
                                       "011505-2"="Census block group: 011505-2"))) + 
  labs(x="",fill="Household type") + 
  theme(legend.background=element_blank())

# p.pop.hhcat.ws1 <- ggplot(householdcat.dat %>% filter(school=="Westside"), # westside (not combined)
#        aes(x=`household cat`,y=population,group=block)) + 
#   geom_point() + geom_line(alpha=0.5,size=1.5,col="purple",aes(lty=block)) +
#   labs(lty="census block group",x="household type") + 
#   scale_x_discrete(labels=hh.types) + 
#   theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
#         axis.text.x=element_text(vjust=0,angle=0),
#         legend.background=element_blank())

p.pop.hhcat.ws2 <- ggplot(householdcat.dat %>% filter(school=="Westside") %>% # westside (combined)
                             group_by(`household cat`) %>% summarize(population=sum(population)),
                           aes(x=`household cat`,y=population,group="Westside")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="purple") + labs(x="household type") + 
  scale_x_discrete(labels=hh.types) + 
  theme(axis.text.x=element_text(vjust=0,angle=0))

p.pop.hhcat.ns1 <- ggplot(householdcat.dat %>% mutate(statcat=new.col) %>% # northside (not combined)
                            filter(school=="Northside"),aes(x=statcat,y=population)) +
  geom_col(aes(fill=`household cat`),width=0.5) + 
  facet_wrap(vars(block),
             labeller=labeller(block=c("011505-1"="Census block group: 011505-1",
                                       "011505-3"="Census block group: 011505-3",
                                       "011506-3"="Census block group: 011506-3"))) + 
  labs(x="",fill="Household type") + 
  theme(legend.background=element_blank())

# p.pop.hhcat.ns1 <- ggplot(householdcat.dat %>% filter(school=="Northside"), # northside (not combined)
#        aes(x=`household cat`,y=population,group=block)) + 
#   geom_point() + geom_line(alpha=0.5,size=1,col="green",aes(lty=block)) +
#   labs(lty="census block group",x="household type") + 
#   scale_x_discrete(labels=hh.types) + 
#   theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
#         axis.text.x=element_text(vjust=0,angle=0),
#         legend.background=element_blank())

p.pop.hhcat.ns2 <- ggplot(householdcat.dat %>% filter(school=="Northside") %>% # northside (combined)
                            group_by(`household cat`) %>% summarize(population=sum(population)),
                          aes(x=`household cat`,y=population,group="Northside")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="green") + labs(x="household type") + 
  scale_x_discrete(labels=hh.types) + 
  theme(axis.text.x=element_text(vjust=0,angle=0))

p.pop.hhcat.ch <- ggplot(householdcat.dat %>% mutate(statcat=new.col) %>% # ch bird (not combined)
                            filter(school=="CH Bird"),aes(x=statcat,y=population)) +
  geom_col(aes(fill=`household cat`),width=0.5) + 
  facet_wrap(vars(block),
             labeller=labeller(block=c("011504-1"="Census block group: 011504-1"))) + 
  labs(x="",fill="Household type") + 
  theme(legend.background=element_blank())

# p.pop.hhcat.ch <- ggplot(householdcat.dat %>% filter(school=="CH Bird"), # ch bird (not combined)
#        aes(x=`household cat`,y=population,group=block)) + 
#   geom_point() + geom_line(alpha=0.5,size=2,col="red",aes(lty=block)) +
#   labs(lty="census block group",x="household type") + 
#   scale_x_discrete(labels=hh.types) + 
#   theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
#         axis.text.x=element_text(vjust=0,angle=0),
#         legend.background=element_blank())

p.pop.hhcat.pm1 <- ggplot(householdcat.dat %>% mutate(statcat=new.col) %>%
                            filter(school=="Patrick Marsh"),aes(x=statcat,y=population)) +
  geom_col(aes(fill=`household cat`),width=0.5) + 
  facet_wrap(vars(block),
             labeller=labeller(block=c("011600-2"="Census block group: 011600-2",
                                       "011700-2"="Census block group: 011700-2",
                                       "011506-1"="Census block group: 011506-1"))) + 
  labs(x="",fill="Household type") + 
  theme(legend.background=element_blank())

# p.pop.hhcat.pm1 <- ggplot(householdcat.dat %>% filter(school=="Patrick Marsh"), # patrick marsh (not combined)
#        aes(x=`household cat`,y=population,group=block)) + 
#   geom_point() + geom_line(alpha=0.5,size=1,col="blue",aes(lty=block)) +
#   labs(lty="census block group",x="household type") + 
#   scale_x_discrete(labels=hh.types) + 
#   theme(legend.position=c(0.95,0.95),legend.justification=c("right","top"),
#         axis.text.x=element_text(vjust=0,angle=0),
#         legend.background=element_blank())

p.pop.hhcat.pm2 <- ggplot(householdcat.dat %>% filter(school=="Patrick Marsh") %>% # patrick marsh (combined)
                            group_by(`household cat`) %>% summarize(population=sum(population)),
                          aes(x=`household cat`,y=population,group="Patrick Marsh")) + 
  geom_point() + geom_line(alpha=0.5,size=2,col="blue") + labs(x="household type") + 
  scale_x_discrete(labels=hh.types) + 
  theme(axis.text.x=element_text(vjust=0,angle=0))

p.pop.hhcat.all1 <- ggplot(householdcat.dat, # all (not combined) 
       aes(x=`household cat`,y=population,group=block)) +
  geom_point() + geom_line(alpha=0.5,size=1,aes(col=school)) + labs(x="household type") + 
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) + 
  scale_x_discrete(labels=hh.types) + 
  theme(legend.position=c(0.2,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background = element_blank())

p.pop.hhcat.all2 <- ggplot(householdcat.dat %>% group_by(school,`household cat`) %>% # all (combined)
                              summarize(population=sum(population)),
                            aes(x=`household cat`,y=population,group=school)) + 
  geom_point() + geom_line(alpha=0.5,size=1,aes(col=school)) + labs(x="household type") + 
  scale_color_manual(values=c("CH Bird"="red","Westside"="purple",
                              "Northside"="green","Patrick Marsh"="blue")) + 
  scale_x_discrete(labels=hh.types) + 
  theme(legend.position=c(0.2,0.95),legend.justification=c("right","top"),
        axis.text.x=element_text(vjust=0,angle=0),
        legend.background = element_blank())

### remove unneeded variables
rm(agecat.dat); rm(householdcat.dat); rm(raceeth.dat)
rm(raceeth.categories); rm(age.brackets); rm(hh.types); rm(hh.types.col); rm(new.col)