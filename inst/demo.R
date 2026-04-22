library(tidyverse)
library(ggSpider)

data("atacazo")

### Simple usage
# In most case you will only need the top level functions, ggspiderplot and whatever geometry is required:
ggspiderplot(atacazo,norm="Nakamura")+geom_line_continuous(aes(colour = SiO2))

# Note the use of geom_line_continuous to skip missing values:
ggspiderplot(atacazo,norm="Nakamura")+geom_line(aes(colour = SiO2))

# This can be made more clear with the dots:
ggspiderplot(atacazo,norm="Nakamura")+geom_line_continuous(aes(colour = SiO2))+geom_point()
ggspiderplot(atacazo,norm="Nakamura")+geom_line(aes(colour = SiO2))+geom_point()

# So of course we have all the usual niceties
ggspiderplot(atacazo,norm="Nakamura")+
  geom_line_continuous(aes(colour = SiO2))+
  geom_point(aes(shape=Volcano))+
  facet_wrap(~cut(MgO,2))

### Manually adding data
atac <- filter(atacazo,Volcano == "Atacazo")
nina <- filter(atacazo,Volcano == "Ninahuilca")

# In this case we need to manually call spider_data()
atac %>% ggspiderplot(norm="Boynton")+geom_line_continuous(color="green")+
  geom_line_continuous(data=spider_data(nina,norm="Boynton"),color="red")

### Normalization schemes are stored in a variable called GCDKitNormScheme
# They can be selected using clever matching (as above),  however there are ambiguities:
ggspiderplot(atacazo,norm="Pearce")+geom_line_continuous(aes(colour = SiO2)) # ERROR !

# We can either be more explicit :
ggspiderplot(atacazo,norm="Pearce 1996")+geom_line_continuous(aes(colour = SiO2))

# ... or manually extract the object we need (item 4, in that case):
ggspiderplot(atacazo,norm=GCDKitNormScheme[[4]])+geom_line_continuous(aes(colour = SiO2))

### We can also manually define norms
# Note that the function is relatively forgiving and will accept most sane formats
mynorm <- c(Rb=100,Sr=10,Y=25)
ggspiderplot(atacazo,norm=mynorm)+geom_line_continuous(aes(colour = SiO2))

mynorm_t <- tibble(Rb=100,Sr=10,Y=25)
ggspiderplot(atacazo,norm=mynorm_t)+geom_line_continuous(aes(colour = SiO2))

mynorm_df <- data.frame(Rb=100,Sr=10,Y=25)
ggspiderplot(atacazo,norm=mynorm_df)+geom_line_continuous(aes(colour = SiO2))

mynorm_mat <- matrix(c(100,10,25),byrow=F,nrow=1)
colnames(mynorm_mat) <- c("Rb","Sr","Y")
ggspiderplot(atacazo,norm=mynorm_mat)+geom_line_continuous(aes(colour = SiO2))

atacazo %>%
  summarise(Rb = mean(Rb), Ba = mean(Ba), K2O = mean(K2O)) -> mynorm2
ggspiderplot(atacazo,norm=mynorm2)+geom_line_continuous(aes(colour = SiO2))

# in this case the need for a log scale is arguable, so we force a natural scale:
ggspiderplot(atacazo,norm=mynorm2)+geom_line_continuous(aes(colour = SiO2))+
  scale_y_continuous()

### Data can also be given using most sane formats

WR <- atacazo %>% select(c("La","Ce","Nd","Sm","Eu","Gd")) %>% as.matrix()
ggspiderplot(WR,norm="Boynton")+geom_line_continuous()

one <- WR[1,,drop=T] #  a vector !
ggspiderplot(one,norm="Boynton")+geom_line_continuous()

# Which allows...
bnt <- get_norm("Boynton")
and <- get_norm("Anders")
nak <- get_norm("Nakamura")

ggspiderplot(bnt,norm="Boynton")+
  geom_line_continuous(data=spider_data(and,norm=bnt),color="red")+
  geom_line_continuous(data=spider_data(nak,norm=bnt),color="blue")

### Some statistics
#The trick is to change the grouping  and use group = Element explicitly !

ggspiderplot(atacazo,norm="Boynton")+
  geom_point()+
  geom_boxplot(aes(group=Element))

ggspiderplot(atacazo,norm="Boynton")+
  geom_violin(aes(group=Element),fill="lightblue",colour=NA)+
  geom_point()+
  facet_wrap(~Volcano)

ata1 <- atacazo %>% select(-Volcano) #A dataset that won't be facetted
ggspiderplot(atacazo,norm="Boynton")+
  geom_boxplot(data=spider_data(ata1,"Boynton"),aes(group=Element),fill="lightblue",colour=NA)+
  geom_point()+
  facet_wrap(~Volcano)

### Range

ggspiderplot(atacazo,norm="Boynton")+
  geom_range(fill="grey")

ggspiderplot(atacazo,norm="Boynton")+
  geom_range(fill="grey")+
  geom_point()+
  geom_line_continuous()

ggspiderplot(atacazo,norm="Boynton")+
  geom_range(fill="grey")+
  geom_point()+
  geom_line_continuous()+
  facet_wrap(~Volcano)

ggspiderplot(atacazo,norm="Boynton")+
  geom_range(data=spider_data(ata1,"Boynton"),fill="grey")+
  geom_point()+
  geom_line_continuous()+
  facet_wrap(~Volcano)

### You can always use themes and other customization !
ggspiderplot(atacazo,norm="Boynton")+
  geom_range(data=spider_data(ata1,"Boynton"),fill="grey")+
  geom_point()+
  geom_line_continuous()+
  facet_wrap(~Volcano)+
  scale_y_log10(breaks=c(1,10,100),limits=c(0.9,110))+
  theme(panel.grid.minor.y = element_blank() )
