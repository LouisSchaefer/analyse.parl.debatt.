#packages
library(tidyverse)
library(quanteda)
library(stringr)
library(lubridate)
library(magrittr)
library(swissparl)
library(tm)
library(ggplot2)
library(expss)

require(quanteda)
require(quanteda.textmodels)
require(quanteda.textplots)
require(dplyr)

#get the data
sessions<- get_data(
  table = "Session",
  Language=c("DE"),
  StartDate = c(">2017-01-01"),
  EndDate = c("<2021-12-31")) 
length(unique(sessions17))

data <-get_data(
  table = "Transcript",
  Language="DE",
  Type = 1,
  IdSession = as.character(sessions$ID))

#variable mutations
data <- data %>% filter(!(IdSession %in% 5007)) %>% 
mutate(IdSession = case_when(  
  
  ##2017
  IdSession == "5007"~"NA",
  IdSession ==  "5008" ~ "Sondersess. Mai '17",
  IdSession ==  "5009" ~ "Sommersess. '17",
  IdSession ==  "5010" ~ "Herbstsess. '17",
  IdSession == "5011"~"Wintersess. '17",
  
  ##2018
  IdSession == "5012"~"Frühjahrssess. '18",
  IdSession ==  "5013" ~ "Sommersess. '18",
  IdSession ==  "5014" ~ "Herbstsess. '18",
  IdSession == "5015"~"Wintersess. '18",
  
  ##2019
  IdSession == "5016"~"Frühjahrssess. '19",
  IdSession ==  "5017" ~ "Sondersess. Mai '19",
  IdSession ==  "5018" ~ "Sommersess. '19",
  IdSession ==  "5019" ~ "Herbstsess. '19",
  IdSession == "5101"~"Wintersess. '19",
  
  ##2020
  IdSession == "5102"~"Frühjahrssess. '20",
  IdSession ==  "5103" ~ "ausserord. Sess. Mai '20",
  IdSession ==  "5104" ~ "Sommersess. '20",
  IdSession ==  "5105" ~ "Herbstsess. '20",
  IdSession == "5106"~"Sondersess. Oktober '20",
  IdSession == "5107"~"Wintersess. '20",
  
  ##2021
  IdSession == "5108"~"Frühjahrssess. '21",
  IdSession ==  "5109" ~ "Sondersess. Mai '21",
  IdSession ==  "5110" ~ "Laufende Sommersess. '21"))

data <- data %>% 
  mutate(ParlGroupName = case_when(ParlGroupName =="Die Mitte-Fraktion. Die Mitte. EVP." ~ "Die Mitte-Fraktion",
                                   ParlGroupName =="Grüne Fraktion" ~ "Grüne Fraktion" ,
                                   ParlGroupName =="Grünliberale Fraktion" ~ "Grünliberale Fraktion",
                                   ParlGroupName =="Sozialdemokratische Fraktion" ~ "Sozialdemokratische Fraktion",
                                   ParlGroupName =="FDP-Liberale Fraktion" ~ "FDP-Liberale Fraktion",
                                   ParlGroupName =="Fraktion der Schweizerischen Volkspartei" ~ "Fraktion der Schweizerischen Volkspartei"))

#ordering of sessions
ordering <- c("Sondersess. Mai '17","Sommersess. '17","Herbstsess. '17","Wintersess. '17",
              "Frühjahrssess. '18","Sommersess. '18","Herbstsess. '18","Wintersess. '18",
              "Frühjahrssess. '19","Sondersess. Mai '19","Sommersess. '19","Herbstsess. '19",
              "Wintersess. '19","Frühjahrssess. '20","ausserord. Sess. Mai '20",
              "Sommersess. '20","Herbstsess. '20","Sondersess. Oktober '20","Wintersess. '20", 
              "Frühjahrssess. '21","Sondersess. Mai '21","Laufende Sommersess. '21")


#filter to get only speeches for "Nationalrat" & "Ständerat"
data<- data %>% filter(!(CouncilName %in% c(NA,"Bundesrat"))) %>% 
  filter(!SpeakerLastName == "leer") %>% 
  filter(!SpeakerFunction %in% c("1VP-F", "1VP-M", "2VP-F", "2VP-M", "AP-M", "P-F", "P-M")) %>% 
  filter(!Function %in% c("1VP-M", "2VP-M", "P-F", "p-m", "P-m", "P-M", "P-MM")) 

#create corpus, tokens & tokens_dfm
umweltdata<- data
umweltdatacor <- corpus(umweltdata,text_field="Text")
umweltdatatoks <- tokens(umweltdatacor,
                          remove_numbers=T,remove_punct=T,
                          remove_symbols=T)

umwelt_dfm <- dfm(umweltdatatoks) 


#create dictionary
env_dict <-  dictionary(list(env=c("klima*","umwelt*","menschengemacht*","erderwärmung*","anthropogen","treibhaus*", "Fridays for Future", "pariser abkommen","pariser klimakonferenz","pariser klimaabkommen","co2*","co2-*","zwei-grad*","2°*","tipping point","erwärmung*","tierschutz","Recy*","hitzewelle*","überschwemmung*","meeresspiegel*","dürre*","energiewende","ökolog*","ökolandbau*","Trinkwasser","smog*","engerigeeffiz*","hochwasser","luftverschmutz*","feinstaub","grundwasser","methan*","ökosystem*","artenschutz","kohlendioxid*","naturkatarophe*","naturschutz", "emission*","fluorkohlenwasserstoff*","kipppunkt*","aerosol*","bio*","dekarbonisierung*","erneuerbare energie*", "ipcc*","kohlenstoffkreislauf","Ozon*","climat*", "environnement*", "réchauffement climatique*", "anthropique*", "Effet de serre*","Grève étudiante pour le climat", "Accord de Paris", "Paris Climat Conférence","accord parisien sur le climat","co2*","co2-*","deux-degree*","2°*","deux degree*","points de basculement","bien-être animal","recy*" , "canicule*", "inondation*", "niveau de la mer*", "sécheresse*", "transition énergétique*", "écologique*", "agriculture biologique*", "eau potable", "smog*", "efficacité énergétique","inondation","pollution de l'air*","particules","eau souterraine","méthane*","écosystème*","protection des espèces","dioxyde de carbone*","catastrophe naturelle*","conservation de la nature", "émission*", "fluorocarbone*", "aérosol*", "bio*", "décarbonisation*", "énergie renouvelable*", "ipcc*", "cycle du carbone", "ozone*","clima*", "ambiente*", "provocato dall'uomo*", "riscaldamento globale*", "riscaldamento", "
anthropogen*"," treibhaus*","effetto serra*","Fridays for Future","venerdì per il futuro","Accordo di Parigi","accordo parigino sul clima","co2*","co2-*","due gradi*","due-gradi*","2°*","punto di non ritorno","riscaldamento*","Protezione degli animali*","raccolta differenziata","ondata di calore*","alluvione*","livello del mare*","siccità*","transizione energetica","ecologico*","agricoltura biologica*","acqua potabile","smog*","efficienza energetica*","alluvione","inquinamento atmosferico*", "polvere fine", "acque sotterranee", "metano*", "ecosistema*", "protezione delle specie", "anidride carbonica*", "catarofe naturale*", "conservazione della natura", "emissione*", "fluorocarbon*" , "punto di svolta*", "aerosol*", "bio*", "decarbonizzazione*", "energia rinnovabile*", "ipcc*", "ciclo del carbonio", "ozono*")))

#create results_data_frame for plots
results_dict <- tokens_lookup(umweltdatatoks,
                              env_dict,
                              nomatch="OTHER")

results_df <- results_dict %>%
  dfm() %>%
  convert("data.frame") %>%
  bind_cols(docvars(results_dict))

#create plot1
plot1 <-results_df  %>% 
  mutate(benv = ifelse(env != 0,1,0)) %>%
  group_by(IdSession, benv) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n)*100) %>%
  filter(benv == 1) %>%
  
  
  ggplot(aes(x=IdSession,y=freq))+
  geom_col(fill="#689e06",color="#2a602b",binwidth=1,stat="identity")+
  theme(legend.position="none",plot.title = element_text(hjust = 0.5, size=26),
        plot.subtitle = element_text(hjust = 0.5, size=18.5),
        axis.title = element_text(size = 18.5),
        axis.text = element_text(size=14)
  )+
  labs(title = "Die Covid-Pandemie vermag die Grüne Welle im Bundesparlament nicht zu stoppen",
       subtitle = "Trotz Corona wurde das Thema nach dem ersten Klimastreik in Schweden vermehrt angesprochen")+
  xlab("Session")+ylab("Wortmeldungen zur Klimathematik")+
  scale_x_discrete(limits=ordering)+scale_y_continuous(limits=c(0,30), labels = c("0","10%","20%","30%"))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_vline(xintercept=6.5, colour="dark green")+ 
  geom_text(aes(x=6.55, label="Erster Klimastreik in Schweden",y=15,size=22),  angle=90, colour="black", vjust = 1.2)+
  
  geom_vline(xintercept=05, colour="white")+ 
  geom_text(aes(x=13.55, label="",y=15, size=18),  angle=90, colour="black", vjust = 1.2)+
  
  geom_vline(xintercept=12.5, colour="black", linetype="dotted")+ 
  geom_text(aes(x=12.55, label="Parlamentswahlen 2019",y=15, size=22), 
            colour="black",  angle=90, vjust = 1.2)+
  
  geom_vline(xintercept=13.5, colour="darkred")+ 
  geom_text(aes(x=13.55, label="Erster Schweizer Corona-Fall",y=15, size=22),  angle=90, colour="black", vjust = 1.2)+
  
  geom_vline(xintercept=14.5, colour="darkred") +
  geom_text(aes(x=14.55, label="Ausserordentliche Lage",y=15, size=22), colour="black", angle=90,vjust = 1.2)+
  
  geom_vline(xintercept=16.5, colour="black", linetype="dotted") +
  geom_text(aes(x=16.55, label="Lockerungen",y=15, size=22), colour="black", angle=90, vjust = 1.2)+
  
  geom_vline(xintercept=19.5, colour="darkred") +
  geom_text(aes(x=19.55, label="Schweizweiter Corona Lockdown",y=15, size=22), colour="black", angle=90, vjust = 1.2)+
  
  geom_vline(xintercept=20.5, colour="black", linetype="dotted") +
  geom_text(aes(x=20.55, label="Neuerliche Lockerungen",y=15, size=22), colour="black", angle=90, vjust = 1.2)

ggsave("plot1.png",height = 9,width=15)

#create Plot2


ordering2 <- c("Grüne Fraktion","Grünliberale Fraktion","Sozialdemokratische Fraktion","Die Mitte-Fraktion", "FDP-Liberale Fraktion","Fraktion der Schweizerischen Volkspartei" )

ordering3 <- c( "Frühjahrssess. '20","ausserord. Sess. Mai '20",
                "Sommersess. '20","Herbstsess. '20","Wintersess. '20")

plot2 <-   results_df %>% filter(!(ParlGroupName%in% NA)) %>% 
  mutate(benv = ifelse(env != 0,1,0)) %>%
  group_by(IdSession, ParlGroupName, benv) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n)*100) %>%
  filter(benv == 1) %>%
  filter(IdSession %in% c( "Frühjahrssess. '20","ausserord. Sess. Mai '20",
     "Sommersess. '20","Herbstsess. '20","Wintersess. '20")) %>% ggplot() +geom_col(aes(x = IdSession, y = freq,
   fill = factor(ParlGroupName, levels=c("Grüne Fraktion","Grünliberale Fraktion","Sozialdemokratische Fraktion","Die Mitte-Fraktion", "FDP-Liberale Fraktion","Fraktion der Schweizerischen Volkspartei" ))), colour = "#2a602b",position = "dodge")+ scale_x_discrete(limits=ordering3)+
  theme_grey()+xlab("Session")+ylab("Wortmeldungen zur Klimathematik (%)")+ggtitle("Nennungen Klima x Sessionen; für Fraktionen")+
  scale_y_continuous(limits=c(0,50), labels = c("0","10%","20%","30%","40%","50%"))+ 
  theme(
    plot.title = element_text(hjust = 0.5, size=20),
    plot.subtitle = element_text(hjust = 0.5, size=14.5),
    axis.title = element_text(size = 14.5),
    axis.text = element_text(size=10),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text =element_text(size=14.5))+
  scale_fill_manual(values = c("Fraktion der Schweizerischen Volkspartei"="#006600",
                               "FDP-Liberale Fraktion"="#0000d1",
                               "Die Mitte-Fraktion"="#FF9900",
                               "Sozialdemokratische Fraktion"="#FF3333",
                               "Grünliberale Fraktion"="purple", 
                               "Grüne Fraktion"="#689e06")) +
  labs(fill="Fraktionen",title = "Im ersten Jahr der Corona-Pandemie erwähnte v.a. die Grüne Fraktion die Klimathematik",
       subtitle = "Insbesondere in der ausserordentlichen Session '20 sticht die Grüne Fraktion durch ihr Engagement hervor")+
  xlab("Session")+ylab("Wortmeldungen zur Klimathematik")+
  guides(size = FALSE)+
  
  
  geom_vline(xintercept=0.5, colour="darkred")+ 
  geom_text(aes(x=0.55, label="Erster Schweizer Corona-Fall",y=25,size=40),  angle=90, colour="black", vjust = 1.2)+
  
  geom_vline(xintercept=1.5, colour="darkred") +
  geom_text(aes(x=1.55, label="Ausserordentliche Lage",y=25,size=40), colour="black", angle=90,vjust = 1.2)+
  
  geom_vline(xintercept=3.5, colour="black", linetype="dotted") +
  geom_text(aes(x=3.55, label="Lockerungen",y=25,size=40), colour="black", angle=90, vjust = 1.2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(aes(x=0.95, label="Durchschnitt",y=17), colour="black",vjust = 1.2)+
  geom_segment(aes(x = 0.75, y = 14.25, xend = 1.5, yend = 14.25), linetype="twodash")+
  geom_segment(aes(x = 1.75, y = 16.909091, xend = 2.5, yend = 16.909091), linetype="twodash")+
  geom_segment(aes(x = 2.75, y = 19.542484, xend = 3.5, yend = 19.542484), linetype="twodash")+
  geom_segment(aes(x = 3.75, y = 16.890380, xend = 4.5, yend = 16.890380), linetype="twodash")+
  geom_segment(aes(x = 4.75, y = 14.187928, xend = 5.5, yend = 14.187928), linetype="twodash")

ggsave("plot2.png",height = 8,width=13)

#create plot3
ordering4 <- c( "Frühjahrssess. '21","Sondersess. Mai '21","Laufende Sommersess. '21","Herbstsess. '21","Wintersess. '21")

plot3 <-   results_df %>% filter(!(ParlGroupName%in% NA)) %>% 
  mutate(benv = ifelse(env != 0,1,0)) %>%
  group_by(IdSession, ParlGroupName, benv) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n)*100) %>%
  filter(benv == 1) %>%
  filter(IdSession %in% c(  "Frühjahrssess. '21","Sondersess. Mai '21","Laufende Sommersess. '21")) %>% ggplot() +geom_col(aes(x = IdSession, y = freq, fill = factor(ParlGroupName, levels=c("Grüne Fraktion","Grünliberale Fraktion","Sozialdemokratische Fraktion","Die Mitte-Fraktion", "FDP-Liberale Fraktion","Fraktion der Schweizerischen Volkspartei" ))), colour = "#2a602b",position = "dodge")+ scale_x_discrete(limits=ordering4)+
  theme_grey()+xlab("Session")+ylab("Wortmeldungen zur Klimathematik (%)")+ggtitle("Nennungen Klima x Sessionen; für Fraktionen")+
  scale_y_continuous(limits=c(0,50), labels = c("0","10%","20%","30%","40%","50%"))+ 
  theme(
    plot.title = element_text(hjust = 0.5, size=20),
    plot.subtitle = element_text(hjust = 0.5, size=14.5),
    axis.title = element_text(size = 14.5),
    axis.text = element_text(size=10),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text =element_text(size=14.5))+
  guides(size = FALSE)+
  scale_fill_manual(values = c("Fraktion der Schweizerischen Volkspartei"="#006600",
                               "FDP-Liberale Fraktion"="#0000d1",
                               "Die Mitte-Fraktion"="#FF9900",
                               "Sozialdemokratische Fraktion"="#FF3333",
                               "Grünliberale Fraktion"="purple", 
                               "Grüne Fraktion"="#689e06")) +
  labs(fill="Fraktionen",title = "Die Grüne Fraktion hielt sich im Frühjahr mit der Klimathematik eher zurück",
       subtitle = "Nach den Lockerungen fand die Klimathematik allerdings wieder stärker Eingang in die politische Debatte")+
  xlab("Session")+ylab("Wortmeldungen zur Klimathematik")+
  
  geom_vline(xintercept=0.5, colour="darkred") +
  geom_text(aes(x=0.55, label="Schweizweiter Corona Lockdown",y=25,size=40), colour="black", angle=90, vjust = 1.2)+
  
  geom_vline(xintercept=1.5, colour="black", linetype="dotted") +
  geom_text(aes(x=1.55, label="Neuerliche Lockerungen",y=25,size=40), colour="black", angle=90, vjust = 1.2)+
  theme(plot.title = element_text(size=18))+  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  
  geom_text(aes(x=0.95, label="Durchschnitt",y=13.5), colour="black",vjust = 1.2)+
  geom_segment(aes(x = 0.75, y = 10.803859, xend = 1.5, yend = 10.803859), linetype="twodash")+
  geom_segment(aes(x = 1.75, y = 18.146718, xend = 2.5, yend = 18.146718), linetype="twodash")+
  geom_segment(aes(x = 2.75, y = 14.800515, xend = 3.5, yend = 14.800515), linetype="twodash")

ggsave("plot3.png",height = 8,width=13)
