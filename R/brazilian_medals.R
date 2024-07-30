# File:   
# 

# INSTALL AND LOAD PACKAGES ################################
#

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
# Packages I load every time; uses "pacman"
pacman::p_load(pacman,tidyverse,rio,rvest,grid,gridExtra
               #dplyr,rio,lubridate,
               #ggplot2,grid,gridExtra,           #ggview, # grid.arrange
               #glue,reshape2,tidyverse,  data.table,
               #tidyquant,tseries,    foreach,doParallel
               #,gt
               #, GGally, ggthemes, 
               #ggvis, httr, lubridate, plotly, rmarkdown, shiny, 
               #stringr, tidyr
               #,quantmod,PerformanceAnalytics,lubridate
) 





olympic_cities <- list(
  "Paris" = "PAR",
  "Tóquio" = "TOK",
  "Rio de Janeiro" = "RIO",
  "Londres" = "LON",
  "Pequim" = "PEK",
  "Atenas" = "ATH",
  "Sydney" = "SYD",
  "Atlanta" = "ATL",
  "Barcelona" = "BAR",
  "Seul" = "SEO",
  "Los Angeles" = "LoA",
  "Moscou" = "MOS",
  "Montreal" = "MON",
  "Munique" = "MUN",
  "Cidade do México" = "MEX",
  "Roma" = "ROM",
  "Melbourne" = "MEL",
  "Helsinque" = "HEL",
  "Berlim" = "BER",
  "Antuérpia" = "ANT",
  "Estocolmo" = "STO",
  "Amsterdam" = "AMS",
  "St. Louis" = "STL"
)

url="https://pt.wikipedia.org/wiki/Lista_de_medalhas_brasileiras_nos_Jogos_Ol%C3%ADmpicos"

html <- url %>% read_html()

# load medals per year
df <- html %>% html_node(xpath='//*[@id="mw-content-text"]/div[1]/table[1]') %>% html_table() %>% as_tibble()

# x %>% html_node(xpath='//*[@id="mw-content-text"]/div[1]/table[3]')
# x %>% html_node(xpath='//*[@id="mw-content-text"]/div[1]/div[5]')

df$ticker <- NA
for(i in 1:nrow(df)) df$ticker[i]=olympic_cities[[df$Local[i]]]

# transform variables
df <- df %>% 
  mutate(Ouro=as.integer(Ouro),Prata=as.integer(Prata),Bronze=as.integer(Bronze),Total=as.integer(Total)) %>% 
  mutate(rank=as.integer(str_remove(`Colocação`,"º"))) %>% 
  mutate(year=factor(Ano)) %>% 
  ungroup()

# load athletes medals
athlete_medals_per_year_df <- NULL
for(i in 4:23){
  athlete_medals_per_year_df <- athlete_medals_per_year_df %>% 
    rbind(
      html %>% html_node(xpath=paste0('//*[@id="mw-content-text"]/div[1]/table[',i-1,']')) %>% html_table() %>% as_tibble() %>% 
        mutate(event_name=(html %>% html_node(xpath=paste0('//*[@id="mw-content-text"]/div[1]/div[',i,']')) %>% html_elements("a") %>% html_attr("title"))[1]) %>% 
        mutate(Local=strsplit(as.character(event_name), " (?=[0-9]{4}$)", perl=TRUE)[[1]][1]) %>% 
        mutate(  Ano=strsplit(as.character(event_name), " (?=[0-9]{4}$)", perl=TRUE)[[1]][2]) %>% 
        mutate(year=factor(Ano))
    )
}



plot_medals_per_year <- function(df){
  df %>% 
    mutate(text_color=as.integer(ifelse(`Colocação`=="NP",max(rank,na.rm=T)+10,ifelse(`Colocação`=="SC",max(rank,na.rm=T),rank)))) %>% 
    mutate(text_color=ifelse(text_color<quantile(text_color,.1,na.rm=T),1,text_color)) %>% 
    ggplot(aes(y=year))+
    geom_text(aes(x=(-.5),color=-text_color,fontface="bold",size=6,label=paste(`Colocação`,ticker,Ano)),hjust=1)+xlim((-7),max(as.integer(df$Total),na.rm=T)+1)+
    geom_col(aes(x=Prata+Ouro+Bronze),fill="sienna3")+
    geom_col(aes(x=Prata+Ouro),fill="gray")+
    geom_col(aes(x=Ouro),fill="gold")+
    annotate(geom="text",y=factor(1920),x=4,size=3,color="white",hjust=0,label="3 medalhas no tiro esportivo")+
    annotate(geom="text",y=factor(1948),x=2,size=3,color="white",hjust=0,label="basquete")+
    annotate(geom="text",y=factor(1952),x=4,size=3,color="white",hjust=0,label="atletismo e natação")+
    annotate(geom="text",y=factor(1956),x=2,size=3,color="white",hjust=0,label="bicampeonato de Adhemar no salto triplo")+
    annotate(geom="text",y=factor(1960),x=3,size=3,color="white",hjust=0,label="atletismo e natação")+
    annotate(geom="text",y=factor(1964),x=2,size=3,color="white",hjust=0,label="basquete")+
    annotate(geom="text",y=factor(1968),x=4,size=3,color="white",hjust=0,label="atletismo, vela e boxe")+
    annotate(geom="text",y=factor(1972),x=3,size=3,color="white",hjust=0,label="atletismo e judô")+
    annotate(geom="text",y=factor(1976),x=3,size=3,color="white",hjust=0,label="atletismo e vela")+
    annotate(geom="text",y=factor(1980),x=5,size=3,color="white",hjust=0,label="2 ouros na vela, atletismo e natação")+
    annotate(geom="text",y=factor(1984),x=9,size=3,color="white",hjust=0,label="atletismo,vela,natação,judô,futebol,volei")+
    annotate(geom="text",y=factor(1988),x=7,size=3,color="white",hjust=0,label="ouro do Aurelio no judô,atletismo,futebol,vela")+
    annotate(geom="text",y=factor(1992),x=4,size=3,color="white",hjust=0,label="ouro do Rogério no judô,ouro no volei, prata na natação")+
    theme_void()+theme(plot.background=element_rect(fill="gray13",colour="gray13"),legend.position="none")
}

plot_athlete_medals_per_year <- function(athlete_medals_per_year_df){
  athlete_medals_per_year_df <- athlete_medals_per_year_df %>% 
    mutate(Atleta=ifelse(nchar(Atleta)>30,Modalidade,Atleta)) %>% 
    mutate(nchar=as.integer(nchar(Atleta)*1.25/2))
  for(r in 1:nrow(athlete_medals_per_year_df)){ # wrap strings
    athlete_medals_per_year_df[r,]$Atleta=
      ifelse(athlete_medals_per_year_df[r,]$nchar<8,athlete_medals_per_year_df[r,]$Atleta,
             paste(strwrap(athlete_medals_per_year_df[r,]$Atleta,width=athlete_medals_per_year_df[r,]$nchar), collapse="\n"))
  }
  athlete_medals_per_year_df %>% 
    left_join(df %>% mutate(Ano=as.character(Ano)) %>% select(Ano,`Colocação`,rank),by="Ano") %>% 
    filter(Esporte %in% c("vela","atletismo","judô","voleibol","natação")) %>% 
    mutate(Esporte=factor(Esporte,levels=c("vela","atletismo","judô","voleibol","natação"))) %>% 
    mutate(medalha_factor=factor(Medalha,levels=c("bronze","prata","ouro"))) %>% 
    group_by(year,medalha_factor,Ano,Medalha,Esporte,Atleta) %>% summarise(N=n()) %>% #filter(Esporte=="voleibol") %>% 
    arrange(year,desc(medalha_factor)) %>% 
    ggplot(aes(y=Ano))+ 
    geom_text(aes(x=(-.5),label=Ano),size=3,color="dodgerblue",hjust=0)+
    geom_col(aes(x=N,fill=medalha_factor),position="stack")+
    geom_text(aes(x=N,label=Atleta),position="stack",hjust=1.05,size=2)+
    scale_fill_manual(values=c("ouro"="gold","prata"="gray","bronze"="sienna3"))+
    facet_grid(cols=vars(Esporte))+
    theme_void()+theme(plot.background=element_rect(fill="gray13",colour="gray13"),legend.position="none")+
    theme(strip.text.x=element_text(colour='white',face="bold",size=19))
}


grid.arrange(
  plot_medals_per_year(df),
  plot_athlete_medals_per_year(athlete_medals_per_year_df),
  widths=c(1,4)
)


# CLEAN UP #################################################


