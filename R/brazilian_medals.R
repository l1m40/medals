# File:   
# 

# INSTALL AND LOAD PACKAGES ################################
#

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
# Packages I load every time; uses "pacman"
pacman::p_load(pacman,tidyverse,rvest,grid,gridExtra) 





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

rvest_wiki_html <- function(){
  url="https://pt.wikipedia.org/wiki/Lista_de_medalhas_brasileiras_nos_Jogos_Ol%C3%ADmpicos"
  cat(paste("Reading from",url,"\n"))
  return(url %>% read_html())
}

read_table_medals_per_year_df <- function(html=rvest_wiki_html()){
  # load medals per year
  df <- html %>% html_node(xpath='//*[@id="mw-content-text"]/div[1]/table[1]') %>% html_table() %>% as_tibble()
  # setup city ticker
  df$ticker <- NA
  for(i in 1:nrow(df)) df$ticker[i]=olympic_cities[[df$Local[i]]]
  # transform medals per year variables
  df <- df %>% 
    mutate(Ouro=as.integer(Ouro),Prata=as.integer(Prata),Bronze=as.integer(Bronze),Total=as.integer(Total)) %>% 
    mutate(rank=as.integer(str_remove(`Colocação`,"º"))) %>% 
    mutate(year=factor(Ano)) %>% 
    ungroup()
  return(df)
}

read_table_medals_per_athlete_df <- function(html=rvest_wiki_html()){# load athletes medals
  athlete_medals_per_year_df <- NULL
  for(i in 4:24){
    athlete_medals_per_year_df <- athlete_medals_per_year_df %>% 
      rbind(
        html %>% html_node(xpath=paste0('//*[@id="mw-content-text"]/div[1]/table[',i-1,']')) %>% html_table() %>% as_tibble() %>% 
          mutate(event_name=(html %>% html_node(xpath=paste0('//*[@id="mw-content-text"]/div[1]/div[',i,']')) %>% html_elements("a") %>% html_attr("title"))[1]) %>% 
          mutate(Local=strsplit(as.character(event_name), " (?=[0-9]{4}$)", perl=TRUE)[[1]][1]) %>% 
          mutate(  Ano=strsplit(as.character(event_name), " (?=[0-9]{4}$)", perl=TRUE)[[1]][2]) %>% 
          mutate(year=factor(Ano))
      )
  }
  # transform athletes medals
  athlete_medals_per_year_df <- athlete_medals_per_year_df %>% 
    mutate(medalha_factor=factor(Medalha,levels=c("bronze","prata","ouro"))) %>% 
    mutate(medalha_nome=ifelse(nchar(Atleta)>30,Modalidade,Atleta)) %>% 
    mutate(nchar=as.integer(nchar(medalha_nome)*1.25/2))
  athlete_medals_per_year_df$medalha_nome_8  <- NA
  athlete_medals_per_year_df$medalha_nome_13 <- NA
  for(r in 1:nrow(athlete_medals_per_year_df)){ # wrap strings
    athlete_medals_per_year_df[r,]$medalha_nome_8 =ifelse(athlete_medals_per_year_df[r,]$nchar<8 ,athlete_medals_per_year_df[r,]$medalha_nome,paste(strwrap(athlete_medals_per_year_df[r,]$medalha_nome,width=athlete_medals_per_year_df[r,]$nchar), collapse="\n"))
    athlete_medals_per_year_df[r,]$medalha_nome_13=ifelse(athlete_medals_per_year_df[r,]$nchar<13,athlete_medals_per_year_df[r,]$medalha_nome,paste(strwrap(athlete_medals_per_year_df[r,]$medalha_nome,width=athlete_medals_per_year_df[r,]$nchar), collapse="\n"))
  }
  return(athlete_medals_per_year_df)
}
medals_year_sport_max <- function(df){ return(max((df %>% group_by(Ano,Esporte) %>% summarise(N=n(),.groups="keep"))$N)) }



plot_medals_per_year <- function(df=read_table_medals_per_year_df()){
  df %>% 
    mutate(text_color=as.integer(ifelse(`Colocação`=="NP",max(rank,na.rm=T)+10,ifelse(`Colocação`=="SC",max(rank,na.rm=T),rank)))) %>% 
    mutate(text_color=ifelse(text_color<quantile(text_color,.1,na.rm=T),1,text_color)) %>% 
    ggplot(aes(y=year))+
    geom_text(aes(x=(-.5),color=-text_color,fontface="bold",size=6,label=paste(`Colocação`,ticker,Ano)),hjust=1)+xlim((-10),max(as.integer(df$Total),na.rm=T)+1)+
    geom_col(aes(x=Prata+Ouro+Bronze),fill="sienna3")+
    geom_col(aes(x=Prata+Ouro),fill="gray")+
    geom_col(aes(x=Ouro),fill="gold")+
    geom_text(aes(x=Ouro+Prata+Bronze ,label=ifelse(as.integer(Ano)>=1996 & Bronze>0,Bronze,"")),hjust=1.5,size=3)+
    geom_text(aes(x=Ouro+Prata        ,label=ifelse(as.integer(Ano)>=1996 & Prata >0,Prata ,"")),hjust=1.5,size=3)+
    geom_text(aes(x=Ouro              ,label=ifelse(as.integer(Ano)>=1996 & Ouro  >0,Ouro  ,"")),hjust=1.5,size=3)+
    annotate(geom="text",y=factor(1920),x=4,size=3,color="white",hjust=0,label="3 medalhas no tiro esportivo")+
    annotate(geom="text",y=factor(1948),x=2,size=3,color="white",hjust=0,label="basquete")+
    annotate(geom="text",y=factor(1952),x=4,size=3,color="white",hjust=0,label="atletismo e natação")+
    annotate(geom="text",y=factor(1956),x=2,size=3,color="white",hjust=0,label="bicampeonato de Adhemar \nno salto triplo")+
    annotate(geom="text",y=factor(1960),x=3,size=3,color="white",hjust=0,label="atletismo e natação")+
    annotate(geom="text",y=factor(1964),x=2,size=3,color="white",hjust=0,label="basquete")+
    annotate(geom="text",y=factor(1968),x=4,size=3,color="white",hjust=0,label="atletismo, vela e boxe")+
    annotate(geom="text",y=factor(1972),x=3,size=3,color="white",hjust=0,label="atletismo e judô")+
    annotate(geom="text",y=factor(1976),x=3,size=3,color="white",hjust=0,label="atletismo e vela")+
    annotate(geom="text",y=factor(1980),x=5,size=3,color="white",hjust=0,label="2 ouros na vela, \natletismo e natação")+
    annotate(geom="text",y=factor(1984),x=9,size=3,color="white",hjust=0,label="atletismo, vela, natação, \njudô, futebol, volei")+
    annotate(geom="text",y=factor(1988),x=7,size=3,color="white",hjust=0,label="ouro do Aurelio no judô,\natletismo, futebol, vela")+
    annotate(geom="text",y=factor(1992),x=4,size=3,color="white",hjust=0,label="ouro do Rogério no judô, \nouro no volei, prata na natação")+
    theme_void()+theme(plot.background=element_rect(fill="gray13",colour="gray13"),legend.position="none")
}

plot_athlete_medals_per_year <- function(df=read_table_medals_per_athlete_df(),medals_per_year_df=read_table_medals_per_year_df(),max_input=NULL,text_size=5,sport_input="vela",title_on=T){
  if(is.null(max_input)) max_input=medals_year_sport_max(df)
  if(text_size<5) { df$medalha_nome <- df$medalha_nome_8 
           } else { df$medalha_nome <- df$medalha_nome_13 }
  plot <- df %>%
    filter(Esporte==sport_input) %>% 
    full_join(medals_per_year_df %>% mutate(Ano=as.character(Ano)) %>% distinct(Ano),by="Ano") %>%
    group_by(year,medalha_factor,Ano,Medalha,Esporte,medalha_nome) %>% summarise(N=n(),.groups="keep") %>%
    # tail(10)
    arrange(year,desc(medalha_factor)) %>% mutate(N=ifelse(is.na(Medalha),0,N)) %>% mutate(Esporte=sport_input) %>%
    ggplot(aes(y=Ano))+xlim((-.6),max_input)+
    geom_text(aes(x=(-.6),label=Ano),size=text_size+1,color="dodgerblue",hjust=0)+
    geom_col(aes(x=N,fill=medalha_factor),position="stack")+
    geom_text(aes(x=N,label=medalha_nome),position="stack",hjust=1.05,size=text_size-1)+
    # geom_text(aes(x=N,label=medalha_nome),position="stack",hjust=1)+
    scale_fill_manual(values=c("ouro"="gold","prata"="gray","bronze"="sienna3"))+
    # facet_grid(cols=vars(Esporte))+
    theme_void()+
    theme(plot.background=element_rect(fill="gray13",colour="gray13"),legend.position="none")#+theme(strip.text.x=element_text(colour='white',face="bold",size=19))
  if(!is.null(title_on))                 plot <- plot+facet_grid(cols=vars(Esporte))+theme(strip.text.x=element_text(colour='white',face="bold",size=19))
  if(sport_input=="tiro esportivo")      plot <- plot+annotate(geom="text",y=("1912"),x=max_input,size=5,color="white",hjust=1,vjust=1,label="* 3 primeiras medalhas do Brasil\nforam no tiro esportivo")
  if(sport_input=="atletismo")           plot <- plot+annotate(geom="text",y=("1956"),x=max_input,size=5,color="white",hjust=1,vjust=0,label="* 2 ouros seguidos de Adhemar\nno salto triplo")
  if(sport_input=="atletismo")           plot <- plot+annotate(geom="text",y=("2004"),x=max_input,size=3,color="white",hjust=1,vjust=1,label="* Wanderlei recebeu a Medalha Pierre de Coubertin\n pelo incidente do padre iralandês que o fez\nperder a medalha de ouro na maratona")
  if(sport_input=="atletismo")           plot <- plot+annotate(geom="text",y=("2012"),x=0        ,size=3,color="white",hjust=0,vjust=1,label="* Maureen Maggi ouro no salto em distância\nassim como nos Jogos Pan-Americanos")
  
  if(sport_input=="voleibol")            plot <- plot+annotate(geom="text",y=("2016"),x=max_input,size=5,color="white",hjust=1,vjust=.5,label="* 4 ouros seguidos nas equipes\nmasculina e feminina")#\nBrasil é o país do volei?
  if(sport_input=="voleibol")            plot <- plot+annotate(geom="text",y=("1992"),x=max_input,size=5,color="white",hjust=1,vjust=.5,label="* Ace do Marcelo Negrão\nno último ponto do 3x0 na Holanda")#\nBrasil é o país do volei?
  plot
}


if(F){
  
  source("R/brazilian_medals.R")
  
  
  html <- rvest_wiki_html()
  medals_per_year_df <- read_table_medals_per_year_df(html)
  athlete_medals_per_year_df <- read_table_medals_per_athlete_df(html)
  sport_list <- unique(athlete_medals_per_year_df$Esporte)
  max_medals <- medals_year_sport_max(athlete_medals_per_year_df)
  
  
  plot_medals_per_year(medals_per_year_df)
  
  athlete_medals_per_year_df %>% 
    filter(Esporte %in% c("vela","atletismo","judô","voleibol","natação")) %>% 
    mutate(Esporte=factor(Esporte,levels=c("vela","atletismo","judô","voleibol","natação"))) %>% 
    plot_athlete_medals_per_year(max_medals,3,c("vela","atletismo","judô","voleibol","natação"))+
    facet_grid(cols=vars(Esporte))+theme(strip.text.x=element_text(colour='white',face="bold",size=19))
  
  athlete_medals_per_year_df %>% 
    filter(Esporte %in% c("vela","atletismo","judô","voleibol","natação")) %>% 
    mutate(Esporte=factor(Esporte,levels=rev(c("vela","atletismo","judô","voleibol","natação")))) %>% 
    mutate(Ano=year) %>%
    plot_athlete_medals_per_year(max_medals,3,c("vela","atletismo","judô","voleibol","natação"))+
    # scale_y_reverse()+
    facet_grid(cols=vars(Esporte))+theme(strip.text.x=element_text(colour='white',face="bold",size=19))

  athlete_medals_per_year_df %>% #filter(grepl("Santos",Atleta)) %>% select(Ano,Esporte,Atleta)
    # filter(Esporte %in% c("tiro esportivo")) %>%
    # filter(Esporte %in% c("ginástica artística")) %>%
    # filter(Esporte %in% c("voleibol")) %>%
    # full_join(medals_per_year_df %>% mutate(Ano=as.character(Ano)) %>% distinct(Ano),by="Ano") %>% 
    # group_by(year,medalha_factor,Ano,Medalha,Esporte,medalha_nome) %>% summarise(N=n()) %>%
    #head(20)
    plot_athlete_medals_per_year(medals_per_year_df,max_medals,3,"voleibol")
    # plot_athlete_medals_per_year(max_medals,3,unique(athlete_medals_per_year_df$Esporte))
  
  grid.arrange(
    plot_medals_per_year(df),
    plot_athlete_medals_per_year(athlete_medals_per_year_df),
    widths=c(1,4)
  )
}


# CLEAN UP #################################################


