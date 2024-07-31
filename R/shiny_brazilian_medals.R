


if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman,shiny)

print(getwd())
# print(dir(file.path(getwd())))
# print(dir(file.path(getwd(),"R")))
if(file.exists(file.path(getwd(),"R","brazilian_medals.R"))){
  source("R/brazilian_medals.R")  
} else {
  source("brazilian_medals.R")
}



if(!exists("html")) html <- rvest_wiki_html()
medals_per_year_df <- suppressWarnings(read_table_medals_per_year_df(html))
athlete_medals_per_year_df <- suppressWarnings(read_table_medals_per_athlete_df(html))
sport_list <- sort(unique(athlete_medals_per_year_df$Esporte))
max_medals <- medals_year_sport_max(athlete_medals_per_year_df)
cat("\nInitialization done!\n\n")

plot_blank <- function(){ NULL %>% ggplot()+theme_void()+theme(plot.background=element_rect(fill="gray13",colour="gray13"),legend.position="none") }



ui <- fluidPage(
  tags$head(tags$style(HTML(" body { background-color: black; color: white; }
                            .justifyAlign { text-align:justify; }")),
            tags$script("
                                var dimension = [0, 0];
                                $(document).on('shiny:connected', function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange('dimension', dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange('dimension', dimension);
                                });
                        ")
            ),
  tabsetPanel(
    tabPanel("Medalhas olímpicas",
             h2("História das conquistas olímpicas do Brasil"),
             p("A participação do Brasil na história dos Jogos Olímpicos é marcada por um crescimento gradual e significativo desde sua primeira aparição em 1920. O país iniciou sua trajetória olímpica com um pequeno grupo de atletas e ao longo das décadas aumentou seu contingente e a diversidade de esportes nos quais compete. Este desenvolvimento refletiu-se não apenas na quantidade de medalhas conquistadas, mas também na excelência em várias modalidades, consolidando o Brasil como uma nação com um forte espírito olímpico.",style="text-align: justify;"),
             plotOutput("plot_medals_per_year",height="800px"),
             div("Visualização feita com os dados da wikipedia em",a(href="https://pt.wikipedia.org/wiki/Lista_de_medalhas_brasileiras_nos_Jogos_Ol%C3%ADmpicos","https://pt.wikipedia.org/wiki/Lista_de_medalhas_brasileiras_nos_Jogos_Ol%C3%ADmpicos")),
             br(),
             p("Entre os destaques da trajetória brasileira, o atleta Adhemar Ferreira da Silva brilhou ao conquistar o bicampeonato olímpico no salto triplo em 1952 e 1956. No campo dos esportes, a vela tem sido particularmente bem-sucedida, destacando-se com duas medalhas de ouro nos Jogos de 1980. O judô é outro esporte de grande destaque para o Brasil, sendo a modalidade que mais trouxe medalhas ao país ao longo dos anos. Por fim, o vôlei merece uma menção especial, com as seleções de vôlei de quadra conquistando quatro ouros consecutivos entre 2004 e 2016, firmando-se como uma potência mundial nesta modalidade.",style="text-align: justify;"),
             br(),
             uiOutput("img_main_sports"),
             br()
             ),
    tabPanel("por Esporte",
             fluidRow(
               column(6,selectInput("sport_select_input","Escolha um esporte:",sport_list))
               # column(6,textInput("athlete_text_input","Pesquise por um atleta"))
             ),
             plotOutput("plot_athlete_medals_per_year",height="1200px")
             )
  )
)



server <- function(input, output, session) {
  
  output$plot_medals_per_year <- renderPlot({ plot_medals_per_year(medals_per_year_df) })
  output$img_main_sports <- renderUI({ img(src="athletes_medals_90_degrees.png",width=input$dimension[1]-40) })
  
  output$plot_athlete_medals_per_year <- renderPlot({
    cat(paste("sport_select_input",input$sport_select_input,"\n"))
    if(input$sport_select_input!=""){
      # athlete_medals_per_year_df %>% 
      #   filter(Esporte %in% c(input$sport_select_input)) %>%
      #   full_join(medals_per_year_df %>% mutate(Ano=as.character(Ano)) %>% distinct(Ano),by="Ano") %>% 
      #   plot_athlete_medals_per_year(max_medals,4)
      # athlete_medals_per_year_df %>% 
      #   filter(Esporte %in% c(input$sport_select_input)) %>%
      #   full_join(medals_per_year_df %>% mutate(Ano=as.character(Ano)) %>% distinct(Ano),by="Ano") %>% 
      #   plot_athlete_medals_per_year(max_medals,4,unique(athlete_medals_per_year_df$Esporte))
      plot_athlete_medals_per_year(athlete_medals_per_year_df,medals_per_year_df,max_medals,4,input$sport_select_input)
    } else { plot_blank() }
  })
  
}



shinyApp(ui = ui, server = server)