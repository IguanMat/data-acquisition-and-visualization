# LIBRERIE----------------------------------------------------------------------

library(shiny)
library(bs4Dash)
library(plotly)
library(thematic)
library(waiter)
library(leaflet)
library(leaflet.minicharts)
library(dplyr)
library(tidyr)
library(readr)

# CARICAMENTO DATI--------------------------------------------------------------

vgsales <- read_csv("C:/Users/matte/Downloads/R/VIDEOGAMES/vgsales.csv")
View(vgsales)

# PULIZIA-----------------------------------------------------------------------

# Copiamo il dataset vgsales in un nuovo dataframe chiamato df
df <- vgsales

# Rimpiazzamo i valori "N/A" con NA (valore mancante)
df[df == "N/A"] <- NA

# Rimuoviamo le righe che contengono valori mancanti
df <- df[complete.cases(df),]

# Convertiamo la colonna Year da factor a numerica
df$Year <- as.numeric(as.character(df$Year))

# Selezioniamo solo le righe dove Year è minore o uguale a 2016
df <- df[df$Year <= 2016,]

# Stampiamo un riepilogo statistico del dataframe df
summary(df)

# Mostriamo la struttura del dataframe df
str(df)

# Creiamo un nuovo dataframe df2, che copia df e sostituisce i nomi delle piattaforme con "Others" se hanno meno di 1000 occorrenze
df2 <- df %>%
  mutate(Platform = ifelse(table(Platform)[as.character(Platform)] <= 1000, "Others", as.character(Platform)))

# Creiamo una variabile var1 che contiene tutti i valori unici nella colonna Year di df
var1 <- unique(df$Year)

# Creiamo una variabile var2 che contiene tutti i valori unici nella colonna Platform di df
var2 <- unique(df$Platform)
var2 <- var2[!var2 %in% c("TG16", "GG", "PCFX")]

# Creiamo una variabile var3 che contiene tutte le colonne di df tranne le prime 6 
var3 <- df[-c(1:6)] %>%
  names()

# Modifichiamo la funzione aggiungendo il colore di sfondo trasparente in "style"
bs4SidebarUserPanel <- function(name, image = NULL) {
  shiny::tags$div(
    class = "user-panel mt-3 pb-3 mb-3 d-flex",
    if (!is.null(image)) {
      shiny::tags$div(
        class = "image",
        shiny::img(src = image, class = "img-circle elevation-2")
      )
    },
    shiny::tags$div(
      class = "info",
      shiny::a(class = "d-block", href = "#", name),
      style = "background-color: transparent !important;"
    ),
    style = "background-color: transparent;"
  )
}

# MAPPE-------------------------------------------------------------------------

# URL per le tiles dell'immagine satellitare di ArcGIS
tilesURL3 <- "https://services.arcgisonline.com/arcgis/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"

# Vendite per regione #
# Selezioniamo le colonne 7-9 (EU_Sales, NA_Sales, JP_Sales) dal dataframe df e calcoliamo la somma per ciascuna colonna
sales_sum <- df[7:9] %>%
  summarise_all(sum)

# Creiamo un dataframe chiamato sales_map con le colonne region, lat, lng, sales e color
sales_map <- data.frame(region = c("Europe", "North America", "Japan"),
                        lat = c(52.5200, 37.0902, 35.6895),
                        lng = c(13.4050, -95.7129, 139.6917),
                        sales = c(sales_sum$EU_Sales, sales_sum$NA_Sales, sales_sum$JP_Sales),
                        color = c("#FFC107", "#009dda", "#E91E63"))

# Vendite per piattaforma #
# Raggruppiamo il dataframe df per piattaforma e calcoliamo la somma delle vendite per regione
sales_by_platform <- df %>%
  group_by(Platform) %>%
  summarise(EU_Sales = sum(EU_Sales),
            NA_Sales = sum(NA_Sales),
            JP_Sales = sum(JP_Sales))

# Trasponiamo il dataframe sales_by_platform in modo da avere una colonna per regione e una per piattaforma
sales_by_platform_transposed <- sales_by_platform %>%
  pivot_longer(cols = c(EU_Sales, NA_Sales, JP_Sales), names_to = "Region", values_to = "Sales") %>%
  pivot_wider(names_from = "Platform", values_from = "Sales")

# Aggiungiamo le colonne latitude e longitude a sales_by_platform_transposed
sales_by_platform_transposed$latitude <- c(52.5200, 37.0902, 35.6895)
sales_by_platform_transposed$longitude <- c(13.4050, -95.7129, 139.6917)

# Creiamo un vettore di colori per le piattaforme
colors <- c("#FF5733", "#FFC300", "#FF6384", "#8B0000", "#00FF7F", "#9370DB", "#FFFF00")

# Top Publishers
# Raggruppiamo il dataframe df per publisher e calcoliamo la somma delle vendite per regione,
# ordiniamo i risultati in ordine decrescente e selezioniamo solo i primi 5 editori
top_publishers <- df %>%
  group_by(Publisher) %>%
  summarise(EU_Sales = sum(EU_Sales),
            NA_Sales = sum(NA_Sales),
            JP_Sales = sum(JP_Sales)) %>%
  arrange(desc(EU_Sales), desc(NA_Sales), desc(JP_Sales)) %>%
  slice(1:5)

# Trasponiamo il dataframe top_publishers in modo da avere una colonna per regione e una per editore
top_publishers_transposed <- top_publishers %>%
  pivot_longer(cols = c(EU_Sales, NA_Sales, JP_Sales), names_to = "Region", values_to = "Sales") %>%
  pivot_wider(names_from = "Publisher", values_from = "Sales")

# Aggiungiamo le colonne latitude e longitude a top_publishers_transposed
top_publishers_transposed$latitude <- c(52.5200, 37.0902, 35.6895)
top_publishers_transposed$longitude <- c(13.4050, -95.7129, 139.6917)

# ALTRO-------------------------------------------------------------------------

# Chiamiamo la funzione prima di eseguire il codice
thematic_shiny()

statusColors <- c(
  "gray-dark",
  "gray",
  "secondary",
  "navy",
  "indigo",
  "purple",
  "primary",
  "lightblue",
  "info",
  "success",
  "olive",
  "teal",
  "lime",
  "warning",
  "orange",
  "danger",
  "fuchsia",
  "maroon",
  "pink",
  "white"
)

# TABELLE-----------------------------------------------------------------------

# Definiamo una scheda dell'interfaccia utente composta da tre tab diverse: "Dati", "Struttura" e "Summary".
# Il layout della scheda utilizza una riga fluida e una colonna di larghezza 12 per contenere la scheda stessa.
# La scheda è definita con tabBox(), con l'opzione type impostata su "tabs" per creare una scheda a più pagine.
# L'opzione selected è impostata su "Dati" per indicare che la prima scheda dovrebbe essere selezionata all'avvio.
# La funzione ribbon() è utilizzata per creare un'intestazione a forma di nastro sopra la scheda,
# impostando il testo su NULL e il colore su "danger". La scheda è impostata come "closable = FALSE"
# e "collapsible = FALSE" per impedire all'utente di chiudere o ridurre la scheda.
dataset_tab <- tabItem(
  tabName = "dataset",
  fluidRow(
    column(
      width = 12,
      tabBox(
        ribbon(
          text = NULL,
          color = "danger"
        ),
        title = NULL,
        elevation = 2,
        id = "tabcard1",
        width = 12,
        collapsible = FALSE, 
        closable = FALSE,
        type = "tabs",
        status = "purple",
        solidHeader = TRUE,
        selected = "Dati",
        tabPanel(
          "Dati",
          DT::dataTableOutput("dataT")
        ),
        tabPanel(
          "Struttura",
          verbatimTextOutput("structure")
        ),
        tabPanel(
          "Summary",
          verbatimTextOutput("summary")
        )
      )
    )
  )
)

social_cards_tab <- tabItem(
  # Creiamo una scheda chiamata "socialcards"
  tabName = "socialcards",
  fluidRow(
    # Creiamo il primo box utente con l'immagine, il titolo e il sottotitolo
    userBox(
      title = userDescription(
        image = "https://i.ibb.co/61ry3k5/profile-pic.png",
        title = "Matteo Gurrieri",
        subtitle = "Kaggle Contributor"
      ),
      collapsible = FALSE,
      status = "success", # Colore di sfondo del box
      elevation = 4, # Ombreggiatura del box
      tags$a(href="https://www.linkedin.com/in/matteogurrieri/", icon("linkedin"), "LinkedIn "),
      tags$a(href="https://github.com/IguanMat", icon("github"), "  GitHub")
    ),
    userBox(
      title = userDescription(
        image = "https://i.ibb.co/wpq09Vc/IMG-20221214-232033.jpg",
        title = "Riccardo Bianchi",
        subtitle = "Sports Enthusiast"
      ),
      collapsible = FALSE,
      status = "danger",
      elevation = 4,
      tags$a(href="https://www.linkedin.com/in/riccardo-bianchi-4928b0251/", icon("linkedin"), "LinkedIn ")
    )
  ),
  tags$hr(), # Aggiungiamo una riga orizzontale
  fluidRow(
    userBox(
      title = userDescription(
        image = "https://i.ibb.co/942B7rS/IMG-20221214-232055.jpg",
        title = "Edoardo Mercuri",
        subtitle = "Trivia Crack Boy"
      ),
      collapsible = FALSE,
      status = "orange",
      elevation = 4,
      tags$a(href="https://www.linkedin.com/in/edoardo-mercuri-b50a04256/", icon("linkedin"), "LinkedIn ")
    ),
    userBox(
      title = userDescription(
        image = "https://i.ibb.co/HKTKVKJ/IMG-20221214-232816-1.jpg",
        title = "Paolo Losacco",
        subtitle = "Procastinatore Seriale"
      ),
      collapsible = FALSE,
      status = "navy",
      elevation = 4,
      tags$a(href="https://www.linkedin.com/in/paolo-losacco-888278239/", icon("linkedin"), "LinkedIn ")
    )
  )
)

dashboard_tab <- tabItem(
  tabName = "dashboard",
  fluidRow(
    # Il primo bs4InfoBox contiene uno sliderInput che permette di selezionare un intervallo di anni da visualizzare nei grafici successivi.
    bs4InfoBox(title = "",
               iconElevation = 0,
               icon = shiny::icon("calendar"), 
               width = 4,
               elevation = 2,
               sliderInput("years", "Seleziona un intervallo di anni", min = min(df$Year), max = max(df$Year), value = c(min(df$Year), max(df$Year)), width = "100%",sep ="")
    ),
    # Il secondo bs4InfoBox contiene una selectInput che permette di selezionare la piattaforma dei videogiochi da visualizzare nei grafici successivi.
    bs4InfoBox(title = "",
               iconElevation = 0,
               icon = shiny::icon("gamepad"), 
               width = 4,
               elevation = 2,
               selectInput("var2", "Seleziona la piattaforma", choices = var2, selected = "PC", multiple = FALSE, width = "100%")
               ),
    # Il terzo bs4InfoBox contiene una selectInput che permette di selezionare la regione geografica dei dati di vendita da visualizzare nei grafici successivi.
    bs4InfoBox(title = "",
               iconElevation = 0,
               icon = shiny::icon("globe"), 
               width = 4,
               elevation = 2,
               selectInput("var3", "Seleziona la regione", choices = var3, selected = "Global_Sales", multiple = FALSE, width = "100%")
    )
  ),
  # Dopo i bs4InfoBox, ci sono due fluidRow che contengono ciascuno due box.
  # Questi box contengono i grafici visualizzati in base alle selezioni effettuate nei widget precedenti.
  # I grafici sono creati con la funzione plotlyOutput e sono definiti come "plot1", "plot2", "plot3" e "plot4".
  fluidRow(
    box(id = "histbox",width = 8, maximizable = TRUE, status = 'purple', solidHeader = F, plotlyOutput("plot1", height = "177", width = "100%")),
    box(width = 4, maximizable = TRUE, status = 'purple', solidHeader = F, plotlyOutput("plot2", height = "177", width = "100%"))
  ),
  fluidRow(
    box(width = 4, maximizable = TRUE, status = 'purple', solidHeader = F, plotlyOutput("plot3", height = "177", width = "100%")),
    box(width = 8, maximizable = TRUE, status = 'purple', solidHeader = F, plotlyOutput("plot4", height = "177", width = "100%"))
  )
)

maps_tab1 <- tabItem(
  tabName = "sales_region",
  fluidRow(
    box(
      width = 12, maximizable = TRUE, collapsible = FALSE, status = 'purple', solidHeader = F,
      leafletOutput("map1", height = "600", width = "100%"))
  )
)

maps_tab2 <- tabItem(
  tabName = "sales_platform",
  fluidRow(
    box(
      width = 12, maximizable = TRUE, collapsible = FALSE, status = 'purple', solidHeader = F,
      leafletOutput("map2", height = "600", width = "100%"))
  )
)

maps_tab3 <- tabItem(
  tabName = "top_publishers",
  fluidRow(
    box(
      width = 12, maximizable = TRUE, collapsible = FALSE, status = 'purple', solidHeader = F,
      leafletOutput("map3", height = "600", width = "100%"))
  )
)

# SHINYAPP----------------------------------------------------------------------

shinyApp(
  # Definizione dell'interfaccia utente (UI)
  ui = dashboardPage(
    # Configurazione del preloader
    preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
    # Configurazione del tema scuro
    dark = TRUE,
    help = FALSE,
    # Configurazione della visualizzazione a schermo intero
    fullscreen = TRUE,
    # Configurazione del pulsante "scroll to top"
    scrollToTop = TRUE,
    # Configurazione dell'intestazione della dashboard
    header = dashboardHeader(
      # Configurazione del titolo e del logo della dashboard
      title = dashboardBrand(
        title = "VIDEOGAME SALES",
        color = "purple",
        href = "https://www.kaggle.com/datasets/gregorut/videogamesales",
        image = "https://e7.pngegg.com/pngimages/977/162/png-clipart-video-game-game-controllers-gaming-miscellaneous-game.png",
        opacity = 0.8
      ),
      # Configurazione del pannello utente
      fixed = TRUE,
      rightUi = tagList(
        userOutput("user")
      )
    ),
    # Configurazione della sidebar
    sidebar = dashboardSidebar(
      fixed = TRUE,
      skin = "light",
      status = "purple",
      id = "sidebar",
      # Configurazione di un'area personalizzata
      customArea = fluidRow(
        actionButton(
          inputId = "myAppButton",
          label = "Clicca qui!",
          icon = icon("education", lib = "glyphicon"),
          width = NULL,
          status = "danger",
          style = "margin: auto"#,
          #dashboardBadge(textOutput("btnVal"), color = "danger")
        )
      ),
      # Configurazione del pannello utente della sidebar
      bs4SidebarUserPanel(
        image = "http://rstudio.github.io/shiny/reference/figures/logo.png",
        name = "Developed with Shiny!"
      ),
      # Configurazione del menu della sidebar
      sidebarMenu(
        id = "current_tab",
        flat = FALSE,
        compact = FALSE,
        childIndent = TRUE,
        # Definizione dei tab della sidebar
        menuItem(
          "Dashboard",
          tabName = "dashboard",
          icon = icon("dashboard")
        ),
        menuItem(
          text = "Maps",
          icon = icon("map"),
          startExpanded = FALSE,
          # Definizione dei sotto-tab
          menuSubItem(
            text = HTML(
              paste(
                "Sales by Region"
              )
            ),
            tabName = "sales_region",
            icon = icon("globe", lib = "glyphicon")
          ),
          menuSubItem(
            text = HTML(
              paste(
                "Sales by Platform"
              )
            ),
            tabName = "sales_platform",
            icon = icon("gamepad")
          ),
          menuSubItem(
            text = HTML(
              paste(
                "Top Publishers"
              )
            ),
            tabName = "top_publishers",
            icon = icon("signal", lib = "glyphicon")
          )
        ),
        # Definizione dei un header nella sidebar
        sidebarHeader("More"),
        menuItem(
          "Dataset",
          tabName = "dataset",
          icon = icon("database")
        ),
        menuItem(
          "Our Group",
          tabName = "socialcards",
          icon = icon("users")
        )
      )
    ),
    # Configurazione della struttura
    body = dashboardBody(
      # Definiamo una funzione che consenta la visualizzazione corretta dei grafici e il loro ingrandimento.
      tags$head(
        tags$script(
          "$(function() {
              $('[data-card-widget=\"maximize\"]').on('click', function() {
                setTimeout(function() {
                  var isMaximized = $('html').hasClass('maximized-card');
                  if (isMaximized) {
                    $('#plot1').css('height', '100%');
                    $('#plot2').css('height', '100%');
                    $('#plot3').css('height', '100%');
                    $('#plot4').css('height', '100%');
                    $('#map1').css('height', '100%');
                    $('#map2').css('height', '100%');
                    $('#map3').css('height', '100%');
                  } else {
                    $('#plot1').css('height', '177px');
                    $('#plot2').css('height', '177px');
                    $('#plot3').css('height', '177px');
                    $('#plot4').css('height', '177px');
                    $('#map1').css('height', '600px');
                    $('#map2').css('height', '600px');
                    $('#map3').css('height', '600px');
                  }
                }, 300);
                $('#plot1').trigger('resize');
                $('#plot2').trigger('resize');
                $('#plot3').trigger('resize');
                $('#plot4').trigger('resize');
                $('#map1').trigger('resize');
                $('#map2').trigger('resize');
                $('#map3').trigger('resize');
              });
            });
            "
        )
      ),
      tabItems(
        dashboard_tab,
        maps_tab1,
        maps_tab2,
        maps_tab3,
        dataset_tab,
        social_cards_tab
      )
    ),
    # Configurazione della barra di controllo sulla destra
    controlbar = dashboardControlbar(
      id = "controlbar",
      skin = "light",
      pinned = TRUE,
      overlay = FALSE,
      controlbarMenu(
        id = "controlbarMenu",
        type = "pills",
        controlbarItem(
          "Skin",
          # Funzione che permette di cambiare i colori di alcuni elementi dell'applicazione
          skinSelector()
        )
      )
    ),
    # Configurazione del footer
    footer = dashboardFooter(
      fixed = FALSE,
      left = a(
        href = "https://www.lumsa.it/",
        target = "_blank", "@LUMSA"
      ),
      right = "2023"
    ),
    title = "Project ACVIDA"
  ),
  # Definizione della parte server 
  server = function(input, output, session) {
    useAutoColor()
    
    # app button in fondo a destra ---------------------------------------------
    #output$btnVal <- renderText(input$myAppButton)
    observeEvent(input$myAppButton, {
      showModal(modalDialog(a(
        href = "https://www.lumsa.it/didattica/corsi-di-laurea/roma/triennale/tecniche-informatiche-gestione-dati",
        target = "_blank", "SCOPRI IL NOSTRO CORSO DI LAUREA!"), align = "center", easyClose = TRUE))
    })
    
    output$dataT <- DT::renderDataTable(
      DT::datatable(vgsales, options=list(scrollX = T)) 
    )
    
    output$structure <- renderPrint({
      vgsales %>%
        str()
    })
    
    output$summary <- renderPrint(
      vgsales %>%
        summary()
    )
    
    # La funzione observe() è una funzione reattiva che monitora le modifiche di un input e aggiorna l'output di conseguenza.
    observe({
      
      # Selezioniamo una piattaforma di interesse tramite input$var2.
      platform_of_interest <- input$var2
      
      # Effetuiamo una sotto-selezione dei dati del dataframe df in base alla piattaforma selezionata.
      platform_data <- subset(df, Platform == platform_of_interest)
      
      # Se non ci sono righe nel dataframe per la piattaforma selezionata, viene restituito NULL.
      if (nrow(platform_data) == 0) {
        return(NULL)
      }
      
      # Aggreghiamo i dati delle vendite per anno, utilizzando la funzione aggregate().
      sales_data <- aggregate(cbind(NA_Sales, EU_Sales, JP_Sales, Other_Sales, Global_Sales) ~ Year, 
                              data = platform_data, FUN = sum)
      
      # Selezionato un intervallo di anni tramite l'input$years.
      selected_years <- input$years
      
      # Effetuiamo una sotto-selezione dei dati delle vendite in base all'intervallo di anni selezionato.
      sales_data <- subset(sales_data, Year >= selected_years[1] & Year <= selected_years[2])
      
      # Generiamo un grafico interattivo utilizzando la libreria plotly.
      output$plot1 <- renderPlotly({
        plot_ly(sales_data, x = ~Year, y = ~NA_Sales, name = "NA Sales", type = "scatter", mode = "lines") %>%
          add_trace(y = ~EU_Sales, name = "EU Sales", type = "scatter", mode = "lines") %>%
          add_trace(y = ~JP_Sales, name = "JP Sales", type = "scatter", mode = "lines") %>%
          add_trace(y = ~Other_Sales, name = "Other Sales", type = "scatter", mode = "lines") %>%
          add_trace(y = ~Global_Sales, name = "Global Sales", type = "scatter", mode = "lines") %>%
          layout(title = paste("Sales of", platform_of_interest, "Games by Year"),
                 font = if (input$dark_mode) list(color = "white", family = "Arial Black") else list(color = "black", family = "Arial Black"),
                 xaxis = list(title = "Year"),
                 yaxis = list(title = "Sales")) %>%
          layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                 paper_bgcolor = "rgba(0, 0, 0, 0)",
                 fig_bgcolor   = "rgba(0, 0, 0, 0)")
      })
    })
    
    output$plot2 <- renderPlotly({
      # Creiamo un grafico a torta che mostra le etichette delle piattaforme e i valori delle vendite di input$var3
      plot_ly(df2, labels = ~Platform, values = ~get(input$var3), type = 'pie', textinfo = "none") %>%
        # Impostiamo il titolo del grafico come "input$var3 by Platform"
        layout(title = paste(input$var3, "by Platform"),
               # Impostiamo la famiglia di caratteri e il colore del font in base all'input di dark_mode
               font = if (input$dark_mode) list(color = "white", family = "Arial Black") else list(color = "black", family = "Arial Black")) %>%
        layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
               paper_bgcolor = "rgba(0, 0, 0, 0)",
               fig_bgcolor   = "rgba(0, 0, 0, 0)")
    })
    
    output$plot3 <- renderPlotly({
      
      # Creiamo un nuovo dataframe con solo i dati relativi alla piattaforma selezionata da input$var2
      df_platform <- subset(df, Platform == input$var2)
      
      # Se il dataframe creato è vuoto, restituisci NULL per non creare il grafico
      if (nrow(df_platform) == 0) {
        return(NULL)
      }
      
      # Creiamo un grafico a torta che mostra le etichette dei generi e i valori delle vendite globali
      plot_ly(df_platform, labels = ~Genre, values = ~Global_Sales, type = 'pie', hole = 0.6, textinfo = "none") %>%
        layout(title = paste("Genres of Games on", input$var2),
               font = if (input$dark_mode) list(color = "white", family = "Arial Black") else list(color = "black", family = "Arial Black")) %>%
        layout(plot_bgcolor = "rgba(0, 0, 0, 0)",
               paper_bgcolor = "rgba(0, 0, 0, 0)",
               fig_bgcolor = "rgba(0, 0, 0, 0)")
    })
    
    output$plot4 <- renderPlotly({
      
      # Creiamo un nuovo dataframe con le vendite aggregate per genere
      df_sales <- aggregate(df[, c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales")], 
                            by = list(Genre = df$Genre), FUN = sum)
      
      # Creiamo un grafico a barre che mostra i valori di input$var3 per ogni genere
      plot_ly(df_sales, x = ~Genre, y = ~get(input$var3), type = 'bar', marker = list(color = "#FF7F00")) %>%
        layout(title = paste(input$var3, "by Genre"),
               xaxis = list(title = "Genre"),
               yaxis = list(title = paste(input$var3)),
               font = if (input$dark_mode) list(color = "white", family = "Arial Black") else list(color = "black", family = "Arial Black")) %>%
        layout(plot_bgcolor = "rgba(0, 0, 0, 0)",
               paper_bgcolor = "rgba(0, 0, 0, 0)",
               fig_bgcolor = "rgba(0, 0, 0, 0)"
        )
    })
    
    # Mappa con mini grafici in base alla posizione geografica delle vendite.
    output$map1 <- renderLeaflet({
      # leaflet() viene utilizzato per creare la mappa.
      leaflet() %>%
        #addMinicharts() viene utilizzato per aggiungere i mini grafici sulla mappa.
        addMinicharts(
          # sales_map contiene le coordinate geografiche di ogni regione, le vendite e il colore corrispondente per ogni regione.
          sales_map$lng, sales_map$lat,
          chartdata = sales_map$sales,
          fillColor = sales_map$color,
          showLabels = TRUE,
          width = 45,
          layerId = sales_map$region
        ) %>%
        # addLegend() viene utilizzato per aggiungere la legenda alla mappa, mostrando il colore e l'etichetta per ogni regione.
        addLegend(
          "topright",
          colors = unique(sales_map$color), opacity = 1,
          labels = unique(sales_map$region)
        ) %>% addTiles(tilesURL3) # addTiles() viene utilizzato per aggiungere i tiles della mappa.
    }) 
    
    # Mappa con mini grafici a torta per ogni posizione geografica delle vendite, in base alla piattaforma.
    output$map2 <- renderLeaflet({
      leaflet() %>%
        addMinicharts(
          sales_by_platform_transposed$longitude, sales_by_platform_transposed$latitude,
          type = "pie",
          chartdata = sales_by_platform_transposed[, c("PS2", "X360", "PS3", "Wii", "DS", "PS", "PSP")], 
          colorPalette = colors, 
          showLabels = FALSE,
          width = 45,
        ) %>% addTiles(tilesURL3)
    })
    
    # Mappa con mini grafici per ogni posizione geografica delle vendite, in base ai top publishers
    output$map3 <- renderLeaflet({
      leaflet() %>%
        addMinicharts(
          top_publishers_transposed$longitude, top_publishers_transposed$latitude,
          chartdata = top_publishers_transposed[, c("Nintendo", "Electronic Arts", "Activision", "Sony Computer Entertainment", "Ubisoft")], 
          colorPalette = colors, 
          showLabels = FALSE,
          width = 75,
        ) %>% addTiles(tilesURL3)
    })
    
    # tab di avviso ------------------------------------------------------------
    
    observeEvent(input$current_tab, {
      if (input$current_tab == "dashboard") {
        showModal(modalDialog(
          tags$h2("IMPORTANTE!", style = "text-align:center;"),
          align = "center",
          "Ti suggeriamo di fare clic su 'Open in Browser' se stai utilizzando questo codice R per visualizzare questa Shiny App, al fine di ottenere un'esperienza migliore.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    })
    
    # info sul tema corrente ---------------------------------------------------
    
    observeEvent(input$dark_mode, {
      toast(
        title = if (input$dark_mode) "Dark theme on!" else "Light theme on",
        options = list(position = "topRight", class = "bg-warning", autohide = TRUE)
      )
    })
    
    # menù utente --------------------------------------------------------------
    
    output$user <- renderUser({
      dashboardUser(
        #name = "Matteo Gurrieri",
        image = "https://cdn-icons-png.flaticon.com/512/21/21104.png",
        title = "You"
        #subtitle = "Student at LUMSA University",
        #footer = p("Google Cert. Data Analyst | Kaggle Contributor", class = "text-center")
      )
    })
  }
)

