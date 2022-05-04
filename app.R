library(ncdf4)
library(raster)
library(rgdal)
library(ggplot2)
library(stringr)

library(sf)
library(leaflet)
library(leaflegend)

library(shiny)
library(shinydashboard)
library(DT)

library(dplyr)
library(plotly)

ThaiBound1 <- raster::getData(name = "GADM", country = "THA", level = 1)
thaibound_sf_1 <- st_as_sf(ThaiBound1)
ThaiBound2 <- raster::getData(name = "GADM", country = "THA", level = 2)
thaibound_sf_2 <- st_as_sf(ThaiBound2)
 
all_df <- read.csv("csv_5_countries_2018-2019-2020-2021-1-10_op.csv")
df_thailand <- read.csv("csv_province_amphoe_thai.csv")
df <- read.csv("csv_5_countries_total_day_value_2018-2019-2020-2021-1-10_op.csv")

df2018 <- df %>% filter(time >= "2018-01-01") %>% filter(time <= "2018-12-31")
df2019 <- df %>% filter(time >= "2019-01-01") %>% filter(time <= "2019-12-31")
df2020 <- df %>% filter(time >= "2020-01-01") %>% filter(time <= "2020-12-31")
df2021 <- df %>% filter(time >= "2021-01-01") %>% filter(time <= "2021-12-31")

colors <- c("yellow", "red")
pal_frp <- colorNumeric(colors, all_df$frp_value)
pal_cofire <- colorNumeric(colors, all_df$cofire_value)
pal_co2fire <- colorNumeric(colors, all_df$co2fire_value)
pal_pm2p5fire <- colorNumeric(colors, all_df$pm2p5fire_value)

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(
    title = "Fire Emission Upper SEA",
    titleWidth = 257
  ),
  dashboardSidebar(
    width = 257,
    sidebarMenu(
      menuItem("Home: wildlandfire.thairen.net.th", href = "http://wildlandfire.thairen.net.th/index.html", icon=icon("home")),
      menuItem("Overview", tabName = "overview", icon=icon("globe")),
      menuItem("Total Day Comparison", tabName = "total_day_value_compare", icon=icon("chart-line")),
      menuItem("Total Year Comparison", tabName = "total_year_value_compare", icon=icon("chart-line")),
      menuItem("Country Data Table", tabName = "datatable", icon=icon("table")),
      menuItem("Thailand Data Table", tabName = "datatable_thailand", icon=icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            width = 3,
            dateInput("inDate", "Date input:", value = "2020-03-31"), 
            selectInput("inSelect", "Select country:",
                      c("Thailand" = "Thailand",
                        "Cambodia" = "Cambodia",
                        "Laos" = "Laos",
                        "Myanmar" = "Myanmar",
                        "Vietnam" = "Vietnam")
            )
          ),
          box(width = 9, leafletOutput(outputId = "mymap"))
        )#,
        #fluidRow(box(width = 12, leafletOutput(outputId = "mymap")))
      ),
      tabItem(
        tabName = "total_day_value_compare",
        fluidRow(
          box(
            width = 3,
            selectInput("inValue", "Select value:",
              c(
                "FRP" = "total_frp",
                "CO Fire" = "total_cofire",
                "CO2 Fire" = "total_co2fire",
                "PM2.5 Fire" = "total_pm2p5fire"
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12, plotlyOutput("plotly_output")
          )
        )
      ),
      tabItem(
        tabName = "total_year_value_compare",
        fluidRow(
          box(
            width = 3,
            selectInput("inValue_bar", "Select value:",
              c(
                "FRP" = "total_frp",
                "CO Fire" = "total_cofire",
                "CO2 Fire" = "total_co2fire",
                "PM2.5 Fire" = "total_pm2p5fire"
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12, plotlyOutput("fig_2018")
          ),
          box(
            width = 12, plotlyOutput("fig_2019")
          ),
          box(
            width = 12, plotlyOutput("fig_2020")
          ),
          box(
            width = 12, plotlyOutput("fig_2021")
          )
        )
      ),
      tabItem(
        tabName = "datatable",
        fluidRow(
          box(
            width = 3,
            dateInput("inDateDatatable", "Date input:", value = "2020-03-31")
          ),
          box(
            width = 3,
            selectInput("inSelectDatatable", "Select country:",
                c("Thailand" = "Thailand",
                "Cambodia" = "Cambodia",
                "Laos" = "Laos",
                "Myanmar" = "Myanmar",
                "Vietnam" = "Vietnam")
            )
          )
        ),
        fluidRow(box(width = 12, dataTableOutput(outputId = "summary_data_table")))
      ),
      tabItem(
        tabName = "datatable_thailand",
        fluidRow(
          box(
            width = 3,
            dateRangeInput("inDateRange_thailand", "Date range input:",
                          min = as.Date(min(all_df$time)),
                          max = as.Date(max(all_df$time)),
                          start = min(all_df$time),
                          end = max(all_df$time)
            ),
          ),
          box(
            width = 3,
            selectInput("provinceSelect", "Select province:",
                c('Amnat Charoen'='Amnat Charoen','Ang Thong'='Ang Thong','Bangkok Metropolis'='Bangkok Metropolis','Bueng Kan'='Bueng Kan','Buri Ram'='Buri Ram','Chachoengsao'='Chachoengsao','Chai Nat'='Chai Nat','Chaiyaphum'='Chaiyaphum','Chanthaburi'='Chanthaburi','Chiang Mai'='Chiang Mai','Chiang Rai'='Chiang Rai','Chon Buri'='Chon Buri','Chumphon'='Chumphon','Kalasin'='Kalasin','Kamphaeng Phet'='Kamphaeng Phet','Kanchanaburi'='Kanchanaburi','Khon Kaen'='Khon Kaen','Krabi'='Krabi','Lampang'='Lampang','Loei'='Loei','Lop Buri'='Lop Buri','Mae Hong Son'='Mae Hong Son','Maha Sarakham'='Maha Sarakham','Mukdahan'='Mukdahan','Nakhon Nayok'='Nakhon Nayok','Nakhon Pathom'='Nakhon Pathom','Nakhon Phanom'='Nakhon Phanom','Nakhon Ratchasima'='Nakhon Ratchasima','Nakhon Sawan'='Nakhon Sawan','Nakhon Si Thammarat'='Nakhon Si Thammarat','Nan'='Nan','Narathiwat'='Narathiwat','Nong Bua Lam Phu'='Nong Bua Lam Phu','Nong Khai'='Nong Khai','Nonthaburi'='Nonthaburi','Pathum Thani'='Pathum Thani','Pattani'='Pattani','Phangnga'='Phangnga','Phatthalung'='Phatthalung','Phayao'='Phayao','Phetchabun'='Phetchabun','Phetchaburi'='Phetchaburi','Phichit'='Phichit','Phitsanulok'='Phitsanulok','Phra Nakhon Si Ayutthaya'='Phra Nakhon Si Ayutthaya','Phrae'='Phrae','Phuket'='Phuket','Prachin Buri'='Prachin Buri','Prachuap Khiri Khan'='Prachuap Khiri Khan','Ranong'='Ranong','Ratchaburi'='Ratchaburi','Rayong'='Rayong','Roi Et'='Roi Et','Sa Kaeo'='Sa Kaeo','Sakon Nakhon'='Sakon Nakhon','Samut Prakan'='Samut Prakan','Samut Songkhram'='Samut Songkhram','Saraburi'='Saraburi','Satun'='Satun','Si Sa Ket'='Si Sa Ket','Sing Buri'='Sing Buri','Songkhla'='Songkhla','Sukhothai'='Sukhothai','Suphan Buri'='Suphan Buri','Surat Thani'='Surat Thani','Surin'='Surin','Tak'='Tak','Trang'='Trang','Trat'='Trat','Ubon Ratchathani'='Ubon Ratchathani','Udon Thani'='Udon Thani','Uthai Thani'='Uthai Thani','Uttaradit'='Uttaradit','Yala'='Yala','Yasothon'='Yasothon')
            )
          )
        ),
        fluidRow(box(width = 12, dataTableOutput(outputId = "summary_table_thailand_province")))
      )
    )
  )
)

server <- function(input, output) {
  data_input <- reactive({
    all_df %>%
      filter(time == input$inDate)
  })

  data_input_by_country <- reactive({
    data_input()[data_input()$country == input$inSelect,]
  })

  data_input_table1 <- reactive({
    all_df %>%
      filter(time == input$inDateDatatable)
  })
  data_input_table2 <- reactive({
    data_input_table1()[data_input_table1()$country == input$inSelectDatatable,]
  })

  data_thailand_province1 <- reactive({
    df_thailand %>%
      filter(time >= input$inDateRange_thailand[1]) %>%
      filter(time <= input$inDateRange_thailand[2])
  })
  data_thailand_province2 <- reactive({
    data_thailand_province1()[data_thailand_province1()$province == input$provinceSelect,]
  })

  output$plotly_output <- renderPlotly(
    df %>%
    group_by(country) %>%
    plot_ly(
        x = ~time,
        color = ~country,
        colors = c("#032EA1", "#000000", "#43B02A", "#FFCD00", "#C8102E"),
        legendgroup = ~country
    ) %>%
    add_lines(
        y = ~get(input$inValue)
    ) %>% layout(yaxis = list(title = input$inValue), xaxis = list(rangeslider = list(type = "date")))
  )

  output$fig_2018 <- renderPlotly(
    df2018 %>%
    group_by(country) %>%
    plot_ly(x = ~time,
        color = ~country,
        colors = c("#032EA1", "#000000", "#43B02A", "#FFCD00", "#C8102E"),
        legendgroup = ~country
    ) %>%
    add_lines(y = ~get(input$inValue_bar)) %>% layout(yaxis = list(title = "2018"), xaxis = list(type = 'date', tickformat = "%b%d") )
  )
  output$fig_2019 <- renderPlotly(
    df2019 %>%
    group_by(country) %>%
    plot_ly(x = ~time,
        color = ~country,
        colors = c("#032EA1", "#000000", "#43B02A", "#FFCD00", "#C8102E"),
        legendgroup = ~country
    ) %>%
    add_lines(y = ~get(input$inValue_bar)) %>% layout(yaxis = list(title = "2019"), xaxis = list(type = 'date', tickformat = "%b%d") )
  )
  output$fig_2020 <- renderPlotly(
    df2020 %>%
    group_by(country) %>%
    plot_ly(x = ~time,
        color = ~country,
        colors = c("#032EA1", "#000000", "#43B02A", "#FFCD00", "#C8102E"),
        legendgroup = ~country
    ) %>%
    add_lines(y = ~get(input$inValue_bar)) %>% layout(yaxis = list(title = "2020"), xaxis = list(type = 'date', tickformat = "%b%d") )
  )
  output$fig_2021 <- renderPlotly(
    df2021 %>%
    group_by(country) %>%
    plot_ly(x = ~time,
        color = ~country,
        colors = c("#032EA1", "#000000", "#43B02A", "#FFCD00", "#C8102E"),
        legendgroup = ~country
    ) %>%
    add_lines(y = ~get(input$inValue_bar)) %>% layout(yaxis = list(title = "2021"), xaxis = list(type = 'date', tickformat = "%b%d") )
  )
  
  output$mymap <- renderLeaflet(

    data_input_by_country() %>%
    leaflet() %>%
    addTiles() %>%
    #addProviderTiles("CartoDB.DarkMatter") %>% #dark theme map
    setView(101,13,5) %>%
    addPolygons(
        data = thaibound_sf_2,
        weight = 0.5,
        opacity = 0.5,
        fillOpacity = 0.05,
        group = "Thailand Amphoe",
        popup = ~NAME_2
    ) %>%
    addPolygons(
        data = thaibound_sf_1,
        weight = 1,
        opacity = 1,
        fillOpacity = 0.05,
        group = "Thailand Province",
        popup = ~NAME_1
    ) %>%
    addCircleMarkers(
        group = "None",
        lat = 0,
        lng = 0,
        opacity = 0
    ) %>%
    addCircleMarkers(
        group = "FRP",
        lat = ~y,
        lng = ~x,
        radius = 2,
        color = ~pal_frp(frp_value),
        opacity = 1,
        popup = ~paste(paste("Latitude:", y),paste("Longitude:", x), paste("FRP:", frp_value,"(W/m2)"), sep = "<br />")
    ) %>%
    addCircleMarkers(
        group = "CO Fire",
        lat = ~y,
        lng = ~x,
        radius = 2,
        color = ~pal_cofire(cofire_value),
        opacity = 1,
        popup = ~paste(paste("Latitude:", y),paste("Longitude:", x), paste("CO Fire:", cofire_value,"(g/m2s)"), sep = "<br />")
    ) %>%
    addCircleMarkers(
        group = "CO2 Fire",
        lat = ~y,
        lng = ~x,
        radius = 2,
        color = ~pal_co2fire(co2fire_value),
        opacity = 1,
        popup = ~paste(paste("Latitude:", y),paste("Longitude:", x), paste("CO2 Fire:", co2fire_value,"(g/m2s)"), sep = "<br />")
    ) %>%
    addCircleMarkers(
        group = "PM2.5 Fire",
        lat = ~y,
        lng = ~x,
        radius = 2,
        color = ~pal_pm2p5fire(pm2p5fire_value),
        opacity = 1,
        popup = ~paste(paste("Latitude:", y),paste("Longitude:", x), paste("PM2.5 Fire:", pm2p5fire_value,"(g/m2s)"), sep = "<br />")
    ) %>%
    addLayersControl(
        position = "topright",
        overlayGroups = c("Thailand Province","Thailand Amphoe"),
        baseGroups = c("FRP", "CO Fire", "CO2 Fire", "PM2.5 Fire", "None"),
        options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addLegendNumeric(
        title = "FRP Value (W/m2)",
        group = "FRP",
        position = "bottomright",
        values = all_df$frp_value,
        pal = pal_frp,
        decreasing = TRUE
    ) %>%
    addLegendNumeric(
        title = "CO Fire Value (g/m2s)",
        group = "CO Fire",
        position = "bottomright",
        values = all_df$cofire_value,
        pal = pal_cofire,
        decreasing = TRUE
    ) %>%
    addLegendNumeric(
        title = "CO2 Fire Value (g/m2s)",
        group = "CO2 Fire",
        position = "bottomright",
        values = all_df$co2fire_value,
        pal = pal_co2fire,
        decreasing = TRUE
    ) %>%
    addLegendNumeric(
        title = "PM2.5 Fire Value (g/m2s)",
        group = "PM2.5 Fire",
        position = "bottomright",
        values = all_df$pm2p5fire_value,
        pal = pal_pm2p5fire,
        decreasing = TRUE
    )
  )

  #output$summary_table <- renderDataTable(
  #  data_input_by_country()
  #)

  output$summary_data_table <- renderDataTable(
    data_input_table2()
  )

  output$summary_table_thailand_province <- renderDataTable(
    data_thailand_province2()
  )

}

shinyApp(ui = ui, server = server)