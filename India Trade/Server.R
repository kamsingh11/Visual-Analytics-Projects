# Load the required packages
library(shiny)
library(shinydashboard)
library(plotly)
library(datasets)
library(DT)

shinyServer(function(input, output, session){
  
 #dataset 
  dat <- reactive({
    switch(input$dataset,
           expo = export_new,
           impo = import_new
    )
  
  })
  
  
  output$table <- DT::renderDataTable({
    dat()
    
  })
  
  output$summary <- renderPrint({
    summary(dat())
  })

  
  output$comm_comp <- renderPlotly({
    com <- export_import_comm %>%
      ggplot(aes(x = reorder(HSCode, Total), y = Total, fill = Trade)) +
      geom_col(position = 'dodge') +
      #scale_fill_manual(values = c("#E69F00", "#FF6666")) +
      scale_fill_brewer(palette="Set1") +
      labs(main = "Export/Import for top 10 countries by Year", x ="Year", y ="Value") +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-03, prefix = "$")) +
      theme_classic() +
      labs(x = "Commodity") +
      scale_x_discrete(labels=c("52" = "cotton", "62" = "clothing","90" = "Optical","15"="Oils","39"="Plastic",
                                "72" = "Iron","85" = "Electrical", "30" = "Medicines",
                                "29" = "Chemicals","84" = "Nuclear", "87" = "Vehicles",
                                "71" = "Metal/Stone","27" = "Minerals"))
    
    
    
    ggplotly(com)
    
  })
  
   output$coun_comp <- renderPlotly({
    com <- export_import_coun %>%
      ggplot(aes(x = reorder(country, Total), y = Total, fill = Trade)) +
      geom_col(position = 'dodge') +
      #scale_fill_manual(values = c("#E69F00", "#FF6666")) +
      scale_fill_brewer(palette="Paired") +
      labs(main = "Export/Import for top 10 countries by Year", x ="Year", y ="Value") + 
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-03, prefix = "$")) +
      theme_classic() +
      labs(x = "Country")
    
    
    
    ggplotly(com)
    
  })
  
  # For Country Plot
  output$bar_coun <- renderPlotly({
    coun <- plot_coun_dat() %>%
    ggplot(aes(x = reorder(country, Total), y = Total)) +
      geom_col(fill="#E69F00") +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-03, prefix = "$")) +
      coord_flip() +
      scale_fill_brewer(palette="Reds") +
      theme_classic() +
      labs(x = "")
  })

  
     output$line_year <- renderPlotly({
    y <- ggplot(export_import, aes(x = year, y = Total, color = Trade)) +
      geom_line() +
      geom_point() +
      labs(main = "Export/Import for top 10 countries by Year", x ="Year", y ="Value") + 
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-03, prefix = "$")) +
      theme_classic()
    
    ggplotly(y)
    
    
  })
  ## for display of first few observations of mtcars dataset
  output$data <- renderTable({
    head(mtcars)
  })
  
  output$deficit <- renderPlotly(
    {
      import$value[is.na(import$value)] = 0.0
      export$value[is.na(export$value)] = 0.0
      exp <- export %>% 
        select(value, year) %>% 
        group_by(year) %>% 
        summarise(export = sum(value))
      
      imp <- import %>% 
        select(value, year) %>% 
        group_by(year) %>% 
        summarise(import = sum(value))
      
      imp_exp <- full_join(imp,exp, by = "year")
      imp_exp$deficit <- (exp$export - imp$import)
      imp_exp$year<- as.factor(imp_exp$year)
      imp_exp <- as.data.frame(imp_exp) 
      class(imp_exp) 
      imp_exp.m <- melt(imp_exp)
      
      m1 <- ggplot(imp_exp.m, aes(year, value, fill = variable)) 
      m1 <- m1 + geom_bar(stat = "identity", position = "dodge") + 
        scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-03, prefix = "$")) +
        #scale_y_log10()+
        scale_fill_brewer(palette="Reds") +
        ggtitle("The Export,Import and Deficit Values over the years")
      
      ggplotly(m1)
    }
  )
  
  # ## Plotly Scatter Plot
  # output$plot1 <- renderPlotly({
  #   plot_ly(data=mtcars, 
  #           x=~wt, 
  #           y=~mpg,
  #           type = "scatter",
  #           mode = "markers")
  #   
  # })
  
#commodity
  plot_comm_dat <- reactive({
    switch(input$plot_comm,
           expo_comm = export_highest_commodity,
           impo_comm = import_highest_commodity
    )
  })
  
  # For Commodity Plot
  output$bar_comm <- renderPlotly({
    com <- plot_comm_dat() %>%
      #rename(Commodity = stringr::str_wrap(Commodity, 15)) %>%
      ggplot(aes(x = reorder(HSCode, Total), y = Total)) +
      geom_col(fill="#56B4E9") +
      coord_flip() +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-03, prefix = "$")) +
      scale_fill_brewer(palette="Dark2") +
      theme_classic() +
      labs(x = " ")+
      scale_x_discrete(labels=c("32" = "Fertilizers", "52" = "cotton", "62" = "clothing",
                                    "72" = "Iron","85" = "Electrical", "30" = "Medicines",
                                "29" = "Chemicals","84" = "Nuclear", "87" = "Vehicles",
                                "71" = "Metal/Stone","27" = "Minerals", "90" = "Optical", "15" = "Oils", "39" = "Plastic"))
    
    ggplotly(com)
    
  })
  
  # For Country Plot
  plot_coun_dat <- reactive({
    switch(input$plot_coun,
           expo_coun = export_highest_country,
           impo_coun = import_highest_country
    )
  })

  # For Region
  plot_reg_dat <- reactive({
    switch(input$plot_reg,
           expo_reg = df_export,
           impo_reg = df_import
    )
  })
  
output$tree_reg <- renderPlot({
plot_reg_dat() %>% head(input$Topchoose) %>%
      ggplot(aes(area = `sum(value)`, fill=`sum(value)`, label = country))+
      geom_treemap()+
      geom_treemap_text()+
      coord_flip() 
      #ggtitle("Top Countries in import section")
    
  })

# For Map
plot_map_dat <- reactive({
  switch(input$plot_map,
         expo_map = map_data_export,
         impo_map = map_data_import
  )
})


output$map_reg <- renderPlotly({
  #map_data_import 
  l <- list(color = toRGB("grey"), width = 0.5)
  g <- list(
    showframe = FALSE,
    showcoastlines = FALSE,
    projection = list(type = 'Mercator')
  )
  p <- plot_geo(plot_map_dat()) %>%
    add_trace(
      z = ~total_value, color = ~total_value, colors = 'Reds',
      text = ~country, locations = ~Code, marker = list(line = l)
    ) %>%
    colorbar(title = 'Value in Million US$', tickprefix = '$',ticksufix= 'M') %>%
    layout(
      title = 'Imports/Exports of Top 50 Countries',
      geo = g
    )
  
  ggplotly(p)
})

impo <- reactive(
  {
    topimport = import %>%
      filter(HSCode %in% (import %>% count(HSCode) %>% 
                            arrange(-n) %>% 
                            head(input$Topchoose1) %>% 
                            pull(HSCode)))
    topimport1 = with(topimport,table(year,HSCode))
    topimport1 = as.data.frame(topimport1)
  }
)

expo <- reactive(
  {
    topexport = export %>%
      filter(HSCode %in% (export %>% count(HSCode) %>% 
                            arrange(-n) %>% 
                            head(input$Topchoose1) %>% 
                            pull(HSCode)))
    topexport1 = with(topexport,table(year,HSCode))
    topexport1 = as.data.frame(topexport1)
  }
)

# For HS Code
plot_HS_dat <- reactive({
  switch(input$plot_HS,
         expo_HS = expo(),
         impo_HS = impo()
  )
})

output$bar_HS <- renderPlotly(
  {
    plot_HS_dat()%>%
    #head(input$Topchoose1) %>% 
    ggplot(aes(HSCode, Freq, fill = year)) +     
      geom_col(position = 'dodge')+
      xlab("Commoditiy HSCode")+
      ylab("Count")+
      scale_fill_brewer(palette="Spectral") 
      #ggtitle("Top Commodities in export section")
  }
)

# For Bubble
plot_bub_dat <- reactive({
  switch(input$plot_bub,
         expo_bub = topcountryexp,
         impo_bub = topcountryimp
  )
})

output$bub_coun <- renderPlotly(
  {
    plot_bub_dat()%>%
      head(input$Topchoose1) %>%
      ggplot(aes(reorder(country,`sum(value)`),`sum(value)`))+
      geom_point(alpha = 0.75, aes(size = `sum(value)`, color = country))+
      coord_flip() +
      #ggtitle("Top Countries in export section") +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-03, prefix = "$")) +
      labs(y = "value", x = "Country")
    
  }
)



})
