library(data.table)
library(shiny)


# Functions/data for app -------------------------------------------------------
format_power <- function(w, kg=NULL, prefix=""){
  if(is.null(kg)){
    output <- sprintf("%s %.0f W", prefix, w)
  }
  
  if(!is.null(kg)){
    output <- sprintf("%s %.0f W (%.2f W/kg)", prefix, w, w/kg)
  }
  
  output
}


cat_boundaries <- data.table(
  cat=rep(LETTERS[1:4], times=2),
  open=rep(c(TRUE, FALSE), each=4),
  zmap=c(5.4, 4.2, 3.3, 0.0, 5.0, 4.2, 3.5, 0.0),
  zftp_kg=c(4.200, 3.360, 2.625, 0.000, 3.880, 3.360, 2.625, 0.000),
  zftp=c(250,200,150,0,0,0,0,0),
  zmap_pretty=c("≥5.4W/kg", "≥4.2W/kg", "≥3.3W/kg", "<3.3W/kg", 
                "≥5.0W/kg", "≥4.2W/kg", "≥3.5W/kg", "<3.5W/kg"),
  zftp_pretty=c("≥4.200W/kg and ≥250W", "≥3.360W/kg and ≥200W",
                "≥2.625W/kg and ≥150W", "<2.625W/kg or <150W",
                "≥3.880W/kg", "≥3.360W/kg", "≥2.625W/kg", "<2.625W/kg")
)





ui <- fluidPage(
  includeHTML("www/page_header.html"),
  includeHTML("www/page_top.html"),
  includeCSS("www/style.css"),
  
  # Get power and weight values 
  # - used tags over numericInput() to skip append of placeholder
  tags$input(name="zFTP", placeholder="zFTP (W)",    type="number", value=NA, min=1),
  tags$input(name="zMAP", placeholder="zMAP (W)",    type="number", value=NA, min=1),
  tags$input(name="kg",  placeholder="Weight (kg)", type="number", value=NA, min=1),
  selectInput("open", label = h3("Race Type:"), 
              choices = list("Open/Mixed" = TRUE, "Womens" = FALSE), 
              selected = 1),  
  tags$h5(textOutput("stats_header")),
  textOutput("race_cat"),
  textOutput("zftp_formatted"),
  textOutput("zmap_formatted"),
  tags$h5("Racing Category Boundaries:"),
  tableOutput("cat_table")
)

server <- function(input, output) {
  
  output$stats_header <- renderText({
    req(input$zFTP)
    req(input$zMAP)
    req(input$kg)
    "Your Stats:"
  })
  
  output$race_cat <- renderText({
    req(input$zFTP)
    req(input$zMAP)
    req(input$kg)
    cat_boundaries[open==as.logical(input$open) &
                     ( (input$zMAP/input$kg)>=zmap |
                         ((input$zFTP/input$kg)>=zftp_kg & input$zFTP>=zftp) ),
                   sprintf("Racing Category: %s", min(cat))]
  })
  
  output$zftp_formatted <- renderText({
    req(input$zFTP)
    req(input$zMAP)
    req(input$kg)
    format_power(input$zFTP, input$kg, prefix="zFTP:")
  })
  
  output$zmap_formatted <- renderText({
    req(input$zFTP)
    req(input$zMAP)
    req(input$kg)
    format_power(input$zMAP, input$kg, prefix="zMAP:")
  })
  
  output$cat_table <- renderTable({
    cat_boundaries[open==as.logical(input$open), .("Cat."=cat, 
                                                   "zFTP"=zftp_pretty, 
                                                   "zMAP"=zmap_pretty)]
  })
}

shinyApp(ui=ui, server=server)