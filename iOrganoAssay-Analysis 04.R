library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(purrr)

if(interactive()){
  ui = fluidPage(
    titlePanel("iOrganoAssay | Analysis"),
    hr(),
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Choose CSV file", accept=".csv"),
        checkboxInput("header", "Header", TRUE),
        actionButton("addRow", "Add Row"), actionButton("delRow", "Delete Row"),
        width = 3
      ),
      layout_columns(
        card(DTOutput("contents")),
        card(DTOutput('x9')),
        col_widths = c(4, 4)
        #DTOutput("inputDF"),
      ),
    ), hr(),
    sidebarLayout(
      sidebarPanel(
        width = 3
      ),
      mainPanel(
        verbatimTextOutput("grpNameVal"),
        textOutput("selDaysVal"), textOutput("selAreaVal"),
      ),
    ), hr(),
    sidebarLayout(
      sidebarPanel(
        selectizeInput("grpList", "Select group list", list("p1","p2","p3","p4","p5","p6","p7","p8","p9","p10"), multiple = TRUE, options = list('plugins' = list('remove_button'), 'create' = TRUE, 'persist' = FALSE)), textOutput("selGrpVal"),
        width = 3
      ),
      
      mainPanel(
        # verbatimTextOutput('x9'),
        plotOutput("x9_plot"),
        ),
    )
  )  
    
  server = function(input, output, session){
    myData = reactive({
      file = input$file1
      ext =  tools::file_ext(file$datapath)
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      read.csv(file$datapath, header = input$header)
    })
    

    df = eventReactive(input$goButton, {contents}, ignoreNULL = FALSE)
    
    output$contents = renderDT({
      req(myData())
      datatable(myData(), selection = list(mode="single", target="column"),
                list(initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().container()).css({'font-size': '12px'});",
                  "}"
                ))
      )
    })
    
    observe({
      req(myData())
      cols = colnames(myData())
      updateSelectInput(session, "selArea", choices = cols)
    })

    d9 = reactiveVal(data.frame(passage = numeric(),days = numeric(), area = numeric(), circularity = numeric(), perimeter = numeric(), stringsAsFactors = FALSE))
  
    observe({
      if (nrow(d9()) == 0) {
        d9(rbind(d9(), data.frame(passage = 0, days = 0, area = 0, circularity = 0, perimeter = 0)))
      }
    })
    
    observeEvent(input$addRow, {
      d9(rbind(d9(), data.frame(passage = 0, days = 0, area = 0, circularity = 0, perimeter = 0)))
    })
    
    output$x9 = renderDT({
      datatable(d9(), colnames = c(ID = 1),extensions = c('Select','Buttons', 'RowReorder'), options = list(select = list(style = 'os', items = 'rows'), dom = 'Birtip', buttons = c('selectNone','selectRows', 'selectColumns' ,'copy', 'csv', 'excel'), rowReorder = T, order = list(c(0, 'asc'))), selection = 'none', editable = 'cell', rownames = T)
    }, server = T) 
    
    observeEvent(input$x9_cell_edit, {
      d9(editData(d9(), input$x9_cell_edit, rownames = T))
    })
    
    # renderPrint({
    #   aVal = input$x9_rows_selected
    #   if (length(aVal)) {
    #     cat('Avarage value:\n\n')
    #     cat(aVal, sep = ', ')
    #   }
    # })

    observeEvent(input$delRow, {
      selected = input$x9_rows_selected
      if (!is.null(selected) && length(selected) > 0) {
        d9(d9()[-selected, ])
      }
    })

    output$x9_plot = renderPlot({
       ggplot(data = d9(), aes(x = days, y = area, group = passage, color = as.factor(passage))) + geom_point(size = 4) + labs(title = "Index vs Area", x = "days", y = "Area") + geom_line(linewidth = 1) + scale_color_brewer(palette = "Dark2", name = "Passage")
    })
    # output$x9_plot = renderPlot({
    #   ggplot(data = d9(), aes(x = days, y = area, colour = passage)) + geom_point(size = 4) + labs(title = "Index vs Area", x = "days", y = "Area") + geom_line(linewidth = 1)
    # })    
  }
  
  shinyApp(ui, server)
}
  
