library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)
library(stringr)
library(tools)

#-----------------------------
# 📁 default 경로
#-----------------------------

DEFAULT_IMG  <- "iOrganoAssay/microscopy"
DEFAULT_SEG  <- "iOrganoAssay/segmentation"
DEFAULT_MET  <- "iOrganoAssay/metrics"


#-----------------------------
# metric 읽기
#-----------------------------
read_metric_vector <- function(path, metric){
  
  if(!file.exists(path)) return(NULL)
  
  sheet <- switch(metric,
                  "area" = "Outline Area (µm²)",
                  "perimeter" = "Perimeter (µm)",
                  "circularity" = "Circularity")
  
  df <- read_excel(path, sheet = sheet)
  
  if(!("Frame 0" %in% colnames(df))) return(NULL)
  
  df[["Frame 0"]]
}

#-----------------------------
# UI
#-----------------------------
ui <- fluidPage(
  
  titlePanel("iOrganoAssay | Analysis"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("1. Metafile"),
      fileInput("meta", "Upload metafile"),
      
      hr(),
      
      h4("2. Folder setting"),
      textInput("img_dir", "IMG_DIR", DEFAULT_IMG),
      textInput("seg_dir", "SEG_DIR", DEFAULT_SEG),
      textInput("met_dir", "MET_DIR", DEFAULT_MET),
      
      hr(),
      
      h4("3. Category condition"),
      selectInput("mice", "Mice", choices = NULL),
      selectInput("passage", "Passage", choices = NULL),
      
      selectInput("well", "Microwell",
                  choices = NULL,
                  multiple = TRUE),
      selectInput("day", "Day", choices = NULL),
      
      actionButton("apply", "Apply"),
      
      hr(),
      
      h4("4. Metric"),
      selectInput("metric", "Metric",
                  choices = c("area","perimeter","circularity")),
      
      hr(),
      
      h4("5. Plot control"),
      sliderInput("pt_size", "Point size", 1, 10, 3),
      sliderInput("line_size", "Line size", 0.5, 5, 1),
      sliderInput("axis_size", "Axis text size", 8, 20, 12),
      sliderInput("title_size", "Title size", 10, 25, 14),
      sliderInput("violin_pt", "Violin dot size", 0.5, 5, 1)
    ),
    
    mainPanel(
      
      h4("Microscopy Images"),
      uiOutput("images"),
      
      hr(),
      
      h4("Segmentation Images"),
      uiOutput("seg_images"),
      
      hr(),
      
      h4("Mean + Std Plot"),
      plotOutput("meanPlot", height = "300px"),
      downloadButton('saveMS','Save image'),
      
      
      hr(),
      
      h4("Violin Plot"),
      plotOutput("violinPlot", height = "400px"),
      downloadButton('saveV','Save image'),
      
    )
  )
)

#-----------------------------
# SERVER
#-----------------------------
server <- function(input, output, session){
  
  # resource path
  observe({
    addResourcePath("img", input$img_dir)
    addResourcePath("seg", input$seg_dir)
  })
  
  # metadata
  meta <- reactive({
    req(input$meta)
    read_excel(input$meta$datapath)
  })
  
  
  # dropdown
  observe({
    df <- meta()
    
    updateSelectInput(session, "mice",
                      choices = c("All", unique(df$mice)))
    
    updateSelectInput(session, "passage",
                      choices = c("All", unique(df$passage)))
    
    updateSelectInput(session, "day",
                      choices = c("All", unique(df$day)))
    
    updateSelectInput(session, "well",
                      choices = unique(df$microwell))
  })
  
  # filtering
  filtered <- eventReactive(input$apply, {
    
    df <- meta()
    
    if(input$mice != "All") df <- df %>% filter(mice == input$mice)
    if(input$passage != "All") df <- df %>% filter(passage == input$passage)
    if(input$day != "All") df <- df %>% filter(day == input$day)
    
    # 🔥 multi well
    
    if(length(input$well) > 0){
      df <- df %>% filter(microwell %in% input$well)
    }
    
    df %>% arrange(day)
  })
  
  #-----------------------------
  # 🔬 RAW 이미지 (Grid)
  #-----------------------------
  output$images <- renderUI({
    
    df <- filtered()
    req(df)
    
    # 🔥 핵심: 알파벳 정렬
    files <- sort(df$filename)
    
    tags$div(
      style = "display:flex; flex-wrap:wrap; gap:15px;",
      
      #  lapply(df$filename, function(f){
      
      lapply(files, function(f){
        
        tags$div(
          style = "text-align:center;",
          
          tags$p(f, style="font-size:10px;"),
          
          tags$img(
            src = file.path("img", f),
            style = "width:120px;"
          )
        )
      })
    )
  })
  
  #-----------------------------
  # 🧬 SEG 이미지 (Grid)
  #-----------------------------
  output$seg_images <- renderUI({
    
    df <- filtered()
    req(df)
    
    # 🔥 핵심: 알파벳 정렬
    files <- sort(df$filename)
    
    tags$div(
      style = "display:flex; flex-wrap:wrap; gap:15px;",
      
      # lapply(df$filename, function(f){
      
      lapply(files, function(f){        
        
        tags$div(
          style = "text-align:center;",
          
          tags$p(f, style="font-size:10px;"),
          
          tags$img(
            src = file.path("seg", f),
            style = "width:120px;"
          )
        )
      })
    )
  })
  
  
  #-----------------------------
  # Mean plot
  #-----------------------------
  meanPlotReactive <- reactive({
    
    df <- filtered()
    req(df)
    
    df$day_num <- as.numeric(str_extract(df$day,"\\d+"))
    
    df$value <- sapply(df$filename, function(f){
      
      path <- file.path(input$met_dir,
                        paste0(file_path_sans_ext(f),"_metrics.xlsx"))
      
      v <- read_metric_vector(path, input$metric)
      if(is.null(v)) return(NA)
      
      mean(v, na.rm=TRUE)
    })
    
    df$sd <- sapply(df$filename, function(f){
      
      path <- file.path(input$met_dir,
                        paste0(file_path_sans_ext(f),"_metrics.xlsx"))
      
      v <- read_metric_vector(path, input$metric)
      if(is.null(v)) return(NA)
      
      sd(v, na.rm=TRUE)
    })
    
    ggplot(df, aes(x=day_num, y=value,
                   color=microwell, group=microwell)) +
      
      geom_point(size=input$pt_size) +
      geom_line(linewidth=input$line_size) +
      
      geom_errorbar(aes(ymin=value-sd,
                        ymax=value+sd),
                    width=0.2) +
      
      scale_x_continuous(breaks=df$day_num,
                         labels=df$day) +
      
      theme_minimal() +
      theme(
        axis.text = element_text(size=input$axis_size),
        axis.title = element_text(size=input$title_size)
      )
  })
  
  output$meanPlot <- renderPlot({
    meanPlotReactive()
  })
  
  output$saveMS <- downloadHandler(
    filename = function(){ "meanplot.png" },
    content = function(file){
      ggsave(file, plot = meanPlotReactive(), width = 10, height = 8, units = "in", dpi = 300)
    }
  )
  
  #-----------------------------
  # 🔥 Violin plot (핵심 수정)
  #-----------------------------
  violinPlotReactive <- reactive({
    
    df <- filtered()
    req(nrow(df) > 0)
    
    all_data <- data.frame()
    
    for(i in 1:nrow(df)){
      
      f <- df$filename[i]
      
      path <- file.path(input$met_dir,
                        paste0(file_path_sans_ext(f),"_metrics.xlsx"))
      
      v <- read_metric_vector(path, input$metric)
      
      if(!is.null(v)){
        
        tmp <- data.frame(
          value=v,
          day=df$day[i],
          microwell=df$microwell[i]
        )
        
        all_data <- rbind(all_data,tmp)
      }
    }
    
    ggplot(all_data,
           aes(x=day, y=value, fill=microwell)) +
      geom_violin(position=position_dodge(0.8),
                  alpha=0.5) +
      
      geom_jitter(aes(color=microwell),
                  position=position_jitterdodge(
                    jitter.width=0.1,
                    dodge.width=0.8
                  ),
                  size=input$violin_pt,
                  alpha=0.4) +
      
      theme_minimal()
  })
  
  output$violinPlot <- renderPlot({
    violinPlotReactive()
  })
  
  output$saveV <- downloadHandler(
    filename = function(){ "violinplot.png" },
    content = function(file){
      ggsave(file, plot = violinPlotReactive(), width = 10, height = 8, units = "in", dpi = 300)
    }
  )
}


shinyApp(ui, server)
