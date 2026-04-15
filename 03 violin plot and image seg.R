library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)
library(stringr)
library(tools)

#-----------------------------
# 📁 기본 경로 (default 유지)
#-----------------------------
DEFAULT_IMG <- "C:/iOrganoAssay/microscopy"
DEFAULT_SEG <- "C:/iOrganoAssay/segmentation"
DEFAULT_MET <- "C:/iOrganoAssay/metrics"

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
      selectInput("day", "Day", choices = NULL),
      
      # 🔥 multi-select
      selectInput("well", "Microwell",
                  choices = NULL,
                  multiple = TRUE),
      
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
      
      hr(),
      
      h4("Violin Plot"),
      plotOutput("violinPlot", height = "400px")
    )
  )
)

#-----------------------------
# SERVER
#-----------------------------
server <- function(input, output, session){
  
  # dynamic resource path
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
  # 이미지 grid
  #-----------------------------
  output$images <- renderUI({
    
    df <- filtered()
    
    tags$div(
      style="display:flex; flex-wrap:wrap; gap:10px;",
      
      lapply(df$filename, function(f){
        tags$img(src=file.path("img",f),
                 style="width:120px;")
      })
    )
  })
  
  output$seg_images <- renderUI({
    
    df <- filtered()
    
    tags$div(
      style="display:flex; flex-wrap:wrap; gap:10px;",
      
      lapply(df$filename, function(f){
        tags$img(src=file.path("seg",f),
                 style="width:120px;")
      })
    )
  })
  
  #-----------------------------
  # Mean + Std plot
  #-----------------------------
  output$meanPlot <- renderPlot({
    
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
      
      # 🔥 std error bar
      geom_errorbar(aes(ymin=value-sd,
                        ymax=value+sd),
                    width=0.2) +
      
      scale_x_continuous(breaks=df$day_num,
                         labels=df$day) +
      
      labs(x="Day", y="Average ± SD") +
      
      theme_minimal() +
      theme(
        axis.text = element_text(size=input$axis_size),
        axis.title = element_text(size=input$title_size)
      )
  })
  
  #-----------------------------
  # Violin plot
  #-----------------------------
  output$violinPlot <- renderPlot({
    
    df <- filtered()
    
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
    
    ggplot(all_data, aes(x=day, y=value,
                         fill=microwell)) +
      
      geom_violin(trim=FALSE, alpha=0.5) +
      
      geom_jitter(width=0.1,
                  size=input$violin_pt,
                  alpha=0.4) +
      
      theme_minimal() +
      theme(
        axis.text = element_text(size=input$axis_size),
        axis.title = element_text(size=input$title_size)
      )
  })
}

shinyApp(ui, server)
