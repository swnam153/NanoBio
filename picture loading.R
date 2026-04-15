library(shiny)
library(bslib)
library(DT)
library(ggplot2)

options(shiny.maxRequestSize = 100 * 1024^2)

if(interactive()){
  ui = fluidPage(
    titlePanel("iOrganoAssay | Analysis"),
     hr(),


    sidebarLayout(
         sidebarPanel(
              fileInput("tiffFile", "Choose TIFF file", accept = c(".tif", ".tiff")),
              width = 5
              ),
         mainPanel(
              imageOutput("img"),
              width = 5
              ),
         ),

  )  


server = function(input, output, session){
      output$img = renderImage({
      req(input$tiffFile)
 
       file = input$tiffFile

      list(
      src = file$datapath,
      contentType = "image/tiff",
      width = "100%"
    )

    }, deleteFile = FALSE)

}



    shinyApp(ui, server)
}




 
