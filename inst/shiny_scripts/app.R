library(shiny)
# This example is adapted from
# Grolemund, G. (2015). Learn Shiny - Video Tutorials. URL:https://shiny.rstudio.com/tutorial/

ui <- navbarPage(title = "MyoManager",
                 # Tab 1: Load & Display
                 tabPanel(title = "Home",
                          titlePanel("Welcome to MyoManager!"),
                          helpText("This is the page where you can upload and preview your
                                    microscopy image."),

                          sidebarLayout(
                            sidebarPanel(
                              tags$p("Note: valid input formats include tif, tiff, jpg, and png"),
                              fileInput(inputId = "localInput",
                                        label = "Choose an image or image folder from local directory"),
                              textInput(inputId = "htmlInput",
                                        label = "Alternatively, choose an image from url"),
                              helpText("(please put url in quotations)"),
                              selectInput("image", "Sample image:", list.files(system.file("images", package="EBImage"))),
                              tags$strong("Please select a display option"),
                              actionButton(inputId = "color",
                                           label = "Color"),
                              actionButton(inputId = "grayscale",
                                           label = "Grayscale")
                            ),
                           # Show a display of the generated distribution
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Static raster", plotOutput("raster")),
                                tabPanel("Interactive browser", displayOutput("widget"))
                              )
                            )
                          ),
                 ),
                 # Tab 2: Image Processor
                 navbarMenu(title = "Image Processor",
                            tabPanel(title = "Select frame"
                            ),
                            tabPanel(title = "Adjust intensity"
                            ),
                            tabPanel(title = "Apply blurring filter"
                            ),
                            tabPanel(title = "View segmented objects"
                            ),
                 ),
                 # Tab 3: Nuclei Counter
                 tabPanel(title = "Nuclei Counter"
                 ),
                 # Tab 4: Nuclei Features
                 navbarMenu(title = "Nuclei Features",
                            tabPanel(title = "Single view",
                                     plotOutput("unif"),
                                     actionButton("reunif", "Resample")
                            ),
                            tabPanel(title = "Matrix View",
                                     plotOutput("chisq"),
                                     actionButton("rechisq", "Resample")
                            )
                 )
)

server <- function(input, output) {
  inFile <- reactive({
    f = system.file("images", input$image, package="EBImage")
    MyoManager::loadImage(f)
  })

  rv <- reactiveValues(
    # display options for Tab 1
    colorMode = NULL,
    # plotting options for Tab 3
    unif = runif(500),
    chisq = rchisq(500, 2))

  # display option buttons
  observeEvent(input$color, {
    rv$colorMode <- 2})
  observeEvent(input$grayscale, {
    rv$colorMode <- 0})

  # render display output
  output$raster <- renderPlot({
    if(is.null(rv$colorMode)){
      return()
    }else{
      plot(EBImage::`colorMode<-`(inFile(), rv$colorMode), all=TRUE)
    }
  })
  output$widget <- renderDisplay({
    if(is.null(rv$colorMode)){
      return()
    }else{
      MyoManager::viewImage(inFile(), rv$colorMode)
    }
  })

  observeEvent(input$reunif, { rv$unif <- runif(500) })
  observeEvent(input$rechisq, { rv$chisq <- rchisq(500, 2) })

  # output$originalInput <- renderPlot({
  #
  #   local <- input$localInput
  #   html <- input$htmlInput
  #
  #   if (!is.null(local) && !is.null(html)) {
  #     print("please only select one input option")
  #   } else if (!is.null(local)) {
  #     inFile <- MyoManager::loadImage(local)
  #   } else if (!is.null(html)){
  #     inFile <- MyoManager::loadImage(html)
  #   }
  #
  #   if (is.null(rv$colorMode)){
  #     return()
  #   }else{
  #     #plot(inFile)
  #     hist(1)
  #     # return(MyoManager::viewImage(inFile, rv$colorMode))
  #   }
  # })

  output$unif <- renderPlot({
    hist(rv$unif, breaks = 30, col = "grey", border = "white",
         main = "500 random draws from a standard uniform distribution")
  })
  output$chisq <- renderPlot({
    hist(rv$chisq, breaks = 30, col = "grey", border = "white",
         main = "500 random draws from a Chi Square distribution with two degree of freedom")
  })
}

shinyApp(server = server, ui = ui)
#[End]
