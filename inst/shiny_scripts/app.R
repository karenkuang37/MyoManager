library(shiny)
# This example is adapted from
# Grolemund, G. (2015). Learn Shiny - Video Tutorials. URL:https://shiny.rstudio.com/tutorial/

ui <- navbarPage(title = "MyoManager",
                 # Tab 1: Load & Display
                 tabPanel(title = "Home",
                          titlePanel("Welcome to MyoManager!"),
                          tags$p("This is the page where you can upload and preview your
                                    microscopy image."),
                          sidebarLayout(
                            sidebarPanel(
                              tags$p("Note: valid input formats include tif, tiff, jpg, and png"),
                              # fileInput(inputId = "local",
                              #           label = "Choose an image or image folder from local directory"),
                              textInput(inputId = "html",
                                        label = "Please enter a valid image file path or url:"),
                              helpText("(Try a sample link: https://user-images.githubusercontent.com/60583839/141215629-f19d4a77-c5f0-491f-9262-b22cd59739e3.jpg)"),
                              selectInput("sample", "Sample images:", list.files(system.file('extdata', package = 'MyoManager'))),
                              tags$strong("Please select display option:"),
                              actionButton(inputId = "color",
                                           label = "Color"),
                              actionButton(inputId = "grayscale",
                                           label = "Grayscale")
                            ),
                           # Show a display of the generated distribution
                            mainPanel(
                              helpText("Display may take a few seconds to render"),
                              tabsetPanel(
                                tabPanel("Static raster", plotOutput("raster_1")),
                                tabPanel("Interactive browser", displayOutput("widget_1"))
                              )
                            )
                          )
                 ),
                 # Tab 2: Image Processor
                 navbarMenu(title = "Image Processor",
                            # Tab 2.1: Adjust Intensity
                            tabPanel(title = "Adjust intensity",
                                     titlePanel("Image Intensity Control"),
                                     tags$p("On this page you can modify the brightess and contrast
                                              level of the image for better viewing quality."),
                                     helpText("(Hint: decreased brightness + increased contrast
                                              is good for object identification)"),
                            sidebarLayout(sidebarPanel(
                                # for picking a frame
                                numericInput(inputId = "fNum",
                                             label = "Please specify a frame",
                                             helpText("(see Home tab for the frame number of nuclei/cell body)"),
                                             value = 3),
                                 # for picking brightness
                                 numericInput(inputId = "brightness",
                                             label = "Please specify the brightness",
                                             helpText("(e.g. -0.2)"),
                                             value = -0.2),
                                 # for picking contrast
                                 numericInput(inputId = "contrast",
                                              label = "Please specify the contrast level",
                                              helpText("(e.g. 3)"),
                                              value = 3)
                              ),
                              mainPanel(
                                helpText("Display may take a few seconds to render"),
                                tabsetPanel(
                                  tabPanel("Interactive browser", displayOutput("widget_2.1"))
                                 )
                               ))
                            ),
                            # Tab 2.2: Blurring Filter
                            tabPanel(title = "Blurring Filter",
                                     titlePanel("Apply a blurring filter"),
                                     tags$p("On this page you can apply a blurring filter on
                                              selected image frame,"),
                                     helpText("Blurring tends to be useful prior to analyzing
                                              individual objects in an image with ill-defined
                                              edges and/or uneven intensities."),
                            sidebarLayout(sidebarPanel(
                                # for picking a frame
                                numericInput(inputId = "fNum",
                                             label = "Please specify a frame",
                                             helpText("(see Home tab for the frame number of nuclei/cell body)"),
                                             value = 3),
                                # for picking brush size
                                numericInput(inputId = "bSize",
                                             label = "Brush size:",
                                             helpText("(e.g.11)"),
                                             value = 11),
                                # for picking brush shape
                                selectInput(inputId = "bShape",
                                            label = "Brush shape:",
                                            choices = list('line','box','disc','diamond','Gaussian'))
                              ),
                              mainPanel(
                                helpText("Display may take a few seconds to render"),
                                tabsetPanel(
                                  tabPanel("Interactive browser", displayOutput("widget_2.2"))
                                )
                              ))
                            ),
                            # Tab 2.3: Object Segmentation
                            tabPanel(title = "Object Segmentation"
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
  # Tab 1: Load & Display
  # Set up reactive image file
  # inFile_local <- reactive({
  #   if(!is.null(input$local)){
  #     MyoManager::readImage(input$local)
  #   }else{
  #     NULL
  #   }
  # })
  inFile_sample <- reactive({
    f = system.file("extdata", input$sample, package="MyoManager")
    MyoManager::loadImage(f)
  })
  inFile_html <- reactive({
    if(!is.null(input$html)){
      MyoManager::loadImage(input$html)
    }else{
      NULL
    }
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
  output$raster_1 <- renderPlot({
    if(is.null(rv$colorMode)){
      return()
    }else{
      plot(EBImage::`colorMode<-`(inFile_sample(), rv$colorMode), all=TRUE)

      # if(!is.null(isolate(inFile_html()))){
      #   plot(EBImage::`colorMode<-`(inFile_html(), rv$colorMode), all=TRUE)
      # } else {
      # plot(EBImage::`colorMode<-`(inFile_sample(), rv$colorMode), all=TRUE)
      # }

    }
  })

  output$widget_1 <- renderDisplay({
    if(is.null(rv$colorMode)){
      return()
    }else{
      MyoManager::viewImage(inFile_sample(), rv$colorMode)

      # if(!is.null(isolate(inFile_html()))){
      #   MyoManager::viewImage(inFile_html(), rv$colorMode)
      # } else {
      #   MyoManager::viewImage(inFile_sample(), rv$colorMode)
      # }

    }
  })


 ######################################## Temporary filler plots below
  observeEvent(input$reunif, { rv$unif <- runif(500) })
  observeEvent(input$rechisq, { rv$chisq <- rchisq(500, 2) })


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
