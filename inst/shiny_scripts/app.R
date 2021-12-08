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
                                        label = "Provide a valid image file path or url:"),
                              helpText("(Try a sample link:
                                       https://user-images.githubusercontent.com/60583839/141215629-f19d4a77-c5f0-491f-9262-b22cd59739e3.jpg)"),
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
                                helpText("Note that this function only shows image in grayscale."),
                                # for picking a frame [*Note: most fluorescent microscopy images have 3~5
                                # channels. Here the upper bound is set to 3, can be increase if needed.*]
                                numericInput(inputId = "fNum_2.1",
                                             label = "Please specify a frame",
                                             helpText("(see Home tab for the frame number of nuclei/cell body)"),
                                             value = 3,
                                             min = 1, max = 3),
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
                                     helpText("(Blurring can be useful prior to analyzing
                                              individual objects in images with ill-defined
                                              edges and/or uneven intensities.)"),
                            sidebarLayout(sidebarPanel(
                                helpText("Note that this function only applies to binary or grayscale images."),
                                # for picking a frame
                                numericInput(inputId = "fNum_2.2",
                                             label = "Please specify a frame",
                                             value = 3,
                                             min = 1, max = 3),
                                # for picking brush size
                                numericInput(inputId = "bSize",
                                             label = "Brush size:",
                                             value = 11),
                                # for picking brush shape
                                selectInput(inputId = "bShape",
                                            label = "Brush shape:",
                                            choices = list('line','box','disc','diamond','Gaussian')),
                                helpText("Guide to using magic brush:
                                         Gaussian is commonly used on smaller objects. Line
                                         brush is good for larger, more clearly defined objects.")
                              ),
                              mainPanel(
                                helpText("Display may take a few seconds to render"),
                                tabsetPanel(
                                  tabPanel("Interactive browser", displayOutput("widget_2.2"))
                                )
                              ))
                            ),
                            # Tab 2.3: Object Segmentation
                            tabPanel(title = "Object Segmentation",
                                     titlePanel("Generate segmented images of cellular structures"),
                                     tags$p("On this page you can perform image segmentation by
                                            providing the frame numbers of cell and nuclei channels."),
                                     helpText("(This function is peformed on colored images. Structures
                                              that are hard-to-distinguish by eye are clearly outlined.)"),

                                     sidebarLayout(sidebarPanel(
                                       # for picking frame of cell body
                                       numericInput(inputId = "fCell",
                                                    label = "Frame number of cell body:",
                                                    value = 2,
                                                    min = 1, max = 3),
                                       # for picking frame of the nuclei
                                       numericInput(inputId = "fNuc",
                                                    label = "Frame number of cell nuclei:",
                                                    value = 3,
                                                    min = 1, max = 3),
                                       # for picking what to segment
                                       selectInput(inputId = "seg",
                                                   label = "Highlight which structure:",
                                                   choices = list('nuclei', 'cell body', 'both')),
                                       helpText("Note: nuclei are outlined in yellow, cell bodies in pink.")
                                     ),
                                     mainPanel(
                                       helpText("Display may take a few seconds to render"),
                                       tabsetPanel(
                                         tabPanel("Interactive browser", displayOutput("widget_2.3"))
                                       ))
                                    )
                 )),
                 # Tab 3: Nuclei Counter
                 tabPanel(title = "Nuclei Counter",
                          titlePanel("Count the number of nuclei in an image"),
                          tags$p("On this page, an automatic count of cell nulei is generate with
                                 an error margin of 10% compared to manual counting"),
                          sidebarLayout(sidebarPanel(
                            # for locating the frame of nuclei
                            numericInput(inputId = "fNuclei_3",
                                         label = "Please indicate the frame with nuclei objects:",
                                         value = 3,
                                         min = 1, max = 3),
                            tags$p("When automatically counting cellular objects (most commonly nuclei),
                                   the following steps take place under the hood:"),
                            tags$p("1. image enhanced if objects have week signals or ill-defined edges"),
                            tags$p("2. otsu threshold applied to turn greyscale image into binary so every pixel value is either 0 or 1"),
                            tags$p("3. foreground objects are counted (with pixel value of 1)")
                          ), mainPanel(
                            helpText("Display may take a few seconds to render"),
                            tabsetPanel(tabPanel("Count Result", verbatimTextOutput("textOut")),
                                        tabPanel("Manually Confirm", displayOutput("widget_3"))),
                            uiOutput("otsu"))
                          )
                  ),
                 # Tab 4: Nuclei Features
                 navbarMenu(title = "Nuclei Features",
                            tabPanel(title = "Single view",
                                     titlePanel("Feature Density Plot"),
                                     tags$p("On this page, you can view the density distribution
                                     of one morphological feature of nuclei"),
                                     sidebarLayout(sidebarPanel(
                                       # for locating the frame of nuclei
                                       numericInput(inputId = "fNuclei_4.1",
                                                    label = "Please indicate the frame with nuclei objects:",
                                                    value = 3,
                                                    min = 1, max = 3),
                                       # for selecting feature
                                       selectInput(inputId = "feature",
                                                   label = "Please indicate the feature to plot:",
                                                   choices = list("area", "perimeter", "radius", "roundness")),
                                       tags$em("Orbital Eccentricity:"),
                                       helpText("a dimensionless parameter that measures
                                       the amount by which an elliptical orrbit deviates from a perfect circle.
                                       It is defined by the square root of (1-(minorAxis)^2/(majorAxis)^2).
                                       This value approaches 0 for rounder objects and 1 for more elongated ones.")
                                     ), mainPanel(
                                        tabsetPanel(tabPanel("Selected Plot", plotOutput("dens")),
                                                    tabPanel("Features Dataframe", tableOutput("table")))
                                     ))
                            ),
                            tabPanel(title = "Matrix View",
                                     titlePanel("Features Scatter Matrix Plot"),
                                     tags$p("On this page, you can view the density distributions, scatter
                                            plots, and correlation of all four morphological features of nuclei."),
                                     sidebarLayout(sidebarPanel(
                                       tags$p("In addition to viewing individual morphological features of
                                       the nuclei side-by-side, it may also be of your interest to see
                                       how they are correlated to fully understand the cellular structure."),
                                       tags$p("For example, recent research in muscle stem cells points to
                                              strong correlation between nuclear size and circularity as an
                                              indicator that the stem cells are transcriptionally active.")
                                     ), mainPanel(
                                       # for locating the frame of nuclei (the reason this is part repeated so many
                                       # times is that we cannot be sure whether the user has indicated this before)
                                       numericInput(inputId = "fNuclei_4.2",
                                                    label = "Please indicate the frame with nuclei objects:",
                                                    value = 3,
                                                    min = 1, max = 3),
                                       tabsetPanel(tabPanel("Features Matrix", plotOutput("matrix")))
                                     ))
                            )
                 )
)

server <- function(input, output) {
  # Set up reactive image file
  inFile <- reactive({
    if(input$html == ""){
      out <- MyoManager::loadImage(system.file("extdata", input$sample, package="MyoManager"))
    } else {
      out <- MyoManager::loadImage(input$html)
    }
    return(out)
  })

  ######################################## Tab 1: Load & Display
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
    if (!is.null(rv$colorMode)){
      plot(EBImage::`colorMode<-`(inFile(), rv$colorMode), all=TRUE)
    } else {
      return()
    }
  })

  output$widget_1 <- renderDisplay({
    if (!is.null(rv$colorMode)){
      MyoManager::viewImage(inFile(), rv$colorMode)
    } else {
      return()
    }
  })
 ######################################## Tab 2: Image Processor
  # Display 2.1: Intensity Ctrl
  output$widget_2.1 <- EBImage::renderDisplay({
    MyoManager::viewImage(MyoManager::intensityCtrl(MyoManager::selectFrame(inFile(), input$fNum_2.1),
                                                    input$brightness,
                                                    input$contrast),
                          color_mode = 0)
  })

  # Display 2.2: Blurring
  output$widget_2.2 <- EBImage::renderDisplay({
    MyoManager::viewImage(MyoManager::blurImage(MyoManager::selectFrame(inFile(), input$fNum_2.2),
                                                input$bSize,
                                                input$bShape),
                          color_mode = 0)
  })

  # Display 2.2: Segmentation
  output$widget_2.3 <- EBImage::renderDisplay({
    MyoManager::viewImage(MyoManager::segmentImage(inFile(),
                                                   input$fCell,
                                                   input$fNuc,
                                                   input$seg)
                          )
  })
  ######################################## Tab 3: Nuclei Counter
  # text output
  output$textOut <- renderPrint({
    MyoManager::countNuclei(MyoManager::selectFrame(inFile(), input$fNuclei_3))
  })

  # accompanying image output
  output$widget_3 <- EBImage::renderDisplay({
    MyoManager::viewImage(MyoManager::segmentImage(inFile(),
                                                   input$fCell,
                                                   input$fNuc,
                                                   'nuclei')
                          )
    })

  # more about Otsu (because it's pretty genius)
  url <- a("Otsu's Thresholding", href="https://learnopencv.com/otsu-thresholding-with-opencv/")
  output$otsu <- renderUI({
    tagList("Learn more about:", url)
  })
 ######################################## Tab 3: Nuclei Feature Plots
 # Tab 4.1 Density Plot & Dataframe
 output$table <- renderTable({
   MyoManager::getFeatureData(MyoManager::selectFrame(inFile(), input$fNuclei_4.1))
 })
 output$dens <- renderPlot({
   tab <- MyoManager::getFeatureData(MyoManager::selectFrame(inFile(), input$fNuclei_4.1))
   MyoManager::plotFeature(tab, input$feature)
 })

 # Tab 4.2 Scatter Matrix
 output$matrix <- renderPlot({
   tab <- MyoManager::getFeatureData(MyoManager::selectFrame(inFile(), input$fNuclei_4.2))
   MyoManager::plotFeatureMatrix(tab)
 })

}
shinyApp(server = server, ui = ui)
#[End]
