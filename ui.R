library(shiny)
library(DT)
library(ggplot2)
library(shinycssloaders)
library(foreach)
library(doParallel)

shinyUI(
  navbarPage("Dirichlet Kernel Ternary Viewer",
             
             tabPanel("Data Preparation",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Check the box to upload your own data.", style = "color: #003366; font-weight: bold;"),
                          checkboxInput("show_upload", "Upload Your Own Data", value = FALSE),
                          
                          tags$hr(style = "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px;"),
                          
                          h4("Input Options", style = "color: #003366; font-weight: bold;"),
                          
                          uiOutput("dataInputUI")
                        ),
                        
                        mainPanel(
                          div(
                            style = "background-color: #fcfcfc; padding: 15px; border: 1px solid #ddd; border-radius: 8px;",
                            
                            uiOutput("previewUI")
                          )
                        )
                      )
             ),
                      
             
             tabPanel("Ternary Plot",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Option", style = "color: #003366; font-weight: bold;"),
                          # 這裡可以放置一些選項，例如顏色、大小等
                          checkboxInput("best.h", "Enter Bandwidth (h) manually", value = FALSE),
                          uiOutput("hUI"),
                          div(style = "height: 20px;"),
                          tags$hr(style = "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px;"),
                          
                          sliderInput("resolution", "Resolution:", 
                                      min = 0.002, max = 0.005, value = 0.005, step = 0.0001),
                          selectInput("alpha", "Alpha:",
                                      choices = c(0,1)),
                          textInput("title", "Plot Title:", value = ""),
                          textInput("scale.bar.title", "Color Bar Title:", value = "Y"),
                          textInput("color1", "Scale color low:", value = "yellow"),
                          textInput("color2", "Scale color high:", value = "blue"),
                          checkboxInput("boundary", "Show Boundary", value = TRUE),
                          actionButton("drawPlot", "Draw Ternary Plot"),
                          tags$hr(style = "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px;")
                          
                        ),
                        mainPanel(
                          uiOutput("ternaryUI")    # 預覽區塊也是動態生成
                        )
                      )
             ),
             tabPanel("Semi-parametric Estimation",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Bootstrap Option", style = "color: #003366; font-weight: bold;"),
                          tags$hr(style = "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px;"),
                          
                          # 這裡可以放置一些選項，例如顏色、大小等
                          sliderInput("cores", "Cores:", 
                                      min = 1, max = 16, value = 1, step = 1),
                          sliderInput("bootstrap.n", "Number of Bootstraps:", 
                                      min = 1, max = 1000, value = 10, step = 1),
                          selectInput("tolerance", "Tolerance:",
                                      choices = c(1e-4,1e-5,1e-6,1e-7,1e-8)),
                          h4("Plot Option", style = "color: #003366; font-weight: bold;"),
                          tags$hr(style = "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px;"),
                          
                          sliderInput("semi.resolution", "Resolution:", 
                                      min = 0.002, max = 0.005, value = 0.005, step = 0.0001),
                          textInput("semi.title", "Plot Title:", value = ""),
                          textInput("semi.scale.bar.title", "Color Bar Title:", value = "Y"),
                          textInput("semi.color1", "Scale color low:", value = "yellow"),
                          textInput("semi.color2", "Scale color high:", value = "blue"),
                          checkboxInput("semi.boundary", "Show Boundary", value = TRUE),
                          actionButton("semi.go", "Run"),
                          tags$hr(style = "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px;")
                          
                        ),
                        mainPanel(
                         h4("Linear Estimation", style = "color: #003366; font-weight: bold;"),
                         DT::dataTableOutput("backfittingTable"),
                         h4("Non-Linear Estimation", style = "color: #003366; font-weight: bold;"),
                         withSpinner(plotOutput("residualPlot"), type = 4, color = "#0dc5c1")
                        )
                      )
             )
             
             
             # 你之後可以在這裡加 tabPanel("Plot", ...)，繼續擴充
  )
)
