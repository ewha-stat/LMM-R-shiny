library(shiny)
library(lme4)
library(gee)

shinyUI(
  fluidPage(
    
    titlePanel("Analysis of Longitudinal data"),
    
    sidebarLayout(
      
      sidebarPanel(
        wellPanel(
          h4("STEP1"),
          fileInput('datafile', 'Choose CSV file',
                    accept=c('text/csv', 'text/comma-separated-values,text/plain')),
          
          uiOutput("vars"),
          
          checkboxInput('header', 'Header', TRUE),
          radioButtons('sep', 'Separator',
                       c(Comma=',',
                         Semicolon=';',
                         Tab='\t'),
                       ','),
          radioButtons('quote', 'Quote',
                       c(None='',
                         'Double Quote'='"',
                         'Single Quote'="'"),
                       '"'),
          
          h4("STEP2"),
          
          uiOutput("varselect"),
          
          uiOutput("varselect2"),
          
          uiOutput("varselect3"),
          
          uiOutput("varselect4"),
          
          checkboxInput('intercept', 'Intercept', TRUE),
          
          uiOutput("formula"),
          
        
          
          
          checkboxInput('text', 'REML', TRUE),
          
          h4("STEP3"),
                 
          uiOutput("formula2")
        
          
        )  )
  ,
  
  mainPanel(
    
    navbarPage("Menu",
               
               tabPanel("Data"    ,  
                  tabsetPanel(
                          tabPanel("Data Table" ,  dataTableOutput("table")),
                          tabPanel("Data Summary" ,   verbatimTextOutput("summary")),
                        hr()
                        )),

               tabPanel("2 stage analysis",plotOutput("plot1"),
                                 h4("The fitted line for each treat"),htmlOutput("plot1.description")),
               
               tabPanel("LMM", verbatimTextOutput("lmm")),
                  
               tabPanel("LRT(Model Comparison)", 
                        
                        h3("Hypothesis"),
                        
                        h5("Null Hypothesis:",verbatimTextOutput("null")),
                        
                        h5("Alternative Hypothesis:",verbatimTextOutput("alter")),
                        
                        hr(),
                        
                        h3("Likelihood Ratio Tests", verbatimTextOutput("LRT")
                        ),
                        
                        code ("Interpretation : p-value < alpha (0.05) → reject NULL hypothesis  ")
                        
                        
                        
               ),
              
               tabPanel("FE Test",
                        tabsetPanel(
                          tabPanel("Satterthwaite", verbatimTextOutput("Satterth")),
                          tabPanel("Kenward-Roger", verbatimTextOutput("Kenward"))
                        )), 
               
               tabPanel("RE Test", 
                        tabsetPanel(
                          tabPanel("Caterpillar Plot", plotOutput("lmmPlot"),
                          #image
                          br(),
                          img(src="s3.png", height = 200, width =200) ),
                          
                          tabPanel("Coef_Table", verbatimTextOutput("coef"))
                          
                        )),
               
               
               tabPanel("GEE",verbatimTextOutput("gee.out"), h5 ("NOTE : correlation structure is assumed independence", 
                                                                 style = " color : blue "))
               
               
              
    )
    ))
    ))