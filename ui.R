library(shinyBS)
library(readr)
library(shinycssloaders)




options(java.parameters = "-Xss2560k")


shinyUI(fluidPage(
  
  HTML('
    <link rel="stylesheet" href="css/nist-combined.css">
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.6.0/jquery.min.js"></script>
    <script src="js/nist-header-footer.js" type="text/javascript" defer="defer"></script>
    <html class="nist-footer-bottom">
    '),
  
  tags$head(HTML("
    <!-- Google tag (gtag.js) -->
    <script async src='https://www.googletagmanager.com/gtag/js?id=G-1L2H9B3YCD'></script>
    <script>
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());
    
      gtag('config', 'G-1L2H9B3YCD');
    </script>")),
  
    tags$style("#mu_est {font-size:14px;
               color:steelblue;
               }"),
    tags$style("#mu_se {font-size:14px;
               color:steelblue;
               }"),
    tags$style("#mu_median {font-size:14px;
               color:steelblue;
               }"),
    tags$style("#mu_quant {font-size:14px;
               color:steelblue;
               }"),
    
    ###
    
    tags$style("#mu_est1 {font-size:14px;
               color:steelblue;
               }"),
    tags$style("#mu_se1 {font-size:14px;
               color:steelblue;
               }"),
    tags$style("#mu_median1 {font-size:14px;
               color:steelblue;
               }"),
    tags$style("#mu_quant1 {font-size:14px;
               color:steelblue;
               }"),
    tags$head(tags$style(
      HTML('#title {
           color: red;
           font-size: 40px;
           font-style: bold;
          }'))),
    tags$head(
      tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
      }
    "))
    ),
    
  theme = "style.css",
     withMathJax(),
     tags$head(
       tags$style(HTML("
                      .shiny-output-error-validation {
                      color: red;
                      }
                      "))
     ),
    


    h1(id="big-heading", strong("NIST Chemical Analysis Package: Design for a plate with 24 wells")),
    tags$style(HTML("#big-heading{color:steelblue;}")),
   
    br(),
    
   
      
     
      
    
    navlistPanel(
      
      widths=c(4,8),
      tabPanel(
      h4("About Well App "),
      h2("About Well App", style = "color:steelblue"), 
      h4("The NIST Well App package is a software application designed to help chemical analysts... "),
      br(),
     
      ),
      
      "Pre-Experiment",
      
      selected =  h4("Data entry"),
      
      tabPanel(
        h4("Background"),

        em(h2("Quick Start: Pre-Experiment", style = "color:steelblue")),
        br(),
        em(h3("Data Entry")),
        hr(),
        h4("Enter known concentrations of analyte in working standard solutions used to prepare calibrants. Multiple working standard concentrations can be input, values separated by commas. An uncertainty estimate of each concetration is required."), 
        hr(),
        img(src='Background_IMG1.png', align = "center", height = 250, width = 800),
        hr(),
        h4("Enter uncertainties associated with adding analyte and internal standard quantities to calibrants."),
        hr(),
        h4("Enter uncertainty of internal standard quantities added to samples. Enter uncertainty of sample substance aliquot (e.g., serum matrix sample)"),
        hr(),
        img(src='Background_IMG2.png', align = "center", height = 400, width = 750),
        hr(),
        em(h4("Data Table Upload")),
        hr(),
        br(),
        h4("Experiments that generate multiple calibration curves to determine a single result are supported. For example:"),
        h4("- measurements conducted over several days with separate calibrations"),
        h4("- employment of multiple different techniques using internal standards"),
        br(),
        em(h4("Multiple data files can be uploaded simultaneously. Upload calibration and corresponding sample tables in the same order.")),
        br(),
        img(src='Background_IMG3.png', align = "center", height = 350, width = 825),
        hr(),
        h4("Calibration Data Tables"),
        
        
        
        em(h3("Model Settings")),
        hr(),
        h4("1) Specify coverage interval of estimated result. Default is 95 %."),
        h4("2) Indicate destired number of decimal places printed for results"),
        h4("3) Total number of iterations should be adjusted if app indicates that more iterations are needed"),
        h4("4) Length of burn in should only be increased if app indicates that the model has not converged. Do not decrease this value."),
        br(),
        img(src='Background_IMG4.png', align = "center", height = 350, width = 825),
        br(),
        hr(style="border-color: black;"), 
        
      ),
    
      tabPanel(
        h4("Data entry"),
        
        
        fluidRow(style='margin: 0px;',
                 column(12,
                        h3("Input number of replicates per sample:",style = "color:steelblue"))),
        fluidRow(style='margin: 0px;',
                 column(6,
                        wellPanel(
                          numericInput("nrepNC",label =h4("Number of reps of NC"), value="3"))),
                 
                 column(6,
                        wellPanel(
                          numericInput("nrepTC1",label = h4("Number of reps of TC1"),value="3"))),
                 
        ),
        
        fluidRow(style='margin: 0px;',
                 column(12,
                        wellPanel(
                          numericInput("nrepTC2",label =h4("Number of reps of TC2"), value="3")))
        ),
        
      
          fluidRow(style='margin: 0px;',
         column(12,
          tabsetPanel(
          tabPanel(
            title=h4("Upload  Data Tables:",style = "color:steelblue"),
            br(),
            column(12, downloadButton('downloadCalTable', 'Download  Data Table Template')),
            
            # column(6, 
            #        em(h5("X data:")),
            #        em(h5("mid: Quantity (mass or volume) of internal standard added to calibrant(Only for internal standard use)")),
            #        em(h5("mad: Quantity (mass or volume) of analyte working standard solution in calibrant")),
            #        em(h5("Analyte working standard solution ID # (e.g. 1, 2)"))),
            
            # column(6,
            #        em(h5("Y data:")),
            #        em(h5("raci: Analyte: Internal Standard Peak Areas Ratio for calibrant repeat measurement i"))),
            
            fileInput(
              inputId = "file",
              label = "",
              multiple = TRUE,
              accept = c("text/csv",".xlsx",
                         "text/comma-separated-values,text/plain",
                         ".csv"),
              placeholder = ""
            )),
          
          
          tabPanel(
            title=h4(""),
           
           
          )))),
        
                    
                   uiOutput("expui"),
                   uiOutput("doe12"),
                  uiOutput("doeinput"),
                  
        
        br(),
        
        ########Fit the model ui#######
        
        ##################################################################
        ### MCMC Results #################################################
        ##################################################################
        mainPanel(
          fluidRow(style='margin: 0px;',
            actionButton("go","Fit the model", icon("paperr-plane"), 
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4", width = '30%')
          ),
          fluidRow(style='margin: 0px;',
            helpText("Please check your data first and then fit the model")
          )
        ),
        
        fluidRow(style='margin: 0px;',
          
          column(6,
                 tableOutput("calnum")
                 
          ),
          column(6,
                 tableOutput("samplenum2")
          )
        )
        ,
        
        fluidRow(style='margin: 0px;',
          column(6,
                 tableOutput("caltables")
          ),
          column(6,
                 tableOutput("sampletables")
          )
        )
        ,
        hr(),
        
       
          
        
        conditionalPanel(

          condition="input.go >= 1",

          h2("Model Results",style = "color:steelblue"),
          tableOutput("preresult")
          ),

          # conditionalPanel(
            # 
            # withSpinner(tableOutput("opt"),type = 8,color = "seagreen"),
            # tableOutput("preresult"),
            # fluidRow(style='margin: 0px;',



                      # uiOutput("doem"),
           # uiOutput("err"),
           # uiOutput("doeout"),
           # uiOutput("doee")
            # )
          # ),
          
           
           uiOutput("muest"),
          
          # fluidRow(
          #   column(8,
          #           p("The posterior mean is: ",withSpinner(textOutput("mu_est",inline=T), type = 8,color = "seagreen")),
          #          p("The standard uncertainty is: ",textOutput("mu_se",inline=T)),
          #          p("The posterior median is: ",textOutput("mu_median",inline=T)),
          #          p("The ",textOutput("coverageProbabilityPercentBayes",inline=T),
          #            " credible interval ranges from: ",textOutput("mu_quant",inline=T))
          #   )),
          
          hr(),
         
          uiOutput("plots"),
          hr(),
          uiOutput("out1"),
          
        
          downloadButton("report", "Generate PDF report"),
         
          #downloadButton('downloadbayesout', 'Download inputs and estimates(csv file)'),
          #downloadButton('downloaddoeinput', 'Download DoE inputs (csv file)'),
          
          plotOutput("mu_post_plot", width = "auto", height = "600px"),
         # downloadButton('download_mu_post_plot', 'Download plot'),
          
          br(),
          br(),
          
          
          plotOutput("mu_trace_plot", width = "auto", height = "600px"),
         # downloadButton('download_mu_trace_plot', 'Download plot')
          
          ),
      
      
        
      "Design of Experiment",
      
      tabPanel(
        h4("Background"),
        #         h3("1. About ",em("Design of Experiment"),style = "color:steelblue"), 
        #         h4("This module produces experiment designs for chemical measurement procedures based on the calibration methods supported by ABACUS.
        #                             This experiment design guarantees that the relative standard uncertainty of the measurement result will be less than a user-specified maximum value."),
        #         h4("In addition to the maximum relative uncertainty, the app requires prior information about various aspects of the measurement procedure. This information can be based on experimental evidence or expert opinion. "),
        #         h4("Estimates of experimental parameters calculated with the", em("Pre-Experiment"), " module can be directly transferred as inputs into this", em("Design of Experiment"), " module"),
        #         
        #         h3("2. Inputs:",style = "color:steelblue"),
        #         h4("Inputs are provided with results of the ", em("Pre-Experiment module."), " They can also be estimates based on the analyst's judgement."),
        #         br(),
        #         h4("Note: for a method of calibration using external standards, values for observations of A:I described below can simply be entered as the respective values for A."),
        #         h4("1. Specify the total number of measurements that can be performed. This includes the number of calibrants, available samples, and repeat measurements."),
        #         h4("2. Specify the Target maximum relative (%) standard uncertainty"),   
        #         h4("3. Provide an approximate expected value of the measurand"),
        #         h4("4. Provide a range of possible expected values of the slope of your calibration line, e.g. 1.2 to 1.4. "),
        #         h4("5. Identify the target calibration region (x-axis); this is an interval in terms of the the ratios of concentrations of A:I in the calibrants."),
        #         h4("6. Provide a range of values (uy) for the expected standard deviation of peak area ratios (A:I) determined from repeated measurement of a calibrant."),
        #         h4("7. Provide a range of values (ux) for the expected standard uncertainties of concentrations (A:I) in calibrants. 
        # Or, provide a range of values (rux) for the expected relative standard uncertainty of the concentrations (A:I) in calibrants.  Input of at least one of these ranges is required. ux can be 0 if rux > 0; rux can be 0 if ux > 0"),
        #         h4("8. Provide a range of values for the expected relative standard uncertainties of the concentrations of I in samples. For calibration methods using external standards, this should be 0."),
        #         h4("9. Provide a range of values for the expected between-sample variability (standard deviation) of the peak area ratios (A:I), e.g., variability due to replicate sample preparation and heterogeneity."),
        #         
        #         
        #         h3("3. Results",style = "color:steelblue"),
        #         h4("Once this information is entered, the App will produce an experiment design that can acheive an expected relative standard uncertainty of the measurand that is less than or equal to the specified maximum. If there are not enough samples available to achieve the Target maximum relative (%) standard uncertainty, ABACUS will indicate the smallest possible uncertainty under the specified experimental conditions and inputs. Specifically, it will give:"),
        #         h4("1.	The number of calibrants and the concentration ratios of A:I; for calibration methods using external standards, the concentrations of A."),
        #         h4("2.	The number of repeat measurements recommended for each calibrant. "),
        #         h4("3.	The number of samples."),
        #         h4("4.	The number of repeat measurements recommended for each sample.")
        #         
      ),
      tabPanel(
        h4("Data entry"),
        
        fluidRow(style='margin: 0px;',
                 column(12,
                        wellPanel(
                          h3(strong("Optimal designs of modules",style = "color:seagreen")),
                          
                          radioButtons("doeoption",
                                       label=NULL,
                                       choices=list(
                                         "Optimal design for Single TC"=1,
                                         "Optimal design with multiple TCs"=2),
                                       inline = T
                          )
                        ))
                 
        ),
        uiOutput("doeoption1"),
        
       
        
        mainPanel( 
          fluidRow(style='margin: 0px;',
                   actionButton("enter","Run the model", icon("paperr-plane"), 
                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4", width = '30%')
          ),
          fluidRow(style='margin: 0px;',
                   
                   helpText("Please check your data first and then run the model")
                   
          )),
        
        br(),
        
        conditionalPanel(
          
          # id="results",
          condition="input.enter >= 1",
          
          
          
          fluidRow(style='margin: 0px;',
                   column(12,
                          withSpinner(uiOutput("doeenter"), type = 8,color = "seagreen"),
                          
                          uiOutput("totnerr") ,
                    
                          uiOutput("outmul"),             
                          
                          uiOutput("doeoutput")

            )
         )
          
        )
      
        )
    
        )
    )
      
)
   
      
      


