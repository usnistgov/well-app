#library(shinyBS)
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
    
    # # tags$style("#rsq {font-size:14px;
    # #            color:steelblue;
    # #            }"),
    #
    theme = "style.css",
     withMathJax(),
     tags$head(
       tags$style(HTML("
                      .shiny-output-error-validation {
                      color: red;
                      }
                      "))
     ),
    
 
  #  titlePanel(tags$h2(
   #    tags$a("NIST ABACUS Chemical Analysis Package: Measurement Calibration")
    # )
     #),

   # h1(id="big-heading", strong("NIST ABACUS Chemical Analysis Package: Measurement Calibration")),
    #tags$style(HTML("#big-heading{color:steelblue;}")),
      headerPanel(
    h2(strong("NIST Experimental Design Package for a Plate with 24 Wells",style = "color:steelblue"))
        ),
   # br(),
    
    
   navlistPanel(
      
     widths=c(4,8),
    #  tabPanel(
    #  h4("About ABACUS"),
      # h2("About ABACUS", style = "color:steelblue"), 
      # h4("The NIST ABACUS (Apps for Bayesian Analysis of Chemical quantities Using Shiny) package is a software application designed to help chemical analysts make well-informed decisions and perform rigorous calculation of results. "),
      # br(),
      # h4('ABACUS is comprised of three modules that can be used separately or in any combination during the measurement process:'),
      # em(h4("1) Pre-Experiment", style = "color:steelblue")),
      # h4("Exploratory measurement data is analyzed to inform an approximate expectation of the measured quantity and an efficient set of experiments. The", em("Pre-Experiment"), " module calculates estimates for several characteristics of the measurement procedure that can inform the", em("Design of Experiment"),"."),
      # em(h4("2) Design of Experiment", style = "color:steelblue")), 
      # h4("A central function ABACUS is the planning of optimal experiment designs to achieve a target measurement uncertainty. Based on results from the", em("Pre-Experiment"), " module, or simply estimated inputs based on user judgement, this module determines an experiment design that can achieve the target uncertainty with as few measurements and prepared samples as possible."),
      # em(h4("3) Analysis", style = "color:steelblue")),
      # h4("The ", em("Analysis"), " module uses all data from the experiments to calculate a result with a rigorous estimate of measurement uncertainty. Calibration methods based on linear regression models or response factors are supported, as is the use of internal or external standards. Data from calibrations performed on separate days or by different methods can be utilized to inform the result."),
      # br(),
      # h4("ABACUS performs calculations for measurements that are based on two-step experiments conducted to:"), 
      # br(),
      # h4("1) estimate a calibration function"), 
      # h4("2) determine unknown quantities of analyte in samples using the calibration function and sample measurement data"),
      # 
      # br(),
      # h4("The statistical approaches employed for the Pre-Experiment and Analysis modules are described in:"),
      # 
      # downloadButton('paper_download', 'Statistical Approach: ID-LC-MS Model'),
      # br(),
      # br(),
      # h4("ABACUS calculations are based on  the following measurement equtions:
      #      "),
      # h3("1) Calibration"),
      # br(),
      # h4("a) Using Internal Standards:"),
      # br(),
      # em(h4("Linear Regression Model")), 
      # h4(withMathJax("Quantities of analyte (A) : internal standard
      #                                      (I), \\(k_{S}\\), in a measured sample can be calculated as:")), 
      # br(),
      # h4(em(withMathJax("\\( k_{S} = \\displaystyle{\\frac{R_{S}-a}{b}}\\),")), align = 'center'), 
      # h4("where a and b are coefficients of a linear regression determined from the measurement of n number of calibrants:"), 
      # br(),
      # h4(em(withMathJax("\\(R_{C_{i}}= a + b \\times k_{C_{i}}, i = 1, . . ., n\\),")), align = 'center'),
      # br(), 
      # h4(em(withMathJax("\\(R_{C_{i}} = \\displaystyle{\\frac{Measured\\ Response\\ of\\ A}{Measured\\ Response\\ of\\ I}}\\) in calibrant \\(i\\),"))),
      # br(),
      # h4("and"),
      # br(),
      # h4(em(withMathJax("\\(k_{C_i} =	 \\displaystyle{\\frac{quantity\\ of\\ A}{quantity\\ of\\ I}}\\) in calibrant \\(i\\);"))), 
      # br(),
      # em(h4("Response Factor Model")),
      # h4(em(withMathJax("\\(R_{C_{i}}= b \\times k_{C_{i}}, i = 1, . . ., n\\)")), align = 'center'),
      # br(),
      # h4("b) Using External Standards:"),
      # br(),
      # h4("Equations for calibration approaches using external standards are similar to those above for use with internal standards except that:"),
      # br(),
      # h4(withMathJax("\\(R_{C_{i}}=\\) measured response of A in calibrant \\(i\\),")),
      # br(),
      # h4(withMathJax("\\(k_{C_i}=\\) quantity of A in calibrant \\(i\\)")),
      # br(),
      # h3("2) Sample Measurement"),
      # br(),
      # h4("Equations below are the same for procedures based on either Linear Regression or Response Factor calibration models"),
      # br(),
      # h4("a) Using Internal Standards:"),
      # br(),
      # h4(em(withMathJax("\\(R_{S} = \\displaystyle{\\frac{Measured\\ Response\\ of\\ A}{Measured\\ Response\\ of\\ I}}\\) in a measured sample"))), 
      # br(),
      # h4(withMathJax("An estimate of \\(k_{S}\\) for each sample can be determined from the calibration model and the corresponding observation of \\(R_{S}\\). The measured quantity of analyte in each sample, \\(w_{A}\\), is then calculated as:")), 
      # br(), 
      # h4(em(withMathJax("\\(w_{A}=\\displaystyle{k_{S} \\times \\frac{m_{I_{S}}}{m_{S}} }\\)"), align = "center")), 
      # h4("where,"), 
      # h4(withMathJax("\\(m_{I_{S}}=\\) quantity of internal standard added to the sample,")), 
      # br(), 
      # h4(withMathJax("\\(m_{S}=\\) quantity of sample substance")),
      # br(),
      # h4("b) Using External Standards:"),
      # br(),
      # h4("Equations for samples without an internal standard (calibration using external standards) are similar as those above except that:"),
      # br(),
      # h4(withMathJax("\\(R_{S}=\\) measured response of A in a sample,")),
      # br(),
      # h4(em(withMathJax("\\(w_{A}=\\displaystyle{\\frac{k_{S}}{m_{S}} }\\)"), align = "center")),
      # 
      # ),
      # 
      # "Pre-Experiment",
      # 
      # selected =  h4("Data entry"),
      # 
      # tabPanel(
      #   h4("Background"),
      # 
      #   em(h2("Quick Start: Pre-Experiment", style = "color:steelblue")),
      #   br(),
      #   em(h3("Data Entry")),
      #   hr(),
      #   h4("Enter known concentrations of analyte in working standard solutions used to prepare calibrants. Multiple working standard concentrations can be input, values separated by commas. An uncertainty estimate of each concetration is required."), 
      #   hr(),
      #   img(src='Background_IMG1.png', align = "center", height = 250, width = 800),
      #   hr(),
      #   h4("Enter uncertainties associated with adding analyte and internal standard quantities to calibrants."),
      #   hr(),
      #   h4("Enter uncertainty of internal standard quantities added to samples. Enter uncertainty of sample substance aliquot (e.g., serum matrix sample)"),
      #   hr(),
      #   img(src='Background_IMG2.png', align = "center", height = 400, width = 750),
      #   hr(),
      #   em(h4("Data Table Upload")),
      #   hr(),
      #   br(),
      #   h4("Experiments that generate multiple calibration curves to determine a single result are supported. For example:"),
      #   h4("- measurements conducted over several days with separate calibrations"),
      #   h4("- employment of multiple different techniques using internal standards"),
      #   br(),
      #   em(h4("Multiple data files can be uploaded simultaneously. Upload calibration and corresponding sample tables in the same order.")),
      #   br(),
      #   img(src='Background_IMG3.png', align = "center", height = 350, width = 825),
      #   hr(),
      #   h4("Calibration Data Tables"),
      #   
        
        
        
      #   em(h3("Model Settings")),
      #   hr(),
      #   h4("1) Specify coverage interval of estimated result. Default is 95 %."),
      #   h4("2) Indicate destired number of decimal places printed for results"),
      #   h4("3) Total number of iterations should be adjusted if app indicates that more iterations are needed"),
      #   h4("4) Length of burn in should only be increased if app indicates that the model has not converged. Do not decrease this value."),
      #   br(),
      #   img(src='Background_IMG4.png', align = "center", height = 350, width = 825),
      #   br(),
      #   hr(style="border-color: black;"), 
      #   
      # ),
      # 
      # tabPanel(
      #   h4("Data entry"),
      #   fluidRow(style='margin: 0px;',
      #   column(12,
      #   wellPanel(
      #     h3(strong("Choose a method of calibration",style = "color:seagreen")),
      #     
      #     radioButtons("choice",
      #                  label=NULL,
      #                  choices=list(
      #                    "Linear Regression"=1,"Response Factor"=2),
      #                  inline = T),
      #     radioButtons("standard",
      #                  label=NULL,
      #                  choices=list(
      #                    "Internal Standard"=1,"External Standard"=2),
      #                  inline = T)
      #     
      #     )
      #     
      #   )),
      #   
      #   uiOutput("standard_ui"),
        
        # fluidRow(style='margin: 0px;',
        # column(12,
        # h3("Calibration standard inputs:",style = "color:steelblue"))),
        # 
        # 
        # fluidRow(style='margin: 0px;',
        #   column(6,
        #          wellPanel(
        #            textInput("wadm",label =h4("Analyte working standard solution concentrations; Comma-separated entries for multiple inputs"), value="94.2, 95.5, 94.5",placeholder = "e.g. '94.2, 95.5, 94.5'"))),
        #   
        #   column(6,
        #          wellPanel(
        #            textInput("uwad",label = h4("Uncertainties in working standard solution; Comma-separated entries for multiple inputs"),value="0.427, 0.434, 0.429",placeholder = "e.g. '0.427, 0.434, 0.429'")))),
        # 
        # fluidRow(style='margin: 0px;',
        #   
        #   column(6,
        #          wellPanel(
        #            numericInput(
        #              "u_mid", 
        #              label = h4("Uncertainty in masses (or volume) of internal standard solution added to calibrants (g, mL, etc.)"),
        #              value = 0.000015
        #            ))), 
        #   
        #   column(6,
        #          wellPanel(
        #            numericInput(
        #              "u_mad", 
        #              label = h4("Uncertainty in masses (or volume) of analyte working standard solution added to calibrants(g, mL, etc.)"),
        #              value = 0.000015
        #            )))), 
        # 
        # 
        # fluidRow(style='margin: 0px;',
        # column(12,
        # h3("Sample preparation uncertainties:",style = "color:steelblue"))), 
        # fluidRow(style='margin: 0px;',
        #   column(6,
        #          wellPanel(
        #            numericInput(
        #              "u_mids", 
        #              label = h4("Uncertainty in masses (or volume) of internal standard solution spiked into samples (g, mL, etc.)"),
        #              value = 0.000015
        #            ))), 
        #   
        #   column(6,
        #          wellPanel(        
        #            numericInput(
        #              "u_mdi", 
        #              label = h4("Uncertainty in the masses (or volume) of substance sampled for measurement (serum, food, blood, etc.)"),
        #              value = 0.000015
        #            )))),
        
        
        # flu outmulidRow(style='margin: 0px;',
        #          column(12,
        # h3("Upload  Data Tables:",style = "color:steelblue"))),
        # br(),
        # 
        # fluidRow(style='margin: 0px;',
        #  column(12,
        #   tabsetPanel(
        #   tabPanel(
        #     title=h4("Upload Calibration Data Tables:"),
        #     br(),
        #     column(12, downloadButton('downloadCalTable', 'Download  Data Table Template')),
        #     
        #     column(6, 
        #            em(h5("X data:")),
        #            em(h5("mid: Quantity (mass or volume) of internal standard added to calibrant(Only for internal standard use)")),
        #            em(h5("mad: Quantity (mass or volume) of analyte working standard solution in calibrant")),
        #            em(h5("Analyte working standard solution ID # (e.g. 1, 2)"))),
        #     
        #     column(6,
        #            em(h5("Y data:")),
        #            em(h5("raci: Analyte: Internal Standard Peak Areas Ratio for calibrant repeat measurement i"))),
        #     
        #     fileInput(
        #       inputId = "calfile",
        #       label = "",
        #       multiple = TRUE,
        #       accept = c("text/csv",".xlsx",
        #                  "text/comma-separated-values,text/plain",
        #                  ".csv"),
        #       placeholder = "Use Ctrl key to choose files"
        #     )),
        #   
        #   
        #   tabPanel(
        #     title=h4("Upload Sample Data Tables:"),
        #     br(),
        #     column(12, downloadButton('downloadSamTable', 'Download  Data Table Template')),
        #     
        #     
        #     column(6,
        #            em(h5("X data:")),
        #            em(h5("mids: Quantity (mass or volume) of internal standard in sample(Only for internal standard use)")),
        #            em(h5("Quantity (mass or volume) of analyzed sample substance (e.g., serum sample)"))),
        #     
        #     column(6,
        #            em(h5("Y data:")),
        #            em(h5("rasi:Analyte: Internal Standard Peak Areas Ratio for sample repeat measurement i"))),
        #     
        #     fileInput(
        #       inputId = "samplefile",
        #       label = "",
        #       multiple = TRUE,
        #       placeholder = "Use Ctrl key to choose files",
        #       accept = c("text/csv",".xlsx",
        #                  "text/comma-separated-values,text/plain",
        #                  ".csv"))
        #   )))),
        # 
        # hr(),
        # 
        # fluidRow(style='margin: 0px;',
        #          column(12,
        # h3("Model Settings",style = "color:steelblue"))), 
        # 
        # fluidRow(style='margin: 0px;',
        #   column(6,
        #          numericInput(
        #            "coverage",
        #            label = 
        #              em("Coverage probability",
        #                 tags$style(type = "text/css", "#covP_q {vertical-align: top;}"),
        #                 bsButton("covP_q", 
        #                          label = "", 
        #                          icon = icon("question"), 
        #                          style = "info", 
        #                          size = "extra-small")),
        #            value = .95,
        #            step=.05, 
        #            min = 0.01, 
        #            max = 0.99
        #          ),
        #          
        #          bsTooltip(
        #            id = "covP_q",
        #            title = "Positive number between 0 and 1. Coverage probability of the calculated confidence interval; default is 0.95",
        #            placement = "right", 
        #            trigger ="focus")
        #   ), 
        #   
        #   column(6,
        #          numericInput("digit",label ="Number of decimal places for results",value = 2)
        #   )
        # ),
        # 
        # fluidRow(style='margin: 0px;',
        #   column(6,
        #          numericInput(
        #            "niters", 
        #            label = 
        #              em("Total number of iterations",
        #                 tags$style(type = "text/css", "#numIters_q {vertical-align: top;}"),
        #                 bsButton("numIters_q", 
        #                          label = "", 
        #                          icon = icon("question"), 
        #                          style = "info", 
        #                          size = "extra-small")), 
        #            value = "10000",
        #            step=1000,
        #            min=1000),
        #          bsTooltip(
        #            id = "numIters_q",
        #            title = "This is the number of iterations of the Markov Chain Monte Carlo. This value should not be changed unless prompted by the app",
        #            placement = "right", 
        #            trigger ="focus")
        #   ), 
        #   
        #   
        #   column(6,
        #          numericInput(
        #            "nburnin", 
        #            label = 
        #              em(
        #                "Length of burn in",
        #                tags$style(type = "text/css", "#numBurn_q {vertical-align: top;}"),
        #                bsButton("numBurn_q", 
        #                         label = "", 
        #                         icon = icon("question"), 
        #                         style = "info", 
        #                         size = "extra-small")),
        #            value = "5000",
        #            step=100,
        #            min=500),
        #          bsTooltip(
        #            id = "numBurn_q",
        #            title = "This value should not be changed unless prompted by the app. This is the number of initial iterations of the Markov Chains that be will discarded.",
        #            placement = "right", 
        #            trigger ="foucs"))), 
        # 
        # # fluidRow(style='margin: 0px;',
        # #          column(12,
        # #                 checkboxInput("exp", label = strong("Run the experiment design module and specify the parameters",style = "color:seagreen"), value = F)
        # #          )),
        #            uiOutput("expui"),
        #            uiOutput("doe12"),
        #           uiOutput("doeinput"),
        #           
        # 
        # br(),
        # 
        # ########Fit the model ui#######
        # 
        # ##################################################################
        # ### MCMC Results #################################################
        # ##################################################################
        # mainPanel(
        #   fluidRow(style='margin: 0px;',
        #     actionButton("go","Fit the model", icon("paperr-plane"), 
        #                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4", width = '30%')
        #   ),
        #   fluidRow(style='margin: 0px;',
        #     helpText("Please check your data first and then fit the model")
        #   )
        # ),
        # 
        # fluidRow(style='margin: 0px;',
        #   
        #   column(6,
        #          tableOutput("calnum")
        #          
        #   ),
        #   column(6,
        #          tableOutput("samplenum2")
        #   )
        # )
        # ,
        # 
        # fluidRow(style='margin: 0px;',
        #   column(6,
        #          tableOutput("caltables")
        #   ),
        #   column(6,
        #          tableOutput("sampletables")
        #   )
        # )
        # ,
        # hr(),
        # 
        # 
        # conditionalPanel(
        #   
        #   id = "results",
        #   condition="input.go >= 1",
        #   
        #    # use_waiter(),
        #   
        #   # p("The posterior mean is: ",withSpinner(textOutput("mu_est",inline=T), type = 8,color = "seagreen")),
        #   h2("Model Results",style = "color:steelblue"),
        #   
        #   conditionalPanel(
        #     # condition = "input.exp == True",
        #     
        #     withSpinner(tableOutput("opt"),type = 8,color = "seagreen"),
        #     tableOutput("results2"),
        #     fluidRow(style='margin: 0px;',
        #    uiOutput("doem"),
        #    uiOutput("err"),
        #    uiOutput("doeout"),
        #    uiOutput("doee")
        #     )
        #   ),
        #   
        #    
        #    uiOutput("muest"),
          
          # fluidRow(
          #   column(8,
          #           p("The posterior mean is: ",withSpinner(textOutput("mu_est",inline=T), type = 8,color = "seagreen")),
          #          p("The standard uncertainty is: ",textOutput("mu_se",inline=T)),
          #          p("The posterior median is: ",textOutput("mu_median",inline=T)),
          #          p("The ",textOutput("coverageProbabilityPercentBayes",inline=T),
          #            " credible interval ranges from: ",textOutput("mu_quant",inline=T))
          #   )),
          
          # hr(),
         
          # uiOutput("plots"),
          # hr(),
          # uiOutput("out1"),
          
        
          # downloadButton("report", "Generate PDF report"),
         
          #downloadButton('downloadbayesout', 'Download inputs and estimates(csv file)'),
          #downloadButton('downloaddoeinput', 'Download DoE inputs (csv file)'),
          
          # plotOutput("mu_post_plot", width = "auto", height = "600px"),
         # downloadButton('download_mu_post_plot', 'Download plot'),
          
          # br(),
          # br(),
          
          
         #  # plotOutput("mu_trace_plot", width = "auto", height = "600px"),
         # # downloadButton('download_mu_trace_plot', 'Download plot')
         #  
         #  )),
      
      
        
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
        
        # fluidRow(style='margin: 0px;',
        #          column(6,
        #                 
        #                 wellPanel(
        #                   h4("Negative control (NC) heterogeneity uncertainty"),
        #                   numericInput("NCsigv",label =NULL, 
        #                                value="0.017"))),
        #          
        #          column(6,
        #                 wellPanel(
        #                   h4( "Test chemical heterogeneity uncertainty"),
        #                   numericInput("TCsigv",label =NULL,
        #                                value="0.031")))),
        # 
        # fluidRow(style='margin: 0px;',
        #          column(6,
        #                 wellPanel(
        #                   h4("Repeatability uncertainty for NC         "),
        #                   numericInput(
        #                     "repsigNCv",
        #                     label = NULL,
        #                     value = 0.035)
        #                 )),
        #          column(6,
        #                 wellPanel(
        #                   h4("Repeatability uncertainty for TC"),
        #                   numericInput(
        #                     "repsigTCv",
        #                     label = NULL,
        #                     value = 0.0029)
        #                 ))),
        # 
        # fluidRow(style='margin: 0px;',
        #          column(6,
        #                 
        #                 wellPanel(
        #                   h4("Number of reps of NC on the 96 well plate"),
        #                   numericInput("nrepNCv",label =NULL, 
        #                                value="3"))),
        #          
        #          column(6,
        #                 wellPanel(
        #                   h4( "Number of reps of TC on the 96 well plate"),
        #                   numericInput("nrepTCv",label =NULL,
        #                                value="3")))),
        # 
        # 
        # 
        # fluidRow(style='margin: 0px;',
        #          column(12,
        #                 wellPanel(
        #                   h4( "Number of wells already in use (for example PC)"),
        #                   numericInput("nminv",label =NULL,
        #                                value="8",min = 1, max = 20, step = 1
        #              )
        #               )
        #             )
        #          
        # ),
    
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
            #uiOutput("singletc"),
            #uiOutput("outname"),
            uiOutput("outmul"),             
           
            # p("totn: Total number of free wells on 24 well plate"),
            # p("dif: Limit of detection"),
            # p("NC: Number of wells assigned to Negative control on 24 well plate"),
            # p("TC: Number of wells assigned to Test chemical on 24 well plate"),
           
            #tableOutput("toPrint"),
            
            uiOutput("doeoutput")

                  )
         )
          
      
        )
      
        ),
      #####analysis module
   #   "Analysis ",
      # tabPanel(
      #   
      #   h4("Background"),
      #   em(h2("Quick Start: Analysis",style = "color:steelblue")),
      #   br(),
      #   em(h3("Data Entry")),
      #   hr(),
      #   h4("Enter known concentrations of analyte in working standard solutions used to prepare calibrants. Multiple working standard concentrations can be input, values separated by commas. An uncertainty estimate of each concetration is required."), 
      #   hr(),
      #   img(src='Background_IMG1.png', align = "center", height = 250, width = 800),
      #   hr(),
      #   h4("Enter uncertainties associated with adding analyte and internal standard quantities to calibrants."),
      #   hr(),
      #   h4("Enter uncertainty of internal standard quantities added to samples. Enter uncertainty of sample substance aliquot (e.g., serum matrix sample)"),
      #   hr(),
      #   img(src='Background_IMG2.png', align = "center", height = 400, width = 750),
      #   hr(),
      #   em(h4("Data Table Upload")),
      #   hr(),
      #   br(),
      #   h4("Experiments that generate multiple calibration curves to determine a single result are supported. For example:"),
      #   h4("- measurements conducted over several days with separate calibrations"),
      #   h4("- employment of multiple different techniques using internal standards"),
      #   br(),
      #   em(h4("Multiple data files can be uploaded simultaneously. Upload calibration and corresponding sample tables in the same order.")),
      #   br(),
      #   img(src='Background_IMG3.png', align = "center", height = 350, width = 825),
      #   hr(),
      #   h4("Calibration Data Tables"),
      #   
      #   
      #   
      #   
      #   em(h3("Model Settings")),
      #   hr(),
      #   h4("1) Specify coverage interval of estimated result. Default is 95 %."),
      #   h4("2) Indicate destired number of decimal places printed for results"),
      #   h4("3) Total number of iterations should be adjusted if app indicates that more iterations are needed"),
      #   h4("4) Length of burn in should only be increased if app indicates that model has not converged. Do not decrease this value."),
      #   br(),
      #   img(src='Background_IMG4.png', align = "center", height = 350, width = 825),
      #   br(),
      #   hr(style="border-color: black;"), 
      #   
      # ),
      # 
      # 
      # 
      # tabPanel(
      #   h4("Data entry"),
      #   
      #   fluidRow(style='margin: 0px;',
      #            column(12,
      #                   wellPanel(
      #                     h3(strong("Choose a method of calibration",style = "color:seagreen")),
      #                     
      #                     radioButtons("choice1",
      #                                  label=NULL,
      #                        choices=list(
      #                                    "Linear Regression"=1,"Response Factor"=2),
      #                                  inline = T
      #                     ),
      #                     
      #                     radioButtons("standard1",
      #                                  label=NULL,
      #                                  choices=list(
      #                                    "Internal Standard"=1,"External Standard"=2),
      #                                  inline = T
      #                     )
      #                     ))
      #            
      #   ),
      #   
      #   uiOutput("standard_ui1"),
      #   
      #   # fluidRow(style='margin: 0px;',
      #   #          column(12,
      #   #                 h3("Calibration standard inputs:",style = "color:steelblue")
      #   #          )),
      #   # 
      #   # 
      #   # fluidRow(style='margin: 0px;',
      #   #          column(6,
      #   #                 wellPanel(
      #   #                   textInput("wadm1",label = h4("Analyte working standard solution concentrations; Comma-separated entries for multiple inputs"), value="94.2, 95.5, 94.5",placeholder = "e.g. '94.2, 95.5, 94.5'"))),
      #   #          
      #   #          column(6,
      #   #                 wellPanel(
      #   #                   textInput("uwad1",label = h4("Uncertainties in  analyte working standard solution ; Comma-separated entries for multiple inputs"),value="0.427, 0.434, 0.429",placeholder = "e.g. '0.427, 0.434, 0.429'")))),
      #   # 
      #   # fluidRow(style='margin: 0px;',
      #   #          
      #   #          column(6,
      #   #                 wellPanel(
      #   #                   numericInput(
      #   #                     "u_mid1", 
      #   #                     label = h4("Uncertainty in masses (or volume) of internal standard solution added to calibrants (g, mL, etc.)"),
      #   #                     value = 0.000015
      #   #                   ))), 
      #   #          
      #   #          column(6,
      #   #                 wellPanel(
      #   #                   numericInput(
      #   #                     "u_mad1", 
      #   #                     label = h4("Uncertainty in masses (or volume) of analyte working standard solution added to calibrants(g, mL, etc.)"),
      #   #                     value = 0.000015
      #   #                   )))),
      #   # fluidRow(style='margin: 0px;',
      #   #          column(12,
      #   #                 h3("Sample preparation uncertainties:",style = "color:steelblue")
      #   #          )), 
      #   # fluidRow(style='margin: 0px;',
      #   #          column(6,
      #   #                 wellPanel(
      #   #                   numericInput(
      #   #                     "u_mids1", 
      #   #                     label = h4("Uncertainty in masses (or volume) of internal standard solution spiked into samples (g, mL, etc.)"),
      #   #                     value = 0.000015
      #   #                   ))), 
      #   #          
      #   #          column(6,
      #   #                 wellPanel(        
      #   #                   numericInput(
      #   #                     "u_mdi1", 
      #   #                     label = h4("Uncertainty in the masses (or volume) of substance sampled for measurement (serum, food, blood, etc.)"),
      #   #                     value = 0.000015
      #   #                   )))),
      #   # 
      #   
      #   fluidRow(style='margin: 0px;',
      #            column(12,
      #                   h3("Upload  Data Tables:",style = "color:steelblue")
      #            )),
      #   
      #   br(),
      #   fluidRow(style='margin: 0px;',
      #            column(12,
      #                   tabsetPanel(
      #                     tabPanel(
      #                       title=h4("Upload Calibration Data Tables:"),
      #                       br(),
      #                        column(12, 
      #                               downloadButton('downloadCalTable1', 'Download Data Table Template')
      #                        ),
      #                       
      #                       column(6, 
      #                              em(h5("X data:")),
      #                              em(h5("mid: Quantity (mass or volume) of internal standard added to calibrant(Only for internal standard use)")),
      #                              em(h5("mad: Quantity (mass or volume) of analyte working standard solution in calibrant")),
      #                              em(h5("Analyte working standard solution ID # (e.g. 1, 2)"))),
      #                       
      #                       column(6,
      #                              em(h5("Y data:")),
      #                              em(h5("raci: Analyte: Internal Standard Peak Areas Ratio for calibrant repeat measurement i"))),
      #                       
      #                       fileInput(
      #                         inputId = "calfile1",
      #                         label = "",
      #                         multiple = TRUE,
      #                         accept = c("text/csv",".xlsx",
      #                                    "text/comma-separated-values,text/plain",
      #                                    ".csv"),
      #                         placeholder = "Use Ctrl key to choose files"
      #                       )),
      #                     
      #                     
      #                     tabPanel(
      #                       title=h4("Upload Sample Data Tables:"),
      #                       br(),
      #                      column(12, downloadButton('downloadSamTable1', 'Download Data Table Template')),
      #                       
      #                       
      #                       column(6,
      #                              em(h5("X data:")),
      #                              em(h5("mids: Quantity (mass or volume) of internal standard in sample(Only for internal standard use)")),
      #                              em(h5("Quantity (mass or volume) of analyzed sample substance (e.g., serum sample)"))),
      #                       
      #                       column(6,
      #                              em(h5("Y data:")),
      #                              em(h5("rasi:Analyte: Internal Standard Peak Areas Ratio for sample repeat measurement i"))),
      #                       
      #                       fileInput(
      #                         inputId = "samplefile1",
      #                         label = "",
      #                         multiple = TRUE,
      #                         placeholder = "Use Ctrl key to choose files",
      #                         accept = c("text/csv",".xlsx",
      #                                    "text/comma-separated-values,text/plain",
      #                                    ".csv"))
      #                     )))),
      #   
      #   fluidRow(style='margin: 0px;',
      #            column(12,
      #                   h3("Model Settings",style = "color:steelblue")
      #            )), 
      #   
      #   fluidRow(style='margin: 0px;',
      #            column(6,
      #                   numericInput(
      #                     "coverage1",
      #                     label = 
      #                       em("Coverage probability",
      #                          tags$style(type = "text/css", "#covP_q {vertical-align: top;}"),
      #                          bsButton("covP_q", 
      #                                   label = "", 
      #                                   icon = icon("question"), 
      #                                   style = "info", 
      #                                   size = "extra-small")),
      #                     value = .95,
      #                     step=.05, 
      #                     min = 0.01, 
      #                     max = 0.99
      #                   ),
      #                   
      #                   bsTooltip(
      #                     id = "covP_q",
      #                     title = "Positive number between 0 and 1. Coverage probability of the calculated confidence interval; default is 0.95",
      #                     placement = "right", 
      #                     trigger ="focus")
      #            ), 
      #            
      #            column(6,
      #                   numericInput("digit1",label ="Number of decimal places for results",value = 2)
      #            )
      #   ),
      #   
      #   fluidRow(style='margin: 0px;',
      #            column(6,
      #                   numericInput(
      #                     "niters1", 
      #                     label = 
      #                       em("Total number of iterations",
      #                          tags$style(type = "text/css", "#numIters_q {vertical-align: top;}"),
      #                          bsButton("numIters_q", 
      #                                   label = "", 
      #                                   icon = icon("question"), 
      #                                   style = "info", 
      #                                   size = "extra-small")), 
      #                     value = "10000",
      #                     step=1000,
      #                     min=1000),
      #                   bsTooltip(
      #                     id = "numIters_q",
      #                     title = "This is the number of iterations of the Markov Chain Monte Carlo. This value should not be changed unless prompted by the app",
      #                     placement = "right", 
      #                     trigger ="focus")
      #            ), 
      #            
      #            
      #            column(6,
      #                   numericInput(
      #                     "nburnin1", 
      #                     label = 
      #                       em(
      #                         "Length of burn in",
      #                         tags$style(type = "text/css", "#numBurn_q {vertical-align: top;}"),
      #                         bsButton("numBurn_q", 
      #                                  label = "", 
      #                                  icon = icon("question"), 
      #                                  style = "info", 
      #                                  size = "extra-small")),
      #                     value = "5000",
      #                     step=100,
      #                     min=500),
      #                   bsTooltip(
      #                     id = "numBurn_q",
      #                     title = "This value should not be changed unless prompted by the app. This is the number of initial iterations of the Markov Chains that be will discarded.",
      #                     placement = "right", 
      #                     trigger ="foucs"))), 
      #  
      # 
      #   
      #   
      #   br(),
      #   ########Fit the model ui#######
      #   
      #   ##################################################################
      #   ### MCMC Results #################################################
      #   ##################################################################
      #   mainPanel(
      #     fluidRow(style='margin: 0px;',
      #              actionButton("analysis","Fit the model", icon("paperr-plane"), 
      #                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4", width = '30%')
      #     ),
      #     fluidRow(style='margin: 0px;',
      #              helpText("Please check your data first and then fit the model")
      #     )
      #   ),
      #   
      #   fluidRow(style='margin: 0px;',
      #            
      #            column(6,
      #                   tableOutput("calnum1")
      #                   
      #            ),
      #            column(6,
      #                   tableOutput("samplenum1")
      #            )
      #   )
      #   ,
      # 
      #   fluidRow(style='margin: 0px;',
      #            column(6,
      #                   tableOutput("caltables1")
      #            ),
      #            column(6,
      #                   tableOutput("sampletables1")
      #            )
      #   ),
      #  
      #   
      #   
      #   conditionalPanel(
      #     
      #     id = "results",
      #     condition="input.analysis >= 1",
      #     
      #     # use_waiter(),
      #     
      #     withSpinner(uiOutput("anaui"), type = 8,color = "seagreen"), 
      #     
      #    
      #   uiOutput("plots2"),
      #   
      #   
      #   uiOutput("out2"),
      #   uiOutput("anaui1"),
      #   uiOutput("anaui2")
 
        
  
     
        
     # )
      
      
      #)
      
      
      )
    )
    
)

