options(java.parameters = "-Xss2560k")

library(shiny)
library(shinycssloaders)
#library(rmarkdown)
#library('xlsx')
#source("./Toman2019-ExpDesignAug14.R")
shinyServer(function(input, output,session) {
  
  
  
  #####display uploaded files
  ###caltables###
  observe({
    if (!is.null(input$calfile)) {
      N_tables <<- length(input$calfile[, 1])
      
      upload <- list()
      for (i in 1:N_tables) {
        upload[[i]] <- read.csv(input$calfile$datapath[i])
      }
      
      output$caltables <- renderUI({
        table_output_list <- lapply(1:N_tables, function(i) {
          tableOutput(paste0("table_name", i))
        })
        do.call(tagList, table_output_list)
      })
      
      for (i in 1:N_tables) {
        local({
          my_i <- i
          output[[paste0("table_name", my_i)]] <- renderTable((
            upload[[my_i]]
          ),spacing = "s", striped = T,rownames = T,digits = 5)
        })
      }
    }
  })
  
  
  #####sample tables######
  
  observe({
    if (!is.null(input$samplefile)) {
      N_tables = length(input$samplefile[, 1])
      
      upload <- list()
      for (i in 1:N_tables) {
        upload[[i]] <- read.csv(input$samplefile$datapath[i])
      }
      
      output$sampletables <- renderUI({
        table_output_list <- lapply(1:N_tables, function(i) {
          tableOutput(paste0("table_name2", i))
        })
        do.call(tagList, table_output_list)
      })
      
      for (i in 1:N_tables) {
        local({
          my_i <- i
          output[[paste0("table_name2", my_i)]] <- renderTable((
            upload[[my_i]]
          ),spacing = "s", striped = T,rownames = T,digits = 5)
        })
      }
    }
  })
  
  
  
  
  
  ###number of calibration
  
  
  nfile <- reactive({
    
    inFile <- input$calfile
    if (is.null(input$calfile)){
      return(NULL)
    }else{
      
      nfile<-nrow(input$calfile)
      
    }
  })
  
  
  a1 <- reactive({
    
    inFile <- input$calfile
    if (is.null(input$calfile)){
      return(NULL)
    }else{
      
      nfile<<-nrow(input$calfile)
      tmp<- read.csv(inFile$datapath[1])
      N<<-nrow(tmp)#calbraton number
      NR<<-ncol(tmp)-3# number of replicates for each calibrant
      NWS<-max(tmp$wsol)# #working solutions from  calibrationtable
      cal<<-c(N,NR,NWS)
      a<-t(as.matrix(cal))
      colnames(a)<-c("Calibration Number","Calibration repeat","working solution")
      return (a)
      
    }
  })
  
  
  
  ######number of sample#####
  
  b1 <- reactive({
    inFile <- input$samplefile
    if (is.null(inFile)){
      return(NULL)
    }else{
      
      tmp<- read.csv(inFile$datapath[1])
      M<<-nrow(tmp)
      MR<<-ncol(tmp)-2
      sam<-c(M,MR)
      b<-t(as.matrix(sam))
      colnames(b)<-c("Number of Sample","Sample repeat")
      return (b)
      
      
    }
  })
  
  ###creates the calibration table template
  
  # cal.table = function(){
  #   n.cols= 5 # mid + mad + replicates 
  #   my.table = matrix("", nrow = 1, ncol = n.cols)
  #   # rac.colnames = sapply(1:input$numberOfCalReps, function(x){paste0("rac", x)})
  #   colnames(my.table) = c('mid', 'mad','wsol', 'rac1','rac2')
  #   return(my.table)
  # }
  
  output$downloadCalTable <- downloadHandler(
    filename = function(){
      paste("calibration_input_template","xlsx",sep=".")
    },
    content = function(file) {
      file.copy("calibration_input_template.xlsx",file)
    }
  )
  
  ### This function creates the samples table template
  
  # sam.table = function(){
  #   n.cols= 4 
  #   my.table = matrix("", nrow = 1, ncol = n.cols)
  #   # ras.colnames = sapply(1:input$numberOfSampleReps, function(x){paste0("ras", x)})
  #   colnames(my.table) = c('mids', 'mdi', 'ras1','ras2')
  #   return(my.table)
  # }
  
  output$downloadSamTable <- downloadHandler(
    filename = function(){
      paste("sample_input_template","xlsx",sep=".")
    },
    content = function(file) {
      file.copy("sample_input_template.xlsx",file)
    }
  )
  
  ####varibles used 
  
  vecwac <<- reactive({
    
    if (!is.null(input$calfile))
    {
      
      vec=list()
      for(i in 1: nfile){
        vec[[i]]<-nrow(read.csv(input$calfile$datapath[i]))
      }
      return(vec)
    }
  })
  
  vecsample <- reactive({
    
    if (!is.null(input$samplefile))
    {
      
      vec=list()
      for(i in 1: nfile){
        vec[[i]]<-nrow(read.csv(input$samplefile$datapath[i]))
      }
      return(vec)
    }
  })
  
  ####work solution of Calibration
  
  ####output tables
  
  
  output$calnum <- renderTable(a1(),digits = 0)
  output$samplenum2 <- renderTable(b1(),digits = 0)
  
  #### Doe Input
  # observe({
  #   if (!is.null(input$calfile)) {
  #    
  output$expui<-renderUI(
    
    fluidRow(style='margin: 0px;',
             column(12,
                    checkboxInput("exp", label = strong("Run the experiment design module and specify the parameters",style = "color:seagreen"), value = F)
             ))
  )
  #   }
  # })
  ######standard options
  
  standardui<-reactive(
    
    input$standard
  )
  
  output$standard_ui<-renderUI({
    
    if(standardui()==1){
      tagList(
        fluidRow(style='margin: 0px;',
                 column(12,
                        h3("Calibration standard inputs:",style = "color:steelblue"))),
        
        
        fluidRow(style='margin: 0px;',
                 column(6,
                        wellPanel(
                          textInput("wadm",label =h4("Analyte working standard solution concentrations; Comma-separated entries for multiple inputs"), value="94.2, 95.5, 94.5",placeholder = "e.g. '94.2, 95.5, 94.5'"))),
                 
                 column(6,
                        wellPanel(
                          textInput("uwad",label = h4("Uncertainties in working standard solution; Comma-separated entries for multiple inputs"),value="0.427, 0.434, 0.429",placeholder = "e.g. '0.427, 0.434, 0.429'")))),
        
        fluidRow(style='margin: 0px;',
                 
                 column(6,
                        wellPanel(
                          numericInput(
                            "u_mid", 
                            label = h4("Uncertainty in masses (or volume) of internal standard solution added to calibrants (g, mL, etc.)"),
                            value = 0.000015
                          ))), 
                 
                 column(6,
                        wellPanel(
                          numericInput(
                            "u_mad", 
                            label = h4("Uncertainty in masses (or volume) of analyte working standard solution added to calibrants(g, mL, etc.)"),
                            value = 0.000015
                          )))), 
        
        
        fluidRow(style='margin: 0px;',
                 column(12,
                        h3("Sample preparation uncertainties:",style = "color:steelblue"))), 
        fluidRow(style='margin: 0px;',
                 column(6,
                        wellPanel(
                          numericInput(
                            "u_mids", 
                            label = h4("Uncertainty in masses (or volume) of internal standard solution spiked into samples (g, mL, etc.)"),
                            value = 0.000015
                          ))), 
                 
                 column(6,
                        wellPanel(        
                          numericInput(
                            "u_mdi", 
                            label = h4("Uncertainty in the masses (or volume) of substance sampled for measurement (serum, food, blood, etc.)"),
                            value = 0.000015
                          ))))
      )
    }else{
      
      tagList(
        fluidRow(style='margin: 0px;',
                 column(12,
                        h3("Calibration standard inputs:",style = "color:steelblue"))),
        
        
        fluidRow(style='margin: 0px;',
                 column(6,
                        wellPanel(
                          textInput("wadm",label =h4("Analyte working standard solution concentrations; Comma-separated entries for multiple inputs"), value="94.2, 95.5, 94.5",placeholder = "e.g. '94.2, 95.5, 94.5'"))),
                 
                 column(6,
                        wellPanel(
                          textInput("uwad",label = h4("Uncertainties in working standard solution; Comma-separated entries for multiple inputs"),value="0.427, 0.434, 0.429",placeholder = "e.g. '0.427, 0.434, 0.429'")))),
        
        fluidRow(style='margin: 0px;',
                 
                 # column(6,
                 #        wellPanel(
                 #          numericInput(
                 #            "u_mid", 
                 #            label = h4("Uncertainty in masses (or volume) of internal standard solution added to calibrants (g, mL, etc.)"),
                 #            value = 0.000015
                 #          ))), 
                 
                 column(12,
                        wellPanel(
                          numericInput(
                            "u_mad", 
                            label = h4("Uncertainty in masses (or volume) of analyte working standard solution added to calibrants(g, mL, etc.)"),
                            value = 0.000015
                          )))), 
        
        
        fluidRow(style='margin: 0px;',
                 column(12,
                        h3("Sample preparation uncertainties:",style = "color:steelblue"))), 
        fluidRow(style='margin: 0px;',
                 # column(6,
                 #        wellPanel(
                 #          numericInput(
                 #            "u_mids",
                 #            label = NULL,
                 #            value = 1
                 #          ))),
                 
                 column(12,
                        wellPanel(        
                          numericInput(
                            "u_mdi", 
                            label = h4("Uncertainty in the masses (or volume) of substance sampled for measurement (serum, food, blood, etc.)"),
                            value = 0.000015
                          ))))
      )
    }
    
  })
  
  # choicea<-reactive (
  #   
  #   input$chocie
  # )
  # observeEvent(input$exp,{
  choicea<-reactive(
    
    input$choice
  )
  
  observe({
    if (!is.null(input$calfile)) {
      
      output$doeinput <- renderUI({
        
        if(input$exp){
          
          if(choicea()==1){
            tagList(
              fluidRow(style='margin: 0px;',
                       column(6,
                              wellPanel(
                                ("Total number of available samples"),
                                numericInput("doetotn",label =NULL, 
                                             value="40"))),
                       
                       column(6,
                              wellPanel(
                                ("Target maximum relative (%) standard uncertainty"),
                                numericInput("doestd",label =NULL, 
                                             value="2.5")))),
              
              
              fluidRow(style='margin: 0px;',
                       
                       column(12,
                              wellPanel(
                                ("Expected standard uncertainty of the concentration ratios of A:I in the calibration experiment; (ux)"),
                                fluidRow(
                                  column(6,
                                         numericInput(
                                           "vxl",
                                           label = h5("Minimum:"),
                                           value = 0)),
                                  column(6,
                                         numericInput(
                                           "vxu",
                                           label = h5("Maximum:"),
                                           value = 0)))
                              )))
              
            )
            
          } else{
            
            tagList(
              
              fluidRow(style='margin: 0px;',
                       column(6,
                              wellPanel(
                                ("Total number of available samples"),
                                numericInput("doetotn1",label =NULL, 
                                             value="40"))),
                       
                       column(6,
                              wellPanel(
                                ("Target maximum relative (%) standard uncertainty"),
                                numericInput("doestd1",label =NULL, 
                                             value="2.5")))),
              
              
              fluidRow(style='margin: 0px;',
                       
                       column(12,
                              wellPanel(
                                ("Expected standard uncertainty of the concentration ratios of A:I in the calibration experiment; (ux)"),
                                fluidRow(
                                  column(6,
                                         numericInput(
                                           "vxl1",
                                           label = h5("Minimum:"),
                                           value = 0)),
                                  column(6,
                                         numericInput(
                                           "vxh1",
                                           label = h5("Maximum:"),
                                           value = 0)))
                              )))
              
            )
            
          }
        }
      })  
      
    }
  })
  
  
  
  observe({
    if (!is.null(input$calfile)) {
      
      output$doem <- renderUI({
        if(input$exp){
          h4("The Experiment Design Results:",style = "color:seagreen")
        }
      })
      output$doee <- renderUI({
        if(input$exp){
          h4("The Posterior Estimates:",style = "color:seagreen")
        }
      })
    }
  })
  
  
  
  doeoption<-reactive({
    
    input$doeoption
  })
  
  output$doeoption1 <- renderUI({
    
    if(doeoption()==1){
      
      tagList(
        
        fluidRow(style='margin: 0px;',
                 column(6,
                        
                        wellPanel(
                          h4("Negative control (NC) heterogeneity uncertainty"),
                          numericInput("NCsigv",label =NULL, 
                                       value="0.017"))),
                 
                 column(6,
                        wellPanel(
                          h4( "Test chemical heterogeneity uncertainty"),
                          numericInput("TCsigv",label =NULL,
                                       value="0.031")))),
        
        fluidRow(style='margin: 0px;',
                 column(6,
                        wellPanel(
                          h4("Repeatability uncertainty for NC         "),
                          numericInput(
                            "repsigNCv",
                            label = NULL,
                            value = 0.035)
                        )),
                 column(6,
                        wellPanel(
                          h4("Repeatability uncertainty for TC"),
                          numericInput(
                            "repsigTCv",
                            label = NULL,
                            value = 0.0029)
                        ))),
       
        fluidRow(style='margin: 0px;',
            column(6,
               
               wellPanel(
                 h4("Number of reps of NC on the 96 well plate"),
                 numericInput("nrepNCv",label =NULL, 
                              value="3"))),
        
        column(6,
               wellPanel(
                 h4( "Number of reps of TC on the 96 well plate"),
                 numericInput("nrepTCv",label =NULL,
                              value="3")))),
      
      
        
        fluidRow(style='margin: 0px;',
                 column(6,
                 wellPanel(
                   h4( "Number of wells already in use (for example PC)"),
                   numericInput("nminv",label =NULL,
                                value="8")
                 )
                 ) ,
                 
                 column(6,
                        wellPanel(
                          h4("Alpha level"),
                          numericInput("alpha",label =NULL, 
                                       value="0.05")))  
        )
        
      )
    }else{
      
      tagList(
        
        fluidRow(style='margin: 0px;',
                 column(6,
                        
                        wellPanel(
                          h4("Negative control (NC) heterogeneity uncertainty"),
                          numericInput("NCsigv",label =NULL, 
                                       value="0.075"))),
                 
                 column(6,
                        wellPanel(
                          h4( "Test chemical heterogeneity uncertainty"),
                          textInput("TCsigv",label =NULL,
                                       value="0.068,0.068,0.27,0.27",placeholder = "e.g. '0.068,0.068,0.27,0.27'")))),
        fluidRow(style='margin: 0px;',
                 column(6,
                        wellPanel(
                          h4("Repeatability uncertainty for NC         "),
                          numericInput(
                            "repsigNCv",
                            label = NULL,
                            value = 0.03)
                        )),
                 column(6,
                        wellPanel(
                          h4("Repeatability uncertainty for TC"),
                          textInput(
                            "repsigTCv",
                            label = NULL,
                            value = "0.03,0.03,0.03,0.03",placeholder = "e.g. '0.03,0.03,0.03,0.03'")
                        ))),
        
        fluidRow(style='margin: 0px;',
                 column(6,
                        
                        wellPanel(
                          h4("Number of reps of NC on the 96 well plate"),
                          numericInput("nrepNCv",label =NULL, 
                                       value="3"))),
                 
                 column(6,
                        wellPanel(
                          h4( "Number of reps of TC on the 96 well plate"),
                          textInput("nrepTCv",label =NULL,
                                       value="3,3,3,3",placeholder = "e.g. '3,3,3,3'")))),
        
        
        
        fluidRow(style='margin: 0px;',
                 column(6,
                        wellPanel(
                          h4( "Number of wells already in use (for example PC)"),
                          numericInput("nminv",label =NULL,
                                       value="13")
                        )
                 ) ,
                 column(6,
                        
                        wellPanel(
                          h4("Alpha level"),
                          numericInput("alpha",label =NULL, 
                                       value="0.05")))   
                 
        )
      )
    }
  })
  
  
  observe({
    if (!is.null(input$calfile)) {
      output$doe12 <- renderUI({
        if(input$exp){
          
          if(N_tables==2){
            
            fluidRow(style='margin: 0px;',
                     column(6,
                            radioButtons("doe1",
                                         label="Output the experimental design for:",
                                         choices=list(
                                           "Datasets 1"=1,"Datasets 2"=2),
                                         inline = T
                            )
                     )
            )
          }
        }
      })
    }
  })
  output$doeenter <- renderUI({
    if(input$enter){
      fluidRow(style='margin: 0px;',
               column(12,
                      h3("Model Results",style = "color:steelblue"))
      )
    }
  })
  # 
  ##################################################################
  ##################################################################
  ### Run Bayesian analysis ########################################
  ##################################################################
  ##################################################################
  
  
  
  outbayes<- eventReactive(input$go,{
    
    NWS1 <- reactive({
      
      inFile <- input$calfile
      if (is.null(input$calfile)){
        return(NULL)
      }else{
        
        nfile<<-nrow(input$calfile)
        tmp<- read.csv(inFile$datapath[1])
        NWS1<-max(tmp$wsol)# #working solutions from  calibrationtable
        return(NWS1)
        
      }
    })
    
    validate(
      need(is.numeric(input$coverage) && input$coverage<=1 && input$coverage>=0, 'Error: Invalid coverage probability, must be numeric between 0 and 1!'),
      # need(input$calfile != "", 'Error: Please upload the calibration data table!'),
      # need(input$samplefile != "", 'Error: Please upload the sample data table!'),
      need((NWS1() != length(as.numeric(unlist(strsplit(gsub("\\s", "", input$wadm),","))))) != TRUE,
           "Error: Incorrect amount of working solution input!"),
      need((NWS1() != length(as.numeric(unlist(strsplit(gsub("\\s", "", input$uwad),","))))) != TRUE,
           "Error: Incorrect amount of uncertainties input in working  solution!")
      #
    )
    
    standard<-reactive(
      input$standard
    )
    choice<-reactive(
      input$choice
    )
    if (choice()==1){
      ###NS==1
      if(N_tables==1){
        
        caldata<-reactive({
          
          if (is.null(input$calfile))
            return()
          else
          {
            csv=list()
            csv=read.csv(input$calfile$datapath[1])
            return(csv)   
          }
          
        })
        
        ####merge sample data talbes
        
        sampledata<-reactive({
          
          if (is.null(input$samplefile))
            return()
          else
          {
            
            csv=list()
            csv=read.csv(input$samplefile$datapath[1])
            return(csv)
          }
          
        })
        
        
        wadm<<-reactive(
          return(append(as.numeric(unlist(strsplit(gsub("\\s", "", input$wadm),","))),1))
          
        )
        
        uwad<<-reactive(
          return(append(as.numeric(unlist(strsplit(gsub("\\s", "", input$uwad),","))),1))
          
        )
        
        ####
        
        
        NWS <- reactive({
          
          inFile <- input$calfile
          if (is.null(input$calfile)){
            return(NULL)
          }else{
            
            tmp<- read.csv(inFile$datapath[1])
            
            return(max(tmp$wsol))# #working solutions from  calibrationtable
            
            
          }
        })
        
        
        umids<-reactive({
          
          if(standard()==1){
            return(c(rep(input$u_mid,nfile)))
          }else{
            return(c(rep(1,nfile)))
            
          }
          
        })
        
        umads<-reactive(
          
          return(c(rep(input$u_mad,nfile)))
          
        )
        
        
        ####
        
        NS<-reactive({
          
          if (is.null(input$calfile))
            return()
          else
          {
            NS<-nrow(input$calfile)
            return(NS)
            
          }
          
        })
        
        
        
        NT<-reactive(
          
          return(N*NR)
          
        )
        
        
        
        ####standard
        
        midsm<-reactive(
          if (standard()==1){
            caldata()$mid
          }else{
            
            caldata()$mad/caldata()$mad
            
          }
        )
        
        madsm<-reactive(
          caldata()$mad
        )
        
        wsol<-reactive(
          caldata()$wsol
        )
        
        
        rac<- reactive({
          
          if (is.null(input$calfile))
            return()
          else
          {
            nfile<-nrow(input$calfile)
            csv=list()
            
            for(i in 1: nfile)
            {
              csv[[i]]=read.csv(input$calfile$datapath[i])
              csv[[i]][1:3]<-NULL
            }
            
            return(unlist(csv)) # rbind the csv
          }
          
        })
        
        
        ras<- reactive({
          
          if (is.null(input$samplefile))
            return()
          else
          {
            nfile<-nrow(input$samplefile)
            
            csv=list()
            
            for(i in 1: nfile)
            {
              csv[[i]]=read.csv(input$samplefile$datapath[i])
              csv[[i]][1:2]<-NULL
            }
            
            return(unlist(csv)) # rbind the csv
          }
          
        })
        
        
        mdi<-reactive(
          
          sampledata()$mdi
          
        )
        
        
        
        midsi<-reactive(
          
          if(standard()==1){
            sampledata()$mids
          }else{
            sampledata()$mdi/sampledata()$mdi
          }
          
        )
        
        MT<-reactive(
          
          return(M*MR)
          
        )
        
        
        
        sol<-reactive({
          rep(1:N,NR)
        })
        
        sampl<-reactive({
          rep(1:M,NR)
          
        })   
        
        
        #####
        
        linedata = list(
          midsm=midsm(),
          madsm=madsm(),
          wsol=wsol(),
          rac=rac(),
          midsi=midsi(),
          mdi=mdi(),
          ras=ras(),
          umids=umids(),
          umads=umads(),
          wadm= wadm(),
          uwad=uwad(),
          NWS=NWS()+1,
          N=N,
          M=M,
          MT=MT(),
          NT=NT(),
          sol=sol(),
          NR=NR,
          sampl=sampl()
        )
        
        
        # 
        require(R2OpenBUGS)
        ##################################################################
        ##################################################################
        ### Define the model
        ##################################################################
        ##################################################################
        
        
        linemodel<-function(){#calculate known mass ratios wac for N calibrants.
          
          midsprec<-1/(umids*umids)
          madsprec<-1/(umads*umads)
          
          for(i in 1:NWS){
            wadprec[i]<-1/(uwad[i]*uwad[i])
            wad[i]~dnorm(wadm[i],wadprec[i])
          }
          
          for(i in 1:N){mids[i]~dnorm(midsm[i],midsprec)
            mads[i]~dnorm(madsm[i],madsprec)
          }
          for(i in 1:N){wac[i]<-wad[wsol[i]]*mads[i]/mids[i]}  
          
          #######
          #calibration equation
          
          a~dnorm(0,1.0E-5)
          b~dnorm(0,1.0E-5)
          
          xins~dnorm(0,0.0016)%_%I(0.001,)
          chsqns~dgamma(0.5,0.5)
          fitprec<-xins/sqrt(chsqns)
          
          for(i in 1:N){mean[i]<-a+b*wac[i]
          predm[i]~dnorm(mean[i],fitprec)}
          for(i in 1:NT){rac[i]~dnorm(mean[sol[i]],fitprec)
          }
          vyp<-1/sqrt(fitprec)
          ###
          # Compute the mass fraction wd
          a.cut<-cut(a)
          b.cut<-cut(b)
          sigras~dgamma(1.0E-5,1.0E-5)
          wdsig~dgamma(1.0E-3,1.0E-3)
          
          wd~dnorm(0,1.0E-5)
          for(i in 1:M){wdm[i]~dnorm(wd,wdsig)
            mdp[i]~dnorm(mdi[i],madsprec)
            midsip[i]~dnorm(midsi[i],midsprec)
            mult[i]<-wdm[i]*mdp[i]/midsip[i]
            wtop[i]<-mdp[i]/midsip[i]
            rasmean[i]<-a.cut+b.cut*wdm[i]*mdi[i]/midsi[i]
            rasmeanp[i]~dnorm(rasmean[i],fitprec)}
          for(i in 1:MT){ras[i]~dnorm(rasmeanp[sampl[i]],sigras)}
          vsp<-1/sqrt(sigras) ### vs for DOE program
          vip<-sd(mult[])/mean(mult[]) ## vi for DOE program
          xnewp<-mean(mult[]) ## xnew for Doe program
        }
        
        ##################################################################
        ##################################################################
        ### Run OpenBUGS
        ##################################################################
        ##################################################################
        
        withProgress(message = 'Running Bayes', style="old",value = 0, {
          
          
          
          lineinits<-function(){list(sigras=1,wdsig=1,a=0,b=1)}
          
          lineout<-bugs(data=linedata,
                        inits=lineinits,
                        parameters=c("a","b","wd","wac","mean","vyp","vsp","mult","vip","xnewp","predm"),   
                        model.file=linemodel,
                        n.iter = input$niters, n.burnin = input$nburnin, n.thin = 10,n.chains = 1)#,debug=T)    
        })
        
        ######
        attach.bugs(lineout) ## imports the random draws for the parameters
        cl<-NULL
        
        for(i in 1:N){
          for(j in 1:NR){cl<-c(cl,quantile(predm[,i], 0.025))}}
        
        cl<-rep(unique(cl), NR)
        cu<-NULL
        for(i in 1:N){
          for(j in 1:NR){cu<-c(cu,quantile(predm[,i], 0.925))}}
        cu<-rep(unique(cu), NR)
        
        
        
        
        outsize<-data.frame(ncal=N, calrep=NR, nsampl=M, samplrep=MR) 
        outrega<-data.frame(mean=lineout$mean$a,sd=lineout$sd$a)
        outregb<-data.frame(mean=lineout$mean$b,sd=lineout$sd$b)
        outwdm<-data.frame(mean=lineout$mean$wdm,sd=lineout$sd$wdm)
        outmean<-data.frame(mean=lineout$mean$mean)
        outwd<-data.frame(mean=lineout$mean$wd,sd=lineout$sd$wd )
        outvyp<-data.frame(mean=lineout$mean$vyp,sd=lineout$sd$vyp)
        outvsp<-data.frame(mean=lineout$mean$vsp,sd=lineout$sd$vsp)
        outvip<-data.frame(mean=lineout$mean$vip,sd=lineout$sd$vip)
        outxnew<-data.frame(mean=lineout$mean$xnewp)
        
        
        
        ########## sets up the data to plot the regression line and calculate the rsq
        
        outwac=lineout$mean$wac
        eta<-lineout$sd$wac/lineout$mean$wac
        lwac<-rep(outwac,NR)
        
        
        ############ calculates the rsq
        ybar<-outmean
        sst<-sum((rac()-mean(rac()))^2)
        sse<-sum((rac()-ybar)^2)
        rsq<-1-sse/sst
        
        
        
        
        bayesres=lineout$sims.list$wd
        
        credible_interval=quantile(bayesres,c((1-input$coverage)/2, (1+input$coverage)/2))
        
        bind <-list(wadm=input$wadm,uwad=input$uwad,
                    u_mid=umids(),u_mad=input$u_mad,
                    u_mids=input$u_mids,u_mdi=input$u_mdi,
                    coverage=input$coverage,Number_iteration=input$niters,Length_burnin=input$nburnin,
                    mean=mean(bayesres),sd=sd(bayesres),median=median(bayesres),
                    credible.interval.L=credible_interval[1],credible.interval.H=credible_interval[2],
                    a=outrega,
                    b=outregb,
                    wd=outwd,
                    vyp=outvyp,
                    vsp=outvsp,
                    vip=outvip,
                    xnew=lineout$mean$xnewp
                    #wac=lineout$mean$wac,rux=lineout$sd$wac/lineout$mean$wac
                    
        )
        
        
        updateTabsetPanel(session = session, inputId = "results",selected = "Fit")
        
        ##### Run DOE Module
        
        
        a<-min(lineout$mean$wac)
        b<-max(lineout$mean$wac)
        
        vyll<-lineout$mean$vyp-2*lineout$sd$vyp
        vyuu<-lineout$mean$vyp+2*lineout$sd$vyp
        
        vyl<-(vyll)^2
        vyu<-(vyuu)^2
        
        
        
        betal<-lineout$mean$b-2*lineout$sd$b
        betau<-lineout$mean$b+2*lineout$sd$b
        
        etall<-min(lineout$sd$wac/lineout$mean$wac) #DOE etal
        etauu<-max(lineout$sd$wac/lineout$mean$wac) #DOE etau
        
        etal<-(etall)^2
        etau<-(etauu)^2
        
        vsll<-lineout$mean$vsp-2*lineout$sd$vsp #DOE vsl
        vsuu<-lineout$mean$vsp+2*lineout$sd$vsp #DOE vsu
        
        vxll<-c("user's input")     #DOE vxl
        vxuu<-c("user's input")    #DOE vxu
        vsl<-(vsll)^2
        vsu<-(vsuu)^2
        
        xnew<-lineout$mean$xnewp 
        
        vill<-lineout$mean$vip-2*lineout$sd$vip #DOE vil
        viuu<-lineout$mean$vip+2*lineout$sd$vip #DOE viu
        
        vil<-(vill)^2
        viu<-(viuu)^2
        ##doe inputs
        expout<-list(
          xnew=xnew,slope_min=betal,slope_max=betau,
          wac_min=a,wac_max=b,
          Uy_min=vyll,Uy_max=vyuu,rux_min=etall,rux_max=etauu,
          Us_min=vsll,Us_max=vsuu,Ui_min=vill,Ui_max=viuu)
        
        if(input$exp){
          
          
          
          vxll<-c("user's input")     #DOE vxl
          vxuu<-c("user's input")    #DOE vxu
          ######### inputs through the shiny interface#########################
          #   if(N_tables==1){ 
          
          
          totn<-input$doetotn
          std<-input$doestd
          vxl<-(input$vxl)^2      #DOE vxl
          vxu<-(input$vxu)^2     #DOE vxu
          
          
          
          
          ## required maximum relative std of the answer
          ###################################################################
          ### R code
          caln<-c(4,6,8,10,12,14,15,16,18,20,22,24,28,30,36,40,44,48)
          
          out<-NULL
          designh1<-expdesign2(caln[1],totn,a,b,vyu,vxu,betau,etau,vsu,xnew,viu)
          out<-rbind(out,c(as.integer(designh1$optI),designh1$optJ,designh1$optr,designh1$optnr,designh1$optstd))
          for(i in 2:18){if(out[i-1,5]>std){
            designh<-expdesign2(caln[i],totn,a,b,vyu,vxu,betau,etau,vsu,xnew,viu)
            out<-rbind(out,c(as.integer(designh$optI),designh$optJ,designh$optr,designh$optnr,designh$optstd)) 
          }else{break}}
          
          outng<-NULL
          outng<-out
          outg<-NULL
          while(dim(out)[1]<18){outg<-out
          totn<-totn-1
          if(totn==0){break}
          
          out<-NULL
          designh1<-expdesign2(caln[1],totn,a,b,vyu,vxu,betau,etau,vsu,xnew,viu)
          out<-rbind(out,c(as.integer(designh1$optI),designh1$optJ,designh1$optr,designh1$optnr,designh1$optstd))
          for(i in 2:18){if(out[i-1,5]>std){
            designh<-expdesign2(caln[i],totn,a,b,vyu,vxu,betau,etau,vsu,xnew,viu)
            out<-rbind(out,c(as.integer(designh$optI),designh$optJ,designh$optr,designh$optnr,designh$optstd)) 
          }else{break}}
          
          dimg<-dim(outg)[1]
          
          outgood<-outg[outg[,5]<std,] ### this contains the experimental designs that satisfy the std requirement
          optI<-outgood[1] # optimal number of calibrants
          optJ<-outgood[2]  # optimal number of replicates
          optr<-outgood[3]  # optimal number of samples in the quantitation experiment
          optnr<-outgood[4] # optimal number of replicates per sample
          optstd<-outgood[5] # optimal expected relative standard deviation of the response
          
          optx<-NULL
          optx[1]<-a
          optIm1<-optI-1
          
          for(i in 1:optIm1){
            optx[i+1]<-a+i*(b-a)/optIm1}}
          
          dimng<-dim(outng)[1]
          
          
          
          #  hide_waiter()
          
          if(outng[dimng,5]>std){
            ################to be printed in shiny#################
            
            #######################################################
            outbest<-outng[18,]
            optIb<-outbest[1]  # number of calibrants
            optJb<-outbest[2]  # number of replicates
            optrb<-outbest[3]  # number of samples in the quantitation experiment
            optnrb<-outbest[4] # number of replicates per sample
            optstdb<-outbest[5] # the smallest expected relative standard deviation of the response given your totn and std
            optxb<-NULL
            optxb[1]<-a
            optIm1b<-optIb-1
            for(i in 1:optIm1b){
              optxb[i+1]<-a+i*(b-a)/optIm1b}
            
            
            #results <- matrix(c(optxb),ncol =length(optxb), nrow = 1)
            optxb<-t(optxb)
            optxb.colnames = sapply(1:length(optxb), function(x){paste0("level", x)})
            colnames(optxb) = c(optxb.colnames)
            
            #results2 <- matrix(c(optIb,optJb,optrb,optnrb),ncol=4, nrow = 1)
            #colnames(results2) <- c("Calibrants","Calibrant replicates","Samples","Sample replicates")
            results2<-list(Calibrants=optIb,Calibrant_replicates=optJb,Samples=optrb,Sample_replicates=optnrb)
            optstdb <- matrix(c(optstdb))
            
            colnames(optstdb) <- c("SExpected Relative (%) Standard Deviation of Result")
            
            output$err<-renderUI({
              h5("Please increase the total number of  observations otherwise the  experimental design is:",style = "color:red")
            })
            # alert<-c("Oops!", "Please increase the total number of  observations otherwise the  possible experimental design is:")
            
            
            # output$Print <- renderTable(optxb,spacing=c( "l"),align="l") ### print in Shiny
            # output$Print2 <- renderTable(results2,spacing=c( "l"),align="l",digits = 0)
            # output$Print3 <- renderTable(results3,spacing=c( "l"),align="l")
            
            
            
            return(list(mcmcout=bayesres,rsq=rsq,outa=outrega,outb=outregb,rac=rac(),result=results2,
                        optxb=optxb,optstd=optstdb,std=std,message=message,
                        expout=expout,
                        wacsd=lineout$sd$wac,expout=expout,waca=a,wacb=b,vyl=vyll,vyu=vyuu,vxl=vxll,vxu=vxuu,
                        betal=betal,betau=betau,etal=etall,etau=etauu,
                        vsl=vsll,vsu=vsuu,xnew=xnew,vil=vill,viu=viuu,
                        outeta=eta,outwac=outwac,outvyp=outvyp,outvsp=outvsp,outvip=outvip,outxnew=outxnew,
                        wac=lwac,linea=lineout$mean$a,lineb=lineout$mean$b,outwd=outwd,cl=cl,cu=cu))
            
            
            
            
            
            
            
            ######################################################
            
          }else{
            
            
            output$err<-renderUI({
              HTML("")
            })
            
            # results <- matrix(c(optx),ncol =length(optx), nrow = 1)
            optx<-t(optx)
            optx.colnames = sapply(1:length(optx), function(x){paste0("level", x)})
            colnames(optx) = c( optx.colnames)
            
            
            # results2 <- matrix(c(optI,optJ,optr,optnr),ncol=4, nrow = 1)
            results2<- list(Calibrants=optI,Calibrant_replicate=optJ,Samples=optr,Sample_replicate=optnr)
            
            optstd<-matrix(c(optstd))
            colnames(optstd)=c("Expected Relative (%) Standard Deviation of Result")
            #optstd <- list(Standard_deviation=optstd)
            
            
            
            # output$Print <- renderTable(results,spacing=c( "l"),align="l") ### print in Shiny
            # output$Print2 <- renderTable(results2,spacing=c( "l"),align="l",digits = 0)
            # output$Print3 <- renderTable(results3,spacing=c( "l"),align="l")
            
            
            return(list(mcmcout=bayesres,rsq=rsq,outa=outrega,outb=outregb,rac=rac(),result=results2,
                        optxb=optx,optstd=optstd,wacsd=lineout$sd$wac,expout=expout,
                        waca=a,wacb=b,vyl=vyll,vyu=vyuu,vxl=vxll,vxu=vxuu,
                        betal=betal,betau=betau,etal=etall,etau=etauu,
                        vsl=vsll,vsu=vsuu,xnew=xnew,vil=vill,viu=viuu,
                        outeta=eta,outwac=outwac,outvyp=outvyp,outvsp=outvsp,outvip=outvip,outxnew=outxnew,
                        wac=lwac,linea=lineout$mean$a,lineb=lineout$mean$b,outwd=outwd,cl=cl,cu=cu))
            
            #######################################################
          }
          
        }
        
        
        return(list(mcmcout=bayesres,bind=bind,rsq=rsq,outa=outrega,outb=outregb,
                    meana=lineout$mean$a,
                    rac=rac(),wacsd=lineout$sd$wac,
                    waca=a,wacb=b,vyl=vyll,vyu=vyuu,vxl=vxll,vxu=vxuu,expout=expout,
                    betal=betal,betau=betau,etal=etall,etau=etauu,
                    vsl=vsll,vsu=vsuu,xnew=xnew,vil=vill,viu=viuu,
                    outeta=eta,outwac=outwac,outvyp=outvyp,outvsp=outvsp,outvip=outvip,outxnew=outxnew,
                    wac=lwac,linea=lineout$mean$a,lineb=lineout$mean$b,outwd=outwd,cl=cl,cu=cu))
        
        
        #####NS>1
        
      }else{
        
        
        ####merge calibration data talbes
        caldata<-reactive({
          
          if (is.null(input$calfile))
            return()
          else
          {
            nfile<<-nrow(input$calfile)
            
            csv=list()
            
            for(i in 1: nfile)
            {
              csv[[i]]=read.csv(input$calfile$datapath[i])
              
            }
            
            do.call(rbind,csv) # rbind the csv
          }
          
        })
        
        ####merge sample data tables
        
        sampledata<-reactive({
          
          if (is.null(input$samplefile))
            return()
          else
          {
            nfile=nrow(input$samplefile)
            csv=list()
            for(i in 1: nfile)
            {
              csv[[i]]=read.csv(input$samplefile$datapath[i])
              
            }
            
            do.call(rbind,csv) # rbind the csv
          }
          
        })
        
        
        wadm<<-reactive(
          return(append(as.numeric(unlist(strsplit(gsub("\\s", "", input$wadm),","))),1))
          
        )
        
        uwad<<-reactive(
          return(append(as.numeric(unlist(strsplit(gsub("\\s", "", input$uwad),","))),1))
          
        )
        
        ####
        
        
        NWS <- reactive({
          
          inFile <- input$calfile
          if (is.null(input$calfile)){
            return(NULL)
          }else{
            
            
            tmp<- read.csv(inFile$datapath[1])
            
            return(max(tmp$wsol))# #working solutions from  calibrationtable
            
            
          }
        })
        
        standard<-reactive(
          input$standard
        )
        
        umids<-reactive({
          
          if (is.null(input$calfile))
            return()
          else
          {
            
            if (standard()==1){
              nfile<<-nrow(input$calfile)
              
              return(c(rep(input$u_mid,nfile)))
            }else {
              nfile<<-nrow(input$calfile)
              return(c(rep(1,nfile)))
            }
            
            
          }
        })
        
        umads<<-reactive(
          
          return(c(rep(input$u_mad,nfile)))
          
        )
        
        
        ####
        
        NS<-reactive({
          
          if (is.null(input$calfile))
            return()
          else
          {
            NS<-nrow(input$calfile)
            return(NS)
            
          }
          
        })
        
        
        
        NT<-reactive(
          
          length(caldata()$mid)
          
        )
        
        expc<-reactive({
          
          if (is.null(input$calfile))
            return()
          else
          {
            nfile<-nrow(input$calfile)
            
            
            expc=list()
            
            for(i in 1: nfile)
            {
              nr= nrow(read.csv(input$calfile$datapath[i]))
              expc[[i]]<-(c(rep(1*i,each=nr)))
              expc[[i]]<-t(expc[[i]])
            } 
            
            
            
            as.vector(do.call(cbind, expc))
            
            
          }
          
        })
        
        
        
        
        midsm<<-reactive({
          
          if(standard()==1){
            return(caldata()$mid)
            
          }else{
            
            return(caldata()$mad/caldata()$mad)
          }
        })
        
        
        madsm<<-reactive(
          caldata()$mad
        )
        
        wsol<<-reactive(
          caldata()$wsol
        )
        
        
        
        NTT <- reactive({
          
          inFile <- input$calfile
          if (is.null(input$calfile)){
            return(NULL)
          }else{
            
            
            tmp=list()
            
            for(i in 1: nfile)
            {
              tmp[[i]]=read.csv(input$calfile$datapath[i])
              
            }
            
            tmp<-do.call(rbind,tmp)
            
            NR<<-ncol(tmp)-3# number of replicates for each calibrant
            NT<-nrow(tmp)
            
            NTT<-NR*NT
            
          }
        })
        
        
        expc1<-reactive({
          
          if (is.null(input$calfile))
            return()
          else
          {
            nfile<-nrow(input$calfile)
            
            
            
            expc1=list()
            
            for(i in 1: nfile)
            {
              nr<<-nrow(read.csv(input$calfile$datapath[i]))
              nrac<-ncol(read.csv(input$calfile$datapath[i]))-3
              n1<-nr*nrac
              expc1[[i]]<-matrix(c(rep(1*i,each=n1)))
              
            } 
            
            
            as.vector(do.call(rbind, expc1))
            
            
          }
          
        })
        
        expc2<-reactive({
          
          if (is.null(input$calfile))
            return()
          else
          {
            nfile<-nrow(input$calfile)
            
            csv=list()
            
            k<-nrow(read.csv(input$calfile$datapath[1]))
            nrac<-ncol(read.csv(input$calfile$datapath[1]))-3
            
            
            csv[[1]]=c(rep(1:k,nrac))
            
            
            for(i in 2: nfile)
            {
              
              c=max(csv[[i-1]])+1
              x=nrow(read.csv(input$calfile$datapath[i]))
              y=c+x-1
              csv[[i]]=c(rep(c:y, nrac))
              
            }
            
            as.vector(unlist(csv))
            
            
            
          }
          
        })
        
        
        
        
        rac<- reactive({
          
          if (is.null(input$calfile))
            return()
          else
          {
            nfile<-nrow(input$calfile)
            
            csv=list()
            
            for(i in 1: nfile)
            {
              csv[[i]]=read.csv(input$calfile$datapath[i])
              csv[[i]][1:3]<-NULL
            }
            
            return(unlist(csv)) # rbind the csv
          }
          
        })
        
        
        ras<- reactive({
          
          if (is.null(input$samplefile))
            return()
          else
          {
            nfile<-nrow(input$samplefile)
            
            csv=list()
            
            for(i in 1: nfile)
            {
              csv[[i]]=read.csv(input$samplefile$datapath[i])
              csv[[i]][1:2]<-NULL
            }
            
            return(unlist(csv)) # rbind the csv
          }
          
        })
        
        
        mdi<<-reactive(
          
          mdi<-sampledata()$mdi
          
        )
        
        
        midsi<<-reactive(
          
          if(standard()==1){
            return(sampledata()$mids)
          }else{
            
            return(sampledata()$mdi/sampledata()$mdi)
            
          }
        )
        
        MT<<-reactive(
          
          length(sampledata()$mids)
          
        )
        
        MTT <- reactive({
          
          inFile <- input$samplefile
          if (is.null(input$samplefile)){
            return(NULL)
          }else{
            
            
            tmp=list()
            
            for(i in 1: nfile)
            {
              tmp[[i]]=read.csv(input$samplefile$datapath[i])
              
            }
            
            tmp<-do.call(rbind,tmp)
            
            NR<<-ncol(tmp)-2# number of replicates for each calibrant
            NT<-nrow(tmp)
            
            NTT<-NR*NT
            
          }
        })
        
        exp<-reactive({
          
          if (is.null(input$samplefile))
            return()
          else
          {
            nfile<-nrow(input$samplefile)
            
            
            exp=list()
            
            for(i in 1: nfile)
            {
              nr= nrow(read.csv(input$samplefile$datapath[i]))
              exp[[i]]<-(c(rep(1*i,each=nr)))
              exp[[i]]<-t(exp[[i]])
            } 
            
            as.vector(do.call(cbind, exp))
            
          }
          
        })
        
        
        expr<-reactive({
          
          if (is.null(input$samplefile))
            return()
          else
          {
            nfile<<-nrow(input$samplefile)
            
            
            
            expr=list()
            
            for(i in 1: nfile)
            {
              nr<<-nrow(read.csv(input$samplefile$datapath[i]))
              nrac<-ncol(read.csv(input$samplefile$datapath[i]))-2
              n1<-nr*nrac
              expr[[i]]<-matrix(c(rep(1*i,each=n1)))
              
            } 
            
            
            as.vector(do.call(rbind, expr))
            
            
          }
          
        })
        
        expr2<-reactive({
          
          if (is.null(input$samplefile))
            return()
          else
          {
            nfile<-nrow(input$samplefile)
            
            csv=list()
            
            k<-nrow(read.csv(input$samplefile$datapath[1]))
            nras<-ncol(read.csv(input$samplefile$datapath[1]))-2
            
            
            csv[[1]]=c(rep(1:k,nras))
            
            
            for(i in 2: nfile)
            {
              
              c=max(csv[[i-1]])+1
              x=nrow(read.csv(input$samplefile$datapath[i]))
              y=c+x-1
              csv[[i]]=c(rep(c:y, nras))
              
            }
            
            as.vector(unlist(csv))
            
            
            
          }
          
        })
        
        nfile<-reactive({
          
          if (!is.null(input$calfile))
          {
            nfile<-nrow(input$calfile)
          }
        })
        
        nrac<-reactive({
          
          if (!is.null(input$calfile))
          {
            nrac<-ncol(read.csv(input$calfile$datapath[1]))-3
            
          }
        })
        
        vecwac <<- reactive({
          
          if (!is.null(input$calfile))
          {
            
            vec=list()
            for(i in 1: nfile){
              vec[[i]]<-nrow(read.csv(input$calfile$datapath[i]))
            }
            return(vec)
          }
        })
        
        vecsample <- reactive({
          
          if (!is.null(input$samplefile))
          {
            
            vec=list()
            for(i in 1: nfile){
              vec[[i]]<-nrow(read.csv(input$samplefile$datapath[i]))
            }
            return(vec)
          }
        })
        
        
        
        vecrac <- reactive({
          
          if (!is.null(input$calfile))
          {
            
            vecrac=list()
            for(i in 1: nfile){
              vecrac[[i]]<-nrac()*nrow(read.csv(input$calfile$datapath[i]))
            }
            return(vecrac)
          }
        })
        
        vecras <- reactive({
          
          if (!is.null(input$samplefile))
          {
            
            vecras=list()
            for(i in 1: nfile){
              vecras[[i]]<-nrac()*nrow(read.csv(input$samplefile$datapath[i]))
            }
            return(vecras)
          }
        })
        
        linedata<-list(umids=umids(),
                       umads=umads(),
                       wadm=wadm(),
                       uwad=uwad(),
                       NWS=NWS()+1,
                       NS=NS(),
                       NT=NT(),
                       expc=expc(),
                       midsm=midsm(),
                       madsm=madsm(),
                       wsol=wsol(), 
                       NTT=NTT(), 
                       expc1=expc1(),
                       expc2=expc2(),
                       rac=rac(),
                       ras=ras(),
                       MT=MT(),MTT=MTT(),
                       exp=exp(),
                       midsi=midsi(),
                       mdi=mdi(),
                       expr=expr(),
                       expr2=expr2())
        
        
        require(R2OpenBUGS)
        # #   
        # #   ##################################################################
        # #   ##################################################################
        # #   ### Define the model
        # #   ##################################################################
        # #   ##################################################################
        # #   
        # #   
        linemodel<-function(){####this program uses NS number of input tables. 
          ### NT is the total number of elements of midsm and madsm (total number of calibrants). 
          ### expc gives the experiment designation for each element in midsm and madsm. NTT is the total number of measurements in the calibration experiment.
          ### expc1 gives the experiment label for each observation in rac, expc2 gives calibrant designation for each rac.
          ### MT gives the total number of samples, MTT gives the total number of observations in the quantitation experiment 
          ### exp gives the experiment designation for each sample, expr gives the experiment designation for each element in ras, 
          ### expr2 gives the sample designation for each element in ras.
          
          
          for(i in 1:NWS){wadprec[i]<-1/(uwad[i]*uwad[i])
          wad[i]~dnorm(wadm[i],wadprec[i])}
          wadmean<-mean(wad[])
          
          for(i in 1:NS){midsprec[i]<-1/(umids[i]*umids[i])
          madsprec[i]<-1/(umads[i]*umads[i])}
          
          
          for(i in 1:NT){
            mids[i]~dnorm(midsm[i],midsprec[expc[i]])
            mads[i]~dnorm(madsm[i],madsprec[expc[i]])
            ratio[i]<-mads[i]/mids[i] 
            wac[i]<-wad[wsol[i]]* ratio[i] } 
          
          #######
          #calibration equation
          for(i in 1:NS){
            a[i]~dnorm(0,1.0E-5)
            b[i]~dnorm(0,1.0E-5)
            
            xins[i]~dnorm(0,0.0016)%_%I(0.001,)
            chsqns[i]~dgamma(0.5,0.5)
            fitprec[i]<-xins[i]/sqrt(chsqns[i])
            
            
          }
          
          for(i in 1:NTT){mean[i]<-a[expc1[i]]+b[expc1[i]]*wac[expc2[i]]
          rac[i]~dnorm(mean[i],fitprec[expc1[i]])
          predm[i]~dnorm(mean[i],fitprec[expc1[i]])
          
          }
          
          
          ###
          # Compute the mass fraction wd
          for(i in 1:NS){
            a.cut[i]<-cut(a[i])
            b.cut[i]<-cut(b[i])
            xinsi[i]~dnorm(0,0.0016)%_%I(0.001,)
            chsqnsi[i]~dgamma(0.5,0.5)
            sigras[i]<-xinsi[i]/sqrt(chsqnsi[i])
            #vsp[i]<-1/sqrt(sigras[i])
            # hetp[i]<-vsp[i]/vyp[i]*100 
            
            xinsw[i]~dnorm(0,0.0016)%_%I(0.001,)
            chsqnsw[i]~dgamma(0.5,0.5)
            wdsig[i]<-xinsw[i]/sqrt(chsqnsw[i])
            wd[i]~dnorm(0,1.0E-5)}
          
          for(i in 1:MT){
            wdm[i]~dnorm(wd[exp[i]],wdsig[exp[i]])
            mdp[i]~dnorm(mdi[exp[i]],madsprec[exp[i]])
            midsip[i]~dnorm(midsi[exp[i]],midsprec[exp[i]])
            rasmean[i]<-a.cut[exp[i]]+b.cut[exp[i]]*wdm[i]*mdi[i]/midsi[i]
            rasmeanp[i]~dnorm(rasmean[i],fitprec[exp[i]])
            mult[i]<-(wdm[i]*mdp[i])/midsip[i]
            wtop[i]<-mdp[i]/midsip[i]
            
          }
          
          
          for(i in 1:MTT){
            ras[i]~dnorm(rasmeanp[expr2[i]],sigras[expr[i]])
            
          }
          
          for(i in 1:NS){
            
            vsp[i]<-1/sqrt(sigras[i])
            vyp[i]<-1/sqrt(fitprec[i])
          }
          # 
          
          #############################################
          T~dcat(P[])
          P[1:NS]~ddirich(alpha[])
          for(i in 1:NS){alpha[i]<-1}
          mumeanfin<-wd[T]
          
        }
        
        ##################################################################
        ##################################################################
        ### Run OpenBUGS
        ##################################################################
        ##################################################################
        
        withProgress(message = 'Running Bayes', style="old",value = 0, {
          
          lineinits<-function(){list(sigras=c(1,1),wdsig=c(1,1),a=c(0,0),b=c(1,1),wd=c(0,0))}
          
          lineout<-bugs(data=linedata,
                        inits=lineinits,
                        parameters=c("a","b","wd","wac","vyp","vsp","mult","mean","predm","mumeanfin"),   
                        model.file=linemodel,
                        n.iter = 10000, n.burnin = 5000, n.thin = 10,n.chains =1)#,debug=T)
        })
        
        ###########################################
        ### additional result generation
        
        attach.bugs(lineout) ## imports the random draws for the parameters
        
        outrega<-data.frame(mean=lineout$mean$a,sd=lineout$sd$a)
        outregb<-data.frame(mean=lineout$mean$b,sd=lineout$sd$b)
        outwdm<-data.frame(mean=lineout$mean$wdm,sd=lineout$sd$wdm)
        outmean<-data.frame(mean=lineout$mean$mean)
        outwd<-data.frame(mean=lineout$mean$wd,sd=lineout$sd$wd )
        outmumeanfin<-data.frame(mean=lineout$mean$mumeanfin,sd=lineout$sd$mumeanfin )
        outhetp<-lineout$mean$hetp
        outvyp<-data.frame(mean=lineout$mean$vyp,sd=lineout$sd$vyp)
        outvsp<-data.frame(mean=lineout$mean$vsp,sd=lineout$sd$vsp)
        outmult<-lineout$mean$mult
        
        
        ########## sets up the data to plot the regression line and calculate the rsq
        
        
        
        require(data.table)
        
        
        ############ calculates the rsq
        
        ybar<-split(outmean,rep(1:nfile,vecrac()))
        rac_rsq<-split(rac(),rep(1:nfile,vecrac()))
        
        sst=list()
        sse=list()
        rsq=list()
        for(i in 1: nfile)
        {
          sst[[i]]<-sum((rac_rsq[[i]]-mean(rac_rsq[[i]]))^2)
          sse[[i]]<-sum((rac_rsq[[i]]-ybar[[i]])^2)
          rsq[[i]]<-1-(sse[[i]]/sst[[i]])
        }
        
        n_rsq<-unlist(rsq)
        
        
        ####calibration plot ##
        
        ##get wac vectors
        
        outwac=lineout$mean$wac
        eta<-lineout$sd$wac/lineout$mean$wac
        lwac<-split(outwac,rep(1:nfile,vecwac()))
        
        lwac<-lapply(lwac,rep,nrac())
        
        
        lrac<-split(rac(),rep(1:nfile,vecrac()))
        
        cl=NULL
        
        length=length(lineout$mean$predm)
        predm=lineout$sims.list$predm
        
        for(i in 1:length){
          cl[[i]]<-quantile(predm[,i],0.025)
        }
        
        cl<-split(cl,rep(1:nfile,vecrac()))
        
        cu=NULL
        for(i in 1:length){
          cu[[i]]<-quantile(predm[,i],0.925)
        }
        
        cu<-split(cu,rep(1:nfile,vecrac()))
        
        
        ###Regression output###
        # output vip and xnew
        outvip<-list()
        mult=outmult
        mult<-split(mult,rep(1:nfile,vecsample()))
        vilmean<-list()
        vilsd<-list()
        vill<-list()
        viuu<-list()
        vipp<-list()
        vipp<-lineout$sd$mult/lineout$mean$mult
        vipp<-split(vipp,rep(1:nfile,vecsample()))
        xnew<-list()
        for (i in 1:nfile){
          
          vilmean[[i]]=mean(vipp[[i]])
          vilsd[[i]]=sd(vipp[[i]])
          
          vill[[i]]=vilmean[[i]]-2*vilsd[[i]]
          viuu[[i]]=vilmean[[i]]+2*vilsd[[i]]
          xnew[[i]]=mean(mult[[i]])
        }
        
        # vill<-unlist(vill)
        # viuu<-unlist(viuu)
        # vilmean(unlist(vilmean))
        # 
        # vilsd(unlist(vilsd))
        outvip<-data.frame(mean=unlist(vilmean),sd=(unlist(vilsd)))
        #outvip<-data.frame(mean=vilmean,sd=vilsd)
        outxnew<-data.frame(mean=unlist(xnew))
        #### Output wac
        mulwac=outwac
        # 
        # 
        mulwac<-split(mulwac,rep(1:nfile,vecwac()))
        mulwac<-sapply(mulwac, `[`, seq_len(max(lengths(mulwac))))
        outwac<-matrix(mulwac,ncol = nfile,nrow = N)
        
        output$wac<-renderTable(mulwac)
        #mulwac[is.na(mulwac)] <- ""
        #output multi eta
        muleta=lineout$sd$wac/lineout$mean$wac
        outeta<-muleta
        muleta<-split(muleta,rep(1:nfile,vecwac()))
        muleta<-sapply(muleta, `[`, seq_len(max(lengths(muleta))))
        outeta<-matrix(muleta,ncol=nfile,nrow=N)
        #muleta[is.na(muleta)] <- ""
        ###
        
        bayesres=lineout$sims.list$mumeanfin
        ####download###
        
        
        
        credible_interval=quantile(bayesres,c((1-input$coverage)/2, (1+input$coverage)/2))
        
        bind <-list(wadm=input$wadm,uwad=input$uwad,u_mid=umids(),u_mad=input$u_mad,u_mids=input$u_mids,u_mdi=input$u_mdi,
                    coverage=input$coverage,Number_iteration=input$niters,Length_burnin=input$nburnin,
                    mean=mean(bayesres),sd=sd(bayesres),median=median(bayesres),
                    credible_interval1=credible_interval[1],credible_interval2=credible_interval[2],
                    R_squared=n_rsq,
                    intercept=outrega,
                    slope=outregb,
                    
                    wd=outwd,
                    vyp=outvyp,
                    vsp=outvsp,
                    vip=outvip,
                    xnew=unlist(xnew)
                    # mult=outmult
                    
        )
        
        
        updateTabsetPanel(session = session, inputId = "results",selected = "Fit")
        #waiter_hide()
        
        
        #####  DOE Inputs 
        
        wacab<-split(lineout$mean$wac,rep(1:nfile,vecwac()))
        vx<-split(lineout$sd$wac,rep(1:nfile,vecwac()))
        rux<-split(lineout$sd$wac/lineout$mean$wac,rep(1:nfile,vecwac()))
        etall<-list()
        etauu<-list()
        
        vxl<-list()
        vxu<-list()
        waca<-list()
        wacb<-list()
        vxll<-list()
        vxuu<-list()
        waca<-list()
        etalll<-list()
        etauuu<-list()
        for (i in 1:nfile){
          vxll[[i]]=min(vx[[i]])
          vxuu[[i]]=max(vx[[i]])
          
          waca[[i]]=min(wacab[[i]])
          wacb[[i]]=max(wacab[[i]])
          
          etalll[i]=min(rux[[i]])
          etauuu[i]=max(rux[[i]])
          
        }
        
        
        vyll<-lineout$mean$vyp-2*lineout$sd$vyp
        vylll<-lineout$mean$vyp-2*lineout$sd$vyp
        vyuu<-lineout$mean$vyp+2*lineout$sd$vyp
        vyuuu<-lineout$mean$vyp+2*lineout$sd$vyp
        
        betal<-lineout$mean$b-2*lineout$sd$b
        betall<-lineout$mean$b-2*lineout$sd$b
        betau<-lineout$mean$b+2*lineout$sd$b
        betauu<-lineout$mean$b+2*lineout$sd$b
        
        
        vxll<-c("user's input")     #DOE vxl
        vxuu<-c("user's input")    #DOE vxu
        
        vsll<-lineout$mean$vsp-2*lineout$sd$vsp #DOE vsl
        vslll<-lineout$mean$vsp-2*lineout$sd$vsp #DOE vsl
        vsuu<-lineout$mean$vsp+2*lineout$sd$vsp #DOE vsu
        vsuuu<-lineout$mean$vsp+2*lineout$sd$vsp #DOE vsu
        
        
        xneww<-outxnew
        
        #####DOE INPUT####
        expout<-list(
          xnew=unlist(xnew),
          slope_min=betal,slope_max=betau,
          wac_min=unlist(waca),wac_max=unlist(wacb),
          Uy_min=unlist(vylll),Uy_max=unlist(vyuuu),rux_min=unlist(etalll),rux_max=unlist(etauuu),
          Us_min=unlist(vslll),Us_max=unlist(vsuuu),Ui_min=unlist(vill),Ui_max=unlist(viuu)
        )
        
        if(input$exp){
          ###download
          
          
          vxll<-c("user's input")     #DOE vxl
          vxuu<-c("user's input")    #DOE vxu
          
          doe1<-reactive(
            
            input$doe1
          )
          
          if(doe1()==1){
            
            totn<-input$doetotn
            std<-input$doestd
            a=waca[[1]]
            
            b=wacb[[1]]
            
            vyl=(vyll[[1]])^2
            
            vyu=(vyuu[[1]])^2
            
            vxl<-(input$vxl)^2       #DOE vxl
            vxu<-(input$vxu)^2      #DOE vxu
            
            betal=betal[[1]]
            
            betau=betau[[1]]
            
            etal=(etalll[[1]])^2
            
            etau=(etauuu[[1]])^2
            
            vsl=(vsll[[1]])^2
            
            vsu=(vsuu[[1]])^2
            
            xnew=xnew[[1]]
            
            vil=(vill[[1]])^2
            viu=(viuu[[1]])^2
            
            caln<-c(4,6,8,10,12,14,15,16,18,20,22,24,28,30,36,40,44,48)
            
            out<-NULL
            designh1<-expdesign2(caln[1],totn,a,b,vyu,vxu,betau,etau,vsu,xnew,viu)
            out<-rbind(out,c(as.integer(designh1$optI),designh1$optJ,designh1$optr,designh1$optnr,designh1$optstd))
            for(i in 2:18){if(out[i-1,5]>std){
              designh<-expdesign2(caln[i],totn,a,b,vyu,vxu,betau,etau,vsu,xnew,viu)
              out<-rbind(out,c(as.integer(designh$optI),designh$optJ,designh$optr,designh$optnr,designh$optstd)) 
            }else{break}}
            
            outng<-NULL
            outng<-out
            outg<-NULL
            while(dim(out)[1]<18){outg<-out
            totn<-totn-1
            if(totn==0){break}
            
            out<-NULL
            designh1<-expdesign2(caln[1],totn,a,b,vyu,vxu,betau,etau,vsu,xnew,viu)
            out<-rbind(out,c(as.integer(designh1$optI),designh1$optJ,designh1$optr,designh1$optnr,designh1$optstd))
            for(i in 2:18){if(out[i-1,5]>std){
              designh<-expdesign2(caln[i],totn,a,b,vyu,vxu,betau,etau,vsu,xnew,viu)
              out<-rbind(out,c(as.integer(designh$optI),designh$optJ,designh$optr,designh$optnr,designh$optstd)) 
            }else{break}}
            
            dimg<-dim(outg)[1]
            
            outgood<-outg[outg[,5]<std,] ### this contains the experimental designs that satisfy the std requirement
            optI<-outgood[1] # optimal number of calibrants
            optJ<-outgood[2]  # optimal number of replicates
            optr<-outgood[3]  # optimal number of samples in the quantitation experiment
            optnr<-outgood[4] # optimal number of replicates per sample
            optstd<-outgood[5] # optimal expected relative standard deviation of the response
            
            optx<-NULL
            optx[1]<-a
            optIm1<-optI-1
            
            for(i in 1:optIm1){
              optx[i+1]<-a+i*(b-a)/optIm1}}
            
            dimng<-dim(outng)[1]
            
            
            
            #  hide_waiter()
            
            if(outng[dimng,5]>std){
              ################to be printed in shiny#################
              
              #######################################################
              outbest<-outng[18,]
              optIb<-outbest[1]  # number of calibrants
              optJb<-outbest[2]  # number of replicates
              optrb<-outbest[3]  # number of samples in the quantitation experiment
              optnrb<-outbest[4] # number of replicates per sample
              optstdb<-outbest[5] # the smallest expected relative standard deviation of the response given your totn and std
              optxb<-NULL
              optxb[1]<-a
              optIm1b<-optIb-1
              for(i in 1:optIm1b){
                optxb[i+1]<-a+i*(b-a)/optIm1b}
              
              output$err<-renderUI({
                h5("Please increase the total number of  observations otherwise the  experimental design is:",style = "color:red")
              })
              #results <- matrix(c(optxb),ncol =length(optxb), nrow = 1)
              optxb<-t(optxb)
              optxb.colnames = sapply(1:length(optxb), function(x){paste0("level", x)})
              colnames(optxb) = c(optxb.colnames)
              
              
              #results2 <- matrix(c(optIb,optJb,optrb,optnrb),ncol=4, nrow = 1)
              #colnames(results2) <- c("Calibrants","Calibrant replicates","Samples","Sample replicates")
              results2<-list(Calibrants=optIb,Calibrant_replicates=optJb,Samples=optrb,Sample_replicates=optnrb)
              optstdb <- matrix(c(optstdb))
              #optstdb<-list(Standard_devaiation=optstdb)
              colnames(optstdb) <- c("Expected Relative (%) Standard Deviation of Result")
              
              
              return(list(mcmcout=bayesres,rsq=n_rsq,
                          outvyp=outvyp,bind=bind,expout=expout,
                          wacsd=lineout$sd$wac,result=results2,optxb=optxb,optstd= optstdb,
                          waca=unlist(waca),wacb=unlist(wacb),vyl=vylll,vyu=vyuuu,vxl=vxll,vxu=vxuu,
                          betal=betall,betau=betauu,etal=unlist(etalll),etau=unlist(etauuu),
                          vsl=vslll,vsu=vsuuu,xnew=xneww,vil=unlist(vill),viu=unlist(viuu),
                          rac=lrac,wac=lwac,outwac=outwac,outeta=outeta,outvip=outvip,outxnew=outxnew,outa=outrega,outaa=lineout$mean$a,outbb=lineout$mean$b,outb=outregb,cl=cl,cu=cu,
                          outvsp=outvsp,outwd=outwd,outmult=outmult))
              
            }else{
              
              
              output$err<-renderUI({
                HTML("")
              })
              
              # results <- matrix(c(optx),ncol =length(optx), nrow = 1)
              optx<-t(optx)
              optx.colnames = sapply(1:length(optx), function(x){paste0("level", x)})
              colnames(optx) = c( optx.colnames)
              
              
              
              results2<- list(Calibrants=optI,Calibrant_replicate=optJ,Samples=optr,Sample_replicate=optnr)
              
              optstd <- matrix(c(optstd))
              colnames(optstd) <- c("Expected Relative (%) Standard Deviation of Result")
              
              # optstd <- list(Standard_deviation=optstd)
              
              return(list(mcmcout=bayesres,rsq=n_rsq,
                          outvyp=outvyp,bind=bind,expout=expout,
                          wacsd=lineout$sd$wac,result=results2,optxb=optx,optstd= optstd,
                          waca=unlist(waca),wacb=unlist(wacb),vyl=vylll,vyu=vyuuu,vxl=vxll,vxu=vxuu,
                          betal=betall,betau=betauu,etal=unlist(etalll),etau=unlist(etauuu),
                          vsl=vslll,vsu=vsuuu,xnew=xneww,vil=unlist(vill),viu=unlist(viuu),
                          rac=lrac,wac=lwac,outwac=outwac,outeta=outeta,outvip=outvip,outxnew=outxnew,outa=outrega,outaa=lineout$mean$a,outbb=lineout$mean$b,outb=outregb,cl=cl,cu=cu,
                          outvsp=outvsp,outwd=outwd,outmult=outmult))
              
              
              #######################################################
            }
            
            
            
            
          }else{
            
            totn<-input$doetotn
            std<-input$doestd
            a=waca[[2]]
            
            b=wacb[[2]]
            
            vyl=(vyll[[2]])^2
            
            vyu=(vyuu[[2]])^2
            
            vxl<-(input$vxl)^2       #DOE vxl
            vxu<-(input$vxu)^2      #DOE vxu
            
            betal=betal[[2]]
            
            betau=betau[[2]]
            
            etal=(etalll[[2]])^2
            
            etau=(etauuu[[2]])^2
            
            vsl=(vsll[[2]])^2
            
            vsu=(vsuu[[2]])^2
            
            xnew=xnew[[2]]
            
            vil=(vill[[2]])^2
            viu=(viuu[[2]])^2
            
            caln<-c(4,6,8,10,12,14,15,16,18,20,22,24,28,30,36,40,44,48)
            
            out<-NULL
            designh1<-expdesign2(caln[1],totn,a,b,vyu,vxu,betau,etau,vsu,xnew,viu)
            out<-rbind(out,c(as.integer(designh1$optI),designh1$optJ,designh1$optr,designh1$optnr,designh1$optstd))
            for(i in 2:18){if(out[i-1,5]>std){
              designh<-expdesign2(caln[i],totn,a,b,vyu,vxu,betau,etau,vsu,xnew,viu)
              out<-rbind(out,c(as.integer(designh$optI),designh$optJ,designh$optr,designh$optnr,designh$optstd)) 
            }else{break}}
            
            outng<-NULL
            outng<-out
            outg<-NULL
            while(dim(out)[1]<18){outg<-out
            totn<-totn-1
            if(totn==0){break}
            
            out<-NULL
            designh1<-expdesign2(caln[1],totn,a,b,vyu,vxu,betau,etau,vsu,xnew,viu)
            out<-rbind(out,c(as.integer(designh1$optI),designh1$optJ,designh1$optr,designh1$optnr,designh1$optstd))
            for(i in 2:18){if(out[i-1,5]>std){
              designh<-expdesign2(caln[i],totn,a,b,vyu,vxu,betau,etau,vsu,xnew,viu)
              out<-rbind(out,c(as.integer(designh$optI),designh$optJ,designh$optr,designh$optnr,designh$optstd)) 
            }else{break}}
            
            dimg<-dim(outg)[1]
            
            outgood<-outg[outg[,5]<std,] ### this contains the experimental designs that satisfy the std requirement
            optI<-outgood[1] # optimal number of calibrants
            optJ<-outgood[2]  # optimal number of replicates
            optr<-outgood[3]  # optimal number of samples in the quantitation experiment
            optnr<-outgood[4] # optimal number of replicates per sample
            optstd<-outgood[5] # optimal expected relative standard deviation of the response
            
            optx<-NULL
            optx[1]<-a
            optIm1<-optI-1
            
            for(i in 1:optIm1){
              optx[i+1]<-a+i*(b-a)/optIm1}}
            
            dimng<-dim(outng)[1]
            
            
            
            #  hide_waiter()
            
            if(outng[dimng,5]>std){
              ################to be printed in shiny#################
              
              #######################################################
              outbest<-outng[18,]
              optIb<-outbest[1]  # number of calibrants
              optJb<-outbest[2]  # number of replicates
              optrb<-outbest[3]  # number of samples in the quantitation experiment
              optnrb<-outbest[4] # number of replicates per sample
              optstdb<-outbest[5] # the smallest expected relative standard deviation of the response given your totn and std
              optxb<-NULL
              optxb[1]<-a
              optIm1b<-optIb-1
              for(i in 1:optIm1b){
                optxb[i+1]<-a+i*(b-a)/optIm1b}
              
              
              output$err<-renderUI({
                h5("Please increase the total number of  observations otherwise the  experimental design is:",style = "color:red")
              })
              
              #results <- matrix(c(optxb),ncol =length(optxb), nrow = 1)
              optxb<-t(optxb)
              optxb.colnames = sapply(1:length(optxb), function(x){paste0("level", x)})
              colnames(optxb) = c(optxb.colnames)
              
              #results2 <- matrix(c(optIb,optJb,optrb,optnrb),ncol=4, nrow = 1)
              #colnames(results2) <- c("Calibrants","Calibrant replicates","Samples","Sample replicates")
              results2<-list(Calibrants=optIb,Calibrant_replicates=optJb,Samples=optrb,Sample_replicates=optnrb)
              #results3 <- matrix(c(optstdb))
              # optstdb<-list(Standard_devaiation=optstdb)
              #  colnames(results3) <- c("Standard Deviation")
              
              optstdb <- matrix(c(optstdb))
              #optstdb<-list(Standard_devaiation=optstdb)
              colnames(optstdb) <- c("Expected Relative (%) Standard Deviation of Result")
              
              return(list(mcmcout=bayesres,rsq=n_rsq,
                          outvyp=outvyp,bind=bind,expout=expout,
                          wacsd=lineout$sd$wac,result=results2,optxb=optxb,optstd= optstdb,
                          waca=unlist(waca),wacb=unlist(wacb),vyl=vylll,vyu=vyuuu,vxl=vxll,vxu=vxuu,
                          betal=betall,betau=betauu,etal=unlist(etalll),etau=unlist(etauuu),
                          vsl=vslll,vsu=vsuuu,xnew=xneww,vil=unlist(vill),viu=unlist(viuu),
                          rac=lrac,wac=lwac,outwac=outwac,outeta=outeta,outvip=outvip,outxnew=outxnew,outa=outrega,outaa=lineout$mean$a,outbb=lineout$mean$b,outb=outregb,cl=cl,cu=cu,
                          outvsp=outvsp,outwd=outwd,outmult=outmult))
              
            }else{
              
              
              output$err<-renderUI({
                HTML("")
              })
              
              # results <- matrix(c(optx),ncol =length(optx), nrow = 1)
              optx<-t(optx)
              optx.colnames = sapply(1:length(optx), function(x){paste0("level", x)})
              colnames(optx) = c( optx.colnames)
              
              
              # results2 <- matrix(c(optI,optJ,optr,optnr),ncol=4, nrow = 1)
              results2<- list(Calibrants=optI,Calibrant_replicate=optJ,Samples=optr,Sample_replicate=optnr)
              
              optstd <- matrix(c(optstd))
              #optstdb<-list(Standard_devaiation=optstdb)
              colnames(optstd) <- c("Expected Relative (%) Standard Deviation of Result")
              
              #optstd <- list(Standard_deviation=optstd)
              
              return(list(mcmcout=bayesres,rsq=n_rsq,
                          outvyp=outvyp,bind=bind,expout=expout,
                          wacsd=lineout$sd$wac,result=results2,optxb=optx,optstd= optstd,
                          waca=unlist(waca),wacb=unlist(wacb),vyl=vylll,vyu=vyuuu,vxl=vxll,vxu=vxuu,
                          betal=betall,betau=betauu,etal=unlist(etalll),etau=unlist(etauuu),
                          vsl=vslll,vsu=vsuuu,xnew=xneww,vil=unlist(vill),viu=unlist(viuu),
                          rac=lrac,wac=lwac,outwac=outwac,outeta=outeta,outvip=outvip,outxnew=outxnew,outa=outrega,outaa=lineout$mean$a,outbb=lineout$mean$b,outb=outregb,cl=cl,cu=cu,
                          outvsp=outvsp,outwd=outwd,outmult=outmult))
              
              
              #######################################################
            }
            
            
          }
          
          
        }
        
        
        
        
        return(list(mcmcout=bayesres,rsq=n_rsq,
                    outvyp=outvyp,bind=bind,expout=expout,
                    wacsd=lineout$sd$wac,
                    waca=unlist(waca),wacb=unlist(wacb),vyl=vylll,vyu=vyuuu,vxl=vxll,vxu=vxuu,
                    betal=betall,betau=betauu,etal=unlist(etalll),etau=unlist(etauuu),
                    vsl=vslll,vsu=vsuuu,xnew=xneww,vil=unlist(vill),viu=unlist(viuu),
                    rac=lrac,wac=lwac,outwac=outwac,outeta=outeta,outvip=outvip,outxnew=outxnew,outa=outrega,outaa=lineout$mean$a,outbb=lineout$mean$b,outb=outregb,cl=cl,cu=cu,
                    outvsp=outvsp,outwd=outwd,outmult=outmult))
      }
      
    }else{
      
      
      
      ####RF model
      ###NS==1
      if(N_tables==1){
        
        caldata<-reactive({
          
          if (is.null(input$calfile))
            return()
          else
          {
            csv=list()
            csv=read.csv(input$calfile$datapath[1])
            return(csv)   
          }
          
        })
        
        ####merge sample data talbes
        
        sampledata<-reactive({
          
          if (is.null(input$samplefile))
            return()
          else
          {
            
            csv=list()
            csv=read.csv(input$samplefile$datapath[1])
            return(csv)
          }
          
        })
        
        
        wadm<<-reactive(
          return(append(as.numeric(unlist(strsplit(gsub("\\s", "", input$wadm),","))),1))
          
        )
        
        uwad<<-reactive(
          return(append(as.numeric(unlist(strsplit(gsub("\\s", "", input$uwad),","))),1))
          
        )
        
        ####
        
        
        NWS <- reactive({
          
          inFile <- input$calfile
          if (is.null(input$calfile)){
            return(NULL)
          }else{
            
            tmp<- read.csv(inFile$datapath[1])
            
            return(max(tmp$wsol))# #working solutions from  calibrationtable
            
            
          }
        })
        
        standard<-reactive(
          input$standard
        )
        
        umids<-reactive({
          
          if (is.null(input$calfile))
            return()
          else
          {
            
            if (standard()==1){
              
              return(input$u_mid)
            }else {
              
              return(1)
            }
            
            
          }
        })
        
        
        umads<-reactive(
          
          return(input$u_mad)
          
        )
        
        
        ####
        
        NS<-reactive({
          
          if (is.null(input$calfile))
            return()
          else
          {
            NS<-nrow(input$calfile)
            return(NS)
            
          }
          
        })
        
        
        
        NT<-reactive(
          
          return(N*NR)
          
        )
        
        
        
        
        
        midsm<-reactive(
          
          if(standard()==1){
            return(caldata()$mid)
          }else{
            return(caldata()$mad/caldata()$mad)
          }
        )
        madsm<-reactive(
          caldata()$mad
        )
        
        wsol<-reactive(
          caldata()$wsol
        )
        
        
        rac<- reactive({
          
          if (is.null(input$calfile))
            return()
          else
          {
            
            csv=list()
            
            for(i in 1: nfile)
            {
              csv[[i]]=read.csv(input$calfile$datapath[i])
              csv[[i]][1:3]<-NULL
            }
            
            return(unlist(csv)) # rbind the csv
          }
          
        })
        
        
        ras<- reactive({
          
          if (is.null(input$samplefile))
            return()
          else
          {
            nfile<-nrow(input$samplefile)
            
            csv=list()
            
            for(i in 1: nfile)
            {
              csv[[i]]=read.csv(input$samplefile$datapath[i])
              csv[[i]][1:2]<-NULL
            }
            
            return(unlist(csv)) # rbind the csv
          }
          
        })
        
        
        mdi<-reactive(
          
          sampledata()$mdi
          
        )
        
        midsi<<-reactive(
          
          if(standard()==1){
            return(sampledata()$mids)
          }else{
            
            return(sampledata()$mdi/sampledata()$mdi)
            
          }
        )
        
        MT<-reactive(
          
          return(M*MR)
          
        )
        
        
        
        sol<-reactive({
          rep(1:N,NR)
        })
        
        sampl<-reactive({
          rep(1:M,NR)
          
        })   
        
        
        #####
        
        linedata = list(
          midsm=midsm(),
          madsm=madsm(),
          wsol=wsol(),
          rac=rac(),
          midsi=midsi(),
          mdi=mdi(),
          ras=ras(),
          umids=umids(),
          umads=umads(),
          wadm= wadm(),
          uwad=uwad(),
          NWS=NWS()+1,
          N=N,
          M=M,
          MT=MT(),
          NT=NT(),
          sol=sol(),
          NR=NR,
          sampl=sampl()
        )
        
        
        # 
        require(R2OpenBUGS)
        ##################################################################
        ##################################################################
        ### Define the model
        ##################################################################
        ##################################################################
        
        
        linemodel<-function(){#calculate known mass ratios wac for N calibrants.
          
          midsprec<-1/(umids*umids)
          madsprec<-1/(umads*umads)
          
          for(i in 1:NWS){
            wadprec[i]<-1/(uwad[i]*uwad[i])
            wad[i]~dnorm(wadm[i],wadprec[i])
          }
          
          for(i in 1:N){mids[i]~dnorm(midsm[i],midsprec)
            mads[i]~dnorm(madsm[i],madsprec)
          }
          for(i in 1:N){wac[i]<-wad[wsol[i]]*mads[i]/mids[i]
          
          }  
          wacm<-mean(wac[])
          
          #######
          #calibration equation
          
          
          b~dnorm(0,1.0E-5)
          
          xins~dnorm(0,0.0016)%_%I(0.001,)
          chsqns~dgamma(0.5,0.5)
          fitprec<-xins/sqrt(chsqns)
          
          for(i in 1:N){mean[i]<-b*wac[i]
          predm[i]~dnorm(mean[i],fitprec)}
          for(i in 1:NT){rac[i]~dnorm(mean[sol[i]],fitprec)
          }
          vyp<-1/sqrt(fitprec)
          ###
          # Compute the mass fraction wd
          
          b.cut<-cut(b)
          sigras~dgamma(1.0E-5,1.0E-5)
          wdsig~dgamma(1.0E-3,1.0E-3)
          
          wd~dnorm(0,1.0E-5)
          for(i in 1:M){wdm[i]~dnorm(wd,wdsig)
            mdp[i]~dnorm(mdi[i],madsprec)
            midsip[i]~dnorm(midsi[i],midsprec)
            mult[i]<-wdm[i]*mdp[i]/midsip[i]
            wtop[i]<-mdp[i]/midsip[i]
            rasmean[i]<-b.cut*wdm[i]*mdi[i]/midsi[i]
            rasmeanp[i]~dnorm(rasmean[i],fitprec)}
          for(i in 1:MT){ras[i]~dnorm(rasmeanp[sampl[i]],sigras)}
          vsp<-1/sqrt(sigras) ### vs for DOE program
          vip<-sd(mult[])/mean(mult[]) ## vi for DOE program
          xnewp<-mean(mult[]) ## xnew for Doe program
        }
        
        ##################################################################
        ##################################################################
        ### Run OpenBUGS
        ##################################################################
        ##################################################################
        
        withProgress(message = 'Running Bayes', style="old",value = 0, {
          
          
          
          lineinits<-function(){list(sigras=1,wdsig=1,b=1)}
          
          lineout<-bugs(data=linedata,
                        inits=lineinits,
                        parameters=c("b","wd","wac","wacm","mean","vyp","vsp","mult","vip","xnewp","predm"),   
                        model.file=linemodel,
                        n.iter = input$niters, n.burnin = input$nburnin, n.thin = 10,n.chains = 1)#,debug=T)    
        })
        
        ######
        attach.bugs(lineout) ## imports the random draws for the parameters
        cl<-NULL
        
        for(i in 1:N){
          for(j in 1:NR){cl<-c(cl,quantile(predm[,i], 0.025))}}
        
        cl<-rep(unique(cl), NR)
        cu<-NULL
        for(i in 1:N){
          for(j in 1:NR){cu<-c(cu,quantile(predm[,i], 0.925))}}
        cu<-rep(unique(cu), NR)
        
        
        
        
        
        
        outregb<-data.frame(mean=lineout$mean$b,sd=lineout$sd$b)
        outwdm<-data.frame(mean=lineout$mean$wdm,sd=lineout$sd$wdm)
        outmean<-data.frame(mean=lineout$mean$mean)
        outwd<-data.frame(mean=lineout$mean$wd,sd=lineout$sd$wd )
        outvyp<-data.frame(mean=lineout$mean$vyp,sd=lineout$sd$vyp)
        outvsp<-data.frame(mean=lineout$mean$vsp,sd=lineout$sd$vsp)
        outvip<-data.frame(mean=lineout$mean$vip,sd=lineout$sd$vip)
        outxnew<-data.frame(mean=lineout$mean$xnewp)
        # wacmean<-data.frame(mean=mean(lineout$mean$wac))
        
        
        ########## sets up the data to plot the regression line and calculate the rsq
        
        wacm=lineout$mean$wacm #avarage of the wac
        outwac=lineout$mean$wac 
        eta<-lineout$sd$wac/lineout$mean$wac
        lwac<-rep(outwac,NR)
        
        
        ############ calculates the rsq
        ybar<-outmean
        sst<-sum((rac()-mean(rac()))^2)
        sse<-sum((rac()-ybar)^2)
        rsq<-1-sse/sst
        
        
        
        
        bayesres=lineout$sims.list$wd
        
        credible_interval=quantile(bayesres,c((1-input$coverage)/2, (1+input$coverage)/2))
        
        bind <-list(wadm=input$wadm,uwad=input$uwad,wacm=wacm,
                    u_mid=umids(),u_mad=input$u_mad,
                    u_mids=input$u_mids,u_mdi=input$u_mdi,
                    coverage=input$coverage,Number_iteration=input$niters,Length_burnin=input$nburnin,
                    mean=mean(bayesres),sd=sd(bayesres),median=median(bayesres),
                    credible.interval.L=credible_interval[1],credible.interval.H=credible_interval[2],
                    
                    b=outregb,
                    wd=outwd,
                    vyp=outvyp,
                    vsp=outvsp,
                    vip=outvip,
                    xnew=lineout$mean$xnewp
                    #wac=lineout$mean$wac,rux=lineout$sd$wac/lineout$mean$wac
                    
        )
        
        updateTabsetPanel(session = session, inputId = "results",selected = "Fit")
        
        
        
        ##### Run DOE Module
        
        
        a<-min(lineout$mean$wac)
        b<-max(lineout$mean$wac)
        
        vyll<-lineout$mean$vyp-2*lineout$sd$vyp
        vyuu<-lineout$mean$vyp+2*lineout$sd$vyp
        vyl<-(vyll)^2
        vyu<-(vyuu)^2
        
        vxll<-c("user's input")    #DOE vxl
        vxuu<-c("user's input")      #DOE vxu
        
        wacm<-lineout$mean$wacm
        
        betal<-lineout$mean$b-2*lineout$sd$b
        betau<-lineout$mean$b+2*lineout$sd$b
        
        etall<-min(lineout$sd$wac/lineout$mean$wac) #DOE etal
        etauu<-max(lineout$sd$wac/lineout$mean$wac) #DOE etau
        
        etal<-(etall)^2
        etau<-(etauu)^2
        
        vsll<-lineout$mean$vsp-2*lineout$sd$vsp #DOE vsl
        vsuu<-lineout$mean$vsp+2*lineout$sd$vsp #DOE vsu
        vsl<-(vsll)^2
        vsu<-(vsuu)^2
        
        xnew<-lineout$mean$xnewp 
        
        vill<-lineout$mean$vip-2*lineout$sd$vip #DOE vil
        viuu<-lineout$mean$vip+2*lineout$sd$vip #DOE viu
        vil<-(vill)^2
        viu<-(viuu)^2
        
        
        
        expout<-list(
          xnew=xnew,slope_min=betal,slope_max=betau,wacm=wacm,
          wac_min=a,wac_max=b,Ux_min=input$vxl,Ux_max=input$vxu,
          Uy_min=vyll,Uy_max=vyuu,rux_min=etall,rux_max=etauu,
          Us_min=vsll,Us_max=vsuu,Ui_min=vill,Ui_max=viuu)
        
        if(input$exp){
          
          vxll<-wacm     #DOE vxl
          vxuu<-c("user's input")    #DOE vxu
          
          ######### inputs through the shiny interface#########################
          #   if(N_tables==1){ 
          
          
          # totn<-input$doetotn
          # std<-input$doestd
          # 
          # vxl<-input$vxl       #DOE vxl
          # vxu<-input$vxu      #DOE vxu
          # 
          
          totn<-input$doetotn1
          std<-input$doestd1 ## required maximum relative std of the answer
          
          xnew<-xnew
          wacm<-wacm
          b <-betau
          
          
          
          vyl<-vyl
          vyh<-vyu
          
          
          vxl<-(input$vxl1)^2
          vxh<-(input$vxh1)^2
          
          lxl <- etal
          lxh<- etau
          
          vsl<-vsl
          vsh<-vsu
          
          
          lnewl<-vil
          lnewh<-viu
          
          #source("./Toman2019-ExpDesignRFF.R")
          
          #######
          expdesign2<-function(caln,totn,b,vyh,vxh,lxh,wacm,vsh,xnew,lnewh){
            
            
            caldes<-function(n,ny,b,vyh,vxh,lxh,wacm,vsh,xnew,lnewh){
              r<-1
              if(vsh>0 |lnewh>0){r=ny}
              vbhat<-1/n*((vyh+b^2*vxh)/wacm^2+lxh*b^2)
              sy<-(xnew^2*vbhat/b^2+vyh/ny/b^2+vsh/r/b^2+(b^2+vbhat)*xnew^2*lnewh/r/b^2)^0.5
              relsq<-sy/xnew*100
              totrep<-n+ny
              samplrep<-ny/r
              result<-list(samplrep=samplrep,nosamples=r,relsq=relsq,totrep=totrep)
              return(result)
            }
            
            
            optx<-NULL
            A<-NULL
            
            for(j in 1:caln){
              quantn<-totn-j
              if(quantn>0){
                for(i in 1:quantn){
                  des<-caldes(j,i,b,vyh,vxh,lxh,wacm,vsh,xnew,lnewh)
                  if(caln>j-1){A<-rbind(A,c(j,des$relsq,des$nosamples,des$samplrep,des$totrep))}
                }}}
            
            rowmin<-apply( A, 2, which.min)
            optI<-A[rowmin[2],1]
            opts<-A[rowmin[2],3]
            optq<-A[rowmin[2],4]
            optstd<-A[rowmin[2],2]
            
            result2<-list(optI=optI,opts=opts,optq=optq,optstd=optstd)
            return(result2)}
          
          
          ## required maximum relative std of the answer
          ###################################################################
          ### R code
          ###################################################################
          ### R code in server.R
          caln<-NULL
          totnm4<-totn-4
          for(i in 1:totnm4){caln[i]<-i+3}
          
          
          out<-NULL
          designh1<-expdesign2(caln[1],totn,b,vyh,vxh,lxh,wacm,vsh,xnew,lnewh)
          out<-rbind(out,c(as.integer(designh1$optI),designh1$opts,designh1$optq,designh1$optstd))
          for(i in 2:totnm4){if(out[i-1,4]>std){
            designh<-expdesign2(caln[i],totn,b,vyh,vxh,lxh,wacm,vsh,xnew,lnewh)
            out<-rbind(out,c(as.integer(designh$optI),designh$opts,designh$optq,designh$optstd)) 
          }else{break}}
          
          outng<-NULL
          outng<-out
          outg<-NULL
          while(dim(out)[1]<totnm4){outg<-out
          totn<-totn-1
          if(totn==0){break}
          out<-NULL
          designh1<-expdesign2(caln[1],totn,b,vyh,vxh,lxh,wacm,vsh,xnew,lnewh)
          out<-rbind(out,c(as.integer(designh1$optI),designh1$opts,designh1$optq,designh1$optstd))
          for(i in 2:totnm4){if(out[i-1,4]>std){
            designh<-expdesign2(caln[i],totn,b,vyh,vxh,lxh,wacm,vsh,xnew,lnewh)
            out<-rbind(out,c(as.integer(designh$optI),designh$opts,designh$optq,designh$optstd)) 
          }else{break}}
          dimg<-dim(outg)[1]
          
          outgood<-outg[outg[,4]<std,] ### this contains the experimental designs that satisfy the std requirement
          optI<-outgood[1] # optimal number of calibrants
          
          opts<-outgood[2]  # optimal number of samples in the quantitation experiment
          optq<-outgood[3] # optimal number of replicates per sample
          optstd<-outgood[4] # optimal expected relative standard deviation of the response
          }
          
          
          dimng<-dim(outng)[1]
          if(outng[dimng,4]>std){
            ################to be printed in shiny#################
            #shinyalert("Oops!", "Please increase the total number observations otherwise the best possible experimental design is:", type = "error")
            output$err<-renderUI({
              h4("Please increase the total number of  observations otherwise the  experimental design is:",style = "color:red")
            })
            #######################################################
            outbest<-outng[totnm4,]
            optIb<-outbest[1]  # number of calibrants
            
            optsb<-outbest[2]  # number of samples in the quantitation experiment
            optqb<-outbest[3] # number of replicates per sample
            optstdb<-outbest[4] # the smallest expected relative standard deviation of the response given your totn and std
            
            
            #results <- matrix(c(optxb),ncol =length(optxb), nrow = 1)
            # 
            # results2<-list(Calibrants=optIb,Samples=optsb,Sample_replicates=optqb)
            # 
            # optstdb <- matrix(c(optstdb))
            # #optstdb<-list(Standard_devaiation=optstdb)
            # colnames(optstdb) <- c("Expected Relative (%) Standard Deviation of Result")
            
            results2 <- matrix(c(optIb,optsb,optqb),ncol=3, nrow = 1)
            colnames(results2)<-c("Calibrants","Samples","Sample replicate")
            
            optstdb <- matrix(c(optstdb))
            
            colnames(optstdb) <- c("Expected Relative (%) Standard Deviation of Result")
            
            
            return(list(mcmcout=bayesres,bind=bind,rsq=rsq,expout=expout,
                        outb=outregb,rac=rac(),result=results2,wacm=wacm,
                        optstd=optstdb,std=std,message=message,wacsd=lineout$sd$wac,
                        waca=a,wacb=b,vyl=vyll,vyu=vyuu,vxl=vxll,vxu=vxuu,
                        betal=betal,betau=betau,etal=etall,etau=etauu,
                        vsl=vsll,vsu=vsuu,xnew=xnew,vil=vill,viu=viuu,
                        outeta=eta,outwac=outwac,outvyp=outvyp,outvsp=outvsp,outvip=outvip,outxnew=outxnew,
                        wac=lwac,linea=lineout$mean$a,lineb=lineout$mean$b,outwd=outwd,cl=cl,cu=cu))
            
            
            
            
            
            
            
            ######################################################
            
          }else{
            
            output$err<-renderUI({
              HTML("")
            })
            
            
            # results2<- list(Calibrants=optI,Samples=opts,Sample_replicate=optq)
            # 
            # optstd <- matrix(c(optstd))
            # #optstdb<-list(Standard_devaiation=optstdb)
            # colnames(optstd) <- c("Expected Relative (%) Standard Deviation of Result")
            
            results2 <- matrix(c(optI,opts,optq),ncol=3, nrow = 1)
            colnames(results2)<-c("Calibrants","Samples","Sample replicate")
            
            optstd <- matrix(c(optstd))
            
            colnames(optstd) <- c("Expected Relative (%) Standard Deviation of Result")
            
            
            return(list(mcmcout=bayesres,bind=bind,rsq=rsq,expout=expout,
                        outb=outregb,rac=rac(),result=results2,wacm=wacm,
                        optstd=optstd,wacsd=lineout$sd$wac,
                        waca=a,wacb=b,vyl=vyll,vyu=vyuu,vxl=vxll,vxu=vxuu,
                        betal=betal,betau=betau,etal=etall,etau=etauu,
                        vsl=vsll,vsu=vsuu,xnew=xnew,vil=vill,viu=viuu,
                        outeta=eta,outwac=outwac,outvyp=outvyp,outvsp=outvsp,outvip=outvip,outxnew=outxnew,
                        wac=lwac,linea=lineout$mean$a,lineb=lineout$mean$b,outwd=outwd,cl=cl,cu=cu))
            
            #######################################################
          }
          
        }
        
        
        return(list(mcmcout=bayesres,bind=bind,rsq=rsq,expout=expout,
                    outb=outregb,rac=rac(),wacsd=lineout$sd$wac,wacm=wacm,
                    waca=a,wacb=b,vyl=vyll,vyu=vyuu,vxl=vxll,vxu=vxuu,
                    betal=betal,betau=betau,etal=etall,etau=etauu,
                    vsl=vsll,vsu=vsuu,xnew=xnew,vil=vill,viu=viuu,
                    outeta=eta,outwac=outwac,outvyp=outvyp,outvsp=outvsp,outvip=outvip,outxnew=outxnew,
                    wac=lwac,linea=lineout$mean$a,lineb=lineout$mean$b,outwd=outwd,cl=cl,cu=cu))
        
        
        ##### RF NS>1
        
      }else{
        
        ####merge calibration data talbes
        caldata<-reactive({
          
          if (is.null(input$calfile))
            return()
          else
          {
            nfile<<-nrow(input$calfile)
            
            csv=list()
            
            for(i in 1: nfile)
            {
              csv[[i]]=read.csv(input$calfile$datapath[i])
              
            }
            
            do.call(rbind,csv) # rbind the csv
          }
          
        })
        
        ####merge sample data tables
        
        sampledata<-reactive({
          
          if (is.null(input$samplefile))
            return()
          else
          {
            nfile=nrow(input$samplefile)
            csv=list()
            for(i in 1: nfile)
            {
              csv[[i]]=read.csv(input$samplefile$datapath[i])
              
            }
            
            do.call(rbind,csv) # rbind the csv
          }
          
        })
        
        
        wadm<<-reactive(
          return(append(as.numeric(unlist(strsplit(gsub("\\s", "", input$wadm),","))),1))
          
        )
        
        uwad<<-reactive(
          return(append(as.numeric(unlist(strsplit(gsub("\\s", "", input$uwad),","))),1))
          
        )
        
        ####
        
        
        NWS <- reactive({
          
          inFile <- input$calfile
          if (is.null(input$calfile)){
            return(NULL)
          }else{
            
            
            tmp<- read.csv(inFile$datapath[1])
            
            return(max(tmp$wsol))# #working solutions from  calibrationtable
            
            
          }
        })
        
        standard<-reactive(
          input$standard
        )
        
        umids<-reactive({
          
          if (is.null(input$calfile))
            return()
          else
          {
            
            if (standard()==1){
              
              nfile<<-nrow(input$calfile)
              
              return(c(rep(input$u_mid,nfile)))
              
            }else{
              nfile<<-nrow(input$calfile)
              
              return(c(rep(1,nfile)))
              
            }
          }
        })
        
        umads<<-reactive(
          
          return(c(rep(input$u_mad,nfile)))
          
        )
        
        
        ####
        
        NS<-reactive({
          
          if (is.null(input$calfile))
            return()
          else
          {
            NS<-nrow(input$calfile)
            return(NS)
            
          }
          
        })
        
        
        
        NT<-reactive(
          
          length(caldata()$mid)
          
        )
        
        expc<-reactive({
          
          if (is.null(input$calfile))
            return()
          else
          {
            nfile<-nrow(input$calfile)
            
            
            expc=list()
            
            for(i in 1: nfile)
            {
              nr= nrow(read.csv(input$calfile$datapath[i]))
              expc[[i]]<-(c(rep(1*i,each=nr)))
              expc[[i]]<-t(expc[[i]])
            } 
            
            
            
            as.vector(do.call(cbind, expc))
            
            
          }
          
        })
        
        midsm<<-reactive({
          
          if(standard()==1){
            return(caldata()$mid)
            
          }else{
            
            return(caldata()$mad/caldata()$mad)
          }
        })
        
        
        
        madsm<<-reactive(
          caldata()$mad
        )
        
        wsol<<-reactive(
          caldata()$wsol
        )
        
        
        
        NTT <- reactive({
          
          inFile <- input$calfile
          if (is.null(input$calfile)){
            return(NULL)
          }else{
            
            
            tmp=list()
            
            for(i in 1: nfile)
            {
              tmp[[i]]=read.csv(input$calfile$datapath[i])
              
            }
            
            tmp<-do.call(rbind,tmp)
            
            NR<<-ncol(tmp)-3# number of replicates for each calibrant
            NT<-nrow(tmp)
            
            NTT<-NR*NT
            
          }
        })
        
        
        expc1<-reactive({
          
          if (is.null(input$calfile))
            return()
          else
          {
            nfile<-nrow(input$calfile)
            
            
            
            expc1=list()
            
            for(i in 1: nfile)
            {
              nr<<-nrow(read.csv(input$calfile$datapath[i]))
              nrac<-ncol(read.csv(input$calfile$datapath[i]))-3
              n1<-nr*nrac
              expc1[[i]]<-matrix(c(rep(1*i,each=n1)))
              
            } 
            
            
            as.vector(do.call(rbind, expc1))
            
            
          }
          
        })
        
        expc2<-reactive({
          
          if (is.null(input$calfile))
            return()
          else
          {
            nfile<-nrow(input$calfile)
            
            csv=list()
            
            k<-nrow(read.csv(input$calfile$datapath[1]))
            nrac<-ncol(read.csv(input$calfile$datapath[1]))-3
            
            
            csv[[1]]=c(rep(1:k,nrac))
            
            
            for(i in 2: nfile)
            {
              
              c=max(csv[[i-1]])+1
              x=nrow(read.csv(input$calfile$datapath[i]))
              y=c+x-1
              csv[[i]]=c(rep(c:y, nrac))
              
            }
            
            as.vector(unlist(csv))
            
            
            
          }
          
        })
        
        
        
        
        rac<- reactive({
          
          if (is.null(input$calfile))
            return()
          else
          {
            nfile<-nrow(input$calfile)
            
            csv=list()
            
            for(i in 1: nfile)
            {
              csv[[i]]=read.csv(input$calfile$datapath[i])
              csv[[i]][1:3]<-NULL
            }
            
            return(unlist(csv)) # rbind the csv
          }
          
        })
        
        
        ras<- reactive({
          
          if (is.null(input$samplefile))
            return()
          else
          {
            nfile<-nrow(input$samplefile)
            
            csv=list()
            
            for(i in 1: nfile)
            {
              csv[[i]]=read.csv(input$samplefile$datapath[i])
              csv[[i]][1:2]<-NULL
            }
            
            return(unlist(csv)) # rbind the csv
          }
          
        })
        
        
        mdi<<-reactive(
          
          mdi<-sampledata()$mdi
          
        )
        
        midsi<<-reactive(
          
          if(standard()==1){
            return(sampledata()$mids)
          }else{
            
            return(sampledata()$mdi/sampledata()$mdi)
            
          }
        )
        
        MT<<-reactive(
          
          length(sampledata()$mids)
          
        )
        
        MTT <- reactive({
          
          inFile <- input$samplefile
          if (is.null(input$samplefile)){
            return(NULL)
          }else{
            
            
            tmp=list()
            
            for(i in 1: nfile)
            {
              tmp[[i]]=read.csv(input$samplefile$datapath[i])
              
            }
            
            tmp<-do.call(rbind,tmp)
            
            NR<<-ncol(tmp)-2# number of replicates for each calibrant
            NT<-nrow(tmp)
            
            NTT<-NR*NT
            
          }
        })
        
        exp<-reactive({
          
          if (is.null(input$samplefile))
            return()
          else
          {
            nfile<-nrow(input$samplefile)
            
            
            exp=list()
            
            for(i in 1: nfile)
            {
              nr= nrow(read.csv(input$samplefile$datapath[i]))
              exp[[i]]<-(c(rep(1*i,each=nr)))
              exp[[i]]<-t(exp[[i]])
            } 
            
            as.vector(do.call(cbind, exp))
            
          }
          
        })
        
        
        expr<-reactive({
          
          if (is.null(input$samplefile))
            return()
          else
          {
            nfile<<-nrow(input$samplefile)
            
            
            
            expr=list()
            
            for(i in 1: nfile)
            {
              nr<<-nrow(read.csv(input$samplefile$datapath[i]))
              nrac<-ncol(read.csv(input$samplefile$datapath[i]))-2
              n1<-nr*nrac
              expr[[i]]<-matrix(c(rep(1*i,each=n1)))
              
            } 
            
            
            as.vector(do.call(rbind, expr))
            
            
          }
          
        })
        
        expr2<-reactive({
          
          if (is.null(input$samplefile))
            return()
          else
          {
            nfile<-nrow(input$samplefile)
            
            csv=list()
            
            k<-nrow(read.csv(input$samplefile$datapath[1]))
            nras<-ncol(read.csv(input$samplefile$datapath[1]))-2
            
            
            csv[[1]]=c(rep(1:k,nras))
            
            
            for(i in 2: nfile)
            {
              
              c=max(csv[[i-1]])+1
              x=nrow(read.csv(input$samplefile$datapath[i]))
              y=c+x-1
              csv[[i]]=c(rep(c:y, nras))
              
            }
            
            as.vector(unlist(csv))
            
            
            
          }
          
        })
        
        nfile<-reactive({
          
          if (!is.null(input$calfile))
          {
            nfile<-nrow(input$calfile)
          }
        })
        
        nrac<-reactive({
          
          if (!is.null(input$calfile))
          {
            nrac<-ncol(read.csv(input$calfile$datapath[1]))-3
            
          }
        })
        
        vecwac <<- reactive({
          
          if (!is.null(input$calfile))
          {
            
            vec=list()
            for(i in 1: nfile){
              vec[[i]]<-nrow(read.csv(input$calfile$datapath[i]))
            }
            return(vec)
          }
        })
        
        vecsample <- reactive({
          
          if (!is.null(input$samplefile))
          {
            
            vec=list()
            for(i in 1: nfile){
              vec[[i]]<-nrow(read.csv(input$samplefile$datapath[i]))
            }
            return(vec)
          }
        })
        
        
        
        vecrac <- reactive({
          
          if (!is.null(input$calfile))
          {
            
            vecrac=list()
            for(i in 1: nfile){
              vecrac[[i]]<-nrac()*nrow(read.csv(input$calfile$datapath[i]))
            }
            return(vecrac)
          }
        })
        
        vecras <- reactive({
          
          if (!is.null(input$samplefile))
          {
            
            vecras=list()
            for(i in 1: nfile){
              vecras[[i]]<-nrac()*nrow(read.csv(input$samplefile$datapath[i]))
            }
            return(vecras)
          }
        })
        
        linedata<-list(umids=umids(),
                       umads=umads(),
                       wadm=wadm(),
                       uwad=uwad(),
                       NWS=NWS()+1,
                       NS=NS(),
                       NT=NT(),
                       expc=expc(),
                       midsm=midsm(),
                       madsm=madsm(),
                       wsol=wsol(), 
                       NTT=NTT(), 
                       expc1=expc1(),
                       expc2=expc2(),
                       rac=rac(),
                       ras=ras(),
                       MT=MT(),MTT=MTT(),
                       exp=exp(),
                       midsi=midsi(),
                       mdi=mdi(),
                       expr=expr(),
                       expr2=expr2())
        
        
        require(R2OpenBUGS)
        # #   
        # #   ##################################################################
        # #   ##################################################################
        # #   ### Define the model
        # #   ##################################################################
        # #   ##################################################################
        # #   
        # #   
        linemodel<-function(){####this program uses NS number of input tables. 
          ### NT is the total number of elements of midsm and madsm (total number of calibrants). 
          ### expc gives the experiment designation for each element in midsm and madsm. NTT is the total number of measurements in the calibration experiment.
          ### expc1 gives the experiment label for each observation in rac, expc2 gives calibrant designation for each rac.
          ### MT gives the total number of samples, MTT gives the total number of observations in the quantitation experiment 
          ### exp gives the experiment designation for each sample, expr gives the experiment designation for each element in ras, 
          ### expr2 gives the sample designation for each element in ras.
          
          
          for(i in 1:NWS){wadprec[i]<-1/(uwad[i]*uwad[i])
          wad[i]~dnorm(wadm[i],wadprec[i])}
          wadmean<-mean(wad[])
          
          for(i in 1:NS){midsprec[i]<-1/(umids[i]*umids[i])
          madsprec[i]<-1/(umads[i]*umads[i])}
          
          
          for(i in 1:NT){
            mids[i]~dnorm(midsm[i],midsprec[expc[i]])
            mads[i]~dnorm(madsm[i],madsprec[expc[i]])
            ratio[i]<-mads[i]/mids[i] 
            wac[i]<-wad[wsol[i]]* ratio[i] } 
          
          #######
          #calibration equation
          for(i in 1:NS){
            
            b[i]~dnorm(0,1.0E-5)
            
            xins[i]~dnorm(0,0.0016)%_%I(0.001,)
            chsqns[i]~dgamma(0.5,0.5)
            fitprec[i]<-xins[i]/sqrt(chsqns[i])
            vyp[i]<-1/sqrt(fitprec[i])
            
          }
          
          for(i in 1:NTT){mean[i]<-b[expc1[i]]*wac[expc2[i]]
          rac[i]~dnorm(mean[i],fitprec[expc1[i]])
          predm[i]~dnorm(mean[i],fitprec[expc1[i]])
          
          }
          
          
          ###
          # Compute the mass fraction wd
          for(i in 1:NS){
            
            b.cut[i]<-cut(b[i])
            xinsi[i]~dnorm(0,0.0016)%_%I(0.001,)
            chsqnsi[i]~dgamma(0.5,0.5)
            sigras[i]<-xinsi[i]/sqrt(chsqnsi[i])
            vsp[i]<-1/sqrt(sigras[i])
            # hetp[i]<-vsp[i]/vyp[i]*100 
            
            xinsw[i]~dnorm(0,0.0016)%_%I(0.001,)
            chsqnsw[i]~dgamma(0.5,0.5)
            wdsig[i]<-xinsw[i]/sqrt(chsqnsw[i])
            wd[i]~dnorm(0,1.0E-5)}
          
          for(i in 1:MT){
            wdm[i]~dnorm(wd[exp[i]],wdsig[exp[i]])
            mdp[i]~dnorm(mdi[exp[i]],madsprec[exp[i]])
            midsip[i]~dnorm(midsi[exp[i]],midsprec[exp[i]])
            rasmean[i]<-b.cut[exp[i]]*wdm[i]*mdi[i]/midsi[i]
            rasmeanp[i]~dnorm(rasmean[i],fitprec[exp[i]])
            mult[i]<-(wdm[i]*mdp[i])/midsip[i]
            wtop[i]<-mdp[i]/midsip[i]
            
          }
          
          
          for(i in 1:MTT){
            ras[i]~dnorm(rasmeanp[expr2[i]],sigras[expr[i]])
            
          } 
          
          # 
          
          #############################################
          T~dcat(P[])
          P[1:NS]~ddirich(alpha[])
          for(i in 1:NS){alpha[i]<-1}
          mumeanfin<-wd[T]
          
          
        }
        
        ##################################################################
        ##################################################################
        ### Run OpenBUGS
        ##################################################################
        ##################################################################
        
        withProgress(message = 'Running Bayes', style="old",value = 0, {
          
          lineinits<-function(){list(sigras=c(1,1),wdsig=c(1,1),b=c(1,1),wd=c(0,0))}
          
          lineout<-bugs(data=linedata,
                        inits=lineinits,
                        parameters=c("b","wd","wac","vyp","vsp","mult","mean","predm","mumeanfin"),   
                        model.file=linemodel,
                        n.iter = 10000, n.burnin = 5000, n.thin = 10,n.chains =1)#,debug=T)
        })
        
        ###########################################
        ### additional result generation
        
        attach.bugs(lineout) ## imports the random draws for the parameters
        
        
        outregb<-data.frame(mean=lineout$mean$b,sd=lineout$sd$b)
        outwdm<-data.frame(mean=lineout$mean$wdm,sd=lineout$sd$wdm)
        outmean<-data.frame(mean=lineout$mean$mean)
        outwd<-data.frame(mean=lineout$mean$wd,sd=lineout$sd$wd )
        outmumeanfin<-data.frame(mean=lineout$mean$mumeanfin,sd=lineout$sd$mumeanfin )
        outhetp<-lineout$mean$hetp
        outvyp<-data.frame(mean=lineout$mean$vyp,sd=lineout$sd$vyp)
        outvsp<-data.frame(mean=lineout$mean$vsp,sd=lineout$sd$vsp)
        outmult<-lineout$mean$mult
        
        
        ########## sets up the data to plot the regression line and calculate the rsq
        
        
        
        require(data.table)
        
        
        ############ calculates the rsq
        
        ybar<-split(outmean,rep(1:nfile,vecrac()))
        rac_rsq<-split(rac(),rep(1:nfile,vecrac()))
        
        sst=list()
        sse=list()
        rsq=list()
        for(i in 1: nfile)
        {
          sst[[i]]<-sum((rac_rsq[[i]]-mean(rac_rsq[[i]]))^2)
          sse[[i]]<-sum((rac_rsq[[i]]-ybar[[i]])^2)
          rsq[[i]]<-1-(sse[[i]]/sst[[i]])
        }
        
        n_rsq<-unlist(rsq)
        
        
        ####calibration plot ##
        
        ##get wac vectors
        
        outwac=lineout$mean$wac
        eta<-lineout$sd$wac/lineout$mean$wac
        
        lwac<-split(outwac,rep(1:nfile,vecwac()))
        
        lwac<-lapply(lwac,rep,nrac())
        
        
        lrac<-split(rac(),rep(1:nfile,vecrac()))
        
        cl=NULL
        
        length=length(lineout$mean$predm)
        predm=lineout$sims.list$predm
        for(i in 1:length){
          cl[[i]]<-quantile(predm[,i],0.025)
        }
        
        cl<-split(cl,rep(1:nfile,vecrac()))
        
        cu=NULL
        for(i in 1:length){
          cu[[i]]<-quantile(predm[,i],0.925)
        }
        
        cu<-split(cu,rep(1:nfile,vecrac()))
        
        ####
        
        ###Regression output###
        # output vip and xnew
        outvip<-list()
        mult=outmult
        mult<-split(mult,rep(1:nfile,vecsample()))
        vilmean<-list()
        vilsd<-list()
        vill<-list()
        viuu<-list()
        vipp<-list()
        vipp<-lineout$sd$mult/lineout$mean$mult
        vipp<-split(vipp,rep(1:nfile,vecsample()))
        xnew<-list()
        wacm<-list()
        wacmean<-split(lineout$mean$wac,rep(1:nfile,vecwac()))
        for (i in 1:nfile){
          
          vilmean[[i]]=mean(vipp[[i]])
          vilsd[[i]]=sd(vipp[[i]])
          
          vill[[i]]=vilmean[[i]]-2*vilsd[[i]]
          viuu[[i]]=vilmean[[i]]+2*vilsd[[i]]
          xnew[[i]]=mean(mult[[i]])
          wacm[[i]]=mean(wacmean[[i]])
        }
        
        # vill<-unlist(vill)
        # viuu<-unlist(viuu)
        # vilmean(unlist(vilmean))
        # 
        # vilsd(unlist(vilsd))
        outvip<-data.frame(mean=unlist(vilmean),sd=(unlist(vilsd)))
        #outvip<-data.frame(mean=vilmean,sd=vilsd)
        outxnew<-data.frame(mean=unlist(xnew))
        outwacmean<-data.frame(mean=unlist(wacm))
        
        #### Output wac
        mulwac=outwac
        
        
        mulwac<-split(mulwac,rep(1:nfile,vecwac()))
        mulwac<-sapply(mulwac, `[`, seq_len(max(lengths(mulwac))))
        outwac<-matrix(mulwac,ncol = nfile,nrow = N)
        #mulwac[is.na(mulwac)] <- ""
        #output multi eta
        muleta=lineout$sd$wac/lineout$mean$wac
        muleta<-split(muleta,rep(1:nfile,vecwac()))
        muleta<-sapply(muleta, `[`, seq_len(max(lengths(muleta))))
        outeta<-matrix(muleta,ncol=nfile,nrow=N)
        #muleta[is.na(muleta)] <- ""
        ###
        
        bayesres=lineout$sims.list$mumeanfin
        
        credible_interval=quantile(bayesres,c((1-input$coverage)/2, (1+input$coverage)/2))
        
        bind <-list(wadm=input$wadm,wacm=unlist(wacm),uwad=input$uwad,u_mid=umids(),u_mad=input$u_mad,u_mids=input$u_mids,u_mdi=input$u_mdi,
                    coverage=input$coverage,Number_iteration=input$niters,Length_burnin=input$nburnin,
                    mean=mean(bayesres),sd=sd(bayesres),median=median(bayesres),
                    credible_interval1=credible_interval[1],credible_interval2=credible_interval[2],
                    R_squared=n_rsq,
                    
                    slope=outregb,
                    
                    wd=outwd,
                    vyp=outvyp,
                    vsp=outvsp,
                    vip=outvip,
                    xnew=unlist(xnew)
                    # mult=outmult
                    
        )
        
        updateTabsetPanel(session = session, inputId = "results",selected = "Fit")
        #waiter_hide()
        
        
        #####  DOE Inputs 
        
        wacab<-split(lineout$mean$wac,rep(1:nfile,vecwac()))
        vx<-split(lineout$sd$wac,rep(1:nfile,vecwac()))
        rux<-split(lineout$sd$wac/lineout$mean$wac,rep(1:nfile,vecwac()))
        etall<-list()
        etauu<-list()
        
        vxl<-list()
        vxu<-list()
        waca<-list()
        wacb<-list()
        vxll<-list()
        vxuu<-list()
        waca<-list()
        etalll<-list()
        etauuu<-list()
        for (i in 1:nfile){
          vxll[[i]]=min(vx[[i]])
          vxuu[[i]]=max(vx[[i]])
          
          waca[[i]]=min(wacab[[i]])
          wacb[[i]]=max(wacab[[i]])
          
          etalll[i]=min(rux[[i]])
          etauuu[i]=max(rux[[i]])
          
        }
        
        
        vyll<-lineout$mean$vyp-2*lineout$sd$vyp
        vylll<-lineout$mean$vyp-2*lineout$sd$vyp
        vyuu<-lineout$mean$vyp+2*lineout$sd$vyp
        vyuuu<-lineout$mean$vyp+2*lineout$sd$vyp
        
        betal<-lineout$mean$b-2*lineout$sd$b
        betall<-lineout$mean$b-2*lineout$sd$b
        betau<-lineout$mean$b+2*lineout$sd$b
        betauu<-lineout$mean$b+2*lineout$sd$b
        
        
        vxll<-c("user's input")      #DOE vxl
        vxuu<-c("user's input")
        
        vsll<-lineout$mean$vsp-2*lineout$sd$vsp #DOE vsl
        vslll<-lineout$mean$vsp-2*lineout$sd$vsp #DOE vsl
        vsuu<-lineout$mean$vsp+2*lineout$sd$vsp #DOE vsu
        vsuuu<-lineout$mean$vsp+2*lineout$sd$vsp #DOE vsu
        
        
        xneww<-outxnew
        
        
        expout<-list(
          xnew=unlist(xnew),wacm=unlist(wacm),
          slope_min=betal,slope_max=betau,
          wac_min=unlist(waca),wac_max=unlist(wacb),
          Uy_min=unlist(vylll),Uy_max=unlist(vyuuu),rux_min=unlist(etalll),rux_max=unlist(etauuu),
          Us_min=unlist(vslll),Us_max=unlist(vsuuu),Ui_min=unlist(vill),Ui_max=unlist(viuu)
        )
        wacmean=wacm
        if(input$exp){
          
          vxll<-c("user's input")     #DOE vxl
          vxuu<-c("user's input")    #DOE vxu
          
          doe1<-reactive(
            
            input$doe1
          )
          
          if(doe1()==1){
            
            totn<-input$doetotn1
            std<-input$doestd1
            xnew=xnew[[1]]
            b=betau[[1]]
            wacm=wacm[[1]]
            betal=betal[[1]]
            
            vyl=(vyll[[1]])^2
            
            vyh=(vyuu[[1]])^2
            
            vxl<-(input$vxl1)^2      #DOE vxl
            vxh<-(input$vxh1)^2     #DOE vxu
            
            # bsigl<-(input$bsigl1)^2 
            # bsigh<-(input$bsigh1)^2 
            
            lxl=(etalll[[1]])^2
            
            lxh=(etauuu[[1]])^2
            
            vsl=(vsll[[1]])^2
            
            vsh=(vsuu[[1]])^2
            
            
            
            lnewl=(vill[[1]])^2
            lnewh=(viuu[[1]])^2
            
            ########
            
            expdesign2<-function(caln,totn,b,vyh,vxh,lxh,wacm,vsh,xnew,lnewh){
              
              
              caldes<-function(n,ny,b,vyh,vxh,lxh,wacm,vsh,xnew,lnewh){
                r<-1
                if(vsh>0 |lnewh>0){r=ny}
                vbhat<-1/n*((vyh+b^2*vxh)/wacm^2+lxh*b^2)
                sy<-(xnew^2*vbhat/b^2+vyh/ny/b^2+vsh/r/b^2+(b^2+vbhat)*xnew^2*lnewh/r/b^2)^0.5
                relsq<-sy/xnew*100
                totrep<-n+ny
                samplrep<-ny/r
                result<-list(samplrep=samplrep,nosamples=r,relsq=relsq,totrep=totrep)
                return(result)
              }
              
              
              optx<-NULL
              A<-NULL
              
              for(j in 1:caln){
                quantn<-totn-j
                if(quantn>0){
                  for(i in 1:quantn){
                    des<-caldes(j,i,b,vyh,vxh,lxh,wacm,vsh,xnew,lnewh)
                    if(caln>j-1){A<-rbind(A,c(j,des$relsq,des$nosamples,des$samplrep,des$totrep))}
                  }}}
              
              rowmin<-apply( A, 2, which.min)
              optI<-A[rowmin[2],1]
              opts<-A[rowmin[2],3]
              optq<-A[rowmin[2],4]
              optstd<-A[rowmin[2],2]
              
              result2<-list(optI=optI,opts=opts,optq=optq,optstd=optstd)
              return(result2)}
            
            
            ## required maximum relative std of the answer
            ###################################################################
            ### R code
            ###################################################################
            ### R code in server.R
            caln<-NULL
            totnm4<-totn-4
            for(i in 1:totnm4){caln[i]<-i+3}
            
            
            out<-NULL
            designh1<-expdesign2(caln[1],totn,b,vyh,vxh,lxh,wacm,vsh,xnew,lnewh)
            out<-rbind(out,c(as.integer(designh1$optI),designh1$opts,designh1$optq,designh1$optstd))
            for(i in 2:totnm4){if(out[i-1,4]>std){
              designh<-expdesign2(caln[i],totn,b,vyh,vxh,lxh,wacm,vsh,xnew,lnewh)
              out<-rbind(out,c(as.integer(designh$optI),designh$opts,designh$optq,designh$optstd)) 
            }else{break}}
            
            outng<-NULL
            outng<-out
            outg<-NULL
            while(dim(out)[1]<totnm4){outg<-out
            totn<-totn-1
            if(totn==0){break}
            out<-NULL
            designh1<-expdesign2(caln[1],totn,b,vyh,vxh,lxh,wacm,vsh,xnew,lnewh)
            out<-rbind(out,c(as.integer(designh1$optI),designh1$opts,designh1$optq,designh1$optstd))
            for(i in 2:totnm4){if(out[i-1,4]>std){
              designh<-expdesign2(caln[i],totn,b,vyh,vxh,lxh,wacm,vsh,xnew,lnewh)
              out<-rbind(out,c(as.integer(designh$optI),designh$opts,designh$optq,designh$optstd)) 
            }else{break}}
            dimg<-dim(outg)[1]
            
            outgood<-outg[outg[,4]<std,] ### this contains the experimental designs that satisfy the std requirement
            optI<-outgood[1] # optimal number of calibrants
            
            opts<-outgood[2]  # optimal number of samples in the quantitation experiment
            optq<-outgood[3] # optimal number of replicates per sample
            optstd<-outgood[4] # optimal expected relative standard deviation of the response
            }
            
            
            dimng<-dim(outng)[1]
            if(outng[dimng,4]>std){
              ################to be printed in shiny#################
              output$err<-renderUI({
                h4("Please increase the total number of  observations otherwise the  experimental design is:",style = "color:red")
              })
              ######################################################
              outbest<-outng[totnm4,]
              optIb<-outbest[1]  # number of calibrants
              
              optsb<-outbest[2]  # number of samples in the quantitation experiment
              optqb<-outbest[3] # number of replicates per sample
              optstdb<-outbest[4] # the smallest expected relative standard deviation of the response given your totn and std
              
              #####
              results2 <- matrix(c(optIb,optsb,optqb),ncol=3, nrow = 1)
              colnames(results2)<-c("Calibrants","Samples","Sample replicate")
              
              optstdb <- matrix(c(optstdb))
              
              colnames(optstdb) <- c("Expected Relative (%) Standard Deviation of Result")
              
              
              ####
              
              return(list(mcmcout=bayesres,rsq=n_rsq,
                          outvyp=outvyp,bind=bind,expout=expout,wacm=unlist(wacmean),
                          wacsd=lineout$sd$wac,result=results2,optstd= optstdb,
                          waca=unlist(waca),wacb=unlist(wacb),vyl=vylll,vyu=vyuuu,vxl=vxll,vxu=vxuu,
                          betal=betall,betau=betauu,etal=unlist(etalll),etau=unlist(etauuu),
                          vsl=vslll,vsu=vsuuu,xnew=xneww,vil=unlist(vill),viu=unlist(viuu),
                          rac=lrac,wac=lwac,outwac=outwac,outeta=outeta,outvip=outvip,outxnew=outxnew,outbb=lineout$mean$b,lineb=lineout$mean$b,outb=outregb,cl=cl,cu=cu,
                          outvsp=outvsp,outwd=outwd,outmult=outmult))
              
            }else{
              
              
              output$err<-renderUI({
                HTML("")
              })
              
              
              results2 <- matrix(c(optI,opts,optq),ncol=3, nrow = 1)
              colnames(results2)<-c("Calibrants","Samples","Sample replicate")
              
              optstd <- matrix(c(optstd))
              
              colnames(optstd) <- c("Expected Relative (%) Standard Deviation of Result")
              
              
              
              return(list(mcmcout=bayesres,rsq=n_rsq,
                          outvyp=outvyp,bind=bind,expout=expout,wacm=unlist(wacmean),
                          wacsd=lineout$sd$wac,result=results2,optstd= optstd,
                          waca=unlist(waca),wacb=unlist(wacb),vyl=vylll,vyu=vyuuu,vxl=vxll,vxu=vxuu,
                          betal=betall,betau=betauu,etal=unlist(etalll),etau=unlist(etauuu),
                          vsl=vslll,vsu=vsuuu,xnew=xneww,vil=unlist(vill),viu=unlist(viuu),
                          rac=lrac,wac=lwac,outwac=outwac,outeta=outeta,outvip=outvip,outxnew=outxnew,outbb=lineout$mean$b,lineb=lineout$mean$b,outb=outregb,cl=cl,cu=cu,
                          outvsp=outvsp,outwd=outwd,outmult=outmult))
              
              
              #######################################################
            }
            
            
            
            
          }else{
            
            totn<-input$doetotn1
            std<-input$doestd1
            xnew=xnew[[2]]
            wacm=wacm[[2]]
            
            b=betau[[2]]
            
            betal=betal[[2]]
            
            vyl=(vyll[[2]])^2
            
            vyh=(vyuu[[2]])^2
            
            vxl<-(input$vxl1)^2      #DOE vxl
            vxh<-(input$vxh1)^2     #DOE vxu
            
            
            lxl=(etalll[[2]])^2
            
            lxh=(etauuu[[2]])^2
            
            vsl=(vsll[[2]])^2
            
            vsh=(vsuu[[2]])^2
            
            
            
            lnewl=(vill[[2]])^2
            lnewh=(viuu[[2]])^2
            
            ######
            expdesign2<-function(caln,totn,b,vyh,vxh,lxh,wacm,vsh,xnew,lnewh){
              
              
              caldes<-function(n,ny,b,vyh,vxh,lxh,wacm,vsh,xnew,lnewh){
                r<-1
                if(vsh>0 |lnewh>0){r=ny}
                vbhat<-1/n*((vyh+b^2*vxh)/wacm^2+lxh*b^2)
                sy<-(xnew^2*vbhat/b^2+vyh/ny/b^2+vsh/r/b^2+(b^2+vbhat)*xnew^2*lnewh/r/b^2)^0.5
                relsq<-sy/xnew*100
                totrep<-n+ny
                samplrep<-ny/r
                result<-list(samplrep=samplrep,nosamples=r,relsq=relsq,totrep=totrep)
                return(result)
              }
              
              
              optx<-NULL
              A<-NULL
              
              for(j in 1:caln){
                quantn<-totn-j
                if(quantn>0){
                  for(i in 1:quantn){
                    des<-caldes(j,i,b,vyh,vxh,lxh,wacm,vsh,xnew,lnewh)
                    if(caln>j-1){A<-rbind(A,c(j,des$relsq,des$nosamples,des$samplrep,des$totrep))}
                  }}}
              
              rowmin<-apply( A, 2, which.min)
              optI<-A[rowmin[2],1]
              opts<-A[rowmin[2],3]
              optq<-A[rowmin[2],4]
              optstd<-A[rowmin[2],2]
              
              result2<-list(optI=optI,opts=opts,optq=optq,optstd=optstd)
              return(result2)}
            
            
            ## required maximum relative std of the answer
            ###################################################################
            ### R code
            ###################################################################
            ### R code in server.R
            caln<-NULL
            totnm4<-totn-4
            for(i in 1:totnm4){caln[i]<-i+3}
            
            
            out<-NULL
            designh1<-expdesign2(caln[1],totn,b,vyh,vxh,lxh,wacm,vsh,xnew,lnewh)
            out<-rbind(out,c(as.integer(designh1$optI),designh1$opts,designh1$optq,designh1$optstd))
            for(i in 2:totnm4){if(out[i-1,4]>std){
              designh<-expdesign2(caln[i],totn,b,vyh,vxh,lxh,wacm,vsh,xnew,lnewh)
              out<-rbind(out,c(as.integer(designh$optI),designh$opts,designh$optq,designh$optstd)) 
            }else{break}}
            
            outng<-NULL
            outng<-out
            outg<-NULL
            while(dim(out)[1]<totnm4){outg<-out
            totn<-totn-1
            if(totn==0){break}
            out<-NULL
            designh1<-expdesign2(caln[1],totn,b,vyh,vxh,lxh,wacm,vsh,xnew,lnewh)
            out<-rbind(out,c(as.integer(designh1$optI),designh1$opts,designh1$optq,designh1$optstd))
            for(i in 2:totnm4){if(out[i-1,4]>std){
              designh<-expdesign2(caln[i],totn,b,vyh,vxh,lxh,wacm,vsh,xnew,lnewh)
              out<-rbind(out,c(as.integer(designh$optI),designh$opts,designh$optq,designh$optstd)) 
            }else{break}}
            dimg<-dim(outg)[1]
            
            outgood<-outg[outg[,4]<std,] ### this contains the experimental designs that satisfy the std requirement
            optI<-outgood[1] # optimal number of calibrants
            
            opts<-outgood[2]  # optimal number of samples in the quantitation experiment
            optq<-outgood[3] # optimal number of replicates per sample
            optstd<-outgood[4] # optimal expected relative standard deviation of the response
            }
            
            
            dimng<-dim(outng)[1]
            if(outng[dimng,4]>std){
              ################to be printed in shiny#################
              #######################################################
              outbest<-outng[totnm4,]
              optIb<-outbest[1]  # number of calibrants
              
              optsb<-outbest[2]  # number of samples in the quantitation experiment
              optqb<-outbest[3] # number of replicates per sample
              optstdb<-outbest[4] # the smallest expected relative standard deviation of the response given your totn and std
              
              ################to be printed in shiny#################
              #shinyalert("Oops!", "Please increase the total number observations otherwise the best possible experimental design is:", type = "error")
              output$err<-renderUI({
                h4("Please increase the total number of  observations otherwise the  experimental design is:",style = "color:red")
              })
              #######################################################
              
              #####
              results2 <- matrix(c(optIb,optsb,optqb),ncol=3, nrow = 1)
              colnames(results2)<-c("Calibrants","Samples","Sample replicate")
              
              optstdb <- matrix(c(optstdb))
              
              colnames(optstdb) <- c("Expected Relative (%) Standard Deviation of Result")
              
              ####
              
              
              
              return(list(mcmcout=bayesres,rsq=n_rsq,
                          outvyp=outvyp,bind=bind,expout=expout,wacm=unlist(wacmean),
                          wacsd=lineout$sd$wac,result=results2,optstd= optstdb,
                          waca=unlist(waca),wacb=unlist(wacb),vyl=vylll,vyu=vyuuu,vxl=vxll,vxu=vxuu,
                          betal=betall,betau=betauu,etal=unlist(etalll),etau=unlist(etauuu),
                          vsl=vslll,vsu=vsuuu,xnew=xneww,vil=unlist(vill),viu=unlist(viuu),
                          rac=lrac,wac=lwac,outwac=outwac,outeta=outeta,outvip=outvip,outxnew=outxnew,outbb=lineout$mean$b,lineb=lineout$mean$b,outb=outregb,cl=cl,cu=cu,
                          outvsp=outvsp,outwd=outwd,outmult=outmult))
              
            }else{
              
              
              output$err<-renderUI({
                HTML("")
              })
              
              results2 <- matrix(c(optI,opts,optq),ncol=3, nrow = 1)
              colnames(results2)<-c("Calibrants","Samples","Sample replicate")
              
              optstd <- matrix(c(optstd))
              
              colnames(optstd) <- c("Expected Relative (%) Standard Deviation of Result")
              
              
              
              return(list(mcmcout=bayesres,rsq=n_rsq,
                          outvyp=outvyp,bind=bind,expout=expout,wacm=unlist(wacmean),
                          wacsd=lineout$sd$wac,result=results2,optstd=optstd,
                          waca=unlist(waca),wacb=unlist(wacb),vyl=vylll,vyu=vyuuu,vxl=vxll,vxu=vxuu,
                          betal=betall,betau=betauu,etal=unlist(etalll),etau=unlist(etauuu),
                          vsl=vslll,vsu=vsuuu,xnew=xneww,vil=unlist(vill),viu=unlist(viuu),
                          rac=lrac,wac=lwac,outwac=outwac,outeta=outeta,outvip=outvip,outxnew=outxnew,outbb=lineout$mean$b,lineb=lineout$mean$b,outb=outregb,cl=cl,cu=cu,
                          outvsp=outvsp,outwd=outwd,outmult=outmult))
              
              
              #######################################################
            }
            
            
          }
          
          
        }
        
        
        
        return(list(mcmcout=bayesres,rsq=n_rsq,
                    outvyp=outvyp,bind=bind,expout=expout,wacm=unlist(wacmean),
                    wacsd=lineout$sd$wac,
                    waca=unlist(waca),wacb=unlist(wacb),vyl=vylll,vyu=vyuuu,vxl=vxll,vxu=vxuu,
                    betal=betall,betau=betauu,etal=unlist(etalll),etau=unlist(etauuu),
                    vsl=vslll,vsu=vsuuu,xnew=xneww,vil=unlist(vill),viu=unlist(viuu),
                    rac=lrac,wac=lwac,outwac=outwac,outeta=outeta,outvip=outvip,outxnew=outxnew,outbb=lineout$mean$b,lineb=lineout$mean$b,outb=outregb,cl=cl,cu=cu,
                    outvsp=outvsp,outwd=outwd,outmult=outmult))
      }
      
      
      
      
    }
    
  })
  
  ##################################################################
  ### Outputs of Bayesian analysis #################################
  ##################################################################
  
  ## TODO: delete all outputs when anything is changed in inputs
  
  output$mu_est <- renderText({
    bayesres=outbayes()[["mcmcout"]]
    paste(format(mean(bayesres),digits = input$digit+2))
  })
  
  
  output$mu_se <- renderText({
    bayesres=outbayes()[["mcmcout"]]
    paste(format(sd(bayesres),digits = input$digit ))
  })
  
  output$mu_median <- renderText({
    bayesres=outbayes()[["mcmcout"]]
    paste(format(median(bayesres),digits = input$digit+2))
  })
  
  
  output$coverageProbabilityPercentBayes <- renderText({
    paste(input$coverage*100,"%",sep="")
  })
  
  
  output$mu_quant <- renderText({
    bayesres=outbayes()[["mcmcout"]]
    credible.interval=quantile(bayesres,c((1-input$coverage)/2, (1+input$coverage)/2))
    
    paste(format(credible.interval[1],digits = input$digit+2),"to",format(credible.interval[2]))
  })
  
  
  output$muest<-renderUI({
    
    fluidRow(
      column(8,
             p("The posterior mean is: ",textOutput("mu_est",inline=T), type = 8,color = "seagreen"),
             p("The standard uncertainty is: ",textOutput("mu_se",inline=T)),
             p("The posterior median is: ",textOutput("mu_median",inline=T)),
             p("The ",textOutput("coverageProbabilityPercentBayes",inline=T),
               " credible interval ranges from: ",textOutput("mu_quant",inline=T))
      ))
    
  })
  
  
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(bayes=outbayes() )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  
  output$downloadbayesout <- downloadHandler(
    filename = "preexp_inputs&estimates.csv",
    content = function(file) {
      write.csv(outbayes()[["bind"]], file, row.names = FALSE)
    }
  )
  output$downloaddoeinput <- downloadHandler(
    filename = "DoEinput.csv",
    content = function(file) {
      write.csv(outbayes()[["expout"]], file, row.names = FALSE)
    }
  )
  
  ####calibration plot
  
  observeEvent(input$go,{
    
    choice<-reactive(
      input$choice
    )
    if (choice()==1){
      
      if (!is.null(input$calfile))
      {
        
        nfile<-nrow(input$calfile)
        
        if(nfile==1){
          
          output$plots <- renderUI({
            
            output$plot2<-renderPlot({
              plot(outbayes()[["wac"]],outbayes()[["rac"]],main = paste("Calibration Plot"),type="p",pch=19, col=c("forestgreen","tomato"),xlab="wac",ylab="rac")
              lines(outbayes()[["wac"]],outbayes()[["cl"]],type="l",col="steelblue",lty="dashed")
              lines(outbayes()[["wac"]],outbayes()[["cu"]],type="l",col="steelblue",lty="dashed")
              abline(a=outbayes()[["linea"]], b=outbayes()[["lineb"]],col="red")
            })
            plotOutput("plot2")
          })
          
          
          
        }else{
          
          output$plots <- renderUI({
            plot_output_list <- lapply(1:nfile, function(i) {
              plotname <- paste("plot", i, sep="")
              plotOutput(plotname, height = 400, width = 560)
            })
            do.call(tagList, plot_output_list)
          })
          
          lapply(1:nfile, function(i){
            output[[paste("plot", i, sep="") ]] <- renderPlot({
              plot(outbayes()[["wac"]][[i]],outbayes()[["rac"]][[i]],main = paste("Calibration Plot", i),type="p",pch=19, col=c("forestgreen","tomato"),xlab="wac",ylab="rac")
              lines(outbayes()[["wac"]][[i]],outbayes()[["cl"]][[i]],type="l",col="steelblue",lty="dashed")
              lines(outbayes()[["wac"]][[i]],outbayes()[["cu"]][[i]],type="l",col="steelblue",lty="dashed")
              abline(a=outbayes()[["outaa"]][[i]], b=outbayes()[["outbb"]][[i]],col="red")
            })
          })
          outwd=outbayes()[["outwd"]]
          outvyp=outbayes()[["outvyp"]]
          outvsp=outbayes()[["outvsp"]]
          output$outwd <- renderTable({outwd},striped = F,spacing = "m",align="c",digits = 5)
          output$outvyp <- renderTable({outvyp},striped = F,spacing = "m",align="c",digits = 5)
          output$outvsp <- renderTable({outvsp},striped = F,spacing = "m",align="c",digits = 5)
          # output vip and xnew
          mult<-list()
          vip<-list()
          xnew<-list()
          mult=outbayes()[["outmult"]]
          mult<-split(mult,rep(1:nfile,vecsample()))
          for (i in 1:nfile){
            vip[[i]]=sd(mult[[i]])/mean(mult[[i]])
            xnew[[i]]=mean(mult[[i]])
          }
          outvip<-data.frame(mean=unlist(vip))
          outxnew<-data.frame(mean=unlist(xnew))
          output$xnewxnew<-renderTable(outxnew)
          
          
          output$outvip <- renderTable({outvip},striped = F,spacing = "m",align="c",digits = 6)
          output$outxnew <- renderTable({outxnew},striped = F,spacing = "m",align="c",digits = 6)
          #### Output wac
          mulwac=outbayes()[["outwac"]]
          mulwac<-split(mulwac,rep(1:nfile,vecwac()))
          mulwac<-sapply(mulwac, `[`, seq_len(max(lengths(mulwac))))
          outwac<-matrix(mulwac,ncol = nfile,nrow = N)
          #mulwac[is.na(mulwac)] <- ""
          output$outwac <- renderTable({outwac},striped = F,spacing = "m",align="c",digits = 6)
          #output multi eta
          muleta=outbayes()[["outeta"]]
          muleta<-split(muleta,rep(1:nfile,vecwac()))
          muleta<-sapply(muleta, `[`, seq_len(max(lengths(muleta))))
          outeta<-matrix(muleta,ncol=nfile,nrow=N)
          #muleta[is.na(muleta)] <- ""
          output$outeta <- renderTable({outeta},striped = F,spacing = "m",align="c",digits = 6)
          
          
        }
      }
      
      ####Pre RF Module###
    }else{
      if (!is.null(input$calfile))
      {
        
        nfile<-nrow(input$calfile)
        
        if(nfile==1){
          ymax=1.2*max(outbayes()[["rac"]])
          xmax=1.2*max(outbayes()[["wac"]])
          output$plots <- renderUI({
            
            output$plot2<-renderPlot({
              plot(outbayes()[["wac"]],outbayes()[["rac"]],main = paste("Calibration Plot"),type="p",pch=19, col=c("forestgreen","tomato"),xlab="wac",ylab="rac",xlim=c(0,xmax),ylim=c(0,ymax))
              abline(a=0, b=outbayes()[["lineb"]],col="red")
            })
            plotOutput("plot2")
          })
          
        }else{
          
          ymax=list()
          xmax=list()
          for(i in 1: nfile){
            ymax[[i]]=1.2*max(outbayes()[["rac"]][[i]])
            xmax[[i]]=1.2*max(outbayes()[["wac"]][[i]])
          }
          
          output$plots <- renderUI({
            plot_output_list <- lapply(1:nfile, function(i) {
              
              plotname <- paste("plot", i, sep="")
              plotOutput(plotname, height = 400, width = 600)
            })
            do.call(tagList, plot_output_list)
          })
          
          lapply(1:nfile, function(i){
            output[[paste("plot", i, sep="") ]] <- renderPlot({
              plot(outbayes()[["wac"]][[i]],outbayes()[["rac"]][[i]],main = paste("Calibration Plot", i),type="p",pch=19, col=c("forestgreen","tomato"),xlab="wac",ylab="rac",xlim=c(0,xmax[[i]]),ylim=c(0,ymax[[i]]))
              abline(a=0, b=outbayes()[["lineb"]][[i]],col="red")
            })
          })
        }
      }
    }
  })
  
  ##### LR model
  
  observeEvent(input$go,{
    
    choice<-reactive(
      input$choice
    )
    if (choice()==1){
      
      if(nfile==1){
        
        if(input$exp){
          
          output$doeout<-renderUI({
            
            output$optnum<-renderTable(outbayes()[["result"]],spacing=c( "l"),align="l",digits = 0)
            output$optxb<-renderTable(outbayes()[["optxb"]],spacing=c( "l"),align="l")
            output$optstd<-renderTable(outbayes()[["optstd"]],spacing=c( "l"),align="l")
            tagList(
              
              fluidRow(style='margin: 0px;',
                       
                       
                       tableOutput("optnum"),
                       
                       tableOutput("optxb"),
                       
                       tableOutput("optstd"),
                       hr()
              )
              
            )
            
          })
        }
        
        
        output$out1 = renderUI ({
          
          output$rsq <- renderTable({outbayes()[["rsq"]]},striped = F,spacing = "m",align="c",digits = 5)
          output$outrega <- renderTable({outbayes()[["outa"]]},striped = F,spacing = "m",align="c",digits = 5)
          output$outregb <- renderTable({outbayes()[["outb"]]},striped = F,spacing = "m",align="c",digits = 5)
          output$outwac <- renderTable({outbayes()[["outwac"]]},striped = F,spacing = "m",align="c",digits = 5)
          output$outeta <- renderTable({outbayes()[["outeta"]]},striped = F,spacing = "m",align="c",digits = 5)
          output$outwd <- renderTable({outbayes()[["outwd"]]},striped = F,spacing = "m",align="c",digits = 5)
          output$outvyp <- renderTable({outbayes()[["outvyp"]]},striped = F,spacing = "m",align="c",digits = 5)
          output$outvsp <- renderTable({outbayes()[["outvsp"]]},striped = F,spacing = "m",align="c",digits = 5)
          output$outvip <- renderTable({outbayes()[["outvip"]]},striped = F,spacing = "m",align="c",digits = 5)
          output$outxnew <- renderTable({outbayes()[["outxnew"]]},striped = F,spacing = "m",align="c",digits = 5)
          output$wac <- renderTable(list(Min={outbayes()[["waca"]]},Max={outbayes()[["wacb"]]}),striped = F,spacing = "m",align="c",digits = 5)
          output$xnew <- renderTable({outbayes()[["xnew"]]},striped = F,spacing = "m",align="c",digits = 5)
          #    output$wacsd <- renderTable({outbayes()[["wacsd"]]},striped = F,spacing = "m",align="c",digits = 5)
          #output$wacm <- renderTable(list(mean={outbayes()[["wacm"]]}),striped = F,spacing = "m",align="c",digits = 5)
          #     
          output$beta <- renderTable(list(Min={outbayes()[["betal"]]},Max={outbayes()[["betau"]]}),striped = F,spacing = "m",align="c",digits = 5)
          output$vy <- renderTable(list(Min={outbayes()[["vyl"]]},Max={outbayes()[["vyu"]]}),striped = F,spacing = "m",align="c",digits = 5)
          output$vi <- renderTable(list(Min={outbayes()[["vil"]]},Max={outbayes()[["viu"]]}),striped = F,spacing = "m",align="c",digits = 5)
          output$vx <- renderTable(list(Min={outbayes()[["vxl"]]},Max={outbayes()[["vxu"]]}),striped = F,spacing = "m",align="c",digits = 5)
          output$rux <- renderTable(list(Min={outbayes()[["etal"]]},Max={outbayes()[["etau"]]}),striped = F,spacing = "m",align="c",digits = 5)
          output$vs <- renderTable(list(Min={outbayes()[["vsl"]]},Max={outbayes()[["vsu"]]}),striped = F,spacing = "m",align="c",digits = 5)
          
          #     
          tagList(
            #      
            fluidRow(
              column(3,
                     p("R-squared: ",tableOutput("rsq"))),
              column(4,
                     p("Intercept: ",tableOutput("outrega"))),
              column(4,
                     p("Slope: ",tableOutput("outregb"))),
            ),
            #       
            br(),
            #       fluidRow(
            #         column(4,
            #                p("Wd: ",tableOutput("outwd"))
            #         ),
            #         column(4,
            #                p("Uy: ",tableOutput("outvyp"))
            #         ),
            #         column(4,
            #                p("Us: ",tableOutput("outvsp"))
            #         )),
            # 
            #         fluidRow(
            # 
            #         column(4,
            #                p("Ui: ",tableOutput("outvip"))
            #         ),
            # 
            #          column(4,
            #                 p("Xnew: ",tableOutput("outxnew"))
            # 
            #         )
            #         ),
            # #       
            #       fluidRow(
            # 
            #         column(4,
            #                p("Ratio of masses of A to I in calibration: ",tableOutput("outwac"))
            #         ),
            # 
            #         column(4,
            #                p("Rux: ",tableOutput("outeta"))
            #         )),
            
            
            hr(),
            h4("The estimated parameters for the design of experiment(the 95% credible interval): ",style = "color:seagreen"),
            fluidRow(
              column(6,
                     p("The expected approximate value of the measurand,quantity of A",tableOutput("xnew"))
              ),
              column(6,
                     p("The Slope of calibration line",tableOutput("beta"))
              )
            ),
            
            fluidRow(
              column(6,
                     p("Target calibration region;x-axis range in terms of concentrations of A:I in the calibration solutions",tableOutput("wac"))
              ),
              column(6,
                     p("Expected standard deviation of peak area ratios (A:I) from a calibration solution; (uy)",tableOutput("vy"))
                     
              )),
            
            fluidRow(
              column(6,
                     p("Expected standard uncertainty of the concentration ratios of A:I in the calibration experiment; (ux)",tableOutput("vx"))
              ),
              column(6,
                     p("Expected relative standard uncertainty of the concentration of I in samples measured for the quantitation experiment; (ui)",tableOutput("vi"))
                     
              )),
            
            fluidRow(
              column(6,
                     p("Expected relative standard uncertainty of the concentration ratios of A:I in the calibration experiment (rux)",tableOutput("rux"))
              ),
              column(6,
                     p("Expected between-sample variability (standard deviation) of peak area ratios (A:I) from the quantitation experiment(us)",tableOutput("vs"))
                     
              ))
            
            
            #     ###
          )
        })
      }else{###LR N>1###
        
        if(input$exp){
          
          output$doeout<-renderUI({
            
            output$optnum<-renderTable(outbayes()[["result"]],spacing=c( "l"),align="l",digits = 0)
            output$optxb<-renderTable(outbayes()[["optxb"]],spacing=c( "l"),align="l")
            output$optstd<-renderTable(outbayes()[["optstd"]],spacing=c( "l"),align="l")
            tagList(
              
              fluidRow(style='margin: 0px;',
                       
                       
                       tableOutput("optnum"),
                       
                       tableOutput("optxb"),
                       
                       tableOutput("optstd"),
                       hr()
              )
              
            )
            
          })
        }
        
        output$rsq <- renderTable({outbayes()[["rsq"]]},striped = F,spacing = "m",align="c",digits = 5)
        output$outrega <- renderTable({outbayes()[["outa"]]},striped = F,spacing = "m",align="c",digits = 5)
        output$outregb <- renderTable({outbayes()[["outb"]]},striped = F,spacing = "m",align="c",digits = 5)
        output$outwd <- renderTable({outbayes()[["outwd"]]},striped = F,spacing = "m",align="c",digits = 5)
        output$outvyp <- renderTable({outbayes()[["outvyp"]]},striped = F,spacing = "m",align="c",digits = 5)
        output$outvsp <- renderTable({outbayes()[["outvsp"]]},striped = F,spacing = "m",align="c",digits = 5)
        output$outvip <- renderTable({outbayes()[["outvip"]]},striped = F,spacing = "m",align="c",digits = 6)
        output$outxnew <- renderTable({outbayes()[["outxnew"]]},striped = F,spacing = "m",align="c",digits = 6)
        output$outwac <- renderTable({outbayes()[["outwac"]]},striped = F,spacing = "m",align="c",digits = 6)
        output$outeta <- renderTable({outbayes()[["outeta"]]},striped = F,spacing = "m",align="c",digits = 6)
        
        output$wac <- renderTable(list(Min={outbayes()[["waca"]]},Max={outbayes()[["wacb"]]}),striped = F,spacing = "m",align="c",digits = 5)
        output$xnew <- renderTable({outbayes()[["xnew"]]},striped = F,spacing = "m",align="c",digits = 5)
        #    output$wacsd <- renderTable({outbayes()[["wacsd"]]},striped = F,spacing = "m",align="c",digits = 5)
        
        #     
        output$beta <- renderTable(list(Min={outbayes()[["betal"]]},Max={outbayes()[["betau"]]}),striped = F,spacing = "m",align="c",digits = 5)
        output$vy <- renderTable(list(Min={outbayes()[["vyl"]]},Max={outbayes()[["vyu"]]}),striped = F,spacing = "m",align="c",digits = 5)
        output$vi <- renderTable(list(Min={outbayes()[["vil"]]},Max={outbayes()[["viu"]]}),striped = F,spacing = "m",align="c",digits = 5)
        output$vx <- renderTable(list(Min={outbayes()[["vxl"]]},Max={outbayes()[["vxu"]]}),striped = F,spacing = "m",align="c",digits = 5)
        output$rux <- renderTable(list(Min={outbayes()[["etal"]]},Max={outbayes()[["etau"]]}),striped = F,spacing = "m",align="c",digits = 5)
        output$vs <- renderTable(list(Min={outbayes()[["vsl"]]},Max={outbayes()[["vsu"]]}),striped = F,spacing = "m",align="c",digits = 5)
        
        
        output$out1 = renderUI ({
          
          
          tagList(
            fluidRow(
              
              column(4,
                     p("R-squared: ",tableOutput("rsq"))),
              column(4,
                     p("Intercept: ",tableOutput("outrega"))),
              column(4,
                     p("Slope: ",tableOutput("outregb")))
            ),
            
            # br(),
            # fluidRow(
            #   column(4,
            #          p("Wd: ",tableOutput("outwd"))
            #   ),
            #   column(4,
            #          p("Uy: ",tableOutput("outvyp"))
            #   ),
            #   column(4,
            #          p("Us: ",tableOutput("outvsp"))
            #   )),
            # fluidRow(
            #   column(4,
            #          p("Ui: ",tableOutput("outvip"))
            #   ),
            #   column(4,
            #          p("Xnew: ",tableOutput("outxnew"))
            #   )),
            # 
            # fluidRow(
            #   column(6,
            #          p("Ratio of masses of A to I in calibration: ",tableOutput("outwac"))
            #   ),
            #   column(6,
            #          p("Rux: ",tableOutput("outeta"))
            #   )),
            # 
            hr(),
            h4("The estimated parameters for the design of experiment(the 95% credible interval): ",style = "color:seagreen"),
            fluidRow(
              column(6,
                     p("The expected approximate value of the measurand,quantity of A",tableOutput("xnew"))
              ),
              column(6,
                     p("The Slope of calibration line",tableOutput("beta"))
              )
            ),
            
            fluidRow(
              column(6,
                     p("Target calibration region;x-axis range in terms of concentrations of A:I in the calibration solutions",tableOutput("wac"))
              ),
              column(6,
                     p("Expected standard deviation of peak area ratios (A:I) from a calibration solution; (uy)",tableOutput("vy"))
                     
              )),
            
            fluidRow(
              column(6,
                     p("Expected standard uncertainty of the concentration ratios of A:I in the calibration experiment; (ux)",tableOutput("vx"))
              ),
              column(6,
                     p("Expected relative standard uncertainty of the concentration of I in samples measured for the quantitation experiment; (ui)",tableOutput("vi"))
                     
              )),
            
            fluidRow(
              column(6,
                     p("Expected relative standard uncertainty of the concentration ratios of A:I in the calibration experiment (rux)",tableOutput("rux"))
              ),
              column(6,
                     p("Expected between-sample variability (standard deviation) of peak area ratios (A:I) from the quantitation experiment(us)",tableOutput("vs"))
                     
              ))
            
          )
        })
        
      }
      
      ###RF model  
    }else{
      
      if(nfile==1){
        
        if(input$exp){
          output$doeout<-renderUI({
            
            output$optnum<-renderTable(outbayes()[["result"]],spacing=c( "l"),align="l",digits = 0)
            # output$optxb<-renderTable(outbayes()[["optxb"]],spacing=c( "l"),align="l")
            output$optstd<-renderTable(outbayes()[["optstd"]],spacing=c( "l"),align="l")
            
            tagList(
              
              fluidRow(style='margin: 0px;',
                       
                       
                       tableOutput("optnum"),
                       
                       # tableOutput("optxb"),
                       
                       tableOutput("optstd"),
                       hr()
              )
              
            )
            
          })
          
        }
        output$out1 = renderUI ({
          
          output$rsq <- renderTable({outbayes()[["rsq"]]},striped = F,spacing = "m",align="c",digits = 5)
          output$outrega <- renderTable({outbayes()[["outa"]]},striped = F,spacing = "m",align="c",digits = 5)
          output$outregb <- renderTable({outbayes()[["outb"]]},striped = F,spacing = "m",align="c",digits = 5)
          output$outwac <- renderTable({outbayes()[["outwac"]]},striped = F,spacing = "m",align="c",digits = 5)
          output$outeta <- renderTable({outbayes()[["outeta"]]},striped = F,spacing = "m",align="c",digits = 5)
          output$outwd <- renderTable({outbayes()[["outwd"]]},striped = F,spacing = "m",align="c",digits = 5)
          output$outvyp <- renderTable({outbayes()[["outvyp"]]},striped = F,spacing = "m",align="c",digits = 5)
          output$outvsp <- renderTable({outbayes()[["outvsp"]]},striped = F,spacing = "m",align="c",digits = 5)
          output$outvip <- renderTable({outbayes()[["outvip"]]},striped = F,spacing = "m",align="c",digits = 5)
          output$outxnew <- renderTable({Mean=outbayes()[["outxnew"]]},striped = F,spacing = "m",align="c",digits = 5)
          #output$wac <- renderTable(list(Mean={outbayes()[["wacb"]]}),striped = F,spacing = "m",align="c",digits = 5)
          #    output$wacsd <- renderTable({outbayes()[["wacsd"]]},striped = F,spacing = "m",align="c",digits = 5)
          output$wacm <- renderTable(list(Mean={outbayes()[["wacm"]]}),striped = F,spacing = "m",align="c",digits = 5)
          output$xnew <- renderTable(list(Mean={outbayes()[["xnew"]]}),striped = F,spacing = "m",align="c",digits = 5)
          
          #     
          output$beta <- renderTable(list(Mean={outbayes()[["betau"]]}),striped = F,spacing = "m",align="c",digits = 5)
          output$vy <- renderTable(list(Min={outbayes()[["vyl"]]},Max={outbayes()[["vyu"]]}),striped = F,spacing = "m",align="c",digits = 5)
          output$vi <- renderTable(list(Min={outbayes()[["vil"]]},Max={outbayes()[["viu"]]}),striped = F,spacing = "m",align="c",digits = 5)
          output$vx <- renderTable(list(Min="user's input",Max="user's input"),striped = F,spacing = "m",align="c",digits = 5)
          output$rux <- renderTable(list(Min={outbayes()[["etal"]]},Max={outbayes()[["etau"]]}),striped = F,spacing = "m",align="c",digits = 5)
          output$vs <- renderTable(list(Min={outbayes()[["vsl"]]},Max={outbayes()[["vsu"]]}),striped = F,spacing = "m",align="c",digits = 5)
          
          tagList(
            fluidRow(
              column(6,
                     p("Response Factor:  ",tableOutput("outregb")))
            ),
            
            br(),
            # fluidRow(
            #   column(4,
            #          p("Wd: ",tableOutput("outwd"))
            #   ),
            #   column(4,
            #          p("Uy: ",tableOutput("outvyp"))
            #   ),
            #   column(4,
            #          p("Us: ",tableOutput("outvsp"))
            #   )),
            # 
            # fluidRow(
            #   column(4,
            #          p("Ui: ",tableOutput("outvip"))
            #   ),
            #   column(4,
            #          p("Xnew: ",tableOutput("outxnew"))
            #   )),
            # fluidRow(
            #   column(6,
            #          p("Ratio of masses of A to I in calibration: ",tableOutput("outwac"))
            #   ),
            #   column(6,
            #          p("Rux: ",tableOutput("outeta"))
            #   )),
            # 
            
            hr(),
            h4("The estimated parameters for the design of experiment(the 95% credible interval): ",style = "color:seagreen"),
            fluidRow(
              column(6,
                     p("The expected approximate value of the measurand,quantity of A",tableOutput("xnew"))
              ),
              column(6,
                     p("The expected Response Factor value",tableOutput("beta"))
              )
            ),
            
            fluidRow(
              column(6,
                     p("Target calibration region;x-axis range in terms of concentrations of A:I in the calibration solutions",tableOutput("wacm"))
              ),
              column(6,
                     p("Expected standard deviation of peak area ratios (A:I) from a calibration solution; (uy)",tableOutput("vy"))
                     
              )),
            
            fluidRow(
              column(6,
                     p("Expected standard uncertainty of the concentration ratios of A:I in the calibration experiment; (ux)",tableOutput("vx"))
              ),
              column(6,
                     p("Expected relative standard uncertainty of the concentration of I in samples measured for the quantitation experiment; (ui)",tableOutput("vi"))
                     
              )),
            
            fluidRow(
              column(6,
                     p("Expected relative standard uncertainty of the concentration ratios of A:I in the calibration experiment (rux)",tableOutput("rux"))
              ),
              column(6,
                     p("Expected between-sample variability (standard deviation) of peak area ratios (A:I) from the quantitation experiment(us)",tableOutput("vs"))
                     
              ))
          )
        })
        
        ####RF N>1
      }else{
        
        if(input$exp){
          
          output$doeout<-renderUI({
            
            output$optnum<-renderTable(outbayes()[["result"]],spacing=c( "l"),align="l",digits = 0)
            output$optxb<-renderTable(outbayes()[["optxb"]],spacing=c( "l"),align="l")
            output$optstd<-renderTable(outbayes()[["optstd"]],spacing=c( "l"),align="l")
            tagList(
              
              fluidRow(style='margin: 0px;',
                       
                       
                       tableOutput("optnum"),
                       
                       tableOutput("optxb"),
                       
                       tableOutput("optstd"),
                       hr()
              )
              
            )
            
          })
        }
        
        output$rsq <- renderTable({outbayes()[["rsq"]]},striped = F,spacing = "m",align="c",digits = 5)
        output$outrega <- renderTable({outbayes()[["outa"]]},striped = F,spacing = "m",align="c",digits = 5)
        output$outregb <- renderTable({outbayes()[["outb"]]},striped = F,spacing = "m",align="c",digits = 5)
        output$outwd <- renderTable({outbayes()[["outwd"]]},striped = F,spacing = "m",align="c",digits = 5)
        output$outvyp <- renderTable({outbayes()[["outvyp"]]},striped = F,spacing = "m",align="c",digits = 5)
        output$outvsp <- renderTable({outbayes()[["outvsp"]]},striped = F,spacing = "m",align="c",digits = 5)
        output$outvip <- renderTable({outbayes()[["outvip"]]},striped = F,spacing = "m",align="c",digits = 6)
        output$outxnew <- renderTable({outbayes()[["outxnew"]]},striped = F,spacing = "m",align="c",digits = 6)
        output$outwac <- renderTable({outbayes()[["outwac"]]},striped = F,spacing = "m",align="c",digits = 6)
        output$outeta <- renderTable({outbayes()[["outeta"]]},striped = F,spacing = "m",align="c",digits = 6)
        
        output$outxnew <- renderTable({outbayes()[["outxnew"]]},striped = F,spacing = "m",align="c",digits = 6)
        output$outwac <- renderTable({outbayes()[["outwac"]]},striped = F,spacing = "m",align="c",digits = 6)
        output$outeta <- renderTable({outbayes()[["outeta"]]},striped = F,spacing = "m",align="c",digits = 6)
        output$wacm <- renderTable(list(Mean={outbayes()[["wacm"]]}),striped = F,spacing = "m",align="c",digits = 5)
        output$wac <- renderTable(list(Mean={outbayes()[["wacb"]]}),striped = F,spacing = "m",align="c",digits = 5)
        output$xnew <- renderTable(list(Mean={outbayes()[["xnew"]]}),striped = F,spacing = "m",align="c",digits = 5)
        #    output$wacsd <- renderTable({outbayes()[["wacsd"]]},striped = F,spacing = "m",align="c",digits = 5)
        
        #     
        output$beta <- renderTable(list(Mean={outbayes()[["betau"]]}),striped = F,spacing = "m",align="c",digits = 5)
        output$vy <- renderTable(list(Min={outbayes()[["vyl"]]},Max={outbayes()[["vyu"]]}),striped = F,spacing = "m",align="c",digits = 5)
        output$vi <- renderTable(list(Min={outbayes()[["vil"]]},Max={outbayes()[["viu"]]}),striped = F,spacing = "m",align="c",digits = 5)
        output$vx <- renderTable(list(Min={outbayes()[["vxl"]]},Max={outbayes()[["vxu"]]}),striped = F,spacing = "m",align="c",digits = 5)
        output$rux <- renderTable(list(Min={outbayes()[["etal"]]},Max={outbayes()[["etau"]]}),striped = F,spacing = "m",align="c",digits = 5)
        output$vs <- renderTable(list(Min={outbayes()[["vsl"]]},Max={outbayes()[["vsu"]]}),striped = F,spacing = "m",align="c",digits = 5)
        
        
        output$out1 = renderUI ({
          
          
          tagList(
            fluidRow(
              
              column(4,
                     p("Response Factor: ",tableOutput("outregb")))
            ),
            
            br(),
            # fluidRow(
            #   column(4,
            #          p("Wd: ",tableOutput("outwd"))
            #   ),
            #   column(4,
            #          p("Uy: ",tableOutput("outvyp"))
            #   ),
            #   column(4,
            #          p("Us: ",tableOutput("outvsp"))
            #   )),
            # fluidRow(
            #   column(4,
            #          p("Ui: ",tableOutput("outvip"))
            #   ),
            #   column(4,
            #          p("Xnew: ",tableOutput("outxnew"))
            #   )),
            # 
            # fluidRow(
            #   column(6,
            #          p("Ratio of masses of A to I in calibration: ",tableOutput("outwac"))
            #   ),
            #   column(6,
            #          p("Rux: ",tableOutput("outeta"))
            #   )),
            
            hr(),
            h4("The estimated parameters for the design of experiment(the 95% credible interval): ",style = "color:seagreen"),
            fluidRow(
              column(6,
                     p("The expected approximate value of the measurand,quantity of A",tableOutput("outxnew"))
              ),
              column(6,
                     p("The expected Response Factor value",tableOutput("beta"))
              )
            ),
            
            fluidRow(
              column(6,
                     p("Target calibration point in terms of concentration of A:I in the calibration solution",tableOutput("wacm"))
              ),
              column(6,
                     p("Expected standard deviation of peak area ratios (A:I) from a calibration solution; (uy)",tableOutput("vy"))
                     
              )),
            
            fluidRow(
              column(6,
                     p("Expected standard uncertainty of the concentration ratios of A:I in the calibration experiment; (ux)",tableOutput("vx"))
              ),
              column(6,
                     p("Expected relative standard uncertainty of the concentration of I in samples measured for the quantitation experiment; (ui)",tableOutput("vi"))
                     
              )),
            
            fluidRow(
              column(6,
                     p("Expected relative standard uncertainty of the concentration ratios of A:I in the calibration experiment (rux)",tableOutput("rux"))
              ),
              column(6,
                     p("Expected between-sample variability (standard deviation) of peak area ratios (A:I) from the quantitation experiment(us)",tableOutput("vs"))
                     
              ))
            
            ###
          )
        })
        
        
      }
      
    }
  })
  
  
  bayesplot_func=function(){
    bayesres=outbayes()[["mcmcout"]]
    #   
    plot(density(bayesres),col="violet",main="Posterior Distribution",xlab = "WD",lwd=3)
    #   
    z = density(bayesres,n=4096)
    #   
    L = quantile(bayesres, probs=(1-input$coverage)/2)
    U = quantile(bayesres, probs=(1+input$coverage)/2)
    #   
    iS = which.min(abs(z$x-L))
    iE = which.min(abs(z$x-U))
    #   
    xp = c(L, U, z$x[iE:iS])
    yp = c(0, 0, z$y[iE:iS])
    #   
    polygon(xp, yp, border=F, col="thistle1")
    lines(z$x, z$y, type="l", col="violet", lwd=3)
    #   
    points(median(bayesres),
           min(z$y)+0.4*par()$cxy[2], pch=19, bg="Blue", col="slateblue4")
    segments(L, 0, U, 0, lwd=2, col="slateblue4")
    #   
    
  }
  
  output$mu_post_plot <- renderPlot({bayesplot_func()})
  
  
  output$download_mu_post_plot <- downloadHandler(
    filename = function() { paste( 'mean_post.pdf', sep='') },
    content = function(file) {
      pdf(file,height=9, width=8)
      bayesplot_func()
      dev.off()
    })
  
  
  ##################################################################
  ##### Trace plot #################################################
  ##################################################################
  
  
  traceplot_func=function(){
    bayesres=outbayes()[["mcmcout"]]
    
    traceColors=rainbow(1)
    plot(bayesres,type="l",ylab = "WD")
    
    
  }
  
  output$mu_trace_plot <- renderPlot({traceplot_func()})
  # 
  output$download_mu_trace_plot <- downloadHandler(
    filename = function() { paste( 'tracePlot.pdf', sep='') },
    content = function(file) {
      pdf(file,height=9, width=8)
      traceplot_func()
      dev.off()
    })
  
  
  
  ###### render Ui 
  
  

  #####DOE module code
  
  
  
  observeEvent(input$enter,{
    
    withProgress(message = 'Running the DoE module...', style="old",value = 0, {
      
     
      
      if(doeoption()==1){
        
        NCsigv<-input$NCsigv  # negative control "heterogeneity" uncertainty s_NC
        TCsigv<-input$TCsigv  # test chemical "heterogeneity" uncertainty s_TC
        
        repsigNCv<-input$repsigNCv   #repeatability uncertainty s_rep for NC
        repsigTCv<-input$repsigTCv  #repeatability uncertainty s_rep for TC
        nminv<-input$nminv         #number of wells already in use (for example PC)
        nrepNCv<-input$nrepNCv     # number of reps of NC on the 96 well plate
        nrepTCv<-input$nrepTCv    # number of reps of TC on the 96 well plate
        alpha <-input$alpha
        
        if(nminv > 20)
          
        {
          nminv=20
        }
        
        divisors <- function(x){
          #  Vector of numbers to test against
          y <- seq_len(x)
          #  Modulo division. If remainder is 0 that number is a divisor of x so return it 
          y[ x%%y == 0 ]}
        ############################################# calculate the difference #################################
        Nexp=1
        tstat=function(nrepNC,nrepTC,nsNC,nsTC,sigTC,sigNC,sigrepNC,sigrepTC){
          vNCB=(nrepNC*sigNC^2+sigrepNC^2)/nsNC/nrepNC
          vTCB=(nrepTC*sigTC^2+sigrepTC^2)/nsTC/nrepTC
          vdif=vNCB+vTCB
          df=nsNC+nsTC-2
          statd<-vdif^0.5*qt((1-alpha),df)
          result=list(stat=statd,sigNC=sigNC,sigTC=sigTC,sigrepNC=sigrepNC,sigrepTC=sigrepTC,nrepNC=nrepNC,nrepTC=nrepTC,nsNC=nsNC,nsTC=nsTC)
          return(result)
        }
        outstat<-NULL
        outres<-NULL
        
        ######################################### find the design that achieves the smallest significant difference dif for given inputs #########
        sigdiff=function(nrepNC,nrepTC,sigNC,sigTC,sigrepNC,sigrepTC,Totn){
          stopi<-Totn-1
          for(i in 1:stopi){
            nsTC=i
            nsNC=Totn-i
            testt=tstat(nrepNC,nrepTC,nsNC,nsTC,sigTC,sigNC,sigrepNC,sigrepTC)
            outstat=rbind(outstat,c(dif=testt$stat,sigNC=testt$sigNC,sigTC=testt$sigTC,sigrepNC=testt$sigrepNC,sigrepTC=testt$sigrepTC,nsNC=testt$nsNC,nsTC=testt$nsTC,nrepNC=testt$nrepNC,nrepTC=testt$nrepTC))}
          #print(outstat)
          rowmin<-apply( outstat, 2, which.min)
          optnrepNC<-outstat[rowmin[1],8]
          optnrepTC<-outstat[rowmin[1],9]
          
          if(outstat[rowmin[1],6]==1){
            optNC<-outstat[rowmin[1],6]+1
            optTC<-outstat[rowmin[1],7]-1}
          else if(outstat[rowmin[1],7]==1){
            optTC<-outstat[rowmin[1],7]+1
            optNC<-outstat[rowmin[1],6]-1} 
          else
          {optNC<-outstat[rowmin[1],6]
          optTC<-outstat[rowmin[1],7]}
          
          optdif<-outstat[rowmin[1],1]
          result2=list(optdif=optdif,sigNC=sigNC,sigTC=sigTC,sigrepNC=sigrepNC,sigrepTC=sigrepTC,Totn=Totn,optnrepNC=optnrepNC,optnrepTC=optnrepTC,optNC=optNC,optTC=optTC)
          return(result2)}
        ######################################### find the best design to minimize dif for each set of inputs ###################################
        for(i in 1:Nexp){sigNC=NCsigv[i]
        nrepNC=nrepNCv[i]
        nrepTC=nrepTCv[i]
        sigTC=TCsigv[i]
        sigrepNC=repsigNCv[i]
        sigrepTC=repsigTCv[i]
        Totn=(24-nminv[i]) #available number of wells on 24 plate
        if(Totn<4){stop("too few wells for the number of test chemicals")}else{
          
          divt<-divisors(Totn)
          totdiv<-length(divt)-2
          rdiv<-rev(divt)
          
          if(totdiv>0){if(rdiv[totdiv]>3){
            for(k in 1:totdiv){
              nrep<-divt[k]
              tot=Totn/nrep
              resdif=sigdiff(nrepNC,nrepTC,sigNC,sigTC,sigrepNC,sigrepTC,tot)
              outres<-rbind(outres,c(totn=format(round(resdif$Totn,0)),optdif=format(round(resdif$optdif,3),nsmall=3), optNC=format(round(resdif$optNC,0)),optTC=format(round(resdif$optTC,0))))
              # outres<-rbind(outres,c(resdif$Totn,resdif$optdif,resdif$optNC,resdif$optTC))
            }}else{
              totdiv<-length(divt)-3
              for(k in 1:totdiv){
                nrep<-divt[k]
                tot=Totn/nrep
                resdif=sigdiff(nrepNC,nrepTC,sigNC,sigTC,sigrepNC,sigrepTC,tot)
                outres<-rbind(outres,c(resdif$Totn,resdif$optdif,resdif$optNC,resdif$optTC))
              }}}else{tot<-Totn
              resdif=sigdiff(nrepNC,nrepTC,sigNC,sigTC,sigrepNC,sigrepTC,tot)
              
              outres<-rbind(outres,c(totn=format(round(resdif$Totn,0)),optdif=format(round(resdif$optdif,3),nsmall=3), optNC=format(round(resdif$optNC,0)),optTC=format(round(resdif$optTC,0))))
              
              #outres<-rbind(outres,c(resdif$Totn,resdif$optdif,resdif$optNC,resdif$optTC))}
              }}
        
        }
        
        
         
       
        output$outmul<-renderUI({
        
       
        colnames(outres) <- c("totn","optdif","optNC","optTC")
        output$toPrint <- renderTable(
          outres,include.colnames=T,striped = F,spacing = "m",align="c"
        )
        
        
        
          p(
            h5("totn: Total number of free wells on 24 well plate"),
            h5("dif: Limit of detection"),
            h5("NC: Number of wells assigned to negative control on 24 well plate"),
            h5("TC: Number of wells assigned to test chemical on 24 well plate"),
                 tableOutput("toPrint")
          )
          
         
        })
       
        
      }else{
        ## Code for optimal allocation of 24 wells. The length of the vectors TCsigv, repsigTCv, and nrepTC
        ## must be the same. This determines the number of test chemicals. This length can be 1, 2, 3, or 4.
        ## A single Negative control is used for all tests.
        ## The alpha level is 0.05 but can be changed in statd<-vdif^0.5*qt(0.95,df). 
        ## The optimality criterion is minimization of the maximum (minimax) of the set of "smallest significant differences"
        ## among the set of test chemicals.
        
       
        
         NCsigv<-input$NCsigv  # negative control "heterogeneity" uncertainty s_NC
         TCsigv<-as.numeric(unlist(strsplit(gsub("\\s", "", input$TCsigv),","))) # test chemical "heterogeneity" uncertainty s_TC
         repsigNCv<-input$repsigNCv   #repeatability uncertainty s_rep for NC
         repsigTCv<-as.numeric(unlist(strsplit(gsub("\\s", "", input$repsigTCv),",")))  #repeatability uncertainty s_rep for TC
         nminv<-input$nminv         #number of wells already in use (for example PC)
         nrepNC= input$nrepNCv
         nrepTC<-as.numeric(unlist(strsplit(gsub("\\s", "", input$nrepTCv),",")))     # number of reps of NC on the 96 well plate
         alpha <-as.numeric(unlist(input$alpha))
         
         
        ######validation of consistent inputs
         
         ltcsigv<-length(TCsigv)
         lrepsigTCv<-length(repsigTCv)
         lnreptc<-length(nrepTC)
         
         
          # validate(
          # 
          #   need((ltcsigv != lrepsigTCv) != TRUE,
          #        "Error: Please check your length of inputs!")
          # 
          #   #
          # ) 
         
      #####
        divisors <- function(x){
          #  Vector of numbers to test against
          y <- seq_len(x)
          #  Modulo division. If remainder is 0 that number is a divisor of x so return it 
          y[ x%%y == 0 ]}
        ##############################################################################
        Totn<-24-nminv
        tottc<-length(TCsigv)
        outstat<-NULL    
        outres<-NULL
        #################################################calculate the difference ##########################################
        tstat=function(nrepNC,nrepTC,nsNC,nsTC,sigTC,sigNC,sigrepNC,sigrepTC){
          vNCB=(nrepNC*sigNC^2+sigrepNC^2)/nsNC/nrepNC
          vTCB=(nrepTC*sigTC^2+sigrepTC^2)/nsTC/nrepTC
          vdif=vNCB+vTCB
          df=nsNC+nsTC-2
          statd<-vdif^0.5*qt((1-alpha),df)
          result=list(stat=statd,sigNC=sigNC,sigTC=sigTC,sigrepNC=sigrepNC,sigrepTC=sigrepTC,nrepNC=nrepNC,nrepTC=nrepTC,nsNC=nsNC,nsTC=nsTC)
          return(result)
        }
        
        ############################################################################
        sigdiff1=function(nrepNC,nrepTC,NCsigv,TCsigv,repsigNCv,repsigTCv,nminv,Totn){
          stopi<-Totn-2
          for(i in 2:stopi){
            nsNC=i
            nsTC=Totn-i
            if(nsTC>1){
              testt=tstat(nrepNC,nrepTC,nsNC,nsTC,TCsigv,NCsigv,repsigNCv,repsigTCv)
              outstat=rbind(outstat,c(dif1=format(round(testt1$stat,3)),nsNC=format(round(testt$nsNC,0)),nsTC=format(round(testt$nsTC,0))))
              
              }
            #print(outstat)
          }
          rowmin<-apply(outstat, 2, which.min)
          optnrepNC<-outstat[rowmin[1],2]
          optnrepTC<-outstat[rowmin[1],3]
          optdif<-outstat[rowmin[1],1]
          result=list(optdif=optdif,optnrepNC=optnrepNC,optnrepTC=optnrepTC)
          result=c(optdif,optnrepNC,optnrepTC)
          return(result)
        }
        #####################################################################
        
        sigdiff2=function(nrepNC,nrepTC,NCsigv,TCsigv,repsigNCv,repsigTCv,nminv,Totn){
          stopi<-Totn-4
          for(i in 2:stopi){nsNC=i
          stopj<-stopi-i
          for(j in 2:stopj){
            nsTC1=j
            nsTC2=Totn-i-j
            if(nsTC1>1&nsTC2>1){
              testt1=tstat(nrepNC,nrepTC[1],nsNC,nsTC1,TCsigv[1],NCsigv,repsigNCv,repsigTCv[1])
              testt2=tstat(nrepNC,nrepTC[2],nsNC,nsTC2,TCsigv[2],NCsigv,repsigNCv,repsigTCv[2])
              outstat=rbind(outstat,c(dif1= format(round(testt1$stat,3)),dif2=format(round(testt2$stat,3)),difmax=format(round(max(testt1$stat,testt2$stat),3)),nsNC=format(round(testt1$nsNC,0)),nsTC1=format(round(testt1$nsTC,0)),nsTC2=format(round(testt2$nsTC,0))))}

            #print(outstat)}
          }}
          rowmin<-apply( outstat, 2, which.min)
          optnrepNC<-outstat[rowmin[3],4]
          optnrepTC1<-outstat[rowmin[3],5]
          optnrepTC2<-outstat[rowmin[3],6]
          optdif1<-outstat[rowmin[3],1]
          optdif2<-outstat[rowmin[3],2]
          result=c(optdif1,optdif2,optnrepNC,optnrepTC1,optnrepTC2)
          return(result)
        }
        #print(outres, digits=3)
        #######################################################################
        sigdiff3=function(nrepNC,nrepTC,NCsigv,TCsigv,repsigNCv,repsigTCv,nminv,Totn){
          stopi<-Totn-6
          for(i in 2:stopi){
            nsNC=i
            stopj<-stopi-i
            for(j in 2:stopj){
              stopk<-stopj-j
              for(k in 2:stopk){
                nsTC1=j
                nsTC2=k
                nsTC3<-Totn-i-j-k
                if(nsTC1>1&nsTC2>1&nsTC3>1){
                  testt1=tstat(nrepNC,nrepTC[1],nsNC,nsTC1,TCsigv[1],NCsigv,repsigNCv,repsigTCv[1])
                  testt2=tstat(nrepNC,nrepTC[2],nsNC,nsTC2,TCsigv[2],NCsigv,repsigNCv,repsigTCv[2])
                  testt3=tstat(nrepNC,nrepTC[3],nsNC,nsTC3,TCsigv[3],NCsigv,repsigNCv,repsigTCv[3])
                  outstat=rbind(outstat,c(dif1=format(round(testt1$stat,3)),dif2=format(round(testt2$stat,3)),dif3=format(round(testt3$stat,3)),difmax=format(round(max(testt1$stat,testt2$stat,testt3$stat),3)),nsNC=format(round(testt1$nsNC,0)),nsTC1=format(round(testt1$nsTC,0)),nsTC2=format(round(testt2$nsTC,0)),nsTC3=format(round(testt3$nsTC,0))))
                  #print(outstat)
                }
              }}}
          rowmin<-apply( outstat, 2, which.min)
          optnrepNC<-outstat[rowmin[4],5]
          optnrepTC1<-outstat[rowmin[4],6]
          optnrepTC2<-outstat[rowmin[4],7]
          optnrepTC3<-outstat[rowmin[4],8]
          optdif1<-outstat[rowmin[4],1]
          optdif2<-outstat[rowmin[4],2]
          optdif3<-outstat[rowmin[4],3]
          result=c(optdif1,optdif2,optdif3,optnrepNC,optnrepTC1,optnrepTC2,optnrepTC3)
          return(result)
        }
        
        ##########################################################################
        
        sigdiff4=function(nrepNC,nrepTC,NCsigv,TCsigv,repsigNCv,repsigTCv,nminv,Totn){
          stopi<-Totn-8
          for(i in 2:stopi){
            nsNC=i
            stopj<-stopi-i
            for(j in 2:stopj){
              stopk<-stopj-j
              for(k in 2:stopk){
                stopl<-stopk-k
                for(l in 2:stopl){
                  nsTC1=j
                  nsTC2=k
                  nsTC3<-l
                  nsTC4<-Totn-i-j-k-l
                  if(nsTC1>1&nsTC2>1&nsTC3>1&nsTC4>1){
                    testt1=tstat(nrepNC,nrepTC[1],nsNC,nsTC1,TCsigv[1],NCsigv,repsigNCv,repsigTCv[1])
                    testt2=tstat(nrepNC,nrepTC[2],nsNC,nsTC2,TCsigv[2],NCsigv,repsigNCv,repsigTCv[2])
                    testt3=tstat(nrepNC,nrepTC[3],nsNC,nsTC3,TCsigv[3],NCsigv,repsigNCv,repsigTCv[3])
                    testt4=tstat(nrepNC,nrepTC[4],nsNC,nsTC4,TCsigv[4],NCsigv,repsigNCv,repsigTCv[4])
                    outstat=rbind(outstat,c(dif1=format(round(testt1$stat,3)),dif2=format(round(testt2$stat,3)),dif3=format(round(testt3$stat,3)),dif4=format(round(testt4$stat,3)),difmax=format(round(max(testt1$stat,testt2$stat,testt3$stat,testt4$stat),3)),nsNC=format(round(testt1$nsNC,0)),nsTC1=format(round(testt1$nsTC,0)),nsTC2=format(round(testt2$nsTC,0)),nsTC3=format(round(testt3$nsTC,0)),nsTC4=format(round(testt4$nsTC,0))))

                    #print(outstat)
                  }
                }}}}
          rowmin<-apply( outstat, 2, which.min)
          optnrepNC<-outstat[rowmin[5],6]
          optnrepTC1<-outstat[rowmin[5],7]
          optnrepTC2<-outstat[rowmin[5],8]
          optnrepTC3<-outstat[rowmin[5],9]
          optnrepTC4<-outstat[rowmin[5],10]
          optdif1<-outstat[rowmin[5],1]
          optdif2<-outstat[rowmin[5],2]
          optdif3<-outstat[rowmin[5],3]
          optdif4<-outstat[rowmin[5],4]
          result=c(optdif1,optdif2,optdif3,optdif4,optnrepNC,optnrepTC1,optnrepTC2,optnrepTC3,optnrepTC4)
          return(result)
        }
        
        ##########################################################################
        # ltcsigv<-length(TCsigv)
        # lrepsigTCv<-length(repsigTCv)
        # lnreptc<-length(nrepTC)
        #output$outmul<-renderUI({
        if (ltcsigv!=lrepsigTCv){
          
          output$outmul<-renderUI({
            h4("Warning: please check the consistency of multiple inputs!",style = "color:red")
          })
          
        }else if((tottc+1)*2>Totn){
          output$outmul<-renderUI({
          h4("Warning: too few wells for the number of test chemicals!",style = "color:red")
          })
            } else{
          if(tottc==1&Totn>3){outstat=sigdiff1(nrepNC,nrepTC,NCsigv,TCsigv,repsigNCv,repsigTCv,nminv,Totn)}
          if(tottc==2&Totn>5){outstat=sigdiff2(nrepNC,nrepTC,NCsigv,TCsigv,repsigNCv,repsigTCv,nminv,Totn)}
          if(tottc==3&Totn>7){outstat=sigdiff3(nrepNC,nrepTC,NCsigv,TCsigv,repsigNCv,repsigTCv,nminv,Totn)}
          if(tottc==4&Totn>9){outstat=sigdiff4(nrepNC,nrepTC,NCsigv,TCsigv,repsigNCv,repsigTCv,nminv,Totn)}
        # print(outstat, digits=3)}
              output$outmul<-renderUI({
           
               
              output$toPrint <- renderTable(
              data.frame(t(outstat)),include.colnames=T,striped = F,spacing = "m",align="c",digits = 3)
              p(
                h5("totn: Total number of free wells on 24 well plate"),
                h5("dif: Limit of detection"),
                h5("NC: Number of wells assigned to negative control on 24 well plate"),
                h5("TC: Number of wells assigned to test chemical on 24 well plate"),
                tableOutput("toPrint")
              )
           
              }) 
      }
 

        
      
      
          
          
        
      }

           
           
         })
    
    
  })
  
 
  observeEvent(input$enter,{
    output$doeoutput<-renderUI(
      
      downloadButton('downloaddoe', 'Download DoE Results')
      
    )
  })
  
  
  
  
  
  #####################
  ###Analysis module###
  #####################
  #####display uploaded files
  ###caltables###
  
  ###caltables###
  
  output$anaui <- renderUI({
    if(input$analysis){
      
      fluidRow(style='margin: 0px;',
               column(12,
                      h3("Model Results",style = "color:steelblue"),
                      p("The posterior mean is: ",textOutput("mu_est1",inline=T)),
                      p("The standard uncertainty is: ",textOutput("mu_se1",inline=T)),
                      p("The posterior median is: ",textOutput("mu_median1",inline=T)),
                      p("The ",textOutput("coverageProbabilityPercentBayes1",inline=T),
                        " credible interval ranges from: ",textOutput("mu_quant1",inline=T))))
    }
  })
  output$anaui1 <- renderUI({
    if(input$analysis){     
      fluidRow(style='margin: 0px;',
               column(12, 
                      downloadButton('downloadbayesout1', 'Download PDF report')))
    }
  })
  output$anaui2 <- renderUI({
    if(input$analysis){     
      fluidRow(style='margin: 0px;',
               column(12, 
                      plotOutput("mu_post_plot1", width = "auto", height = "600px"),
                      downloadButton('download_mu_post_plot1', 'Download plot'),
                      
                      
                      plotOutput("mu_trace_plot1", width = "auto", height = "600px"),
                      downloadButton('download_mu_trace_plot1', 'Download plot')
               )
      )
      
    }
  })
  
  
  ####
  observe({
    if (!is.null(input$calfile1)) {
      N_tables1 <<- length(input$calfile1[, 1])
      
      upload <- list()
      for (i in 1:N_tables1) {
        upload[[i]] <- read.csv(input$calfile1$datapath[i])
      }
      
      output$caltables1 <- renderUI({
        table_output_list <- lapply(1:N_tables1, function(i) {
          tableOutput(paste0("table_name", i))
        })
        do.call(tagList, table_output_list)
      })
      
      for (i in 1:N_tables1) {
        local({
          my_i <- i
          output[[paste0("table_name", my_i)]] <- renderTable((
            upload[[my_i]]
          ),spacing = "s", striped = T,rownames = T,digits = 5)
        })
      }
    }
  })
  
  
  #####sample tables######
  
  observe({
    if (!is.null(input$samplefile1)) {
      N_tables1<- length(input$samplefile1[, 1])
      
      upload <- list()
      for (i in 1:N_tables1) {
        upload[[i]] <- read.csv(input$samplefile1$datapath[i])
      }
      
      output$sampletables1 <- renderUI({
        table_output_list <- lapply(1:N_tables1, function(i) {
          tableOutput(paste0("table_name2", i))
        })
        do.call(tagList, table_output_list)
      })
      
      for (i in 1:N_tables1) {
        local({
          my_i <- i
          output[[paste0("table_name2", my_i)]] <- renderTable((
            upload[[my_i]]
          ),spacing = "s", striped = T,rownames = T,digits = 5)
        })
      }
    }
  })
  
  ###number of calibration
  
  cala <- reactive({
    
    inFile <- input$calfile1
    if (is.null(input$calfile1)){
      return(NULL)
    }else{
      
      nfile1<<-nrow(input$calfile1)
      tmp<- read.csv(inFile$datapath[1])
      N1<<-nrow(tmp)#calbraton number
      NR1<<-ncol(tmp)-3# number of replicates for each calibrant
      NWS1<<-max(tmp$wsol)# #working solutions from  calibrationtable
      cal<-c(N1,NR1,NWS1)
      a<-t(as.matrix(cal))
      colnames(a)<-c("Calibration Number","Calibration repeat","working solution")
      return (a)
      
    }
  })
  
  
  
  ######number of sample#####
  
  samb <- reactive({
    inFile <- input$samplefile1
    if (is.null(inFile)){
      return(NULL)
    }else{
      
      tmp<- read.csv(inFile$datapath[1])
      M1<<-nrow(tmp)
      MR1<<-ncol(tmp)-2
      sam1<-c(M1,MR1)
      b<-t(as.matrix(sam1))
      colnames(b)<-c("Number of Sample","Sample repeat")
      return (b)
      
      
    }
  })
  
  ###creates the calibration table template
  
  cal.table1 = function(){
    n.cols= 5 # mid + mad + replicates 
    my.table = matrix("", nrow = 1, ncol = n.cols)
    # rac.colnames = sapply(1:input$numberOfCalReps, function(x){paste0("rac", x)})
    colnames(my.table) = c('mid', 'mad','wsol', 'rac1','rac2')
    return(my.table)
  }
  
  output$downloadCalTable1 <- downloadHandler(
    filename = function(){
      paste("calibration_input_template","xlsx",sep=".")
    },
    content = function(file) {
      file.copy("calibration_input_template.xlsx",file)
    }
  )
  
  ### This function creates the samples table template
  
  sam.table1 = function(){
    n.cols= 4 
    my.table = matrix("", nrow = 1, ncol = n.cols)
    # ras.colnames = sapply(1:input$numberOfSampleReps, function(x){paste0("ras", x)})
    colnames(my.table) = c('mids', 'mdi', 'ras1','ras2')
    return(my.table)
  }
  
  output$downloadSamTable1 <- downloadHandler(
    
    filename = function(){
      paste("sample_input_template","xlsx",sep=".")
    },
    content = function(file) {
      file.copy("sample_input_template.xlsx",file)
    }
  )
  
  ####merge calibration data talbes
  ######standard options
  
  standardui1<-reactive(
    
    input$standard1
  )
  
  output$standard_ui1<-renderUI({
    
    if(standardui1()==1){
      tagList(
        fluidRow(style='margin: 0px;',
                 column(12,
                        h3("Calibration standard inputs:",style = "color:steelblue"))),
        
        
        fluidRow(style='margin: 0px;',
                 column(6,
                        wellPanel(
                          textInput("wadm1",label =h4("Analyte working standard solution concentrations; Comma-separated entries for multiple inputs"), value="94.2, 95.5, 94.5",placeholder = "e.g. '94.2, 95.5, 94.5'"))),
                 
                 column(6,
                        wellPanel(
                          textInput("uwad1",label = h4("Uncertainties in working standard solution; Comma-separated entries for multiple inputs"),value="0.427, 0.434, 0.429",placeholder = "e.g. '0.427, 0.434, 0.429'")))),
        
        fluidRow(style='margin: 0px;',
                 
                 column(6,
                        wellPanel(
                          numericInput(
                            "u_mid1", 
                            label = h4("Uncertainty in masses (or volume) of internal standard solution added to calibrants (g, mL, etc.)"),
                            value = 0.000015
                          ))), 
                 
                 column(6,
                        wellPanel(
                          numericInput(
                            "u_mad1", 
                            label = h4("Uncertainty in masses (or volume) of analyte working standard solution added to calibrants(g, mL, etc.)"),
                            value = 0.000015
                          )))), 
        
        
        fluidRow(style='margin: 0px;',
                 column(12,
                        h3("Sample preparation uncertainties:",style = "color:steelblue"))), 
        fluidRow(style='margin: 0px;',
                 column(6,
                        wellPanel(
                          numericInput(
                            "u_mids1", 
                            label = h4("Uncertainty in masses (or volume) of internal standard solution spiked into samples (g, mL, etc.)"),
                            value = 0.000015
                          ))), 
                 
                 column(6,
                        wellPanel(        
                          numericInput(
                            "u_mdi1", 
                            label = h4("Uncertainty in the masses (or volume) of substance sampled for measurement (serum, food, blood, etc.)"),
                            value = 0.000015
                          ))))
      )
    }else{
      
      tagList(
        fluidRow(style='margin: 0px;',
                 column(12,
                        h3("Calibration standard inputs:",style = "color:steelblue"))),
        
        
        fluidRow(style='margin: 0px;',
                 column(6,
                        wellPanel(
                          textInput("wadm1",label =h4("Analyte working standard solution concentrations; Comma-separated entries for multiple inputs"), value="94.2, 95.5, 94.5",placeholder = "e.g. '94.2, 95.5, 94.5'"))),
                 
                 column(6,
                        wellPanel(
                          textInput("uwad1",label = h4("Uncertainties in working standard solution; Comma-separated entries for multiple inputs"),value="0.427, 0.434, 0.429",placeholder = "e.g. '0.427, 0.434, 0.429'")))),
        
        fluidRow(style='margin: 0px;',
                 
                 # column(6,
                 #        wellPanel(
                 #          numericInput(
                 #            "u_mid", 
                 #            label = h4("Uncertainty in masses (or volume) of internal standard solution added to calibrants (g, mL, etc.)"),
                 #            value = 0.000015
                 #          ))), 
                 
                 column(12,
                        wellPanel(
                          numericInput(
                            "u_mad1", 
                            label = h4("Uncertainty in masses (or volume) of analyte working standard solution added to calibrants(g, mL, etc.)"),
                            value = 0.000015
                          )))), 
        
        
        fluidRow(style='margin: 0px;',
                 column(12,
                        h3("Sample preparation uncertainties:",style = "color:steelblue"))), 
        fluidRow(style='margin: 0px;',
                 # column(6,
                 #        wellPanel(
                 #          numericInput(
                 #            "u_mids",
                 #            label = NULL,
                 #            value = 1
                 #          ))),
                 
                 column(12,
                        wellPanel(        
                          numericInput(
                            "u_mdi1", 
                            label = h4("Uncertainty in the masses (or volume) of substance sampled for measurement (serum, food, blood, etc.)"),
                            value = 0.000015
                          ))))
      )
    }
    
  })
  
  
  
  
  ####work solution of Calibration
  
  ####output tables
  
  
  
  output$calnum1 <- renderTable(cala(),striped = T,spacing = "l",align="c",digits = 0)
  output$samplenum1 <- renderTable(samb(),striped = T,spacing = "l",align="c",digits = 0)
  
  ##################################################################
  ##################################################################
  ### Run Bayesian analysis ########################################
  ##################################################################
  ##################################################################
  
  outbayes1<- eventReactive(input$analysis,{
    
    N<-N1
    NR<-NR1
    #NWS<-NWS1
    
    M<-M1
    MR<-MR1
    
    N_tables<-N_tables1
    nfile<-N_tables1
    
    standard1<-reactive(
      input$standard1
    )
    
    choice<-reactive(
      input$choice1
    )
    if (choice()==1){
      ###NS==1
      if(N_tables==1){
        
        caldata<-reactive({
          
          if (is.null(input$calfile1))
            return()
          else
          {
            csv=list()
            csv=read.csv(input$calfile1$datapath[1])
            return(csv)   
          }
          
        })
        
        ####merge sample data talbes
        
        sampledata<-reactive({
          
          if (is.null(input$samplefile1))
            return()
          else
          {
            
            csv=list()
            csv=read.csv(input$samplefile1$datapath[1])
            return(csv)
          }
          
        })
        
        
        wadm<<-reactive(
          return(append(as.numeric(unlist(strsplit(gsub("\\s", "", input$wadm1),","))),1))
          
        )
        
        uwad<<-reactive(
          return(append(as.numeric(unlist(strsplit(gsub("\\s", "", input$uwad1),","))),1))
          
        )
        
        ####
        
        
        NWS <- reactive({
          
          inFile <- input$calfile1
          if (is.null(input$calfile1)){
            return(NULL)
          }else{
            
            tmp<- read.csv(inFile$datapath[1])
            
            return(max(tmp$wsol))# #working solutions from  calibrationtable
            
            
          }
        })
        
        
        umids<-reactive({
          
          if (standard1()==1){
            
            return(input$u_mid1)
          }else{
            
            return(1)
          }
        })
        
        umads<-reactive(
          
          return(input$u_mad1)
          
        )
        
        
        ####
        
        NS<-reactive({
          
          if (is.null(input$calfile1))
            return()
          else
          {
            NS<-nrow(input$calfile1)
            return(NS)
            
          }
          
        })
        
        
        
        NT<-reactive(
          
          return(N*NR)
          
        )
        
        
        
        
        midsm<<-reactive({
          
          if(standard1()==1){
            return(caldata()$mid)
            
          }else{
            
            return(caldata()$mad/caldata()$mad)
          }
        })
        
        madsm<-reactive(
          caldata()$mad
        )
        
        wsol<-reactive(
          caldata()$wsol
        )
        
        
        rac<- reactive({
          
          if (is.null(input$calfile1))
            return()
          else
          {
            
            csv=list()
            
            for(i in 1: nfile)
            {
              csv[[i]]=read.csv(input$calfile1$datapath[i])
              csv[[i]][1:3]<-NULL
            }
            
            return(unlist(csv)) # rbind the csv
          }
          
        })
        
        
        ras<- reactive({
          
          if (is.null(input$samplefile1))
            return()
          else
          {
            nfile<-nrow(input$samplefile1)
            
            csv=list()
            
            for(i in 1: nfile)
            {
              csv[[i]]=read.csv(input$samplefile1$datapath[i])
              csv[[i]][1:2]<-NULL
            }
            
            return(unlist(csv)) # rbind the csv
          }
          
        })
        
        
        mdi<-reactive(
          
          sampledata()$mdi
          
        )
        
        ####standard
        midsi<-reactive(
          
          if(standard1()==1){
            return(sampledata()$mids)
          }else{
            
            return(sampledata()$mdi/sampledata()$mdi)
            
          }
        )
        
        MT<-reactive(
          
          return(M*MR)
          
        )
        
        
        
        sol<-reactive({
          rep(1:N,NR)
        })
        
        sampl<-reactive({
          rep(1:M,NR)
          
        })   
        
        
        #####
        
        linedata = list(
          midsm=midsm(),
          madsm=madsm(),
          wsol=wsol(),
          rac=rac(),
          midsi=midsi(),
          mdi=mdi(),
          ras=ras(),
          umids=umids(),
          umads=umads(),
          wadm= wadm(),
          uwad=uwad(),
          NWS=NWS()+1,
          N=N,
          M=M,
          MT=MT(),
          NT=NT(),
          sol=sol(),
          NR=NR,
          sampl=sampl()
        )
        
        
        # 
        require(R2OpenBUGS)
        ##################################################################
        ##################################################################
        ### Define the model
        ##################################################################
        ##################################################################
        
        
        linemodel<-function(){#calculate known mass ratios wac for N calibrants.
          
          midsprec<-1/(umids*umids)
          madsprec<-1/(umads*umads)
          
          for(i in 1:NWS){
            wadprec[i]<-1/(uwad[i]*uwad[i])
            wad[i]~dnorm(wadm[i],wadprec[i])
          }
          
          for(i in 1:N){mids[i]~dnorm(midsm[i],midsprec)
            mads[i]~dnorm(madsm[i],madsprec)
          }
          for(i in 1:N){wac[i]<-wad[wsol[i]]*mads[i]/mids[i]}  
          
          #######
          #calibration equation
          
          a~dnorm(0,1.0E-5)
          b~dnorm(0,1.0E-5)
          
          xins~dnorm(0,0.0016)%_%I(0.001,)
          chsqns~dgamma(0.5,0.5)
          fitprec<-xins/sqrt(chsqns)
          
          for(i in 1:N){mean[i]<-a+b*wac[i]
          predm[i]~dnorm(mean[i],fitprec)}
          for(i in 1:NT){rac[i]~dnorm(mean[sol[i]],fitprec)
          }
          vyp<-1/sqrt(fitprec)
          ###
          # Compute the mass fraction wd
          a.cut<-cut(a)
          b.cut<-cut(b)
          sigras~dgamma(1.0E-5,1.0E-5)
          wdsig~dgamma(1.0E-3,1.0E-3)
          
          wd~dnorm(0,1.0E-5)
          for(i in 1:M){wdm[i]~dnorm(wd,wdsig)
            rasmean[i]<-a.cut+b.cut*wdm[i]*mdi[i]/midsi[i]
            rasmeanp[i]~dnorm(rasmean[i],fitprec)}
          for(i in 1:MT){ras[i]~dnorm(rasmeanp[sampl[i]],sigras)}
          vsp<-1/sqrt(sigras) ### vs
          hetp<-vsp/vyp*100 
        }
        
        ##################################################################
        ##################################################################
        ### Run OpenBUGS
        ##################################################################
        ##################################################################
        
        withProgress(message = 'Running Bayes', style="old",value = 0, {
          
          
          
          lineinits<-function(){list(sigras=1,wdsig=1,a=0,b=1)}
          
          lineout<-bugs(data=linedata,
                        inits=lineinits,
                        parameters=c("a","b","wd","wdm","wac","mean","hetp","predm"),   
                        model.file=linemodel,
                        n.iter = input$niters, n.burnin = input$nburnin, n.thin = 10,n.chains = 1)#,debug=T)    
        })
        
        ######
        attach.bugs(lineout) ## imports the random draws for the parameters
        cl<-NULL
        
        for(i in 1:N){
          for(j in 1:NR){cl<-c(cl,quantile(predm[,i], 0.025))}}
        
        cl<-rep(unique(cl), NR)
        cu<-NULL
        for(i in 1:N){
          for(j in 1:NR){cu<-c(cu,quantile(predm[,i], 0.925))}}
        cu<-rep(unique(cu), NR)
        
        
        
        
        outsize<-data.frame(ncal=N, calrep=NR, nsampl=M, samplrep=MR) 
        outrega<-data.frame(mean=lineout$mean$a,sd=lineout$sd$a)
        
        outregb<-data.frame(mean=lineout$mean$b,sd=lineout$sd$b)
        outwdm<-data.frame(mean=lineout$mean$wdm,sd=lineout$sd$wdm)
        outmean<-data.frame(mean=lineout$mean$mean)
        outwd<-data.frame(mean=lineout$mean$wd,sd=lineout$sd$wd )
        outhetp<-data.frame(mean=lineout$mean$hetp)
        
        
        
        
        ########## sets up the data to plot the regression line and calculate the rsq
        
        outwac=lineout$mean$wac
        
        lwac<-rep(outwac,NR)
        
        ############ calculates the rsq
        ybar<-outmean
        sst<-sum((rac()-mean(rac()))^2)
        sse<-sum((rac()-ybar)^2)
        rsq<-1-sse/sst
        
        ###Regression output###
        
        ########
        bayesres=lineout$sims.list$wd
        
        
        bind1 <-list(wadm=input$wadm1,uwad=input$uwad1,
                     u_mid=umids(),u_mad=input$u_mad1,
                     u_mids=input$u_mids1,u_mdi=input$u_mdi1,
                     coverage=input$coverage1,Number_iteration=input$niters1,Length_burnin=input$nburnin1,
                     mean=mean(bayesres),sd=sd(bayesres),median=median(bayesres),
                     a=outrega,
                     b=outregb,
                     rsq=rsq,
                     vyp=outhetp)
        
        ########
        
        
        
        
        
        updateTabsetPanel(session = session, inputId = "results",selected = "Fit")
        
        
        return(list(mcmcout=bayesres,bind1=bind1,rsq=rsq,outa=outrega,outb=outregb,rac=rac(),wac=lwac,linea=lineout$mean$a,lineb=lineout$mean$b,cl=cl,cu=cu,hetp=outhetp))
        
        #####N>1
        
      }else{
        ####merge calibration data talbes
        caldata<-reactive({
          
          if (is.null(input$calfile1))
            return()
          else
          {
            nfile<-nrow(input$calfile1)
            
            csv=list()
            
            for(i in 1: nfile)
            {
              csv[[i]]=read.csv(input$calfile1$datapath[i])
              
            }
            
            do.call(rbind,csv) # rbind the csv
          }
          
        })
        
        ####merge sample data talbes
        
        sampledata<-reactive({
          
          if (is.null(input$samplefile1))
            return()
          else
          {
            nfile=nrow(input$samplefile1)
            csv=list()
            for(i in 1: nfile)
            {
              csv[[i]]=read.csv(input$samplefile1$datapath[i])
              
            }
            
            do.call(rbind,csv) # rbind the csv
          }
          
        })
        
        
        wadm<<-reactive(
          return(append(as.numeric(unlist(strsplit(gsub("\\s", "", input$wadm1),","))),1))
          
        )
        
        uwad<<-reactive(
          return(append(as.numeric(unlist(strsplit(gsub("\\s", "", input$uwad),","))),1))
          
        )
        
        ####
        
        
        NWS <- reactive({
          
          inFile <- input$calfile1
          if (is.null(input$calfile1)){
            return(NULL)
          }else{
            
            
            tmp<- read.csv(inFile$datapath[1])
            
            return(max(tmp$wsol))# #working solutions from  calibrationtable
            
            
          }
        })
        
        #output$nws<-renderTable(NWS)
        
        standard1<-reactive(
          input$standard1
        )
        
        umids<-reactive({
          
          if (is.null(input$calfile1))
            return()
          else
          {
            
            if (standard1()==1){
              nfile<<-nrow(input$calfile1)
              
              return(c(rep(input$u_mid1,nfile)))
            }else{
              nfile<<-nrow(input$calfile1)
              return(c(rep(1,nfile)))
              
            }
          }
        })
        
        
        
        umads<<-reactive(
          
          return(c(rep(input$u_mad1,nfile)))
          
        )
        
        
        ####
        
        NS<-reactive({
          
          if (is.null(input$calfile1))
            return()
          else
          {
            NS<-nrow(input$calfile1)
            return(NS)
            
          }
          
        })
        
        
        
        NT<-reactive(
          
          length(caldata()$mid)
          
        )
        
        expc<-reactive({
          
          if (is.null(input$calfile1))
            return()
          else
          {
            nfile<-nrow(input$calfile1)
            
            
            expc=list()
            
            for(i in 1: nfile)
            {
              nr= nrow(read.csv(input$calfile1$datapath[i]))
              expc[[i]]<-(c(rep(1*i,each=nr)))
              expc[[i]]<-t(expc[[i]])
            } 
            
            
            
            as.vector(do.call(cbind, expc))
            
            
          }
          
        })
        
        ######standard
        
        midsm<-reactive(
          if (standard1()==1){
            caldata()$mid
          }else{
            
            caldata()$mad/caldata()$mad
          }
        )
        
        madsm<<-reactive(
          caldata()$mad
        )
        
        wsol<<-reactive(
          caldata()$wsol
        )
        
        
        
        NTT <- reactive({
          
          inFile <- input$calfile1
          if (is.null(input$calfile1)){
            return(NULL)
          }else{
            
            
            tmp=list()
            
            for(i in 1: nfile)
            {
              tmp[[i]]=read.csv(input$calfile1$datapath[i])
              
            }
            
            tmp<-do.call(rbind,tmp)
            
            NR<<-ncol(tmp)-3# number of replicates for each calibrant
            NT<-nrow(tmp)
            
            NTT<-NR*NT
            
          }
        })
        
        
        expc1<-reactive({
          
          if (is.null(input$calfile1))
            return()
          else
          {
            nfile<-nrow(input$calfile1)
            
            
            
            expc1=list()
            
            for(i in 1: nfile)
            {
              nr<<-nrow(read.csv(input$calfile1$datapath[i]))
              nrac<-ncol(read.csv(input$calfile1$datapath[i]))-3
              n1<-nr*nrac
              expc1[[i]]<-matrix(c(rep(1*i,each=n1)))
              
            } 
            
            
            as.vector(do.call(rbind, expc1))
            
            
          }
          
        })
        
        expc2<-reactive({
          
          if (is.null(input$calfile1))
            return()
          else
          {
            nfile<-nrow(input$calfile1)
            
            csv=list()
            
            k<-nrow(read.csv(input$calfile1$datapath[1]))
            nrac<-ncol(read.csv(input$calfile1$datapath[1]))-3
            
            
            csv[[1]]=c(rep(1:k,nrac))
            
            
            for(i in 2: nfile)
            {
              
              c=max(csv[[i-1]])+1
              x=nrow(read.csv(input$calfile1$datapath[i]))
              y=c+x-1
              csv[[i]]=c(rep(c:y, nrac))
              
            }
            
            as.vector(unlist(csv))
            
            
            
          }
          
        })
        
        
        
        
        rac<- reactive({
          
          if (is.null(input$calfile1))
            return()
          else
          {
            nfile<-nrow(input$calfile1)
            
            csv=list()
            
            for(i in 1: nfile)
            {
              csv[[i]]=read.csv(input$calfile1$datapath[i])
              csv[[i]][1:3]<-NULL
            }
            
            return(unlist(csv)) # rbind the csv
          }
          
        })
        
        
        ras<- reactive({
          
          if (is.null(input$samplefile1))
            return()
          else
          {
            nfile<-nrow(input$samplefile1)
            
            csv=list()
            
            for(i in 1: nfile)
            {
              csv[[i]]=read.csv(input$samplefile1$datapath[i])
              csv[[i]][1:2]<-NULL
            }
            
            return(unlist(csv)) # rbind the csv
          }
          
        })
        
        
        md<<-reactive(
          
          mdi<-sampledata()$mdi
          
        )
        
        
        #####standard
        
        midsi<-reactive(
          if (standard1()==1){
            mdi<-sampledata()$mids
            
          }else{
            
            sampledata()$mdi/sampledata()$mdi
          }
        )
        
        MT<<-reactive(
          
          length(sampledata()$mids)
          
        )
        
        MTT <- reactive({
          
          inFile <- input$samplefile1
          if (is.null(input$samplefile1)){
            return(NULL)
          }else{
            
            
            tmp=list()
            
            for(i in 1: nfile)
            {
              tmp[[i]]=read.csv(input$samplefile1$datapath[i])
              
            }
            
            tmp<-do.call(rbind,tmp)
            
            NR<<-ncol(tmp)-2# number of replicates for each calibrant
            NT<-nrow(tmp)
            
            NTT<-NR*NT
            
          }
        })
        
        exp<-reactive({
          
          if (is.null(input$samplefile1))
            return()
          else
          {
            nfile<-nrow(input$samplefile1)
            
            
            exp=list()
            
            for(i in 1: nfile)
            {
              nr= nrow(read.csv(input$samplefile1$datapath[i]))
              exp[[i]]<-(c(rep(1*i,each=nr)))
              exp[[i]]<-t(exp[[i]])
            } 
            
            as.vector(do.call(cbind, exp))
            
          }
          
        })
        
        
        expr<-reactive({
          
          if (is.null(input$samplefile1))
            return()
          else
          {
            nfile<<-nrow(input$samplefile1)
            
            
            
            expr=list()
            
            for(i in 1: nfile)
            {
              nr<<-nrow(read.csv(input$samplefile1$datapath[i]))
              nrac<-ncol(read.csv(input$samplefile1$datapath[i]))-2
              n1<-nr*nrac
              expr[[i]]<-matrix(c(rep(1*i,each=n1)))
              
            } 
            
            
            as.vector(do.call(rbind, expr))
            
            
          }
          
        })
        
        expr2<-reactive({
          
          if (is.null(input$samplefile1))
            return()
          else
          {
            nfile<-nrow(input$samplefile1)
            
            csv=list()
            
            k<-nrow(read.csv(input$samplefile1$datapath[1]))
            nras<-ncol(read.csv(input$samplefile1$datapath[1]))-2
            
            
            csv[[1]]=c(rep(1:k,nras))
            
            
            for(i in 2: nfile)
            {
              
              c=max(csv[[i-1]])+1
              x=nrow(read.csv(input$samplefile1$datapath[i]))
              y=c+x-1
              csv[[i]]=c(rep(c:y, nras))
              
            }
            
            as.vector(unlist(csv))
            
            
            
          }
          
        })
        
        nfile<-reactive({
          
          if (!is.null(input$calfile1))
          {
            nfile<-nrow(input$calfile1)
          }
        })
        
        nrac<-reactive({
          
          if (!is.null(input$calfile1))
          {
            nrac<-ncol(read.csv(input$calfile1$datapath[1]))-3
            
          }
        })
        
        vecwac <- reactive({
          
          if (!is.null(input$calfile1))
          {
            
            vec=list()
            for(i in 1: nfile){
              vec[[i]]<-nrow(read.csv(input$calfile1$datapath[i]))
            }
            return(vec)
          }
        })
        
        vecrac <- reactive({
          
          if (!is.null(input$calfile1))
          {
            
            vecrac=list()
            for(i in 1: nfile){
              vecrac[[i]]<-nrac()*nrow(read.csv(input$calfile1$datapath[i]))
            }
            return(vecrac)
          }
        })
        
        vecras <- reactive({
          
          if (!is.null(input$samplefile1))
          {
            
            vecras=list()
            for(i in 1: nfile){
              vecras[[i]]<-nrac()*nrow(read.csv(input$samplefile1$datapath[i]))
            }
            return(vecras)
          }
        })
        
        linedata<-list(umids=umids(),
                       umads=umads(),
                       wadm=wadm(),
                       uwad=uwad(),
                       NWS=NWS()+1,
                       NS=NS(),
                       NT=NT(),
                       expc=expc(),
                       midsm=midsm(),
                       madsm=madsm(),
                       wsol=wsol(), 
                       NTT=NTT(), 
                       expc1=expc1(),
                       expc2=expc2(),
                       rac=rac(),
                       ras=ras(),
                       MT=MT(),MTT=MTT(),
                       exp=exp(),
                       midsi=midsi(),
                       md=md(),
                       expr=expr(),
                       expr2=expr2())
        
        
        require(R2OpenBUGS)
        # #   
        # #   ##################################################################
        # #   ##################################################################
        # #   ### Define the model
        # #   ##################################################################
        # #   ##################################################################
        # #   
        # #   
        linemodel<-function(){####this program uses NS number of input tables. 
          ### NT is the total number of elements of midsm and madsm (total number of calibrants). 
          ### expc gives the experiment designation for each element in midsm and madsm. NTT is the total number of measurements in the calibration experiment.
          ### expc1 gives the experiment label for each observation in rac, expc2 gives calibrant designation for each rac.
          ### MT gives the total number of samples, MTT gives the total number of observations in the quantitation experiment 
          ### exp gives the experiment designation for each sample, expr gives the experiment designation for each element in ras, 
          ### expr2 gives the sample designation for each element in ras.
          
          
          for(i in 1:NWS){wadprec[i]<-1/(uwad[i]*uwad[i])
          wad[i]~dnorm(wadm[i],wadprec[i])}
          wadmean<-mean(wad[])
          
          for(i in 1:NS){midsprec[i]<-1/(umids[i]*umids[i])
          madsprec[i]<-1/(umads[i]*umads[i])}
          
          
          for(i in 1:NT){
            mids[i]~dnorm(midsm[i],midsprec[expc[i]])
            mads[i]~dnorm(madsm[i],madsprec[expc[i]])
            ratio[i]<-mads[i]/mids[i] 
            wac[i]<-wad[wsol[i]]* ratio[i] } 
          
          #######
          #calibration equation
          for(i in 1:NS){
            a[i]~dnorm(0,1.0E-5)
            b[i]~dnorm(0,1.0E-5)
            
            xins[i]~dnorm(0,0.0016)%_%I(0.001,)
            chsqns[i]~dgamma(0.5,0.5)
            fitprec[i]<-xins[i]/sqrt(chsqns[i])
            vyp[i]<-1/sqrt(fitprec[i])
          }
          
          for(i in 1:NTT){mean[i]<-a[expc1[i]]+b[expc1[i]]*wac[expc2[i]]
          rac[i]~dnorm(mean[i],fitprec[expc1[i]])
          predm[i]~dnorm(mean[i],fitprec[expc1[i]])
          
          }
          
          
          ###
          # Compute the mass fraction wd
          for(i in 1:NS){
            a.cut[i]<-cut(a[i])
            b.cut[i]<-cut(b[i])
            xinsi[i]~dnorm(0,0.0016)%_%I(0.001,)
            chsqnsi[i]~dgamma(0.5,0.5)
            sigras[i]<-xinsi[i]/sqrt(chsqnsi[i])
            vsp[i]<-1/sqrt(sigras[i])
            hetp[i]<-vsp[i]/vyp[i]*100 
            
            xinsw[i]~dnorm(0,0.0016)%_%I(0.001,)
            chsqnsw[i]~dgamma(0.5,0.5)
            wdsig[i]<-xinsw[i]/sqrt(chsqnsw[i])
            wd[i]~dnorm(0,1.0E-5)}
          
          for(i in 1:MT){
            wdm[i]~dnorm(wd[exp[i]],wdsig[exp[i]])
            rasmean[i]<-a.cut[exp[i]]+b.cut[exp[i]]*wdm[i]*md[i]/midsi[i]
            rasmeanp[i]~dnorm(rasmean[i],fitprec[exp[i]])}
          
          
          
          for(i in 1:MTT){
            ras[i]~dnorm(rasmeanp[expr2[i]],sigras[expr[i]])
            
          } 
          
          # 
          
          #############################################
          T~dcat(P[])
          P[1:NS]~ddirich(alpha[])
          for(i in 1:NS){alpha[i]<-1}
          mumeanfin<-wd[T]
          
        }
        
        ##################################################################
        ##################################################################
        ### Run OpenBUGS
        ##################################################################
        ##################################################################
        
        withProgress(message = 'Running Bayes', style="old",value = 0, {
          
          lineinits<-function(){list(a=c(0,0),b=c(1,1),wd=c(0,0))}
          
          lineout<-bugs(data=linedata,
                        inits=lineinits,
                        parameters=c("wd","wdm","mumeanfin","wac","a","b","hetp","mean","predm"),   
                        model.file=linemodel,
                        n.iter = 10000, n.burnin = 5000, n.thin = 10,n.chains =1)#,debug=T)
        })
        
        ###########################################
        ### additional result generation
        
        attach.bugs(lineout) ## imports the random draws for the parameters
        
        outrega<-data.frame(mean=lineout$mean$a,sd=lineout$sd$a)
        outregb<-data.frame(mean=lineout$mean$b,sd=lineout$sd$b)
        outwdm<-data.frame(mean=lineout$mean$wdm,sd=lineout$sd$wdm)
        outmean<-data.frame(mean=lineout$mean$mean)
        outwd<-data.frame(mean=lineout$mean$wd,sd=lineout$sd$wd )
        outmumeanfin<-data.frame(mean=lineout$mean$mumeanfin,sd=lineout$sd$mumeanfin )
        outhetp<-lineout$mean$hetp
        ########## sets up the data to plot the regression line and calculate the rsq
        
        require(data.table)
        
        
        ############ calculates the rsq
        
        ybar<-split(outmean,rep(1:nfile,vecrac()))
        rac_rsq<-split(rac(),rep(1:nfile,vecrac()))
        
        sst=list()
        sse=list()
        rsq=list()
        for(i in 1: nfile)
        {
          sst[[i]]<-sum((rac_rsq[[i]]-mean(rac_rsq[[i]]))^2)
          sse[[i]]<-sum((rac_rsq[[i]]-ybar[[i]])^2)
          rsq[[i]]<-1-(sse[[i]]/sst[[i]])
        }
        
        n_rsq<-unlist(rsq)
        
        
        ####calibration plot ##
        
        ##get wac vectors
        
        outwac=lineout$mean$wac
        
        lwac<-split(outwac,rep(1:nfile,vecwac()))
        
        lwac<-lapply(lwac,rep,nrac())
        
        
        lrac<-split(rac(),rep(1:nfile,vecrac()))
        
        cl=NULL
        
        length=length(lineout$mean$predm)
        predm=lineout$sims.lis$predm
        for(i in 1:length){
          cl[[i]]<-quantile(predm[,i],0.025)
        }
        
        cl<-split(cl,rep(1:nfile,vecrac()))
        
        cu=NULL
        for(i in 1:length){
          cu[[i]]<-quantile(predm[,i],0.925)
        }
        
        cu<-split(cu,rep(1:nfile,vecrac()))
        
        
        ###Regression output###
        
        ########download
        
        bayesres=lineout$sims.list$mumeanfin
        
        bind1 <-list(wadm=input$wadm1,uwad=input$uwad1,
                     u_mid=umids(),u_mad=input$u_mad1,
                     u_mids=input$u_mids1,u_mdi=input$u_mdi1,
                     coverage=input$coverage1,Number_iteration=input$niters1,Length_burnin=input$nburnin1,
                     mean=mean(bayesres),sd=sd(bayesres),median=median(bayesres),
                     a=outrega,
                     b=outregb,
                     rsq=n_rsq,
                     vyp=outhetp)
        
        
        #######
        
        
        updateTabsetPanel(session = session, inputId = "results",selected = "Fit")
        #waiter_hide()
        
        
        return(list(mcmcout=bayesres,bind1=bind1,rsq=n_rsq, rac=lrac,wac=lwac,outa=outrega,linea=lineout$mean$a,lineb=lineout$mean$b,outb=outregb,cl=cl,cu=cu,hetp=outhetp))
        
      }
      ###choice=RF
    }else{
      ###N=1
      if(N_tables==1){
        
        caldata<-reactive({
          
          if (is.null(input$calfile1))
            return()
          else
          {
            csv=list()
            csv=read.csv(input$calfile1$datapath[1])
            return(csv)   
          }
          
        })
        
        ####merge sample data talbes
        
        sampledata<-reactive({
          
          if (is.null(input$samplefile1))
            return()
          else
          {
            
            csv=list()
            csv=read.csv(input$samplefile1$datapath[1])
            return(csv)
          }
          
        })
        
        
        wadm<<-reactive(
          return(append(as.numeric(unlist(strsplit(gsub("\\s", "", input$wadm1),","))),1))
          
        )
        
        uwad<<-reactive(
          return(append(as.numeric(unlist(strsplit(gsub("\\s", "", input$uwad1),","))),1))
          
        )
        
        ####
        
        
        NWS <- reactive({
          
          inFile <- input$calfile1
          if (is.null(input$calfile1)){
            return(NULL)
          }else{
            
            tmp<- read.csv(inFile$datapath[1])
            
            return(max(tmp$wsol))# #working solutions from  calibrationtable
            
            
          }
        })
        
        standard1<-reactive(
          input$standard1
        )
        umids<-reactive({
          
          if(standard1()==1){
            return(c(rep(input$u_mid1,nfile)))
          }else{
            return(c(rep(1,nfile)))
          }
        })
        
        umads<-reactive(
          
          return(c(rep(input$u_mad1,nfile)))
          
        )
        
        
        ####
        
        NS<-reactive({
          
          if (is.null(input$calfile1))
            return()
          else
          {
            NS<-nrow(input$calfile1)
            return(NS)
            
          }
          
        })
        
        
        
        NT<-reactive(
          
          return(N*NR)
          
        )
        
        
        
        
        #####
        
        midsm<-reactive(
          if(standard1()==1){
            caldata()$mid
          }else{
            
            caldata()$mad/caldata()$mad
            
          }
        ) 
        
        madsm<-reactive(
          caldata()$mad
        )
        
        wsol<-reactive(
          caldata()$wsol
        )
        
        
        rac<- reactive({
          
          if (is.null(input$calfile1))
            return()
          else
          {
            
            csv=list()
            
            for(i in 1: nfile)
            {
              csv[[i]]=read.csv(input$calfile1$datapath[i])
              csv[[i]][1:3]<-NULL
            }
            
            return(unlist(csv)) # rbind the csv
          }
          
        })
        
        
        ras<- reactive({
          
          if (is.null(input$samplefile1))
            return()
          else
          {
            nfile<-nrow(input$samplefile1)
            
            csv=list()
            
            for(i in 1: nfile)
            {
              csv[[i]]=read.csv(input$samplefile1$datapath[i])
              csv[[i]][1:2]<-NULL
            }
            
            return(unlist(csv)) # rbind the csv
          }
          
        })
        
        
        mdi<-reactive(
          
          sampledata()$mdi
          
        )
        
        
        midsi<-reactive(
          if(standard1()==1){
            sampledata()$mids
            
          }else{
            
            sampledata()$mdi/sampledata()$mdi
          }
        )
        
        MT<-reactive(
          
          return(M*MR)
          
        )
        
        
        
        sol<-reactive({
          rep(1:N,NR)
        })
        
        sampl<-reactive({
          rep(1:M,NR)
          
        })   
        
        
        #####
        
        linedata = list(
          midsm=midsm(),
          madsm=madsm(),
          wsol=wsol(),
          rac=rac(),
          midsi=midsi(),
          mdi=mdi(),
          ras=ras(),
          umids=umids(),
          umads=umads(),
          wadm= wadm(),
          uwad=uwad(),
          NWS=NWS()+1,
          N=N,
          M=M,
          MT=MT(),
          NT=NT(),
          sol=sol(),
          NR=NR,
          sampl=sampl()
        )
        
        
        # 
        require(R2OpenBUGS)
        ##################################################################
        ##################################################################
        ### Define the model
        ##################################################################
        ##################################################################
        
        
        linemodel<-function(){#calculate known mass ratios wac for N calibrants.
          
          midsprec<-1/(umids*umids)
          madsprec<-1/(umads*umads)
          
          for(i in 1:NWS){
            wadprec[i]<-1/(uwad[i]*uwad[i])
            wad[i]~dnorm(wadm[i],wadprec[i])
          }
          
          for(i in 1:N){mids[i]~dnorm(midsm[i],midsprec)
            mads[i]~dnorm(madsm[i],madsprec)
          }
          for(i in 1:N){wac[i]<-wad[wsol[i]]*mads[i]/mids[i]}  
          
          #######
          #calibration equation
          
          
          b~dnorm(0,1.0E-5)
          
          xins~dnorm(0,0.0016)%_%I(0.001,)
          chsqns~dgamma(0.5,0.5)
          fitprec<-xins/sqrt(chsqns)
          
          for(i in 1:N){mean[i]<-b*wac[i]
          predm[i]~dnorm(mean[i],fitprec)}
          for(i in 1:NT){rac[i]~dnorm(mean[sol[i]],fitprec)
          }
          
          ###
          # Compute the mass fraction wd
          
          b.cut<-cut(b)
          sigras~dgamma(1.0E-5,1.0E-5)
          wdsig~dgamma(1.0E-3,1.0E-3)
          
          wd~dnorm(0,1.0E-5)
          for(i in 1:M){wdm[i]~dnorm(wd,wdsig)
            rasmean[i]<-b.cut*wdm[i]*mdi[i]/midsi[i]
            rasmeanp[i]~dnorm(rasmean[i],fitprec)}
          for(i in 1:MT){ras[i]~dnorm(rasmeanp[sampl[i]],sigras)}
        }
        
        ##################################################################
        ##################################################################
        ### Run OpenBUGS
        ##################################################################
        ##################################################################
        
        withProgress(message = 'Running Bayes', style="old",value = 0, {
          
          
          
          lineinits<-function(){list(sigras=1,wdsig=1,b=1)}
          
          lineout<-bugs(data=linedata,
                        inits=lineinits,
                        parameters=c("b","wd","wdm","wac","mean","predm"),   
                        model.file=linemodel,
                        n.iter = input$niters, n.burnin = input$nburnin, n.thin = 10,n.chains = 1)#,debug=T)    
        })
        
        ######
        attach.bugs(lineout) ## imports the random draws for the parameters
        cl<-NULL
        
        for(i in 1:N){
          for(j in 1:NR){cl<-c(cl,quantile(predm[,i], 0.025))}}
        
        cl<-rep(unique(cl), NR)
        cu<-NULL
        for(i in 1:N){
          for(j in 1:NR){cu<-c(cu,quantile(predm[,i], 0.925))}}
        cu<-rep(unique(cu), NR)
        
        
        
        
        
        
        outregb<-data.frame(mean=lineout$mean$b,sd=lineout$sd$b)
        outwdm<-data.frame(mean=lineout$mean$wdm,sd=lineout$sd$wdm)
        outmean<-data.frame(mean=lineout$mean$mean)
        outwd<-data.frame(mean=lineout$mean$wd,sd=lineout$sd$wd )
        
        
        
        
        
        ########## sets up the data to plot the regression line and calculate the rsq
        
        outwac=lineout$mean$wac
        
        lwac<-rep(outwac,NR)
        
        ############ calculates the rsq
        ybar<-outmean
        sst<-sum((rac()-mean(rac()))^2)
        sse<-sum((rac()-ybar)^2)
        rsq<-1-sse/sst
        
        ###Regression output###
        bayesres=lineout$sims.list$wd
        ####dowload
        
        bind1 <-list(wadm=input$wadm1,uwad=input$uwad1,
                     u_mid=umids(),u_mad=input$u_mad1,
                     u_mids=input$u_mids1,u_mdi=input$u_mdi1,
                     coverage=input$coverage1,Number_iteration=input$niters1,Length_burnin=input$nburnin1,
                     mean=mean(bayesres),sd=sd(bayesres),median=median(bayesres),
                     rfactor=outregb )
        
        ####
        
        
        
        updateTabsetPanel(session = session, inputId = "results",selected = "Fit")
        
        
        return(list(mcmcout=bayesres,bind1=bind1,outb=outregb,rac=rac(),wac=lwac,lineb=lineout$mean$b,cl=cl,cu=cu))
        
        ####N>1
      }else{
        ####merge calibration data talbes
        caldata<-reactive({
          
          if (is.null(input$calfile1))
            return()
          else
          {
            nfile<<-nrow(input$calfile1)
            
            csv=list()
            
            for(i in 1: nfile)
            {
              csv[[i]]=read.csv(input$calfile1$datapath[i])
              
            }
            
            do.call(rbind,csv) # rbind the csv
          }
          
        })
        
        ####merge sample data talbes
        
        sampledata<-reactive({
          
          if (is.null(input$samplefile1))
            return()
          else
          {
            nfile=nrow(input$samplefile1)
            csv=list()
            for(i in 1: nfile)
            {
              csv[[i]]=read.csv(input$samplefile1$datapath[i])
              
            }
            
            do.call(rbind,csv) # rbind the csv
          }
          
        })
        
        
        wadm<<-reactive(
          return(append(as.numeric(unlist(strsplit(gsub("\\s", "", input$wadm1),","))),1))
          
        )
        
        uwad<<-reactive(
          return(append(as.numeric(unlist(strsplit(gsub("\\s", "", input$uwad1),","))),1))
          
        )
        
        ####
        
        
        NWS <- reactive({
          
          inFile <- input$calfile1
          if (is.null(input$calfile1)){
            return(NULL)
          }else{
            
            if(stringr::str_ends(input$calfile1$datapath[1], "csv")) {
              tmp=read.csv(input$calfile1$datapath[1])
              
            }
            else if(stringr::str_ends(input$calfile1$datapath[1], "xlsx")) {
              tmp=read.xlsx(input$calfile1$datapath[1],1)
            }
            #tmp<- read.csv(inFile$datapath[1])
            
            return(max(tmp$wsol))# #working solutions from  calibrationtable
            
            
          }
        })
        
        
        
        standard1<-reactive(
          input$standard1
        )
        umids<-reactive({
          
          if (is.null(input$calfile1))
            return()
          else
          {
            
            if (standard1()==1){
              nfile<<-nrow(input$calfile1)
              
              return(c(rep(input$u_mid1,nfile)))
              
            }else{
              
              nfile<<-nrow(input$calfile1)
              return(c(rep(1,nfile)))
            }
          }
        })
        
        umads<-reactive(
          if (!is.null(input$calfile1)){
            nfile<-nrow(input$calfile1)
            return(c(rep(input$u_mad1,nfile)))
          }
        )
        
        
        ####
        
        NS<-reactive({
          
          if (is.null(input$calfile1))
            return()
          else
          {
            NS<-nrow(input$calfile1)
            return(NS)
            
          }
          
        })
        
        
        
        NT<-reactive(
          
          length(caldata()$mid)
          
        )
        
        expc<-reactive({
          
          if (is.null(input$calfile1))
            return()
          else
          {
            nfile<-nrow(input$calfile1)
            
            
            expc=list()
            
            for(i in 1: nfile)
            {
              nr= nrow(read.csv(input$calfile1$datapath[i]))
              expc[[i]]<-(c(rep(1*i,each=nr)))
              expc[[i]]<-t(expc[[i]])
            } 
            
            
            
            as.vector(do.call(cbind, expc))
            
            
          }
          
        })
        
        
        
        midsm<-reactive(
          if(standard1()==1){
            caldata()$mid
            
          }else{
            
            caldata()$mad/ caldata()$mad
            
          }
        )
        madsm<<-reactive(
          caldata()$mad
        )
        
        wsol<<-reactive(
          caldata()$wsol
        )
        
        
        
        NTT <- reactive({
          
          inFile <- input$calfile1
          if (is.null(input$calfile1)){
            return(NULL)
          }else{
            
            
            tmp=list()
            
            for(i in 1: nfile)
            {
              tmp[[i]]=read.csv(input$calfile1$datapath[i])
              
            }
            
            tmp<-do.call(rbind,tmp)
            
            NR<<-ncol(tmp)-3# number of replicates for each calibrant
            NT<-nrow(tmp)
            
            NTT<-NR*NT
            
          }
        })
        
        
        expc1<-reactive({
          
          if (is.null(input$calfile1))
            return()
          else
          {
            nfile<-nrow(input$calfile1)
            
            
            
            expc1=list()
            
            for(i in 1: nfile)
            {
              nr<<-nrow(read.csv(input$calfile1$datapath[i]))
              nrac<-ncol(read.csv(input$calfile1$datapath[i]))-3
              n1<-nr*nrac
              expc1[[i]]<-matrix(c(rep(1*i,each=n1)))
              
            } 
            
            
            as.vector(do.call(rbind, expc1))
            
            
          }
          
        })
        
        expc2<-reactive({
          
          if (is.null(input$calfile1))
            return()
          else
          {
            nfile<-nrow(input$calfile1)
            
            csv=list()
            
            k<-nrow(read.csv(input$calfile1$datapath[1]))
            nrac<-ncol(read.csv(input$calfile1$datapath[1]))-3
            
            
            csv[[1]]=c(rep(1:k,nrac))
            
            
            for(i in 2: nfile)
            {
              
              c=max(csv[[i-1]])+1
              x=nrow(read.csv(input$calfile1$datapath[i]))
              y=c+x-1
              csv[[i]]=c(rep(c:y, nrac))
              
            }
            
            as.vector(unlist(csv))
            
            
            
          }
          
        })
        
        
        
        
        rac<- reactive({
          
          if (is.null(input$calfile1))
            return()
          else
          {
            nfile<-nrow(input$calfile1)
            
            csv=list()
            
            for(i in 1: nfile)
            {
              csv[[i]]=read.csv(input$calfile1$datapath[i])
              csv[[i]][1:3]<-NULL
            }
            
            return(unlist(csv)) # rbind the csv
          }
          
        })
        
        
        ras<- reactive({
          
          if (is.null(input$samplefile1))
            return()
          else
          {
            nfile<-nrow(input$samplefile1)
            
            csv=list()
            
            for(i in 1: nfile)
            {
              csv[[i]]=read.csv(input$samplefile1$datapath[i])
              csv[[i]][1:2]<-NULL
            }
            
            return(unlist(csv)) # rbind the csv
          }
          
        })
        
        
        md<<-reactive(
          
          mdi<-sampledata()$mdi
          
        )
        
        
        
        midsi<<-reactive(
          if(standard1()==1){
            sampledata()$mids
          }else{
            
            sampledata()$mdi/sampledata()$mdi
          } 
          
        )
        
        MT<<-reactive(
          
          length(sampledata()$mids)
          
        )
        
        MTT <- reactive({
          
          inFile <- input$samplefile1
          if (is.null(input$samplefile1)){
            return(NULL)
          }else{
            
            
            tmp=list()
            
            for(i in 1: nfile)
            {
              tmp[[i]]=read.csv(input$samplefile1$datapath[i])
              
            }
            
            tmp<-do.call(rbind,tmp)
            
            NR<<-ncol(tmp)-2# number of replicates for each calibrant
            NT<-nrow(tmp)
            
            NTT<-NR*NT
            
          }
        })
        
        exp<-reactive({
          
          if (is.null(input$samplefile1))
            return()
          else
          {
            nfile<-nrow(input$samplefile1)
            
            
            exp=list()
            
            for(i in 1: nfile)
            {
              nr= nrow(read.csv(input$samplefile1$datapath[i]))
              exp[[i]]<-(c(rep(1*i,each=nr)))
              exp[[i]]<-t(exp[[i]])
            } 
            
            as.vector(do.call(cbind, exp))
            
          }
          
        })
        
        
        expr<-reactive({
          
          if (is.null(input$samplefile1))
            return()
          else
          {
            nfile<<-nrow(input$samplefile1)
            
            
            
            expr=list()
            
            for(i in 1: nfile)
            {
              nr<<-nrow(read.csv(input$samplefile1$datapath[i]))
              nrac<-ncol(read.csv(input$samplefile1$datapath[i]))-2
              n1<-nr*nrac
              expr[[i]]<-matrix(c(rep(1*i,each=n1)))
              
            } 
            
            
            as.vector(do.call(rbind, expr))
            
            
          }
          
        })
        
        expr2<-reactive({
          
          if (is.null(input$samplefile1))
            return()
          else
          {
            nfile<-nrow(input$samplefile1)
            
            csv=list()
            
            k<-nrow(read.csv(input$samplefile1$datapath[1]))
            nras<-ncol(read.csv(input$samplefile1$datapath[1]))-2
            
            
            csv[[1]]=c(rep(1:k,nras))
            
            
            for(i in 2: nfile)
            {
              
              c=max(csv[[i-1]])+1
              x=nrow(read.csv(input$samplefile1$datapath[i]))
              y=c+x-1
              csv[[i]]=c(rep(c:y, nras))
              
            }
            
            as.vector(unlist(csv))
            
            
            
          }
          
        })
        
        nfile<-reactive({
          
          if (!is.null(input$calfile1))
          {
            nfile<-nrow(input$calfile1)
          }
        })
        
        nrac<-reactive({
          
          if (!is.null(input$calfile1))
          {
            nrac<-ncol(read.csv(input$calfile1$datapath[1]))-3
            
          }
        })
        
        vecwac <- reactive({
          
          if (!is.null(input$calfile1))
          {
            
            vec=list()
            for(i in 1: nfile){
              vec[[i]]<-nrow(read.csv(input$calfile1$datapath[i]))
            }
            return(vec)
          }
        })
        
        vecrac <- reactive({
          
          if (!is.null(input$calfile1))
          {
            
            vecrac=list()
            for(i in 1: nfile){
              vecrac[[i]]<-nrac()*nrow(read.csv(input$calfile1$datapath[i]))
            }
            return(vecrac)
          }
        })
        
        vecras <- reactive({
          
          if (!is.null(input$samplefile1))
          {
            
            vecras=list()
            for(i in 1: nfile){
              vecras[[i]]<-nrac()*nrow(read.csv(input$samplefile1$datapath[i]))
            }
            return(vecras)
          }
        })
        
        linedata<-list(umids=umids(),
                       umads=umads(),
                       wadm=wadm(),
                       uwad=uwad(),
                       NWS=NWS()+1,
                       NS=NS(),
                       NT=NT(),
                       expc=expc(),
                       midsm=midsm(),
                       madsm=madsm(),
                       wsol=wsol(), 
                       NTT=NTT(), 
                       expc1=expc1(),
                       expc2=expc2(),
                       rac=rac(),
                       ras=ras(),
                       MT=MT(),MTT=MTT(),
                       exp=exp(),
                       midsi=midsi(),
                       md=md(),
                       expr=expr(),
                       expr2=expr2())
        
        
        require(R2OpenBUGS)
        # #   
        # #   ##################################################################
        # #   ##################################################################
        # #   ### Define the model
        # #   ##################################################################
        # #   ##################################################################
        # #   
        # #   
        linemodel<-function(){####this program uses NS number of input tables. 
          ### NT is the total number of elements of midsm and madsm (total number of calibrants). 
          ### expc gives the experiment designation for each element in midsm and madsm. NTT is the total number of measurements in the calibration experiment.
          ### expc1 gives the experiment label for each observation in rac, expc2 gives calibrant designation for each rac.
          ### MT gives the total number of samples, MTT gives the total number of observations in the quantitation experiment 
          ### exp gives the experiment designation for each sample, expr gives the experiment designation for each element in ras, 
          ### expr2 gives the sample designation for each element in ras.
          
          
          for(i in 1:NWS){wadprec[i]<-1/(uwad[i]*uwad[i])
          wad[i]~dnorm(wadm[i],wadprec[i])}
          wadmean<-mean(wad[])
          
          for(i in 1:NS){midsprec[i]<-1/(umids[i]*umids[i])
          madsprec[i]<-1/(umads[i]*umads[i])}
          
          
          for(i in 1:NT){
            mids[i]~dnorm(midsm[i],midsprec[expc[i]])
            mads[i]~dnorm(madsm[i],madsprec[expc[i]])
            ratio[i]<-mads[i]/mids[i] 
            wac[i]<-wad[wsol[i]]* ratio[i] } 
          
          #######
          #calibration equation
          for(i in 1:NS){
            
            b[i]~dnorm(0,1.0E-5)
            
            xins[i]~dnorm(0,0.0016)%_%I(0.001,)
            chsqns[i]~dgamma(0.5,0.5)
            fitprec[i]<-xins[i]/sqrt(chsqns[i])
            
          }
          
          for(i in 1:NTT){mean[i]<-b[expc1[i]]*wac[expc2[i]]
          rac[i]~dnorm(mean[i],fitprec[expc1[i]])
          predm[i]~dnorm(mean[i],fitprec[expc1[i]])
          
          }
          
          
          ###
          # Compute the mass fraction wd
          for(i in 1:NS){
            
            b.cut[i]<-cut(b[i])
            xinsi[i]~dnorm(0,0.0016)%_%I(0.001,)
            chsqnsi[i]~dgamma(0.5,0.5)
            sigras[i]<-xinsi[i]/sqrt(chsqnsi[i])
            
            xinsw[i]~dnorm(0,0.0016)%_%I(0.001,)
            chsqnsw[i]~dgamma(0.5,0.5)
            wdsig[i]<-xinsw[i]/sqrt(chsqnsw[i])
            wd[i]~dnorm(0,1.0E-5)}
          
          for(i in 1:MT){
            wdm[i]~dnorm(wd[exp[i]],wdsig[exp[i]])
            rasmean[i]<-b.cut[exp[i]]*wdm[i]*md[i]/midsi[i]
            rasmeanp[i]~dnorm(rasmean[i],fitprec[exp[i]])}
          
          
          
          for(i in 1:MTT){
            ras[i]~dnorm(rasmeanp[expr2[i]],sigras[expr[i]])
            
          } 
          
          # 
          
          #############################################
          T~dcat(P[])
          P[1:NS]~ddirich(alpha[])
          for(i in 1:NS){alpha[i]<-1}
          mumeanfin<-wd[T]
          
        }
        
        ##################################################################
        ##################################################################
        ### Run OpenBUGS
        ##################################################################
        ##################################################################
        
        withProgress(message = 'Running Bayes', style="old",value = 0, {
          
          lineinits<-function(){list(b=c(1,1),wd=c(0,0))}
          
          lineout<-bugs(data=linedata,
                        inits=lineinits,
                        parameters=c("wd","wdm","mumeanfin","wac","b","mean","predm"),   
                        model.file=linemodel,
                        n.iter = 10000, n.burnin = 5000, n.thin = 10,n.chains =1)#,debug=T)
        })
        
        ###########################################
        ### additional result generation
        
        attach.bugs(lineout) ## imports the random draws for the parameters
        
        
        outregb<-data.frame(mean=lineout$mean$b,sd=lineout$sd$b)
        outwdm<-data.frame(mean=lineout$mean$wdm,sd=lineout$sd$wdm)
        outmean<-data.frame(mean=lineout$mean$mean)
        outwd<-data.frame(mean=lineout$mean$wd,sd=lineout$sd$wd )
        outmumeanfin<-data.frame(mean=lineout$mean$mumeanfin,sd=lineout$sd$mumeanfin )
        
        ########## sets up the data to plot the regression line and calculate the rsq
        
        require(data.table)
        
        
        ############ calculates the rsq
        
        ybar<-split(outmean,rep(1:nfile,vecrac()))
        rac_rsq<-split(rac(),rep(1:nfile,vecrac()))
        
        sst=list()
        sse=list()
        rsq=list()
        for(i in 1: nfile)
        {
          sst[[i]]<-sum((rac_rsq[[i]]-mean(rac_rsq[[i]]))^2)
          sse[[i]]<-sum((rac_rsq[[i]]-ybar[[i]])^2)
          rsq[[i]]<-1-(sse[[i]]/sst[[i]])
        }
        
        n_rsq<-unlist(rsq)
        
        
        ####calibration plot ##
        
        ##get wac vectors
        
        outwac=lineout$mean$wac
        
        lwac<-split(outwac,rep(1:nfile,vecwac()))
        
        lwac<-lapply(lwac,rep,nrac())
        
        
        lrac<-split(rac(),rep(1:nfile,vecrac()))
        
        cl=NULL
        
        length=length(lineout$mean$predm)
        predm=lineout$sims.list$predm
        for(i in 1:length){
          cl[[i]]<-quantile(predm[,i],0.025)
        }
        
        cl<-split(cl,rep(1:nfile,vecrac()))
        
        cu=NULL
        for(i in 1:length){
          cu[[i]]<-quantile(predm[,i],0.9995)
        }
        
        cu<-split(cu,rep(1:nfile,vecrac()))
        
        
        ###Regression output###
        
        
        bayesres=lineout$sims.list$mumeanfin
        
        ##### download
        bind1 <-list(wadm=input$wadm1,uwad=input$uwad1,
                     u_mid=umids(),u_mad=input$u_mad1,
                     u_mids=input$u_mids1,u_mdi=input$u_mdi1,
                     coverage=input$coverage1,Number_iteration=input$niters1,Length_burnin=input$nburnin1,
                     mean=mean(bayesres),sd=sd(bayesres),median=median(bayesres),
                     rfactor=outregb )
        
        updateTabsetPanel(session = session, inputId = "results",selected = "Fit")
        #waiter_hide()
        
        
        return(list(mcmcout=bayesres,bind1=bind1,rsq=n_rsq, rac=lrac,wac=lwac,lineb=lineout$mean$b,outb=outregb,cl=cl,cu=cu))
        
        
      }
      
      
    }
    # waiter_show(
    #   spin_circle(),
    #   color = "#87A96B",
    #   "Just a moment ..."
    #   
    # )
    
    ###
    
  })
  
  ##################################################################
  ### Outputs of Bayesian analysis #################################
  ##################################################################
  
  ## TODO: delete all outputs when anything is changed in inputs
  
  output$mu_est1 <- renderText({
    bayesres1=outbayes1()[["mcmcout"]]
    paste(format(mean(bayesres1),digits = input$digit1+2))
  })
  
  
  output$mu_se1 <- renderText({
    bayesres1=outbayes1()[["mcmcout"]]
    paste(format(sd(bayesres1),digits = input$digit1 ))
  })
  
  output$mu_median1 <- renderText({
    bayesres1=outbayes1()[["mcmcout"]]
    paste(format(median(bayesres1),digits = input$digit1+2))
  })
  
  
  output$coverageProbabilityPercentBayes1 <- renderText({
    paste(input$coverage1*100,"%",sep="")
  })
  
  
  output$mu_quant1 <- renderText({
    bayesres1=outbayes1()[["mcmcout"]]
    credible.interval=quantile(bayesres1,c((1-input$coverage1)/2, (1+input$coverage1)/2))
    
    paste(format(credible.interval[1],digits = input$digit1+2),"to",format(credible.interval[2]))
  })
  
  
  # output$downloadbayesout1 <- downloadHandler(
  #   filename = "analysis_inputs$estimates.csv",
  #   content = function(file) {
  #     write.csv(outbayes1()[["bind1"]], file, row.names = FALSE)
  #   }
  # )
  # 
  output$downloadbayesout1 <- downloadHandler(
    filename = "Analysis Report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report1.Rmd")
      file.copy("report1.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(bayes=outbayes1() )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  ####calibration plot
  ###
  observeEvent(input$analysis,{
    
    choice1<-reactive(
      input$choice1
    )
    
    if (choice1()==1){
      
      if (!is.null(input$calfile1))
      {
        
        nfile<-nrow(input$calfile1)
        
        if(nfile==1){
          
          output$plots2 <- renderUI({
            
            output$plot3<-renderPlot({
              plot(outbayes1()[["wac"]],outbayes1()[["rac"]],main = paste("Calibration Plot"),type="p",pch=19, col=c("forestgreen","tomato"),xlab="wac",ylab="rac")
              lines(outbayes1()[["wac"]],outbayes1()[["cl"]],type="l",col="steelblue",lty="dashed")
              lines(outbayes1()[["wac"]],outbayes1()[["cu"]],type="l",col="steelblue",lty="dashed")
              abline(a=outbayes1()[["linea"]], b=outbayes1()[["lineb"]],col="red")
            })
            plotOutput("plot3")
          })
          ####test
          # output$plot4<-renderPlot({
          #   plot(outbayes1()[["wac"]],outbayes1()[["rac"]],main = paste("Calibration Plot"),type="p",pch=19, col=c("forestgreen","tomato"),xlab="wac",ylab="rac")
          #   lines(outbayes1()[["wac"]],outbayes1()[["cl"]],type="l",col="steelblue",lty="dashed")
          #   lines(outbayes1()[["wac"]],outbayes1()[["cu"]],type="l",col="steelblue",lty="dashed")
          #   abline(a=outbayes1()[["linea"]], b=outbayes1()[["lineb"]],col="red")
          # })
          
        }else{
          
          output$plots2 <- renderUI({
            plot_output_list <- lapply(1:nfile, function(i) {
              plotname <- paste("plot3", i, sep="")
              plotOutput(plotname, height = 400, width = 600)
            })
            do.call(tagList, plot_output_list)
          })
          
          lapply(1:nfile, function(i){
            output[[paste("plot3", i, sep="") ]] <- renderPlot({
              plot(outbayes1()[["wac"]][[i]],outbayes1()[["rac"]][[i]],main = paste("Calibration Plot", i),type="p",pch=19, col=c("forestgreen","tomato"),xlab="wac",ylab="rac")
              lines(outbayes1()[["wac"]][[i]],outbayes1()[["cl"]][[i]],type="l",col="steelblue",lty="dashed")
              lines(outbayes1()[["wac"]][[i]],outbayes1()[["cu"]][[i]],type="l",col="steelblue",lty="dashed")
              abline(a=outbayes1()[["linea"]][[i]], b=outbayes1()[["lineb"]][[i]])
            })
          })
        }
      }
    }else{
      
      if (!is.null(input$calfile1))
      {
        
        nfile<-nrow(input$calfile1)
        
        if(nfile==1){
          ymax=1.2*max(outbayes1()[["rac"]])
          xmax=1.2*max(outbayes1()[["wac"]])
          output$plots2 <- renderUI({
            
            output$plot3<-renderPlot({
              plot(outbayes1()[["wac"]],outbayes1()[["rac"]],main = paste("Calibration Plot"),type="p",pch=19, col=c("forestgreen","tomato"),xlab="wac",ylab="rac",xlim=c(0,xmax),ylim=c(0,ymax))
              abline(a=0, b=outbayes1()[["lineb"]],col="red")
            })
            plotOutput("plot3")
          })
          
        }else{
          
          ymax=list()
          xmax=list()
          for(i in 1: nfile){
            ymax[[i]]=1.2*max(outbayes1()[["rac"]][[i]])
            xmax[[i]]=1.2*max(outbayes1()[["wac"]][[i]])
          }
          
          output$plots2 <- renderUI({
            plot_output_list <- lapply(1:nfile, function(i) {
              
              plotname <- paste("plot3", i, sep="")
              plotOutput(plotname, height = 400, width = 600)
            })
            do.call(tagList, plot_output_list)
          })
          
          lapply(1:nfile, function(i){
            output[[paste("plot3", i, sep="") ]] <- renderPlot({
              plot(outbayes1()[["wac"]][[i]],outbayes1()[["rac"]][[i]],main = paste("Calibration Plot", i),type="p",pch=19, col=c("forestgreen","tomato"),xlab="wac",ylab="rac",xlim=c(0,xmax[[i]]),ylim=c(0,ymax[[i]]))
              abline(a=0, b=outbayes1()[["lineb"]][[i]],col="red")
            })
          })
        }
      }
    }
  })
  ####
  observeEvent(input$analysis,{
    
    choice1<-reactive(
      input$choice1
    )
    
    if (choice1()==1){
      output$out2 = renderUI ({
        
        output$rsq1 <- renderTable({outbayes1()[["rsq"]]},striped = F,spacing = "m",align="c",digits = 5)
        output$outrega1 <- renderTable({outbayes1()[["outa"]]},striped = F,spacing = "m",align="c",digits = 5)
        output$outregb1 <- renderTable({outbayes1()[["outb"]]},striped = F,spacing = "m",align="c",digits = 5)
        output$outhetp1 <- renderTable({outbayes1()[["hetp"]]},striped = F,spacing = "m",align="c",digits = 5)
        tagList(
          fluidRow(
            column(4,
                   p("R-squared: ",tableOutput("rsq1"))),
            column(4,
                   p("Intercept: ",tableOutput("outrega1"))),
            column(4,
                   p("Slope: ",tableOutput("outregb1")))),
          fluidRow(
            column(12,
                   p("heterogeneity variance as % of total y variance",tableOutput("outhetp1"))
            )),
        )
      })
    }else{
      output$out2 = renderUI ({
        output$outregb1 <- renderTable({outbayes1()[["outb"]]},striped = F,spacing = "m",align="c",digits = 5)
        fluidRow(
          column(6,
                 p("RF: ",tableOutput("outregb1"))
          ))
      })
      
    }
    
    
  })
  
  bayesplot_func1=function(){
    bayesres1=outbayes1()[["mcmcout"]]
    #   
    plot(density(bayesres1),col="violet",main="Posterior Distribution",xlab = "WD",lwd=3)
    #   
    z = density(bayesres1,n=4096)
    #   
    L = quantile(bayesres1, probs=(1-input$coverage1)/2)
    U = quantile(bayesres1, probs=(1+input$coverage1)/2)
    #   
    iS = which.min(abs(z$x-L))
    iE = which.min(abs(z$x-U))
    #   
    xp = c(L, U, z$x[iE:iS])
    yp = c(0, 0, z$y[iE:iS])
    #   
    polygon(xp, yp, border=F, col="thistle1")
    lines(z$x, z$y, type="l", col="violet", lwd=3)
    #   
    points(median(bayesres1),
           min(z$y)+0.4*par()$cxy[2], pch=19, bg="Blue", col="slateblue4")
    segments(L, 0, U, 0, lwd=2, col="slateblue4")
    #   
    
  }
  
  output$mu_post_plot1 <- renderPlot({bayesplot_func1()})
  
  
  output$download_mu_post_plot1 <- downloadHandler(
    filename = function() { paste( 'mean_post.pdf', sep='') },
    content = function(file) {
      pdf(file,height=9, width=8)
      bayesplot_func1()
      dev.off()
    })
  
  
  ##################################################################
  ##### Trace plot #################################################
  ##################################################################
  
  
  traceplot_func1=function(){
    bayesres1=outbayes1()[["mcmcout"]]
    
    traceColors=rainbow(1)
    plot(bayesres1,type="l",ylab = "WD")
    
    
  }
  
  output$mu_trace_plot1 <- renderPlot({traceplot_func1()})
  # 
  output$download_mu_trace_plot1 <- downloadHandler(
    filename = function() { paste( 'tracePlot.pdf', sep='') },
    content = function(file) {
      pdf(file,height=9, width=8)
      traceplot_func1()
      # dev.off()
    })
  
  ### 
})