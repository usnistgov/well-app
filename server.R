options(java.parameters = "-Xss2560k")

library(shiny)
library(shinycssloaders)
library(rmarkdown)
library('xlsx')

shinyServer(function(input, output,session) {
  
  
  
  #####display uploaded files
  ###caltables###
  observe({
    if (!is.null(input$file)) {
      N_tables <<- length(input$file[, 1])
      
      upload <- list()
      for (i in 1:N_tables) {
        upload[[i]] <- read.csv(input$file$datapath[i])
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
  
  
  
  output$downloadCalTable <- downloadHandler(
    filename = function(){
      paste("calibration_input_template","xlsx",sep=".")
    },
    content = function(file) {
      file.copy("calibration_input_template.xlsx",file)
    }
  )
  

  output$downloadSamTable <- downloadHandler(
    filename = function(){
      paste("sample_input_template","xlsx",sep=".")
    },
    content = function(file) {
      file.copy("sample_input_template.xlsx",file)
    }
  )
  
  #####run pre-experiment module
  
  
  data <- reactive({
    req(input$file)
    read_csv(input$file$datapath)
  })
  
  NCsigv <- reactive({
    b1 <- lm(NC ~ factor(NCs), data = data())
    anova_b1 <- anova(b1)
    MSANC <- anova_b1["Mean Sq"][1,]
    MSENC <- anova_b1["Mean Sq"][2,]
    repsigNCv <- sqrt(MSENC)
    sqrt((MSANC-MSENC)/input$nrepNC)
  })
  
  TCsigv1 <- reactive({
    b2 <- lm(TC1 ~ factor(TC1s), data = data())
    anova_b2 <- anova(b2)
    MSATC1 <- anova_b2["Mean Sq"][1,]
    MSETC1 <- anova_b2["Mean Sq"][2,]
    repsigTCv1 <- sqrt(MSETC1)
    sqrt((MSATC1-MSETC1)/input$nrepTC1)
  })
  
  TCsigv2 <- reactive({
    b3 <- lm(TC2 ~ factor(TC2s), data = data())
    anova_b3 <- anova(b3)
    MSATC2 <- anova_b3["Mean Sq"][1,]
    MSETC2 <- anova_b3["Mean Sq"][2,]
    repsigTCv2 <- sqrt(MSETC2)
    sqrt((MSATC2-MSETC2)/input$nrepTC2)
  })
  
  repsigNCv <- reactive({
    b1 <- lm(NC ~ factor(NCs), data = data())
    anova_b1 <- anova(b1)
    anova_b1["Mean Sq"][2,]
  })
  
  repsigTCv1 <- reactive({
    b2 <- lm(TC1 ~ factor(TC1s), data = data())
    anova_b2 <- anova(b2)
    anova_b2["Mean Sq"][2,]
  })
  
  repsigTCv2 <- reactive({
    b3 <- lm(TC2 ~ factor(TC2s), data = data())
    anova_b3 <- anova(b3)
    anova_b3["Mean Sq"][2,]
  })

 
  
observeEvent(input$go,{
  output$preresult <- renderTable({
  
      data.frame(
        NCsigv = format(NCsigv(), digits = 4),
         TCsigv1 = format(TCsigv1(),digits = 4),
         TCsigv2 = format(TCsigv2(),digits = 4),
        repsigNCv = format(repsigNCv(),digits = 4),
        repsigTCv1 = format(repsigTCv1(),digits = 4),
        repsigTCv2 = format(repsigTCv2(),digits = 4)
      )
   
  })
})
  ####varibles used 
  
  
  ####Run experiment in this module
  


  output$expui<-renderUI(
    
    fluidRow(style='margin: 0px;',
             column(12,
                    checkboxInput("exp", label = strong("Run the experiment design module and specify the parameters",style = "color:seagreen"), value = F)
             ))
  )
  #   }
  # })
  ######standard options
  
  
  
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
  ### DOE code ########################################
  ##################################################################
  ##################################################################
  

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
            h4("Warning: please check the inputs!",style = "color:red")
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
 
})