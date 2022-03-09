#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(tidyr)
library(truncdist)
library(vroom)
library(janitor)
library(zoo)
#library(hrbrthemes)
library(shiny)
library(flexdashboard)
library(parallel)
library(ggplot2)
library(data.table)
library(DT)
library(personograph)
library(RColorBrewer)
library(plotly)
library(shinythemes)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # navbarPage("NCAA Swimming", theme = shinytheme("lumen"),
  #            #theme = shinytheme("cerulean"),
  #            tabPanel("Program Finder", fluid = TRUE, icon = icon("globe-americas"),
  #                     tags$style(button_color_css),
  #                     # Sidebar layout with a input and output definitions
  #                     sidebarLayout(
  #                       sidebarPanel(
  
  
  #shinythemes::themeSelector(),
  # Application title
  titlePanel("P. aeruginosa risk dashboard"),
  fluidRow(
    HTML(paste0(
      "<p><a href='https://hecoira.leeds.ac.uk'>HECOIRA</a> is an EPSRC funded project which aims to estimate exposure to microorganisms in hospital patient rooms.</p>",
      "<p>This app predicts the infection risk to P. aeruginosa to a patient using a medical physiotherapy room after an infectious patient has left.</p>",
      "<p>Use the options below to investigate the effect that room ventilation rate, appointment length and time between appointments have on risk! \U0001F57A</p>"
    )),
    
    column(3,
           
           
           actionButton("go", "Go"),
           #Button not coded yet
           
           sliderInput("times", label = "Appointment time (mins):",
                       min = 30, max = 360, value = 180, step = 30),
           
           sliderInput("ach", label = "Air change rate:",
                       min = 0.2, max = 6, value = 2.3, step = .2),
           
           sliderInput("k", label = "Dose response (1/k):",
                       min = 1e-4, max = 5e-4, value = 0.000105, step = 1e-4),
           
           sliderInput("half_life", label = "Pathogen Half life (mins):",
                       min = 10, max = 500, value = 250, step = 10),
           
           sliderInput("delay", label = "Time between appointments (mins):",
                       min = 10, max = 60, value = 30, step = 10),
           
          # checkboxGroupInput('Mit', 'Mitigations:', c('Window Open', 'Air Cleaner', 'Door Open'), 
                         #     selected = c(1)),
           
          # selectInput("ach1", "Ventilation:",
                      # c("Good" = "AchGD",
                       #  "Average" = "AchAVG",
                       #  "Poor" = "AchPR")),
           
        #   selectInput("NM", "Number of people after infectious person:",
          #             c("1" = "NM1",
           #              "2" = "NM2",
            #             "3" = "NM3",
             #            "4" = "NM4",
              #           "5" = "NM5",
               #          "6" = "NM6",
                #         "7" = "NM7",
                 #        "8" = "NM8",
                  #       "9" = "NM9"
                   #    )),
           
        #   selectInput("Rm", "Room:",
         #              c("1" = "Room1",
          #               "2" = "Room2",
           #              "3" = "Room3",
            #             "4" = "Room4",
             #            "5" = "Room5",
              #           "6" = "Room6",
               #          "7" = "Room7"
                #       )),
           
         #  selectInput("Bug", "Pathogen Type:",
          #             c("Covid" = "CV",
           #              "Influenza" = "IN",
            #             "TB" = "TB",
             #            "Pseudomonas" = "PS",
              #           "Capacia" = "CP"
               #        )),
           
       #    selectInput("Factor", "Any evidence that the patient was contagious?:",
        #               c("Yes" = "Factor1",
         #                "No" = "Factor2"
          #             )),
           
          
           # will probs need a uniform Dist function slider as well
           
        #   sliderInput("lambda_d", label = "Pathogen Deposition Rate:",
         #              min = 0, max = 10, value = 6, step = 2),
           # need to look up what suitable deposition rate is
           # will probs need a uniform Dist function slider as well
           
        #   sliderInput("Person", label = "Infected person position:",
         #              min = 0, max = 10, value = 5, step = 1),
           
           # do I need a slider for E for source?
           
       
        #   img(src = "UOL_logo.png", height = 80, width = 200)
    ),
    
    # AGO slider i have added in do not do anything atm 26/01
    
    column(5,
           h3("Airborne concentration [particles per m3]"),
           plotOutput("violinPlot"),
           h3("Risk of infection to a single patient "),
           DT::dataTableOutput("Risk"),
           # h3("Dose inhaled by patient 3 [cfu] "),
           # DT::dataTableOutput("Dose")
           
           
    ),
    column(4,
           h3(" Risk per 100 patients"),
           plotOutput("waffle")
           
    )
    
  )
  
  
  
  # sidebarLayout(
  #     sidebarPanel(
  #         # sliderInput("bins",
  #         #             "Number of bins:",
  #         #             min = 1,
  #         #             max = 50,
  #         #             value = 30),
  #         
  #         sliderInput("ach", label = "Air change rage:",
  #                     min = 0.2, max = 6, value = 2.3, step = .2),
  #         
  #         sliderInput("k", label = "Dose response (1/k):",
  #                     min = 1e-4, max = 5e-4, value = 0.000105, step = 1e-4),
  #         
  #         sliderInput("times", label = "Appointment time (mins):",
  #                     min = 30, max = 360, value = 180, step = 30),
  #         
  #         sliderInput("delay", label = "Time between appointments (mins):",
  #                     min = 10, max = 60, value = 30, step = 10)
  #     ),
  # 
  #     # Show a plot of the generated distribution
  #     mainPanel(
  #        #plotOutput("distPlot"),
  #        plotOutput("violinPlot"),
  #        h3("Risk of infection to patient 3 [0-1]"),
  #        DT::dataTableOutput("Risk")
  #        #DT::dataTableOutput("mytable")
  #        
  #     )
  # )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  m<-1.52365; s<-1.151533
  # Breathing rate
  #p is in l/min
  p1<-c(10.8,13.1,8.2,11.6,7.2,8.6,10.9,11.0,10.8,9.9,11.4,10.6,9.8,10.9,
        9.3,8.6,12.0,12.1,12.6,9.9)
  p2=c(10.46, 11.48, 13.41, 16.13,16.55)
  #p3=c(10.01,9.94, 11.48,8.78, 13.41, 16.13,16.5) #AGO added in p3
  pval=c(p1,p2)#p3) #AGO added in p3
  meanp=mean(pval)
  stdp=sd(pval)
  pmin=meanp-stdp
  pmax=meanp+stdp
  pdiff=pmax-pmin
  SEM = stdp/sqrt(NROW(pval))             # Standard Error
  ts =  qt(c(.025, .975), df=NROW(pval)-1)   # T-Score
  CI = meanp + ts*SEM                     # Confidence Intervals
  
  N=500
  logE <- truncdist::rtrunc(n = N , spec = "norm" ,a = 0 ,b = 6, mean=m, sd=s)
  p <- ((CI[1]+runif(N)*CI[2]+runif(N))*0.001)/60 
  #p <- ((CI[1]+runif(N)*CI[2]+runif(N)*CI[3])*0.001)/60 #AGO added in p3
  # monte carlo simu for N, running it N times
  # error I get sum(unlist(data))==1 is not TRUE
  
  numerical_ODE<-function(t, state, parameters) {
    with(as.list(c(state, parameters)),{
      dC<-E/V-lambda*C
      list(dC)
    })
  }
  # numerical ode calc, dont need to run^^
  # AGO - dont fully understand this bit^^
  # AGO - think is adding in probability of infection for person, depending on variation in breathing rate, dont understand where random l/min comes from
  # AGO - also dont understand how this works^^^^
  
  # Analytical Solution --------------------------------------------------------
  
  
  
  analyticalODE <- function(times,E,V,lambda,C01){
    t=seq(0,NROW(times)-2) #need to convert from times seconds to 0-maxtime, to allow comparison with the ODE solver # problem if lambda is a function of time
    data.frame(times=t,
               C=E/(V*lambda)*(1-exp(-lambda*t))+C01*exp(-lambda*t))
  }
  
  # AUC function -------------------------------------------------------
  # AGO AUC means Area Under Curve, to find dose, then can apply to dose response curve
  
  AUC <- function(x,y){ #To allow for non-uniform time-steps
    sum(diff(x[order(x)])*zoo::rollmean(y[order(x)],2))
  }
  
  
  
  # Exposure function ----(AGO, dont know what variable p is)---------------------------------------------------
  exposure<-function(logE,p,ach,times,delay){
    
    
    E <- (10^(logE))/3600
    #Find lambda which is the addtion of the removal rates
    lambda_v <- ach/3600 #1 ac/h, so divide by 3600 to get ac/s
    #half life of PsA 30 to 151 mins from "Viability of Pseudomonas aeruginosa in cough aerosols generated by persons with cystic fibrosis "
    half_life <- (30+ (runif(1)*121));
    # AGO think this line will need to be deleted ^
    half_life_s  <- half_life *60; #get in seconds
    lambda_i <- log(2)/half_life_s   #removal due to inactivation
    lambda_d  <- 0     #removal due to deposition
    # AGO think this line will need to be deleted ^
    lambda  <- lambda_v+lambda_i+lambda_d
    
    #Variable volume
    V <- 31.9+(runif(1)*11.8)  #area from all room sizes
    # maybe could have a slider here to decide room volume 
    
    ####### Part 1 patient in the room
    
    t01=0
    tend1= as.numeric(input$times)*60 #X * 60 X is time in mins, to give seconds
    C01=0   #assume no intial concentration of CFU in the air
    
    times <- seq(t01, tend1, by = 1)
    parameters<-c(E,V,lambda)
    init<-c(C=C01)
    
    #out1 <- deSolve::lsoda(init, times = times, func = numerical_ODE, parms = parameters)%>% as_tibble()
    analyticalODE(times,E,V,lambda,C01)-> out1
    
    
    #l1=NROW(out[,1])
    #allC1[1:l1,i]=out[,2]
    #allt1[1:l1,i]=out[,1]
    #finalC1[1,i]=out[NROW(out[,2]),2]
    
    ###### Part 2 patient leaves the room
    
    #Intial conditions
    t02=tend1
    tend2= tend1 +(as.numeric(input$delay)*60); #X * 60 X is time in mins
    
    times <- seq(t02, tend2, by = 1)
    #C02=C1(end)    #takes the last concentration value form simulation 1
    #E0=0           #emission now 0
    
    parameters<-c(0,V,lambda)
    init<-c(C=out1$C[NROW(out1)])
    #out2 <- deSolve::lsoda(init, times = times, func = numerical_ODE, parms = parameters)%>% as_tibble()
    
    analyticalODE(times,0,V,lambda,out1$C[NROW(out1)])-> out2
    

    
    #for the AUC function
    #Inhaled particles
    #AGO tmp<-
    #  AGOout3%>%
    #AGO summarise(dose=AUC(times,C*p))%>%
    #AGO mutate(risk=1-exp(-dose*as.numeric(input$k))) #Note the multiplication instead of division, this is because of how the dose-response parameter is given
    
    
    #AGOn_result<-data.frame(C1=out1$C[NROW(out1)],
    # AGO                  C3=out3$C[NROW(out3)],
    #  AGO                  C2=out2$C[NROW(out2)],
    #   AGO               dose=tmp$dose,
    #     AGO            risk=tmp$risk
    #AGO)
    #AGOreturn(n_result)
    
    tmp<-
      out2%>%
      summarise(dose=AUC(times,C*p))%>%
      mutate(risk=1-exp(-dose*as.numeric(input$k))) #Note the multiplication instead of division, this is because of how the dose-response parameter is given
    
    
    n_result<-data.frame(C1=out1$C[NROW(out1)],
                         C2=out2$C[NROW(out2)],
                         dose=tmp$dose,
                         risk=tmp$risk
    )
    #randomVals <-  dont know how to work this button
    return(n_result)
    
    
  }
  
  
  # Reactive output ---------------------------------------------------------
  
  
  #AGO res<-reactive({
  #AGO  a<-mcmapply(FUN = exposure,logE,p,as.numeric(input$ach),as.numeric(input$times),as.numeric(input$delay),mc.cores = 1)%>%t()%>%as.data.frame() %>% unnest(cols=c(C1, C2, C3, dose, risk))%>%pivot_longer(!c(dose,risk))
  #AGO return(a)
  #AGO})
  res<-eventReactive(input$go,{
    a<-mcmapply(FUN = exposure,logE,p,as.numeric(input$ach),as.numeric(input$times),as.numeric(input$delay),mc.cores = 1)%>%t()%>%as.data.frame() %>% unnest(cols=c(C1, C2, dose, risk))%>%pivot_longer(!c(dose,risk))
    return(a)
  })
  #### Believe currently getting total risk for all patients, change this to be for just 1 and see what happens
  #AGO changed from reactive({ to eventReactive(input$go,{, therefore, calculation only happens when press go
  # res<-reactive({
  #   res<-data.frame(value=runif(1e3)*as.numeric(input$ach),name=rep(c("A","B"),each=500),risk=runif(1e3)*as.numeric(input$k))  
  #   res
  # })
  
  output$violinPlot<-renderPlot({
    
    
    res()%>%
      ggplot()+
      geom_violin(aes(y=value,x=name,fill=name),draw_quantiles = c(0.25,0.5,0.75))+
      scale_y_log10()+
      #facet_wrap(~surface,scales = "free",nrow = 2,labeller = labeller(.cols = surfaceTypes))+
      #scale_color_ipsum() +
      #scale_fill_ipsum() +
      scale_fill_brewer(palette = "Set1")+
      ylab("Final airborne concentration [cfu/m3]")+
      scale_x_discrete(labels= c("Infectious Patient","Susceptible patient 1", "Susceptible patient 2","Susceptible patient 3"))+
      xlab("")+
      labs(
        title="Airborne concentration",
        subtitle="Comparison between patient sessions",
        caption="1st patient is infecitous, 2nd,3rd and 4th patients are susceptible"
      ) +
      theme(legend.position="none")
    
    
  })
  
  output$Risk <- DT::renderDataTable(
    #reactive({
    res()%>%
      summarise(Mean =round(mean(risk,na.rm = T),digits = 3),
                SD = round(sd(risk,na.rm = T),digits = 3),
                Min = round(min(risk,na.rm = T),digits = 3),
                Max = round(max(risk,na.rm = T),digits = 3)
      ),
    options = list(dom = 't'),
    rownames = FALSE,
    caption = 'Table 1:  Risk of infection to patient 3 [0-1].'
    
    
    
  )
  #
  # output$Dose <- DT::renderDataTable(
  #   #reactive({
  #   res()%>%
  #     summarise(Mean =round(mean(dose,na.rm = T),digits = 3),
  #               SD = round(sd(dose,na.rm = T),digits = 3),
  #               Min = round(min(dose,na.rm = T),digits = 3),
  #               Max = round(max(dose,na.rm = T),digits = 3)
  #     ),
  #   options = list(dom = 't'),
  #   rownames = FALSE,
  #   caption = 'Table 2:  Dose inhaled by patient 3 [cfu].'
  #
  #
  #
  # )
  
  #})
  
  w <- reactive({
    a<-res() %>%
      filter(name=="C2")%>% #AGO (changed C1 to C4) Choose any patient otherwise you're triplicating the subsequent calculations
      select(risk) %>%
      mutate(NumInfected = list(rbinom(n=100, size = 100, prob = risk)))
    
    #Calculate mean and sd of all values in the nested lists
    ms <- function(a, col="NumInfected") {
      u <- unlist(a[[col]])
      return(data.frame(Mean=ceiling(mean(u)),SD=ceiling(sd(u))))
    }
    
    a<-ms(a)
    
    z<-rbind(a,c(100-a$Mean,100-a$SD))
    z<-z/100
    z$Infected <-  as.factor(c("Infected", "Uninfected"))
    data<-list(Infected=z$Mean[1], Possibly=z$SD[1],Uninfected=1-(z$Mean[1]+z$SD[1]) )
    return(data)
  })
  
  
  output$waffle<-renderPlot({
    
    wafflePlot<-
      w() %>%
      personograph( 
        n.icons=100, 
        dimensions=c(10,10), 
        plot.width=0.8,
        icon.style=2,
        colors=list(Uninfected="grey", Infected="red",Possibly="orange"),
        force.fill = TRUE,
        fig.cap = "Number of additional infected patients per 100 (mean=red, SD=orange)"
      )
    
    wafflePlot
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
