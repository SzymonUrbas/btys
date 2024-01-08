library(shiny)
library(shinyjs)
Nplayer = 6
Nmilk = 4

ColPal = palette.colors(palette = "Okabe-Ito")

mostProbCol = function(MilkMatrix){
  freqTab = rep(0,ncol(MilkMatrix))
  nmc = 1e3
  iter = 0
  while(iter<nmc){
    tmp = apply(MilkMatrix,2,function(x){sample(x,1)})
    idx = which(tmp == max(tmp))
    freqTab[idx] = freqTab[idx]+1
    iter = iter+1
  }
  return(which(freqTab == max(freqTab)))
}

# Define UI for random distribution app ----
ui <- fluidPage(
  shinyjs::useShinyjs(),

  # App title ----
  titlePanel("Dairy Dynamics: Select the milk sample to get the highest score"),
  wellPanel(

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      actionButton("updateData", "Next round"),
      actionButton("updateChoice", "Submit choice"),
      actionButton('resetButton',"RESTART"),

      # Input: Selector for milk ----
      selectInput("choice", "Make your choice:",
                  c(1:Nmilk)),
      helpText("True values will be revealed after submitting a choice"),

      
      # actionButton("Button1", "Run"),
      # shinyjs::hidden(p(id = "text1", "Processing..."))
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Summary", verbatimTextOutput("summary"))
      )

    )
  ))
  # ,
  # # wellPanel(
  # #   fluidRow(verbatimTextOutput("phase"))
  # # )
  ,
  wellPanel(
    fluidRow(tableOutput("Results"))
  )
  ,
  wellPanel(
    fluidRow(plotOutput("ResultsPlot"))
  )

)

# Define server logic for random distribution app ----
server <- function(input, output, session) {

  Accrued = reactiveVal(rep(0,Nplayer+1))
  Niter = reactiveVal(0)

  RunningAverage = reactiveVal(numeric(0))


  currPhase = reactiveVal(TRUE) # !resultPhase

  currResult = reactiveVal(NULL)

  NPCchoice = reactiveVal(NULL)
  shinyjs::disable("updateData")

  # initial ----
  mus = rnorm(Nmilk)+100
  sig = 1/rgamma(Nmilk, 5, 5)

  x = matrix(rnorm(3e2*Nmilk,mus, sig),nrow = 3e3, ncol = Nmilk,
            byrow = TRUE)
  # boxplot(x)
  MilkMatrix <- reactiveVal(x)


  # eventReactive expression to generate the requested distribution ----


  
# ==========================================================================
# Update the data ----
# ==========================================================================
  observeEvent(input$updateData,{

    mus = rnorm(Nmilk)+100
    sig = 1/rgamma(Nmilk, 5, 5)

    x = matrix(rnorm(1e2*Nmilk,mus, sig),nrow = 1e3, ncol = Nmilk,
              byrow = TRUE)
    MilkMatrix(x)

    newPhase = !currPhase()
    currPhase(newPhase)
    shinyjs::enable("updateChoice")
    shinyjs::disable("updateData")
  })
  
# ==========================================================================
# Update the choice ----
# ==========================================================================
  observeEvent(input$updateChoice,{

    currResult(sample(seq(Nmilk),1))


    newPhase = !currPhase()
    currPhase(newPhase)

    tmpRes = MilkMatrix()[currResult(),]

    tmpProfit = rep(0,Nplayer+1)


    # Player ----
    tmpProfit[1] = tmpRes[as.numeric(input$choice)]
    # NPC ----

    tmpSmm = rbind(colMeans(MilkMatrix()),
                apply(MilkMatrix(), 2, quantile, probs = 0.025),
                apply(MilkMatrix(), 2, quantile, probs = 0.975),
                apply(MilkMatrix(),2, median)
                )

    tmpNPC = rep(NA, Nplayer)

    tmpNPC[1] = which(tmpSmm[1,] == max(tmpSmm[1,]) )
    tmpNPC[2] = which(tmpSmm[2,] == max(tmpSmm[2,]) )
    tmpNPC[3] = which(tmpSmm[3,] == max(tmpSmm[3,]) )
    tmpNPC[4] = which(tmpSmm[4,] == max(tmpSmm[4,]) )
    tmpNPC[5] = mostProbCol(MilkMatrix())
    tmpNPC[6] = sample(seq(Nmilk),1)


    tmpProfit[2:7] = tmpRes[tmpNPC]

    Accrued(Accrued() +tmpProfit)
    print(tmpNPC)
    NPCchoice(tmpNPC)


    
    Niter(Niter()+1)
    RunningAverage(cbind(RunningAverage(),Accrued()/Niter()))


    shinyjs::disable("updateChoice")
    shinyjs::enable("updateData")
  })



  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot <- renderPlot({
    # boxplot(MilkMatrix())
    yrng = range(MilkMatrix())
    yrng[2] = yrng[2]+3
    plot(0,0, type = 'l', xlim = c(0.8,Nmilk+0.2), ylim = yrng,
      xlab = 'Milk option', ylab = 'Predicted Score', xaxt = 'n',
      cex.lab = 1.4)
    axis(1, at = 1:4, las = 1)
    abline(h = seq(floor(yrng[1]), ceiling(yrng[2])), lty = 2,
     col = 'grey', lwd = 0.5)
    for(i in seq(Nmilk)){
      lines(c(i,i), quantile(MilkMatrix()[,i],c(0.025,0.975)),
       type = 'l', lwd = 18, col = 'grey')
      points(c(i,i), quantile(MilkMatrix()[,i],c(0.025,0.975)),
       lwd = 5, pch = c(24,25), cex = 1.5, bg = 'black')
      points(i, mean(MilkMatrix()[,i]), pch = 16, cex = 3)
    }



    if(!currPhase()){
      points(1:Nmilk, MilkMatrix()[currResult(),], lwd = 4, cex = 3,
        bg = 'red', pch = 23, col = 'white')
      ChoiceCounts = rep(0,Nmilk)
      text(as.numeric(input$choice),yrng[2], "Player", cex = 1.5)
      ChoiceCounts[as.numeric(input$choice)] = ChoiceCounts[as.numeric(input$choice)]+1

      for(i in seq(Nplayer)){
        NPCtmp = as.numeric(NPCchoice()[i])
        text(NPCtmp,yrng[2] - diff(yrng)*0.06*ChoiceCounts[NPCtmp], paste0("NPC ",i), cex = 1.5,
          col = ColPal[i+1])
        ChoiceCounts[NPCtmp] = ChoiceCounts[NPCtmp]+1
      }
      # points(as.numeric(input$choice), MilkMatrix()[currResult(),][as.numeric(input$choice)],
      #    col = 'blue', lwd = 3, pch = 3)

      # points(NPCchoice(), MilkMatrix()[currResult(),][NPCchoice()],
      #    col = 'green', lwd = 3, pch = 3)

    }
  })

  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(MilkMatrix())
  })

  # # Generate an HTML table view of the data ----
  # output$table <- renderTable({
  #   head(MilkMatrix())
  # })

  output$Results <- renderTable({
    # tmp = Accrued()
    # names(tmp) = c("Player", paste0("NPC_",1:Nplayer))
    # tmp
    tmp = data.frame(matrix(Accrued(),1,Nplayer+1))
    colnames(tmp) = c("Player", paste0("NPC_",1:Nplayer))
    rownames(tmp) = c("Total score")
    tmp
  }, rownames = TRUE)

  output$ResultsPlot <- renderPlot({
    # print(paste0("Niter():",as.numeric(Niter())))
    # print(paste0("RunningAverage():",as.matrix(RunningAverage())))
    Iter = as.numeric(Niter())
    CurrRA = as.matrix(RunningAverage())
    if(Iter == 0){
      plot(0,0, type = 'l', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n',
        main = 'Play the game to see results')
      # text(0,0, 'Play the game to see results', cex = 3)
    } else if(Iter<11) {
      # print(Iter)
      plot(0,0, type = 'l', ylim = range(CurrRA), xlim = c(1,(Iter)),
        xlab = 'Round', main = 'Running average results', ylab = '')
        for(i in 1:(Nplayer+1)){
          lines(1:Iter, CurrRA[i,], col = i, lty = i, lwd = 3)
          points(Iter, CurrRA[i,Iter], col = i, lwd = 3, cex = 2, pch = 16)
        }
        legend('topleft', legend = c('Player', paste('NPC', seq(Nplayer))),
                lty = 1:8, col = ColPal[1:8], lwd =3, pch = 16)

    } else {
      plot(0,0, type = 'l', ylim = range(CurrRA[,(Iter-10):Iter]), xlim = c(Iter-10,Iter),
        xlab = 'Round', main = 'Running average results', ylab = '')
      for(i in (Nplayer+1):1){
        lines((Iter-10):Iter, CurrRA[i,(Iter-10):Iter], col = ColPal[i], lty = i, lwd = 3)
        points(Iter, CurrRA[i,Iter], col = ColPal[i], lwd = 3, cex = 2, pch = 16)
      }
      legend('topleft', legend = c('Player', paste('NPC', seq(Nplayer))),
              lty = 1:8, col = ColPal[1:8], lwd =3, pch = 16)

    }
    
    # tmp = Accrued()
    # names(tmp) = c("Player", paste0("NPC_",1:Nplayer))
    # tmp
    # tmp = data.frame(matrix(Accrued(),1,Nplayer+1))
    # colnames(tmp) = c("Player", paste0("NPC_",1:Nplayer))
    # tmp
  })



  output$phase <- renderPrint({
    cat(paste0("currPhase:", currPhase(),"\n",
      "profit: ", Accrued()[1],"\n",
      "runningAverage: ", round(Accrued()/Niter(),2))[1])
  })

  observeEvent(input$resetButton,{
    aggg_result = -1
    if(aggg_result == -1)
    {
      session$reload()
      return()
      print("session reload not working")
    }

    print("Code running this line")

    output$code_ran <- renderText("code Ran this line without refreshing")

  })

  # plotReady <- reactiveValues(ok = FALSE)

  # observeEvent(input$Button1, {
  #   shinyjs::disable("Button1")
  #   shinyjs::show("text1")
  #   plotReady$ok <- FALSE
  #   # do some cool and complex stuff
  #   Sys.sleep(2)
  #   plotReady$ok <- TRUE
  # })  

  # output$plot <-renderPlot({
  #   if (plotReady$ok) {
  #     shinyjs::enable("Button1")
  #     shinyjs::hide("text1")
  #     hist(rnorm(100, 4, 1),breaks = 50)
  #   }
  # })

}

shinyApp(ui = ui, server = server)