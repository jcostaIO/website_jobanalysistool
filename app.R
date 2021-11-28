library(dragulaR)
library(shiny)
library(shinyjs)
library(rhandsontable)

#####Load Data#####
load("ksa_cmr.RData")

#####Run custom functions####
makeElement <- function(data, name)
{
  div(style = "border-width:2px;border-style:solid;",
      drag = name,
      div(class = "active title", name),
      div(class = "active content"))
}

fields<-c("task1","task2","task3","task4")
####User-interface#####
ui <- fluidPage(h1((strong(span("Standard Setting and Job Analysis Tool",style="color:blue")))),
                tabsetPanel(
                  tabPanel("Select Data from O*NET Database (23.3)",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput('dataset','Choose a dataset:',choices=c("Knowledge","Skills","Abilities","Definitions"))),
                             mainPanel(DT::dataTableOutput('selected'))
                           )
                  ),
                  tabPanel("Move KSAs",fluid=TRUE,
                           mainPanel(fluidRow(
                             style = "margin: 15px;",
                             column(6,
                                    h2(p("KSAs",span("not true",style="color:red"),"of minimally competent candidate")),
                                    div(id = "Available", style = "min-height: 600px;",
                                        lapply(df$KSA, makeElement, data = df))
                             ),
                             column(6,
                                    h2(p("KSAs",span("true",style="color:green"),"of minimally competent candidate")),
                                    div(id = "Model", style = "min-height: 600px;"))
                           ),
                           dragulaOutput("dragula")
                           )
                  ),
                  tabPanel("Make Custom KSAs",
                           sidebarLayout(
                             sidebarPanel(fileInput('file1', 'Choose CSV File',
                                                    accept=c('text/csv', 
                                                             'text/comma-separated-values,text/plain', 
                                                             '.csv'))),
                             mainPanel(
                               DT::DTOutput("custom")
                               
                             )
                           )
                  ),
                  tabPanel("Task Builder",
                           sidebarLayout(
                             sidebarPanel(
                               textInput("task1","Do what?"),
                               textInput("task2","To what/whom?"),
                               textInput("task3","Why?"),
                               textInput("task4","How?"),
                               actionButton("save","Add")),
                             mainPanel(
                               DT::dataTableOutput("tasks"),
                               downloadButton('downloadData', 'Download'))
                           ))
                )
)

#####Server#####
server <- function(input, output) {
  #file input for custom KSAs
  df<-reactive({
    inFile <- input$file1
    
    if (is.null(input$file1))
      return(NULL)
    
    read.csv(inFile$datapath,stringsAsFactors = FALSE)
  })
  
  output$custom <- DT::renderDT(df(), editable=list(target='cell'),extensions = 'Buttons', options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  ))
  #reactive values so KSAs can be modified
  values<-reactiveValues()
  x<-df
  
  proxy<-DT::dataTableProxy("custom")
  
  #needed for editing cells in KSA table
  observeEvent(input$custom_cell_edit, {
    info = input$custom_cell_edit
    str(info)
    x <<- DT::editData(x(), info)
    DT::replaceData(proxy, x, resetPaging = FALSE)  # important
  })
  
  #necessary for making draggable KSAs
  output$dragula <- renderDragula({
    dragula(c("Available", "Model"))
  })
  
  #displays the ONET data
  output$selected<-DT::renderDataTable(DT::datatable({
    if(input$dataset=="Knowledge"){
      data<-knowledge[,c(1:4)]
    }
    if(input$dataset=="Skills"){
      data<-skills[,c(1:4)]
    }
    if(input$dataset=="Abilities"){
      data<-abilities[,c(1:4)]
    }
    if(input$dataset=="Definitions"){
      data<-cmr
    }
    data
  }))
  
  
  
  saveData <- function(data) {
    data <- as.data.frame(t(data))
    if (exists("responses")) {
      responses <<- rbind(responses, data)
      DT::datatable(responses,colnames=c("Do what?","To what/whom?","Why?","How?"),editable = 'cell')
    } else {
      responses <<- data
      DT::datatable(responses,colnames=c("Do what?","To what/whom?","Why?","How?"),editable = 'cell')
    }
  }
  
  loadData <- function() {
    if (exists("responses")) {
      responses
      DT::datatable(responses,colnames=c("Do what?","To what/whom?","Why?","How?"),editable = 'cell')
    }
  }
  
  # Whenever a field is filled, aggregate all form data
  #formData is a reactive function
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  # When the Save button is clicked, save the form data
  observeEvent(input$save, {
    saveData(formData())
    
  })
  
  # Show the previous responses
  # (update with current response when save is clicked)
  
  output$tasks <-  DT::renderDataTable({
    input$save
    loadData()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("modified tasks", Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(responses, file)
    }
  )
  
}

####Run Application####
shinyApp(ui = ui, server = server)
