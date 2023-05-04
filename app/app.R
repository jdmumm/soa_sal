# https://jdmumm.shinyapps.io/soa_sal/

library(shiny)
library(tidyverse)
library(plotly)
library(knitr)
library(kableExtra)
library(DT)
library(scales)

read.csv("sal_allDepts_22.csv") -> raw
raw %>% mutate (Class = str_remove(Title, "\\s*\\d+$"),
                wages = as.numeric(gsub("[^0-9.]", "", Annual.Wages))) -> data

read.csv("salSched_c23_220701_ann_long.csv") -> sal_long

read.csv("sal_allDepts_22_sum.csv", row.names = 1) -> summ
  format_col <- function(x) {paste0("$", format(round(x, 2), nsmall = 2, big.mark = ","))}
  summ[,2:4] <- lapply(summ[,2:4], format_col) 

default_classes <- c('Fish and Game Coordinator', 'Program Coordinator', 'Project Assistant',
  'Fisheries Scientist', 'Fishery Biologist', 'Wildlife Biologist',
  'Wildlife Scientist', 'Data Processing Manager', 'Fish & Game Coordinator',
  'Biometrician','Research Analyst', 'Analyst/Programmer', 'Gis Analyst',
  'Fish & Game Coordinator', 'Commissioner', 'Division Director', 'Deputy Director',
  'Habitat Biologist', 'Natural Resource Manager', 'Division Operations Manager',
  'Special Projects Assistant', 'Special Assistant To The Commissioner', 'Special Projects Assitant')

ui <- navbarPage(
  title = "State of Alaska Salaries, 2022",
  tabPanel("Salary Plot by Title",
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("employers", "Select Department(s)", 
                         choices = c("All", unique(data$Employer)),
                         selected = "Alaska Department of Fish and Game"),
      selectizeInput("classes", "Select Job Class(es)", 
                     choices = c("All", unique(arrange(data, Class)$Class)),
                     multiple = TRUE, selected = (default_classes)
      ), 
      width = 3,
      helpText("Warning: Selecting 'All' for both Department and Class may take several minutes to process."),
      HTML("<a href='https://www.openthebooks.com/alaska-state-employees/'> Data Source </a>"),
    ),
    
    mainPanel(
      plotlyOutput("plot", height = "800px", width = "100%")
    )
  )),
  
    tabPanel("Nominal Salary Schedule",
            plotlyOutput("plot2", height = "800px", width = "100%"), 
            helpText("Step advancement is annual until step J, at which point it becomes biennial."),
            HTML("<a href='https://doa.alaska.gov/dof/payroll/sal_sched.html'> Data Source </a>")
          ),
    
  #tabPanel("Summary Table by Title", tableOutput("kable_tab"))
  tabPanel("Summary Table", DT::dataTableOutput("table"))
)

server <- function(input, output, session) {
  #Tab 1
    output$plot <- renderPlotly({
      if ("All" %in% input$employers && "All" %in% input$classes) {data -> data_subset}
      if ("All" %in% input$employers && !("All" %in% input$classes)) {
        data %>% filter(Class %in% input$classes) -> data_subset}
      if (!("All" %in% input$employers) && "All" %in% input$classes){
        data %>% filter(Employer %in% input$employers) -> data_subset}
      if (!("All" %in% input$employers) && !("All" %in% input$classes)){
        data %>% filter(Employer %in% input$employers & Class %in% input$classes) -> data_subset}
      
      
      p <- ggplot(data_subset, aes(x=Title, y=wages)) +
        geom_point(alpha = .15)
      
      p <- p + aes(text = paste(Name, "<br>",
                                Title, "<br>",
                                Employer, "<br>",
                                Annual.Wages))
      
            p <- p + labs(x = "Title",
                    y = "Annual Wages (thousands)") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        scale_y_continuous(labels = scales::comma_format(scale = 1e-3))
      
      ggplotly(p, tooltip = "text")
    })
  
  #Tab 2
    output$plot2 <- renderPlotly({
    ggplot(sal_long, aes(x = Step, y = Salary/1000, group = Range, color = as.factor(Range))) +
      geom_line() +
      labs(x = "Step", y = "Salary (thousands)", color = "Range")-> s
    
    s <- s + aes(text = paste("Range: ", Range, "<br>",
                              "Step: ", Step, "<br>",
                              "Salary: ", format(Salary, big.mark = ",", decimal.mark = ".", 
                                                 nsmall = 2), "<br>"
    ))
    ggplotly(s, tooltip = "text")
    })
    
  #Tab 3
    output$table <- DT::renderDataTable({
      datatable(summ,
                filter = "top",
                options = list(
                  lengthMenu = list(c(10, 20, 50, 100, -1), c("10", "20", "50", "100",  "All")),
                  pageLength = -1,
                  orderClasses = TRUE
                ))
    })
}

shinyApp(ui = ui, server = server)

