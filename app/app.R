library(shiny)
library(tidyverse)
library(plotly)

read.csv("./app/sal_allDepts_22_10K.csv") -> raw
raw %>% mutate (Class = str_remove(Title, "\\s*\\d+$")) -> all -> data

read.csv("./app/salSched_c23_220701_ann_long.csv") -> sal_long

ui <- navbarPage(
  title = "State of Alaska Salaries, 2022",
  tabPanel("Salary by Title",
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("employers", "Select Department(s)", 
                         choices = c("All", unique(data$Employer)),
                         selected = "Alaska Department of Fish and Game"),
      selectizeInput("classes", "Select Job Class(es)", 
                     choices = c("All", unique(arrange(data, Class)$Class)),
                     multiple = TRUE, selected = (c
                                                  ('Gis Analyst','Biometrician','Research Analyst', 'Analyst/Programmer',
                                                    'Fish and Game Coordinator', 'Program Coordinator', 'Project Assistant',
                                                    'Fisheries Scientist', 'Fishery Biologist', 'Wildlife Biologist',
                                                    'Wildlife Scientist', 'Data Processing Manager', 'Fish & Game Coordinator', 
                                                    'Fish & Game Coordinator', 'Commissioner', 'Division Director', 'Deputy Director',
                                                    'Habitat Biologist', 'Natural Resource Manager', 'Division Operations Manager'))
      ), 
      width = 3,
      helpText("Warning: Selecting 'All' for both Department and Class may take several minutes to process."),
      HTML("<a href='https://www.openthebooks.com/alaska-state-employees/'> Data Source </a>"),
    ),
    
    mainPanel(
      plotlyOutput("plot", height = "800px", width = "100%")
    )
  )),
  
    tabPanel("Salary by Range and Step",
            plotlyOutput("plot2", height = "800px", width = "100%"), 
            helpText("Step advancement is annual until step J, at which point it becomes biennial."),
            HTML("<a href='https://doa.alaska.gov/dof/payroll/sal_sched.html'> Data Source </a>")
          )
        )

server <- function(input, output, session) {
  # Server logic for Tab 1
    # Create the scatter plot
    output$plot <- renderPlotly({
      if ("All" %in% input$employers && "All" %in% input$classes) {data -> data_subset}
      if ("All" %in% input$employers && !("All" %in% input$classes)) {
        data %>% filter(Class %in% input$classes) -> data_subset}
      if (!("All" %in% input$employers) && "All" %in% input$classes){
        data %>% filter(Employer %in% input$employers) -> data_subset}
      if (!("All" %in% input$employers) && !("All" %in% input$classes)){
        data %>% filter(Employer %in% input$employers & Class %in% input$classes) -> data_subset}
      
      
      p <- ggplot(data_subset, aes(x=Title, y=wages)) +
        geom_point(alpha = .2)
      
      # Add tooltip text
      p <- p + aes(text = paste(Name, "<br>",
                                Title, "<br>",
                                Employer, "<br>",
                                Annual.Wages))
      
      # Customize the plot
      p <- p + labs(x = "Title",
                    y = "Annual Wages (thousands)") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        scale_y_continuous(labels = scales::comma_format(scale = 1e-3))
      
      # Convert the plot to interactive mode using plotly
      ggplotly(p, tooltip = "text")
    })
  
  # Server logic for Tab 2
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
}

shinyApp(ui = ui, server = server)

