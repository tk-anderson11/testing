# Demo RShiny app to show Revenues and Expenses for the entity: the State of Utah from table A-4
# The user can select 'type', then a specific range of years, and a 'line_item' to explore

library(shiny)
library(plotly)
library(tidyverse)
library(scales)
library(shinyWidgets)
library(reactable)


# figure out how to freeze side bar?
# 
# FILTER OUT "Other Financing Sources (Uses)"
# Update initial line graph of percent change
# add in revised percent change tables (add in dollar change) -- reformat, drop % character

# Add a percentage amount by year table?


## Build user interface
ui <- fluidPage(
  titlePanel("Demo Data App"),
  
  sidebarLayout(
    sidebarPanel(
      
      uiOutput("uio_type"),
      uiOutput("uio_year"),
      uiOutput("uio_line_item"),
      
      
    ),
    
    mainPanel(
      plotlyOutput("linePlot"),
      
      # Adding in the stacked bar chart
      plotlyOutput("barPlot"),
      tags$p("Numbers from table A-4 in the 2024 ACFR (pg 249)", style = "font-size: smaller; color: gray;"),
      reactableOutput("percentageChangeTable"),
      plotlyOutput("percentageChangePlot"),
      
      
      hr(),
      
      tags$h1("Change Over Time Section:"),
      tabsetPanel(
        tabPanel("Year-over-Year Change",
                 br(),
                 reactableOutput("year_table"),
                 br(),
                 plotlyOutput("year_plot_change"),
                 plotlyOutput("year_plot"),
        ),
        tabPanel("5-Year Change",
                 br(),
                 tableOutput("five_year_table")
        ),
        tabPanel("10-Year Change",
                 br(),
                 tableOutput("ten_year_table")
        )
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  joint_data_long_r <- reactive({
    ## Load data, remove NAs from 'type' column and remove 1st column
    joint_data_long <- read_csv("joint_data_long.csv") %>%
      filter(type != "Special Item") %>%
      filter(!is.na(type)) %>%
      select(-1)
  })
  
  output$uio_type <-renderUI({
    req(joint_data_long_r())
    
    items <- joint_data_long_r() %>% 
      pull(type) %>% 
      unique() %>% 
      sort()
      
    selectInput("type", 
                "Select Type:",
                choices = items)
  })
  
  output$uio_year <-renderUI({
    req(input$type)
    
    items <- joint_data_long_r() %>% 
      filter(type %in% input$type) %>% 
      pull(year) %>% 
      unique() %>% 
      sort()
    
    pickerInput(
      inputId = "year",
      label = "Select Year(s):",
      choices = items,
      selected = items,
      multiple = TRUE,
      options = list(`actions-box` = TRUE) # Add a select/deselect all button
    )
    
  })
  
  output$uio_line_item <-renderUI({
    req(input$type,
        input$year)
    
    # print(input$type)
    # print(input$year)
    
    items <- joint_data_long_r() %>% 
      filter(type %in% input$type) %>% 
      filter(year %in% input$year) %>% 
      pull(line_item) %>% 
      unique() %>% 
      sort()
    
    
    # print(items)
    
    pickerInput(
      inputId = "line_item",
      label = "Select Line Item(s):",
      choices = items, # Populate choices here
      selected = items[1], # Select the first item
      multiple = TRUE,
      options = list(`actions-box` = TRUE) # Add a select/deselect all button
    )
    
  })
  

  # Reactive data filtering
  filtered_data <- reactive({
    
    req(joint_data_long_r(),
        input$type,
        input$year,
        input$line_item
        )
    
    data <- joint_data_long_r() %>%
      filter(type %in% input$type)

    if (!is.null(input$year) && length(input$year) > 0) {
      data <- data %>%
        filter(year %in% input$year)
    }

    if (!is.null(input$line_item) && length(input$line_item) > 0) {
      data <- data %>%
        filter(line_item %in% input$line_item)
    }
    return(data)
  })

  # Create the line plot
  output$linePlot <- renderPlotly({
    data <- filtered_data()

      p <- ggplot(data, aes(x = factor(year),
                            y = amount,
                            color = line_item,
                            group = line_item)) +
        geom_line() +
        geom_point() +
        labs(title = paste(input$type, "over time"),
             x = "Fiscal Year",
             y = "Amount",
             color = "Line Item") +
        theme_bw() +
        scale_y_continuous(labels = label_currency(
                            # make the axis have nice abbreviations for millions, billions, etc
                            scale_cut = cut_short_scale(),
                            style_negative = "parens" )) +
        # scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
        aes(text = paste0("Year: ", year, "<br>",
                          "Line Item: ", line_item, "<br>",
                          "Amount:", scales::dollar(amount)
                          )
            )

      ggplotly(p, tooltip = "text")
  })
  
  # Adding in the stacked bar chart
  output$barPlot <- renderPlotly({
    data <- filtered_data()
    
    p <- ggplot(data, aes(x = factor(year),
                          y = amount,
                          fill = line_item)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = paste(input$type, "over time"),
           x = "Fiscal Year",
           y = "Amount",
           fill = "Line Item") +
      theme_bw() +
      scale_y_continuous(labels = label_currency(
        scale_cut = cut_short_scale(),
        style_negative = "parens" )) +
      aes(text = paste0("Year: ", year, "<br>",
                        "Line Item: ", line_item, "<br>",
                        "Amount:", scales::dollar(amount)))
    
    ggplotly(p, tooltip = "text")
  })
  
  
  

  # Percent Change Stuff =======================================================
  
  
  
  # Reactive Data Filtering
  # filtered_data <- reactive({
  #   req(input$line_item) # Ensure line_item is selected
  #   
  #   revenue_data %>%
  #     filter(line_item == input$line_item)
  # })
  
  # Year-over-Year Calculation and Table
  year_data <- reactive({
    filtered_data() %>%
      janitor::clean_names() %>%
      group_by(line_item) %>%
      arrange(year) %>%
      mutate( 
        amount_lag = lag(amount, default = first(amount))
        ) %>% 
    
    mutate(
        amount_change = amount - amount_lag,
        percentage_change=(amount_lag - amount)/amount_lag
      ) %>%
      ungroup() %>%
      # mutate(percentage_change = sprintf("%.2f%%", percentage_change)) %>%
      select(year, line_item, amount,amount_lag, amount_change, percentage_change)
  })
  
  output$year_table <- renderReactable({
    year_data() %>% 
      mutate(
        amount = amount %>% dollar(),
        amount_change = amount_change %>% dollar(),
        percentage_change = percentage_change %>% percent()
      ) %>% 
      reactable(
        columns = list(
          year = colDef(name="Fiscal Year"),
          line_item = colDef(name="Line Item"),
          amount = colDef(name="Amount"),
          amount_change = colDef(name="Change"),
          percentage_change = colDef(name="Percent Change")
        )
      )
  })
  
  output$year_plot <- renderPlotly({
    
    # Find the maximum year
    # max_year <- max(year_data()$year)
    
    # Filter out the maximum year for plotting
    plot_data <- year_data() %>%
      # filter(year != max_year) %>%
      arrange(year) # Ensure data is sorted by year for correct line plotting
    
    # Convert percentage change to numeric (and handle potential errors)
    plot_data <- plot_data %>%
      mutate(year = year %>% factor())
      # mutate(percentage_change_numeric = as.numeric(gsub("%", "", percentage_change)))
    
    
    p <- ggplot(plot_data, aes(x = year,  # Use year directly if it's already numeric
                               y = percentage_change, # use the converted numeric column
                               color = line_item,
                               group = line_item,
                               text = paste("Year: ", year, "<br>Percentage Change: ", percentage_change))) +
      geom_line() +
      geom_point() +
      labs(title = paste(input$type, "Dollar Change over time"),
           x = "Fiscal Year",
           y = "Percent",
           color = "Line Item") +
      theme_bw() +
      scale_y_continuous(labels = label_percent(
        # make the axis have nice abbreviations for millions, billions, etc
        scale_cut = cut_short_scale(),
        style_negative = "parens" )) +
      # scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
      aes(text = paste0("Year: ", year, "<br>",
                        "Line Item: ", line_item, "<br>",
                        "Percentage Change::", scales::percent(percentage_change)))
    
    ggplotly(p, tooltip = "text")
  })
  
  
  output$year_plot_change <- renderPlotly({
    
    # Find the maximum year
    # max_year <- max(year_data()$year)
    
    # Filter out the maximum year for plotting
    plot_data <- year_data() %>%
      # filter(year != max_year) %>%
      arrange(year) # Ensure data is sorted by year for correct line plotting
    
    # Convert percentage change to numeric (and handle potential errors)
    plot_data <- plot_data %>%
      mutate(year = year %>% factor())
    # mutate(percentage_change_numeric = as.numeric(gsub("%", "", percentage_change)))
    
    
    p <- ggplot(plot_data, aes(x = year,  # Use year directly if it's already numeric
                               y = amount_change, # use the converted numeric column
                               color = line_item,
                               group = line_item,
                               text = paste("Year: ", year, "<br>Amount Change: ", amount_change))) +
      geom_line() +
      geom_point() +
      labs(title = "Percentage Change over time",
           x = "Fiscal Year",
           y = "Amount Change",
           color = "Line Item") +
      theme_bw() +
      scale_y_continuous(labels = label_currency(
        # make the axis have nice abbreviations for millions, billions, etc
        scale_cut = cut_short_scale(),
        style_negative = "parens" )) +
      # scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
      aes(text = paste0("Year: ", year, "<br>",
                        "Line Item: ", line_item, "<br>",
                        "Amount Change::", scales::dollar(amount_change)
      )
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  
  # 5-Year Change Calculation and Table
  five_year_data <- reactive({
    filtered_data() %>%
      janitor::clean_names() %>%
      arrange(year) %>%
      mutate(
        amount_lag5 = lag(amount, n = 5),
        amount_lag5_change = amount - amount_lag5,
        amount_lag5_change_perc = (amount - amount_lag5) / amount_lag5
      ) %>%
      select(year, amount, amount_lag5_change, amount_lag5_change_perc)
  })
  
  output$five_year_table <- renderTable({
    five_year_data()
  })
  
  # 10-Year Change Calculation and Table
  ten_year_data <- reactive({
    filtered_data() %>%
      janitor::clean_names() %>%
      arrange(year) %>%
      mutate(
        amount_lag10 = lag(amount, n = 9),  # corrected lag n=9
        amount_lag10_change = amount - amount_lag10,
        amount_lag10_change_perc = (amount - amount_lag10) / amount_lag10
      ) %>%
      select(year, amount, amount_lag10_change, amount_lag10_change_perc)
  })
  
  output$ten_year_table <- renderTable({
    ten_year_data()
  })
  
  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
