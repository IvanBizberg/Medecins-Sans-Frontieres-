
# Libraries ---------------------------------------------------------------


library(shiny)
library(bslib)
library(crosstalk) # to use filter_select()
library(tidyverse)
library(readxl)
library(plotly)
library(here)


# Settings ----------------------------------------------------------------

options(scipen = 999)

# Data --------------------------------------------------------------------
df <- read_excel("MSF_DATA.xlsx") %>% 
  mutate(`Years of donate` = as.numeric(`Years of donate`))

# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- page_fluid(
  title = NULL,
  theme = bs_theme(version = 5, bootswatch = "solar",
                   base_font = font_google("Ubuntu"),
                   primary = "grey",
                   secondary = "#C44B00"),
  h1(
    class = "text-center",
    class = "text-primary",
    "Ad Targeting Campaign"
  ),
  layout_column_wrap(
    width = 1/3,
    value_box(
      "Total donations amount",
      scales::dollar(6193380),
      showcase = bsicons::bs_icon("coin"),
      theme = "bg-danger"
    ),
    value_box(
      "Average donation amount",
      scales::dollar(1227.384),
      showcase = bsicons::bs_icon("coin"),
      theme = "bg-info"
    ),
    value_box(
      "Probability to receive donation by audience",
      62.5,
      showcase = bsicons::bs_icon("Percent"),
      theme = "bg-warning"
    )
  ),
  layout_column_wrap(
    width = 1/2,
    card(
      full_screen = T,
      card_header("Gender"),
      layout_sidebar(
        card_body(
          plotlyOutput("Gender"),
          fillable = T,
          min_height = 600
        ),
        fill = T,
        sidebar = sidebar(
          position = c("left"),
          open = F,
          gap = 0,
          accordion_panel( 
            " Click here to select plot type",
            icon = bsicons::bs_icon("sliders"),
            # class = "text-secondary",
            card(
              radioButtons(inputId = "Gender_ID", label = h5("Plot mean or total donation"),
                           choices = list("Show mean" = "Mean", "Show total amount" = "Total"), 
                           selected = "Total"),
            )
          )
        )
      )
    ), 
    card(
      full_screen = T,
      card_header("Age"),
      layout_sidebar(
        card_body(
          plotlyOutput("Age"),
          fillable = T,
          min_height = 600
        ),
        fill = T,
        sidebar = sidebar(
          position = c("left"),
          open = F,
          gap = 0,
          accordion_panel( 
            " Click here to select plot type",
            icon = bsicons::bs_icon("sliders"),
            # class = "text-secondary",
            card(
              radioButtons(inputId = "Age_ID", label = h5("Plot mean or total donation"),
                           choices = list("Show mean" = "Mean", "Show total amount" = "Total"), 
                           selected = "Total"),
            )
          )
        )
      )
    ), card(
      full_screen = T,
      card_header("Profession"),
      layout_sidebar(
        card_body(
          plotlyOutput("Profession"),
          fillable = T,
          min_height = 600
        ),
        fill = T,
        sidebar = sidebar(
          position = c("left"),
          open = F,
          gap = 0,
          accordion_panel( 
            " Click here to select plot type",
            icon = bsicons::bs_icon("sliders"),
            # class = "text-secondary",
            card(
              radioButtons(inputId = "Profession_ID", label = h5("Plot mean or total donation"),
                           choices = list("Show mean" = "Mean", "Show total amount" = "Total"), 
                           selected = "Total"),
            )
          )
        )
      )
    ), card(
      full_screen = T,
      card_header("Ever_Married"),
      layout_sidebar(
        card_body(
          plotlyOutput("Ever_Married"),
          fillable = T,
          min_height = 600
        ),
        fill = T,
        sidebar = sidebar(
          position = c("left"),
          open = F,
          gap = 0,
          accordion_panel( 
            " Click here to select plot type",
            icon = bsicons::bs_icon("sliders"),
            # class = "text-secondary",
            card(
              radioButtons(inputId = "Ever_Married_ID", label = h5("Plot mean or total donation"),
                           choices = list("Show mean" = "Mean", "Show total amount" = "Total"), 
                           selected = "Total"),
            )
          )
        )
      )
    ),  
  )
)

server <- function(input, output) {
  
  
  
  output$Gender <- renderPlotly({
    
    if(input$Gender_ID == "Mean"){
      Gender_avg <- df %>% dplyr::summarise(`Average Donation amount` = mean(`Donation amount`, na.rm = T), .by = "Gender") %>% 
        mutate(Gender = fct_reorder(Gender, `Average Donation amount`)) %>% 
        ggplot(aes(x = Gender, y = `Average Donation amount`)) + geom_col(aes(fill = `Average Donation amount`)) +
        theme_minimal() +
        scale_fill_viridis_c() + 
        theme(axis.title = element_text(size = 20),
              axis.title.x = element_text(size = 20, colour = "grey"),
              axis.title.y = element_text(size = 20, colour = "grey"),
              axis.text = element_text(size = 12, colour = "grey"),
              panel.background = element_rect(fill = "#1e434a"),
              panel.grid = element_line(colour = "#292929", linetype = "dashed"),
              plot.background = element_rect(fill = "#1e434a"),
              legend.background = element_rect(fill = "#1e434a"),
              legend.text = element_text(colour = "grey"),
              legend.title = element_text(colour = "grey")) 
      
      Gender_avg_plotly <- Gender_avg %>% ggplotly(.)
      
      Gender_avg_plotly
      
    } else {
      Gender_sum <- df %>% dplyr::summarise(`Total Donation amount` = sum(`Donation amount`, na.rm = T), .by = "Gender") %>% 
        mutate(Gender = fct_reorder(Gender, `Total Donation amount`)) %>% 
        ggplot(aes(x = Gender, y = `Total Donation amount`)) + geom_col(aes(fill = `Total Donation amount`)) +
        theme_minimal() +
        scale_fill_viridis_c() + 
        theme(axis.title = element_text(size = 20),
              axis.title.x = element_text(size = 20, colour = "grey"),
              axis.title.y = element_text(size = 20, colour = "grey"),
              axis.text = element_text(size = 12, colour = "grey"),
              panel.background = element_rect(fill = "#1e434a"),
              panel.grid = element_line(colour = "#292929", linetype = "dashed"),
              plot.background = element_rect(fill = "#1e434a"),
              legend.background = element_rect(fill = "#1e434a"),
              legend.text = element_text(colour = "grey"),
              legend.title = element_text(colour = "grey")) 
      
      Gender_sum_plotly <- ggplotly(Gender_sum)
      
      Gender_sum_plotly
    }
  })
  
  output$Age <- renderPlotly({
    
    if(input$Age_ID == "Mean"){
      
      Age_avg <- df %>% dplyr::summarise(`Average Donation amount` = mean(`Donation amount`, na.rm = T), .by = "Age") %>% 
        ggplot(aes(x = Age, y = `Average Donation amount`)) + geom_col(aes(fill = `Average Donation amount`)) +
        theme_minimal() +
        scale_fill_viridis_c() + 
        theme(axis.title = element_text(size = 20),
              axis.title.x = element_text(size = 20, colour = "grey"),
              axis.title.y = element_text(size = 20, colour = "grey"),
              axis.text = element_text(size = 12, colour = "grey"),
              panel.background = element_rect(fill = "#1e434a"),
              panel.grid = element_line(colour = "#292929", linetype = "dashed"),
              plot.background = element_rect(fill = "#1e434a"),
              legend.background = element_rect(fill = "#1e434a"),
              legend.text = element_text(colour = "grey"),
              legend.title = element_text(colour = "grey")) 
      
      Age_avg_plotly <- Age_avg %>% ggplotly(.)
      Age_avg_plotly 
      
    } else {
      Age_sum <- df %>% dplyr::summarise(`Total Donation amount` = sum(`Donation amount`, na.rm = T), .by = "Age") %>% 
        # mutate(Age = fct_reorder(Age, `Total Donation amount`)) %>% 
        ggplot(aes(x = Age, y = `Total Donation amount`)) + geom_col(aes(fill = `Total Donation amount`)) +
        theme_minimal() +
        scale_fill_viridis_c() + 
        theme(axis.title = element_text(size = 20),
              axis.title.x = element_text(size = 20, colour = "grey"),
              axis.title.y = element_text(size = 20, colour = "grey"),
              axis.text = element_text(size = 12, colour = "grey"),
              panel.background = element_rect(fill = "#1e434a"),
              panel.grid = element_line(colour = "#292929", linetype = "dashed"),
              plot.background = element_rect(fill = "#1e434a"),
              legend.background = element_rect(fill = "#1e434a"),
              legend.text = element_text(colour = "grey"),
              legend.title = element_text(colour = "grey")) 
      
      Age_sum_plotly <- Age_sum %>% ggplotly(.)
      Age_sum_plotly
    }
  })
  output$Profession <- renderPlotly({
    
    if(input$Profession_ID == "Mean"){
      
      Profession_avg <- df %>% dplyr::summarise(`Average Donation amount` = mean(`Donation amount`, na.rm = T), .by = "Profession") %>% 
        mutate(Profession = fct_reorder(Profession, `Average Donation amount`)) %>% drop_na() %>% 
        ggplot(aes(x = Profession, y = `Average Donation amount`)) + geom_col(aes(fill = `Average Donation amount`)) +
        theme_minimal() +
        scale_fill_viridis_c() + 
        theme(axis.title = element_text(size = 20),
              axis.title.x = element_text(size = 20, colour = "grey"),
              axis.title.y = element_text(size = 20, colour = "grey"),
              axis.text.y = element_text(size = 12, colour = "grey"),
              axis.text.x = element_text(size = 12, colour = "grey", angle=45),
              panel.background = element_rect(fill = "#1e434a"),
              panel.grid = element_line(colour = "#292929", linetype = "dashed"),
              plot.background = element_rect(fill = "#1e434a"),
              legend.background = element_rect(fill = "#1e434a"),
              legend.text = element_text(colour = "grey"),
              legend.title = element_text(colour = "grey")) 
      
      Profession_avg_plotly <- Profession_avg %>% ggplotly(.)
      Profession_avg_plotly
      
    } else {
      Profession_sum <- df %>% dplyr::summarise(`Total Donation amount` = sum(`Donation amount`, na.rm = T), .by = "Profession") %>% 
        mutate(Profession = fct_reorder(Profession, `Total Donation amount`)) %>% drop_na() %>% 
        ggplot(aes(x = Profession, y = `Total Donation amount`)) + geom_col(aes(fill = `Total Donation amount`)) +
        theme_minimal() +
        scale_fill_viridis_c() + 
        theme(axis.title = element_text(size = 20),
              axis.title.x = element_text(size = 20, colour = "grey"),
              axis.title.y = element_text(size = 20, colour = "grey"),
              axis.text.y = element_text(size = 12, colour = "grey"),
              axis.text.x = element_text(size = 12, colour = "grey", angle=45),
              panel.background = element_rect(fill = "#1e434a"),
              panel.grid = element_line(colour = "#292929", linetype = "dashed"),
              plot.background = element_rect(fill = "#1e434a"),
              legend.background = element_rect(fill = "#1e434a"),
              legend.text = element_text(colour = "grey"),
              legend.title = element_text(colour = "grey")) 
      
      Profession_sum_plotly <- Profession_sum %>% ggplotly(.)
      Profession_sum_plotly
    }
  })
  output$Ever_Married <- renderPlotly({
    
    if(input$Ever_Married_ID == "Mean"){
      
      
      Ever_Married_avg <- df %>% 
        dplyr::summarise(`Average Donation amount` = mean(`Donation amount`, na.rm = T), .by = "Ever_Married") %>% 
        mutate(Ever_Married = fct_reorder(Ever_Married, `Average Donation amount`)) %>% drop_na() %>% 
        ggplot(aes(x = Ever_Married, y = `Average Donation amount`)) + geom_col(aes(fill = `Average Donation amount`)) +
        theme_minimal() +
        scale_fill_viridis_c() + xlab("Was the person ever married?") +
        theme(axis.title = element_text(size = 20),
              axis.title.x = element_text(size = 20, colour = "grey"),
              axis.title.y = element_text(size = 20, colour = "grey"),
              axis.text = element_text(size = 12, colour = "grey"),
              panel.background = element_rect(fill = "#1e434a"),
              panel.grid = element_line(colour = "#292929", linetype = "dashed"),
              plot.background = element_rect(fill = "#1e434a"),
              legend.background = element_rect(fill = "#1e434a"),
              legend.text = element_text(colour = "grey"),
              legend.title = element_text(colour = "grey")) 
      
      Ever_Married_avg_plotly <- Ever_Married_avg %>% ggplotly(.)
      
    } else {
      Ever_Married_sum <- df %>% dplyr::summarise(`Total Donation amount` = sum(`Donation amount`, na.rm = T), .by = "Ever_Married") %>% 
        mutate(Ever_Married = fct_reorder(Ever_Married, `Total Donation amount`)) %>% drop_na() %>% 
        ggplot(aes(x = Ever_Married, y = `Total Donation amount`)) + geom_col(aes(fill = `Total Donation amount`)) +
        theme_minimal() +
        scale_fill_viridis_c() + xlab("Was the person ever married?") +
        theme(axis.title = element_text(size = 20),
              axis.title.x = element_text(size = 20, colour = "grey"),
              axis.title.y = element_text(size = 20, colour = "grey"),
              axis.text = element_text(size = 12, colour = "grey"),
              panel.background = element_rect(fill = "#1e434a"),
              panel.grid = element_line(colour = "#292929", linetype = "dashed"),
              plot.background = element_rect(fill = "#1e434a"),
              legend.background = element_rect(fill = "#1e434a"),
              legend.text = element_text(colour = "grey"),
              legend.title = element_text(colour = "grey")) 
      
      Ever_Married_sum_plotly <- Ever_Married_sum %>% ggplotly(.)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
