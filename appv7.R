#!/usr/bin/env Rscript

library(shiny)
library(tidyverse)
library(readxl)
library(scales)
library(extrafont)
library(grid)
library(colourpicker)
library(shinythemes)
library(yaml)
library(rsconnect)

#setwd("~/HHS/Projects/Scatterplot Shiny App/Scatterplot_Creator")




# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = shinytheme('journal'),
  # Application title
  fluidRow(
   column(width = 4, titlePanel("Scatterplot Creator")),
   column(width = 4, textOutput('last_update'))),
      fluidRow(
        column(width = 2, offset = 1,
               titlePanel("Basic Inputs")),
        column(width = 2, offset = 2,
               titlePanel("Highlight/Restrict")),
        column(width = 2, offset = 2,
               titlePanel("Format Axes"))
        
      ),
   
    # Sidebar with basic inputs for plot 
      fluidRow(
        column(width = 2,
          #Choose player or team data
          radioButtons(inputId = 'data_type', label = 'Player or Team data?',
                       choices = c('Player', 'Team'), selected = 'Player'),
          
          #Choose which year for data
          selectInput(inputId = 'year', label = 'Year', 
                      choices = c(2018:2019),
                      selected = '2018'), 
          radioButtons(inputId = 'natl_conf', label = 'National/Conference',
                      choices = c('natl', 'conf'),
                      selected = 'natl'),
          conditionalPanel(('input.natl_conf == "conf" & input.year == 2019 & input.data_type == "Team"'),
                           textOutput('warning_message')),
          
          #Choose X and Y axis statistic
          textInput(inputId = 'plot_title', label = 'Plot Title'),
          textInput(inputId = 'filename', label = 'File Name:'),
          downloadButton('download', label = 'Download')
        ),
        column(width = 2,
          
          #change basic aesthethics
          uiOutput('x_stat'),
          uiOutput('y_stat'),
          numericInput('basic_size', label = 'Point size', 
                      min = 0.5, max = 100, value = 3, step = 0.5),
          selectInput('basic_colour', label = 'Select colour',
                      choices = c('Grey' = 'grey60',
                                  'Black' = 'black',
                                  'Green' = 'darkgreen',
                                  'Red' = 'darkred',
                                  'Gold' = 'gold4',
                                  'Blue' = 'darkblue',
                                  'Input hex code'),
                      selected = 'Grey'),
          conditionalPanel("input.basic_colour == 'Input hex code'",
                           colourInput('manual_colour', label ='Hex code',
                                       value = 'white')
                           ),
          sliderInput('basic_alpha', label = 'Transparency of basic points', value = 0.5,
                      min = 0, max = 1),
          actionButton('plot_button1', label = 'Create plot')
          
        
      ), #end of sidebar panel
      
      # Show a plot of the generated distribution

               column(width = 2,
                      uiOutput('include_team_ph'),
                      conditionalPanel('input.data_type == "Player"',
                        numericInput('mp', 'Minimum minutes played', value = 200,
                                   min = 0)),
                      radioButtons('restrict_q', label = 'Restrict result',
                                   choices = c('Yes', 'No'),
                                   selected = 'No'),
                      uiOutput('restrict_stat_ph'),
                      uiOutput('restrict_values_ph')
                      ),#end of column
            column(width = 2,
                   conditionalPanel("input.data_type == 'Player'",
                                    uiOutput('highlight_player_ph')),
                   uiOutput('highlight_team_ph'),
                   textOutput('highlight_message'),
                   numericInput('hl_size', 'Size of highlighted point(s)',
                                value = 5),
                   selectInput('basic_hl_colour', label = 'Colour of highlighted point(s)',
                               choices = c('Grey' = 'grey60',
                                           'Black' = 'black',
                                           'Green' = 'darkgreen',
                                           'Red' = 'darkred',
                                           'Gold' = 'gold4',
                                           'Blue' = 'darkblue',
                                           'Input hex code'),
                               selected = 'darkred'),
                   conditionalPanel("input.basic_hl_colour == 'Input hex code'",
                                    colourInput('manual_hl_colour', label ='Hex code',
                                                value = 'white')),
                   sliderInput('hl_alpha', label = 'Transparency of highlighted points',
                               min = 0, max = 1, value = 0)
                   
            ), #end of column
    
             column(width = 2,
                    uiOutput('x_axis_slider'),
                    uiOutput(outputId = 'x_mark_dist'),
                    uiOutput('y_axis_slider'),
                    uiOutput('y_mark_dist')
               ),
             column(width = 2,
                    selectInput('basic_axis_colour', label = 'Select axis colour',
                                choices = c('Grey' = 'grey60',
                                            'Black' = 'black',
                                            'Green' = 'darkgreen',
                                            'Red' = 'darkred',
                                            'Gold' = 'gold4',
                                            'Blue' = 'darkblue',
                                            'Input hex code'),
                                selected = 'Grey'),
                    conditionalPanel("input.basic_axis_colour == 'Input hex code'",
                                     colourInput('manual_axis_colour', label ='Hex code',
                                                 value = 'white')
                    ),
                    uiOutput('x_title_ph'),
                    checkboxInput('xpct', 'As percentage (X-axis)'),
                    uiOutput('y_title_ph'),
                    checkboxInput('ypct', 'As percentage (Y-axis)')
                    )
               ), #end of main panel
                      
                      
                      
                      
          fluidRow(
            column(width = 8,
           plotOutput(outputId = 'scatterplot1', click = 'plot_click', height = "700px",
                      width = '1000px', brush = 'plot_brush')),
           column(width = 4,
           DT::dataTableOutput('selected_point1'))
          )
   
   
)

# Define server logic required 
server <- function(input, output) {

 plot_button_any <- reactive({ plot_button1 + plot_button2 + plot_button3 }) 
  
  #load all data
  player_all <- read_csv('player_data_all.csv')
  team_all <- read_csv('team_data_all.csv')
    
  #filter relevant data
  plot_data <- reactive({
    if(input$data_type == 'Player') {
    player_all %>%  filter(year == input$year, type == input$natl_conf, mp > input$mp) %>%
        mutate(team_include = if (('All' %in% input$include_team)) {
                 TRUE }
               else {
                   (conf %in% input$include_team)
               }
               ) %>%
        filter(team_include == TRUE)
    } else {
    team_all %>% filter(year == input$year, type == input$natl_conf) %>%
        mutate(team_include = if (('All' %in% input$include_team)) {
                 TRUE }
               else {
                   (conf %in% input$include_team)
              }
        ) %>%
        filter(team_include == TRUE)
    } 
  })
  
  #restrict data based on inputs
  plot_data_restricted <- reactive({
    if(input$restrict_q == 'Yes') { plot_data() %>% 
        filter(eval(as.symbol(input$restrict_stat)) > input$restrict_values[1],
               eval(as.symbol(input$restrict_stat)) < input$restrict_values[2])
    } else {plot_data()}
  })
  
  #highlight data based on inputs
  plot_data_highlighted <- reactive ({
    if(input$data_type == 'Player') {
      plot_data_restricted() %>%
        mutate(point_hl = (player %in% input$highlight_player) |
                 (team %in% input$highlight_team))
    } else {
      plot_data_restricted() %>%
        mutate(point_hl = team %in% input$highlight_team)
    }
  })
  

#Options for user to select - used for selecting stats to plot and restrict by
stat_options <- reactive({
  if(input$data_type == 'Player') {
  player_all %>%
  select(-team, -player, -rollup, -attribute,
         -year, -type, -'<NA>', -conf) 
  } else {
    team_all %>%
      select(-team, -year, -rollup, -conf, -type)
  }
})

##X and Y stats are chosen

output$x_stat <- renderUI ({
  selectInput('x', label = 'Select X-axis statistic', 
              choices = colnames(stat_options()))
})

output$y_stat <- renderUI ({
  
  selectInput('y', label = 'Select Y-axis statistic', 
              choices = colnames(stat_options()))
})

#Max and min restrict points are max and min points for the selected statistics

output$restrict_stat_ph <- renderUI ({
  selectInput('restrict_stat', label = 'Restrict based on:', 
              choices = colnames(stat_options()))
})

max_data_point_restrict <- reactive ({
  req(input$restrict_stat)
  max(plot_data()[,input$restrict_stat], na.rm = TRUE)
})

min_data_point_restrict <- reactive ({
  req(input$restrict_stat)
  min(plot_data()[,input$restrict_stat], na.rm = TRUE)
})

output$restrict_values_ph <- renderUI ({
  req(input$restrict_stat)
  sliderInput('restrict_values', 'Between:',
              value = c(min_data_point_restrict(), max_data_point_restrict()),
              max = max_data_point_restrict(), min  = min_data_point_restrict())
})

##Axis options based on restrictions
##Max and min data points are adjusted based on restrictions applied
##Default minimum is 0 for non-negative stats even when restrictions applied

min_data_point_x <- reactive ({
  min(plot_data_restricted()[,input$x], na.rm = TRUE)
})

max_data_point_x <- reactive ({
  max(plot_data_restricted()[,input$x], na.rm = TRUE)
})

output$x_axis_slider <- renderUI ({
  req(input$restrict_stat)
  sliderInput('x_axis_limits', label = 'X-axis limits',
              max = signif(1.4 * max_data_point_x(), digits = 2),
              min = signif(min(c(1.4*min_data_point_x(), 0)), digits = 2),
              value = c(signif(min(c(1.01 * min_data_point_x(), 0)), digits = 2),
                        signif(1.1 * max_data_point_x(), digits = 2)))
})

min_data_point_y <- reactive ({
  min(plot_data_restricted()[,input$y], na.rm = TRUE)
})

max_data_point_y <- reactive ({
  max(plot_data_restricted()[,input$y], na.rm = TRUE)
})

output$y_axis_slider <- renderUI ({
  req(input$restrict_stat)
  sliderInput('y_axis_limits', label = 'y-axis limits',
              max = signif(1.4 * max_data_point_y(), digits = 2),
              min = signif(min(c(1.4*min_data_point_y(), 0)), digits = 2),
              value = c(signif(min(c(1.01 * min_data_point_y(), 0)), digits = 2),
                        signif(1.1 * max_data_point_y(), digits = 2)))
})

##Axis mark distances, defaults are based on max and min values
##Axis tick marks find tick dist such that there are 5 ticks on the axis, then rounds to the nearest significant figure
##Axis breaks based on mark distances
##Labels are altered if percentage option is selected

tick_dist_default_x <- reactive({
      req(input$x_axis_limits)
       i <- 0.1
       while(((input$x_axis_limits[2] - input$x_axis_limits[1]) %/% i) > 5) {
         i <- i + 0.1
       } 
       signif(i, digits = 1)
})

tick_dist_default_y <- reactive({
  req(input$y_axis_limits)
  i <- 0.1
  while(((input$y_axis_limits[2] - input$y_axis_limits[1]) %/% i) > 5) {
    i <- i + 0.1
  } 
  signif(i, digits = 1)
})


output$x_mark_dist <- renderUI({
  numericInput(inputId = 'tick_dist_x', label = "Distance between ticks on x-axis",
               value = tick_dist_default_x())
})

output$y_mark_dist <- renderUI({
  numericInput('tick_dist_y', "Distance between ticks on y-axis",
               value = tick_dist_default_y())
})


x_breaks <- reactive({
  seq(input$x_axis_limits[1],
      ((input$x_axis_limits[2] - input$x_axis_limits[1]) %/% input$tick_dist_x) * input$tick_dist_x,
      by = input$tick_dist_x)
})

x_labels <- reactive({
  if(input$xpct) {
    sapply(x_breaks() * 100, paste, '%', sep = '')
  } else {
    x_breaks()
  }
})



y_breaks <- reactive({
  seq(input$y_axis_limits[1],
      ((input$y_axis_limits[2] - input$y_axis_limits[1]) %/% input$tick_dist_y) * input$tick_dist_y,
      by = input$tick_dist_y)
})

y_labels <- reactive({
  if(input$ypct) {
    sapply(y_breaks() * 100, paste, '%', sep = '')
  } else {
    y_breaks()
  }
})



##Load potential players/teams based on restrictions

player_names <- reactive({
 plot_data_restricted()$player
})

player_names_ordered <- reactive({
  player_names()[order(player_names())]
})

team_names_ordered <- reactive({
plot_data_restricted()$team[order(plot_data_restricted()$team)]
})

conf_names_ordered <- reactive({
  plot_data_restricted()$conf[order(plot_data_restricted()$conf)]
})

all_team_names_ordered <- team_all$team[order(team_all$team)]

all_conf_names_ordered <- team_all$conf[order(team_all$conf)]

all_player_names_ordered <- player_all$player[order(player_all$player)]


##Points to include/highlight


output$include_team_ph <-  renderUI ({
  selectInput('include_team', label = 'Conferences to include',
            choices =  c('All', all_conf_names_ordered), selected = 'All',
            multiple = FALSE)
})



output$download <- downloadHandler(
  filename = function() { paste(input$filename, '.png', sep='') },
  content = function(file) {
    ggsave(file, plot = scatterplot_actual(), device = "png")
  }
)

output$highlight_player_ph <- renderUI({
  selectInput('highlight_player', label = 'Highlight player(s)',
              choices = c('None', player_names_ordered()), selected = 'None',
              multiple = TRUE)
})

output$highlight_message <- renderText({
  'Note: Highlighted points will reset to "None" when data changes (e.g. when you restrict)'
})

output$highlight_team_ph <- renderUI ({
  selectInput('highlight_team', label = 'Highlight Team(s)',
              choices = c('None',team_names_ordered()),
              selected = 'None', multiple = TRUE)
})



 


##Colour options
 
 
basic_colour <- reactive({
  if(input$basic_colour != 'Input hex code') {
    input$basic_colour } else {
      input$manual_colour
    }
})

hl_colour <- reactive({
  if(input$basic_hl_colour != 'Input hex code') {
    input$basic_hl_colour } else {
      input$manual_hl_colour
    }
})

axis_colour <- reactive({
  if(input$basic_axis_colour != 'Input hex code') {
    input$basic_axis_colour } else {
      input$manual_axis_colour
    }
})

basic_alpha <- reactive({
  input$basic_alpha
})

hl_alpha <- reactive({
  input$hl_alpha
})

##Define plot

scatterplot_actual <- reactive({ 
  input$plot_button1
  input$plot_button2
  input$plot_button3
  isolate ({
  if(('None' %in% input$highlight_player) & ('None' %in% input$highlight_team)) {
  ggplot(data = plot_data_restricted(), 
         mapping = aes_string(x = input$x, y = input$y)
  ) +
    geom_point(size = input$basic_size, alpha = 1 - basic_alpha(),
               color = basic_colour()) +
    scale_alpha_manual(values = c((1 - basic_alpha()), (1 - hl_alpha()))) +
    scale_size_manual(values = c(input$basic_size, input$hl_size)) +
    scale_color_manual(values = c(if(input$basic_colour != 'Input hex code') {
      input$basic_colour } else {
        input$manual_colour
      },   if(input$basic_hl_colour != 'Input hex code') {
        input$basic_hl_colour } else {
          input$manual_hl_colour
        })) +
    
    ##Title and Axis formatting
    labs('title' = input$plot_title, 'x' = input$x_title, 'y' = input$y_title) +
    theme(axis.title.x = element_text(angle = 0, size = 28, family = 'Dylan'),
          axis.title.y = element_text(angle = 0, size = 30, family = 'Markerfield', vjust = 0.5),
          axis.text = element_text(size = 24, family = 'sans serif', color = axis_colour()),
          axis.ticks = element_blank(),
          plot.title = element_text(size = 35, family = 'Markerfield', color = 'grey15', hjust = 0.4, vjust = 1)) +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank()) +
    scale_x_continuous(limits = c(input$x_axis_limits[1], input$x_axis_limits[2]),
                       breaks = x_breaks(), labels = x_labels()) +
    scale_y_continuous(limits = c(input$y_axis_limits[1], input$y_axis_limits[2]),
                       breaks = y_breaks(), labels = y_labels()) +
    guides(size = FALSE, color = FALSE, alpha = FALSE)
  } else {
    ggplot(data = plot_data_highlighted(), 
           mapping = aes_string(x = input$x, y = input$y)
    ) +
      geom_point(mapping = aes(size = factor(point_hl), alpha = factor(point_hl),
                               color = factor(point_hl))) +
      scale_alpha_manual(values = c((1 - basic_alpha()), (1 - hl_alpha()))) +
      scale_size_manual(values = c(input$basic_size, input$hl_size)) +
      scale_color_manual(values = c(basic_colour(), hl_colour())) +
      
      ##Title and Axis formatting
      labs('title' = input$plot_title, 'x' = input$x_title, 'y' = input$y_title) +
      theme(axis.title.x = element_text(angle = 0, size = 28, family = 'Dylan'),
            axis.title.y = element_text(angle = 0, size = 30, family = 'Markerfield', vjust = 0.5),
            axis.text = element_text(size = 24, family = 'sans serif', color = axis_colour()),
            axis.ticks = element_blank(),
            plot.title = element_text(size = 35, family = 'Markerfield', color = 'grey15', hjust = 0.4, vjust = 1)) +
      theme(panel.background = element_blank(),
            panel.grid.major = element_blank()) +
      scale_x_continuous(limits = c(input$x_axis_limits[1], input$x_axis_limits[2]),
                         breaks = x_breaks(), labels = x_labels()) +
      scale_y_continuous(limits = c(input$y_axis_limits[1], input$y_axis_limits[2]),
                         breaks = y_breaks(), labels = y_labels()) +
      guides(size = FALSE, color = FALSE, alpha = FALSE)
  }
  })
})


##Render the plot

masterplot <- renderPlot({
  req(input$plot_button1)
    req(input$y)
    req(input$x_axis_limits)
    req(input$y_axis_limits)
    scatterplot_actual()
})




output$scatterplot1 <- masterplot
output$scatterplot2 <- masterplot
output$scatterplot3 <- masterplot






##Define tables which show points that are clicked on

mastertable <- DT::renderDataTable ({
  req(input$plot_brush)
    if(input$data_type == 'Player') {
      brushedPoints(plot_data_restricted(), brush = input$plot_brush) %>%
      select(player, team, input$x, input$y)
    } else{
      brushedPoints(plot_data_restricted(), brush = input$plot_brush) %>%
      select(team, input$x, input$y)
    }
})

output$selected_point1 <- mastertable
output$selected_point2 <- mastertable
output$selected_point3 <- mastertable

output$warning_message <- renderText({
  "Warning: There is currently no conf data for teams for 2019"
})

output$x_title_ph <- renderUI ({
  textInput('x_title', label = 'X-Axis label',
            value = input$x)
})

output$y_title_ph <- renderUI ({
  textInput('y_title', label = 'Y-Axis label',
            value = input$y)
})

output$last_update <- renderText ({
  c("Updated:", date())
})

#rsconnect::deployApp('C:/Users/Daniel/Documents/HHS/Projects/Scatterplot_Shiny_App/Scatterplot_Creator',
#                     appFiles = c('player_data_all.csv', 'team_data_all.csv', 'appv7.R'), upload = TRUE, launch.browser = FALSE,
#                     appName = 'Scatterplot_Creator_v4', forceUpdate = TRUE,
#                     appPrimaryDoc = 'appv7.R')

}
# Run the application 
shinyApp(ui = ui, server = server)

