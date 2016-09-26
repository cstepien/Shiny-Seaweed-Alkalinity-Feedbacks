# add Bicarbonate + CO2 users, versus CO2 only users for each selection
# Add a title overall, a description of the interactive feature
library(shiny)
library(ggplot2)
library(dplyr)
library(modeest)
rawdata <- read.csv("data.csv")
rawdata$del13c <- round(rawdata$del13c, digits = 0)
rawdata$color <- factor(ifelse(rawdata$del13c < -30, "purple", "pink"))
yaxis <- c(70, 140, 170, 200, 215)

ui <- fluidPage(
  tags$h1("Seaweed State of the Union"),
  tags$h2("About this demo"),
  tags$h3("What is it?"),
  tags$p(style = "font-size:13.5pt", "Hello Seaweed True Believers! I'm Courtney Stepien, and this project is an R Shiny interactive graph for a figure from my paper.
         This demo shows how seaweeds and other marine plants like surfgrasses use carbon in the oceans.
         It's a histogram - a count of how many seaweeds have a certain value - in this case, how many seaweed
         and surfgrass species have a certain tissue carbon isotope value, which tells us whether or not they
         can access bicarbonate for photosynthesis (see more below)."),
  tags$h3("What are the controls?"),
  tags$p(style = "font-size:13.5pt", "You can choose which seaweed Phyla to show data for (and the 1 surfgrass Phylum too). Also, 
         you can change the size of the bins. The output next to the histogram will show you the maximum, minimum
         and mode isotope value for your selection."),
  tags$h2("Discover Which Marine Plants use Bicarbonate, and Which are Stuck on the CO2-only Diet"),
  tags$br(),
  fluidRow(
    column(3, checkboxGroupInput(inputId = "taxa", label = "Select Taxa to Display", 
                     c("Green Seaweeds (Chlorophyta)" = "Chlorophyta", "Red Seaweeds (Rhodophyta)" = "Rhodophyta", 
                       "Brown Seaweeds (Ochrophyta)" = "Ochrophyta", "Surfgrasses (Tracheophyta)" = "Tracheophyta"),
                     selected = c("Chlorophyta", "Rhodophyta", 
                                  "Ochrophyta", "Tracheophyta")),
           selectInput(inputId = "binwidth", 
                       label = "Bin width (isotope units)", 
                       choices = c(1, 2, 3, 4, 5), 
                       selected = 1,
                       width = '70px'), 
           verbatimTextOutput(outputId = "max"),
           verbatimTextOutput(outputId = "min"),
           verbatimTextOutput(outputId = "mo")), 
    column(9, align = "left", plotOutput(outputId = "hist"))),
  tags$hr(),
  tags$h2("The Science of Seaweeds"),
  fluidRow(
    column(7, tags$h3("Wait, Why Do We Care?"), 
           tags$p(style = "font-size:13.5pt", "Now, before we get started, you might be wondering – why? Why do we care about this? 
           This is a good question! When a lot of people think about climate change, 
           we hear a lot about CO2 in the atmosphere, global warming, but the flip side of this is 
           that the oceans act as big sponges, big sinks that just suck up all that extra carbon dioxide, 
           and that's actually changing the ocean chemistry – how much carbon is available to seaweeds, 
           what type of carbon is available to them, and these can have some implications for ocean 
           communities and ecosystems down the line."), 
           tags$h3("Some Quick Ocean Chemistry"),
           tags$p(style = "font-size:13.5pt", "In our gardens and parks, plants photosynthesize and use carbon dioxide, super easy, no big deal.
            In the oceans though, carbon dioxide is actually rare - less than 1% of all the dissolved inorganic
            carbon in water! So lots of seaweeds have swtiched over to using not only that rare carbon dioxide,
            but also the super-abundant bicarbonate. It's a little more costly energy-wise to use bicarbonate, 
            so no all seaweeds have adopted this new lifestyle.")),
    column(5, tags$img(height = 405, 
           width = 540,
           src = "seaweed.jpg"), 
           tags$p("Taken by Orissa Moulton"))),
  tags$h3("You Are What You Eat"),
  tags$p(style = "font-size:13.5pt", "Now, in order to tease out which seaweeds are using carbon dioxide only, 
         and which seaweeds can also use bicarbonate, all we have to do is look at
         the seaweed plant tissue - because it turns out, you are what you photosynthesize if you're
         a seaweed! Seaweeds that use ONLY carbon dioxide have very low (very negative) carbon isotope signatures (less than -30 units!),
         while seaweeds that use bicarbonate in addition to carbon dioxide have really high (less negative) carbon
         isotope signatures (greater than -30 units!). This has to do with the original isotope signatures
         of the carbon dioxide vs the bicarbonate - if you eat 'heavier' bicarbonate food as a seaweed, 
         you get a more positive isotope ratio. Seaweeds on the CO2 only diet, a much isotopically 'lighter'
         food, end up with more negative values. It turns out that most seaweeds are not on a diet. "),
  tags$hr(),
  tags$h2("Learn more about the project"),
  tags$p(style = "font-size:13.5pt", "The original paper can be found", tags$a(href = "http://onlinelibrary.wiley.com/doi/10.1111/1365-2745.12451/full", "here"), 
         "Impacts of geography, taxonomy and functional group on inorganic carbon use 
         patterns in marine macrophytes (2015) Journal of Ecology 103 (6), 1372-1383."),
  tags$p(style = "font-size:13.5pt", "I made a podcast with slides describing the project", tags$a(href = "https://www.youtube.com/watch?v=5PULuVG0694", "on YouTube here")),
  tags$p(style = "font-size:13.5pt", "And Hannah Brechka wrote a cool general article about the project", 
         tags$a(href = "https://sciencelife.uchospitals.edu/2015/10/21/seaweed-state-of-the-union/", 
                "over at UChicago Science Life")),
  tags$p(style = "font-size:13.5pt", "You can contact me at cstepien@uchicago.edu and follow my current seaweed project on", 
         tags$a(href = "https://github.com/cstepien/Evolution-of-CCMs", "GitHub"))
)

server <- function(input, output) {
  data <- reactive({filter(rawdata, division %in% input$taxa)})
  bin <- reactive({as.numeric(input$binwidth)})
  lim <- reactive({yaxis[as.numeric(input$binwidth)]})
  output$hist <- renderPlot({
    ggplot(data(), aes(x = del13c, fill = color, color = "black")) + xlim(-45,0) + ylim(0,lim()) +
      geom_histogram(binwidth = bin(), color = "black") +
      #geom_histogram(data = filter(data(), del13c >= -30), binwidth = bin(), fill = "pink", color = "black") +
      scale_fill_manual(drop = FALSE, name = "Legend", values = c("purple" = "purple", "pink" = "pink"), 
                        labels = c("Bicarbonate User", "CO2 Only")) +
      geom_vline(xintercept = -30, linetype = "dotted", size = 1) +
      xlab(expression(paste("\nMean species ", delta^{13}, "C ", "(\u2030)"))) +
      ylab("Species Count") +
      theme(panel.grid.major.y = element_line(color = "gray"), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.text.x = element_text(size=20, color = "black"), 
            axis.text.y = element_text(size=20, color = "black"),
            axis.title.x = element_text(size=20), 
            axis.title.y = element_text(size=20), 
            legend.text = element_text(size = 15), 
            legend.title = element_text(size = 15),
            legend.position = c(0.9,0.7))
    })
  output$max <- renderText({paste("Max species ", "\u03B413C", "(\u2030) =", max(data()$del13c))})
  output$min <- renderText({paste("Min species ", "\u03B413C", "(\u2030) =", min(data()$del13c))})
  output$mo <- renderText({paste("Mode ", "\u03B413C", "(\u2030) =", 
                                 paste(mfv(data()$del13c), collapse = ", "))})
}

shinyApp(ui = ui, server = server)