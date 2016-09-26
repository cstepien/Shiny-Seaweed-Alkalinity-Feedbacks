library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
raw<-read.csv("data.csv")
seaweed<-subset(raw, seaweed!="Seawater")

dataerr<-subset(seaweed, select=c(taxa, tadiff, tase,delta.hco3.pH.calc.se,delta.co2.pH.calc.se, 
                                  type, division, bicarb.user.text))
percent<-subset(seaweed, select=c(taxa, hco3.percent, co2.percent))
data<-subset(seaweed, select=c(taxa, tadiff, tase, Bicarbonate, CarbonDioxide, 
                               type, division, bicarb.user.text))

test<-melt(data, id=c("taxa", "tadiff", "tase", "type", "division", "bicarb.user.text"))

error<-melt(dataerr, id=c("taxa", "tadiff", "tase", "type", "division", "bicarb.user.text"))
code<-melt(percent, id=c("taxa"))

test$error_value<-error$value
test$percent<-code$value
test$percent<-as.factor(test$percent)
test$ccm <- c(rep("CCM", 48))
test$bicarb.user.text <- paste(test$ccm, test$bicarb.user.text)
test$bicarb.user.text <- factor(test$bicarb.user.text, levels = c("CCM Present", "CCM Absent"))

cols<-c("Brown"="saddlebrown", "Green"="#339900", "Red"="red","Red calcifying"="magenta",
        "Surfgrass"= "black")

ui <- fluidPage(
  tags$h1("Seaweeds Boost Their Own Food Source"),
  tags$h2("About this demo"),
  tags$h3("What is it?"),
  tags$p(style = "font-size:13.5pt", "Hello Seaweed True Believers! I'm Courtney Stepien, and this project is an R Shiny interactive graph for a figure from my paper.
         This demo shows how seaweeds and other marine plants like surfgrasses change seawater chemistry to their advantage.
         By changing the alkalinity of the water around them, seaweeds increase the amount of carbon available to them 
         for photosynthesis (more explanation below)"),
  tags$h3("What are the controls?"),
  tags$p(style = "font-size:13.5pt", "You can choose which seaweed Phyla to show data for (and the 1 surfgrass Phylum too), 
         whether the seaweeds you want to view can use bicarbonate and carbon dioxide, or only carbon dioxide, and 
         the response variable - bicarbonate or carbon dioxide."),
  tags$h2("See How Much Carbon Seaweeds Gain by Changing the Seawater Chemistry Around Them"),
  tags$br(),
  fluidRow(
    column(3, checkboxGroupInput(inputId = "taxa", label = "Select Taxa to Display", 
                     c("Green Seaweeds (Chlorophyta)" = "Chlorophyta", "Red Seaweeds (Rhodophyta)" = "Rhodophyta", 
                       "Brown Seaweeds (Ochrophyta)" = "Ochrophyta", "Surfgrasses (Tracheophyta)" = "Tracheophyta"),
                     selected = c("Chlorophyta", "Rhodophyta", 
                                  "Ochrophyta", "Tracheophyta")),
           selectInput(inputId = "CCM", 
                       label = "Select seaweeds that can use...", 
                       choices = c("Carbon dioxide only", "Carbon dioxide and bicarbonate", "Select everyone"), 
                       selected = "Select everyone")), 
    column(9, plotOutput(outputId = "plot"))
  ),
  tags$hr(),
  tags$h2("The Science of Seaweeds"),
  fluidRow(
    column(7, tags$h3("Wait, Why Do We Care?"), 
           tags$p(style = "font-size:13.5pt", "Now, before we get started, you might be wondering – why? Why do we care about this? 
           This is a good question! When a lot of people think about climate change, 
           we hear a lot about CO2 in the atmosphere, global warming, but the flip side of this is 
           that the oceans act as big sponges, big sinks that just suck up all that extra carbon dioxide, 
           and that's actually changing the ocean chemistry. We've got a decent handle on the chemistry of 
           what's happening, but we know less about how plants and animals are feeding back into 
           the global carbon cycle."), 
           tags$h3("Some Quick Ocean Chemistry"),
           tags$p(style = "font-size:13.5pt", "In our gardens and parks, plants photosynthesize and use carbon dioxide, super easy, no big deal.
            In the oceans though, carbon dioxide is actually rare - less than 1% of all the dissolved inorganic
            carbon in water! So lots of seaweeds have swtiched over to using not only that rare carbon dioxide,
            but also the super-abundant bicarbonate. It's a little more costly energy-wise to use bicarbonate, 
            so no all seaweeds have adopted this new lifestyle. We found that to help locally boost how much
            carbon dioxide and bicarbonate is in the water, seaweeds have actually managed to tweak the
            local seawater chemistry in a way that favors their favorite carbon resources.")),
    column(5, tags$img(height = 405, 
           width = 540,
           src = "garage_science.JPG"), 
           tags$p("Garage science - how we did this experiment"))),
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
  data <- reactive({filter(test, division %in% input$taxa)})
  bin <- reactive({as.numeric(input$binwidth)})
  lim <- reactive({yaxis[as.numeric(input$binwidth)]})
  output$plot <- renderPlot({
    ggplot(data(), aes(x=tadiff, y=value, color=type)) + geom_point() +
      geom_vline(xintercept = 0, linetype = "solid", color = "gray18") +
      geom_hline(yintercept=0, linetype="solid", color = "gray18") +
      geom_errorbar(size = 1, data = data(), aes(x = tadiff, y = value, ymin = value - error_value, ymax = value + error_value), colour = 'black') +
      geom_errorbarh(size = 1, data = data(), aes(x = tadiff, y = value, xmin = tadiff - tase, xmax = tadiff + tase), colour= "black") +
      geom_point(size = 5) +
      #facet_grid(variable~bicarb.user.text, scales="free_y") +
      scale_color_manual(values=cols, 
                         labels=c("Browns", "Greens", "Reds", "Calcifying Reds", 
                                               "Surfgrass"), 
                         breaks = c("Green", "Brown", "Red", "Red calcifying", "Surfgrass"), name="Seaweed") +
      ylab("Carbon gained\n(μmol/kg seawater)") +
      xlab("\nSeaweed-induced change\nin water alkalinity (μmol/kg seawater)") +
      theme(legend.key = element_blank(), 
            strip.text.x = element_text(size=18), strip.text.y = element_text(size=18), 
            axis.title.x = element_text(size=18), axis.title.y = element_text(size=18), 
            axis.text.x = element_text(size=18), axis.text.y = element_text(size=18), 
            legend.text = element_text(size=18), legend.title = element_text(size=18))
    })
}

shinyApp(ui = ui, server = server)