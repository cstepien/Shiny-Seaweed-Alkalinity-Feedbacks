library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
seaweed<-read.csv("data.csv")
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
 
ymaxlim <- list("CarbonDioxide" = 5.0, "Bicarbonate" = 400)
yminlim <- list("CarbonDioxide" = -4.0, "Bicarbonate" = -200)


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
  tags$p(style = "font-size:13.5pt","Keep an eye on the scale bar for the y axis - it's two orders of magnitude different for
          carbon dioxide versus bicarbonate!"),
  tags$br(),
  fluidRow(
    column(3, selectInput(inputId = "taxa", label = "Select Taxa to Display", choices =
                     c("Green Seaweeds (Chlorophyta)" = "Chlorophyta", "Red Seaweeds (Rhodophyta)" = "Rhodophyta",
                       "Brown Seaweeds (Ochrophyta)" = "Ochrophyta", "Surfgrasses (Tracheophyta)" = "Tracheophyta"),
                     selected = "Chlorophyta",
                     width = '190px'),
           radioButtons(inputId = "carbon", "Which carbon compound to measure response?", 
                        choices = c("Carbon dioxide" = "CarbonDioxide", "Bicarbonate" = "Bicarbonate"), 
                        width = '190px')),
    column(9, plotOutput(outputId = "plot"))
  ),
  tags$hr(),
  tags$h2("The Science of Seaweeds"),
  fluidRow(
    column(7, tags$h3("Charismatic Seaweeds - Or, Why Care?"), 
           tags$p(style = "font-size:13.5pt", "When a lot of people talk about climate change, 
           we hear a lot about carbon dioxide in the atmosphere, aka global warming, but the flip side of this is 
           that the oceans act as big sponges that suck up all that extra carbon dioxide, 
           and that's actually changing the ocean chemistry. We've got a decent handle on the chemistry of 
           what's happening, but we know less about how plants and animals are feeding back into 
           the global carbon cycle."), 
           tags$h3("Some Quick Ocean Chemistry"),
           tags$p(style = "font-size:13.5pt", "In our gardens and parks, plants photosynthesize and use carbon dioxide, super easy, no big deal.
            In the oceans though, carbon dioxide is actually rare - less than 1% of all the dissolved inorganic
            carbon in water! So lots of seaweeds have swtiched over to using not only that rare carbon dioxide,
            but also the super-abundant bicarbonate. We found that to help locally boost how much
            carbon dioxide and bicarbonate is in the water, seaweeds have actually managed to tweak the
            local seawater chemistry in a way that favors their favorite carbon resources.")),
    column(5, tags$img(height = 405, 
           width = 540,
           src = "garage_science.JPG"), 
           tags$p("Garage science - how we did this experiment"))),
  tags$hr(),
  tags$h2("Learn more about the project"),
  tags$p(style = "font-size:13.5pt", "See the code for this Shiny app", 
         tags$a(href = "https://github.com/cstepien/Shiny-Seaweed-Alkalinity-Feedbacks", "on GitHub.")),
  tags$p(style = "font-size:13.5pt", "The full text of the original paper can be found", tags$a(href = "http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0159062", "here"), 
         "CC Stepien, CA Pfister, JT Wootton. Functional Traits for Carbon Access in Macrophytes (2016) PloS one 11 (7), e0159062."),
  tags$p(style = "font-size:13.5pt", "And Kevin Jiang wrote a cool general article about the project and my
         bootleg garage science", 
         tags$a(href = "https://issuu.com/medicineonthemidway/docs/medicineonthemidway-fall2015", 
                "over on pages 24-25 of UChicago's Medicine on the Midway")),
  tags$p(style = "font-size:13.5pt", "You can contact me at", strong("cstepien@uchicago.edu"), "and follow my current seaweed project on", 
         tags$a(href = "https://github.com/cstepien/Evolution-of-CCMs", "GitHub"))
  )

# How to incorporate sizing column based on whether a box is checked or not..hmm (conditional panel?)
# Plot empty null graph
# Add CCM users or not as second category in determining reactive data value (just filter by this input too)

server <- function(input, output) {
  data <- reactive({filter(test, division %in% input$taxa, variable == input$carbon)})
  maxlim <- reactive({as.numeric(ymaxlim[input$carbon])})
  minlim <- reactive({as.numeric(yminlim[input$carbon])})
  output$plot <- renderPlot({
      ggplot(data(), aes(x=tadiff, y=value, color=type)) + geom_point() +
      ylim(minlim(), maxlim()) + xlim(-1400, 400) + #coord_fixed(150) +
      geom_vline(xintercept = 0, linetype = "solid", color = "gray18") +
      geom_hline(yintercept=0, linetype="solid", color = "gray18") +
      geom_errorbar(size = 1, data = data(), aes(x = tadiff, y = value, ymin = value - error_value, ymax = value + error_value), colour = 'black') +
      geom_errorbarh(size = 1, data = data(), aes(x = tadiff, y = value, xmin = tadiff - tase, xmax = tadiff + tase), colour= "black") +
      geom_point(size = 5) +
      #facet_grid(variable~bicarb.user.text, scales="free_y") +
      scale_color_manual(drop = FALSE, values=cols, 
                         labels=c("Green", "Brown", "Red", "Calcifying Red", 
                                               "Surfgrass"), 
                         breaks = c("Green", "Brown", "Red", "Red calcifying", "Surfgrass"), name="Seaweed") +
      ylab("Carbon gained\n(μmol/kg seawater)") +
      xlab("\nSeaweed-induced change\nin water alkalinity (μmol/kg seawater)") +
      theme(legend.key = element_blank(), 
            strip.text.x = element_text(size=18), strip.text.y = element_text(size=18), 
            axis.title.x = element_text(size=18), axis.title.y = element_text(size=18), 
            axis.text.x = element_text(size=18), axis.text.y = element_text(size=18), 
            legend.text = element_text(size=18), legend.title = element_text(size=18))
      #scale_size_manual(values = c(3.5, 5.5, 8, 11), breaks = c("<110%", "111-300%", "301-500%", ">500%"))
    })
}

shinyApp(ui = ui, server = server)