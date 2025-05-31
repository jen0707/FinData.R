# Pfo Corr mu-sigma plane (Shiny)

# if (!require(shiny)) install.packages("shiny") # installation of packages

library(shiny) #loading of packages

ui <- fluidPage(
    titlePanel("상관계수와 포트폴리오 수익률"),
    fluidRow(
        sidebarPanel(
            
            tags$head(tags$style("#plot1{height:80vh !important;}")),
            tags$head(tags$style("#plot2{height:80vh !important;}")),
            tags$head(tags$style("#plot3{height:80vh !important;}")),
            tags$head(tags$style("#plot4{height:80vh !important;}")),
            tags$head(tags$style("#plot5{height:80vh !important;}")),
            br(),
            sliderInput("ra",
                        "주식 A의 기대수익률:",
                        min = 0,
                        max = 0.4,
                        value = 0.1,
                        step  = 0.01,
                        animate = F
            ), 
            br(),
            sliderInput("sa",
                        "주식 A 수익률의 표준편차:",
                        min = 0,
                        max = 0.4,
                        value = 0.1,
                        step  = 0.01,
                        animate = F
            ),
            br(),
            sliderInput("rb",
                        "주식 B의 기대수익률",
                        min = 0,
                        max = 0.5,
                        value = 0.3,
                        step  = 0.01,
                        animate = F
            ),
            br(),
            sliderInput("sb",
                        "주식 B 수익률의 표준편차",
                        min = 0,
                        max = 0.5,
                        value = 0.2,
                        step  = 0.01,
                        animate = F
            ),
            br(),
            sliderInput("rho",
                        "수익률 A, B의 상관계수",
                        min = -1,
                        max = 1,
                        value = 0,
                        step  = 0.1,
                        animate = T
            ),
            br()
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("pfo_corr", plotOutput("plot1")),
                tabPanel("45도선그래프", plotOutput("plot2"))
            )
        )
    )
)

server <- function(input, output) {
  observe({
    
    wt  <- 0.01*(0:100)
    r.p <- wt*input$ra + (1-wt)*input$rb
    
    wv.a <- input$sa^2
    wv.b <- input$sb^2
    wcov <- input$rho*input$sa*input$sb
    var.p<- wt^2*wv.a + (1-wt)^2*wv.b + 2*wt*(1-wt)*wcov
    
    sigma.p <- var.p^0.5 
    
    output$plot1 <- renderPlot({
            plot(
                x = sigma.p,
                y = r.p,
                type = "b",
                pch  = 18,
                col  = 'red',
                xlab = "pfo 수익률의 표준편차",
                ylab = "pfo 기대수익률",
                xlim = c(0, max(input$sa, input$sb)*1.1),
                ylim = c(0, max(r.p)*1.1),
                main = 'pfo 수익과 위험'
            )})
    
    output$plot2 <- renderPlot({
      plot(
        1:10,1:10
            )})
    })
}

shinyApp(ui = ui, server = server)