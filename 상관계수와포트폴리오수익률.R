library(shiny) # shiny 패키지 설치 필요
ui <- fluidPage(
  titlePanel("상관계수와 포트폴리오 수익률"),
  fluidRow(
    sidebarPanel( br(),
                  sliderInput("rho","수익률 상관계수",
                              min=-1, max=1, value=0,
                              step=0.1, animate=T), br(),
                  sliderInput("ra","기대수익률A",0,0.2,0.1,
                              step=0.02, animate=T), br(),
                  sliderInput("sa","표준편차A", 0,0.2,0.1,
                              step=0.02, animate=T), br()
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("pfo_corr", plotOutput("plot1", width="120%"))
      ))))
server <- function(input, output) {
  observe({
    ra <- input$ra; rb <- 0.2
    sa <- input$sa; sb <- 0.3
    wt <- 0.01*(-50:150)
    r.p <- wt*ra + (1-wt)*rb
    var <- function(rho){
      (wt*sa)^2 + ((1-wt)*sb)^2 + 2*wt*(1-wt)*sa*sb*rho
    }
    sigma.p <- var(input$rho)^0.5
    sigma.2 <- var(1)^0.5
    output$plot1 <- renderPlot({
      plot( x = sigma.p,
            y = r.p,
            type = 'b',
            pch = 18,
            col = 'red',
            xlab = 'pfo 수익률의 표준편차',
            ylab = 'pfo 기대수익률',
            xlim = c(0, max(sa, sb)*1.1),
            ylim = c(0, max(r.p)*1.1),
            main = 'pfo 수익과 위험')
      lines(sigma.2, r.p, col='blue')
      points(c(sa,sb),c(ra,rb), pch=10, cex=5)
    },
    height = 600,
    width = 700)
  })
}
shinyApp(ui = ui, server = server)

