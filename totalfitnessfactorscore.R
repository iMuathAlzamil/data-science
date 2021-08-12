fitness_score <- function(input) {
	weight <- input$Wt
	forwFlex <- input$FF
	return(0.161940 * (forwFlex - weight) + 90.251095) 
}
