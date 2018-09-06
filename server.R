library(DT)
library(shiny)
library(rCharts)
library(lpSolve)

options(shiny.trace=TRUE, stringsAsFactors = FALSE)

shinyServer(function(input, output, session){

preds <- reactiveValues()
preds$df <-  read.csv("http://s3.amazonaws.com/tcbdfs/preds_nfl.csv")

refresh_data <- reactive({

        preds$df <- read.csv("http://s3.amazonaws.com/tcbdfs/preds_nfl.csv")

	return(preds$df)
})

filterData <- reactive({
 	
	preds$df$player_names <- as.character(preds$df$player_names)
 	preds$df$team <-as.character(preds$df$teams)
        preds$df$positions <- as.character(preds$df$positions)
	preds$df$title <- as.character(preds$df$title)
	preds2 <- preds$df
	preds2 <- preds2[preds2$line >= input$line,]
#	preds2 <- preds2[preds2$value <= input$value,]
	preds2 <- preds2[preds2$Salary >= input$salary[1] & preds2$Salary <= input$salary[2],]
	preds2 <- preds2[preds2$title %in% input$games,]
	preds2$spread <- preds2$spread * -1
	if(input$fav == TRUE){
		preds2 <- preds2[preds2$spread < 0,]
	}
	
	
#	if(input$type == 'NF'){
#		obj <- preds2$nf_pred
#	} else {
#		obj <- preds2$score
#	}

	obj <- preds2$FP
	con <- rbind(t(model.matrix(~ positions + 0,preds2)), t(model.matrix(~ team + 0, preds2)), rep(1,nrow(preds2)), preds2$Salary)
	dir <- c("=","=","<=","<=","<=", rep('<=',length(unique(preds2$team))),"=","<=")
	rhs <- c(1,1,3,2,4,rep(4,length(unique(preds2$team))),9,60000)
	result <- lp("max", obj, con, dir, rhs, all.bin = TRUE)	
	results <- preds2[which(result$solution == 1),]
	results$spread <- as.numeric(results$spread)
	#results$spread <- results$spread * -1
	results <- results[,c("player_names", "positions", "team", "opponent", "Salary", "FP","line", "spread", "pa", "value")]
	colnames(results) <- c("Player", "Position", "Team", "Opp", "Salary", "FP", "Line", "Spread", "PA", "Value")
	return(list(results, result))
   
})

deleteRow <- observeEvent(input$delete.button, {
	chosen_player <- preds$df[c(input$results_rows_selected),]$player_names
        preds$df <- preds$df[-which(preds$df$player_names %in% chosen_player),]
})

refresh <- observeEvent(input$refresh, {
	refresh_data()
})

output$results <- renderDataTable({
	datatable(filterData()[[1]], options=list(autowidth=TRUE, selection="single", columnDefs = list(list(width='200px', targets="_all"))), rownames=FALSE, escape = FALSE)
})

output$update <- renderPrint({
	the_time <- Sys.time() - (3600 * 4)
	if(Sys.time() < format(Sys.time(), '%Y-%m-%d %H:30:00')){
		last_update <- paste0("<b>Last Updated:</b> ", as.Date(Sys.time()), " ", format(the_time, "%H:00"), " EST")
	} else{
		last_update <- paste0("<b>Last Updated:</b> ", as.Date(Sys.time()), " ", format(the_time, "%H:30"), " EST")
	}
	cat(as.character(HTML(last_update)))
})

output$allplayers <- renderDataTable({
	preds3 <- preds$df
	preds3$spread <- preds3$spread * -1
	datatable(preds3, options=list(autowidth=TRUE))
})

output$total <- renderPrint({
  r <- filterData()[[2]]
  
  info <-  paste0("Total:", sum(filterData()[[1]]$nf_pred))
  cat(as.character(HTML(info)))
})

output$about <- renderPrint({
	info <- "This optimizer generates optimal lineups based on NumberFire projections:<p>
		<ul><li><b>numberFire</b>:  <a href = 'https://www.numberfire.com/nfl/fantasy/fantasy-football-projections'>NumberFire.com</a> projections</li>
		<p>
		You can filter by games, salary and value. You can exclude certain players. You can also select 'Favorites' only,
		which will only allow players on teams that are favored to win according to Vegas. Data is updated every 30 minutes.
		<p>
		Legend
		<ul><li><b>NF</b> - NumberFire Projections</li>
			<li><b>TCB</b> - Proprietary TCB Optimization</li>
			<li><b>pa</b> - average Fanduel points against that position</li>
		</ul>
		Game schedule data powered by  <a href = 'https://www.stattleship.com' target='_blank'>Stattleship</a>.
		</p>
		To take advantage of this tool, sign up for <a href = 'https://www.fanduel.com/?invitedby=tcash21&cnl=da' target='_blank'>FanDuel</a> to start playing today."
	cat(as.character(HTML(info)))
})

})





