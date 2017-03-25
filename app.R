
# Libraries needed
library(igraph)
library(lattice) 
library(ggplot2)




# Call functions needed to simulate model
folder_dir<-getwd()
source(paste0(folder_dir,'/def_game.R'))
source(paste0(folder_dir,'/def_objects/def_agents.R'))
source(paste0(folder_dir,'/def_objects/def_network.R'))

# Plot functions
source(paste0(folder_dir,'/plot_functions/plot_graph.R'))
source(paste0(folder_dir,'/plot_functions/plot_num_culture.R'))

server <- function(input, output) {
 
 
 observeEvent(input$simulation, {
  	  N<-input$size	
	  Time_s<-input$time
	  n_attr<-input$attributes
	  n_obj<-input$traits
	  c_network<-input$selectmodel
	 if(input$selectmodel =='Lattice') c_network<-'lattice' else  if(input$selectmodel =='Small World')	 c_network<-'small_world'  else c_network<-"scale_free"   
	  network_simulated<-select_network(N=N, p= input $probability,neig= input$neighbour,dimension= input$dimension, c_network, input$power, input$newnodes
)
      history_track<-culture_game(Time_s,n_attr,n_obj,  graph_def = network_simulated)
    
 observeEvent(input$plot_network, {
  	       plot_period<-input$time_plot
      nodesize<-input$node_size
	 output$plotnetwork <- renderPlot({
	plot_graph_culture(network_simulated , history_track,  plot_period, n_attr,layout_chosen = c_network, nodesize= nodesize)
 }, height = 400, width = 500)
 
 
 
plot_period<-if(plot_period<1) plot_period else plot_period
plot_period<-if(plot_period> Time_s) Time_s else plot_period
 n_attr2<-n_attr+1
 attributes<-matrix(0, nrow=N, ncol=n_attr2)
 attributes[,1]<-1:N

 for(i in 1:N){
 	attributes[i,2:n_attr2]<-history_track[[plot_period]]$actors[[i]]$attributes
              }

 attributes<-data.frame(attributes)
 names(attributes)[1]<-'Actor'
 names(attributes)[2:n_attr2]<-sapply('Attribute ',paste0,1: n_attr)

 output$plotnetwork_period1 <- renderDataTable({
attributes 
 },options = list(pageLength = 10, searching=F))
})

 observeEvent(input$plot_numcultures, {
  output$plotnum_cultures <- renderPlot({ plot_number_cultures(threshold_c= n_attr, history= history_track, n_attr= n_attr, n_obje= n_obj)
 	})
})

	  output$downloadData <- downloadHandler(
   filename = function() {
     paste('data-', Sys.Date(), '.RData', sep='')
   },
   content = function(con) {
    save(history_track, file=con)
                           }
 )

})

}


ui <- fluidPage(htmlTemplate( "simulations.html",


# Plot network

plot_network_period1= dataTableOutput("plotnetwork_period1" ),
plot_network= plotOutput("plotnetwork"  ),

 
# Plot number of cultures
n=actionButton(inputId ='plot_numcultures',label='Plot Number of Cultures'),
plot_num_cultures= plotOutput("plotnum_cultures" ),

downloadlink=downloadLink("downloadData", "Download Simulation")



))

shinyApp(ui=ui,server=server)