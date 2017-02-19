
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
	  c_network<-input$graph
	 if(input$graph =='Lattice') c_network<-'lattice' else  if(input$graph =='Small World')	 c_network<-'small_world'  else c_network<-"scale_free"   
	  network_simulated<-select_network(N=N, p= input $probability,neig= input$neighbour,dimension= input$dimension, c_network, input$power, input$newnodes
)
      history_track<-culture_game(Time_s,n_attr,n_obj,  graph_def = network_simulated)
    
 observeEvent(input$plot_network, {
  	       plot_period<-input$time_plot
      nodesize<-input$node_size
	 output$plotnetwork <- renderPlot({
	plot_graph_culture(network_simulated , history_track,  plot_period, n_attr,layout_chosen = c_network, nodesize= nodesize)
 })
 
 output$plotnetwork_period1 <- renderPlot({
	plot_graph_culture(network_simulated , history_track,  1, n_attr,layout_chosen = c_network, nodesize= nodesize)
 })
})

 observeEvent(input$plot_numcultures, {
  output$plotnum_cultures <- renderPlot({ plot_number_cultures(threshold_c= n_attr, history= history_track, n_attr= n_attr, n_obje= n_obj)
 	})
})


})

}


ui <- fluidPage(htmlTemplate( "simulations.html",


startsimulations=actionButton(inputId ='simulation',label='Simulation'),

choosegraph=selectInput("graph", '', choices = c('Small World', 'Power Law','Lattice'), selectize=F),

choose_size=tags$input(id='size',type="number", value=10, min=5, max=15),

choose_periods=tags$input(id='time',type="number", value=10, min=5, max=500),

choose_attributes=tags$input(id='attributes',type="number", value=2, min=2, max=16),
choose_traits=tags$input(id='traits',type="number", value=2, min=2, max=16),

# Plot network

plot_network_period1= plotOutput("plotnetwork_period1" ),
plot_network= plotOutput("plotnetwork" ),
choose_period_network=tags$input(id='time_plot',type="number", value=5, min=1, max=500),
choose_node_size=tags$input(id='node_size',type="number", value=10, min=0.1, max=20),

plotsimulations=actionButton(inputId ='plot_network',label='Plot Network'),

# Network parameters
dimension= tags$input(id='dimension',type="number", value=1, min=1, max=2, step=1),
neighbour=tags$input(id='neighbour',type="number", value=1, min=1, max=4, step=1),
probability=tags$input(id='probability',type="number", value=.1, min=0, max=1, step=.05),

power=tags$input(id='power',type="number", value=1.2, min=1, max=3, step=.1),
n_new_nodes=tags$input(id='newnodes',type="number", value=2, min=1, max=4, step=1), 
calculating=textOutput('calculating'),


# Plot number of cultures
plotnumcultures=actionButton(inputId ='plot_numcultures',label='Plot Number of Cultures'),
plot_num_cultures= plotOutput("plotnum_cultures" )


))

shinyApp(ui=ui,server=server)