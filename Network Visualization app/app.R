library("igraph")
library(ggplot2)
library(shiny)
library(visNetwork)
library(rsconnect)

fnQuiltView<-function(){
  nodes <- read.csv("NodesG.csv", header=T, as.is=T)
  links <- read.csv("EdgesG.csv",, header=T, as.is=T)
  
  net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
  net<- simplify(net)
  
  
  
  vis.nodes<-data.frame(id=nodes$Module,label=nodes$Module,title=nodes$Module,Module=nodes$Module)
  vis.links<-data.frame(from=links$fromModule,to=links$toModule,width=0.2,arrows=list(to=list(enabled=TRUE,scalefactor=0.2)))
  
  vis.nodes$shape  <- c("dot")[nodes$TYPE]
  vis.nodes$shadow <- TRUE
  vis.nodes$title  <- nodes$Module # Text on click
  vis.nodes$label  <- vis.nodes$id
  deg <- degree(net, mode="all")
  vis.nodes$size   <- log(deg)*15
  vis.nodes$size[vis.nodes$size<12]<-12
  vis.nodes$size[vis.nodes$size>50]<-50
  vis.nodes$borderWidth <- 2
  vis.nodes$color.background <- nodes$Color
  vis.nodes$color.border <- "black"
  vis.nodes$color.highlight.background <- "orange"
  vis.nodes$color.highlight.border <- "darkred"
  
  visNetwork(vis.nodes,vis.links,height = "1000",
             width = "1000")%>%
    visPhysics(stabilization=TRUE,enabled = TRUE,solver ="forceAtlas2Based",adaptiveTimestep = TRUE,timestep = 0.25)%>%
    
    visEdges(smooth=TRUE)%>%
    visOptions(highlightNearest = TRUE, nodesIdSelection =TRUE,collapse=TRUE)%>%
    #visIgraphLayout(layout="layout_in_circle")%>%
    visLayout(randomSeed = 1234)
}

fnSubModelView<-function(){
  nodes <- read.csv("NodesG2.csv", header=T, as.is=T)
  links <- read.csv("EdgesG2.csv", header=T, as.is=T)
  
  net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
  net
  
  vis.nodes<-data.frame(id=nodes$id,label=nodes$Module,title=nodes$Module,Module=nodes$Module)
  vis.links<-data.frame(from=links$fromid,to=links$toid,width=0.2,arrows=list(to=list(enabled=TRUE,scalefactor=0.2)))
  
  vis.nodes$shape  <- c("dot")
  vis.nodes$shadow <- TRUE
  vis.nodes$title  <- nodes$Module # Text on click
  vis.nodes$label  <- vis.nodes$Module
  deg <- degree(net, mode="all")
  vis.nodes$size   <- log(deg)*15
  vis.nodes$size[vis.nodes$size<5]<-5
  vis.nodes$size[vis.nodes$size>30]<-30
  vis.nodes$borderWidth <- 2
  vis.nodes$color.background <- nodes$Color
  vis.nodes$color.border <- "black"
  vis.nodes$color.highlight.background <- "orange"
  vis.nodes$color.highlight.border <- "darkred"
  
  visNetwork(vis.nodes,vis.links,height = "1000",
             width = "1000")%>%
    visPhysics(stabilization=TRUE,enabled = TRUE,solver ="forceAtlas2Based",adaptiveTimestep = TRUE,timestep = 0.25)%>%
    
    visEdges(smooth=FALSE)%>%
    visOptions(highlightNearest = TRUE, nodesIdSelection =TRUE,collapse=TRUE)%>%
    #visIgraphLayout(layout="layout_on_sphere")%>%
    visLayout(randomSeed = 1234)
  
  
}




ui<- fluidPage(
  titlePanel("Understanding the International Futures system through network analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("View","Select View",choices=c("Quilt View","Sub-Model View","Individual Model View")),selectInput("Model", "Please Select Model", choices=c("ALL","Agriculture", "Demography", "Economy","Education","Energy","Environment","Infrastructure","Interstate","Other","Sociopolitical")),
      selectInput("Items","Select items for display",choices=c("Variables only","Variables and parameters"))),
    mainPanel(p("This visualization helps us understand the composition of the International Futures Model. The dots represent variables while the boxes represent parameters. The sizes of the variables and the parameters are determined on the basis of the number of connections these variables and parameters share with the rest of the model."),p("The user can make use of multiple options. The Select View option enables the user to select either a Quilt View which provides an overview of the different systems in IFs, The sub-model view that enables a user to view interactions between sub-models and the Individual Model View which enables the user to look at components of individual models. Please click on the Store positions option at the bottom to stabilize the visualization.")
              ,visNetworkOutput("network",height = 1000,width=1000), 
              actionButton("store_position", "Store positions !"),
              downloadLink('downloadNetwork', 'Download network'))))

server <- function(input,output){
  
  output$network <- renderVisNetwork({
    if(input$View=="Quilt View"){r<-fnQuiltView()
    r}
    else if(input$View=="Sub-Model View"){q<- fnSubModelView()
    q}
    else{
      nodes <- read.csv("Nodes.csv", header=T, as.is=T)
      links <- read.csv("Edges.csv", header=T, as.is=T)
      
      if(input$Items=="Variables only"){
        links2<- subset(links, (links$Type1==1))
        links3<- subset(links2, (links2$Type2==1))
        #links<- rbind(links2,links3)
        links4<- (unique(links3[,1:9]))
        #nodes<- nodes[(nodes$NAME %in% (links$from)]| nodes[(nodes$NAME %in% (links$to)]
        nodes2<- subset(nodes,(nodes$NAME %in% links4$from))
        nodes3<- subset(nodes,(nodes$NAME %in% links4$to))
        nodes4<- rbind(nodes2,nodes3)
        nodes5<- (unique(nodes4[,1:9]))
      } else {#links2<- subset(links, (links$fromModule==input$Model))
        #links3<- subset(links, (links$toModule==input$Model))
        #links<- rbind(links2,links3)
        links4<- (unique(links[,1:9]))
        #nodes<- nodes[(nodes$NAME %in% (links$from)]| nodes[(nodes$NAME %in% (links$to)]
        #nodes2<- subset(nodes,(nodes$NAME %in% links4$from))
        #nodes3<- subset(nodes,(nodes$NAME %in% links4$to))
        #nodes<- rbind(nodes2,nodes3)
        nodes5<- (unique(nodes[,1:9]))}
      
      
      
      if (input$Model=='ALL'){
        #links2<- subset(links, (links$fromModule==input$Model))
        #links3<- subset(links, (links$toModule==input$Model))
        #links<- rbind(links2,links3)
        links5<- (unique(links4[,1:9]))
        #nodes<- nodes[(nodes$NAME %in% (links$from)]| nodes[(nodes$NAME %in% (links$to)]
        #nodes2<- subset(nodes,(nodes$NAME %in% links4$from))
        #nodes3<- subset(nodes,(nodes$NAME %in% links4$to))
        #nodes<- rbind(nodes2,nodes3)
        nodes6<- (unique(nodes5[,1:9]))
      } else {links6<- subset(links4, (links4$fromModule==input$Model))
      links7<- subset(links4, (links4$toModule==input$Model))
      links8<- rbind(links6,links7)
      links5<- (unique(links8[,1:9]))
      #nodes<- nodes[(nodes$NAME %in% (links$from)]| nodes[(nodes$NAME %in% (links$to)]
      nodes7<- subset(nodes5,(nodes5$NAME %in% links5$from))
      nodes8<- subset(nodes5,(nodes5$NAME %in% links5$to))
      nodes9<- rbind(nodes7,nodes8)
      nodes6<- (unique(nodes9[,1:9]))#HereNow
      }
      net <- graph_from_data_frame(d=links5, vertices=nodes6, directed=T) 
      net<- simplify(net)
      
      vis.nodes<-data.frame(id=nodes6$NAME,label=nodes6$NAME,title=nodes6$NAME,Module=nodes6$Module)
      vis.links<-data.frame(from=links5$from,to=links5$to,width=0.1,arrows=list(to=list(enabled=TRUE,scalefactor=0.5)))
      
      vis.nodes$shape  <- c("dot","square")[nodes6$TYPE]
      vis.nodes$shadow <- TRUE
      vis.nodes$title  <- nodes6$ID2 # Text on click
      vis.nodes$label  <- vis.nodes$id
      deg <- degree(net, mode="all")
      vis.nodes$size   <- log(deg)*15
      #vis.nodes$size   <- 12
      vis.nodes$size[vis.nodes$size<12]<-12
      vis.nodes$size[vis.nodes$size>25]<-25
      vis.nodes$borderWidth <- 1
      vis.nodes$color.background <- nodes6$Color
      vis.nodes$color.border <- "black"
      vis.nodes$color.highlight.background <- "orange"
      vis.nodes$color.highlight.border <- "darkred"
      
      visNetwork(vis.nodes,vis.links,height = "1000",
                          width = "1000")%>%
        visPhysics(stabilization=TRUE,enabled = TRUE,solver ="forceAtlas2Based",adaptiveTimestep = TRUE,timestep = 0.25)%>%
        #visPhysics(enabled = FALSE)%>%
        visEdges(smooth=FALSE)%>%
        visOptions(highlightNearest = TRUE, nodesIdSelection =TRUE,collapse=TRUE)%>%
        visLayout(randomSeed = 1234)
      
      
      }  
  })
  
  observeEvent(input$store_position, {
    visNetworkProxy("network") %>% visGetPositions()%>%visPhysics(stabilization=TRUE,enabled = TRUE,solver ="forceAtlas2Based",adaptiveTimestep = TRUE,timestep = 0.0005)%>%
      visEdges(smooth=TRUE)%>%
      visOptions(highlightNearest = TRUE, nodesIdSelection =TRUE,collapse=TRUE)%>%
      visLayout(randomSeed = 1234) %>%visGetPositions()
  })
  
  nodes_positions <- reactive({
    positions <- input$network_positions
    if(!is.null(positions)){
      nodes_positions <- do.call("rbind", lapply(positions, function(x){ data.frame(x = x$x, y = x$y)}))
      nodes_positions$id <- names(positions)
      nodes_positions
    } else {
      NULL
    }
  })
  
  output$downloadNetwork <- downloadHandler(
    filename = function() {
      paste('network-', Sys.Date(), '.html', sep='')
    },
    content = function(con) {
      nodes_positions <- nodes_positions()
      if(!is.null(nodes_positions)){
        nodes_save <- merge(nodes4, nodes_positions, by = "id", all = T)
      } else  {
        nodes_save <- nodes4
      }
      
      visNetwork(nodes = nodes_save, edges = links4, height = "1000") %>%
        visOptions(highlightNearest = TRUE) %>% visExport() %>%
        visPhysics(enabled = FALSE) %>% visEdges(smooth = FALSE) %>% visSave(con)
    }
  )
}   


shinyApp(ui = ui, server = server)
