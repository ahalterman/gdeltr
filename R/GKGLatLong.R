#' Create a geographic node/edgelist from a GKG dataframe.
#' 
#' This takes a GKG dataframe and creates node and edgelists of the co-mentioned geographic locations. These node and edgelists can be imported into Gephi and viewed with the "Geo Layout" option.
#' Its saves the edge and node lists in the working directory under file names that you specify.
#'
#' @param \code{gkg.df} A subset of the Global Knowledge Graph 
#' @param \code{filename} The filename of the output (exclude file endings)
#'
#' @return edgelist.csv A semicolon-separated csv with an edgelist using the full geographic names.
#' @return nodelist.csv A semicolon-separated csv including label, id, latitude, and longitude.
#'
#' @keywords GDELT, gdeltr
#'
#' @details The files that the function saves can be imported into Gephi. Import the nodelist file first, and make sure that \code{lat} and \code{lng} are set to "Double". Import the edgelist next. After importing both, clear up any duplicates by going to Data Labratory > More Actions > Detect and Merge Duplicates, merging on the Label field.
#' Make sure you have the Geo Layout plugin installed, as well as the "Map of Countries" plugin if you want to do the full visualization inside Gephi.
#'
#'Feature to add: Take the full dataframe, separate by date to allow dynamic graphs.
#'
#' @export
#' 
#' @examples
#' ieds <- gkg[grep("LANDMINE", gkg$THEMES),]
#' GKGLatLong(ieds, file="ied.network")

GKGLatLong <- function(gkg.df, filename){
  if (!"ORGANIZATIONS" %in% names(gkg.df)) stop("No column named 'ORGANIZATIONS'")
  locations.original <- as.character(gkg.df$LOCATIONS)
  locations <- strsplit(locations.original, split=";")
  nMax <- max(sapply(locations, length))
  locations <- cbind(t(sapply(locations, function(i) i[1:nMax])))
  locations <- as.data.frame(locations, stringsAsFactors=FALSE)
  ## Remove all the single-node (non-edge) rows
  locations <- subset(locations, locations[,2] != "NA")
  # Now we need to change each cell into just a FullName
  locations.df <- as.data.frame(locations[1,])
  
  for (col.tmp in 1:ncol(locations)){
    locations.one <- t(locations[col.tmp,])
    locations.one <- strsplit(locations.one, "#")
    nMax <- max(sapply(locations.one, length))
    locations.one <- cbind(t(sapply(locations.one, function(i) i[1:nMax])))
    locations.one <- as.data.frame(locations.one, stringsAsFactors=FALSE)
    locations.one <- as.data.frame(t(locations.one[,2]))
    locations.df <- rbind(locations.df, locations.one)
  }
  locations.df <- subset(locations.df, locations.df[,2] != "NA")
  locations.df <- locations.df[2:49,]
  locations.edgelist <- GKGedgelist(locations.df, max.connections=24)
  locations.edgelist <- as.data.frame(locations.edgelist)
  locations.edgelist$type <- "Undirected"
  names(locations.edgelist) <- c("Source", "Target", "Type")
  
  node.latlong <- strsplit(locations.original, split=";")
  node.latlong <- unlist(node.latlong)
  node.latlong <- strsplit(node.latlong, split="#")
  nMax <- max(sapply(node.latlong, length))
  node.latlong <- cbind(t(sapply(node.latlong, function(i) i[1:nMax])))
  node.latlong <- as.data.frame(node.latlong, stringsAsFactors=FALSE)
  node.latlong <- node.latlong[,c(2,5,6)]
  names(node.latlong) <- c("id", "lat", "lng")
  node.latlong$label <- node.latlong$id
  
  node.filename <- paste0(filename, ".nodes.csv")
  edge.filename <- paste0(filename, ".edges.csv")
  write.table(locations.edgelist, file=edge.filename, sep=";", row.names=FALSE)
  write.table(node.latlong, file=node.filename, sep=";", row.names=FALSE)
}