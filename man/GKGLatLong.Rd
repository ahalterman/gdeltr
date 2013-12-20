\name{GKGLatLong}
\alias{GKGLatLong}
\title{Create a geographic node/edgelist from a GKG dataframe.}
\usage{
GKGLatLong(gkg.df, filename)
}
\arguments{
  \item{\code{gkg.df}}{A subset of the Global Knowledge
  Graph}

  \item{\code{filename}}{The filename of the output
  (exclude file endings)}
}
\value{
edgelist.csv A semicolon-separated csv with an edgelist
using the full geographic names.

nodelist.csv A semicolon-separated csv including label, id,
latitude, and longitude.
}
\description{
This takes a GKG dataframe and creates node and edgelists
of the co-mentioned geographic locations. These node and
edgelists can be imported into Gephi and viewed with the
"Geo Layout" option. Its saves the edge and node lists in
the working directory under file names that you specify.
}
\details{
The files that the function saves can be imported into
Gephi. Import the nodelist file first, and make sure that
\code{lat} and \code{lng} are set to "Double". Import the
edgelist next. After importing both, clear up any
duplicates by going to Data Labratory > More Actions >
Detect and Merge Duplicates, merging on the Label field.
Make sure you have the Geo Layout plugin installed, as well
as the "Map of Countries" plugin if you want to do the full
visualization inside Gephi.

Feature to add: Take the full dataframe, separate by date
to allow dynamic graphs.
}
\examples{
ieds <- gkg[grep("LANDMINE", gkg$THEMES),]
GKGLatLong(ieds, file="ied.network")
}
\keyword{GDELT,}
\keyword{gdeltr}

