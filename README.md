gdeltr
======

`gdeltr` is my ad-hoc collection of functions for working with [GDELT](http://gdelt.utdallas.edu/).  It is completely untested on any other machine, has no error catching functions, and has completely excessive dependencies on other packages.  _I recommend copying and pasting any code you find useful rather than installing the whole package._

Two basic utilities for working with the traditional event stream:

* `gDate` for converting dates from a "yyymmdd" string to a "yyyy-mm-dd" Date class.
* `fillSeries` to add missing days to a GDELT data frame for plotting or time series analysis

Two more advanced functions for pulling events from a `dplyr`/SQLite setup, as described [here](http://andrewhalterman.com/2013/08/28/gdelt_dplyr_sqlite/):
* `subsetEventCountry` for returning lat/long fields given a country name and EventRootCode.
* `getEventCounts` for returning base, root, or regular event codes per month for a given country.


### Global Knowledge Graph
The Global Knowledge Graph is the newest component of GDELT.  In his [announcement](http://gdeltblog.wordpress.com/2013/10/27/announcing-the-debut-of-the-gdelt-global-knowledge-graph/), Kalev describes it as an attempt "to connect every person, organization, location, count, theme, news source, and event across the planet into a single massive network that captures what’s happening around the world, what its context is and who’s involved, and how the world is feeling about it, every single day."
He also points out that it's much more difficult to work with than the original event stream and recommends using Perl or Python for working with it.  (For one example, it's a nested structure using a combination of tabs, hashtags, and semicolons as separators.)  While Perl/Python may be better, there are lots of people (including me) who are much more comfortable working in R, even if it's inferior.   Here are my quick hacks for working with the alpha experimental release of GDELT's Global Knowledge Graph:
* `GKGcomentions` for pulling co-mentioned organizations, people, or countries from a subsetted GKG file.
* `GKGextractcameo` will return the events from the traditional stream associated with a subset of the GKG namespaces.  It can return either the vector of GLOBALEVENTIDs, or, if you have a dplyr/sqlite setup, the full data frame of events.
* `toner` will, for a given GKG subset, return the tones associated with each person/place/organization associated with it.
* `GKGcounts` will take a subset of the GKG and return just the info in the `COUNTS` column, nicely formatted.  This refers to info in the "Counts" column, not sums of number of events as above.
* `GKGedgelist` will take a ragged data frame with co-mentions and format it into a two-column edge list for export to Gephi or other network analysis tool.
* `write.gephi`: a wrapper for `write.table` that puts quotes around all elements in the df and writes with semicolon separators and without row/column names.
* `nameFixer` will standardize names from the GKG.  Only has about 30 (mostly Syria-related) names right now.  This isn't really worth using yet and I'm sure there's a better approach than this.
