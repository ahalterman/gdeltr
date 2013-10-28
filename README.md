gdeltr
======

R package with tools for working with GDELT.


`gdeltr` is my ad-hoc collection of functions for working with GDELT.  It is completely untested on any other machine, has no error catching functions, and has completely excessive dependencies on other packages.  I recommend copying and pasting any code you find useful rather than installing the whole package.

Two basic utilities for working with the traditional event stream:

* `gDate` for converting dates from "yyymmdd" to "yyyy-mm-dd"
* `fillSeries` to add missing days to a GDELT data frame for plotting or time series analysis

Two more advanced functions for pulling events from a `dplyr`/SQLite setup, as described [here](http://andrewhalterman.com/2013/08/28/gdelt_dplyr_sqlite/):
* `subsetEventCountry` for returning lat/long fields given a country name and EventRootCode.
* `getEventCounts` for returning base, root, or regular event codes per month for a given country.


### Global Knowledge Graph
Kalev (rightly) recommends using 

It also includes some quick hacks for working with the alpha experimental release of GDELT's Global Knowledge Graph, coming soon:
* `GKGcomentions` for pulling co-mentioned organizations, people, or countries from a subsetted GKG file.
* `GKGextractcameo` will return the events from the traditional stream associated with a subset of the GKG namespaces.  It can return either the vector of GLOBALEVENTIDs, or, if you have a dplyr/sqlite setup, the full data frame of events.
* `toner` will, for a given GKG subset, return the tones associated with each person/place/organization associated with it.
* `GKGcounts` will take a subset of the GKG and return just the info in the `COUNTS` column, nicely formatted.  This refers to info in the "Counts" column, not sums of number of events as above.
* `write.gephi`: a wrapper for `write.table` that puts quotes around all elements in the df and writes with semicolon separators and without row/column names.
* `nameFixer` will standardize names from the GKG.  Only has about 30 (mostly Syria-related) names right now.  This isn't really worth using yet and I'm sure there's a better approach than this.
