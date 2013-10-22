gdeltr
======

R package with tools for working with GDELT.


`gdeltr` is my ad-hoc collection of functions for working with GDELT.  It is completely untested on any other machine, has no error catching functions, and has completely excessive dependencies on other packages.  I recommend copying and pasting any code you find useful rather than installing the whole package.

For working with the traditional event stream, it includes:

* `gDate` for converting dates from "yyymmdd" to "yyyy-mm-dd"
* `subsetEventCountry` for returning lat/long fields given a country name and EventRootCode
* `getEventCounts` for returning base, root, or regular event codes per month for a given country.
* `fillSeries` to add missing days to a GDELT data frame for plotting or time series analysis


### Global Knowledge Graph

It also includes some quick hacks for working with the alpha experimental release of GDELT's Global Knowledge Graph, coming soon:
* `GKGcomentions` for pulling co-mentioned organizations, people, or countries from a subsetted GKG file.
* `write.gephi`: a wrapper for `write.table` that puts quotes around all elements in the df and writes with semicolon separators and without row/column names.
* `GKGcounts` will take a subset of the GKG and return just the info in the `COUNTS` column, nicely formatted.
* `nameFixer` will standardize names from the GKG.  Only has about 30 right now.
