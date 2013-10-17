gdeltr
======

R package with tools for working with GDELT


`gdelter` is my ad-hoc collection of functions for working with GDELT.  
For working with the traditional event stream, it includes:

* `gDate` for converting dates from `yyymmdd` to `yyyy-mm-dd`
* `subsetEventCountry` for returning lat/long fields given a country name and EventRootCode
* `fillSeries` to add missing days for plotting or time series analysis

These tools are very quick hacks for working with the alpha experimental release of GDELT's Global Knowledge Graph, coming soon:
* `getComentions` for pulling co-mentioned organizations, people, or countries from a subsetted GKG file.
* `write.gephi`: a wrapper for `write.table` that puts quotes around all elements in the df and writes with semicolon separators and without row/column names.
* `getCounts` will take a subset of the GKG and return just the info in the `COUNTS` column, nicely formatted.
