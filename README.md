# Open Trade Statistics Shiny Dashboard

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3738793.svg)](https://doi.org/10.5281/zenodo.3738793)

Visit the dashboard website: https://shiny.tradestatistics.io

We created this dashboard as  way to provide a graphical interface to our [API](https://api.tradestatistics.io).

Of course the API is more flexible than this application, but we tried to do our best to provide useful options such as: 

* Downloading the data in different formats (xlsx, csv, tsv, json, dta and sav)
* Downloading the charts in png, jpg, pdf and svg format
* Dynamic URLs for sharing your searches
* BibTeX ready citation that you can also use with Zotero and Mendeley

We hope you like this, and if you don't know much about APIs, you can also try obtaining the same information this dashboard provides with our [R package](https://github.com/ropensci/tradestatistics/).

To run locally, comment line 8 in `global.R`, then forward port 5432 (needs an SSH key)
```
ssh -L 5432:localhost:5432 me@tradestatistics.io
```