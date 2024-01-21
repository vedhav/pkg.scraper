# pkg.scraper

Understand the internal function dependencies of large R packages. `pkg.scraper` provides functions to scrape the internal function dependencies of R packages. The package is currently under development and is not yet ready for use.

## Installation

```r
remotes::install_github("vedhav/pkg.scraper")
```

## Usage

The `build_vis_network` helps to build a network of internal function dependencies. The function takes a package name as input and returns a `visNetwork` object.

```r
pkg.scraper::build_vis_network("teal")
```