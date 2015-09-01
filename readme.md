# preprocessr
Tools to clean up messy strings when dealing with biographic data.

I often need to match records between data sets that have no common identifier. The most time-consuming part of this process is usually pre-processing entity identifiers (names in the same case, stripping extraneous symbols from phone numbers, etc) in order to facilitate matching. These are some functions I use in order to help with this process. Many of these functions rely heavily on the wonderful  `stringr` package.

## Installation

```R
# install devtools if you haven't already
# install.packages("devtools")
devtools::install_github("tarakc02/preprocessr")
```