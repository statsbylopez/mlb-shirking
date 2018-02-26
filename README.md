# mlb-shirking

This package contains code (scraping and analysis) for a paper on shirking among MLB umpires written by Michael Lopez and Brian Mills. A pre-print of the paper can be found at [SSRN](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3129084)

See the attached code to replicate our analysis, which can be done in two steps:

1) `scraping.R` will download MLB pitch data, and use some data wrangling steps to filter to the desired sample of pitches. 
2) `analysis.R` uses generalized additive models to assess shirking behavior. 
