``` r
library(arrow)
#> 
#> Attaching package: 'arrow'
#> The following object is masked from 'package:utils':
#> 
#>     timestamp

data <- readRDS("problem.rds")

arrow::write_dataset(
    data, 
    path = "~/problem", 
    partitioning = c("publication_year", "page"), 
    format = "parquet"
)
#> Error: Invalid: Problem with column 3 (au_orcid): Invalid: Expecting a character vector
```

<sup>Created on 2024-03-21 with [reprex v2.1.0](https://reprex.tidyverse.org)</sup>
