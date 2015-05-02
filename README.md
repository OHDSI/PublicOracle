# PublicOracle [In development]

This package provides an simple `R` interface to the public OHDSI server through the WebAPI and direct SQL queries of the OMOP CDM and public vocabulary.

* To install in `R`:
```R
install_github(c("OHDSI/DatabaseConnector","OHDSI/SqlRender"))
install_github("OHDSI/PublicOracle")
```

* To use:
```R
library(PublicOracle)

# Access the WebAPI
query("vocabulary/concept/19059796")

# Query the OMOP CDM
getPublicOhdsiColumnNames(tableName = "concept")

# Alternative to WebAPI vocabulary query
getConceptInformation(conceptIds = c(19059796,705755))

# Find the common ancestors of multiple concept IDs
findCommonAncestors(conceptIds = c(703470, 705755, 738156))
```

License
=======
PublicOracle is licensed under Apache License 2.0.   

Development
===========
PublicOracle is being developed in R Studio.

###Development status

Alpha

Acknowledgements
================
- This project is supported in part through the National Science Foundation grants IIS 1251151.
