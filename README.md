# MDDv2_ms
## Codebase for J Mammalogy 2022 ms documenting changes to the Mammal Diversity Database from v1.0 to v2.0

Live site: https://mammaldiversity.org/
Versioning: https://doi.org/10.5281/zenodo.5945626

### _Contents_
#### Data

- MDD_versions: CSV formatted versions of the v1.0 to v1.8 MDD taxonomy
- Diff_files: Tracked differences between adjacent versions of the MDD, from v1.31 to v1.8
- Metadata_files: Versions of the field definitions for each versions of the MDD, from v1.2 to v1.8

#### Scripts

- country_matrix.R: script for parsing the 'countryDistribution' field of the MDD v1.8 database into a species-by-country matrix.

#### Outputs

- countryDistribution: Parsing of the 'countryDistribution' field in the MDD v1.8 database into a species-by-country matrix; each country in which the species is extant or reintroduced into its native distribution is listed and separated by pipes (|); 0=absence, 1=presence, 2=potential presence.  The script 'country_matrix.R' is used to create the files 'country_matrix_v1.8.txt' and 'country_totals_v1.8.txt'

#### Figures



