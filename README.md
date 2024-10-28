# MDDv2_ms
## Codebase for J Mammalogy ms documenting changes to the Mammal Diversity Database from v1.0 to v2.0

- Live site: https://mammaldiversity.org/
- Versioning: https://doi.org/10.5281/zenodo.5945626

### _Contents_
#### Data

- MDD_versions: CSV formatted versions of the v1.0 to v2.0 MDD taxonomy
- Diff_files: Tracked differences between adjacent versions of the MDD, from v1.31 to v2.0
- Metadata_files: Versions of the field definitions for each versions of the MDD, from v1.2 to v2.0

#### Scripts

- country_matrix_v2.R: script for parsing the 'countryDistribution' field of the MDD v2.0 database into a species-by-country matrix.

#### Outputs

- countryDistribution: Parsing of the 'countryDistribution' field in the MDD v2.0 database into a species-by-country matrix; each country in which the species is extant or reintroduced into its native distribution is listed and separated by pipes (|); 0=absence, 1=presence, 2=potential presence.  The script 'country_matrix_v2.R' is used to create the files 'country_matrix_v2.0.txt' and 'country_totals_v2.0.txt'

#### Figures



