# MDDv2_ms
## Codebase for J Mammalogy ms documenting changes to the Mammal Diversity Database from v1.0 to v2.0

- Live site: https://mammaldiversity.org/
- Versioning: https://doi.org/10.5281/zenodo.5945626

### _Contents_
#### inputs

- MDD_v2.0: full taxonomy files for the v2.0 release of the MDD, including the Species_Syn_v2.0.csv file
- MDD_versions: CSV formatted versions for all releases between v1.0 and v2.0 MDD taxonomy
- Diff_files: Tracked differences between adjacent versions of the MDD, from v1.0 to v2.0
- Metadata_files: Versions of the field definitions for each versions of the MDD, from v1.2 to v2.0

#### scripts

- mdd2_manuscript_code.R: all code for performing analyses and making figures in this paper.
- country_matrix_v2.R: script for parsing the 'countryDistribution' field of the MDD v2.0 database into a species-by-country matrix.

#### outputs

- graphs: non-map plotting of data for the MDD v2.0 manuscript.
- map_figures: map-based plotting of data for the MDD v2.0 manuscript.
- non_scripted_figures: Fig. 7, which was made in Whimsical online software.
- supplementary_files: additional files of relevance for analyses, including intermediary files.
	- Includes a parsing of the 'countryDistribution' field in the MDD v2.0 database into a species-by-country matrix; each country in which the species is extant or reintroduced into its native distribution is listed and separated by pipes (|); 0=absence, 1=presence, 2=potential presence.  The script 'country_matrix_v2.R' is used to create the files 'v2_country_matrix.txt' and 'v2_country_totals.txt'

- tables: HTML output of tables from the paper.

#### mdd2_supplemental_data

- SD1 to SD11 from the _Journal of Mammalogy_ paper

	- SD1: MDD2 species list, plus associated metadata, in a zipped folder.
	- SD2: MDD2 synonym list, plus associated metadata, in a zipped folder.
	- SD3: All MDD version species lists, plus associated metadata, in a zipped folder.
	- SD4: All MDD ‘diff’ files, plus associated metadata, in a zipped folder.
	- SD5: ‘Diff’ file specifically between MDD1 and MDD2, plus associated metadata, in a zipped folder.
	- SD6: Summary of nomenclatural data and species totals by taxonomic group, plus associated metadata, in a zipped folder.
	- SD7: Summary of nomenclatural data and species totals by geographic region, plus associated metadata, in a zipped folder.
	- SD8: Summary of nomenclatural data totals by nomenclatural and validity status, plus associated metadata, in a zipped folder.
	- SD9: Total available and valid names described per year since 1758 and summary of 10-year running means used to create Figure 1, plus associated metadata, in a zipped folder.
	- SD10: Occurrence matrix for species country and offshore territory distribution data in a zipped folder.
	- SD11: Per country usage of the MDD website at mammaldiversity.org in a zipped folder.

