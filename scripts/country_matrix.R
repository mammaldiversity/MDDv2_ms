####################################################################
#R-Script country_matrix.R Madeleine Becker
#
#This script generates .txt file with a presence matrix (0=absence, 1=presence, 2=potential presence)
#from on an MDD-formatted 2-column csv input file of species and country distributions (separated by pipes and no spaces).
#It also generates a .txt file with total number of species by country (strict, lenient, and endemics only)
#This script can also be used for spell-checking countries if you stop it after line 51 & "print(countries)"
#Replace file names with full paths before running script
#
#Can also work for biogeographic realms, if formatted properly. N.B. that totals may be less applicable (e.g. strict vs. lenient)
#
#N.B. If you open output files with Excel, the top row will be offset by 1 and you'll have to manually one cell shift right
#
#Updated: 2/5/22 MAB
####################################################################

#install.packages("readtext")
#install.packages("stringr")

library(readtext)
library(stringr)

#define input and output files (& path)

#input file needs to be a 2 column comma-delimited text file of species and country distributions 
#assumes that they have headers & countries should be separated by |
#? after country names also accepted as potential presence
country_csv_path = "C:/Users/madel/Documents/R_scripts_files/MDD/mdd_country_v1.8.csv"
#output file is a tab-delimited text file in the form of a large (0,1,2) matrix
#top row = alphabetized countries are vertical
#leftmost column = organisms in the order the input file gives
country_matrix_path = "C:/Users/madel/Documents/R_scripts_files/MDD/country_matrix_v1.8.txt"
#second output file giving the number of species per country
#column 1: alphabetized country names
#column 2: number of species in country (strict). total number of species, but only for certain presence (1)
#column 3: number of species in country (lenient). total number of species, but for certain and potential presence (1,2)
#column 4: number of endemic species in country. number of species found in that country (1) but either no or only potential presence elsewhere (0,2)
country_totals_tsv = "C:/Users/madel/Documents/R_scripts_files/MDD/country_totals_v1.8.txt"

#read in country_csv
init <- read.csv(country_csv_path, header= TRUE,sep=",",encoding="UTF-8",stringsAsFactors = FALSE)
l <-length(unlist(init[1]))

#makes vector of unique entries of country names from init
#can be used as a way to check consistency, spelling
#also sort vector and remove empty "" starting the vector
countries <- c()
for (index in 1:l){
  entry <- init[index,2]
  indivs <- unlist(str_split(entry,"\\|"))
  for (i in 1:length(indivs))
    if (indivs[i] %in% countries)
      x=1
    else
      countries <- c(countries,indivs[i])
}
countries <- sort(countries)
countries <- countries[2:length(countries)]


#now that the theoretical spellcheck phase is done,
#remove the entries with question marks in order to make
#a vector of the column names for the final matrix
m_countries <- c()
for (i in 1:length(countries)){
  entry <- countries[i]
  if(substring(entry,nchar(entry)) == "?")
    if(substring(entry,1,nchar(entry)-1) %in% m_countries)
      x=1
    else
      m_countries <- c(m_countries,substring(entry,1,nchar(entry)-1))
  else
    m_countries <- c(m_countries,entry)
}
n <- length(m_countries)

#instantiate final matrix and add species as row names
#and country names as column names
m <- matrix(,nrow=l,ncol=n)
species <- (init[1])
names <- list(unlist(species),m_countries)
dimnames(m) <- names

#makes a country list (NOT VECTOR), using | split to make
#a vector for each set of distributions
vector_list <- list()
for (index in 1:l){
  entry <- init[index,2]
  country_vector <- str_split(entry,"\\|")
  #country_vectors[index,2] <- country_vector
  vector_list[index] <- country_vector
}

#fill in the final matrix
#go through each country for each species:
#if the country is within the species's vector --> presence --> 1
#if the country? is within the species's vector --> potential presence --> 2
#if the country is not in the species's vector --> absence --> 0
for (i in 1:l){
  for (j in 1:n){
    if (colnames(m)[j] %in% vector_list[[i]])
      m[i,j]<-1
    else if (paste(colnames(m)[j],"?",sep="") %in% vector_list[[i]])
      m[i,j]<-2
    else
      m[i,j]<-0
  }
}

#write distribution matrix to tsv
write.table(m,file=country_matrix_path,sep ="\t",fileEncoding="UTF-8")


#make endemics matrix
species_totals = rowSums(m=='1', na.rm=TRUE)
endemics_only = names(species_totals[species_totals=='1'])
endemics_m = m[endemics_only,]

#calculate totals
total_strict = colSums(m=='1', na.rm=TRUE)
total_lenient = total_strict + colSums(m=='2', na.rm=TRUE)
total_endemics = colSums(endemics_m=='1', na.rm=TRUE)

#make totals table
totals_table = cbind(total_strict, total_lenient, total_endemics)

#write totals matrix to tsv
write.table(totals_table,file=country_totals_tsv,sep ="\t",fileEncoding="UTF-8")

