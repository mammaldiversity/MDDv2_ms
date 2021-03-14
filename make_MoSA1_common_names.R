####################################################################
#make_MoSA1_common_names.R Madeleine Becker
#
#This script generates .txt file with a two-column table of scientific and common names
#from a formatted text-scrape of Mammals of South America Volume 1, (also has been used on MoSA2)
#Common names are separated by '|'
#N.B. If you open file with something like Notepad, quotation marks will appear around each cell's contents
#
#Updated: 2/26/21 MAB
####################################################################

library(readtext)
library(stringr)

#reads in file and delimits based on entry into a list, then converts to a character vector
in_file <- readtext("C:/Users/madel/Documents/R_scripts_files/MDD/MOSA1synonyms.txt",dvsep="------------------",encoding="UTF-8")
string_file <- in_file[2]
entries <- unlist(str_split(string_file,"------------------\n"))

#takes char_vector and returns species name char_vector: [1] Genus_species [2] Genus_species
get_species <-function(x){
  space_species <- word(x,1,2)
  MDD_species <- gsub(" ","_",space_species)
  return(MDD_species)}

#takes char_vector and returns char_vector for all common names: [1] Common Name [2] Common Name|Common Name
get_common_name <-function(x){
  all_names <- word(x,2,2,"\n")
  replaced_comma <- gsub(", ","|",all_names)
  replaced_semicolon <- gsub("; ","|",replaced_comma)
  replaced_or <- gsub(" or ","|",replaced_semicolon)
  deleted_and <- gsub(" and ","",replaced_or)
  return(deleted_and)}

MoSA1_Sci_Name<-sapply(entries,get_species,USE.NAMES=FALSE)
Common_Names<-sapply(entries,get_common_name,USE.NAMES=FALSE)

#Creates table of just scientific and common names, write to tab-delimited text file
#LIMITATIONS:
#1) Everything is printed with simple quotes around it b/c they're formatted as strings.
#Fix with Ctrl+H or just open in Excel
#2) Cannot get rid of UTF-8 quotes for some reason inside scripts
#Fix with Ctrl+H or just open in Excel (with UTF-8)
#3) Some didn't work but this was due to incorrect formatting in the original spacing (unexpected newlines) 
#Also N.B. generates a first row N/A N/A
common_table <- cbind(MoSA1_Sci_Name,Common_Names)
write.table(common_table,file="C:/Users/madel/Documents/R_scripts_files/MDD/MoSA1_common_names_TEST.txt",sep="\t",row.names=FALSE, col.names=TRUE,fileEncoding="UTF-8")
