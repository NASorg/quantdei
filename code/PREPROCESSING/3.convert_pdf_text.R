here <- rprojroot::find_rstudio_root_file()
library(glue)
source(glue("{here}/code/setup.R"))

pdf_files <- list.files(glue("{here}/data/"), recursive = T, full.names = T, pattern = 'pdf')

convert_pdf_to_txt<- function(pdf){
  txt_file <- str_replace(pdf, "\\.pdf", "\\.txt")
  if(!file.exists(txt_file)){
    print(paste0(pdf, " converting"))
    system(glue('pdftotext -raw "{pdf}" "{txt_file}"'))
    return()
  }else{
    print(paste0(pdf, " exists"))
    return()
  }

}

future_map(pdf_files, ~convert_pdf_to_txt(.x))


