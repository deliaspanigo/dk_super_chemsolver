
# 00) Tabla periodica
data00_tabla_periodica <- openxlsx::read.xlsx(xlsxFile = "ChemClass_Oso/data00_PeriodicTable_esp.xlsx",
                                              sheet = 1) 
data00_tabla_periodica$"valence" <- as.character(data00_tabla_periodica$"valence")

vector_fq_interno <- c("oxidos", "hidroxidos", "acidos", 
                       "hidruros", "oxosales", "sales")

vector_fq_interno_mod <- c("óxidos", "hidróxidos", "ácidos", 
                       "hidruros", "oxosales", "sales")

names(vector_fq_interno_mod) <- vector_fq_interno

vector_fq_externo <- c("1) Óxidos (226 ecuaciones)", "2) Hidróxidos (194 ecuaciones)", 
                       "3) Ácidos (31 ecuaciones)", 
                      "4) Hidruros (5 ecuaciones)", "5) Oxosales (6014 ecuaciones)", 
                      "6) Sales (970 ecuaciones)")

# Opciones de selectInput
vector_familia_quimica <- vector_fq_interno
names(vector_familia_quimica) <- vector_fq_externo 

# Names General
data01_names_general <- openxlsx::read.xlsx(xlsxFile = "ChemClass_Oso/data01_names.xlsx",
                                    sheet = 1) 
# Names de cada familia Quimica
data02_names_fq <- sapply(2:7, function(x){
  openxlsx::read.xlsx(xlsxFile = "ChemClass_Oso/data01_names.xlsx",
                      sheet = x) 
})
names(data02_names_fq) <- vector_fq_interno

###############################
vector_reso_folder_new <- list.files("ChemClass_Oso/outputs_global", full.names = T)
vector_reso_folder_new <- grep("\\.xlsx$", vector_reso_folder_new, value = TRUE)
vector_reso_folder_new <- sort(vector_reso_folder_new)
LAS_NUEVAS <- sapply(vector_reso_folder_new, function(x){
    openxlsx::read.xlsx(xlsxFile =  x, sheet = 1) 
}, USE.NAMES = T)
names(LAS_NUEVAS) <- vector_fq_interno

data01_names_general_mod <- openxlsx::read.xlsx(xlsxFile = "ChemClass_Oso/v2/data01_names.xlsx",
                                            sheet = 1) 
###################
vector_reso_folder <- list.dirs("ChemClass_Oso/estequimetria")
vector_reso_folder <- vector_reso_folder[-1]
vector_nombres_folder <- sapply(vector_reso_folder, function(x){
  strsplit(x,"/")[[1]][3]
}, USE.NAMES = F)

las_reso <- sapply(vector_reso_folder, function(x){
  vector_las_reso <- list.files(x)
  sapply(vector_las_reso, function(y){
    the_file <- paste0(x, "/", y)
    openxlsx::read.xlsx(xlsxFile =  the_file, sheet = 1) 
  }, simplify = F, USE.NAMES = T)
}, USE.NAMES = T)
########################




vector_labels <- paste0(data00_tabla_periodica$atomic_number, " - ", 
                        data00_tabla_periodica$name, " - ",
                        data00_tabla_periodica$symbol, " - ",
                        data00_tabla_periodica$type, " - ",
                        data00_tabla_periodica$state)

vector_elemento <- data00_tabla_periodica$symbol
names(vector_elemento) <- vector_labels




js <- "
$(document).on('shiny:value', function(event) {
  if(event.name === 'table'){
    setTimeout(function() {
      MathJax.Hub.Queue(['Typeset', MathJax.Hub]);
    }, 100);
  }
});
$(document).on('shiny:inputchanged', function(event) {
  if(event.name === 'table'){
    setTimeout(function() {
      MathJax.Hub.Queue(['Typeset', MathJax.Hub]);
    }, 100);
  }
});
"