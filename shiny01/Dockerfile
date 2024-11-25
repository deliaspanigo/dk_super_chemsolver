# Usa la imagen base de rocker/shiny
FROM rocker/shiny:latest

# Instala las dependencias necesarias
RUN R -e "install.packages(c('auth0', 'shiny', 'shinydashboard', 'shinyjs', 'DT', 'Ryacas', 'Ryacas0', 'mathjaxr', 'stringi', 'openxlsx', 'tapLock', 'glue'))"

RUN rm -rf /srv/shiny-server/*   

# Cambia el workdirectory                            
WORKDIR /srv/shiny-server/ 

# Copia los archivos de tu aplicación en el contenedor
COPY app ./

# Expone el puerto 3838
EXPOSE 3838

# Ejecuta la aplicación Shiny
# Automaticamente busca y levanta app.R
#CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', port=8080, host='0.0.0.0')"]
#CMD ["R", "-e", "auth0::shinyAppAuth0(ui = ui, server = server)"]
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]



