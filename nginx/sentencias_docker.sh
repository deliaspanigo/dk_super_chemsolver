# Construir la imagen Docker
docker build -t chemsolver_img01_nginx:v0.0.1 .

docker tag chemsolver_img01_nginx:v0.0.1 legion949/chemsolver_img01_nginx:v0.0.1

docker push legion949/chemsolver_img01_nginx:v0.0.1

docker tag chemsolver_img01_nginx:v0.0.1 legion949/chemsolver_img01_nginx:latest


docker push legion949/chemsolver_img01_nginx:latest


##################################################################################################

# Ejecutar el contenedor Docker
# docker run -d -p 8080:8080 --name cont_chemsolver --restart unless-stopped img_chemsolver


# Entrar al contenedor
# docker exec -it cont_chemsolver /bin/bash

##################################################################################################

# docker stop cont_chem_solver

# docker rm cont_chem_solver


##################################################################################################



# docker run -d -p 8080:8080 --name cont_chem_solver --restart unless-stopped legion949/img_chem_solver:v0.0.1

