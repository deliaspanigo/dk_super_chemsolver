# Detener todos los contenedores en ejecución
docker stop $(docker ps -q)

# Eliminar todos los contenedores (en ejecución y detenidos)
docker rm $(docker ps -a -q)

# Eliminar todas las imágenes
docker rmi $(docker images -q)

# Eliminar volúmenes no utilizados
docker volume prune -f

# Eliminar redes no utilizadas
docker network prune -f

# Limpiar el sistema (contenedores, imágenes, volúmenes, redes) de una vez
docker system prune -a --volumes -f

