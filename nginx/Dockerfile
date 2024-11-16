#FROM nginx:latest
FROM nginx:alpine
COPY nginx.conf /etc/nginx/conf.d/default.conf


# Copiar el archivo de la página inicial (index.html) dentro del contenedor
COPY ./html /usr/share/nginx/html

# Exponer el puerto 80 para acceder a la página web
EXPOSE 80

# Usar el comando predeterminado de Nginx para ejecutar el servidor
CMD ["nginx", "-g", "daemon off;"]


