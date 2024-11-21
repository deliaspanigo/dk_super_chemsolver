

sudo apt update


sudo apt install certbot python3-certbot-nginx -y



###############################################################
# # sudo nano /etc/nginx/sites-available/default
############################################

server {
    listen 80;
    server_name www.chemsolver.xyz;

    location /.well-known/acme-challenge/ {
        root /usr/share/nginx/html;
    }

    location / {
        return 301 https://$host$request_uri;
    }
}

server {
    listen 443 ssl; # managed by Certbot
    server_name www.chemsolver.xyz;

    ssl_certificate /etc/letsencrypt/live/www.chemsolver.xyz/fullchain.pem; # managed by Certbot
    ssl_certificate_key /etc/letsencrypt/live/www.chemsolver.xyz/privkey.pem; # managed by Certbot
    include /etc/letsencrypt/options-ssl-nginx.conf; # managed by Certbot
    ssl_dhparam /etc/letsencrypt/ssl-dhparams.pem; # managed by Certbot

    location /shiny01 {
        proxy_pass http://localhost:3838/;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}

server {
    if ($host = www.chemsolver.xyz) {
        return 301 https://$host$request_uri;
    } # managed by Certbot

    listen 80;
    server_name www.chemsolver.xyz;
    return 404; # managed by Certbot
}

################################################################################





