# Final de Programacion Funcional - Augusto Henestrosa

Aplicación diseñada con Haskell e IHP para el firmado de archivos en una fecha y su correspondiente verificación con la firma correspondiente y para la fecha correspondiente. Utiliza HsOpenSSL. 

## Instalación

Se asume que se usa Linux con Ubuntu/Debian/Mint/Fedora.

Clonar este repositorio:
```bash
git clone git@github.com:ahenestrosa/programacion-funcional.git
```

Instalar Nix packet manager:
```bash
sudo apt update
sudo apt upgrade
sudo apt install git curl make -y
curl -L https://nixos.org/nix/install | sh
. ~/.nix-profile/etc/profile.d/nix.sh
```

Instalar IHP
```bash
nix-env -f https://downloads.digitallyinduced.com/ihp-new.tar.gz -i ihp-new
```

## Ejecucción

Para correr el programa:
```bash
cd signer
./start
```

> La primera vez puede tardar bastante porque se instalan todos los paquetes y dependencias.

Se levantará la webapp en el puerto 8000 de localhost:

Acceder: [localhost:8000](http://localhost:8000)


