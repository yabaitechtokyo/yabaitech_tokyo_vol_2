#! /bin/bash

function installFont() {
    local name=$1
    local url=$2
    curl -L ${url} -o ${name}.zip
    unzip -nq ${name}.zip -d .satysfi/local/fonts/${name}
    rm ${name}.zip
}

installFont CMU https://www.fontsquirrel.com/fonts/download/computer-modern
installFont Inconsolata https://www.fontsquirrel.com/fonts/download/Inconsolata
installFont NotoSans https://noto-website-2.storage.googleapis.com/pkgs/NotoSans-hinted.zip
installFont NotoSansMono https://noto-website-2.storage.googleapis.com/pkgs/NotoMono-hinted.zip
installFont NotoSerif https://noto-website-2.storage.googleapis.com/pkgs/NotoSerif-hinted.zip
installFont RobotoMono https://www.wfonts.com/download/data/2016/05/18/roboto-mono/roboto-mono.zip

curl -L http://sourceforge.net/projects/dejavu/files/dejavu/2.37/dejavu-fonts-ttf-2.37.zip -o DeJaVu.zip
unzip DeJaVu.zip -d DeJaVu
mv DeJaVu/dejavu-fonts-ttf-2.37/ttf .satysfi/local/fonts/DeJaVu
rm DeJaVu.zip
rm -rf DeJaVu
