#!bin/sh
#
# Automates initial config of MihailDono's files.

echo "Powering up systems...\n"
sleep 1s

echo "\nInstalling packages via apt...\n"
sudo apt install git terminator zsh silversearcher-ag markdown -y

echo "\nMoving init.el...\n"
mkdir ~/.emacs.d/
cp init.el ~/.emacs.d/

echo "\nMoving .zshrc...\n"
cp config/.zshrc ~/

echo "\nSetuping jump...\n"
wget https://github.com/gsamokovarov/jump/releases/download/v0.30.1/jump_0.30.1_amd64.deb
sudo dpkg -i jump_0.30.1_amd64.deb
rm jump_0.30.1_amd64.deb

echo "\nSetuping terminal...\n"

wget -O zshinstall.sh https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh
# Prevent the script from stopping.
sed -i.tmp 's:::g' zshinstall.sh
sed -i.tmp 's:'
sed '111d' zshinstall.sh
chmod +x zshinstall.sh
./zshinstall.sh
rm zshinstall.sh
git clone https://github.com/zsh-users/zsh-autosuggestions ~/.oh-my-zsh/plugins/zsh-autosuggestions

echo "\nInstalling NVM and node...\n"
mkdir ~/.nvm
wget -qO- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash
nvm install node

echo "\nInstalling npm packages...\n"
npm install -g typescript typescript-language-server spaceship-prompt  # --allow-root --unsafe-perm=true

echo "\nInstalling emacs27...\n"
snap install emacs --classic

echo "\nHappy Hacking!\n"
