#!bin/sh
#
# Automates initial config of Solenya's files.

echo "Welcome to Solenya's configation installation script\n"
sleep 1s

echo "\nPreparing Nodejs...\n"
curl -sL https://deb.nodesource.com/setup_12.x | sudo -E bash -

echo "\nInstalling packages via apt...\n"
sudo add-apt-repository ppa:kelleyk/emacs -y
sudo apt update
sudo apt install git python-virtualenv terminator zsh gnome-tweak-tool nodejs python3-pip emacs26 libssl-dev libreadline-dev zlib1g-dev -y

echo "\nInstalling npm packages...\n"
sudo npm install -g tern pure-prompt --allow-root --unsafe-perm=true

echo "\nInstalling pip packages...\n"
pip3 install pylint flake8 jedi

echo "\nMoving init.el...\n"
mkdir ~/.emacs.d/
mv init.el ~/.emacs.d/

echo "\nMoving .zshrc...\n"
mv .zshrc ~/

echo "\nSetuping jump...\n"
wget https://github.com/gsamokovarov/jump/releases/download/v0.21.0/jump_0.21.0_amd64.deb
sudo dpkg -i jump_0.21.0_amd64.deb

echo "\nSetuping terminal...\n"
chsh -s $(which zsh)
wget https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh
# Prevent the script from stopping.
sed -i.tmp 's:env zsh::g' install.sh
sed -i.tmp 's:chsh -s .*$::g' install.sh
sed '111d' install.sh
chmod +x install.sh
./install.sh
git clone https://github.com/zsh-users/zsh-autosuggestions ~/.oh-my-zsh/plugins/zsh-autosuggestions

echo "\nInstalling rbenv...\n"
wget -q https://github.com/rbenv/rbenv-installer/raw/master/bin/rbenv-installer -O- | bash

echo "\nHappy Hacking!\n"
