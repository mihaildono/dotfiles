#!bin/bash
#
# Automates initial config of Solenya's files.

echo "Welcome to Solenya's configation installation script\n"
sleep 1s

echo "\nPreparing Nodejs...\n"
curl -sL https://deb.nodesource.com/setup_10.x | sudo -E bash -

echo "\nInstalling packages via apt...\n"
sudo add-apt-repository ppa:kelleyk/emacs -y
sudo apt update
sudo apt install git virtualenv terminator zsh gnome-tweak-tool nodejs python-pip emacs26 -y

echo "\nSetuping terminal...\n"
chsh -s $(which zsh)
sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
wget https://github.com/gsamokovarov/jump/releases/download/v0.21.0/jump_0.21.0_amd64.deb
sudo dpkg -i jump_0.21.0_amd64.deb
git clone https://github.com/zsh-users/zsh-autosuggestions ~/.oh-my-zsh/plugins/zsh-autosuggestions

echo "\nInstalling npm packages...\n"
sudo npm install -g tern pure-prompt

echo "\nSetuping linters...\n"
echo $'"extends": "airbnb"' >> ~/.eslintrc

echo "\nMoving .zshrc...\n"
mv .zshrc ~/.zshrc

echo "\nMoving init.el...\n"
mv .zshrc ~/.emacs.d/init.el

echo "\nMoving jsconfig.json...\n"
mv jsconfig.json ~/jsconfig.json

echo "\nHappy hacking!"
