#!bin/sh
#
# Automates initial config of MihailDono's files.

echo "Powering up systems...\n"
sleep 1s

echo "\nInstalling packages via apt...\n"
sudo apt install git terminator zsh silversearcher-ag markdown -y

echo "\nMoving .zshrc...\n"
cp ./.zshrc ~/

echo "\nSetuping terminal...\n"
wget -O zshinstall.sh https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh
# Prevent the script from stopping.
sed -i.tmp 's:::g' zshinstall.sh
sed -i.tmp 's:'
sed '111d' zshinstall.sh
chmod +x zshinstall.sh
./zshinstall.sh
rm zshinstall.sh

# Add zsh plugins
git clone https://github.com/zsh-users/zsh-autosuggestions ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
git clone https://github.com/romkatv/powerlevel10k.git ~/.oh-my-zsh/custom/themes/powerlevel10k

curl -fsSL https://fnm.vercel.app/install | bash

echo "\nInstalling npm packages...\n"
npm install -g typescript typescript-language-server # --allow-root --unsafe-perm=true

echo "\nInstalling emacs27...\n"
snap install emacs --classic

echo "\nHappy Hacking!\n"

# MacOS
# echo "Powering up systems...\n"
# sleep 1s

# Install brew packages
# echo "Installing brew packages...\n"
# /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
# brew install autojump fnm warp fzf

# Install zsh
# wget -O zshinstall.sh https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh

# Add zsh plugins
# git clone https://github.com/zsh-users/zsh-autosuggestions ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
# git clone https://github.com/romkatv/powerlevel10k.git ~/.oh-my-zsh/custom/themes/powerlevel10k

# echo "\nInstalling npm packages...\n"
# npm install -g typescript typescript-language-server # --allow-root --unsafe-perm=true

# echo "\nMoving .zshrc...\n"
# cp ./.zshrc ~/
