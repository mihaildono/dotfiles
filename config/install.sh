#!bin/sh
#
# Automates initial config of MihailDono's files.

echo "Powering up systems...\n"
sleep 1s

echo "\nInstalling packages via apt...\n"
sudo apt install git terminator zsh silversearcher-ag markdown -y

echo "\nMoving .zshrc...\n"
cp config/.zshrc ~/

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
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting
git clone https://github.com/lukechilds/zsh-nvm ~/.oh-my-zsh/custom/plugins/zsh-nvm

echo "\nInstalling npm packages...\n"
npm install -g typescript typescript-language-server # --allow-root --unsafe-perm=true

echo "\nInstalling emacs27...\n"
snap install emacs --classic

echo "\nHappy Hacking!\n"

# MacOS
# Install brew
# /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
# Install brew packages
# Install zsh
# wget -O zshinstall.sh https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh
# brew install autojump markdown the_silver_searcher aspell
# Add zsh plugins
# git clone https://github.com/zsh-users/zsh-autosuggestions ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
# git clone https://github.com/romkatv/powerlevel10k.git ~/.oh-my-zsh/custom/themes/powerlevel10k
# git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting
# git clone https://github.com/lukechilds/zsh-nvm ~/.oh-my-zsh/custom/plugins/zsh-nvm

# echo "\nInstalling npm packages...\n"
# npm install -g typescript typescript-language-server # --allow-root --unsafe-perm=true
