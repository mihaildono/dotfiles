Before dot files


![alt tag](https://cloud.githubusercontent.com/assets/15171105/19050258/83ed92e2-89b6-11e6-8c8f-6c3ebd4991bf.jpg)


After dot files

![alt tag](https://cloud.githubusercontent.com/assets/15171105/19050494/8194e5ee-89b7-11e6-9d75-afd1652e3130.jpg)

# Setup
* Prepare nodejs
``` sh
$ curl -sL https://deb.nodesource.com/setup_10.x | sudo -E bash -
```
* Install packages
* * Create ssh key for github
* * Remap caps lock to ctrl
``` sh
$ sudo apt install git virtualenv terminator zsh gnome-tweak-tool nodejs setuptools wheel 
```
* Setup terminal
``` sh
$ chsh -s $(which zsh)
$ sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
$ wget https://github.com/gsamokovarov/jump/releases/download/v0.21.0/jump_0.21.0_amd64.deb
$ sudo dpkg -i jump_0.21.0_amd64.deb
$ git clone https://github.com/zsh-users/zsh-autosuggestions ~/.oh-my-zsh/plugins/zsh-autosuggestions
```
* Prepare emacs and install
``` sh
$ sudo add-apt-repository ppa:kelleyk/emacs
$ sudo apt update
$ sudo apt install emacs26
```
* Install npm packages
``` sh
$ sudo npm install -g tern eslint babel-eslint eslint-plugin-import eslint-plugin-react eslint-plugin-jsx-a11y pure-prompt eslint-config-airbnb
```
* Setup pure prompt for zsh - https://github.com/sindresorhus/pure
``` sh
$ cd /usr/lib/node_modules/pure-prompt
$ sudo ln -sf "$PWD/pure.zsh" /usr/local/share/zsh/site-functions//prompt_pure_setup
$ sudo ln -sf "$PWD/async.zsh" /usr/local/share/zsh/site-functions//async
```
* Setup linters
``` sh
$ npx install-peerdeps --dev eslint-config-airbnb
$ echo $'"extends": "airbnb"' >> .eslintrc
```
* Run inside Emacs for jedi setup
```
jedi install server
jedi:start-dedicated-server
```
