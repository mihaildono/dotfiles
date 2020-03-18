Before dot files


![alt tag](https://cloud.githubusercontent.com/assets/15171105/19050258/83ed92e2-89b6-11e6-8c8f-6c3ebd4991bf.jpg)


After dot files


![alt tag](https://cloud.githubusercontent.com/assets/15171105/19050494/8194e5ee-89b7-11e6-9d75-afd1652e3130.jpg)


# Setup
* Install prerequisites
``` sh
$ apt install git curl
```

* Install dependencies
``` sh
$ sh confish.sh
```
* * Remap caps lock to ctrl

* Run inside Emacs for jedi setup
```
jedi:install server
jedi:start-dedicated-server
```

* Add tldr to bin (bin may be created beforehand by pt)
```
mkdir -p ~/bin
curl -o ~/bin/tldr https://raw.githubusercontent.com/raylee/tldr/master/tldr
chmod +x ~/bin/tldr
```

tested on ubuntu 16+

TODO: Consider substituting terminator for terminus
