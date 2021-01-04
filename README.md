## Install

### Emacs

 - Your version of emacs should be at least 27.1
 - For this you need to clone [the emacs repository](https://github.com/emacs-mirror/emacs) and either checkout the `emacs.27.1` (or higher) branch or compile from the `main` branch.
 - I tend to forget what dependencies are needed by emacs so here is the command I usually invoke before configuring and compiling emacs:
   - `sudo apt install autoconf make gcc libgtk-3-dev libxpm-dev libjpeg-dev libgif-dev libtiff5-dev libgnutls28-dev libncurses5-dev pkg-config texinfo`
   - (`pkg-config` may not be needed but I got strange behaviours when not installing it)
 - If you feel confident and better than me at installing emacs from scratch, don't hesitate to tell me where I'm wrong, I always love to learn (and I'm pretty bad at understanding Unix systems, dependencies and things like this)
 - Once `./autogen.sh` and `./configure` tell you you can make, `make -j 4` then `sudo make install`

### For ligatures, all-the-icons and pretty things:

 - `sudo apt install fonts-firacode fonts-material-design-icons-iconfont`

### Once emacs is installed:

 - `M-x package-install` <kbd>RET</kbd> `no-littering`

### Install all the packages

 - `M-x package-install-selected-packages`

If this command fails:

 - `M-x package-refresh-contents`
 - `M-x package-install-selected-packages`

### Configure packages

 - `M-x all-the-icons-install-fonts`
 - `M-x pdf-tools-install`

Restart emacs and everything should work

(thanks to [@coquera](https://github.com/coquera) for forcing me to create a README, this was one big example of procrastination)
