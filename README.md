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

### External files

 - I have a directory **`~/org`** that contains 4 files:
 
        org
        ├── agenda.org
        ├── calendar_company.org
        ├── calendar_user.org
        └── orgzly.org

    - This repository is synchronised on all my devices with [Syncthing](https://syncthing.net/) but the directory is checked entirely by org so you can put the files you want in it
  - I don't like having things everywhere. That's why I configured org-gcal to synchronise with my company's Google Calendar. For this I needed a secret key that I can't realistically put in a public repository. This key is located in **`~/.secrets/gcal-secrets.json`**

        {
            "org-gcal-client-id": "my_id.apps.googleusercontent.com",
            "org-gcal-client-secret": "my_secret_key",
            "calendar-company": "my_company_calendar_id",
            "calendar-user": "my_user_calendar_id"
        }
        
    - Thanks to [-jz-](https://www.reddit.com/user/-jz-/) for [this thread](https://www.reddit.com/r/emacs/comments/d1ehpy/security_tip_if_you_push_initel_to_a_public_repo/)

Restart emacs and everything should work

### Big Thanks

 - [daviwil](https://github.com/daviwil) for his [emacs from scratch](https://github.com/daviwil/emacs-from-scratch) serie
 - [hlissner](https://github.com/hlissner) for [doom-emacs](https://github.com/hlissner/doom-emacs/)
 - Many other that I stupidly didn't store for small config tricks, bug fixes etc that I found on StackOverflow, Reddit, GitHub, personal blogs etc

### TODOS:
 - [ ] Rewrite this README in org
 - [ ] Maybe try this "literate" programming thing
 - [ ] I experimented with `emacs-daemon` and `emacsclient` but not enough to make it viable
 - [ ] Complete this TODO list
 - [ ] Add all the remaining thanks

(thanks to [@coquera](https://github.com/coquera) for forcing me to create a README, this was one big example of procrastination)
