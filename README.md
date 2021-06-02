# [HEXmacs](https://xenobyte.xyz.localhost/projects/?nav=hexmacs)

EMACS is perhaps the most powerful tool in a programmers arsenal, though its power 
lies hidden deep within layers of initial complexity, as one learns the basics of
the system it becomes quite apparent why an increasing amount of developers find
it worthwhile to switch despite the relativelty steep learning curve.
HEXmacs organizes the emacs files into an template that comes preconfigured with 
fully featured development environments for the languages used in the [XENOBYTE.xyz](https://xenobyte.xyz)
projects plus a few, general purpose utilities.
    
To manage all these packages, HEXmacs uses a very simple script that scans the
package list in emacs.d/hexmacs-packages.el and automatically searches, downloads,
installs and updates them. To configure the installed packages the .emacs.d/custom/ 
folder contains the loading and configuration files ordered by package type.
Simply add whatever custom commands their package requires and the main .emacs file will
load it on startup. The main .emacs file also contains the settings for debugging, file backups
and user data. 

**HEXmacs should work with any EMACS 25+ installation, but it is maintained for the newest stable release.**

## Features

* Updated and ready for EMACS 27+
* Fully featured development environments and setups for C, C++, lua, python, javascript, LISP, bash, PHP and more
* Grammar & spelling correction suite for all available langauges (locale.gen)
* Organized template based on pure EMACS LISP (no package loading dependencies at all)
* Unobtrusive configuration that can be easily exapnded upon
* Automated package management
* Much smaller and faster than alternatives like spacemacs
* Regularly updated and kept at bleeding-edge state

## Previews

VER 1.22

<a href="https://i.imgur.com/mFKOEMF.jpg"><img src="https://i.imgur.com/mFKOEMF.jpg" alt="desktop" border="0" style="width:100%;"></a>
<a href="https://i.imgur.com/bTLvJ9p.gif"><img src="https://i.imgur.com/bTLvJ9p.gif" alt="desktop" border="0" style="width:100%;"></a>
<a href="https://i.imgur.com/SseourF.gif"><img src="https://i.imgur.com/SseourF.gif" alt="desktop" border="0" style="width:100%;"></a>
<a href="https://i.imgur.com/OE5Tjma.gif"><img src="https://i.imgur.com/OE5Tjma.gif" alt="desktop" border="0" style="width:100%;"></a>

## Installation & Setup

Visit [xenobyte.xyz](https://xenobyte.xyz/projects/?nav=hexmacs) for a complete setup guide.

## F.A.Q. & Troubleshooting

**Q: A package fails to download / install.**

Make sure the package is properly named, use M-x + list-packages to get a list of all the available pacakges and C-s + the package name to validate. If this doesn't work then I suggest manually cloning the repo into the ~/.emacs.d/elpa folder. EMACS packages are constantly changing, make sure the package giving you trouble is even available in the repos.


**Q: Loading a package crashes EMACS during startup. (.emacs is ignored / default EMACS)**

This usually means that the package is missing / can't find a SYSTEM dependency. If the package depends on third-party binaries (e.g. tern, indium** double check the installation path and delete the contents of the ~/.emacs.d/elpa folder so HEXmacs can refresh the configuration. This is only required if EMACS fails to boot!


**Q: The EMACS cursor looks different when running EMACS as a deamon and attaching a client than when running a server per client.**

The emacsclient command doesn't start a new frame, it simply inherits the server's. Because EMACS was started as a clientless daemon it didn't start a frame in the first place. A potential workaround is to attach the cursor configuration to an non-init hook or call it after the emacsclient is initialized.


**Q: What is the project's license? Do I have to pay or credit you in any way?**

The project is MIT licensed and free. 


