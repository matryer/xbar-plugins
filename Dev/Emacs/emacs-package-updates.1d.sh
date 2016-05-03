#!/bin/sh

# <bitbar.title>Emacs Package Updates</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>Takashi Masuda</bitbar.author>
# <bitbar.author.github>masutaka</bitbar.author.github>
# <bitbar.desc>List available updates from Emacs package</bitbar.desc>
# <bitbar.image>http://i.imgur.com/SBlrP2t.png</bitbar.image>
# <bitbar.dependencies>emacs</bitbar.dependencies>

EMACS=$(which emacs)

exit_with_error() {
  echo "🐮err | color=red";
  exit 1;
}

cat <<EOE > /tmp/emacs-package-updates.el
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(list-packages)

(defun my-package-version (pkg-desc)
  (package-version-join (package-desc-version pkg-desc)))

(let ((upgrades (with-current-buffer "*Packages*"
		  (package-menu--find-upgrades)))
      pkg-desc pkg-name)
  (message "🐮%d" (length upgrades))
  (message "---")
  (dolist (upgrade upgrades)
    (setq pkg-desc (cdr upgrade))
    (setq pkg-name (package-desc-name pkg-desc))
    (message "%s (%s < %s)"
	     pkg-name
	     (my-package-version (cadr (assoc pkg-name package-alist)))
	     (my-package-version pkg-desc))))
EOE

($EMACS -Q --batch -l /tmp/emacs-package-updates.el 2>&1 || exit_with_error) | sed -ne '/🐮/,$p'
