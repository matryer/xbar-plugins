## Install Acquia Cloud plugins

1. Install the pre-requisites:
   1. [Composer](https://getcomposer.org/download/)
   2. [BitBar PHP](https://github.com/SteveEdson/bitbar-php)
      ```
      composer global require steveedson/bitbar-php
      ```
   3. [Acquia CLI](https://docs.acquia.com/acquia-cli/install/)
      ```
      curl -OL https://github.com/acquia/cli/releases/latest/download/acli.phar
      chmod +x acli.phar
      mv acli.phar /usr/local/bin/acli
      ```
   4. Authenticate with Acquia CLI:
      ```
      acli login
      ```
2. Follow the typical installation process for xBar plugins by browsing for the plugin in the xBar UI and clicking "install."
      
That's it!