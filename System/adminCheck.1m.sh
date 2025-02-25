#!/usr/bin/env zsh

#
# <xbar.title>Admin Status for mac</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Laurent</xbar.author>
# <xbar.author.github>Laurent Taupiac</xbar.author.github>
# <xbar.desc>This plugin indicates whether the user is an admin or not.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/ltaupiac/adminCheck/main/Admin.png</xbar.image>
# <xbar.dependencies>macos, zsh, dscl</xbar.dependencies>
# <xbar.abouturl>https://github.com/ltaupiac/adminCheck/README.md</xbar.abouturl>

if dseditgroup -o checkmember admin > /dev/null; then
    echo "⭐️"  # Icon for "Admin"
else
    echo "👤"  # Icon for "not admin"
fi
