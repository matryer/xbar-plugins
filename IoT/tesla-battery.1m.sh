#!/usr/bin/env bash

# <xbar.title>Tesla xBar</xbar.title>
# <xbar.version>v0.1.0</xbar.version>
# <xbar.author>kolisko</xbar.author>
# <xbar.author.github>kolisko</xbar.author.github>
# <xbar.desc>Tesla battery range, percentage and charging controls via Fleet API. Requires a separate Tesla xBar installation and your own Tesla Developer app; setup links appear until installed.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/kolisko/tesla-xbar/v0.1.0/docs/images/menu.png</xbar.image>
# <xbar.dependencies>bash,python3</xbar.dependencies>
# <xbar.abouturl>https://github.com/kolisko/tesla-xbar</xbar.abouturl>

# SPDX-License-Identifier: MIT
# Install and configure the application: https://github.com/kolisko/tesla-xbar#install
# The installer builds the helpers and preserves this plugin's refresh filename.
# Credentials and signing keys belong to the user's local profile, not this file.
# Each xBar refresh calls the installed menu once. Only explicit menu actions wake
# the vehicle or change charging; normal live polling may delay vehicle sleep.

TESLA_XBAR_LAUNCHER="$HOME/Library/Application Support/Tesla xBar/tesla-action.sh"

if [ -x "$TESLA_XBAR_LAUNCHER" ]; then
    exec "$TESLA_XBAR_LAUNCHER" menu
fi

printf '%s\n' \
    'Tesla setup' \
    '---' \
    'Tesla xBar needs installation | disabled=true' \
    'Install Tesla xBar... | href=https://github.com/kolisko/tesla-xbar#install' \
    'Tesla Developer setup... | href=https://github.com/kolisko/tesla-xbar/blob/main/docs/SETUP.md' \
    '---' \
    'Requires your own Tesla Developer app | disabled=true' \
    'Refresh | refresh=true'
