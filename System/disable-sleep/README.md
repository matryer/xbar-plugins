# Disable sleep

Menu bar toggle for disabling/enabling sleep.

## Setup

### 1. Install xbar

Install [xbar](https://xbarapp.com/) (free menubar toolkit). For example with Homebrew:

```bash
brew install --cask xbar
```

Launch xbar once so it creates its plugins folder (`~/Library/Application Support/xbar/plugins`).

### 2. Install this plugin

Run the `instal.sh` - downloads scripts and assets, links the plugin into xbar’s folder, runs setup:

```bash
curl -fsSLO https://raw.githubusercontent.com/kiprasmel/xbar-plugins/refs/heads/main/System/disable-sleep/install.sh
# optionally inspect install.sh, then:
bash install.sh
```

Note: first time setup will ask for sudo password, to allow running `sudo pmset` (toggle sleep) without password.
