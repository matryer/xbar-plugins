# JSON Utils

## Description

A helper for **format** or **compact** JSON from clipboard and then write to clipboard.

## Screenshot

![JSON Utils main](https://raw.githubusercontent.com/cnfn/grocery/master/images/blog/bitbar_plugin_json_utils_main.png)
![JSON Utils notification](https://raw.githubusercontent.com/cnfn/grocery/master/images/blog/bitbar_plugin_json_utils_notification.png)

## Usage

### Format

1. Copy JSON string to clipboard, like `{"key": "value", "list": [1, 2]}`;

2. Click `JSON, Format`;

3. Paste to somewhere will get:

   ```json
   {
       "key": "value",
       "list": [
           1,
           2
       ]
   }
   ```

### Compact

1. Copy JSON string to clipboard, like:

   ```json
   {
       "key": "value",
       "list": [
           1,
           2
       ]
   }

   ```

2. Click `JSON, Compact`;

3. Paste to somewhere will get:

   ```json
   {"key":"value","list":[1,2]}

   ```

## Install

1. Install [jq](https://github.com/stedolan/jq):

   ```sh
   brew install jq
   ```

2. Install [BitBar](https://github.com/matryer/bitbar):

   ```sh
   brew cask install bitbar
   ```

3. Open [this plugin's page](https://getbitbar.com/plugins/Tools/JsonUtils/JsonUtils.sh), and click **+Add to BitBar**;

4. Enjoy it! :smile:
