#!/usr/bin/env -S PATH="${PATH}:/opt/homebrew/bin:/usr/local/bin" php
<?php
// <xbar.title>BitBar Plugins Installer</xbar.title>
// <xbar.version>v1.0</xbar.version>
// <xbar.author>Aleš Farčnik</xbar.author>
// <xbar.author.github>alesf</xbar.author.github>
// <xbar.desc>Quickly install bitbar plugins.</xbar.desc>
// <xbar.image>http://i.imgur.com/Qn8TJ60.png</xbar.image>
// <xbar.dependencies>php >= 5</xbar.dependencies>

define('CAT_URL', 'https://api.github.com/repos/matryer/xbar-plugins/git/trees/main?recursive=1');
define('UA', 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; '.
    '.NET CLR 1.0.3705; .NET CLR 1.1.4322; Media Center PC 4.0)');

function curl($url)
{
    $ch = curl_init();
    curl_setopt($ch, CURLOPT_URL, $url);
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
    curl_setopt($ch, CURLOPT_USERAGENT, UA);
    $output = curl_exec($ch);
    curl_close($ch);
    return json_decode($output);
}

function bitbar_link($name, $path)
{
    return "bitbar://openPlugin?title={$name}&".
        "src=https://github.com/matryer/bitbar-plugins/raw/master/{$path}";
}

echo "BitBar Plugins\n";
echo "---\n";

$cats = curl(CAT_URL);
$cats = isset($cats->tree) && is_array($cats->tree) ? $cats->tree : array();

array_map(function ($item) {

    $level = substr_count($item->path, '/');
    $level_dash = str_repeat('--', $level);
    $sub_start = strrpos($item->path, '/') ? strrpos($item->path, '/')+1 : 0;
    $name = substr($item->path, $sub_start);

    if ($item->type == 'tree') {
        if ($name != 'Enabled') {
            echo "{$level_dash}{$name}\n";
        }
    } elseif ($item->type == 'blob' && $level > 0 && $name[0] != '.') {
        $link = bitbar_link($name, $item->path);
        echo "{$level_dash}{$name} | href={$link}\n";
    }
}, $cats);
