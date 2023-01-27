#!/usr/bin/env php
<?php
//  <xbar.title>pivpn information</xbar.title>
//  <xbar.version>v1.0</xbar.version>
//  <xbar.author>Alexandre Espinosa Menor</xbar.author>
//  <xbar.author.github>alexandregz</xbar.author.github>
//  <xbar.desc>Clients and status from pivpn machine, actually only wireguard. You need to instal api.php into your pivpn machine (see doc)</xbar.desc>
//  <xbar.image>https://i.imgur.com/vItSk6W.png</xbar.image>
//  <xbar.abouturl>https://github.com/alexandregz/bitbar-plugins-alexandregz/tree/master/pivpn/README.md</xbar.abouturl>

// config
$pivpn_host = '192.168.1.14';
$token = '';    // not used actually, ToDo


// ripped from danbee - https://xbarapp.com/docs/plugins/Network/wireguard.sh.html
// just wireguard because it's what I use :-)
$img_enabled = "iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAMAAABEpIrGAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAY1BMVEVHcEwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD////Iv5pwAAAAH3RSTlMAA0yMvd6sez0V7/qzbglj5qR0Ncj1KA7VLlQflkGEXYPQGAAAAAFiS0dEILNrPYAAAAAJcEhZcwAAFiUAABYlAUlSJPAAAAAHdElNRQfiCB0GISSxSlkqAAABpklEQVQ4y2VT7YKEIAik1GyzTNus1nbz/d/y8AO3u+OXIMLMgABkTcu4EJ1kvJN9A3/twYYQghqnHvQ8BMMev+/neB2sZEt8++SrcvJWpbEhmdiy3wL0IfBapOlCMZkDezsL9A7KWMq1aVPVThhzpADPXTZVEpzgL/Q9+VRxrX44WB8xCvJdbNKHu6klYhjvJSrCsE7PrbV2R9iUITCbOqo5Qz7fmPEYSrQHT+99FfV9qyvhXU42jcPja/hsX+pj5fDEp8h+QKIPLCYrCFMQ4EuWxMkVqLADV1TEhLEcIjeSQgEdUNUrHrpmRnU+lTsQyw+ARuhumTWireq6WmuMDL31GghNAcnpiDpNrySEnkxNGCvcEK4p7Yv2vtlIyMCqkmaScZInW8/YK1RoZRajhileIOsB8ZI8pinsD8SGIHZH0dI5jiVRZpIPKHCbwy+ahUqoaPa+Ls9Js2B5AQYCDM1RxgGWGkfr8zwEtj5Rlg5HrmPInLQifcY8pT9RpTy279c6Ew7V1p+ITty8m3061ENde9654Ozr3//ePeOHsNfCxsvrGv4BOvFCaHw59FkAAAAldEVYdGRhdGU6Y3JlYXRlADIwMTgtMDgtMjlUMTA6MzM6MzYtMDQ6MDCYYoC0AAAAJXRFWHRkYXRlOm1vZGlmeQAyMDE4LTA4LTI5VDEwOjMzOjM2LTA0OjAw6T84CAAAAABJRU5ErkJggg==";

$img_disabled = "iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAMAAABEpIrGAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAY1BMVEVHcEwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD////Iv5pwAAAAIHRSTlMAAhsxQk09K0Q6FgEHVFk/JVEoEklWDyAKBBkNHTUfLpaxL30AAAABYktHRCCzaz2AAAAACXBIWXMAABYlAAAWJQFJUiTwAAAAB3RJTUUH4ggdBiEgtiedMwAAAaFJREFUOMtdU1GigyAMKyIiKihOVDY373/LVypl7vVHSjFNEwDgEJWslWq0bI3uevgfg7QOY5w6EJN1Xg6/9YnKRstZYPaolxD0DUUYR6HWK68AOufaAtI3LofO/apNYbbwiTmXfUWojfJe0UYtqL6GfCCo+oH5xjkjLiV3i+wSR8V52IHo3GPGrTjeIZrv/9NjfRoTcSw+oXoYSn26KL8knhhs3uzg4PrBU+/vG66Gd14ZsuOTRn+u39FHaPMKBxxwHpu+CKaZBOSRAmokSZwLgYEtZFU89/XQp9mKFMALVPVM30ZM+10cRsB5ICJEmDc8KpiZCwWrSRN+zEH+yAKgyhRJp+m6D2L25cBY6Dp3zq9UjtshVhbSyaKkn3VycpdtcrByhVrMFCIkHw/kbPFgn5t4lIeMW5DbhhrSTF4UoZItT2qla3vkNcmevSBW7P1W9HmxF/Ky1zJhEEu2A+ghqHhdgO5SU2HrvUa+aHlMP/kXX5HOl+sIMbKU/IyoCxkZnpxvaYD48zi7JvU5aXN4O2tW+B/DJz1/c85yPI/v339ndyeoPSR2pwAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAxOC0wOC0yOVQxMDozMzozMy0wNDowMMparxMAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMTgtMDgtMjlUMTA6MzM6MzItMDQ6MDAdcBwbAAAAAElFTkSuQmCC";


$json = file_get_contents('http://'.$pivpn_host.'/admin/api-pivpn.php');
$data = json_decode($json, true);


$status = "Active";
$status_color = '#008000';
$img = $img_enabled;
if($data['status'] !== 'active') {
    $status = 'Disabled';
    $status_color = '#FF0000';
    $img = $img_disabled;
}

$str_connected = '';
foreach($data['connected'] as $c) {
    $str_connected .= $c[0]."\n--".$c[2].': '.$c[1].' - Last seen: '.$c[5]."\n";
}
// remove last newline
$str_connected = trim($str_connected);


$str_disabled = '';
foreach($data['disabled'] as $c) {
    $str_disabled .= $c[1]."\n";
}
// remove last newline
$str_connected = trim($str_connected);


echo '| templateImage='.$img.'
---
Status: '.$status.' | color='.$status_color.'
---
Clients
'.$str_connected.'
---
Disabled
'.$str_disabled;

echo "---
Open shell pivpn | bash='/usr/bin/open' param1='-a' param2='iterm' param3='ssh://pi@$pivpn_host'
";