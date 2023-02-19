#!/usr/bin/env /usr/local/bin/node
// <xbar.title>fan_speed</xbar.title>
// <xbar.version>v1.0</xbar.version>
// <xbar.author>Masayuki Sunahara</xbar.author>
// <xbar.author.github>tamanishi</xbar.author.github>
// <xbar.desc>Shows fan speed. Strongly inpired by Eric Ripa's "Fan Speed" plugin.</xbar.desc>
// <xbar.image>https://github.com/tamanishi/fan_speed/blob/master/image.png?raw=true</xbar.image>
// <xbar.dependencies>node,smc</xbar.dependencies>
// <xbar.abouturl>https://github.com/tamanishi/fan_speed</xbar.abouturl>

// To install smc:
//   brew install --cask smcfancontrol
//   ln -sf /Applications/smcFanControl.app/Contents/Resources/smc /usr/local/bin/smc

const execSync = require('child_process').execSync

let speeds= '♨ '

for (var fanCount = 0; fanCount < 2; fanCount++) {
    // smc command responds like "  F0Ac  [flt ]  (bytes d0 f4 9c 44)".
    let str = execSync(`/usr/local/bin/smc -k F${fanCount}Ac -r`).toString()
    // extract inside parrens.
    let result = str.match(/\(bytes (.+)\)/)
    
    let array = result[1].split(' ')
    
    let buffer = new ArrayBuffer(4)
    let bytes = new Uint8Array(buffer)
    
    array.forEach((element, index) => {
        bytes[index] = parseInt(element, 16)
    })
    
    let view = new DataView(buffer)
    
    // "true" means "treat as Little-endian".
    speeds += Math.floor(view.getFloat32(0, true)).toString()
    speeds += ' rpm '
}

console.log(speeds + '| size=12')
