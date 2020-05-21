#!/usr/local/bin/node
// <bitbar.title>Ejector</bitbar.title>
// <bitbar.version>v1.0</bitbar.version>
// <bitbar.author>Nigel Scott</bitbar.author>
// <bitbar.author.github>gruntfuggly</bitbar.author.github>
// <bitbar.desc>Allows volumes to be ejected/unmounted.</bitbar.desc>
// <bitbar.image>https://i.imgur.com/EcCmyng.png</bitbar.image>
// <bitbar.dependencies>node</bitbar.dependencies>

var child_process = require( 'child_process' );

var mountedVolumes = child_process.execSync( "ls -1 /Volumes" ).toString().trim().split( '\n' );
var volumes = child_process.execSync( "diskutil list external physical" ).toString().trim().split( '\n' );

var volumeMap = {};

volumes.map( function( volume )
{
    // Should find a better way to do this...
    var name = volume.substr( 33, 23 ).trim();
    var identifier = volume.substr( 68 );

    if( name && identifier !== 'IDENTIFIER' )
    {
        volumeMap[ name ] = identifier;
    }
} );

var entries = [];

mountedVolumes.map( function( volume )
{
    if( volumeMap[ volume ] )
    {
        var entry = volume + " | terminal=false refresh=true bash=/usr/sbin/diskutil param1=unmount param2=" + volumeMap[ volume ];
        entries.push( entry );
    }
} );

if( entries.length > 0 )
{
    console.log( "⏏" );
    console.log( "---" );
    entries.map( function( entry )
    {
        console.log( entry );
    } );
}
