#!/usr/local/bin/node
// <xbar.title>Unmount</xbar.title>
// <xbar.version>v1.0</xbar.version>
// <xbar.author>Nigel Scott</xbar.author>
// <xbar.author.github>gruntfuggly</xbar.author.github>
// <xbar.desc>Allows volumes to be ejected/unmounted.</xbar.desc>
// <xbar.image>https://i.imgur.com/EcCmyng.png</xbar.image>
// <xbar.dependencies>node</xbar.dependencies>

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
