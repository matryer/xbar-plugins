#!/bin/bash
# <xbar.title>CommitStrip</xbar.title>
# <xbar.version>0.0.1</xbar.version>
# <xbar.author>Yukai Huang</xbar.author>
# <xbar.author.github>Yukaii</xbar.author.github>
# <xbar.desc>Random CommitStrip comics</xbar.desc>
# <xbar.image>https://i.imgur.com/CllAUAl.png</xbar.image>
# <xbar.dependencies>bash, jq</xbar.dependencies>
# <xbar.abouturl>https://github.com/parvez/bitbar-plugins</xbar.abouturl>
echo "| image=/9j/4AAQSkZJRgABAQAAAQABAAD//gAgQ29tcHJlc3NlZCBieSBqcGVnLXJlY29tcHJlc3MA/9sAhAADAwMDAwMEBAQEBQUFBQUHBwYGBwcLCAkICQgLEQsMCwsMCxEPEg8ODxIPGxUTExUbHxoZGh8mIiImMC0wPj5UAQMDAwMDAwQEBAQFBQUFBQcHBgYHBwsICQgJCAsRCwwLCwwLEQ8SDw4PEg8bFRMTFRsfGhkaHyYiIiYwLTA+PlT/wgARCAAQABADASIAAhEBAxEB/8QAFgABAQEAAAAAAAAAAAAAAAAABAUH/9oACAEBAAAAAMPam/8A/8QAFAEBAAAAAAAAAAAAAAAAAAAAB//aAAgBAhAAAABA/8QAFAEBAAAAAAAAAAAAAAAAAAAABv/aAAgBAxAAAAA//8QAIhAAAgICAgICAwAAAAAAAAAAAgQBAwUSBhEAEwcUIkFR/9oACAEBAAE/AHOHq4zAqPP5xOh11KXFsfpaZlTJahuYDIgZ9TIj/POSfG73F8Enkncoh7WV1r4SgGRt9bQRYEgZ1DVb1Extoc9eMcw4o+mmxkcAy7llcVSgG7mifS9fqquKsBgyIRiPx3iO48b+UsSXDcpx9VDLxQ+rTUCLOS+yglaBic3LVnXuE9jPUbfvz//EABsRAAEEAwAAAAAAAAAAAAAAAAIAARExBCIy/9oACAECAQE/ACLKkIZ+9qpf/8QAGBEAAgMAAAAAAAAAAAAAAAAAASEAA0H/2gAIAQMBAT8AArbxT//Z"
echo ---

JQ=$(command -v jq)
CURL=$(curl --silent "http://www.commitstrip.com/en/wp-json/wp/v2/posts?per_page=100")
IMG_NUM=$(echo "${CURL}" | $JQ -r 'length')
IMG_RAND=$(( ( RANDOM % IMG_NUM )  + 1 ))
ID=$(echo "${CURL}" | $JQ -r ".[$IMG_RAND].id")

CURL=$(curl --silent "http://www.commitstrip.com/en/wp-json/wp/v2/posts/$ID")
IMG_URL=$(echo "${CURL}" | $JQ -r '.content.rendered | match("http[^ \"]+") | .string')
LINK=$(echo "${CURL}" | $JQ -r '.link')
TITLE=$(echo "${CURL}" | $JQ -r '.title.rendered')
IMAGE=$(base64 -w 0 <(curl --silent "$IMG_URL"))
echo "| image=$IMAGE"
echo ---
echo "$TITLE | size=14 href='$LINK'"
echo ---
echo "Refresh... | refresh=true"
