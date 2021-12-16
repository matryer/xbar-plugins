#!/bin/bash

# <xbar.title>Docker disk usage</xbar.title>
# <xbar.version>v0.0.1</xbar.version>
# <xbar.author>Horatiu Ion</xbar.author>
# <xbar.author.github>Link512</xbar.author.github>
# <xbar.desc>Shows docker disk usage (via docker system df).</xbar.desc>
# <xbar.dependencies>bash,docker</xbar.dependencies>
#
# Docker disk usage plugin
#
# Displays the total disk usage of docker. Clicking the menu will show a detailed breakdown (images, containers, volumes, build cache)

export PATH="/usr/local/bin:/usr/bin:$PATH"
DOCKER="$(command -v docker)"

humanize() {
    local L_BYTES="${1:-0}"
    local L_BASE="${2:-1000}"
    echo "$1" | awk -v bytes="${L_BYTES}" -v base="${L_BASE}" 'function human(x, base) {
         if(base!=1024)base=1000
         basesuf=(base==1024)?"iB":"B"

         s="BKMGTEPYZ"
         while (x>=base && length(s)>1)
               {x/=base; s=substr(s,2)}
         s=substr(s,1,1)

         xf=((s=="B")?"%d":"%.2f")
         s=(s!="B") ? (s basesuf) : s

         return sprintf( (xf " %s\n"), x, s)
      }
      BEGIN{print human(bytes, base)}'
}

dehumanise() {
    read -r v
    echo "$v" | awk \
        'BEGIN{IGNORECASE = 1}
        function printpower(n,b,p) {printf "%d\n", n*b^p}
        /[0-9](B)?$/{ printpower($1, 10,  1); next;};
        /K(B)?$/{     printpower($1, 10,  3); next;};
        /M(B)?$/{     printpower($1, 10,  6); next;};
        /G(B)?$/{     printpower($1, 10,  9); next;};
        /T(B)?$/{     printpower($1, 10, 12); next;};'
}

DOCKER_DF=$(${DOCKER} system df --format "{{.Size}}")

IMAGE_SIZES=$(echo "${DOCKER_DF}" | sed -n 1p | dehumanise)
CONTAINER_SIZES=$(echo "${DOCKER_DF}" | sed -n 2p | dehumanise)
VOLUME_SIZES=$(echo "${DOCKER_DF}" | sed -n 3p | dehumanise)
BUILD_SIZES=$(echo "${DOCKER_DF}" | sed -n 4p | dehumanise)

TOTAL=$((IMAGE_SIZES + CONTAINER_SIZES + VOLUME_SIZES + BUILD_SIZES))

echo "🐳: $(humanize ${TOTAL})"
echo "---"
echo "images: $(echo "${DOCKER_DF}" | sed -n 1p)"
echo "containers: $(echo "${DOCKER_DF}" | sed -n 2p)"
echo "volumes: $(echo "${DOCKER_DF}" | sed -n 3p)"
echo "build cache: $(echo "${DOCKER_DF}" | sed -n 4p)"
echo "---"
echo "Refresh | refresh=true"
