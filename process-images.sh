FILE_COUNT=0
IMAGE_COUNT=0
FILES=`grep -rn '<xbar.image>http' ./* | cut -f1 -d : | grep -v .md | grep -v process-images.sh`
TOTAL_FILES=`echo $FILES | wc -w`;
echo "The total number of files to process: $TOTAL_FILES"
for FILE in `echo $FILES`; do
    echo "Processing: $FILE";
    URLS=`grep xbar.image $FILE | sed -E -r 's/(.*)\<xbar.image\>(.*)\<\/xbar.image\>(.*)/\2/'`
    # echo $URLS;
    for URL in `echo $URLS`; do
        # echo $URL;
        FILENAME=`echo $URL | rev | cut -f1 -d '/' | rev`
        wget -q --timeout=15 $URL;
        if test -f "./$FILENAME"; then
            IMAGE_BASE64=`base64 ./$FILENAME`;
            # echo $FILE;
            sed -E -r "s#$URL#$IMAGE_BASE64#g" $FILE > temp-file.txt;
            if test -s "./temp-file.txt"; then
                mv temp-file.txt $FILE;
            else
                sed -n '/\<xbar.image>/q;p' $FILE > temp-file.txt;
                echo "# <xbar.image>$IMAGE_BASE64</xbar.image>" >> temp-file.txt;
                OFFSET=`wc -l ./temp-file.txt | awk {'print $1'}`;
                tail -n +$OFFSET $FILE | tail -n +2 >> temp-file.txt;
                mv temp-file.txt $FILE;
            fi
            rm $FILENAME;
            ((IMAGE_COUNT=IMAGE_COUNT+1))
        fi
    done
    ((FILE_COUNT=FILE_COUNT+1))
done
printf 'Files updated: '
echo $FILE_COUNT;
printf 'Images updated: '
echo $IMAGE_COUNT;
