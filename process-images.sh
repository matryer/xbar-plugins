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
            IMAGE_BASE64=`base64 ./$FILENAME`
            # echo $FILE;
            sed -E -r "s#$URL#$IMAGE_BASE64#g" $FILE > temp-file.txt;
            mv temp-file.txt $FILE;
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
