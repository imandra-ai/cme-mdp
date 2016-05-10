for f in ../../public/10-5-16-09-02-00/*.json 
do
    DN=$(dirname $f)
    BN=$(basename $f)
    BN="${BN%.*}"
    ../ocamlcode/json2bin.native -i $f -o $DN-bin/$BN.bin
    if [ $? -ne 0 ] 
    then echo $BN 
    fi 
done
