#
#  Installs latest ocaml + llvm  

sudo apt-get install git unzip build-essential libx11-dev cmake libxml2-dev libgmp3-dev m4 libffi-dev libzmq3-dev libssl-dev

mkdir -p install
mkdir -p sources
export INSTALLDIR=$(readlink -f install)
export SOURCESDIR=$(readlink -f sources)

source env.sh

cd $SOURCESDIR
    git clone https://github.com/ocaml/opam.git
    cd opam
        git checkout 1.2
        make cold
        make install DESTDIR=$INSTALLDIR
    cd ..
cd ..

echo "n" | $INSTALLDIR/usr/local/bin/opam init --root=$INSTALLDIR/opam --comp=4.02.1

cat > env.sh << "EOF"
MY_PATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
. $MY_PATH/install/opam/opam-init/init.sh > /dev/null 2> /dev/null || true
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$MY_PATH/install/lib
export PATH=$PATH:$MY_PATH/install/bin:$MY_PATH/install/usr/local/bin/
EOF

source env.sh

#This doesnt work yet ... 

#
#git clone https://github.com/ocaml/camlp4.git
#cd camlp4
#./configure -prefix $INSTALLDIR
#make all 
#make install
#cd ..

#git clone https://github.com/ocaml/opam.git
#cd opam
#PATH=$INSTALLDIR/bin:$PATH ./configure --prefix=$INSTALLDIR 
#PATH=$INSTALLDIR/bin:$PATH make lib-ext
#cd -

#opam init
#opam install iocaml


