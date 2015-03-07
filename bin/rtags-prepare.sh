# /bin/bash

shellpath=`dirname $0`"/../package/rtags/bin/"

mkdir ~/bin
ln -s $shellpath"gcc-rtags-wrapper.sh" ~/bin/cc
ln -s $shellpath"gcc-rtags-wrapper.sh" ~/bin/c++
ln -s $shellpath"gcc-rtags-wrapper.sh" ~/bin/gcc
ln -s $shellpath"gcc-rtags-wrapper.sh" ~/bin/g++
