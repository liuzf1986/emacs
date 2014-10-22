#!/bin/bash
# 用于自动将ndk的

#$ ln -s /path/to/rtags/bin/gcc-rtags-wrapper.sh /somewhere/that/is/in/your/path/before/usr/bin/gcc
#$ ln -s /path/to/rtags/bin/gcc-rtags-wrapper.sh /somewhere/that/is/in/your/path/before/usr/bin/c++
#$ ln -s /path/to/rtags/bin/gcc-rtags-wrapper.sh /somewhere/that/is/in/your/path/before/usr/bin/cc
#$ ln -s /path/to/rtags/bin/gcc-rtags-wrapper.sh /somewhere/that/is/in/your/path/before/usr/bin/g++

WRAPDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
NDKDIR=" $( dirname $( which ndk-build) )"
ARCHARM="arm"


if [ "$2" = "arm" ] 
then
	if [ "$3" = "4.8" ]; then
		LBINDIR="$NDKDIR/toolchains/arm-linux-androideabi-4.8/prebuilt/linux-x86_64/arm-linux-androideabi/bin"
		RBINDIR="$NDKDIR/toolchains/arm-linux-androideabi-4.8/prebuilt/linux-x86_64/bin"
	elif [ "$3" = "4.6" ]; then
		LBINDIR="$NDKDIR/toolchains/arm-linux-androideabi-4.6/prebuilt/linux-x86_64/arm-linux-androideabi/bin"
		RBINDIR="$NDKDIR/toolchains/arm-linux-androideabi-4.6/prebuilt/linux-x86_64/bin"
	fi
fi

if [ "$1" = "setup" ]; then
	mv "$LBINDIR/gcc" "$LBINDIR/gcc.bak"
	mv "$LBINDIR/c++" "$LBINDIR/c++.bak"
	mv "$LBINDIR/cc" "$LBINDIR/cc.bak"
	mv "$LBINDIR/g++" "$LBINDIR/g++.bak"

	ln -s "$WRAPDIR/gcc-rtags-wrapper.sh" "$LBINDIR/gcc"
	ln -s "$WRAPDIR/gcc-rtags-wrapper.sh" "$LBINDIR/c++"
	ln -s "$WRAPDIR/gcc-rtags-wrapper.sh" "$LBINDIR/cc"
	ln -s "$WRAPDIR/gcc-rtags-wrapper.sh" "$LBINDIR/g++"
	
elif [ "$1" = "clean" ]; then
	echo "cleaning"
fi
