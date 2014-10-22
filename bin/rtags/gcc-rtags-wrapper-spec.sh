#!/bin/bash

rc=`which rc`
compiler=`which $0`"-rtags"

if [ -x $rc ]; then
	[ -n "$RTAGS_SERVER_FILE" ] && RTAGS_ARGS="$RTAGS_ARGS -n$RTAGS_SERVER_FILE"
    [ -n "$RTAGS_PROJECT" ] && RTAGS_ARGS="$RTAGS_ARGS --project-root=$RTAGS_PROJECT"
    [ -z "$RTAGS_COMPILE_TIMEOUT" ] && RTAGS_COMPILE_TIMEOUT=3000

    $rc --timeout=$RTAGS_COMPILE_TIMEOUT $RTAGS_ARGS --silent --compile "$0" "${@/-march=armv5te/}" &
#    disown &>/dev/null # rc might be finished by now and if so disown will yell at us
fi

if [ -x $compiler ]; then
	$compiler $@
fi

#echo "${@/-march=armv5te/}"
