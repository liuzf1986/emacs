
rc=`which rc`
compiler=`which $0`"-rtags"

if [ -x $compiler ]
	$compiler $@
fi

if [ -x $rc ]
	[ -n "$RTAGS_SERVER_FILE" ] && RTAGS_ARGS="$RTAGS_ARGS -n$RTAGS_SERVER_FILE"
    [ -n "$RTAGS_PROJECT" ] && RTAGS_ARGS="$RTAGS_ARGS --project-root=$RTAGS_PROJECT"
    [ -z "$RTAGS_COMPILE_TIMEOUT" ] && RTAGS_COMPILE_TIMEOUT=3000

	if [ -z "$RTAGS_DISABLED" ] && [ -x "$rc" ]; then
        $rc --timeout=$RTAGS_COMPILE_TIMEOUT $RTAGS_ARGS --silent --compile "$0" "$@" &
        disown &>/dev/null # rc might be finished by now and if so disown will yell at us
    fi
fi

#echo "${@/-march=/}"
