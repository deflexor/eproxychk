#!/bin/sh

#
# Giving smiles application FastCGI runner
#
APP_NAME=eproxychk
APP_PATH=/home/dfr/coding/erlang/eproxycheck

PID=`ps ax -o pid= -o command=|grep "[b]eam"|grep $APP_NAME|awk '{print $1}'`

case $1 in
    start)
        if [ "X$PID" != "X" ]; then
            echo "$APP_NAME seem already running, pid: '$PID'"
            exit 0
        fi

        echo -n "Starting $APP_NAME..."
        cd $APP_PATH
        erl -pa ebin edit deps/*/ebin -boot start_sasl \
            -detached \
            -sasl sasl_error_logger "{file, \"priv/log/app.log\"}" \
            -sasl errlog_type "all" \
            -sname $APP_NAME \
            -mnesia dir "db" \
            -s $APP_NAME

        # Wait for the app to start
        TIMEOUT=5
	while [ ! -n $PID ]; do
            PID=`ps ax -o pid= -o command=|grep "[b]eam"|awk '{print $1}'`
            echo -n '.'; sleep 1;
            TIMEOUT=$((TIMEOUT - 1));
            if [ $TIMEOUT -eq 0 ]; then
                echo " ERROR: TIMED OUT"; exit 0;
            fi;
        done
        echo " started."
        ;;

    stop)
        echo -n "Stopping $APP_NAME: "
        if [ "X$PID" != "X" ]; then
            echo -n "killing $PID... "
            kill $PID
            echo "done."
        else
            echo "$APP_NAME not running."
        fi
        ;;

    restart|force-reload)
        $0 stop
        echo -n "A necessary sleep... "; sleep 1; echo "done."
        $0 start
        ;;

    *)
        echo "Usage: $0 { stop | start | restart }"
        exit 1
        ;;
esac
