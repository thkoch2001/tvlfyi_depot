# js function syntax for bash

function trimend {
    LENGTH=${#1}
    AMT=$2
    TAKE=$((LENGTH-AMT))
    echo $1 | cut "-c-$TAKE"
}

function trimfront {
    TMP0=$(echo $1 | rev)
    TMP1=$(trimend $TMP0 $2 | rev)
    echo $TMP1
}

function length {
    echo ${#1}
}

function slice {
    STR=$3
    AMT0=$1
    AMT1=$2

    LEN=$(length $STR)
    BACK=$((LEN-AMT1))

    RES0=$(trimfront $STR $AMT0)
    RES1=$(trimend $RES0 $BACK)

    echo $RES1
}
