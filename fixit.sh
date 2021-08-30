#!/usr/bin/env sh
#: A template for making a change to a group of files,
#: previewing each change before it is made.

force=
#bflag=

function modify () {
    sed '
s/UnitInterval/ConstrainedDouble/g
s/uiToDouble/toDouble/g
s/doubletoUI/fromDouble/g
s/forceDoubletoUI/force/g
s/uiApply/apply/g
s/adjustUIDouble/adjust/g
s/uiDiff/diff/g
s/uiVectorDiff/vectorDiff/g
s/adjustUIVector/adjustVector/g
s/adjustUIVectorPreserveLength/adjustVectorPreserveLength/g
s/uiDoublesTo8BitHex/to8BitHexes/g
s/uiDoubleTo8BitHex/to8BitHex/g
s/PlusMinusOne/ConstrainedDouble/g
s/pm1ToDouble/toDouble/g
s/doubleToPM1/fromDouble/g
s/forceDoubleToPM1/force/g
s/pm1Apply/apply/g
s/adjustPM1Double/adjust/g
s/pm1Diff/diff/g
s/pm1VectorDiff/vectorDiff/g
s/adjustPM1Vector/adjustVector/g
s/adjustPM1VectorPreserveLength/adjustVectorPreserveLength/g
s/pm1DoublesTo8BitHex/to8BitHexes/g
s/pm1DoubleTo8BitHex/to8BitHex/g
'
}

while getopts ab: name
do
    case $name in
    a)    force=1;;
#    b)    bflag=1
#         bval="$OPTARG";;
    ?)    printf "Usage: %s: [-a] [-b value] args\n" $0
          exit 2;;
    esac
done

if [ ! -z "$force" ]; then
    printf "Option -a specified\n"
fi
if [ ! -z "$bflag" ]; then
    printf 'Option -b "%s" specified\n' "$bval"
fi
#shift $(($OPTIND - 1))
#printf "Remaining arguments are: %s\n" "$*"

for file in `find . -name '*.hs'`
do
	echo Editing $file
#	sed 's/aaaaa/bbbbb/' $file > $file.backup
	modify < $file > $file.backup
	diff $file $file.backup
	result=$?
	if [ $result -gt 0 ] ; then 
		if [ ! -z "$force" ]; then
		    answer="y"
		else
		    read -p "ok (y/n/A) " answer
		    echo "answer=" $answer
		    if [ "$answer" = "A" ] ; then
			force=1
		    fi
		fi
		if [ "$answer" = "y" ] ; then 
			cp $file.backup $file
			echo File replaced
		fi
	else
		rm $file.backup
	fi
done
