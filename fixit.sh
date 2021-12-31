#!/usr/bin/env sh
#: A template for making a change to a group of files,
#: previewing each change before it is made.

force=
#bflag=

function modify () {
    sed '
s/UI.UIDouble/UI.Double/g
s/UIDouble/UI.Double/g
s/NN.NNDouble/NN.Double/g
s/NNDouble/NN.Double/g
s/PM1.PM1Double/PM1.Double/g
s/PM1Double/PM1.Double/g
s/toUI.Doubles/toUIDoubles/g
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
