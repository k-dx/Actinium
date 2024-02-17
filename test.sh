#!/bin/bash

# echo "Did you remember to compile to 'a.out'?"
# echo ""

echo "Compiling..."
dune build || exit

echo "Running tests..."

for i in {0..50}
do
    OUTPUT=$(./_build/default/bin/main.exe "tests/t$i.in")
    EXPECTED=$(cat "tests/t$i.out")

    if diff -wq <(echo "$OUTPUT") <(echo "$EXPECTED")
    then
        echo "OK $i"
    else
        echo "WA $i"
        break
    fi
done

