mkdir -p tmp build

rm -f tmp/* 

echo Compiling
wla-z80 -I src -o tmp/columns.o src/columns.asm

rm -f build/*

echo Linking
wlalink -d -S -b linkfile build/columns.sms

# Fix for bank number
sed -i 's/00:\([4-7][0-9a-f]\{3\}\)/01:\1/' build/columns.sym

if sha1sum --status -c <<<"3d16b0954b5419b071de270b44d38fc6570a8439 *build/columns.sms"; then
    echo "Ok!"
    exit
fi

echo "Diff :/"
exit 1
