PKGNAME="grub"
VERSION="2.12"

build() {
    echo Building $PKGNAME $VERSION

    # Check if ./configure was already ran
    if [ ! -f Makefile ]; then
        ./configure --prefix=$CHIMERA_SRC_ROOT/iso
    fi

    make -j8
}

clean() {
    make distclean
}

install() {
    make install
}

# cd to directory of this package, then execute the function specified as an argument
cd "${0%/*}"
"$@"