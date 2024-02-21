PKGNAME="grub"
VERSION="2.12"

build() {
    echo Building $PKGNAME $VERSION

    # Check if ./configure was already ran
    if [ ! -f Makefile ]; then
        ./configure
    fi

    make -j8
}

clean() {
    make distclean
}

# cd to directory of this package, then execute the function specified as an argument
cd "${0%/*}"
"$@"