PKGNAME="initializer"
VERSION="0.0.0"

build() {
    echo Building $PKGNAME $VERSION

    make -j8
}

clean() {
    make clean
}

install() {
    make install
}

# cd to directory of this package, then execute the function specified as an argument
cd "${0%/*}"
"$@"