build() {
    ./configure
    make -j8
}

clean() {
    make distclean
}

# cd to directory of this package, then execute the function specified as an argument
cd "${0%/*}"
"$@"