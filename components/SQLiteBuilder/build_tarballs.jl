# Note that this script can accept some limited command-line arguments, run
# `julia build_tarballs.jl --help` to see a usage message.
using BinaryBuilder

name = "SQLite"
version = v"3.28.0"

# Collection of sources required to build SQLite
sources = [
    "https://www.sqlite.org/2019/sqlite-autoconf-3280000.tar.gz" =>
    "d61b5286f062adfce5125eaf544d495300656908e61fca143517afcc0a89b7c3",
]

# Bash recipe for building across all platforms
script = raw"""
cd $WORKSPACE/srcdir
cd sqlite-autoconf-3280000/
./configure --prefix=$prefix --host=$target && make && make install
"""

# These are the platforms we will build for by default, unless further
# platforms are passed in on the command line
platforms = BinaryBuilder.supported_platforms()

# The products that we will ensure are always built
products(prefix) = [
    LibraryProduct(prefix, "libsqlite3", :libsqlite)
]

# Dependencies that must be installed before this package can be built
dependencies = []

# Build the tarballs, and possibly a `build.jl` as well.
build_tarballs(ARGS, name, version, sources, script, platforms, products, dependencies)
