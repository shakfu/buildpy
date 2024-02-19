#!/usr/bin/env python3
"""
% python3 -m builder

usage: __main__.py [-h] [-v]  ...

builder: builds python and py-js max externals from source or other methods.

options:
  -h, --help     show this help message and exit
  -v, --version  show program's version number and exit

subcommands:
  valid subcommands

                 additional help
    dep          dependency commands
    fix          fix references and things
    help         display online help
    package      package, sign and release external
    pyjs         build pyjs externals
    python       download and build python from src
    test         run all tests

"""
from . import utils
from .cli import Commander, option, option_group
from .ext.relocatable_python import relocatable_options, fix_framework

# from .factory import builder_factory
from .factory import FactoryManager
from .package import PackageManager
from .config import Project

# ----------------------------------------------------------------------------
# Commandline interface

common_options = option_group(
    option(
        "-p",
        "--python-version",
        type=str,
        help="set required python version to download and build",
    ),
    option(
        "-d", "--download", action="store_true", help="download python build/downloads"
    ),
    option("-r", "--reset", action="store_true", help="reset python build"),
    option("-i", "--install", action="store_true", help="install python to build/lib"),
    option("-b", "--build", action="store_true", help="build python in build/src"),
    option("-c", "--clean", action="store_true", help="clean python in build/src"),
    option("-z", "--ziplib", action="store_true", help="zip python library"),
    option("--dump", action="store_true", help="dump project and product vars"),
    option("--release", action="store_true", help="set configuration to release"),
)

# combined_options = common_options + relocatable_options


class Application(Commander):
    """builder: builds python and py-js max externals from source or other methods."""

    name = "builder"
    epilog = ""
    version = "0.1"
    default_args = ["--help"]
    _argparse_levels = 1

    def __init__(self):
        self.factory_mgr = FactoryManager()

    def ordered_dispatch(self, name, args):
        """generic ordered argument dispatcher"""
        order = ["dump", "download", "install", "build", "clean", "ziplib"]
        kwdargs = vars(args)
        builder = self.factory_mgr.builder_factory(name, **kwdargs)
        if args.dump:
            builder.to_yaml()
        for method in order:
            if method in kwdargs and kwdargs[method]:
                getattr(builder, method)()

    # ----------------------------------------------------------------------------
    # python builder methods

    def do_python(self, args):
        "download and build python from src"

    @common_options
    def do_python_static(self, args):
        """build static python"""
        self.ordered_dispatch("python_static", args)

    @common_options
    def do_python_shared(self, args):
        """build shared python"""
        self.ordered_dispatch("python_shared", args)

    @common_options
    def do_python_shared_ext(self, args):
        """build shared python to embed in external"""
        self.ordered_dispatch("python_shared_ext", args)

    @common_options
    def do_python_shared_pkg(self, args):
        """build shared python to embed in package"""
        self.ordered_dispatch("python_shared_pkg", args)

    @common_options
    def do_python_framework(self, args):
        """build framework python"""
        self.ordered_dispatch("python_framework", args)

    @common_options
    def do_python_framework_ext(self, args):
        """build framework python to embed external"""
        self.ordered_dispatch("python_framework_ext", args)

    @common_options
    def do_python_framework_pkg(self, args):
        """build framework python to embed in a package"""
        self.ordered_dispatch("python_framework_pkg", args)

    @common_options
    def do_python_cmake(self, args):
        """download and build python using cmake"""
        # TODO: add cmake-specific options
        self.ordered_dispatch("python_cmake", args)

    @option("--dump", action="store_true", help="dump project and product vars")
    @option("-i", "--install", action="store_true", help="install python to build/lib")
    @relocatable_options
    def do_python_relocatable(self, args):
        """download relocatable framework python"""
        self.ordered_dispatch("python_relocatable", args)

    @common_options
    def do_python_static_tiny(self, args):
        """build tiny static python"""
        self.ordered_dispatch("python_static_tiny", args)

    @common_options
    def do_python_shared_tiny(self, args):
        """build tiny shared python"""
        self.ordered_dispatch("python_shared_tiny", args)

    @common_options
    def do_python_beeware(self, args):
        """build beeware python framework"""
        self.ordered_dispatch("python_beeware", args)

    # ----------------------------------------------------------------------------
    # dependency builder methods

    def do_dep(self, args):
        """dependency commands"""

    def do_dep_bz2(self, args):
        """build bzip2 dependency"""
        self.factory_mgr.builder_factory("bz2").build()

    def do_dep_ssl(self, args):
        """build openssl dependency"""
        self.factory_mgr.builder_factory("ssl").build()

    def do_dep_xz(self, args):
        """build xz dependency"""
        self.factory_mgr.builder_factory("xz").build()

    # ----------------------------------------------------------------------------
    # help methods

    def do_help(self, args):
        """display online help"""
        utils.display_help()


    # ----------------------------------------------------------------------------
    # package management methods

    @option("-i", "--dev-id", help="Developer ID")
    @option("-k", "--keychain-profile", help="Keychain Profile")
    @option("-d", "--dry-run", action="store_true", help="run without actual changes.")
    @option("-v", "--variant", help="build variant name")
    def do_package(self, args):
        """package, sign and release external"""
        mgr = PackageManager(
            args.variant, args.dev_id, args.keychain_profile, args.dry_run
        )
        mgr.process()

    def do_package_sign(self, args):
        """sign all required folders recursively"""
        mgr = PackageManager()
        mgr.sign_all()

    def do_package_dist(self, args):
        """create project distribution folder"""
        mgr = PackageManager()
        mgr.create_dist()

    def do_package_dmg(self, args):
        """package distribution folder as .dmg"""
        mgr = PackageManager()
        mgr.package_as_dmg()

    def do_package_sign_dmg(self, args):
        """sign dmg"""
        mgr = PackageManager()
        mgr.sign_dmg()

    def do_package_notarize_dmg(self, args):
        """notarize dmg"""
        mgr = PackageManager()
        mgr.notarize_dmg()

    def do_package_staple_dmg(self, args):
        """staple dmg"""
        mgr = PackageManager()
        mgr.staple_dmg()

    def do_package_collect_dmg(self, args):
        """collect dmg"""
        mgr = PackageManager()
        mgr.collect_dmg()


if __name__ == "__main__":
    app = Application()
    app.cmdline()
