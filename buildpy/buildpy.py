#!/usr/bin/env python3

import logging
import os
import platform
import shutil
import stat
import subprocess
import sys
import tarfile
import datetime
from pathlib import Path
from typing import Optional, Union
from urllib.request import urlretrieve

# ----------------------------------------------------------------------------
# type aliases

Pathlike = Union[str, Path]

# ----------------------------------------------------------------------------
# constants

PYTHON = sys.executable
PLATFORM = platform.system()
ARCH = platform.machine()
PY_VER_MINOR = sys.version_info.minor
if PLATFORM == "Darwin":
    MACOSX_DEPLOYMENT_TARGET = os.getenv("MACOSX_DEPLOYMENT_TARGET", "12.6")
    os.environ["MACOSX_DEPLOYMENT_TARGET"] = MACOSX_DEPLOYMENT_TARGET

DEBUG = True

# ----------------------------------------------------------------------------
# utility classes


class CustomFormatter(logging.Formatter):
    """custom logging formatting class"""

    white = "\x1b[97;20m"
    grey = "\x1b[38;20m"
    green = "\x1b[32;20m"
    cyan = "\x1b[36;20m"
    yellow = "\x1b[33;20m"
    red = "\x1b[31;20m"
    bold_red = "\x1b[31;1m"
    reset = "\x1b[0m"
    # fmt = "%(delta)s - {}%(levelname)s{} - %(name)s.%(funcName)s - %(message)s"
    fmt = f"{white}%(delta)s{reset} - {{}}%(levelname)s{{}} - {white}%(name)s.%(funcName)s{reset} - %(message)s"

    FORMATS = {
        logging.DEBUG: fmt.format(grey, reset),
        logging.INFO: fmt.format(green, reset),
        logging.WARNING: fmt.format(yellow, reset),
        logging.ERROR: fmt.format(red, reset),
        logging.CRITICAL: fmt.format(bold_red, reset),
    }

    def format(self, record):
        log_fmt = self.FORMATS.get(record.levelno)
        duration = datetime.datetime.utcfromtimestamp(record.relativeCreated / 1000)
        record.delta = duration.strftime("%H:%M:%S")
        formatter = logging.Formatter(log_fmt)
        return formatter.format(record)


handler = logging.StreamHandler()
handler.setFormatter(CustomFormatter())
logging.basicConfig(level=logging.DEBUG if DEBUG else logging.INFO, handlers=[handler])


class ShellCmd:
    """Provides platform agnostic file/folder handling."""

    log: logging.Logger

    def cmd(self, shellcmd: str, cwd: Pathlike = "."):
        """Run shell command within working directory"""
        self.log.info(shellcmd)
        subprocess.call(shellcmd, shell=True, cwd=str(cwd))

    def download(self, url: str, tofolder: Optional[Pathlike] = None) -> Pathlike:
        """Download a file from a url to an optional folder"""
        _path = Path(os.path.basename(url))
        if tofolder:
            _path = Path(tofolder).joinpath(_path)
            if _path.exists():
                return _path
        filename, _ = urlretrieve(url, filename=_path)
        return Path(filename)

    def extract(self, archive: Pathlike, tofolder: Pathlike = "."):
        """extract a tar archive"""
        if tarfile.is_tarfile(archive):
            with tarfile.open(archive) as f:
                f.extractall(tofolder)
        # elif zipfile.is_zipfile(archive):
        #     with zipfile.ZipFile(archive) as f:
        #         f.extractall(tofolder)
        else:
            raise TypeError("cannot extract from this file.")

    def fail(self, msg: Optional[str] = None, *args):
        """exits the program with an optional error msg."""
        if msg:
            self.log.critical(msg, *args)
        sys.exit(1)

    def git_clone(
        self,
        url: str,
        recurse: bool = False,
        branch: Optional[str] = None,
        cwd: Pathlike = ".",
    ):
        """git clone a repository source tree from a url"""
        _cmds = ["git clone --depth 1"]
        if branch:
            _cmds.append(f"--branch {branch}")
        if recurse:
            _cmds.append("--recurse-submodules --shallow-submodules")
        _cmds.append(url)
        self.cmd(" ".join(_cmds), cwd=cwd)

    def getenv(self, key: str, default: bool = False) -> bool:
        """convert '0','1' env values to bool {True, False}"""
        self.log.info("checking env variable: %s", key)
        return bool(int(os.getenv(key, default)))

    def chdir(self, path: Pathlike):
        """Change current workding directory to path"""
        self.log.info("changing working dir to: %s", path)
        os.chdir(path)

    def chmod(self, path: Pathlike, perm=0o777):
        """Change permission of file"""
        self.log.info("change permission of %s to %s", path, perm)
        os.chmod(path, perm)

    def get(self, shellcmd, cwd: Pathlike = ".", shell: bool = False) -> str:
        """get output of shellcmd"""
        if not shell:
            shellcmd = shellcmd.split()
        return subprocess.check_output(
            shellcmd, encoding="utf8", shell=shell, cwd=str(cwd)
        ).strip()

    def makedirs(self, path: Pathlike, mode: int = 511, exist_ok: bool = True):
        """Recursive directory creation function"""
        self.log.info("making directory: %s", path)
        os.makedirs(path, mode, exist_ok)

    def move(self, src: Pathlike, dst: Pathlike):
        """Move from src path to dst path."""
        self.log.info("move path %s to %s", src, dst)
        shutil.move(src, dst)

    def copy(self, src: Pathlike, dst: Pathlike):
        """copy file or folders -- tries to be behave like `cp -rf`"""
        self.log.info("copy %s to %s", src, dst)
        src, dst = Path(src), Path(dst)
        if src.is_dir():
            shutil.copytree(src, dst)
        else:
            shutil.copy2(src, dst)

    def remove(self, path: Pathlike, silent: bool = False):
        """Remove file or folder."""

        # handle windows error on read-only files
        def remove_readonly(func, path, exc_info):
            "Clear the readonly bit and reattempt the removal"
            if func not in (os.unlink, os.rmdir) or exc_info[1].winerror != 5:
                raise exc_info[1]
            os.chmod(path, stat.S_IWRITE)
            func(path)

        path = Path(path)
        if path.is_dir():
            if not silent:
                self.log.info("remove folder: %s", path)
            shutil.rmtree(path, ignore_errors=not DEBUG, onerror=remove_readonly)
        else:
            if not silent:
                self.log.info("remove file: %s", path)
            try:
                path.unlink()
            except FileNotFoundError:
                if not silent:
                    self.log.warning("file not found: %s", path)

    def pip_install(
        self,
        *pkgs,
        reqs: Optional[str] = None,
        upgrade: bool = False,
        pip: Optional[str] = None,
    ):
        """Install python packages using pip"""
        _cmds = []
        if pip:
            _cmds.append(pip)
        else:
            _cmds.append("pip3")
        _cmds.append("install")
        if reqs:
            _cmds.append(f"-r {reqs}")
        else:
            if upgrade:
                _cmds.append("--upgrade")
            _cmds.extend(pkgs)
        self.cmd(" ".join(_cmds))

    def apt_install(self, *pkgs, update: bool = False):
        """install debian packages using apt"""
        _cmds = []
        _cmds.append("sudo apt install")
        if update:
            _cmds.append("--upgrade")
        _cmds.extend(pkgs)
        self.cmd(" ".join(_cmds))

    def brew_install(self, *pkgs, update: bool = False):
        """install using homebrew"""
        _pkgs = " ".join(pkgs)
        if update:
            self.cmd("brew update")
        self.cmd(f"brew install {_pkgs}")

    def cmake_config(self, src_dir: Pathlike, build_dir: Pathlike, *scripts, **options):
        """activate cmake configuration / generation stage"""
        _cmds = [f"cmake -S {src_dir} -B {build_dir}"]
        if scripts:
            _cmds.append(" ".join(f"-C {path}" for path in scripts))
        if options:
            _cmds.append(" ".join(f"-D{k}={v}" for k, v in options.items()))
        self.cmd(" ".join(_cmds))

    def cmake_build(self, build_dir: Pathlike, release: bool = False):
        """activate cmake build stage"""
        _cmd = f"cmake --build {build_dir}"
        if release:
            _cmd += " --config Release"
        self.cmd(_cmd)

    def cmake_install(self, build_dir: Pathlike, prefix: Optional[str] = None):
        """activate cmake install stage"""
        _cmds = ["cmake --install", str(build_dir)]
        if prefix:
            _cmds.append(f"--prefix {prefix}")
        self.cmd(" ".join(_cmds))

    def install_name_tool(self, src: Pathlike, dst: Pathlike, mode: str = "id"):
        """change dynamic shared library install names"""
        _cmd = f"install_name_tool -{mode} {src} {dst}"
        self.log.info(_cmd)
        self.cmd(_cmd)

# ----------------------------------------------------------------------------
# config class


# ----------------------------------------------------------------------------
# main classes

class Project:
    """Utility class to hold project directory structure"""

    def __init__(self):
        self.cwd = Path.cwd()
        self.build = self.cwd / "build"

        self.downloads = self.build / "downloads"
        self.src = self.build / "src"
        self.install = self.build / "install"

        self.bin = self.install / "bin"
        self.include = self.install / "include"
        self.lib = self.install / "lib"
        self.lib_static = self.lib / "static"
        self.share = self.install / "share"

        self.scripts = self.cwd / "scripts"
        self.patch = self.cwd / "patch"
        self.tests = self.cwd / "tests"
        self.dist = self.cwd / "dist"
        self.wheels = self.cwd / "wheels"

    def setup(self):
        self.build.mkdir(exist_ok=True)
        self.downloads.mkdir(exist_ok=True)
        self.install.mkdir(exist_ok=True)
        self.src.mkdir(exist_ok=True)


class AbstractBuilder(ShellCmd):
    """Abstract builder class with additional methods common to subclasses."""

    name: str
    version: str
    url_template: str
    libs_static: list[str]
    depends_on: list[type['Builder']]

    def __init__(
        self, version: Optional[str] = None, project: Optional[Project] = None
    ):
        self.version = version or self.version
        self.project = project or Project()
        self.log = logging.getLogger(self.__class__.__name__)

    def __repr__(self):
        return f"<{self.__class__.__name__} '{self.name}-{self.version}'>"

    def __iter__(self):
        for dependency in self.depends_on:
            yield dependency
            yield from iter(dependency)

    def setup_project(self):
        folders = [
            self.project.build,
            self.project.downloads,
        ]
        for folder in folders:
            if not folder.exists():
                self.makedirs(folder)

    @property
    def ver(self):
        return ".".join(self.version.split(".")[:2])

    @property
    def ver_major(self):
        return self.version.split(".")[0]

    @property
    def ver_minor(self):
        return self.version.split(".")[1]

    @property
    def ver_patch(self):
        return self.version.split(".")[2]

    @property
    def ver_nodot(self):
        return self.ver.replace(".", "")

    @property
    def name_version(self):
        return f"{self.name}-{self.version}"

    @property
    def name_ver(self):
        return f"{self.name.lower()}{self.ver}"

    @property
    def url(self):
        return self.url_template.format(ver=self.version)

    @property
    def name_archive(self):
        return f"{self.name_version}.tgz"

    @property
    def download_path(self):
        return self.project.downloads / self.name_archive

    @property
    def src_path(self):
        return self.project.src / self.name_version

    @property
    def build_dir(self):
        return self.src_path / "build"

    @property
    def executable_name(self):
        if PLATFORM == "Windows":
            name = f"{self.name}.exe"
        return name

    @property
    def executable(self):
        return self.project.bin / self.executable_name

    @property
    def libname(self):
        return f"lib{self.name}"

    @property
    def staticlib_name(self):
        suffix = ".a"
        if PLATFORM == "Windows":
            suffix = ".lib"
        return f"{self.libname}{suffix}"

    @property
    def dylib_name(self):
        if PLATFORM == "Darwin":
            return f"{self.libname}.dylib"
        if PLATFORM == "Linux":
            return f"{self.libname}.so"
        if PLATFORM == "Windows":
            return f"{self.NAME}.dll"
        raise self.fail("platform not supported")

    @property
    def dylib_linkname(self):
        if PLATFORM == "Darwin":
            return f"{self.libname}.dylib"
        if PLATFORM == "Linux":
            return f"{self.libname}.so"
        raise self.fail("platform not supported")

    @property
    def dylib(self):
        return self.project.lib / self.dylib_name

    @property
    def dylib_link(self):
        return self.project.lib / self.dylib_linkname

    @property
    def staticlib(self):
        return self.project.lib_static / self.staticlib_name

    @property
    def prefix(self):
        return self.project.install / self.name.lower()

    def libs_static_exist(self):
        return all((self.prefix / 'lib' / lib).exists() for lib in self.libs_static)

    def pre_process(self):
        """override by subclass if needed"""

    def setup(self):
        """setup build environment"""

    def configure(self):
        """configure build"""

    def build(self):
        """build target"""

    def install(self):
        """install target"""

    def clean(self):
        """clean build"""

    def post_process(self):
        """override by subclass if needed"""

    def process(self):
        """main builder process"""
        self.pre_process()
        self.setup()
        self.configure()
        self.build()
        self.install()
        self.clean()
        self.post_process()


class Builder(AbstractBuilder):
    """concrete builder class"""

    def setup(self):
        """setup build environment"""
        self.project.setup()
        archive = self.download(self.url, tofolder=self.project.downloads)
        self.log.info("downloaded %s", archive)
        self.extract(archive, tofolder=self.project.src)
        assert self.src_path.exists(), f"could not extract from {archive}"


class OpensslBuilder(Builder):
    name = "openssl"
    version = "1.1.1w"
    url_template = "https://www.openssl.org/source/old/1.1.1/openssl-{ver}.tar.gz"
    depends_on = []
    libs_static = ["libssl.a", "libcrypto.a"]

    def build(self):
        if not self.libs_static_exist():
            self.cmd(
                f"./config no-shared no-tests --prefix={self.prefix}", cwd=self.src_path
            )
            self.cmd("make install_sw", cwd=self.src_path)


class Bzip2Builder(Builder):
    name = "bzip2"
    version = "1.0.8"
    url_template = "https://sourceware.org/pub/bzip2/bzip2-{ver}.tar.gz"
    depends_on = []
    libs_static = ["libbz2.a"]

    def build(self):
        if not self.libs_static_exist():
            self.cmd(f"make install PREFIX={self.prefix}", cwd=self.src_path)


class XzBuilder(Builder):
    name = "xz"
    version = "5.2.5"
    url_template = "http://tukaani.org/xz/xz-{ver}.tar.gz"
    depends_on = []
    libs_static = ["liblzma.a"]

    def build(self):
        if not self.libs_static_exist():
            self.cmd(
                f"./configure --disable-shared --enable-static --prefix={self.prefix}",
                cwd=self.src_path,
            )
            self.cmd("make && make install", cwd=self.src_path)


class PythonBuilder(Builder):
    """Builds python locally"""

    name = "Python"

    version = "3.11.7"

    url_template = "https://www.python.org/ftp/python/{ver}/Python-{ver}.tar.xz"

    config_options: list[str] = [

        # "--disable-profiling",
        "--disable-test-modules",
        # "--enable-framework",
        # "--enable-framework=INSTALLDIR",
        # "--enable-ipv6",
        # "--enable-optimizations",
        # "--enable-shared",
        # "--enable-universalsdk",
        # "--enable-universalsdk=SDKDIR",
        # "--with-lto",
        # "--with-lto=thin",
        # "--with-openssl-rpath=auto",
        # "--with-openssl=DIR",
        # "--with-readline=editline",
        # "--with-system-expat",
        # "--with-system-ffi",
        # "--with-system-libmpdec",
        # "--without-builtin-hashlib-hashes",
        # "--without-doc-strings",
        "--without-ensurepip",
        # "--without-readline",
        "--without-static-libpython",
    ]

    required_packages: list[str] = []

    ignore_patterns: list[str] = [
        "*.exe",
        "*.pyc",
        "*config-3*",
        "*tcl*",
        "*tdbc*",
        "*tk*",
        "__phello__",
        "__pycache__",
        "_codecs_*.so",
        "_test*",
        "_tk*",
        "_xx*.so",
        "distutils",
        "ensurepip",
        "idlelib",
        "lib2to3",
        "libpython*",
        "LICENSE.txt",
        "pkgconfig",
        "pydoc_data",
        "site-packages",
        "test",
        "Tk*",
        "turtle*",
        "venv",
        "xx*.so",
    ]

    depends_on = [OpensslBuilder, Bzip2Builder, XzBuilder]

    def __init__(
        self,
        version: str = "3.11.7",
        project: Optional[Project] = None,
        config: Pathlike = "patch/static.local",
        optimize: bool = False,
        pkgs: Optional[list[str]] = None,
    ):

        super().__init__(version, project)
        self.config = Path(config)
        self.optimize = optimize
        self.pkgs = pkgs or [] 
        self.log = logging.getLogger(self.__class__.__name__)

    @property
    def python(self):
        return self.prefix / "bin" / "python3"

    @property
    def pip(self):
        return self.prefix / "bin" / "pip3"

    def pre_process(self):
        """override by subclass if needed"""

    def configure(self):
        """configure build"""
        if not self.config.exists():
            self.fail("configureation does not exist")

        _type, _size = self.config.name.split('.')
        if _type == 'static':
            self.config_options.remove("--without-static-libpython")
        elif _type == 'shared':
            self.config_options.append("--enable-shared")
        elif _type == "framework":
            self.config_options.append(f"--enable-framework={self.prefix}")
        else:
            self.fail(f"{_type} not recognized build type")

        if self.optimize:
            self.config_options.append("--enable-optimizations")

        if self.pkgs or self.required_packages:
            self.config_options.remove("--without-ensurepip")
            self.ignore_patterns.remove("ensurepip")
            self.pkgs.extend(self.required_packages)

        self.copy(self.config, self.src_path / "Modules" / "Setup.local")
        config_opts = " ".join(self.config_options)
        self.cmd(f"./configure --prefix={self.prefix} {config_opts}", cwd=self.src_path)

    def build(self):
        self.cmd("make", cwd=self.src_path)

    def install(self):
        self.cmd(f"make install", cwd=self.src_path)

    def clean(self):
        src = self.prefix / "lib" / self.name_ver
        dst = self.project.build / "cleaned"

        shutil.copytree(
            src,
            dst,
            symlinks=False,
            ignore=shutil.ignore_patterns(*self.ignore_patterns),
            ignore_dangling_symlinks=True,
            dirs_exist_ok=True,
        )
        ver = self.ver
        bins = [
            "2to3",
            "idle3",
            f"idle{ver}",
            "pydoc3",
            f"pydoc{ver}",
            f"2to3-{ver}",
        ]
        for f in bins:
            self.remove(self.prefix / "bin" / f)

    def ziplib(self):
        """zip python site-packages"""
        cleaned = self.project.build / "cleaned"
        self.move(
            cleaned / "lib-dynload",
            self.project.build / "lib-dynload",
        )
        self.move(
            cleaned / "os.py", self.project.build / "os.py"
        )

        zip_path = self.prefix / "lib" / f"python{self.ver_nodot}"
        shutil.make_archive(str(zip_path), "zip", str(cleaned))
        self.remove(cleaned)

        src = self.prefix / "lib" / self.name_ver
        site_packages = src / "site-packages"
        self.remove(self.prefix / "lib" / "pkgconfig")
        self.remove(src)
        src.mkdir()
        site_packages.mkdir()
        self.move(self.project.build / "lib-dynload", src / "lib-dynload")
        self.move(self.project.build / "os.py", src / "os.py")

    def install_pkgs(self):
        required_pkgs = " ".join(self.required_packages)
        self.cmd(f"{self.python} -m ensurepip")
        self.cmd(f"{self.pip} install {required_pkgs}")

    def post_process(self):
        """override by subclass if needed"""

    def process(self):
        for dependency_class in self.depends_on:
            dependency_class().process()
        self.pre_process()
        self.setup()
        self.configure()
        self.build()
        self.install()
        self.clean()
        self.ziplib()
        if self.pkgs:
            self.install_pkgs()
        self.post_process()


class PythonDebugBuilder(PythonBuilder):
    """Builds debug python locally"""

    name = "python"

    config_options = [
        "--enable-shared",
        "--disable-test-modules",
        "--without-static-libpython",
        "--with-pydebug",
        # "--with-trace-refs",
        # "--with-valgrind",
        # "--with-address-sanitizer",
        # "--with-memory-sanitizer",
        # "--with-undefined-behavior-sanitizer",
    ]

    required_packages = [
        "pkgconfig",
        "cython",
        "pytest",
    ]

    def post_process(self):
        memray = self.project.downloads / "memray"
        self.git_clone(
            "https://github.com/bloomberg/memray.git",
            cwd=self.project.downloads
        )
        self.cmd(f"{self.python} setup.py build", cwd=memray)
        self.cmd(f"{self.python} setup.py install", cwd=memray)


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        prog="buildpy.py",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description="A python builder",
        # epilog="epilog here",
    )
    opt = parser.add_argument

    opt("--debug", "-d", help="build debug python", action="store_true")
    opt("--version", "-v", default="3.11.7", help="python version")
    opt("--config", "-c", default="patch/static.max", help="build configuration")
    opt("--reset", "-r", help="reset build", action="store_true")
    opt("--optimize", "-o", help="optimize build", action="store_true")
    opt("--mac-dep-target", default="13.6", help="mac dep target")
    opt("--pkgs", "-p", type=str, nargs='+')

    args = parser.parse_args()
    if args.debug:
        dbuilder = PythonDebugBuilder(version=args.version)
        dbuilder.process()
    else:
        builder = PythonBuilder(
            version=args.version, 
            config=args.config, 
            optimize=args.optimize,
            pkgs=args.pkgs,
        )
        if args.reset:
            builder.remove("build")
        builder.process()




