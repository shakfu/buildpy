#!/usr/bin/env python3
"""get_numpy.py - builds python from source

features:

- Single script which downloads, builds numpy from source
- Trims numpy builds and zips the result

"""
import argparse
import datetime
import json
import logging
import os
import platform
import shutil
import stat
import subprocess
import sys
import sysconfig
import tarfile
import time
from fnmatch import fnmatch
from pathlib import Path
from typing import Callable, Optional, Union
from urllib.request import urlretrieve

__version__ = "0.0.1"

# ----------------------------------------------------------------------------
# type aliases

Pathlike = Union[str, Path]
MatchFn = Callable[[Path], bool]
ActionFn = Callable[[Path], None]

# ----------------------------------------------------------------------------
# env helpers

def getenv(key: str, default: bool = False) -> bool:
    """convert '0','1' env values to bool {True, False}"""
    return bool(int(os.getenv(key, default)))

def setenv(key: str, default: str):
    """get environ variable if it is exists else set default"""
    if key in os.environ:
        return os.getenv(key, default)
    else:
        os.environ[key] = default
        return default

# ----------------------------------------------------------------------------
# constants

PYTHON = sys.executable
PLATFORM = platform.system()
ARCH = platform.machine()
PY_VER_MINOR = sys.version_info.minor
if PLATFORM == "Darwin":
    MACOSX_DEPLOYMENT_TARGET = setenv("MACOSX_DEPLOYMENT_TARGET", "12.6")
DEFAULT_PY_VERSION = "3.12.2"
DEBUG = getenv('DEBUG', default=True)
COLOR = getenv('COLOR', default=True)

# ----------------------------------------------------------------------------
# logging config

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
    fmt = "%(delta)s - %(levelname)s - %(name)s.%(funcName)s - %(message)s"
    cfmt = (f"{white}%(delta)s{reset} - "
            f"{{}}%(levelname)s{{}} - "
            f"{white}%(name)s.%(funcName)s{reset} - "
            f"{grey}%(message)s{reset}")

    FORMATS = {
        logging.DEBUG: cfmt.format(grey, reset),
        logging.INFO: cfmt.format(green, reset),
        logging.WARNING: cfmt.format(yellow, reset),
        logging.ERROR: cfmt.format(red, reset),
        logging.CRITICAL: cfmt.format(bold_red, reset),
    }

    def __init__(self, use_color=COLOR):
        self.use_color = use_color

    def format(self, record):
        """custom logger formatting method"""
        if not self.use_color:
            log_fmt = self.fmt
        else:
            log_fmt = self.FORMATS.get(record.levelno)
        if PY_VER_MINOR > 10:
            duration = datetime.datetime.fromtimestamp(
                record.relativeCreated / 1000, datetime.UTC
            )
        else:
            duration = datetime.datetime.utcfromtimestamp(
                record.relativeCreated / 1000)
        record.delta = duration.strftime("%H:%M:%S")
        formatter = logging.Formatter(log_fmt)
        return formatter.format(record)


strm_handler = logging.StreamHandler()
strm_handler.setFormatter(CustomFormatter())
# file_handler = logging.FileHandler("log.txt", mode='w')
# file_handler.setFormatter(CustomFormatter(use_color=False))
logging.basicConfig(
    level=logging.DEBUG if DEBUG else logging.INFO,
    handlers=[strm_handler],
    # handlers=[strm_handler, file_handler],
)



# ----------------------------------------------------------------------------
# utility classes

class ShellCmd:
    """Provides platform agnostic file/folder handling."""

    log: logging.Logger

    def cmd(self, shellcmd: str, cwd: Pathlike = "."):
        """Run shell command within working directory"""
        self.log.info(shellcmd)
        try:
            subprocess.check_call(shellcmd, shell=True, cwd=str(cwd))
        except subprocess.CalledProcessError:
            self.log.critical("", exc_info=True)
            sys.exit(1)

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

    def fail(self, msg, *args):
        """exits the program with an error msg."""
        self.log.critical(msg, *args)
        sys.exit(1)

    def git_clone(
        self,
        url: str,
        branch: Optional[str] = None,
        directory: Optional[str] = None,
        recurse: bool = False,
        cwd: Pathlike = ".",
    ):
        """git clone a repository source tree from a url"""
        _cmds = ["git clone --depth 1"]
        if branch:
            _cmds.append(f"--branch {branch}")
        if recurse:
            _cmds.append("--recurse-submodules --shallow-submodules")
        _cmds.append(url)
        if directory:
            _cmds.append(str(directory))
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
            if PY_VER_MINOR < 11:
                if func not in (os.unlink, os.rmdir) or exc_info[1].winerror != 5:
                    raise exc_info[1]
            else:
                if func not in (os.unlink, os.rmdir) or exc_info.winerror != 5:
                    raise exc_info
            os.chmod(path, stat.S_IWRITE)
            func(path)


        path = Path(path)
        if path.is_dir():
            if not silent:
                self.log.info("remove folder: %s", path)
            if PY_VER_MINOR < 11:
                shutil.rmtree(path, ignore_errors=not DEBUG, onerror=remove_readonly)
            else:
                shutil.rmtree(path, ignore_errors=not DEBUG, onexc=remove_readonly)
        else:
            if not silent:
                self.log.info("remove file: %s", path)
            try:
                path.unlink()
            except FileNotFoundError:
                if not silent:
                    self.log.warning("file not found: %s", path)

    def walk(
        self,
        root: Pathlike,
        match_func: MatchFn,
        action_func: ActionFn,
        skip_patterns: list[str],
    ):
        """general recursive walk from root path with match and action functions"""
        for root_, dirs, filenames in os.walk(root):
            _root = Path(root_)
            if skip_patterns:
                for skip_pat in skip_patterns:
                    if skip_pat in dirs:
                        dirs.remove(skip_pat)
            for _dir in dirs:
                current = _root / _dir
                if match_func(current):
                    action_func(current)

            for _file in filenames:
                current = _root / _file
                if match_func(current):
                    action_func(current)

    def glob_remove(self, root: Pathlike, patterns: list[str], skip_dirs: list[str]):
        """applies recursive glob remove using a list of patterns"""

        def match(entry: Path) -> bool:
            # return any(fnmatch(entry, p) for p in patterns)
            return any(fnmatch(entry.name, p) for p in patterns)

        def remove(entry: Path):
            self.remove(entry)

        self.walk(root, match_func=match, action_func=remove,
                  skip_patterns=skip_dirs)

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

# ----------------------------------------------------------------------------
# main classes

class Project(ShellCmd):
    """Utility class to hold project directory structure"""

    def __init__(self):
        self.cwd = Path.cwd()
        self.build = self.cwd / "build"
        self.src = self.build / "src"
        self.log = logging.getLogger('Project')

    def setup(self):
        """create main project directories"""
        self.build.mkdir(exist_ok=True)
        self.src.mkdir(exist_ok=True)

    def reset(self):
        """prepare project for a rebuild"""
        self.remove(self.build)

def get_numpy(version='2.0.0'):
    np_ver = int(version[0])
    py_ver = sysconfig.get_config_var('py_version_short')
    p = Project()
    if p.build.exists():
        p.reset()
    p.setup()
    p.git_clone("https://github.com/numpy/numpy.git", recurse=True, branch=f'v{version}', cwd=p.src)
    src_np = p.src / 'numpy'
    venv = src_np / 'venv'
    p.cmd('virtualenv venv', cwd=src_np)
    os.system(f'cd {src_np} && source venv/bin/activate && pip install .')
    venv_np = venv / 'lib' / f'python{py_ver}' / 'site-packages' / 'numpy'
    p.glob_remove(venv_np, patterns=["tests"], skip_dirs=[".git"])
    p.glob_remove(venv_np, patterns=[
        "__pycache__",
        "*.pyi",
        "*.pxd",
        "*.pyx",
    ], skip_dirs=[".git"])
    subdirs = [
        "random/lib",
        "random/_examples",
        "f2py",
        "_pyinstaller",
        "testing",
        "typing",
    ]
    if np_ver >= 2:
        subdirs.extend([
            "_core/include",
            "_core/lib",
        ])
    else:
        subdirs.extend([
           "array_api",
            "core/include",
            "core/lib",
        ])        
    for subdir in subdirs:
        target_dir = venv_np / subdir
        shutil.rmtree(target_dir)
    ts = datetime.datetime.fromtimestamp(time.time()).strftime('%y%m%d%H%M%S')
    shutil.make_archive(f'numpy-{version}-{ts}', 'zip', root_dir=venv_np.parent, base_dir=venv_np.relative_to(venv_np.parent))


    
if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='retrieve an build a version of numpy')
    parser.add_argument('-v', '--version', default='2.0.0', help='semantic version of numpy')
    args = parser.parse_args()
    get_numpy(args.version)
