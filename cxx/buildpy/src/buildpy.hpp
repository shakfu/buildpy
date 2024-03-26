#include <argparse/argparse.hpp>
#include <fmt/core.h>
#include <logy.h>
#include <semver.hpp>
#include <taskflow/taskflow.hpp>

#include <algorithm>
#include <initializer_list>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#define BUFFERSIZE 4096


namespace fs = std::filesystem;

class ShellCmd {
    // Utility class to hold common file operations
public:

    void cmd_exe(std::string exe, std::vector<std::string> args, fs::path dir = ".")
    {
        args.insert(args.begin(), exe);
        this->cmd(args, dir);
    }

    void cmd(std::vector<std::string> args, fs::path dir = ".")
    {
        fs::path cwd;
        if (dir != "." ) {
            cwd = fs::current_path();
            fs::current_path(dir);            
        }
        std::string _cmd = this->join(args, " ");
        Info("%s", _cmd.c_str());
        std::system(_cmd.c_str());
        if (dir != "." )
            fs:current_path(cwd);
    }

    void run(std::string shellcmd, fs::path dir = ".")
    {
        fs::path cwd;
        if (dir != "." ) {
            cwd = fs::current_path();
            fs::current_path(dir);            
        }
        Info("%s", shellcmd.c_str());
        std::system(shellcmd.c_str());        
        if (dir != "." )
            fs:current_path(cwd);
    }


    void run_list(std::initializer_list<std::string> args, fs::path dir = ".")
    {
        std::vector<std::string> vargs = std::vector(args);
        this->cmd(vargs, dir);
    }

    std::vector<std::string> split(std::string s, char delimiter)
    {
        std::vector<std::string> output;
        for (auto cur = std::begin(s), beg = cur;; ++cur) {
            if (cur == std::end(s) || *cur == delimiter || !*cur) {
                output.insert(output.end(), std::string(beg, cur));
                if (cur == std::end(s) || !*cur)
                    break;
                else
                    beg = std::next(cur);
            }
        }
        return output;
    }


    std::string join(std::vector<std::string> elements, const char* const delimiter)
    {
        std::ostringstream os;
        auto b = std::begin(elements);
        auto e = std::end(elements);

        if (b != e) {
            std::copy(b, std::prev(e), std::ostream_iterator<std::string>(os, delimiter));
            b = std::prev(e);
        }
        if (b != e) {
            os << *b;
        }

        return os.str();
    }

    void create_dir(fs::path p)
    {
        if (!fs::exists(p)) {
            fs::create_directory(p);
        }
    }

    void git_clone(std::string url, std::string branch, fs::path dir,
                   bool recurse = false)
    {
        if (fs::exists(dir)) {
            Warning("skipping git clone, dir exists: %s", dir.c_str());
            return;
        }
        std::vector<std::string> args;
        args.insert(
            args.end(),
            {"clone", "--depth=1", "-b", branch });
        if (recurse) {
            args.insert(
                args.end(),
                { "--recurse-submodules", "--shallow-submodules", url, dir });
        } else {
            args.insert(args.end(), { url, dir });
        }
        this->cmd_exe("/usr/bin/git", args);
    }

    std::vector<std::string> split(std::string s, std::string sep)
    {
        std::vector<std::string> res;
        int pos = 0;
        while (pos < s.size()) {
            pos = s.find(sep);
            res.push_back(s.substr(0, pos));
            s.erase(0,
                    pos + sep.size()); // length of the seperator, sep
        }
        return res;
    }

    std::string join(const std::vector<std::string>& lst,
                     const std::string& delim)
    {
        std::string ret;
        for (const auto& s : lst) {
            if (!ret.empty())
                ret += delim;
            ret += s;
        }
        return ret;
    }
};


class Project : public ShellCmd {
    // Utility class to hold project directory structure
public:
    // -----------------------------------------------------------------------
    // attributes

    fs::path cwd;
    fs::path build;
    fs::path downloads;
    fs::path src;
    fs::path install;

    // -----------------------------------------------------------------------
    // constructor

    Project()
    {
        this->cwd = fs::current_path();
        this->build = this->cwd / "build";
        this->downloads = this->build / "build";
        this->src = this->build / "src";
        this->install = this->build / "install";
    }

    // -----------------------------------------------------------------------
    // methods

    void setup()
    {
        // create main project directories
        this->create_dir(this->build);
        this->create_dir(this->downloads);
        this->create_dir(this->install);
        this->create_dir(this->src);
    }
};


class OpenSSLBuilder : public ShellCmd {
    // builds openssl from source
public:
    semver::version version;
    std::string name;
    std::string repo_url;
    Project project;

    // -----------------------------------------------------------------------
    // constructor

    OpenSSLBuilder(std::string version)
    {
        this->version = semver::version::parse(version);
        this->name = "openssl";
        this->repo_url = "https://github.com/openssl/openssl.git";
        this->project = Project();
    }

    // -----------------------------------------------------------------------
    // operators

    void operator()()
    {
        // can be used in taskflow
        this->process();
    }

    // -----------------------------------------------------------------------
    // properties

    fs::path src_dir() { return this->project.src / this->name; }

    fs::path build_dir() { return this->src_dir() / std::string("build"); }

    fs::path prefix() { return this->project.install / this->name; }

    fs::path staticlib() { return this->prefix() / "lib" / "libssl.a"; }

    fs::path dylib() { return this->prefix() / "lib" / "libssl.dylib"; }

    std::string repo_branch()
    {
        std::string s = this->version.str();
        std::replace(s.begin(), s.end(), '.', '_'); // replace all '.' to '_'
        return fmt::format("OpenSSL_{}w", s);
    }

    std::string download_url()
    {
        return fmt::format(
            "https://www.openssl.org/source/old/1.1.1/openssl-{}w.tar.gz",
            this->version.str());
    }

    // -----------------------------------------------------------------------
    // predicates
    
    bool libs_exist()
    {
        return fs::exists(this->staticlib()) || fs::exists(this->dylib());
    }

    // -----------------------------------------------------------------------
    // methods

    void download()
    {
        Info("OpenSSLBuilder.download()");
        this->git_clone(this->repo_url, this->repo_branch(), this->src_dir());
    }

    void build()
    {
        Info("OpenSSLBuilder.build()");
        if (!this->libs_exist()) {
            std::string _cmd = fmt::format("/bin/bash ./config no-shared no-tests --prefix={}",
                this->prefix().string());
            this->run(_cmd, this->src_dir());
            this->run("make install_sw", this->src_dir());
        }

    }

    void process()
    {
        Info("OpenSSLBuilder.process() start");
        this->download();
        this->build();
        Info("OpenSSLBuilder.process() end");
    }
};


class Bzip2Builder : public ShellCmd {
    // builds bzip2 from source
public:
    semver::version version;
    std::string name;
    std::string repo_url;
    Project project;

    // -----------------------------------------------------------------------
    // constructor

    Bzip2Builder(std::string version)
    {
        this->version = semver::version::parse(version);
        this->name = "bzip2";
        this->repo_url = "https://github.com/libarchive/bzip2.git";
        this->project = Project();
    }

    // -----------------------------------------------------------------------
    // operators

    void operator()()
    {
        // can be used in taskflow
        this->process();
    }

    // -----------------------------------------------------------------------
    // properties


    fs::path src_dir() { return this->project.src / this->name; }

    fs::path build_dir() { return this->src_dir() / std::string("build"); }

    fs::path prefix() { return this->project.install / this->name; }

    fs::path staticlib() { return this->prefix() / "lib" / "libbzip2.a"; }

    fs::path dylib() { return this->prefix() / "lib" / "libbzip2.dylib"; }

    std::string repo_branch()
    {
        return fmt::format("bzip2-{}", this->version.str());
    }

    std::string download_url()
    {
        return fmt::format("https://sourceware.org/pub/bzip2/bzip2-{}.tar.gz",
                           this->version.str());
    }

    // -----------------------------------------------------------------------
    // predicates
    
    bool libs_exist()
    {
        return fs::exists(this->staticlib()) || fs::exists(this->dylib());
    }

    // -----------------------------------------------------------------------
    // methods

    void download()
    {
        Info("Bzip2Builder.download source code");
        this->git_clone(this->repo_url, this->repo_branch(), this->src_dir());
    }

    void build()
    {
        Info("Bzip2Builder.build()");
        if (!this->libs_exist()) {
            std::string _cmd = fmt::format("make install CFLAGS='-fPIC' PREFIX={}",
                this->prefix().string());
            this->run(_cmd, this->src_dir());
        }
    }

    void process()
    {
        Info("Bzip2Builder process starting");
        this->download();
        this->build();
        Info("Bzip2Builder process ending");
    }
};


class XzBuilder : public ShellCmd {
    // builds xz from source
public:
    semver::version version;
    std::string name;
    std::string repo_url;
    Project project;

    // -----------------------------------------------------------------------
    // constructor

    XzBuilder(std::string version)
    {
        this->version = semver::version::parse(version);
        this->name = "xz";
        this->repo_url = "https://github.com/tukaani-project/xz.git";
        this->project = Project();
    }

    // -----------------------------------------------------------------------
    // operators

    void operator()()
    {
        // can be used in taskflow
        this->process();
    }

    // -----------------------------------------------------------------------
    // properties


    fs::path src_dir() { return this->project.src / this->name; }

    fs::path build_dir() { return this->src_dir() / std::string("build"); }

    fs::path prefix() { return this->project.install / this->name; }

    fs::path staticlib() { return this->prefix() / "lib" / "liblzma.a"; }

    fs::path dylib() { return this->prefix() / "lib" / "liblzma.dylib"; }

    std::string repo_branch()
    {
        return fmt::format("v{}", this->version.str());
    }

    std::string download_url()
    {
        return fmt::format("https://github.com/tukaani-project/xz/releases/"
                           "download/v{}/xz-{}.tar.gz",
                           this->version.str(), this->version.str());
    }

    // -----------------------------------------------------------------------
    // predicates
    
    bool libs_exist()
    {
        return fs::exists(this->staticlib()) || fs::exists(this->dylib());
    }

    // -----------------------------------------------------------------------
    // methods

    void download()
    {
        Info("XzBuilder.download source code");
        this->git_clone(this->repo_url, this->repo_branch(), this->src_dir());
    }

    void process()
    {
        Info("XzBuilder process starting");
        this->download();
        Info("XzBuilder process ending");
    }
};


class PythonBuilder : public ShellCmd {
    // builds python from source
public:
    // -----------------------------------------------------------------------
    // attributes

    semver::version version;
    std::string name;
    std::string repo_url;
    Project project;

    // -----------------------------------------------------------------------
    // constructor

    PythonBuilder(std::string version)
    {
        this->version = semver::version::parse(version);
        this->name = "python";
        this->repo_url = "https://github.com/python/cpython.git";
        this->project = Project();
    }

    // -----------------------------------------------------------------------
    // operators

    void operator()()
    {
        // can be used in taskflow
        this->process();
    }

    // -----------------------------------------------------------------------
    // properties

    std::string ver()
    {
        return fmt::format("{}.{}", this->version.major(),
                           this->version.minor());
    }

    std::string ver_nodot()
    {
        return fmt::format("{}{}", this->version.major(),
                           this->version.minor());
    }

    std::string name_version()
    {
        return fmt::format("{}{}", this->name, this->version.str());
    }

    std::string name_ver()
    {
        return fmt::format("{}{}", this->name, this->ver());
    }

    fs::path src_dir() { return this->project.src / this->name; }

    fs::path build_dir() { return this->src_dir() / std::string("build"); }

    fs::path prefix() { return this->project.install / this->name; }

    std::string repo_branch()
    {
        return fmt::format("v{}", this->version.str());
    }

    std::string download_url()
    {
        return fmt::format(
            "https://www.python.org/ftp/python/{}/Python-{}.tar.xz",
            this->version.str(), this->version.str());
    }

    std::string executable_name()
    {
        // return fmt::format("python{}", this->version.str());
        return fmt::format("{}3", this->name);
    }

    fs::path executable()
    {
        return this->project.install / "bin" / this->executable_name();
    }

    std::string lib_name()
    {
        return fmt::format("lib{}{}", this->name, this->version.str());
    }

    std::string staticlib_name()
    {
        return fmt::format("{}.a", this->lib_name());
    }

    fs::path staticlib()
    {
        return this->project.install / "lib" / this->staticlib_name();
    }

    std::string dylib_name()
    {
        return fmt::format("{}.dylib", this->lib_name());
    }

    fs::path dylib()
    {
        return this->project.install / "lib" / this->dylib_name();
    }

    // std::string dylib_link_name() = 0;
    // fs::path dylib_link() = 0;

    // predicates
    bool libs_exist()
    {
        return fs::exists(this->staticlib()) || fs::exists(this->dylib());
    }

    // -----------------------------------------------------------------------
    // actions

    void preprocess() { }
    void download()
    {
        Info("PythonBuilder.download source code");
        std::string dir = (this->project.src / this->name).string();
        this->git_clone(this->repo_url, this->repo_branch(), dir);
    }
    void setup() { }
    void configure() { }
    void build() { }
    void install() { }
    void clean() { }
    void postprocess() { }

    void process()
    {
        Info("PythonBuilder process starting");
        this->preprocess();
        this->download();
        this->setup();
        this->configure();
        this->build();
        this->install();
        this->clean();
        this->postprocess();
        Info("PythonBuilder process ending");
    }
};
