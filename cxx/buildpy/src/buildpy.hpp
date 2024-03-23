#include <argparse/argparse.hpp>
#include <fmt/core.h>
#include <logy.h>
#include <semver.hpp>
#include <subprocess.h>
#include <taskflow/taskflow.hpp>

#include <algorithm>
#include <initializer_list>
#include <string>
#include <vector>


namespace fs = std::filesystem;


class ShellCmd {
    // Utility class to hold common file operations
public:
    void cmd(std::initializer_list<std::string> args)
    {
        // std::vector<std::string> vs = std::vector(args);
        std::vector<char*> vc;

        std::transform(vc.begin(), vc.end(), std::back_inserter(vc),
            [](const std::string& s) -> char* {
               char* pc = new char[s.size() + 1];
               std::strcpy(pc, s.c_str());
               return pc;
            });

        struct subprocess_s subprocess;
        int result = subprocess_create(&vc[0], 0, &subprocess);
        if (0 != result) {
            Error("error occurred.\n");
        } else {
            FILE* p_stdout = subprocess_stdout(&subprocess);
            char data[32];
            fgets(data, 32, p_stdout);
            Info("result: %s\n", data);
        }
    }

    void create_dir(fs::path p)
    {
        if (!fs::exists(p)) {
            fs::create_directory(p);
        }
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

    std::string repo_branch()
    {   // "OpenSSL_1_1_1w"
        std::string s = this->version.str();
        std::replace( s.begin(), s.end(), '.', '_'); // replace all '.' to '_'
        return fmt::format("OpenSSL_{}", s);
    }

    std::string download_url()
    {
        return fmt::format(
            "https://www.openssl.org/source/old/1.1.1/openssl-{}.tar.gz",
            this->version.str());
    }

    // -----------------------------------------------------------------------
    // methods

    void process() {}
    
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
    void setup() { }
    void configure() { }
    void build() { }
    void install() { }
    void clean() { }
    void postprocess() { }

    void process()
    {
        Info("python process starting");
        this->preprocess();
        this->setup();
        this->configure();
        this->build();
        this->install();
        this->clean();
        this->postprocess();
        Info("python process ending");
    }
};
