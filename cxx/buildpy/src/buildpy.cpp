#include "buildpy.hpp"


const char* VERSION = "1.0.1";


int run_tasks(std::string pyversion)
{
    // OpenSSLBuilder("1.1.1").process();
    // Bzip2Builder("1.0.8").process();
    XzBuilder("5.6.0").process();
    // PythonBuilder(pyversion).process();
    return 0;
}


int main(int argc, char* argv[])
{
    argparse::ArgumentParser program("buildpy", VERSION);

    // program.add_argument("square")
    //     .help("display the square of a given integer")
    //     .scan<'i', int>();

    program.add_argument("-p", "--pyversion")
        .help("python version")
        .default_value("3.12.2")
        .implicit_value(true);

    program.add_argument("-c", "--config")
        .help("build configuration name")
        .default_value("static_max")
        .implicit_value(true);

    program.add_argument("-o", "--optimize")
        .help("optimize python build")
        .flag();

    program.add_argument("-g", "--usegit").help("download using git").flag();

    try {
        program.parse_args(argc, argv);
    } catch (const std::exception& err) {
        std::cerr << err.what() << std::endl;
        std::cerr << program;
        return 1;
    }

    // auto input = program.get<int>("square");
    // std::cout << "square: " << (input * input) << std::endl;

    auto pyversion = program.get<std::string>("pyversion");
    std::cout << "pyversion: " << pyversion << std::endl;

    auto config = program.get<std::string>("config");
    std::cout << "config: " << config << std::endl;

    run_tasks(pyversion);
    // cmd({ "/bin/bash", "--version" });
    auto p = PythonBuilder(pyversion);
    Info("ver: %s", p.ver().c_str());
    Info("name_ver: %s", p.name_ver().c_str());
    Info("prefix: %s", p.prefix().c_str());
    // p(); // test () operator overload
    // p.cmd({"/bin/bash", "--version"});

    std::map<std::string, std::vector<std::string>> zmap = {
        { "abc", { "foo", "moo" } }, { "def", { "var", "baz" } }
    };
    std::cout << zmap["abc"][0] << std::endl;

    return 0;
}