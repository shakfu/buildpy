#include "buildpy.hpp"


const char* VERSION = "1.0.1";



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
    auto config = program.get<std::string>("config");
    bool optimize = program.get<bool>("optimize");

    std::cout << "pyversion: " << pyversion << std::endl;
    std::cout << "config: " << config << std::endl;
    std::cout << "optimize: " << optimize << std::endl;

    auto p = PythonBuilder(pyversion, config, optimize);
    p.process();

    // std::map<std::string, std::vector<std::string>> zmap = {
    //     { "abc", { "foo", "moo" } }, { "def", { "var", "baz" } }
    // };
    // std::cout << zmap["abc"][0] << std::endl;

    return 0;
}