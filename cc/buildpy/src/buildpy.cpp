#include "buildpy.hpp"

const char* VERSION = "1.0.1";

int main(int argc, char* argv[])
{
    argparse::ArgumentParser program("buildpy", VERSION);

    program.add_argument("-p", "--pyversion")
        .help("python version")
        .default_value("3.11.8")
        .nargs(1);

    program.add_argument("-c", "--config")
        .help("build configuration name")
        .default_value("static_max")
        .nargs(1);

    program.add_argument("-o", "--optimize")
        .help("optimize python build")
        .flag();

    program.add_argument("-g", "--git")
        .help("download using git")
        .flag();

    program.add_argument("-r", "--reset")
        .help("reset build environment")
        .flag();

    try {
        program.parse_args(argc, argv);
    } catch (const std::exception& err) {
        std::cerr << err.what() << std::endl;
        std::cerr << program;
        return 1;
    }

    auto pyversion = program.get<std::string>("pyversion");
    auto config = program.get<std::string>("config");
    bool optimize = program.get<bool>("optimize");
    bool reset = program.get<bool>("reset");

    std::cout << "pyversion: " << pyversion << std::endl;
    std::cout << "config: " << config << std::endl;
    std::cout << "optimize: " << optimize << std::endl;
    std::cout << "reset: " << reset << std::endl;

    auto p = buildpy::PythonBuilder(pyversion, config, optimize, reset);
    p.process();

    return 0;
}