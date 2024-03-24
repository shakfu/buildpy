#include "buildpy.hpp"


const char* VERSION = "1.0.1";

void task_a() { Info("task A"); }

void task_b() { Info("task B"); }

void task_c() { Info("task C"); }

void task_d()
{
    const char* name = "task D";
    // Info("task D");
    Info(name);
}


int run_tasks()
{
    tf::Executor executor;
    tf::Taskflow taskflow;

    auto [ssl, bz2, xz, py] = taskflow.emplace( // create four tasks
        OpenSSLBuilder("1.1.1"), 
        Bzip2Builder("1.0.8"), 
        XzBuilder("5.6.0"), 
        PythonBuilder("3.12.2")
    );

    py.succeed(ssl, bz2, xz); // D runs after A, B and C

    executor.run(taskflow).wait();

    return 0;
}


int main(int argc, char* argv[])
{
    argparse::ArgumentParser program("buildpy", VERSION);

    program.add_argument("square")
        .help("display the square of a given integer")
        .scan<'i', int>();

    program.add_argument("-c", "--config")
        .help("build configuration name")
        .default_value("static_max")
        .implicit_value(true);

    program.add_argument("-o", "--optimize")
        .help("optimize python build")
        .flag();

    try {
        program.parse_args(argc, argv);
    } catch (const std::exception& err) {
        std::cerr << err.what() << std::endl;
        std::cerr << program;
        return 1;
    }

    auto input = program.get<int>("square");
    std::cout << "square: " << (input * input) << std::endl;

    auto config = program.get<std::string>("config");
    std::cout << "config: " << config << std::endl;

    run_tasks();
    // cmd({ "/bin/bash", "--version" });
    auto p = PythonBuilder("3.12.2");
    Info("ver: %s", p.ver().c_str());
    Info("name_ver: %s", p.name_ver().c_str());
    Info("prefix: %s", p.prefix().c_str());
    // p(); // test () operator overload
    // p.cmd({"/bin/bash", "--version"});


    return 0;
}