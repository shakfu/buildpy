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

    // std::system("python3 --version");
    // const char *command_line[] = {"echo", "\"Hello, world!\"", NULL};
    const char* command_line[] = { "/opt/homebrew/bin/python3", "--version",
                                   NULL };
    struct subprocess_s subprocess;
    int result = subprocess_create(command_line, 0, &subprocess);
    if (0 != result) {
        // an error occurred!
        Error("%s: error occurred.", name);
    } else {
        FILE* p_stdout = subprocess_stdout(&subprocess);
        char data[32];
        fgets(data, 32, p_stdout);
        Info("  data: %s", data);
    }
}


int run_tasks()
{
    tf::Executor executor;
    tf::Taskflow taskflow;

    auto [A, B, C, D] = taskflow.emplace( // create four tasks
        task_a, task_b, task_c, task_d);

    A.precede(B, C); // A runs before B and C
    D.succeed(B, C); // D runs after  B and C

    executor.run(taskflow).wait();

    return 0;
}


char* convert(const std::string& s)
{
    char* pc = new char[s.size() + 1];
    std::strcpy(pc, s.c_str());
    return pc;
}

int cmd(std::initializer_list<std::string> args)
{
    std::vector<char*> vc;
    std::transform(args.begin(), args.end(), std::back_inserter(vc), convert);
    // vc.push_back(NULL);

    struct subprocess_s subprocess;
    int result = subprocess_create(&vc[0], 0, &subprocess);
    if (0 != result) {
        printf("error occurred.\n");
    } else {
        FILE* p_stdout = subprocess_stdout(&subprocess);
        char data[32];
        fgets(data, 32, p_stdout);
        printf("  data: %s\n", data);
    }

    // std::cout << "vc.size(): " << vc.size() << std::endl;

    // for ( size_t i = 0 ; i < vc.size() ; i++ )
    //     std::cout << vc[i] << std::endl;

    for (size_t i = 0; i < vc.size(); i++)
        delete[] vc[i];

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

    // run_tasks();
    // cmd({ "/bin/bash", "--version" });
    auto p = PythonBuilder("3.12.2");
    Info("ver: %s", p.ver().c_str());
    Info("name_ver: %s", p.name_ver().c_str());
    p(); // test () operator overload
    p.cmd({"/bin/bash", "--version"});


    return 0;
}