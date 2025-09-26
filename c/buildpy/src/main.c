#include "buildpy.h"
#include <getopt.h>

void print_usage(const char* program_name) {
    printf("Usage: %s [OPTIONS]\n", program_name);
    printf("\nA Python builder - builds Python from source with customizable options\n\n");
    printf("Options:\n");
    printf("  -a, --cfg-opts=OPTS     Add config options\n");
    printf("  -c, --config=CONFIG     Build configuration (default: static_max)\n");
    printf("  -d, --debug             Build debug python\n");
    printf("  -o, --optimize          Optimize build\n");
    printf("  -p, --pkgs=PKGS         Install packages\n");
    printf("  -r, --reset             Reset build\n");
    printf("  -v, --version=VERSION   Python version (default: 3.12.9)\n");
    printf("  -w, --write             Write configuration\n");
    printf("  -j, --jobs=JOBS         Number of build jobs (default: 4)\n");
    printf("  -s, --json=FILE         Serialize config to json file\n");
    printf("      --verbose           Show verbose output\n");
    printf("      --help              Show this help message\n");
}

void print_version(void) {
    printf("buildpy C version 1.0.0\n");
}

int parse_args(int argc, char* argv[], PythonBuilder* python_builder, int* reset, int* write, char* json_file) {
    int opt;
    int verbose = 0;

    static struct option long_options[] = {
        {"cfg-opts",  required_argument, 0, 'a'},
        {"config",    required_argument, 0, 'c'},
        {"debug",     no_argument,       0, 'd'},
        {"optimize",  no_argument,       0, 'o'},
        {"pkgs",      required_argument, 0, 'p'},
        {"reset",     no_argument,       0, 'r'},
        {"version",   required_argument, 0, 'v'},
        {"write",     no_argument,       0, 'w'},
        {"jobs",      required_argument, 0, 'j'},
        {"json",      required_argument, 0, 's'},
        {"verbose",   no_argument,       0, 1},
        {"help",      no_argument,       0, 2},
        {0, 0, 0, 0}
    };

    while ((opt = getopt_long(argc, argv, "a:c:dop:rv:wj:s:", long_options, NULL)) != -1) {
        switch (opt) {
            case 'a':
                python_builder_add_cfg_opt(python_builder, optarg);
                break;
            case 'c':
                python_builder_set_config(python_builder, optarg);
                break;
            case 'd':
                python_builder_set_debug(python_builder, 1);
                break;
            case 'o':
                python_builder_set_optimize(python_builder, 1);
                break;
            case 'p':
                python_builder_add_pkg(python_builder, optarg);
                break;
            case 'r':
                *reset = 1;
                break;
            case 'v':
                // Update Python version - we need to recreate the builder
                // This is a limitation of this simple implementation
                free(python_builder->base.version);
                python_builder->base.version = malloc(strlen(optarg) + 1);
                if (python_builder->base.version) {
                    strcpy(python_builder->base.version, optarg);
                }
                break;
            case 'w':
                *write = 1;
                break;
            case 'j':
                python_builder_set_jobs(python_builder, atoi(optarg));
                break;
            case 's':
                if (json_file) {
                    strncpy(json_file, optarg, MAX_PATH - 1);
                    json_file[MAX_PATH - 1] = '\0';
                }
                break;
            case 1:
                verbose = 1;
                break;
            case 2:
                print_usage(argv[0]);
                return 1;
            case '?':
                print_usage(argv[0]);
                return -1;
            default:
                print_usage(argv[0]);
                return -1;
        }
    }

    // Set verbose mode on shell
    if (python_builder && python_builder->base.shell) {
        shell_set_verbose(python_builder->base.shell, verbose);
    }

    return 0;
}

int main(int argc, char* argv[]) {
    int reset = 0;
    int write = 0;
    char json_file[MAX_PATH] = {0};

    // Create project and setup build environment
    Project* project = project_new();
    if (!project) {
        log_error("Failed to create project");
        return EXIT_FAILURE;
    }

    // Create Python builder with default settings
    PythonBuilder* python_builder = python_builder_new("3.12.9", project);
    if (!python_builder) {
        log_error("Failed to create Python builder");
        project_free(project);
        return EXIT_FAILURE;
    }

    // Parse command line arguments
    int parse_result = parse_args(argc, argv, python_builder, &reset, &write, json_file);
    if (parse_result != 0) {
        if (parse_result > 0) {
            // Help was shown, exit successfully
            free(python_builder);
            project_free(project);
            return EXIT_SUCCESS;
        } else {
            // Parse error
            free(python_builder);
            project_free(project);
            return EXIT_FAILURE;
        }
    }

    // Print verbose information if enabled
    if (python_builder->base.shell->verbose) {
        printf("C buildpy starting with options:\n");
        printf("  version: %s\n", python_builder->base.version);
        printf("  config: %s\n", python_builder->config);
        printf("  debug: %s\n", python_builder->debug ? "true" : "false");
        printf("  optimize: %s\n", python_builder->optimize ? "true" : "false");
        printf("  jobs: %d\n", python_builder->jobs);
        printf("  reset: %s\n", reset ? "true" : "false");
        printf("  write: %s\n", write ? "true" : "false");
        if (strlen(json_file) > 0) {
            printf("  json: %s\n", json_file);
        }
        printf("\n");
    }

    // Reset build environment if requested
    if (reset) {
        log_info("Resetting build environment...");
        project_clean(project);
    }

    // Setup build environment
    if (project_setup(project) != 0) {
        log_error("Failed to setup build environment");
        free(python_builder);
        project_free(project);
        return EXIT_FAILURE;
    }

    // Handle write configuration (placeholder)
    if (write) {
        log_info("Write configuration not yet implemented");
        free(python_builder);
        project_free(project);
        return EXIT_SUCCESS;
    }

    // Handle JSON serialization (placeholder)
    if (strlen(json_file) > 0) {
        log_info("JSON serialization not yet implemented: %s", json_file);
    }

    // Validate config
    const char* valid_configs[] = {
        "static_max", "static_mid", "static_min",
        "shared_max", "shared_mid", "shared_min"
    };
    int valid_config = 0;
    for (size_t i = 0; i < sizeof(valid_configs) / sizeof(valid_configs[0]); i++) {
        if (strcmp(python_builder->config, valid_configs[i]) == 0) {
            valid_config = 1;
            break;
        }
    }

    if (!valid_config) {
        log_error("Invalid config '%s'. Valid configs: static_max, static_mid, static_min, shared_max, shared_mid, shared_min", python_builder->config);
        free(python_builder);
        project_free(project);
        return EXIT_FAILURE;
    }

    // Build Python
    log_info("Building Python %s with config: %s", python_builder->base.version, python_builder->config);
    python_builder->base.build((Builder*)python_builder);

    log_info("Build completed successfully!");

    // Cleanup
    free(python_builder);
    project_free(project);

    return EXIT_SUCCESS;
}