#ifndef BUILDPY_H
#define BUILDPY_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <errno.h>
#include <glob.h>
#include <libgen.h>

#define MAX_PATH 4096
#define MAX_COMMAND 8192
#define MAX_ARGS 256
#define MAX_PATTERNS 64

// Forward declarations
typedef struct Shell Shell;
typedef struct Project Project;
typedef struct Builder Builder;

// Shell command structure
struct Shell {
    char cwd[MAX_PATH];
    int verbose;
};

// Project structure
struct Project {
    Shell* shell;
    char build_dir[MAX_PATH];
    char downloads_dir[MAX_PATH];
    char source_dir[MAX_PATH];
    char install_dir[MAX_PATH];
    char bin_dir[MAX_PATH];
    char lib_dir[MAX_PATH];
    char lib_static_dir[MAX_PATH];
};

// Base builder structure
struct Builder {
    char* name;
    char* version;
    Project* project;
    Shell* shell;
    void (*build)(Builder*);
    void (*clean)(Builder*);
};

// Specific builder structures
typedef struct {
    Builder base;
} OpenSSLBuilder;

typedef struct {
    Builder base;
} Bzip2Builder;

typedef struct {
    Builder base;
} XzBuilder;

typedef struct {
    Builder base;
    char config[64];
    int debug;
    int optimize;
    int jobs;
    char** cfg_opts;
    int cfg_opts_count;
    char** pkgs;
    int pkgs_count;
} PythonBuilder;

// Shell functions
Shell* shell_new(void);
void shell_free(Shell* shell);
void shell_set_verbose(Shell* shell, int verbose);
char* shell_get_cwd(Shell* shell);
int shell_chdir(Shell* shell, const char* path);
int shell_exists(Shell* shell, const char* path);
int shell_is_dir(Shell* shell, const char* path);
int shell_is_file(Shell* shell, const char* path);
int shell_mkdir(Shell* shell, const char* path);
int shell_mkdir_p(Shell* shell, const char* path);
int shell_rmtree(Shell* shell, const char* path);
int shell_copy(Shell* shell, const char* src, const char* dst);
int shell_move(Shell* shell, const char* src, const char* dst);
int shell_symlink(Shell* shell, const char* src, const char* dst);
int shell_chmod(Shell* shell, const char* path, mode_t mode);
int shell_run_command(Shell* shell, const char* cmd);
int shell_run_command_in_dir(Shell* shell, const char* cmd, const char* work_dir);
char* shell_run_command_output(Shell* shell, const char* cmd);
void shell_glob_remove(Shell* shell, const char** patterns, int pattern_count, const char* base_path);
int shell_zip_directory(Shell* shell, const char* source_dir, const char* dest_zip);

// Project functions
Project* project_new(void);
void project_free(Project* project);
int project_setup(Project* project);
void project_clean(Project* project);

// Builder functions
Builder* builder_new(const char* name, const char* version, Project* project);
void builder_free(Builder* builder);

// Specific builder constructors
OpenSSLBuilder* openssl_builder_new(const char* version, Project* project);
Bzip2Builder* bzip2_builder_new(const char* version, Project* project);
XzBuilder* xz_builder_new(const char* version, Project* project);
PythonBuilder* python_builder_new(const char* version, Project* project);

// Builder functions
void openssl_builder_build(Builder* builder);
void openssl_builder_clean(Builder* builder);
void bzip2_builder_build(Builder* builder);
void bzip2_builder_clean(Builder* builder);
void xz_builder_build(Builder* builder);
void xz_builder_clean(Builder* builder);
void python_builder_build(Builder* builder);
void python_builder_clean(Builder* builder);

// PythonBuilder specific functions
void python_builder_set_config(PythonBuilder* builder, const char* config);
void python_builder_set_debug(PythonBuilder* builder, int debug);
void python_builder_set_optimize(PythonBuilder* builder, int optimize);
void python_builder_set_jobs(PythonBuilder* builder, int jobs);
void python_builder_add_cfg_opt(PythonBuilder* builder, const char* opt);
void python_builder_add_pkg(PythonBuilder* builder, const char* pkg);

// CLI functions
int parse_args(int argc, char* argv[], PythonBuilder* python_builder, int* reset, int* write, char* json_file);
void print_usage(const char* program_name);
void print_version(void);

// Utility functions
char* join_path(const char* path1, const char* path2);
char* string_replace(const char* str, const char* old, const char* new_str);
int string_ends_with(const char* str, const char* suffix);
void log_info(const char* format, ...);
void log_error(const char* format, ...);

#endif // BUILDPY_H