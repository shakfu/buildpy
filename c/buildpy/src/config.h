#ifndef CONFIG_H
#define CONFIG_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#ifndef BUILDPY_DEBUG
#define BUILDPY_DEBUG 0
#endif

#define MAX_CONFIG_NAME 64
#define MAX_VERSION_STRING 16
#define MAX_HEADER_LINES 16
#define MAX_HEADER_LENGTH 256
#define MAX_EXTENSIONS 256
#define MAX_EXT_NAME 64
#define MAX_EXT_FILES 32
#define MAX_EXT_FILE_LENGTH 256
#define MAX_CORE_MODULES 32
#define MAX_MODULE_SETS 128

// Platform detection
#ifdef __APPLE__
    #define PLATFORM_DARWIN 1
    #define PLATFORM_LINUX 0
#elif __linux__
    #define PLATFORM_DARWIN 0
    #define PLATFORM_LINUX 1
#else
    #define PLATFORM_DARWIN 0
    #define PLATFORM_LINUX 0
#endif

// Version structure
typedef struct {
    int major;
    int minor;
    int patch;
} Version;

// Extension definition
typedef struct {
    char name[MAX_EXT_NAME];
    char files[MAX_EXT_FILES][MAX_EXT_FILE_LENGTH];
    int file_count;
} Extension;

// Module set (for static, shared, disabled)
typedef struct {
    char modules[MAX_MODULE_SETS][MAX_EXT_NAME];
    int count;
} ModuleSet;

// Main configuration structure
typedef struct {
    char name[MAX_CONFIG_NAME];
    Version version;

    // Headers for Setup.local
    char headers[MAX_HEADER_LINES][MAX_HEADER_LENGTH];
    int header_count;

    // Extension definitions
    Extension extensions[MAX_EXTENSIONS];
    int extension_count;

    // Core modules (always built-in)
    char core[MAX_CORE_MODULES][MAX_EXT_NAME];
    int core_count;

    // Module categorization
    ModuleSet static_modules;
    ModuleSet shared_modules;
    ModuleSet disabled_modules;
} Config;

// Configuration management functions
Config* config_new(const char* name, const char* version_str);
void config_free(Config* config);

// Version parsing and utilities
Version version_parse(const char* version_str);
char* version_string(const Version* version);
char* version_short_string(const Version* version); // e.g., "3.12"

// Module management functions
void config_add_extension(Config* config, const char* name, const char* files[], int file_count);
void config_add_core_module(Config* config, const char* name);
void config_add_static_module(Config* config, const char* name);
void config_add_shared_module(Config* config, const char* name);
void config_add_disabled_module(Config* config, const char* name);

// Module state transitions
void config_static_to_shared(Config* config, const char* names[], int count);
void config_shared_to_static(Config* config, const char* names[], int count);
void config_static_to_disabled(Config* config, const char* names[], int count);
void config_shared_to_disabled(Config* config, const char* names[], int count);
void config_disabled_to_static(Config* config, const char* names[], int count);
void config_disabled_to_shared(Config* config, const char* names[], int count);

// Single module transitions
void config_move_module(Config* config, const char* name, ModuleSet* from, ModuleSet* to);

// Module set utilities
bool module_set_contains(const ModuleSet* set, const char* name);
void module_set_add(ModuleSet* set, const char* name);
void module_set_remove(ModuleSet* set, const char* name);
void module_set_clear(ModuleSet* set);

// Configuration logic
void config_configure(Config* config);
void config_apply_platform_defaults(Config* config);
void config_apply_version_specific(Config* config);
void config_apply_configuration_type(Config* config);

// Extension lookup
Extension* config_find_extension(Config* config, const char* name);

// Setup.local generation
void config_write_setup_local(Config* config, const char* path);
void config_print_setup_local(Config* config);

// Debug and utilities
void config_print_summary(Config* config);
void config_debug_module_sets(Config* config);

// Helper functions for string arrays
char** string_array_create(int size);
void string_array_free(char** array, int size);
void string_array_copy(const char* src[], char** dest, int count);

// Internal initialization functions
void config_initialize_extensions(Config* config);
void config_initialize_static_modules(Config* config);
void config_initialize_disabled_modules(Config* config);

#endif // CONFIG_H