#include "config.h"
#include "buildpy.h"
#include <ctype.h>

// Version utilities
Version version_parse(const char* version_str) {
    Version v = {0, 0, 0};
    if (version_str) {
        sscanf(version_str, "%d.%d.%d", &v.major, &v.minor, &v.patch);
    }
    return v;
}

char* version_string(const Version* version) {
    char* result = malloc(MAX_VERSION_STRING);
    if (result) {
        snprintf(result, MAX_VERSION_STRING, "%d.%d.%d",
                version->major, version->minor, version->patch);
    }
    return result;
}

char* version_short_string(const Version* version) {
    char* result = malloc(MAX_VERSION_STRING);
    if (result) {
        snprintf(result, MAX_VERSION_STRING, "%d.%d",
                version->major, version->minor);
    }
    return result;
}

// Module set utilities
bool module_set_contains(const ModuleSet* set, const char* name) {
    if (!set || !name) return false;

    for (int i = 0; i < set->count; i++) {
        if (strcmp(set->modules[i], name) == 0) {
            return true;
        }
    }
    return false;
}

void module_set_add(ModuleSet* set, const char* name) {
    if (!set || !name || set->count >= MAX_MODULE_SETS) return;
    if (module_set_contains(set, name)) return; // Already exists

    strncpy(set->modules[set->count], name, MAX_EXT_NAME - 1);
    set->modules[set->count][MAX_EXT_NAME - 1] = '\0';
    set->count++;
}

void module_set_remove(ModuleSet* set, const char* name) {
    if (!set || !name) return;

    for (int i = 0; i < set->count; i++) {
        if (strcmp(set->modules[i], name) == 0) {
            // Shift remaining elements
            for (int j = i; j < set->count - 1; j++) {
                strcpy(set->modules[j], set->modules[j + 1]);
            }
            set->count--;
            return;
        }
    }
}

void module_set_clear(ModuleSet* set) {
    if (set) {
        set->count = 0;
    }
}

// Configuration creation and management
Config* config_new(const char* name, const char* version_str) {
    Config* config = calloc(1, sizeof(Config));
    if (!config) return NULL;

    // Set name
    strncpy(config->name, name, MAX_CONFIG_NAME - 1);
    config->name[MAX_CONFIG_NAME - 1] = '\0';

    // Parse version
    config->version = version_parse(version_str);

    // Initialize default headers
    const char* default_headers[] = {
        "DESTLIB=$(LIBDEST)",
        "MACHDESTLIB=$(BINLIBDEST)",
        "DESTPATH=",
        "SITEPATH=",
        "TESTPATH=",
        "COREPYTHONPATH=$(DESTPATH)$(SITEPATH)$(TESTPATH)",
        "PYTHONPATH=$(COREPYTHONPATH)",
        "OPENSSL=$(srcdir)/../../install",
        "BZIP2=$(srcdir)/../../install",
        "LZMA=$(srcdir)/../../install"
    };

    config->header_count = sizeof(default_headers) / sizeof(default_headers[0]);
    for (int i = 0; i < config->header_count; i++) {
        strncpy(config->headers[i], default_headers[i], MAX_HEADER_LENGTH - 1);
        config->headers[i][MAX_HEADER_LENGTH - 1] = '\0';
    }

    // Initialize core modules
    const char* core_modules[] = {
        "_abc", "_codecs", "_collections", "_functools", "_io",
        "_locale", "_operator", "_signal", "_sre", "_stat", "_symtable",
        "_thread", "_tracemalloc", "_weakref", "atexit", "errno",
        "faulthandler", "itertools", "posix", "pwd", "time"
    };

    config->core_count = sizeof(core_modules) / sizeof(core_modules[0]);
    for (int i = 0; i < config->core_count; i++) {
        strncpy(config->core[i], core_modules[i], MAX_EXT_NAME - 1);
        config->core[i][MAX_EXT_NAME - 1] = '\0';
    }

    // Initialize default extensions and modules
    config_initialize_extensions(config);
    config_initialize_static_modules(config);
    config_initialize_disabled_modules(config);

    return config;
}

void config_free(Config* config) {
    if (config) {
        free(config);
    }
}

// Extension management
void config_add_extension(Config* config, const char* name, const char* files[], int file_count) {
    if (!config || !name || config->extension_count >= MAX_EXTENSIONS) return;

    Extension* ext = &config->extensions[config->extension_count];
    strncpy(ext->name, name, MAX_EXT_NAME - 1);
    ext->name[MAX_EXT_NAME - 1] = '\0';

    ext->file_count = (file_count > MAX_EXT_FILES) ? MAX_EXT_FILES : file_count;
    for (int i = 0; i < ext->file_count; i++) {
        strncpy(ext->files[i], files[i], MAX_EXT_FILE_LENGTH - 1);
        ext->files[i][MAX_EXT_FILE_LENGTH - 1] = '\0';
    }

    config->extension_count++;
}

Extension* config_find_extension(Config* config, const char* name) {
    if (!config || !name) return NULL;

    for (int i = 0; i < config->extension_count; i++) {
        if (strcmp(config->extensions[i].name, name) == 0) {
            return &config->extensions[i];
        }
    }
    return NULL;
}

// Module management
void config_add_core_module(Config* config, const char* name) {
    if (!config || !name || config->core_count >= MAX_CORE_MODULES) return;

    strncpy(config->core[config->core_count], name, MAX_EXT_NAME - 1);
    config->core[config->core_count][MAX_EXT_NAME - 1] = '\0';
    config->core_count++;
}

void config_add_static_module(Config* config, const char* name) {
    if (config) module_set_add(&config->static_modules, name);
}

void config_add_shared_module(Config* config, const char* name) {
    if (config) module_set_add(&config->shared_modules, name);
}

void config_add_disabled_module(Config* config, const char* name) {
    if (config) module_set_add(&config->disabled_modules, name);
}

// Module transitions
void config_move_module(Config* config, const char* name, ModuleSet* from, ModuleSet* to) {
    if (!config || !name || !from || !to) return;
    if (!module_set_contains(from, name)) return;

    module_set_remove(from, name);
    module_set_add(to, name);
}

void config_static_to_shared(Config* config, const char* names[], int count) {
    if (!config || !names) return;

    for (int i = 0; i < count; i++) {
        config_move_module(config, names[i], &config->static_modules, &config->shared_modules);
    }
}

void config_shared_to_static(Config* config, const char* names[], int count) {
    if (!config || !names) return;

    for (int i = 0; i < count; i++) {
        config_move_module(config, names[i], &config->shared_modules, &config->static_modules);
    }
}

void config_static_to_disabled(Config* config, const char* names[], int count) {
    if (!config || !names) return;

    for (int i = 0; i < count; i++) {
        config_move_module(config, names[i], &config->static_modules, &config->disabled_modules);
    }
}

void config_shared_to_disabled(Config* config, const char* names[], int count) {
    if (!config || !names) return;

    for (int i = 0; i < count; i++) {
        config_move_module(config, names[i], &config->shared_modules, &config->disabled_modules);
    }
}

void config_disabled_to_static(Config* config, const char* names[], int count) {
    if (!config || !names) return;

    for (int i = 0; i < count; i++) {
        config_move_module(config, names[i], &config->disabled_modules, &config->static_modules);
    }
}

void config_disabled_to_shared(Config* config, const char* names[], int count) {
    if (!config || !names) return;

    for (int i = 0; i < count; i++) {
        config_move_module(config, names[i], &config->disabled_modules, &config->shared_modules);
    }
}

// Configuration initialization functions are implemented in config_data.c