#include "config.h"
#include "buildpy.h"

// Platform-specific configuration
void config_apply_platform_defaults(Config* config) {
    if (!config) return;

    if (PLATFORM_DARWIN) {
        log_debug("Applying macOS platform defaults");           
        // Enable _scproxy on macOS
        const char* enable_modules[] = {"_scproxy"};
        config_disabled_to_static(config, enable_modules, 1);

    } else if (PLATFORM_LINUX) {
        log_debug("Applying Linux platform defaults");         
        // Enable ossaudiodev on Linux
        const char* enable_modules[] = {"ossaudiodev"};
        config_disabled_to_static(config, enable_modules, 1);

        // Update SSL extensions for static linking on Linux
        Extension* ssl_ext = config_find_extension(config, "_ssl");
        if (ssl_ext) {
            const char* ssl_files[] = {
                "_ssl.c",
                "-I$(OPENSSL)/include",
                "-L$(OPENSSL)/lib",
                "-l:libssl.a -Wl,--exclude-libs,libssl.a",
                "-l:libcrypto.a -Wl,--exclude-libs,libcrypto.a"
            };
            ssl_ext->file_count = 5;
            for (int i = 0; i < ssl_ext->file_count; i++) {
                strncpy(ssl_ext->files[i], ssl_files[i], MAX_EXT_FILE_LENGTH - 1);
                ssl_ext->files[i][MAX_EXT_FILE_LENGTH - 1] = '\0';
            }
        }

        Extension* hashlib_ext = config_find_extension(config, "_hashlib");
        if (hashlib_ext) {
            const char* hashlib_files[] = {
                "_hashopenssl.c",
                "-I$(OPENSSL)/include",
                "-L$(OPENSSL)/lib",
                "-l:libcrypto.a -Wl,--exclude-libs,libcrypto.a"
            };
            hashlib_ext->file_count = 4;
            for (int i = 0; i < hashlib_ext->file_count; i++) {
                strncpy(hashlib_ext->files[i], hashlib_files[i], MAX_EXT_FILE_LENGTH - 1);
                hashlib_ext->files[i][MAX_EXT_FILE_LENGTH - 1] = '\0';
            }
        }
    }
}

// Version-specific configuration
void config_apply_version_specific(Config* config) {
    if (!config) return;

    if (config->version.minor >= 11) {
        log_debug("Applying Python 3.11+ specific configuration");
        // No specific changes for base 3.11+ yet

    }

    if (config->version.minor >= 12) {
        log_debug("Applying Python 3.12+ specific configuration");

        // Update hash modules for HACL* implementation
        Extension* md5_ext = config_find_extension(config, "_md5");
        if (md5_ext) {
            const char* md5_files[] = {
                "md5module.c",
                "-I$(srcdir)/Modules/_hacl/include",
                "_hacl/Hacl_Hash_MD5.c",
                "-D_BSD_SOURCE",
                "-D_DEFAULT_SOURCE"
            };
            md5_ext->file_count = 5;
            for (int i = 0; i < md5_ext->file_count; i++) {
                strncpy(md5_ext->files[i], md5_files[i], MAX_EXT_FILE_LENGTH - 1);
                md5_ext->files[i][MAX_EXT_FILE_LENGTH - 1] = '\0';
            }
        }

        Extension* sha1_ext = config_find_extension(config, "_sha1");
        if (sha1_ext) {
            const char* sha1_files[] = {
                "sha1module.c",
                "-I$(srcdir)/Modules/_hacl/include",
                "_hacl/Hacl_Hash_SHA1.c",
                "-D_BSD_SOURCE",
                "-D_DEFAULT_SOURCE"
            };
            sha1_ext->file_count = 5;
            for (int i = 0; i < sha1_ext->file_count; i++) {
                strncpy(sha1_ext->files[i], sha1_files[i], MAX_EXT_FILE_LENGTH - 1);
                sha1_ext->files[i][MAX_EXT_FILE_LENGTH - 1] = '\0';
            }
        }

        // Add _sha2 module (replaces _sha256 and _sha512)
        config_add_extension(config, "_sha2", (const char*[]){
            "sha2module.c",
            "-I$(srcdir)/Modules/_hacl/include",
            "_hacl/Hacl_Hash_SHA2.c",
            "-D_BSD_SOURCE",
            "-D_DEFAULT_SOURCE",
            "Modules/_hacl/libHacl_Hash_SHA2.a"
        }, 6);

        Extension* sha3_ext = config_find_extension(config, "_sha3");
        if (sha3_ext) {
            const char* sha3_files[] = {
                "sha3module.c",
                "-I$(srcdir)/Modules/_hacl/include",
                "_hacl/Hacl_Hash_SHA3.c",
                "-D_BSD_SOURCE",
                "-D_DEFAULT_SOURCE"
            };
            sha3_ext->file_count = 5;
            for (int i = 0; i < sha3_ext->file_count; i++) {
                strncpy(sha3_ext->files[i], sha3_files[i], MAX_EXT_FILE_LENGTH - 1);
                sha3_ext->files[i][MAX_EXT_FILE_LENGTH - 1] = '\0';
            }
        }

        // Remove old SHA modules and add new ones
        module_set_remove(&config->static_modules, "_sha256");
        module_set_remove(&config->static_modules, "_sha512");
        config_add_static_module(config, "_sha2");

        // Add disabled modules
        config_add_disabled_module(config, "_xxinterpchannels");
    }

    if (config->version.minor >= 13) {
        log_debug("Applying Python 3.13+ specific configuration");

        // Add new interpreter modules
        config_add_extension(config, "_interpchannels", (const char*[]){"_interpchannelsmodule.c"}, 1);
        config_add_extension(config, "_interpqueues", (const char*[]){"_interpqueuesmodule.c"}, 1);
        config_add_extension(config, "_interpreters", (const char*[]){"_interpretersmodule.c"}, 1);
        config_add_extension(config, "_sysconfig", (const char*[]){"_sysconfig.c"}, 1);
        config_add_extension(config, "_testexternalinspection", (const char*[]){"_testexternalinspection.c"}, 1);

        // Add to static modules
        const char* new_static[] = {"_interpchannels", "_interpqueues", "_interpreters", "_sysconfig"};
        for (int i = 0; i < 4; i++) {
            config_add_static_module(config, new_static[i]);
        }

        // Remove deprecated modules
        module_set_remove(&config->disabled_modules, "_crypt");
        module_set_remove(&config->disabled_modules, "_xxsubinterpreters");
        module_set_remove(&config->disabled_modules, "audioop");
        module_set_remove(&config->disabled_modules, "nis");
        module_set_remove(&config->disabled_modules, "ossaudiodev");
        module_set_remove(&config->disabled_modules, "spwd");

        // Add new disabled module
        config_add_disabled_module(config, "_testexternalinspection");
    }
}

// Configuration type specific logic
void config_apply_configuration_type(Config* config) {
    if (!config) return;

    if (strcmp(config->name, "static_max") == 0) {
        log_debug("Applying static_max configuration");
        if (config->version.minor >= 11 && PLATFORM_LINUX) {
            const char* disable_modules[] = {"_decimal"};
            config_static_to_disabled(config, disable_modules, 1);
        }

    } else if (strcmp(config->name, "static_mid") == 0) {
        log_debug("Applying static_mid configuration");
        const char* disable_modules[] = {"_decimal"};
        config_static_to_disabled(config, disable_modules, 1);

    } else if (strcmp(config->name, "static_min") == 0) {
        log_debug("Applying static_min configuration");
        const char* disable_modules[] = {
            "_bz2", "_decimal", "_csv", "_json", "_lzma", "_scproxy",
            "_sqlite3", "_ssl", "pyexpat", "readline"
        };
        config_static_to_disabled(config, disable_modules, 10);

    } else if (strcmp(config->name, "shared_max") == 0) {
        log_debug("Applying shared_max configuration");
        if (config->version.minor >= 11) {
            if (PLATFORM_LINUX) {
                const char* disable_modules[] = {"_decimal"};
                config_static_to_disabled(config, disable_modules, 1);
            } else {
                const char* enable_shared[] = {"_ctypes"};
                config_disabled_to_shared(config, enable_shared, 1);

                const char* move_to_shared[] = {"_decimal", "_ssl", "_hashlib"};
                config_static_to_shared(config, move_to_shared, 3);
            }
        }

    } else if (strcmp(config->name, "shared_mid") == 0) {
        log_debug("Applying shared_mid configuration");
        const char* disable_modules[] = {"_decimal", "_ssl", "_hashlib"};
        config_static_to_disabled(config, disable_modules, 3);

    } else if (strcmp(config->name, "shared_min") == 0) {
        log_debug("Applying shared_min configuration");
        // Similar to shared_mid for now
        const char* disable_modules[] = {"_decimal", "_ssl", "_hashlib"};
        config_static_to_disabled(config, disable_modules, 3);
    }
}

// Main configuration function
void config_configure(Config* config) {
    if (!config) return;

    log_debug("Configuring Python %d.%d.%d with profile: %s",
            config->version.major, config->version.minor, config->version.patch,
            config->name);

    config_apply_platform_defaults(config);
    config_apply_version_specific(config);
    config_apply_configuration_type(config);

    log_debug("Configuration complete");
}

// Setup.local generation
void config_write_setup_local(Config* config, const char* path) {
    if (!config || !path) return;

    FILE* file = fopen(path, "w");
    if (!file) {
        log_error("Cannot create Setup.local file: %s", path);
        return;
    }

    // Write header comment
    fprintf(file, "# -*- makefile -*-\n");
    fprintf(file, "# name: %s\n", config->name);
    fprintf(file, "# version: %d.%d.%d\n\n",
            config->version.major, config->version.minor, config->version.patch);

    // Write headers
    fprintf(file, "# headers\n");
    for (int i = 0; i < config->header_count; i++) {
        fprintf(file, "%s\n", config->headers[i]);
    }
    fprintf(file, "\n");

    // Write core modules
    fprintf(file, "# core\n");
    for (int i = 0; i < config->core_count; i++) {
        Extension* ext = config_find_extension(config, config->core[i]);
        if (ext) {
            fprintf(file, "%s", ext->name);
            for (int j = 0; j < ext->file_count; j++) {
                fprintf(file, " %s", ext->files[j]);
            }
            fprintf(file, "\n");
        }
    }
    fprintf(file, "\n");

    // Write static modules
    if (config->static_modules.count > 0) {
        fprintf(file, "*static*\n");
        for (int i = 0; i < config->static_modules.count; i++) {
            Extension* ext = config_find_extension(config, config->static_modules.modules[i]);
            if (ext) {
                fprintf(file, "%s", ext->name);
                for (int j = 0; j < ext->file_count; j++) {
                    fprintf(file, " %s", ext->files[j]);
                }
                fprintf(file, "\n");
            }
        }
        fprintf(file, "\n");
    }

    // Write shared modules
    if (config->shared_modules.count > 0) {
        fprintf(file, "*shared*\n");
        for (int i = 0; i < config->shared_modules.count; i++) {
            Extension* ext = config_find_extension(config, config->shared_modules.modules[i]);
            if (ext) {
                fprintf(file, "%s", ext->name);
                for (int j = 0; j < ext->file_count; j++) {
                    fprintf(file, " %s", ext->files[j]);
                }
                fprintf(file, "\n");
            }
        }
        fprintf(file, "\n");
    }

    // Write disabled modules
    fprintf(file, "*disabled*\n");
    for (int i = 0; i < config->disabled_modules.count; i++) {
        fprintf(file, "%s\n", config->disabled_modules.modules[i]);
    }

    fprintf(file, "\n# end\n");

    fclose(file);
    log_info("Setup.local written to: %s", path);
}

void config_print_setup_local(Config* config) {
    if (!config) return;

    printf("# -*- makefile -*-\n");
    printf("# name: %s\n", config->name);
    printf("# version: %d.%d.%d\n\n",
           config->version.major, config->version.minor, config->version.patch);

    printf("Configuration ready for Setup.local generation\n");
    printf("Static modules: %d\n", config->static_modules.count);
    printf("Shared modules: %d\n", config->shared_modules.count);
    printf("Disabled modules: %d\n", config->disabled_modules.count);
}

// Debug utilities
void config_print_summary(Config* config) {
    if (!config) return;

    printf("\n=== Configuration Summary ===\n");
    printf("Name: %s\n", config->name);
    printf("Version: %d.%d.%d\n", config->version.major, config->version.minor, config->version.patch);
    printf("Platform: %s\n", PLATFORM_DARWIN ? "macOS" : PLATFORM_LINUX ? "Linux" : "Unknown");
    printf("Extensions defined: %d\n", config->extension_count);
    printf("Core modules: %d\n", config->core_count);
    printf("Static modules: %d\n", config->static_modules.count);
    printf("Shared modules: %d\n", config->shared_modules.count);
    printf("Disabled modules: %d\n", config->disabled_modules.count);
    printf("==============================\n\n");
}

void config_debug_module_sets(Config* config) {
    if (!config) return;

    printf("=== Module Sets Debug ===\n");
    printf("Static modules (%d):\n", config->static_modules.count);
    for (int i = 0; i < config->static_modules.count; i++) {
        printf("  %s\n", config->static_modules.modules[i]);
    }

    printf("\nShared modules (%d):\n", config->shared_modules.count);
    for (int i = 0; i < config->shared_modules.count; i++) {
        printf("  %s\n", config->shared_modules.modules[i]);
    }

    printf("\nDisabled modules (%d):\n", config->disabled_modules.count);
    for (int i = 0; i < config->disabled_modules.count; i++) {
        printf("  %s\n", config->disabled_modules.modules[i]);
    }
    printf("========================\n\n");
}