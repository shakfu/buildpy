#include "buildpy.h"

// Utility functions
char* join_path(const char* path1, const char* path2) {
    if (!path1 || !path2) return NULL;

    size_t len1 = strlen(path1);
    size_t len2 = strlen(path2);
    size_t total_len = len1 + len2 + 2; // +1 for '/', +1 for '\0'

    char* result = malloc(total_len);
    if (!result) return NULL;

    strcpy(result, path1);
    if (len1 > 0 && path1[len1-1] != '/') {
        strcat(result, "/");
    }
    strcat(result, path2);

    return result;
}

char* string_replace(const char* str, const char* old, const char* new_str) {
    if (!str || !old || !new_str) return NULL;

    char* result = malloc(strlen(str) * 2); // Rough estimate
    if (!result) return NULL;

    const char* p = str;
    char* r = result;

    while (*p) {
        if (strstr(p, old) == p) {
            strcpy(r, new_str);
            r += strlen(new_str);
            p += strlen(old);
        } else {
            *r++ = *p++;
        }
    }
    *r = '\0';

    return result;
}

int string_ends_with(const char* str, const char* suffix) {
    if (!str || !suffix) return 0;

    size_t str_len = strlen(str);
    size_t suffix_len = strlen(suffix);

    if (suffix_len > str_len) return 0;

    return strcmp(str + str_len - suffix_len, suffix) == 0;
}

// Base builder functions
Builder* builder_new(const char* name, const char* version, Project* project) {
    if (!name || !version || !project) return NULL;

    Builder* builder = malloc(sizeof(Builder));
    if (!builder) return NULL;

    builder->name = malloc(strlen(name) + 1);
    if (!builder->name) {
        free(builder);
        return NULL;
    }
    strcpy(builder->name, name);

    builder->version = malloc(strlen(version) + 1);
    if (!builder->version) {
        free(builder->name);
        free(builder);
        return NULL;
    }
    strcpy(builder->version, version);

    builder->project = project;
    builder->shell = project->shell;
    builder->build = NULL;
    builder->clean = NULL;

    return builder;
}

void builder_free(Builder* builder) {
    if (builder) {
        free(builder->name);
        free(builder->version);
        free(builder);
    }
}

// OpenSSL Builder
OpenSSLBuilder* openssl_builder_new(const char* version, Project* project) {
    OpenSSLBuilder* openssl_builder = malloc(sizeof(OpenSSLBuilder));
    if (!openssl_builder) return NULL;

    Builder* base = builder_new("OpenSSL", version, project);
    if (!base) {
        free(openssl_builder);
        return NULL;
    }

    openssl_builder->base = *base;
    openssl_builder->base.build = openssl_builder_build;
    openssl_builder->base.clean = openssl_builder_clean;

    free(base); // We copied the contents
    return openssl_builder;
}

void openssl_builder_build(Builder* builder) {
    if (!builder) return;

    log_info("Building %s %s", builder->name, builder->version);

    // Clean version string (remove dots and dashes)
    char* clean_version = string_replace(builder->version, ".", "_");
    char* tag_version = string_replace(clean_version, "-", "_");

    char* source_dir = join_path(builder->project->source_dir, "openssl");
    char* install_dir = builder->project->install_dir;

    // Download and extract
    char cmd[MAX_COMMAND];
    snprintf(cmd, sizeof(cmd),
        "cd \"%s\" && "
        "wget -O openssl-%s.tar.gz https://github.com/openssl/openssl/archive/OpenSSL_%s.tar.gz && "
        "tar xzf openssl-%s.tar.gz && "
        "rm -rf \"%s\" && "
        "mv openssl-OpenSSL_%s \"%s\"",
        builder->project->downloads_dir, builder->version, tag_version, builder->version,
        source_dir, tag_version, source_dir);

    if (shell_run_command(builder->shell, cmd) != 0) {
        log_error("Failed to download OpenSSL");
        goto cleanup;
    }

    // Configure and build
    snprintf(cmd, sizeof(cmd),
        "cd \"%s\" && "
        "./config --prefix=\"%s\" --openssldir=\"%s/ssl\" no-shared && "
        "make -j4 && "
        "make install",
        source_dir, install_dir, install_dir);

    if (shell_run_command(builder->shell, cmd) != 0) {
        log_error("Failed to build OpenSSL");
        goto cleanup;
    }

    log_info("OpenSSL %s build complete", builder->version);

cleanup:
    free(clean_version);
    free(tag_version);
    free(source_dir);
}

void openssl_builder_clean(Builder* builder) {
    if (!builder) return;

    char* source_dir = join_path(builder->project->source_dir, "openssl");
    if (shell_exists(builder->shell, source_dir)) {
        shell_rmtree(builder->shell, source_dir);
    }
    free(source_dir);
}

// Bzip2 Builder
Bzip2Builder* bzip2_builder_new(const char* version, Project* project) {
    Bzip2Builder* bzip2_builder = malloc(sizeof(Bzip2Builder));
    if (!bzip2_builder) return NULL;

    Builder* base = builder_new("Bzip2", version, project);
    if (!base) {
        free(bzip2_builder);
        return NULL;
    }

    bzip2_builder->base = *base;
    bzip2_builder->base.build = bzip2_builder_build;
    bzip2_builder->base.clean = bzip2_builder_clean;

    free(base);
    return bzip2_builder;
}

void bzip2_builder_build(Builder* builder) {
    if (!builder) return;

    log_info("Building %s %s", builder->name, builder->version);

    char* source_dir = join_path(builder->project->source_dir, "bzip2");
    char* install_dir = builder->project->install_dir;

    char cmd[MAX_COMMAND];
    snprintf(cmd, sizeof(cmd),
        "cd \"%s\" && "
        "wget -O bzip2-%s.tar.gz https://sourceware.org/pub/bzip2/bzip2-%s.tar.gz && "
        "tar xzf bzip2-%s.tar.gz && "
        "rm -rf \"%s\" && "
        "mv bzip2-%s \"%s\"",
        builder->project->downloads_dir, builder->version, builder->version, builder->version,
        source_dir, builder->version, source_dir);

    if (shell_run_command(builder->shell, cmd) != 0) {
        log_error("Failed to download Bzip2");
        goto cleanup;
    }

    snprintf(cmd, sizeof(cmd),
        "cd \"%s\" && "
        "make -j4 && "
        "make install PREFIX=\"%s\"",
        source_dir, install_dir);

    if (shell_run_command(builder->shell, cmd) != 0) {
        log_error("Failed to build Bzip2");
        goto cleanup;
    }

    log_info("Bzip2 %s build complete", builder->version);

cleanup:
    free(source_dir);
}

void bzip2_builder_clean(Builder* builder) {
    if (!builder) return;

    char* source_dir = join_path(builder->project->source_dir, "bzip2");
    if (shell_exists(builder->shell, source_dir)) {
        shell_rmtree(builder->shell, source_dir);
    }
    free(source_dir);
}

// XZ Builder
XzBuilder* xz_builder_new(const char* version, Project* project) {
    XzBuilder* xz_builder = malloc(sizeof(XzBuilder));
    if (!xz_builder) return NULL;

    Builder* base = builder_new("XZ", version, project);
    if (!base) {
        free(xz_builder);
        return NULL;
    }

    xz_builder->base = *base;
    xz_builder->base.build = xz_builder_build;
    xz_builder->base.clean = xz_builder_clean;

    free(base);
    return xz_builder;
}

void xz_builder_build(Builder* builder) {
    if (!builder) return;

    log_info("Building %s %s", builder->name, builder->version);

    char* source_dir = join_path(builder->project->source_dir, "xz");
    char* install_dir = builder->project->install_dir;

    char cmd[MAX_COMMAND];
    snprintf(cmd, sizeof(cmd),
        "cd \"%s\" && "
        "wget -O xz-%s.tar.gz https://github.com/tukaani-project/xz/releases/download/v%s/xz-%s.tar.gz && "
        "tar xzf xz-%s.tar.gz && "
        "rm -rf \"%s\" && "
        "mv xz-%s \"%s\"",
        builder->project->downloads_dir, builder->version, builder->version, builder->version, builder->version,
        source_dir, builder->version, source_dir);

    if (shell_run_command(builder->shell, cmd) != 0) {
        log_error("Failed to download XZ");
        goto cleanup;
    }

    snprintf(cmd, sizeof(cmd),
        "cd \"%s\" && "
        "./configure --prefix=\"%s\" --disable-shared --enable-static && "
        "make -j4 && "
        "make install",
        source_dir, install_dir);

    if (shell_run_command(builder->shell, cmd) != 0) {
        log_error("Failed to build XZ");
        goto cleanup;
    }

    log_info("XZ %s build complete", builder->version);

cleanup:
    free(source_dir);
}

void xz_builder_clean(Builder* builder) {
    if (!builder) return;

    char* source_dir = join_path(builder->project->source_dir, "xz");
    if (shell_exists(builder->shell, source_dir)) {
        shell_rmtree(builder->shell, source_dir);
    }
    free(source_dir);
}

// Python Builder
PythonBuilder* python_builder_new(const char* version, Project* project) {
    PythonBuilder* python_builder = malloc(sizeof(PythonBuilder));
    if (!python_builder) return NULL;

    Builder* base = builder_new("Python", version, project);
    if (!base) {
        free(python_builder);
        return NULL;
    }

    python_builder->base = *base;
    python_builder->base.build = python_builder_build;
    python_builder->base.clean = python_builder_clean;

    strcpy(python_builder->config, "static_max");
    python_builder->debug = 0;
    python_builder->optimize = 0;
    python_builder->jobs = 4;
    python_builder->cfg_opts = NULL;
    python_builder->cfg_opts_count = 0;
    python_builder->pkgs = NULL;
    python_builder->pkgs_count = 0;

    // Initialize configuration system
    python_builder->python_config = config_new("static_max", version);
    if (python_builder->python_config) {
        config_configure(python_builder->python_config);
    }

    free(base);
    return python_builder;
}

void python_builder_set_config(PythonBuilder* builder, const char* config) {
    if (builder && config) {
        strncpy(builder->config, config, sizeof(builder->config) - 1);
        builder->config[sizeof(builder->config) - 1] = '\0';

        // Update configuration system
        if (builder->python_config) {
            config_free(builder->python_config);
            builder->python_config = config_new(config, builder->base.version);
            if (builder->python_config) {
                config_configure(builder->python_config);
            }
        }
    }
}

void python_builder_set_debug(PythonBuilder* builder, int debug) {
    if (builder) {
        builder->debug = debug;
    }
}

void python_builder_set_optimize(PythonBuilder* builder, int optimize) {
    if (builder) {
        builder->optimize = optimize;
    }
}

void python_builder_set_jobs(PythonBuilder* builder, int jobs) {
    if (builder) {
        builder->jobs = jobs;
    }
}

void python_builder_add_cfg_opt(PythonBuilder* builder, const char* opt) {
    if (!builder || !opt) return;

    builder->cfg_opts = realloc(builder->cfg_opts, (builder->cfg_opts_count + 1) * sizeof(char*));
    if (builder->cfg_opts) {
        builder->cfg_opts[builder->cfg_opts_count] = malloc(strlen(opt) + 1);
        if (builder->cfg_opts[builder->cfg_opts_count]) {
            strcpy(builder->cfg_opts[builder->cfg_opts_count], opt);
            builder->cfg_opts_count++;
        }
    }
}

void python_builder_add_pkg(PythonBuilder* builder, const char* pkg) {
    if (!builder || !pkg) return;

    builder->pkgs = realloc(builder->pkgs, (builder->pkgs_count + 1) * sizeof(char*));
    if (builder->pkgs) {
        builder->pkgs[builder->pkgs_count] = malloc(strlen(pkg) + 1);
        if (builder->pkgs[builder->pkgs_count]) {
            strcpy(builder->pkgs[builder->pkgs_count], pkg);
            builder->pkgs_count++;
        }
    }
}

void python_builder_build(Builder* builder) {
    if (!builder) return;

    PythonBuilder* py_builder = (PythonBuilder*)builder;

    log_info("Building Python %s with config: %s", builder->version, py_builder->config);

    // Build dependencies first
    log_info("Building dependencies...");

    OpenSSLBuilder* openssl = openssl_builder_new("1.1.1w", builder->project);
    if (openssl) {
        openssl->base.build((Builder*)openssl);
        free(openssl);
    }

    Bzip2Builder* bzip2 = bzip2_builder_new("1.0.8", builder->project);
    if (bzip2) {
        bzip2->base.build((Builder*)bzip2);
        free(bzip2);
    }

    XzBuilder* xz = xz_builder_new("5.8.2", builder->project);
    if (xz) {
        xz->base.build((Builder*)xz);
        free(xz);
    }

    // Download Python
    log_info("Downloading Python %s...", builder->version);
    char* source_dir = join_path(builder->project->source_dir, "python");

    char cmd[MAX_COMMAND];
    snprintf(cmd, sizeof(cmd),
        "cd \"%s\" && "
        "wget -O python-%s.tar.xz https://www.python.org/ftp/python/%s/Python-%s.tar.xz && "
        "tar xJf python-%s.tar.xz && "
        "rm -rf \"%s\" && "
        "mv Python-%s \"%s\"",
        builder->project->downloads_dir, builder->version, builder->version, builder->version,
        builder->version, source_dir, builder->version, source_dir);

    if (shell_run_command(builder->shell, cmd) != 0) {
        log_error("Failed to download Python");
        goto cleanup;
    }

    // Configure Python
    log_info("Configuring Python...");

    // Generate Setup.local using configuration system
    if (py_builder->python_config) {
        char* setup_local_path = join_path(source_dir, "Modules/Setup.local");
        if (setup_local_path) {
            log_info("Generating Setup.local for Python configuration");
            config_write_setup_local(py_builder->python_config, setup_local_path);
            free(setup_local_path);
        }
    }

    char configure_opts[MAX_COMMAND];
    snprintf(configure_opts, sizeof(configure_opts),
        "--prefix=\"%s\" "
        "--enable-static-libs "
        "--disable-shared "
        "--with-openssl=\"%s\" "
        "--with-ensurepip=no",
        builder->project->install_dir,
        builder->project->install_dir);

    if (py_builder->debug) {
        strcat(configure_opts, " --with-pydebug");
    }

    if (py_builder->optimize) {
        strcat(configure_opts, " --enable-optimizations");
    }

    snprintf(cmd, sizeof(cmd),
        "cd \"%s\" && "
        "./configure %s",
        source_dir, configure_opts);

    if (shell_run_command(builder->shell, cmd) != 0) {
        log_error("Failed to configure Python");
        goto cleanup;
    }

    // Build Python
    log_info("Building Python...");
    snprintf(cmd, sizeof(cmd),
        "cd \"%s\" && "
        "make -j%d",
        source_dir, py_builder->jobs);

    if (shell_run_command(builder->shell, cmd) != 0) {
        log_error("Failed to build Python");
        goto cleanup;
    }

    // Install Python
    log_info("Installing Python...");
    snprintf(cmd, sizeof(cmd),
        "cd \"%s\" && "
        "make install",
        source_dir);

    if (shell_run_command(builder->shell, cmd) != 0) {
        log_error("Failed to install Python");
        goto cleanup;
    }

    // Clean up Python library
    log_info("Cleaning Python library...");
    const char* cleanup_patterns[] = {
        "*.pyo", "*.pyc", "__pycache__", "*.so", "test*"
    };

    char* lib_python_dir = join_path(builder->project->lib_dir, "python*");
    shell_glob_remove(builder->shell, cleanup_patterns, 5, lib_python_dir);
    free(lib_python_dir);

    // Create ZIP archive
    log_info("Creating Python library archive...");
    char* zip_file = join_path(builder->project->build_dir, "python-lib.zip");
    shell_zip_directory(builder->shell, builder->project->lib_dir, zip_file);
    free(zip_file);

    log_info("Python %s build complete", builder->version);

cleanup:
    free(source_dir);
}

void python_builder_clean(Builder* builder) {
    if (!builder) return;

    PythonBuilder* py_builder = (PythonBuilder*)builder;

    // Clean up configuration
    if (py_builder->python_config) {
        config_free(py_builder->python_config);
        py_builder->python_config = NULL;
    }

    char* source_dir = join_path(builder->project->source_dir, "python");
    if (shell_exists(builder->shell, source_dir)) {
        shell_rmtree(builder->shell, source_dir);
    }
    free(source_dir);
}