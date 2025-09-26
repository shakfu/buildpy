#include "buildpy.h"

Project* project_new(void) {
    Project* project = malloc(sizeof(Project));
    if (!project) return NULL;

    project->shell = shell_new();
    if (!project->shell) {
        free(project);
        return NULL;
    }

    // Initialize directory paths
    char* cwd = shell_get_cwd(project->shell);
    if (!cwd) {
        shell_free(project->shell);
        free(project);
        return NULL;
    }

    snprintf(project->build_dir, sizeof(project->build_dir), "%s/build", cwd);
    snprintf(project->downloads_dir, sizeof(project->downloads_dir), "%s/build/downloads", cwd);
    snprintf(project->source_dir, sizeof(project->source_dir), "%s/build/src", cwd);
    snprintf(project->install_dir, sizeof(project->install_dir), "%s/build/install", cwd);
    snprintf(project->bin_dir, sizeof(project->bin_dir), "%s/build/install/bin", cwd);
    snprintf(project->lib_dir, sizeof(project->lib_dir), "%s/build/install/lib", cwd);
    snprintf(project->lib_static_dir, sizeof(project->lib_static_dir), "%s/build/install/lib", cwd);

    return project;
}

void project_free(Project* project) {
    if (project) {
        shell_free(project->shell);
        free(project);
    }
}

int project_setup(Project* project) {
    if (!project) return -1;

    log_info("Setting up build environment");

    const char* dirs[] = {
        project->build_dir,
        project->downloads_dir,
        project->source_dir,
        project->install_dir,
        project->bin_dir,
        project->lib_dir
    };

    for (size_t i = 0; i < sizeof(dirs) / sizeof(dirs[0]); i++) {
        if (!shell_exists(project->shell, dirs[i])) {
            log_info("Creating directory: %s", dirs[i]);
            if (shell_mkdir_p(project->shell, dirs[i]) != 0) {
                log_error("Failed to create directory: %s", dirs[i]);
                return -1;
            }
        }
    }

    log_info("Build environment setup complete");
    return 0;
}

void project_clean(Project* project) {
    if (!project) return;

    log_info("Cleaning build environment");

    if (shell_exists(project->shell, project->build_dir)) {
        log_info("Removing build directory: %s", project->build_dir);
        shell_rmtree(project->shell, project->build_dir);
    }

    log_info("Build environment cleaned");
}