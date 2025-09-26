#include "buildpy.h"
#include <stdarg.h>
#include <dirent.h>
#include <ftw.h>

Shell* shell_new(void) {
    Shell* shell = malloc(sizeof(Shell));
    if (!shell) return NULL;

    shell->verbose = 0;
    if (getcwd(shell->cwd, sizeof(shell->cwd)) == NULL) {
        free(shell);
        return NULL;
    }

    return shell;
}

void shell_free(Shell* shell) {
    if (shell) {
        free(shell);
    }
}

void shell_set_verbose(Shell* shell, int verbose) {
    if (shell) {
        shell->verbose = verbose;
    }
}

char* shell_get_cwd(Shell* shell) {
    if (!shell) return NULL;

    if (getcwd(shell->cwd, sizeof(shell->cwd)) == NULL) {
        return NULL;
    }
    return shell->cwd;
}

int shell_chdir(Shell* shell, const char* path) {
    if (!shell || !path) return -1;

    if (chdir(path) == 0) {
        if (getcwd(shell->cwd, sizeof(shell->cwd)) == NULL) {
            return -1;
        }
        return 0;
    }
    return -1;
}

int shell_exists(Shell* shell, const char* path) {
    if (!shell || !path) return 0;

    struct stat st;
    return stat(path, &st) == 0;
}

int shell_is_dir(Shell* shell, const char* path) {
    if (!shell || !path) return 0;

    struct stat st;
    if (stat(path, &st) == 0) {
        return S_ISDIR(st.st_mode);
    }
    return 0;
}

int shell_is_file(Shell* shell, const char* path) {
    if (!shell || !path) return 0;

    struct stat st;
    if (stat(path, &st) == 0) {
        return S_ISREG(st.st_mode);
    }
    return 0;
}

int shell_mkdir(Shell* shell, const char* path) {
    if (!shell || !path) return -1;

    return mkdir(path, 0755);
}

int shell_mkdir_p(Shell* shell, const char* path) {
    if (!shell || !path) return -1;

    char tmp[MAX_PATH];
    char *p = NULL;
    size_t len;

    snprintf(tmp, sizeof(tmp), "%s", path);
    len = strlen(tmp);
    if (tmp[len - 1] == '/') {
        tmp[len - 1] = 0;
    }

    for (p = tmp + 1; *p; p++) {
        if (*p == '/') {
            *p = 0;
            if (mkdir(tmp, 0755) != 0 && errno != EEXIST) {
                return -1;
            }
            *p = '/';
        }
    }

    if (mkdir(tmp, 0755) != 0 && errno != EEXIST) {
        return -1;
    }

    return 0;
}

static int remove_file(const char* fpath, const struct stat* sb, int typeflag, struct FTW* ftwbuf) {
    (void)sb;
    (void)typeflag;
    (void)ftwbuf;

    return remove(fpath);
}

int shell_rmtree(Shell* shell, const char* path) {
    if (!shell || !path) return -1;

    return nftw(path, remove_file, 64, FTW_DEPTH | FTW_PHYS);
}

int shell_copy(Shell* shell, const char* src, const char* dst) {
    if (!shell || !src || !dst) return -1;

    FILE *source, *dest;
    char buffer[4096];
    size_t bytes;

    source = fopen(src, "rb");
    if (!source) return -1;

    dest = fopen(dst, "wb");
    if (!dest) {
        fclose(source);
        return -1;
    }

    while ((bytes = fread(buffer, 1, sizeof(buffer), source)) > 0) {
        if (fwrite(buffer, 1, bytes, dest) != bytes) {
            fclose(source);
            fclose(dest);
            return -1;
        }
    }

    fclose(source);
    fclose(dest);
    return 0;
}

int shell_move(Shell* shell, const char* src, const char* dst) {
    if (!shell || !src || !dst) return -1;

    if (rename(src, dst) == 0) {
        return 0;
    }

    // If rename fails, try copy and delete
    if (shell_copy(shell, src, dst) == 0) {
        return remove(src);
    }

    return -1;
}

int shell_symlink(Shell* shell, const char* src, const char* dst) {
    if (!shell || !src || !dst) return -1;

    return symlink(src, dst);
}

int shell_chmod(Shell* shell, const char* path, mode_t mode) {
    if (!shell || !path) return -1;

    return chmod(path, mode);
}

int shell_run_command(Shell* shell, const char* cmd) {
    if (!shell || !cmd) return -1;

    if (shell->verbose) {
        log_info("Running: %s", cmd);
    }

    return system(cmd);
}

int shell_run_command_in_dir(Shell* shell, const char* cmd, const char* work_dir) {
    if (!shell || !cmd || !work_dir) return -1;

    char* old_cwd = shell_get_cwd(shell);
    if (!old_cwd) return -1;

    if (shell_chdir(shell, work_dir) != 0) {
        return -1;
    }

    int result = shell_run_command(shell, cmd);

    shell_chdir(shell, old_cwd);
    return result;
}

char* shell_run_command_output(Shell* shell, const char* cmd) {
    if (!shell || !cmd) return NULL;

    FILE* fp = popen(cmd, "r");
    if (!fp) return NULL;

    char* output = malloc(4096);
    if (!output) {
        pclose(fp);
        return NULL;
    }

    size_t len = fread(output, 1, 4095, fp);
    output[len] = '\0';

    pclose(fp);
    return output;
}

static int match_pattern(const char* pattern, const char* string) {
    if (strcmp(pattern, string) == 0) return 1;

    if (strchr(pattern, '*') != NULL) {
        // Simple glob matching - convert * to .* for basic regex-like behavior
        const char* p = pattern;
        const char* s = string;

        while (*p && *s) {
            if (*p == '*') {
                p++;
                if (*p == '\0') return 1;  // Pattern ends with *, match rest
                while (*s && *s != *p) s++;  // Skip until we find matching char
                if (*s == '\0') return 0;
            } else if (*p == *s) {
                p++;
                s++;
            } else {
                return 0;
            }
        }

        return (*p == '\0' || (*p == '*' && *(p+1) == '\0')) && *s == '\0';
    }

    return strstr(string, pattern) != NULL;
}

static void walk_directory(Shell* shell, const char* base_path, const char** patterns, int pattern_count) {
    DIR* dir = opendir(base_path);
    if (!dir) return;

    struct dirent* entry;
    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
            continue;
        }

        char* full_path = join_path(base_path, entry->d_name);
        if (!full_path) continue;

        if (shell_is_dir(shell, full_path)) {
            walk_directory(shell, full_path, patterns, pattern_count);
        } else {
            // Check if file matches any pattern
            for (int i = 0; i < pattern_count; i++) {
                if (match_pattern(patterns[i], entry->d_name)) {
                    if (shell->verbose) {
                        log_info("Removing: %s", full_path);
                    }
                    remove(full_path);
                    break;
                }
            }
        }

        free(full_path);
    }

    closedir(dir);
}

void shell_glob_remove(Shell* shell, const char** patterns, int pattern_count, const char* base_path) {
    if (!shell || !patterns || pattern_count <= 0 || !base_path) return;

    if (shell->verbose) {
        log_info("Removing files matching patterns in %s", base_path);
    }

    walk_directory(shell, base_path, patterns, pattern_count);
}

int shell_zip_directory(Shell* shell, const char* source_dir, const char* dest_zip) {
    if (!shell || !source_dir || !dest_zip) return -1;

    char cmd[MAX_COMMAND];
    snprintf(cmd, sizeof(cmd), "cd \"%s\" && zip -r \"%s\" .", source_dir, dest_zip);

    return shell_run_command(shell, cmd);
}

void log_info(const char* format, ...) {
    va_list args;
    va_start(args, format);
    printf("[INFO] ");
    vprintf(format, args);
    printf("\n");
    va_end(args);
}

void log_error(const char* format, ...) {
    va_list args;
    va_start(args, format);
    fprintf(stderr, "[ERROR] ");
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n");
    va_end(args);
}