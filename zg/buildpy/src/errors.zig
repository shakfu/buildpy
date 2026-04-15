//! Error definitions for buildpy
//!
//! Provides a comprehensive error set for all build operations.

const std = @import("std");

/// Build-related errors
pub const BuildError = error{
    /// Command execution failed
    CommandFailed,
    /// Download operation failed
    DownloadFailed,
    /// Checksum verification failed
    ChecksumMismatch,
    /// Archive extraction failed
    ExtractionFailed,
    /// Path traversal attack detected
    PathTraversalDetected,
    /// Archive type not supported
    UnsupportedArchiveType,
    /// Validation of input failed
    ValidationFailed,
    /// URL format is invalid
    InvalidUrl,
    /// Required file not found
    FileNotFound,
    /// I/O operation error
    IoError,
    /// Memory allocation failed
    OutOfMemory,
    /// Platform not supported
    PlatformNotSupported,
    /// Configuration error
    ConfigError,
    /// Build type not recognized
    InvalidBuildType,
    /// Version parsing failed
    InvalidVersion,
    /// Archive not found
    ArchiveNotFound,
    /// Source directory not found
    SourceNotFound,
    /// Dependency build failed
    DependencyFailed,
    /// Invalid argument
    InvalidArgument,
};

/// Format a BuildError into a human-readable string
pub fn formatError(err: BuildError) []const u8 {
    return switch (err) {
        error.CommandFailed => "Command execution failed",
        error.DownloadFailed => "Download operation failed",
        error.ChecksumMismatch => "Checksum verification failed",
        error.ExtractionFailed => "Archive extraction failed",
        error.PathTraversalDetected => "Path traversal attack detected",
        error.UnsupportedArchiveType => "Unsupported archive type",
        error.ValidationFailed => "Input validation failed",
        error.InvalidUrl => "Invalid URL format",
        error.FileNotFound => "Required file not found",
        error.IoError => "I/O operation error",
        error.OutOfMemory => "Memory allocation failed",
        error.PlatformNotSupported => "Platform not supported",
        error.ConfigError => "Configuration error",
        error.InvalidBuildType => "Invalid build type",
        error.InvalidVersion => "Invalid version format",
        error.ArchiveNotFound => "Archive not found",
        error.SourceNotFound => "Source directory not found",
        error.DependencyFailed => "Dependency build failed",
        error.InvalidArgument => "Invalid argument",
    };
}

test "error formatting" {
    const err_str = formatError(error.CommandFailed);
    try std.testing.expectEqualStrings("Command execution failed", err_str);
}
