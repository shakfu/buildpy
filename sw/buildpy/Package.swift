// swift-tools-version: 5.9
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "buildpy",
    platforms: [
        .macOS(.v14)
    ],
    products: [
        .library(
            name: "BuildPyLib",
            targets: ["BuildPyLib", "CLib"]),
        .executable(
            name: "buildpy",
            targets: ["buildpy"]),
    ],
    dependencies: [
        .package(url: "https://github.com/apple/swift-argument-parser.git", from: "1.2.0"),
        .package(url: "https://github.com/apple/swift-log.git", from: "1.0.0"),
        .package(url: "https://github.com/onevcat/Rainbow", .upToNextMajor(from: "4.0.0")),
        .package(url: "https://github.com/sersoft-gmbh/semver", from: "5.0.0"),
        .package(url: "https://github.com/kylef/PathKit", from: "1.0.1"),
    ],
    targets: [
        .target(
            name: "BuildPyLib",
            dependencies: [
                .product(name: "Logging", package: "swift-log"),
                .product(name: "Rainbow", package: "Rainbow"),
                .product(name: "SemVer", package: "semver"),
                .product(name: "PathKit", package: "PathKit"),
                "CLib"]),
        .testTarget(
            name: "BuildPyLibTests",
            dependencies: ["BuildPyLib"]),
        .target(
            name: "CLib",
            dependencies: [],
            exclude: [],
            sources: [
                "./clib.c",
            ],
            cSettings: [
                .headerSearchPath("./include"),
            ]),
        .executableTarget(
            name: "buildpy",
            dependencies: [
                .product(name: "ArgumentParser", package: "swift-argument-parser"),
                "BuildPyLib",
            ]),
    ]
)
