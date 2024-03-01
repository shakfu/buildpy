/*
Copyright © 2024 NAME HERE <EMAIL ADDRESS>
*/
package cmd

import (
	"github.com/charmbracelet/log"
	"github.com/shakfu/buildpy/internal/models"
	"github.com/spf13/cobra"
	"runtime"
)

// buildCmd represents the build command
var buildCmd = &cobra.Command{
	Use:   "build",
	Short: "Build python from source",
	Long: `A tool to download, configure, build, install, and shrink python from source
From source. Can be used as follows:

	$ buildpy build -v 3.12.2 -c "static_max" -p "cython,ipython"

	$ buildpy build --opts="--disable-ipv6,--with-lto=thin"
`,
	Run: func(cmd *cobra.Command, args []string) {
		config, _ := cmd.Flags().GetString("config")
		version, _ := cmd.Flags().GetString("version")
		pkgs, _ := cmd.Flags().GetStringSlice("pkgs")
		opts, _ := cmd.Flags().GetStringSlice("opts")
		optimize, _ := cmd.Flags().GetBool("optimize")
		reset, _ := cmd.Flags().GetBool("reset")

		log.SetTimeFormat("15:04:05")
		// log.SetReportCaller(true)
		log.Info("buildpy build", "cfg", config, "opts", opts, "pkgs", pkgs)

		builder := models.NewPythonBuilder(version, config)
		log.Info("Environment", "runtime", runtime.Version(), "os/arch", builder.PlatformArch())
		if len(opts) > 0 {
			builder.SetConfigOptions(opts)
		}
		if len(pkgs) > 0 {
			builder.SetPackages(pkgs)
		}
		builder.Optimize = optimize
		if reset {
			builder.Project.Reset()
		}
		builder.Process()
	},
}

func init() {
	rootCmd.AddCommand(buildCmd)

	// Here you will define your flags and configuration settings.

	// Cobra supports Persistent Flags which will work for this command
	// and all subcommands, e.g.:
	// buildCmd.PersistentFlags().String("foo", "", "A help for foo")

	// Cobra supports local flags which will only run when this command
	// is called directly, e.g.:
	// buildCmd.Flags().BoolP("toggle", "t", false, "Help message for toggle")
	buildCmd.Flags().StringP("version", "v", "3.11.7", "Build configuration")
	buildCmd.Flags().StringP("config", "c", "shared_mid", "Python version")
	buildCmd.Flags().StringSliceP("opts", "o", []string{}, "Override python config options")
	buildCmd.Flags().StringSliceP("pkgs", "p", []string{}, "Add python packages")
	buildCmd.Flags().BoolP("optimize", "O", false, "Optimize build")
	buildCmd.Flags().BoolP("reset", "r", false, "Reset build")
}
