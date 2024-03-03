package cmd

import (
	"runtime"

	"github.com/charmbracelet/log"
	"github.com/shakfu/buildpy/internal/models"
	"github.com/spf13/cobra"
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
		version, _ := cmd.Flags().GetString("version")
		config, _ := cmd.Flags().GetString("config")
		pkgs, _ := cmd.Flags().GetStringSlice("pkgs")
		opts, _ := cmd.Flags().GetStringSlice("opts")
		jobs, _ := cmd.Flags().GetInt("jobs")
		optimize, _ := cmd.Flags().GetBool("optimize")
		reset, _ := cmd.Flags().GetBool("reset")
		debug, _ := cmd.Flags().GetBool("debug")
		git, _ := cmd.Flags().GetBool("git")

		log.SetTimeFormat("15:04:05")
		if debug {
			log.SetReportCaller(true)
		}
		log.Info("Environment", "runtime", runtime.Version(), "platform", runtime.GOOS, "arch", runtime.GOARCH)
		log.Info("build cmd", "ver", version, "cfg", config, "opts", opts, "pkgs", pkgs, "jobs", jobs)
		builder := models.NewPythonBuilder(version, config)
		builder.ConfigOptions = opts
		builder.Packages = pkgs
		builder.Optimize = optimize
		builder.Jobs = jobs
		builder.UseGit = git
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
	buildCmd.Flags().IntP("jobs", "j", 4, "Set # of build jobs")
	buildCmd.Flags().BoolP("optimize", "O", false, "Optimize build")
	buildCmd.Flags().BoolP("reset", "r", false, "Reset build")
	buildCmd.Flags().BoolP("debug", "d", false, "Debug build")
	buildCmd.Flags().BoolP("git", "g", false, "Use git to download python")
}
