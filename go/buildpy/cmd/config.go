package cmd

import (
	"fmt"

	"github.com/charmbracelet/log"
	"github.com/shakfu/buildpy/internal/config"
	"github.com/spf13/cobra"
)

// configCmd represents the config command
var configCmd = &cobra.Command{
	Use:   "config",
	Short: "configure build",
	Long: `The configure subcmd is handles everything related to build 
configurations. For example:

This can include dumping the existing configuration, changing it,
saving it, etc..`,
	Run: func(cmd *cobra.Command, args []string) {
		fmt.Println("config called")

		version, _ := cmd.Flags().GetString("version")
		cfg, _ := cmd.Flags().GetString("config")
		write, _ := cmd.Flags().GetString("write")

		log.SetTimeFormat("15:04:05")
		// log.Info("Environment", "runtime", runtime.Version(), "platform", runtime.GOOS, "arch", runtime.GOARCH)
		// log.Info("build cmd", "ver", version, "cfg", config, "opts", opts, "pkgs", pkgs, "jobs", jobs)

		// config.Demo()
		if write != "" {
			config.ConfigWrite(version, cfg, write)
		} else {
			config.ConfigWrite(version, cfg, "build/src/python/Modules/Setup.local")
		}
	},
}

func init() {
	rootCmd.AddCommand(configCmd)

	// Here you will define your flags and configuration settings.

	// Cobra supports Persistent Flags which will work for this command
	// and all subcommands, e.g.:
	// configCmd.PersistentFlags().String("foo", "", "A help for foo")

	// Cobra supports local flags which will only run when this command
	// is called directly, e.g.:
	// configCmd.Flags().BoolP("toggle", "t", false, "Help message for toggle")
	configCmd.Flags().StringP("write", "w", "", "Write config to path")
	configCmd.Flags().StringP("version", "v", "3.11.7", "Build configuration")
	configCmd.Flags().StringP("config", "c", "shared_mid", "Python version")
	// configCmd.Flags().StringSliceP("opts", "o", []string{}, "Override python config options")
	// configCmd.Flags().StringSliceP("pkgs", "p", []string{}, "Add python packages")
	// configCmd.Flags().IntP("jobs", "j", 1, "Set # of build jobs")
	// configCmd.Flags().BoolP("optimize", "O", false, "Optimize build")
	// configCmd.Flags().BoolP("reset", "r", false, "Reset build")
	// configCmd.Flags().BoolP("debug", "d", false, "Debug build")
	// configCmd.Flags().BoolP("git", "g", false, "Use git to download python")
}
