/*
Copyright © 2024 NAME HERE <EMAIL ADDRESS>
*/
package cmd

import (
	// "fmt"
	"github.com/charmbracelet/log"
	"github.com/shakfu/buildpy/internal/models"
	"github.com/spf13/cobra"
	// "os/exec"
)

// buildCmd represents the build command
var buildCmd = &cobra.Command{
	Use:   "build",
	Short: "Build python from source",
	Long: `A longer description that spans multiple lines and likely contains examples
and usage of using your command. For example:

Cobra is a CLI library for Go that empowers applications.
This application is a tool to generate the needed files
to quickly create a Cobra application.`,
	Run: func(cmd *cobra.Command, args []string) {
		config, _ := cmd.Flags().GetString("config")
		version, _ := cmd.Flags().GetString("version")
		pkgs, _ := cmd.Flags().GetStringSlice("pkgs")
		opts, _ := cmd.Flags().GetStringSlice("opts")

		log.SetTimeFormat("15:04:05")
		// log.SetReportCaller(true)
		log.Info("build called")
		log.Printf("config:%v opts:%v pkgs:%v args: %v\n", config, opts, pkgs, args)

		builder := models.NewPythonBuilder(version, config)
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
	buildCmd.Flags().BoolP("optimize", "O", false, "Optimize build")
	buildCmd.Flags().BoolP("reset", "r", false, "Reset build")
	buildCmd.Flags().StringP("config", "c", "shared_mid", "Python version")
	buildCmd.Flags().StringP("version", "v", "3.11.7", "Build configuration")

	buildCmd.Flags().StringSliceP("pkgs", "p", []string{}, "Add python packages")
	buildCmd.Flags().StringSliceP("opts", "o", []string{}, "Add python config options")

}
