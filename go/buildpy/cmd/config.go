/*
Copyright © 2024 NAME HERE <EMAIL ADDRESS>
*/
package cmd

import (
	"fmt"

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
		config.Demo()
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
	configCmd.Flags().BoolP("toggle", "t", false, "Help message for toggle")
	configCmd.Flags().StringP("write", "w", "", "Write config to path")
}
