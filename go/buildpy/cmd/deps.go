/*
Copyright © 2024 NAME HERE <EMAIL ADDRESS>
*/
package cmd

import (
	"fmt"

	"github.com/charmbracelet/log"
	"github.com/shakfu/buildpy/internal/models"
	"github.com/spf13/cobra"
)

// depsCmd represents the deps command
var depsCmd = &cobra.Command{
	Use:   "deps",
	Short: "Build and manage python dependencies",
	Long:  "",
	Run: func(cmd *cobra.Command, args []string) {
		fmt.Println("deps called")
		all, _ := cmd.Flags().GetBool("all")
		ssl, _ := cmd.Flags().GetBool("ssl")
		bz2, _ := cmd.Flags().GetBool("bz2")
		xz, _ := cmd.Flags().GetBool("xz")

		log.SetTimeFormat("15:04:05")

		if ssl || all {
			log.Info("building/install openssl")
			models.InstallOpenssl()
		}
		if bz2 || all {
			log.Info("building/install bz2")
			models.InstallBzip2()
		}
		if xz || all {
			log.Info("building/install xz")
			models.InstallXz()
		}
	},
}

func init() {
	rootCmd.AddCommand(depsCmd)

	// Here you will define your flags and configuration settings.

	// Cobra supports Persistent Flags which will work for this command
	// and all subcommands, e.g.:
	// depsCmd.PersistentFlags().String("foo", "", "A help for foo")

	// Cobra supports local flags which will only run when this command
	// is called directly, e.g.:
	// depsCmd.Flags().BoolP("toggle", "t", false, "Help message for toggle")
	depsCmd.Flags().BoolP("all", "a", false, "build/install all deps")
	depsCmd.Flags().BoolP("ssl", "s", false, "build/install openssl")
	depsCmd.Flags().BoolP("bz2", "b", false, "build/install bzip2")
	depsCmd.Flags().BoolP("xz", "x", false, "build/install xz")
}
