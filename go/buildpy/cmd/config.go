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
	Short: "Manage python configuration",
	Long:  "",
	Run: func(cmd *cobra.Command, args []string) {
		skip, _ := cmd.Flags().GetBool("skip")
		yaml, _ := cmd.Flags().GetBool("yaml")
		write, _ := cmd.Flags().GetString("write")
		version, _ := cmd.Flags().GetString("version")
		name, _ := cmd.Flags().GetString("name")

		log.SetTimeFormat("15:04:05")
		log.SetLevel(log.DebugLevel)

		fmt.Println("config called")

		cfg := config.NewConfig(name, version)

		if !skip {
			cfg.Configure()
		}

		if yaml {
			if write != "" {
				cfg.WriteYaml(write)
			} else {
				cfg.PrintYaml()
			}
		} else {
			if write != "" {
				cfg.WriteSetupLocal(write)
			} else {
				// cfg.PrintSetupLocal()
				cfg.WriteSetupLocal("build/src/python/Modules/Setup.local")
			}
		}

	},
}

func init() {
	rootCmd.AddCommand(configCmd)

	configCmd.Flags().BoolP("skip", "s", false, "Skip configuration step")
	configCmd.Flags().BoolP("yaml", "y", false, "Output configuration in yaml")

	configCmd.Flags().StringP("version", "v", "3.11.7", "Python version ")
	configCmd.Flags().StringP("name", "c", "shared_mid", "Name of configuration")
	configCmd.Flags().StringP("write", "w", "", "Write config to path")

}
