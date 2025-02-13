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
		version, _ := cmd.Flags().GetString("version")
		name, _ := cmd.Flags().GetString("name")

		write, _ := cmd.Flags().GetString("write")
		read, _ := cmd.Flags().GetString("read")

		skip, _ := cmd.Flags().GetBool("skip")
		yaml, _ := cmd.Flags().GetBool("yaml")
		local, _ := cmd.Flags().GetBool("local")


		log.SetTimeFormat("15:04:05")
		log.SetLevel(log.DebugLevel)

		fmt.Println("config called")

		cfg := config.NewConfig(name, version)

		if !skip {
			cfg.Configure()
		}

		if read != "" {
			cfg.FromYaml(read)
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
				cfg.PrintSetupLocal()
				if local {				
					cfg.WriteSetupLocal("build/src/python/Modules/Setup.local")
				}
			}
		}
	},
}

func init() {
	rootCmd.AddCommand(configCmd)
	configCmd.Flags().BoolP("skip", "s", false, "skip configuration step")
	configCmd.Flags().BoolP("yaml", "y", false, "output configuration in yaml")
	configCmd.Flags().StringP("version", "v", "3.12.7", "python version ")
	configCmd.Flags().StringP("name", "c", "shared_mid", "name of configuration")
	configCmd.Flags().StringP("write", "w", "", "write config to path")
	configCmd.Flags().StringP("read",  "r", "", "read yaml config from path")
	configCmd.Flags().BoolP("local", "l", false, "write setup.local")
}
