package cmd

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
	"github.com/syedtaz/tsuku/cmd/switches"
)

var rootCmd = &cobra.Command{
	Use:   "tsuku",
	Short: "package manager for the Koka programming language.",
	Long:  "tsuku is an experimental package manager for the Koka programming language.",
	Run:   func(cmd *cobra.Command, args []string) {},
}

func init() {
	rootCmd.AddCommand(switches.SwitchCmd)
}

func Execute() {
	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %s", err)
		os.Exit(1)
	}
}
