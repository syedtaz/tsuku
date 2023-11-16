package switches

import (
	"fmt"

	"github.com/spf13/cobra"
)

var SwitchCmd = &cobra.Command{
	Use:   "switch",
	Short: "Manage multiple installations of the Koka language.",
	Long:  "Manage multiple installations of the Koka language.",
	Run: func(cmd *cobra.Command, args []string) {
		fmt.Printf("Hello!")
	},
}
