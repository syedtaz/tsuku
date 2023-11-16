package switches

import (
	"github.com/hashicorp/go-retryablehttp"
	"github.com/spf13/cobra"
)

var client *retryablehttp.Client

const kokaReleasePrefix string = "https://api.github.com/repos/koka-lang/koka/releases"
const kokaReleaseTagPrefix string = "https://api.github.com/repos/koka-lang/koka/releases/tags"
const githubApiVersion string = "2022-11-28"

var SwitchCmd = &cobra.Command{
	Use:   "switch",
	Short: "Manage multiple installations of the Koka language.",
	Long:  "Manage multiple installations of the Koka language.",
	Run:   func(cmd *cobra.Command, args []string) {},
}

func init() {
	SwitchCmd.AddCommand(listCmd)
	SwitchCmd.AddCommand(installCmd)
	client = retryablehttp.NewClient()
	client.Logger = nil
}
