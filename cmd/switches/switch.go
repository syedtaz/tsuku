package switches

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"strings"
	"time"

	"github.com/fatih/color"
	"github.com/hashicorp/go-retryablehttp"
	rhttp "github.com/hashicorp/go-retryablehttp"
	"github.com/spf13/cobra"
	List "github.com/syedtaz/tsuku/utils/list"
	Result "github.com/syedtaz/tsuku/utils/result"
)

// Static variables

var client *retryablehttp.Client

const kokaReleaseLink string = "https://api.github.com/repos/koka-lang/koka/releases"
const githubApiVersion string = "2022-11-28"

var SwitchCmd = &cobra.Command{
	Use:   "switch",
	Short: "Manage multiple installations of the Koka language.",
	Long:  "Manage multiple installations of the Koka language.",
	Run:   func(cmd *cobra.Command, args []string) {},
}

func init() {
	SwitchCmd.AddCommand(listCmd)
	listCmd.Flags().Bool("available", false, "Lists all releases of Koka that can be installed.")
	client = retryablehttp.NewClient()
	client.Logger = nil
}

var versions = []string{"1.0.1", "1.0.2"}

var listCmd = &cobra.Command{
	Use:   "list",
	Short: "List available or installed versions of the Koka language.",
	Long:  "List available or installed versions of the Koka language.",
	Run: func(cmd *cobra.Command, args []string) {
		flag, err := cmd.Flags().GetBool("available")
		if err != nil {
			log.Fatalf("no flag passed")
			return
		}

		if flag {
			getReleases()
		} else {
			List.Iter[string](versions, func(x string) { fmt.Printf("%s\n", x) })
		}

	},
}

func getReleases() {
	req := Result.New(rhttp.NewRequest(http.MethodGet, kokaReleaseLink, nil)).Unwrap()

	req.Header.Add("Accept", "application/vnd.github+json")
	req.Header.Add("X-GitHub-Api-Version", githubApiVersion)

	resp := Result.New(client.Do(req)).Unwrap()
	defer resp.Body.Close()

	switch resp.StatusCode {
	case 200:
		body := Result.New(io.ReadAll(resp.Body)).Unwrap()

		var result ReleaseInfos
		json.Unmarshal(body, &result)

		type pack struct {
			version string
			date    string
		}

		extractTag := func(r ReleaseInfo) pack {
			return pack{strings.TrimSpace(r.TagName), r.PublishedAt.Local().UTC().Format(time.DateOnly)}
		}
		filterBlank := func(s pack) bool { return len(s.version) > 0 }
		tags := List.Filter(List.Map(result, extractTag), filterBlank)

		// Print
		c := color.New(color.FgRed, color.Bold)
		c.Println("Version\t\tDate Published")
		List.Iter(tags, func(x pack) { fmt.Printf("%s\t\t%s\n", x.version, x.date) })
	}
}
