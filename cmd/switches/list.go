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
	rhttp "github.com/hashicorp/go-retryablehttp"
	"github.com/spf13/cobra"
	"github.com/syedtaz/tsuku/utils/list"
	"github.com/syedtaz/tsuku/utils/result"
)

var versions = []string{"1.0.1", "1.0.2"}

func init() {
	listCmd.Flags().Bool("available", false, "Lists all releases of Koka that can be installed.")
}

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
			list.Iter[string](versions, func(x string) { fmt.Printf("%s\n", x) })
		}

	},
}

func getReleases() {
	req := result.New(rhttp.NewRequest(http.MethodGet, kokaReleasePrefix, nil)).Unwrap()

	req.Header.Add("Accept", "application/vnd.github+json")
	req.Header.Add("X-GitHub-Api-Version", githubApiVersion)

	resp := result.New(client.Do(req)).Unwrap()
	defer resp.Body.Close()

	switch resp.StatusCode {
	case 200:
		body := result.New(io.ReadAll(resp.Body)).Unwrap()

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
		tags := list.Filter(list.Map(result, extractTag), filterBlank)

		// Print
		c := color.New(color.FgRed, color.Bold)
		c.Println("Version\t\tDate Published")
		list.Iter(tags, func(x pack) { fmt.Printf("%s\t\t%s\n", x.version, x.date) })
	default:
		log.Fatalf("Unexpected resposne code: %d", resp.StatusCode)
	}
}
