package switches

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"runtime"

	rhttp "github.com/hashicorp/go-retryablehttp"
	"github.com/schollz/progressbar/v3"
	"github.com/spf13/cobra"
	"github.com/syedtaz/tsuku/utils/list"
	"github.com/syedtaz/tsuku/utils/result"
)

var installCmd = &cobra.Command{
	Use:   "install",
	Short: "Install a specific version of the Koka compiler.",
	Long:  "Install a specific version of the Koka compiler.",
	Run: func(cmd *cobra.Command, args []string) {
		get_release("v2.4.2")
	},
}

func get_release(tag string) {
	req := result.New(rhttp.NewRequest(http.MethodGet, fmt.Sprintf("%s/%s", kokaReleaseTagPrefix, tag), nil)).Unwrap()

	req.Header.Add("Accept", "application/vnd.github+json")
	req.Header.Add("X-GitHub-Api-Version", githubApiVersion)

	resp := result.New(client.Do(req)).Unwrap()
	defer resp.Body.Close()

	switch resp.StatusCode {
	case 200:
		body := result.New(io.ReadAll(resp.Body)).Unwrap()

		var result ReleaseInfo
		json.Unmarshal(body, &result)

		targetTag := generate_download_string(tag)
		findf := func(s ReleaseAsset) bool { return s.Name == targetTag }
		downloadUrl := list.First(list.Filter[ReleaseAsset](result.Assets, findf)).BrowserDownloadURL
		download_release(downloadUrl, targetTag)
	default:
		log.Fatalf("Unexpected resposne code: %d", resp.StatusCode)
	}
}

func download_release(url string, tag string) {
	req := result.New(rhttp.NewRequest(http.MethodGet, url, nil)).Unwrap()

	resp := result.New(client.Do(req)).Unwrap()
	defer resp.Body.Close()

	fh := result.New(os.OpenFile(tag, os.O_CREATE|os.O_WRONLY, 0644)).Unwrap()
	defer fh.Close()

	bar := progressbar.DefaultBytes(
		resp.ContentLength,
		fmt.Sprintf("Downloading %s", tag),
	)

	copyBuffer := make([]byte, 4*1024)
	io.CopyBuffer(io.MultiWriter(fh, bar), resp.Body, copyBuffer)
}

func getOS() string {
	switch runtime.GOOS {
	case "darwin":
		return "macos"
	default:
		return runtime.GOOS
	}
}

func generate_download_string(version string) string {
	return fmt.Sprintf("koka-%s-%s-%s.tar.gz", version, getOS(), runtime.GOARCH)
}
