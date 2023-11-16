package switches

import "time"

type ReleaseAsset struct {
	URL                string    `json:"url"`
	BrowserDownloadURL string    `json:"browser_download_url"`
	ID                 int       `json:"id"`
	NodeID             string    `json:"node_id"`
	Name               string    `json:"name"`
	Label              string    `json:"label"`
	State              string    `json:"state"`
	ContentType        string    `json:"content_type"`
	Size               int       `json:"size"`
	CreatedAt          time.Time `json:"created_at"`
	UpdatedAt          time.Time `json:"updated_at"`
}

type ReleaseInfo struct {
	URL             string         `json:"url"`
	HTMLURL         string         `json:"html_url"`
	AssetsURL       string         `json:"assets_url"`
	UploadURL       string         `json:"upload_url"`
	TarballURL      string         `json:"tarball_url"`
	ZipballURL      string         `json:"zipball_url"`
	ID              int            `json:"id"`
	NodeID          string         `json:"node_id"`
	TagName         string         `json:"tag_name"`
	TargetCommitish string         `json:"target_commitish"`
	Name            string         `json:"name"`
	Draft           bool           `json:"draft"`
	Prerelease      bool           `json:"prerelease"`
	CreatedAt       time.Time      `json:"created_at"`
	PublishedAt     time.Time      `json:"published_at"`
	Assets          []ReleaseAsset `json:"assets"`
}

type ReleaseInfos []ReleaseInfo
