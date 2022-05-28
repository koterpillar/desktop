import requests

from .archive import ArchivePackage


class URLPackage(ArchivePackage):
    def __init__(self, *, url: str, **kwargs) -> None:
        self.url = url
        super().__init__(**kwargs)

    def archive_url(self) -> str:
        return self.url

    @property
    def name(self):
        parts = self.url.split("/")
        while True:
            if len(parts) == 0:
                raise ValueError(f"Cannot parse package name from {self.url}.")
            elif parts[0] in ("", "https:"):
                parts.pop(0)
            elif parts[0] == "github.com":
                return "/".join(parts[1:2])
            else:
                return parts[0]

    def get_remote_version(self) -> str:
        head_response = requests.head(self.url, allow_redirects=True)
        return head_response.headers["etag"]
