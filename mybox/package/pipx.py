import json
import subprocess
from typing import Optional

import requests

from ..utils import run, run_output
from .base import Package


def get_pipx_versions() -> dict[str, str]:
    pipx_list = json.loads(
        run_output("pipx", "list", "--json", stderr=subprocess.DEVNULL)
    )
    packages = (
        item["metadata"]["main_package"] for item in pipx_list["venvs"].values()
    )
    return {package["package"]: package["package_version"] for package in packages}


class PipxPackage(Package):
    def __init__(self, *, pipx: str, **kwargs) -> None:
        self.pipx = pipx
        super().__init__(**kwargs)

    @property
    def name(self) -> str:
        return self.pipx

    def get_remote_version(self) -> str:
        pypi_info = requests.get(f"https://pypi.org/pypi/{self.pipx}/json").json()
        return pypi_info["info"]["version"]

    @property
    def local_version(self) -> Optional[str]:
        return get_pipx_versions().get(self.pipx)

    def install(self) -> None:
        cmd = "install" if self.local_version is None else "upgrade"
        run("pipx", cmd, self.pipx)
